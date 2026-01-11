library(shiny)
library(shinyjs)
library(dplyr)
library(highcharter)
library(rmarkdown)
library(DT)
library(tidyr)
library(shinyWidgets)
library(promises)
library(future)

# Cargar módulos
source("modules/mod_navigation.R")
source("modules/mod_indice_dashboard.R")
source("modules/mod_dimension_buttons.R")
source("modules/mod_dimension_view.R")
source("modules/mod_acciones_mejora.R")
source("modules/mod_download.R")

# Cargar utilidades y lógica de negocio
source("utils/odk_helpers.R")
source("utils/report_utils.R")
source("utils/generar_datos_reales.R")
source("utils/safe_data_processing.R")
source("utils/data_etl.R")
source("utils/generar_dataframes_estaticos.R")
source("utils/generar_dataframes_dinamicos.R")

# Definir pestañas para cada componente
pestanas_satisfaccion <- c(
  "indice" = "Índice de Satisfacción y Experiencia",
  "percepcion" = "Percepción ciudadana sobre el servicio",
  "mejora" = "Acciones de Mejora"
)

pestanas_prestacion <- c(
  "indice" = "Índice de calidad del servicio prestado",
  "gestion" = "Gestión Efectiva de las respuestas",
  "eficiencia" = "Eficiencia Institucional",
  "capacidad" = "Capacidad y protocolos del servicio",
  "mejora" = "Acciones de Mejora"
)

server <- function(input, output, session) {
  suppressWarnings({
  session$sendCustomMessage("showOverlay", TRUE)
  
  # ===========================================
  # DESCARGA DE DATOS Y CONSTRUCCIÓN DE ESTRUCTURAS
  # ===========================================
  
  suppressWarnings(descarga_matrices_fastapi())
  suppressWarnings(proceso_etl())
  suppressWarnings(source("utils/calculo_indicadores.R"))
  
  # Definir los valores reactivos
  rv_bg <- reactiveValues(
    general = NULL,         
    historico = NULL,
    canal = NULL,         
    subcanal = NULL,
    ranking_entidades = NULL,
    errores = list()
  )
  
  filtros_confirmados <- reactiveVal(list(
    nivel = "Distrito",
    ids = NULL,
    anios = NULL,
    meses = NULL,
    canal = NULL,
    subcanal = NULL
  ))
  
  # Guardar datos de startup para reutilizar
  rv_startup <- reactiveValues(
    general = NULL,
    historico = NULL,
    canal = NULL,
    ranking_entidades = NULL
  )
  
  # Definir las señales
  startup_ready_ts <- reactiveVal(NULL)
  general_ready_ts <- reactiveVal(NULL)
  historico_ready_ts <- reactiveVal(NULL)
  canal_ready_ts <- reactiveVal(NULL)
  subcanal_ready_ts <- reactiveVal(NULL)
  nivel_change_ts <- reactiveVal(NULL)
  filtros_fecha_change_ts <- reactiveVal(NULL)
  
  # Flags de control
  job_historico_running <- reactiveVal(FALSE)
  job_canal_running <- reactiveVal(FALSE)
  job_subcanal_running <- reactiveVal(FALSE)
  job_filtros_fecha_running <- reactiveVal(FALSE)
  job_nivel_running <- reactiveVal(FALSE)
  
  # Empaquetar las señales
  signals <- list(
    startup   = startup_ready_ts,
    general   = general_ready_ts,
    historico = historico_ready_ts,
    canales   = canal_ready_ts,
    subcanal  = subcanal_ready_ts,
    nivel_change = nivel_change_ts,
    filtros_fecha = filtros_fecha_change_ts
  )
  
  # ===========================================
  # HELPER DE RECALCULO
  # ===========================================
  reset_datos_dinamicos <- function() {
    rv_bg$general <- NULL
    rv_bg$historico <- NULL
    rv_bg$canal <- NULL
    rv_bg$subcanal <- NULL
    rv_bg$ranking_entidades <- NULL
  }
  
  generar_historico_ultimos_periodos <- function(nivel, ids_seleccionados) {
    fecha_actual <- lubridate::floor_date(Sys.Date(), "month")
    ultimos_4 <- seq(from = fecha_actual, by = "-1 month", length.out = 4)
    
    historicos <- list()
    for (periodo in ultimos_4) {
      periodo_key <- format(as.Date(periodo), "%Y-%m")
      historicos[[periodo_key]] <- generar_dataframes_con_filtros(
        anios = as.integer(format(as.Date(periodo), "%Y")),
        meses = as.integer(format(as.Date(periodo), "%m")),
        nivel = nivel,
        ids_seleccionados = ids_seleccionados
      )
    }
    historicos
  }
  
  normalizar_periodos <- function(anios, meses) {
    # If meses is already a vector of period strings "YYYY-MM", return it directly
    if (!is.null(meses) && length(meses) > 0 && is.character(meses) && all(grepl("^\\d{4}-\\d{2}$", meses))) {
      return(list(anios = NULL, meses = meses))
    }

    if (is.null(anios) || length(anios) == 0) {
      return(list(anios = NULL, meses = NULL))
    }

    if (length(anios) > 1) {
      return(list(anios = anios, meses = 1:12))
    }

    if (is.null(meses) || length(meses) == 0) {
      return(list(anios = anios, meses = 1:12))
    }

    list(anios = anios, meses = as.integer(meses))
  }
  
  obtener_ids_desde_estado <- function(estado) {
    nivel <- estado$filtro_nivel
    
    if (is.null(nivel) || nivel == "Distrito") {
      return(list(ids = NULL, vacio = FALSE, todos = TRUE))
    }
    
    if (nivel == "Sector") {
      sectores_sel <- estado$sector_checks
      todos_sectores <- sort(df_sectores$Sector)
      if (is.null(sectores_sel)) return(list(ids = NULL, vacio = TRUE, todos = FALSE))
      if (length(sectores_sel) == 0) return(list(ids = NULL, vacio = TRUE, todos = FALSE))
      todos <- length(sectores_sel) == length(todos_sectores) && all(sort(sectores_sel) == todos_sectores)
      ids <- df_sectores %>% filter(Sector %in% sectores_sel) %>% pull(Id_Sector)
      return(list(ids = ids, vacio = FALSE, todos = todos))
    }
    
    entidades_sel <- estado$entidad_checks
    todas_entidades <- sort(df_entidades$Entidad)
    if (is.null(entidades_sel)) return(list(ids = NULL, vacio = TRUE, todos = FALSE))
    if (length(entidades_sel) == 0) return(list(ids = NULL, vacio = TRUE, todos = FALSE))
    todos <- length(entidades_sel) == length(todas_entidades) && all(sort(entidades_sel) == todas_entidades)
    ids <- df_entidades %>% filter(Entidad %in% entidades_sel) %>% pull(Id_Entidad)
    list(ids = ids, vacio = FALSE, todos = todos)
  }
  
  filtros_sin_restriccion <- function(estado, ids_info) {
    if (!exists("periodos")) return(FALSE)
    
    anios_totales <- sort(unique(periodos$Año))
    anios_sel <- estado$filtro_anio
    meses_sel <- estado$filtro_mes
    
    anios_completos <- is.null(anios_sel) ||
      (length(anios_sel) == length(anios_totales) &&
         all(sort(as.integer(anios_sel)) == sort(as.integer(anios_totales))))
    
    meses_completos <- is.null(meses_sel) || length(meses_sel) == 12
    
    anios_completos && meses_completos && isTRUE(ids_info$todos) && estado$filtro_nivel == "Distrito"
  }
  
  lanzar_recalculo_completo <- function(filtros, estado_origen, notification_id, done_message, on_complete = NULL, on_error = NULL) {
    showNotification(done_message, type = "message", id = notification_id, duration = NULL)
    
    reset_datos_dinamicos()
    general_ready_ts(NULL)
    historico_ready_ts(NULL)
    canal_ready_ts(NULL)
    subcanal_ready_ts(NULL)
    
    filtros_confirmados(
      modifyList(filtros_confirmados(), list(
        nivel = filtros$nivel,
        ids = filtros$ids,
        anios = filtros$anios,
        meses = filtros$meses
      ))
    )
    
    if (!is.null(rv_startup$general) && filtros_sin_restriccion(estado_origen, filtros$ids_info)) {
      isolate({
        rv_bg$general <- rv_startup$general
        rv_bg$historico <- rv_startup$historico
        rv_bg$canal <- rv_startup$canal
        rv_bg$ranking_entidades <- rv_startup$ranking_entidades
      })
      
      general_ready_ts(Sys.time())
      historico_ready_ts(Sys.time())
      canal_ready_ts(Sys.time())
      
      removeNotification(notification_id)
      showNotification("Filtros aplicados correctamente.", type = "message", duration = 3)
      
      if (isTRUE(estado_origen$filtro_canal_detalle)) {
        rv_bg$subcanal <- NULL
        subcanal_ready_ts(NULL)
        lanzar_job_subcanales(estado_origen)
      }
      
      if (is.function(on_complete)) on_complete()
      return(invisible(NULL))
    }
    
    # FUTURE 1: General y Ranking
    future_promise({
      list(
        general = actualizar_dataframes_general(
          anios = filtros$anios,
          meses = filtros$meses,
          canal = NULL,
          subcanal = NULL,
          ids_seleccionados = filtros$ids,
          nivel = filtros$nivel
        ),
        ranking = actualizar_ranking_entidades(
          anios = filtros$anios,
          meses = filtros$meses,
          canal = NULL,
          subcanal = NULL
        )
      )
    }) %...>% (function(res) {
      isolate({
        rv_bg$general <- res$general
        rv_bg$ranking_entidades <- res$ranking
      })
      general_ready_ts(Sys.time())
      removeNotification(notification_id)
      showNotification("Filtros aplicados correctamente.", type = "message", duration = 3)
      if (is.function(on_complete)) on_complete()
    }) %...!% (function(err) {
      isolate({ rv_bg$errores$filtros <- err$message })
      removeNotification(notification_id)
      showNotification(paste("Error:", err$message), type = "error")
      if (is.function(on_error)) on_error()
    })
    
    # FUTURE 2: Histórico
    if (!job_historico_running()) {
      rv_bg$historico <- NULL
      job_historico_running(TRUE)
      
      future_promise({
        is_period_vector <- !is.null(filtros$meses) && length(filtros$meses) > 0 && is.character(filtros$meses) && all(grepl("^\\d{4}-\\d{2}$", filtros$meses))

        # If no years selected and meses is NOT a period vector, fallback to últimos 4
        if ((is.null(filtros$anios) || length(filtros$anios) == 0) && !is_period_vector) {
          generar_historico_ultimos_periodos(
            nivel = filtros$nivel,
            ids_seleccionados = filtros$ids
          )
        } else {
          actualizar_dataframes_historico(
            anios = filtros$anios,
            meses = filtros$meses,
            canal = NULL,
            subcanal = NULL,
            nivel = filtros$nivel,
            ids_seleccionados = filtros$ids
          )
        }
      }) %...>% (function(res_hist) {
        isolate({ rv_bg$historico <- res_hist })
        historico_ready_ts(Sys.time())
        job_historico_running(FALSE)
      }) %...!% (function(err) {
        isolate({ rv_bg$errores$historico <- err$message })
        job_historico_running(FALSE)
      })
    }
    
    # FUTURE 3: Canales
    if (!job_canal_running()) {
      rv_bg$canal <- NULL
      job_canal_running(TRUE)
      
      future_promise({
        canales_obj <- obtener_canales()
        res_canales <- list()
        for (c_nm in canales_obj) {
          res_canales[[c_nm]] <- generar_dataframes_con_filtros(
            anios = filtros$anios,
            meses = filtros$meses,
            canal = c_nm,
            nivel = filtros$nivel,
            ids_seleccionados = filtros$ids
          )
        }
        res_canales
      }) %...>% (function(lista_canales) {
        isolate({ rv_bg$canal <- lista_canales })
        canal_ready_ts(Sys.time())
        job_canal_running(FALSE)
        
        if (isTRUE(estado_origen$filtro_canal_detalle)) {
          rv_bg$subcanal <- NULL
          subcanal_ready_ts(NULL)
          lanzar_job_subcanales(estado_origen)
        }
      }) %...!% (function(err) {
        isolate({ rv_bg$errores$canales <- err$message })
        job_canal_running(FALSE)
      })
    }
  }
  
  
  # ===========================================
  # JOB 1: STARTUP (Datos generales iniciales)
  # ===========================================
  future_promise({
    generar_dataframes_filtrados_startup()
  }) %...>% (function(datos_startup) {
    isolate({
      # Guardar en rv_bg (activo)
      rv_bg$general <- datos_startup$general
      rv_bg$historico <- datos_startup$historico
      rv_bg$canal <- datos_startup$canales
      rv_bg$ranking_entidades <- list(
        ranking_entidades_c1 = datos_startup$ranking_entidades_c1,
        ranking_entidades_c2 = datos_startup$ranking_entidades_c2
      )
      
      # GUARDAR COPIA EN STARTUP (para reutilizar)
      rv_startup$general <- datos_startup$general
      rv_startup$historico <- datos_startup$historico
      rv_startup$canal <- datos_startup$canales
      rv_startup$ranking_entidades <- rv_bg$ranking_entidades
    })
    
    general_ready_ts(Sys.time())
    historico_ready_ts(Sys.time())
    canal_ready_ts(Sys.time())
    startup_ready_ts(Sys.time())
    
    generar_indicadores_criticos()
  }) %...!% (function(err) {
    isolate({ rv_bg$errores$startup <- err$message })
    startup_ready_ts(Sys.time())
    general_ready_ts(NULL)
  })
  
  # ===========================================
  # CAPTURAR FILTROS DESDE EL MÓDULO
  # ===========================================
  filtros_modulo <- indice_dashboard_server(
    "indice_dash", 
    id_componente_actual,
    rv_bg,
    signals
  )
  
  # ===========================================
  # JOB 2: CAMBIO DE NIVEL (Solo actualiza general - instantáneo)
  # ===========================================
  
  nivel_observers_registry <- reactiveVal(character(0))
  
  observar_cambio_nivel <- function(origen, clave) {
    if (is.null(origen)) return()
    
    nivel_registrado <- reactiveVal(FALSE)
    if (isolate(nivel_registrado())) return()
    nivel_registrado(TRUE)
    
    
    ultimo_estado_hash <- reactiveVal(NULL)
    procesando <- reactiveVal(FALSE)
    
    # NUEVO: Timestamp para controlar el rebote
    last_complex_calc_ts <- reactiveVal(0) 
    
    observeEvent(origen$estado()$filtro_nivel, {
      nivel <- origen$estado()$filtro_nivel
      req(nivel)
      
      filtros_confirmados(
        modifyList(filtros_confirmados(), list(nivel = nivel))
      )
      
      nivel_change_ts(Sys.time())
    })
  }
  
  # ===========================================
  # JOB 3: OBSERVAR CAMBIOS EN IDS (Sectores/Entidades)
  # ===========================================
  
  ids_observers_registry <- reactiveVal(character(0))
  
  observar_cambio_ids <- function(origen, clave) {
    if (is.null(origen)) return()
    
    registrados <- isolate(ids_observers_registry())
    if (clave %in% registrados) return()
    
    
    ultimos_ids_str <- reactiveVal("")
    
    observeEvent(list(
      origen$estado()$sector_checks,
      origen$estado()$entidad_checks
    ), {
      
      estado <- isolate(origen$estado())
      nivel <- estado$filtro_nivel
      
      if (!nivel %in% c("Sector", "Entidad")) return()
      
      ids_info <- obtener_ids_desde_estado(estado)
      if (isTRUE(ids_info$vacio)) return()
      
      filtros_confirmados(
        modifyList(filtros_confirmados(), list(ids = ids_info$ids))
      )
    }, ignoreInit = TRUE)
    
    ids_observers_registry(c(registrados, clave))
  }
  
  # ===========================================
  # JOB 4: FILTROS DE FECHA (Botón - actualiza TODO)
  # ===========================================
  
  manejar_click_fechas <- function(origen) {
    observeEvent(origen$btn_click(), {
      if (job_filtros_fecha_running()) {
        showNotification("Ya hay un cálculo en proceso.", type = "warning")
        return()
      }
      
      sel_event <- origen$evento()
      sel_anios <- sel_event$filtro_anio
      sel_meses <- sel_event$filtro_mes
      
      if (is.null(sel_anios) || length(sel_anios) == 0) {
        showNotification("Debe seleccionar al menos un año.", type = "warning")
        return()
      }
      
      periodos <- normalizar_periodos(sel_anios, sel_meses)
      estado_origen <- isolate(origen$estado())
      ids_info <- obtener_ids_desde_estado(estado_origen)
      
      if (isTRUE(ids_info$vacio)) {
        showNotification("Debe seleccionar al menos un elemento para filtrar por nivel.", type = "warning")
        return()
      }
      
      filtros <- list(
        anios = periodos$anios,
        meses = periodos$meses,
        nivel = estado_origen$filtro_nivel,
        ids = ids_info$ids,
        ids_info = ids_info
      )
      
      job_filtros_fecha_running(TRUE)
      lanzar_recalculo_completo(
        filtros,
        estado_origen,
        "filtros_proc",
        "Aplicando filtros de fecha...",
        on_complete = function() job_filtros_fecha_running(FALSE),
        on_error = function() job_filtros_fecha_running(FALSE)
      )
      
    }, ignoreInit = TRUE)
  }
  
  manejar_click_nivel <- function(origen) {
    observeEvent(origen$nivel_click(), {
      if (job_nivel_running()) {
        showNotification("Ya hay un cálculo en proceso.", type = "warning")
        return()
      }
      
      estado_origen <- isolate(origen$estado())
      ids_info <- obtener_ids_desde_estado(estado_origen)
      
      if (isTRUE(ids_info$vacio)) {
        showNotification("Debe seleccionar al menos un elemento para filtrar por nivel.", type = "warning")
        return()
      }
      
      periodos <- normalizar_periodos(estado_origen$filtro_anio, estado_origen$filtro_mes)
      
      filtros <- list(
        anios = periodos$anios,
        meses = periodos$meses,
        nivel = estado_origen$filtro_nivel,
        ids = ids_info$ids,
        ids_info = ids_info
      )
      
      job_nivel_running(TRUE)
      lanzar_recalculo_completo(
        filtros,
        estado_origen,
        "nivel_proc",
        "Aplicando filtrado por nivel...",
        on_complete = function() job_nivel_running(FALSE),
        on_error = function() job_nivel_running(FALSE)
      )
    }, ignoreInit = TRUE)
  }
  
  # ===========================================
  # SUBCANALES (Solo carga, no recalcula)
  # ===========================================
  
  lanzar_job_subcanales <- function(estado_origen) {
    
    detalle_activo <- estado_origen$filtro_canal_detalle
    canal_seleccionado <- estado_origen$filtro_canal_selector
    
    
    if (!isTRUE(detalle_activo) || is.null(canal_seleccionado)) {
      return()
    }
    
    if (job_subcanal_running()) {
      showNotification("Cargando subcanales...", type = "message", duration = 2)
      return()
    }
    
    showNotification("Cargando datos de subcanales...", type = "message", id = "subcanal_proc", duration = NULL)
    job_subcanal_running(TRUE)
    
    filtros <- filtros_confirmados()
    future_promise({
      subs <- obtener_subcanales(canal_seleccionado)
      
      if (length(subs) == 0) {
        return(list(canal = canal_seleccionado, datos = NULL, error = "No hay subcanales"))
      }
      
      res_subcanales <- list()
      for (sub_nm in subs) {
        
        sub_id <- df_canales %>% filter(Subcanal == sub_nm) %>% pull(Id_Subcanal)
        
        if (length(sub_id) == 0) {
          next
        }
        
        res_subcanales[[sub_nm]] <- generar_dataframes_filtrados_subcanal(
          anios = filtros$anios,
          meses = filtros$meses,
          canal = canal_seleccionado,
          subcanal = sub_id,
          nivel = estado_origen$filtro_nivel,
          ids_seleccionados = filtros$ids
        )
      }
      
      list(canal = canal_seleccionado, datos = res_subcanales, error = NULL)
    }) %...>% (function(resultado) {
      
      isolate({
        if (!is.null(resultado$error)) {
          showNotification(resultado$error, type = "warning")
        } else {
          if (is.null(rv_bg$subcanal)) rv_bg$subcanal <- list()
          rv_bg$subcanal[[resultado$canal]] <- resultado$datos
          subcanal_ready_ts(Sys.time())
          showNotification("Subcanales cargados correctamente.", type = "message", duration = 3)
        }
      })
      job_subcanal_running(FALSE)
      removeNotification("subcanal_proc")
    }) %...!% (function(err) {
      isolate({ rv_bg$errores$subcanales <- err$message })
      job_subcanal_running(FALSE)
      removeNotification("subcanal_proc")
      showNotification(paste("Error al cargar subcanales:", err$message), type = "error")
    })
  }
  
  # ===========================================
  # REGISTRO DE MANEJADORES
  # ===========================================
  
  manejadores_registrados <- reactiveVal(character(0))
  registrar_manejador <- function(origen, clave) {
    if (is.null(origen)) return()
    registrados <- isolate(manejadores_registrados())
    if (clave %in% registrados) return()
    manejar_click_fechas(origen)
    manejar_click_nivel(origen)
    manejadores_registrados(c(registrados, clave))
  }
  
  filtros_dimension_preview <- reactiveVal(NULL)
  filtros_dimension_view <- reactiveVal(NULL)
  
  manejadores_subcanal <- reactiveVal(character(0))
  registrar_manejador_subcanal <- function(origen, clave) {
    if (is.null(origen)) return()
    registrados <- isolate(manejadores_subcanal())
    if (clave %in% registrados) return()
    
    observeEvent({
      list(
        origen$estado()$filtro_canal_detalle,
        origen$estado()$filtro_canal_selector
      )
    }, {
      estado <- isolate(origen$estado())
      if (isTRUE(estado$filtro_canal_detalle)) {
        lanzar_job_subcanales(estado)
      } else {
        isolate({
          rv_bg$subcanal <- NULL
          subcanal_ready_ts(NULL)
        })
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    manejadores_subcanal(c(registrados, clave))
  }
  
  # Registrar para el módulo principal
  registrar_manejador(filtros_modulo, "indice")
  registrar_manejador_subcanal(filtros_modulo, "indice")
  observar_cambio_nivel(filtros_modulo, "indice")
  observar_cambio_ids(filtros_modulo, "indice")
  
  # ===========================================
  # ESTADOS DE NAVEGACIÓN
  # ===========================================
  
  active_main_tab <- reactiveVal("satisfaccion")
  active_sub_tab <- reactiveVal("indice")
  active_dimension <- reactiveVal(NULL)
  
  id_componente_actual <- reactive({
    if (active_main_tab() == "satisfaccion") 2 else 1
  })

  observeEvent(id_componente_actual(), {
    if (!is.null(rv_startup$general)) {
      rv_bg$general <- rv_startup$general
      rv_bg$historico <- rv_startup$historico
      rv_bg$canal <- rv_startup$canal
      rv_bg$ranking_entidades <- rv_startup$ranking_entidades
      rv_bg$subcanal <- NULL

      general_ready_ts(Sys.time())
      historico_ready_ts(Sys.time())
      canal_ready_ts(Sys.time())
      subcanal_ready_ts(NULL)

      filtros_confirmados(
        modifyList(filtros_confirmados(), list(
          nivel = "Distrito",
          ids = NULL,
          anios = NULL,
          meses = NULL,
          canal = NULL,
          subcanal = NULL
        ))
      )
    }
  }, ignoreInit = TRUE)
  
  pestanas_componente <- reactive({
    if (active_main_tab() == "satisfaccion") pestanas_satisfaccion else pestanas_prestacion
  })
  
  # ============================================
  # NAVEGACIÓN PRINCIPAL
  # ============================================
  
  observeEvent(input$nav_satisfaccion, {
    active_main_tab("satisfaccion")
    active_sub_tab("indice")
    active_dimension(NULL)
    shinyjs::addClass("nav_satisfaccion", "active")
    shinyjs::removeClass("nav_prestacion", "active")
  })
  
  observeEvent(input$nav_prestacion, {
    active_main_tab("prestacion")
    active_sub_tab("indice")
    active_dimension(NULL)
    shinyjs::addClass("nav_prestacion", "active")
    shinyjs::removeClass("nav_satisfaccion", "active")
  })

  observeEvent(list(active_sub_tab(), active_dimension()), {
    sub_tab <- active_sub_tab()
    if (sub_tab %in% c("indice", "mejora")) return()
    if (!is.null(rv_startup$general)) {
      rv_bg$general <- rv_startup$general
      rv_bg$historico <- rv_startup$historico
      rv_bg$canal <- rv_startup$canal
      rv_bg$ranking_entidades <- rv_startup$ranking_entidades
      rv_bg$subcanal <- NULL

      general_ready_ts(Sys.time())
      historico_ready_ts(Sys.time())
      canal_ready_ts(Sys.time())
      subcanal_ready_ts(NULL)

      filtros_confirmados(
        modifyList(filtros_confirmados(), list(
          nivel = "Distrito",
          ids = NULL,
          anios = NULL,
          meses = NULL,
          canal = NULL,
          subcanal = NULL
        ))
      )
    }
  }, ignoreInit = TRUE)
  
  # ============================================
  # RENDERIZADO PRINCIPAL
  # ============================================
  
  output$main_content_wrapper <- renderUI({
    div(class = "white-box", uiOutput("main_content"))
  })
  
  # Ocultar overlay cuando el contenido principal esté renderizado
  observeEvent(general_ready_ts(), {
    # Esperar un momento para que el contenido se renderice
    invalidateLater(200, session)
    isolate({
      if (!is.null(general_ready_ts())) {
        session$sendCustomMessage("hideOverlay", TRUE)
      }
    })
  }, once = TRUE)
  
  output$main_content <- renderUI({
    req(startup_ready_ts())
    
    pestanas <- pestanas_componente()
    pilar_keys <- names(pestanas)
    botones_lista <- list()
    
    for (i in seq_along(pilar_keys)) {
      id <- pilar_keys[i]
      clase_color <- dplyr::case_when(id == "indice" ~ "btn-indice", id == "mejora" ~ "btn-mejora", TRUE ~ "btn-pilar")
      clase_active <- ifelse(id == active_sub_tab(), "active", "")
      
      botones_lista[[length(botones_lista) + 1]] <- actionButton(
        paste0("sub_btn_", id), pestanas[id], class = paste("top-button", clase_color, clase_active)
      )
      
      if (id == "indice" || (i == length(pilar_keys) - 1 && pilar_keys[i+1] == "mejora")) {
        botones_lista[[length(botones_lista) + 1]] <- div(class = "button-separator")
      }
    }
    
    tagList(
      div(class = "top-buttons-container", botones_lista),
      uiOutput("dynamic_content_area")
    )
  })
  
  observe({
    pestanas <- pestanas_componente()
    lapply(names(pestanas), function(id) {
      observeEvent(input[[paste0("sub_btn_", id)]], {
        active_sub_tab(id)
        active_dimension(NULL)
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    req(active_sub_tab())
    shinyjs::removeClass(selector = ".top-button", class = "active")
    shinyjs::addClass(paste0("sub_btn_", active_sub_tab()), "active")
  })
  
  # ============================================
  # CONTENIDO DINÁMICO
  # ============================================
  
  output$dynamic_content_area <- renderUI({
    sub_tab <- active_sub_tab()
    if (sub_tab == "indice") {
      indice_dashboard_ui("indice_dash", id_componente_actual())
    } else if (sub_tab == "mejora") {
      acciones_mejora_ui("acciones_mejora")
    } else {
      render_pilar_view(sub_tab)
    }
  })
  
  # ============================================
  # VISTA DE PILAR Y DIMENSIONES
  # ============================================
  
  id_pilar_actual <- reactive({
    sub_tab <- active_sub_tab()
    id_comp <- id_componente_actual()
    if (id_comp == 2) { if (sub_tab == "percepcion") return(4) }
    else {
      if (sub_tab == "gestion") return(1)
      if (sub_tab == "eficiencia") return(2)
      if (sub_tab == "capacidad") return(3)
    }
    return(NULL)
  })
  
  render_pilar_view <- function(pilar_key) {
    id_pil <- id_pilar_actual()
    id_comp <- id_componente_actual()
    
    if (!exists("df_indicadores") || nrow(df_indicadores) == 0) return(div("Cargando estructura..."))
    
    dimensiones <- df_indicadores %>%
      filter(Id_Componente == id_comp, Id_Pilar == id_pil, Id_Dimension != 0) %>%
      select(Id_Dimension, Dimensión) %>%
      distinct() %>%
      arrange(Id_Dimension)
    
    if (nrow(dimensiones) == 0) return(div("No hay dimensiones disponibles"))
    
    dim_list <- setNames(dimensiones$Dimensión, as.character(dimensiones$Id_Dimension))
    
    tagList(
      dimension_buttons_ui("dim_buttons", dim_list),
      uiOutput("dimension_content")
    )
  }
  
  observe({
    sub_tab <- active_sub_tab()
    if (!sub_tab %in% c("indice", "mejora")) {
      id_pil <- id_pilar_actual()
      if (!is.null(id_pil) && exists("df_indicadores")) {
        id_comp <- id_componente_actual()
        dim_data <- df_indicadores %>% filter(Id_Componente == id_comp, Id_Pilar == id_pil, Id_Dimension != 0)
        
        if (nrow(dim_data) > 0) {
          dim_list <- setNames(unique(dim_data$Dimensión), unique(dim_data$Id_Dimension))
          dimension_buttons_server("dim_buttons", dim_list, active_dimension)
        }
      }
    }
  })
  
  output$dimension_content <- renderUI({
    if (is.null(active_dimension())) dimension_preview_ui("dimension_preview")
    else dimension_view_ui("dimension_view")
  })
  
  observe({
    sub_tab <- active_sub_tab()
    id_pil <- id_pilar_actual()
    
    if (!sub_tab %in% c("indice", "mejora") && is.null(active_dimension()) && !is.null(id_pil)) {
      if (is.null(filtros_dimension_preview())) {
        filtros_dimension_preview(
          dimension_preview_server(
            "dimension_preview",
            id_componente = id_componente_actual,
            id_pilar = id_pilar_actual,
            rv_bg = rv_bg,
            signals = signals
          )
        )
        registrar_manejador(filtros_dimension_preview(), "dimension_preview")
        observar_cambio_nivel(filtros_dimension_preview(), "dimension_preview")
      }
    }
  })
  
  observe({
    id_pil <- id_pilar_actual()
    
    if (!is.null(active_dimension()) && !is.null(id_pil)) {
      if (is.null(filtros_dimension_view())) {
        filtros_dimension_view(
          dimension_view_server(
            "dimension_view",
            id_componente = id_componente_actual,
            id_pilar = id_pilar_actual,
            id_dimension = active_dimension,
            rv_bg,
            signals
          )
        )
        registrar_manejador(filtros_dimension_view(), "dimension_view")
        registrar_manejador_subcanal(filtros_dimension_view(), "dimension_view")
        observar_cambio_nivel(filtros_dimension_view(), "dimension_view")
      }
    }
  })
  
  observe({
    if (active_sub_tab() == "mejora") {
      acciones_mejora_server("acciones_mejora", id_componente_actual)
    }
  })
  
  observeEvent(input$reload_app, { session$reload() })
  })
}
