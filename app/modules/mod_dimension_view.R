library(shinycssloaders)
library(scales)
source("modules/mod_filtros.R")

# ============================================
# 1. PREVIEW DE PILAR (Ranking de dimensiones)
# ============================================

dimension_preview_local_ui <- function(id) {
  NULL
}

dimension_preview_plot_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "plot-area",
    div(class = "plot-box plot-box-full",
        withSpinner(highchartOutput(ns("plot_ranking_dimensiones"), height = "500px"), 
                    type=4, color="#225495")
    )
  )
}

dimension_preview_server <- function(id, id_componente, id_pilar, rv_bg, signals, filtros) {
  moduleServer(id, function(input, output, session) {

    # ============================================
    # 1. FILTROS: vienen del modulo global (mod_filtros.R)
    # ============================================
    filtros_evento    <- filtros$evento
    ids_seleccionados <- filtros$ids_seleccionados
    
    # ============================================
    # 4. FUENTE DE DATOS
    # ============================================
    
    datos_actuales <- reactive({
      req(signals$general())
      return(rv_bg)
    })
    
    output$plot_ranking_dimensiones <- renderHighchart({
      datos <- datos_actuales()
      req(datos$general)
      
      id_pil  <- id_pilar()
      if (is.null(id_pil)) return(NULL)
      
      ranking <- obtener_indices_dimensiones(datos$general, id_pil)

      if (!is.null(ranking) && nrow(ranking) > 0)
        ranking <- ranking %>% filter(!is.na(Valor))

      if (is.null(ranking) || nrow(ranking) == 0) {
        return(.error_chart("No hay datos de dimensiones disponibles."))
      }
      
      ranking_coloreado <- ranking %>%
        mutate(
          Indice = hud_round_pct(Valor),
          Color = hud_color_indicador(Indice)
        )
      
      hchart(ranking_coloreado, "bar",
             hcaes(x = Dimensión, y = Indice, color = Color)) %>%
        hc_chart(options3d = list(enabled = TRUE, alpha = 0, beta = 0)) %>%
        hc_plotOptions(bar = list(depth = 40, shape = 'cylinder',
                                  dataLabels = list(enabled = TRUE, format = '{y}%'),
                                  colorByPoint = TRUE)) %>%
        hc_title(text = "Ranking de Dimensiones") %>%
        hc_legend(enabled = FALSE)
    })
    
    invisible(NULL)
  })
}

# ============================================
# 2. VISTA DE DIMENSIÓN (Indicadores + Detalle)
# ============================================
dimension_view_local_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "filter-panel",
    tags$h4("Seleccione el Indicador", style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
    div(style="margin-bottom: 5px; font-size: 0.9em;",
        actionLink(ns("indicador_select_all"), "Todos"), " | ",
        actionLink(ns("indicador_deselect_all"), "Ninguno")),
    div(class = "scrollable-checkbox-group",
        uiOutput(ns("indicador_selector_ui"))),

    hr(),
    downloadButton(ns("descargar_reporte"), "Imprimir PDF", style = "width: 100%; margin-top: 10px;"),
    uiOutput(ns("excel_detallado_ui"))
  )
}

dimension_view_plot_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "plot-area",
    div(class = "plot-box plot-box-full",
        highchartOutput(ns("plot_ranking_indicadores"), height = "400px")),
    div(class = "plot-box plot-box-full",
        withSpinner(highchartOutput(ns("plot_desempeno_periodo"), height = "400px"),
                    type=4, color="#225495")),
    div(class = "plot-box plot-box-full",
        withSpinner(highchartOutput(ns("plot_desempeno_canal"), height = "400px"),
                    type=4, color="#225495"))
  )
}

dimension_view_server <- function(id, id_componente, id_pilar, id_dimension, rv_bg, signals, filtros) {
  moduleServer(id, function(input, output, session) {

    color_azul <- "#225495"

    placeholder_chart <- function(texto) {
      highchart() %>%
        hc_chart(backgroundColor = "transparent") %>%
        hc_title(text = texto, align = "center",
                 useHTML = TRUE,
                 style = list(fontWeight = "bold", fontSize = "16px")) %>%
        hc_xAxis(visible = FALSE) %>%
        hc_yAxis(visible = FALSE) %>%
        hc_plotOptions(series = list(enableMouseTracking = FALSE, showInLegend = FALSE))
    }

    # ============================================
    # 1. FILTROS: vienen del modulo global (mod_filtros.R)
    # ============================================
    filtros_evento <- filtros$evento
    output$excel_detallado_ui <- renderUI({
      req(id_componente(), id_pilar(), id_dimension())
      
      if (id_componente() == 1 && id_pilar() == 1 && id_dimension() == 1) {
        descargar_excel_ui(session$ns("excel_detallado"))
      } else {
        NULL
      }
    })
    
    observe({
      req(id_componente(), id_pilar(), id_dimension())
      
      if (id_componente() == 1 && id_pilar() == 1 && id_dimension() == 1) {
        # Crear reactive con periodos seleccionados
        periodos_seleccionados <- reactive({
          est <- filtros$estado()
          req(est$filtro_anio, est$filtro_mes)
          
          anios <- est$filtro_anio
          meses <- est$filtro_mes
          
          # Generar combinaciones de año-mes en formato YYYY-MM
          periodos <- c()
          for (anio in anios) {
            for (mes in meses) {
              periodos <- c(periodos, paste0(anio, "-", mes))
            }
          }
          
          return(periodos)
        })
        
        # Reactive con las entidades permitidas segun nivel/sector/entidad de
        # los filtros comunes. NULL = sin restriccion (nivel "Distrito").
        entidades_seleccionadas <- reactive({
          est   <- filtros$estado()
          nivel <- est$filtro_nivel
          
          if (is.null(nivel) || nivel == "Distrito") return(NULL)
          
          if (nivel == "Sector") {
            req(est$sector_checks)
            ids_sector <- df_sectores %>% filter(Sector %in% est$sector_checks) %>% pull(Id_Sector)
            return(df_entidades %>% filter(Id_Sector %in% ids_sector) %>% pull(Id_Entidad))
          }
          
          if (nivel == "Entidad") {
            req(est$entidad_checks)
            return(df_entidades %>% filter(Entidad %in% est$entidad_checks) %>% pull(Id_Entidad))
          }
          
          NULL
        })
        
        descargar_excel_server("excel_detallado", periodos_seleccionados, entidades_seleccionadas)
      }
    })
    
    # ============================================
    # 2. HELPERS UI DINÁMICOS
    # (los de entidad/canal/subcanal ahora viven en mod_filtros.R;
    #  aqui solo queda la seleccion de indicadores, propia de esta vista)
    # ============================================

    observeEvent(input$indicador_select_all, {
      indics <- indicadores_dimension()
      updateCheckboxGroupInput(session, "indicador_seleccionado", selected = as.character(indics$Id_Indicador))
    })
    observeEvent(input$indicador_deselect_all, {
      updateCheckboxGroupInput(session, "indicador_seleccionado", selected = character(0))
    })

    # ============================================
    # 5. FUENTE DE DATOS
    # ============================================
    
    datos_actuales <- reactive({
      req(signals$general())
      return(rv_bg)
    })
    
    indicadores_dimension <- reactive({
      req(id_componente(), id_pilar(), id_dimension())
      df_indicadores %>%
        filter(
          Id_Componente == id_componente(),
          Id_Pilar == id_pilar(),
          Id_Dimension == id_dimension(),
          Id_Indicador != 0
        ) %>%
        distinct(Id_Indicador, Indicador) %>%
        mutate(Id_Indicador = as.character(Id_Indicador)) %>%
        arrange(Id_Indicador)
    })
    
    ranking_data <- reactive({
      datos <- datos_actuales()
      req(datos$general)
      id_comp <- id_componente()
      id_pil <- id_pilar()
      id_dim <- id_dimension()
      if (is.null(id_comp) || is.null(id_pil) || is.null(id_dim)) return(NULL)
      ranking <- obtener_indicadores(datos$general, id_comp, id_pil, id_dim)
      if (is.null(ranking) || nrow(ranking) == 0) return(NULL)
      # Asegurar que el DF tenga Id_Indicador como carácter y columnas esperadas
      ranking <- ranking %>% mutate(Id_Indicador = as.character(Id_Indicador))
      ids_sel <- id_indicador_seleccionado()
      if (!is.null(ids_sel) && length(ids_sel) > 0) ranking <- ranking %>% filter(Id_Indicador %in% ids_sel)
      if (nrow(ranking) == 0) return(NULL)
      return(ranking)
    })
    
    output$indicador_selector_ui <- renderUI({
      indics <- indicadores_dimension()
      if (is.null(indics) || nrow(indics) == 0) return(p("No hay datos de indicadores..."))
      
      choices <- setNames(as.character(indics$Id_Indicador), indics$Indicador)
      seleccion_actual <- isolate(input$indicador_seleccionado)
      seleccion_filtrada <- if (is.null(seleccion_actual)) unname(choices) else seleccion_actual[seleccion_actual %in% choices]
      
      checkboxGroupInput(
        session$ns("indicador_seleccionado"),
        NULL,
        choices = choices,
        selected = seleccion_filtrada
      )
    })
    
    observeEvent(indicadores_dimension(), {
      indics <- indicadores_dimension()
      if (is.null(indics) || nrow(indics) == 0) return()
      if (is.null(input$indicador_seleccionado) || length(input$indicador_seleccionado) == 0) {
        updateCheckboxGroupInput(
          session,
          "indicador_seleccionado",
          selected = as.character(indics$Id_Indicador)
        )
      }
    }, ignoreInit = FALSE)
    
    observeEvent(list(id_componente(), id_pilar(), id_dimension()), {
      indics <- indicadores_dimension()
      if (is.null(indics) || nrow(indics) == 0) return()
      updateCheckboxGroupInput(
        session,
        "indicador_seleccionado",
        selected = as.character(indics$Id_Indicador)
      )
    }, ignoreInit = TRUE)
    
    id_indicador_seleccionado <- reactive({
      indics <- indicadores_dimension()
      if (is.null(indics) || nrow(indics) == 0) return(character(0))
      
      seleccion <- input$indicador_seleccionado
      if (is.null(seleccion)) return(character(0))
      
      seleccion_chr <- as.character(seleccion)
      seleccion_chr[seleccion_chr %in% as.character(indics$Id_Indicador)]
    })
    
    nombres_indicadores_seleccionados <- reactive({
      indics <- indicadores_dimension()
      ids_sel <- id_indicador_seleccionado()
      
      if (is.null(indics) || length(ids_sel) == 0) return(character(0))
      
      indics %>%
        filter(Id_Indicador %in% ids_sel) %>%
        arrange(Id_Indicador) %>%
        pull(Indicador)
    })
    
    # ============================================
    # 7. GRÁFICA - RANKING INDICADORES
    # ============================================
    
    output$plot_ranking_indicadores <- renderHighchart({
      datos    <- datos_actuales()
      id_comp  <- id_componente()
      id_pil   <- id_pilar()
      id_dim   <- id_dimension()
      ids_sel  <- id_indicador_seleccionado()
      
      if (is.null(ids_sel) || length(ids_sel) == 0) {
        return(placeholder_chart("Seleccione al menos un indicador"))
      }
      
      ev          <- filtros_evento()
      ver_detalle <- isTRUE(ev$filtro_canal_detalle)
      
      if (ver_detalle && !is.null(ev$filtro_canal_selector)) {
        # ── Modo subcanal: los indicadores no existen a nivel de subcanal,
        # se usan los datos del canal padre seleccionado ──────────────────
        signals$canal()
        canal_sel  <- ev$filtro_canal_selector
        datos_canal_sel = list(general = NULL)
        datos_canal_sel$general <- rv_bg$canal[[canal_sel]]
        
        if (is.null(datos_canal_sel)) {
          return(placeholder_chart("⚠ Los datos del canal aún no están disponibles. Presione 'Aplicar Filtros'."))
        }
        
        ranking <- tryCatch(
          obtener_indicadores(datos_canal_sel, id_comp, id_pil, id_dim),
          error = function(e) NULL
        )
        
        if (is.null(ranking) || nrow(ranking) == 0) {
          return(placeholder_chart(paste("Sin datos de indicadores para la selección actual de subcanal(es) del canal", canal_sel)))
        }
        
        ranking <- ranking %>%
          mutate(Id_Indicador = as.character(Id_Indicador)) %>%
          filter(Id_Indicador %in% ids_sel)
        
      } else {
        # ── Modo normal: datos generales, filtrar por canales si aplica ──
        ranking <- obtener_indicadores(datos, id_comp, id_pil, id_dim)
        if (is.null(ranking) || nrow(ranking) == 0) {
          return(placeholder_chart("No hay datos para el ranking de indicadores con la selección actual"))
        }
        ranking <- ranking %>%
          mutate(Id_Indicador = as.character(Id_Indicador)) %>%
          filter(Id_Indicador %in% ids_sel)
      }
      
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(placeholder_chart("No hay datos para el ranking de indicadores con la selección actual"))
      }
      
      # Redondear antes del hcaes para que el tooltip muestre "Valor" limpio
      es_calidad_pqrsd <- as.numeric(id_comp) == 1 &&
        as.numeric(id_pil) == 1 &&
        as.numeric(id_dim) == 1

      if (es_calidad_pqrsd) {
        ranking <- ranking %>%
          mutate(
            Valor = hud_round_pct(Valor),
            SinDatos = is.na(Valor),
            ValorGrafica = ifelse(SinDatos, 0, Valor),
            Color = ifelse(SinDatos, "#9E9E9E", hud_color_indicador(Valor))
          )

        return(
          hchart(ranking, "bar", hcaes(x = Indicador, y = ValorGrafica, color = Color)) %>%
            hc_plotOptions(bar = list(
              depth = 40,
              shape = "cylinder",
              dataLabels = list(
                enabled = TRUE,
                formatter = JS("function () { return this.point.SinDatos ? 'Sin datos' : Highcharts.numberFormat(this.y, 1) + '%'; }")
              ),
              colorByPoint = TRUE
            )) %>%
            hc_tooltip(
              formatter = JS("function () { return '<b>' + this.point.Indicador + '</b><br/>' + (this.point.SinDatos ? 'Sin datos para los filtros seleccionados' : Highcharts.numberFormat(this.y, 1) + '%'); }")
            ) %>%
            hc_title(text = "Ranking de Indicadores") %>%
            hc_legend(enabled = FALSE)
        )
      }

      ranking <- ranking %>%
        mutate(
          Valor = hud_round_pct(Valor),
          Color = hud_color_indicador(Valor)
        )
      
      hchart(ranking, "bar", hcaes(x = Indicador, y = Valor, color = Color)) %>%
        hc_plotOptions(bar = list(depth = 40, shape = 'cylinder',
                                  dataLabels = list(enabled = TRUE, format = '{y}%'),
                                  colorByPoint = TRUE)) %>%
        hc_title(text = "Ranking de Indicadores") %>%
        hc_legend(enabled = FALSE)
    })
    
    # ============================================
    # 8. GRÁFICA - DESEMPEÑO POR CANAL
    # ============================================
    
    output$plot_desempeno_canal <- renderHighchart({
      signals$canal()
      req(rv_bg$canal)
      
      ids_sel <- id_indicador_seleccionado()
      if (is.null(ids_sel) || length(ids_sel) == 0) {
        return(placeholder_chart("Seleccione al menos un indicador para visualizar el desempeño por Canal"))
      }
      
      # Usar snapshot confirmado al hacer clic en "Aplicar Filtros",
      # no inputs en vivo — los checkboxes no deben re-renderizar el gráfico.
      ev          <- filtros_evento()
      ver_detalle <- isTRUE(ev$filtro_canal_detalle)
      
      if (!ver_detalle) {
        datos <- obtener_valor_indicador_por_canal(lista_canales = rv_bg$canal, ids_sel)
        
        if (is.null(datos) || nrow(datos) == 0) {
          return(placeholder_chart("La información por Canal no está disponible/no aplica para este Indicador"))
        }
        
        # Filtrar por canales seleccionados (si hay selección parcial)
        canales_sel <- ev$canales_checks
        if (!is.null(canales_sel) && length(canales_sel) > 0) {
          datos <- datos %>% filter(Canal %in% canales_sel)
        }
        
        if (nrow(datos) == 0) {
          return(placeholder_chart("Ningún canal seleccionado tiene datos para este indicador"))
        }
        
        # Redondear antes del hcaes para que el tooltip muestre "Valor" limpio
        datos <- datos %>%
          mutate(
            Valor = hud_round_pct(Indice),
            Color = hud_color_indicador(Valor)
          )
        
        hchart(datos, "column",
               hcaes(x = Canal, y = Valor, color = Color)) %>%
          hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                       dataLabels = list(enabled = TRUE, format = '{y}%'),
                                       colorByPoint = TRUE)) %>%
          hc_title(text = "Desempeño por Canal") %>%
          hc_yAxis(min = 0, max = 100) %>%
          hc_legend(enabled = FALSE)
      } else {
        req(ev$filtro_canal_selector)
        req(ev$subcanales_checks)
        signals$subcanal()
        
        canal_sel      <- ev$filtro_canal_selector
        subcanales_sel <- ev$subcanales_checks
        
        if (is.null(rv_bg$subcanal) || is.null(rv_bg$subcanal[[canal_sel]])) {
          return(placeholder_chart("⚠ Los datos de subcanales aún no están disponibles. Presione 'Aplicar Filtros'."))
        }
        
        lista_subcanales <- rv_bg$subcanal[[canal_sel]]
        resultados <- data.frame(Subcanal = character(), Valor = numeric(), stringsAsFactors = FALSE)
        
        for (sub_nm in subcanales_sel) {
          if (!is.null(lista_subcanales[[sub_nm]])) {
            valor <- extraer_indicadores_subcanal(lista_subcanales[[sub_nm]], ids_sel)
            if (!is.na(valor)) {
              resultados <- rbind(resultados, data.frame(Subcanal = sub_nm,
                                                         Valor = round(valor, 1)))
            }
          }
        }
        
        if (nrow(resultados) == 0) {
          return(placeholder_chart("La información por Canal no está disponible/no aplica para este Indicador"))
        }
        
        resultados <- resultados %>%
          mutate(Color = hud_color_indicador(Valor))

        hchart(resultados, "column",
               hcaes(x = Subcanal, y = Valor, color = Color)) %>%
          hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                       dataLabels = list(enabled = TRUE, format = '{y}%'),
                                       colorByPoint = TRUE)) %>%
          hc_title(text = paste("Subcanales -", canal_sel)) %>%
          hc_yAxis(min = 0, max = 100) %>%
          hc_legend(enabled = FALSE)
      }
    })
    
    # ============================================
    # 9. GRÁFICA - EVOLUCIÓN HISTÓRICA
    # ============================================
    
    output$plot_desempeno_periodo <- renderHighchart({
      signals$historico()
      req(rv_bg$historico)
      
      ids_sel <- id_indicador_seleccionado()
      if (is.null(ids_sel) || length(ids_sel) == 0) {
        return(placeholder_chart("Seleccione al menos un indicador para visualizar el desempeño Histórico"))
      }
      
      datos <- calcular_evolucion_indicador_historica(rv_bg$historico, ids_sel)
      
      if (is.null(datos) || nrow(datos) == 0) {
        return(placeholder_chart("La información por Histórico no está disponible/no aplica para este Indicador"))
      }
      
      hchart(
        datos %>%
          mutate(
            Valor = hud_round_pct(Valor),
            Color = hud_color_indicador(Valor)
          ),
        "column",
        hcaes(x = Periodo, y = Valor, color = Color)
      ) %>%
        hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                     dataLabels = list(enabled = TRUE, format = '{y}%'),
                                     colorByPoint = TRUE)) %>%
        hc_title(text = "Evolución Histórica") %>%
        hc_subtitle(text = "Si el Periodo de Análisis es 'Todos', solo se muestran los 4 meses más recientes") %>%
        hc_xAxis(title = list(text = "Periodo")) %>%
        hc_yAxis(min = 0, max = 100) %>%
        hc_legend(enabled = FALSE)
    })
    
    # ============================================
    # 10. GENERACIÓN DE PDF - CORREGIDO
    # ============================================
    output$descargar_reporte <- downloadHandler(
      filename = function() {
        id_comp <- id_componente()
        id_pil <- id_pilar()
        id_dim <- id_dimension()
        
        # Nombres seguros para archivo (sin tildes ni espacios)
        componente_nombre <- if(id_comp == 1) "Prestacion" else "Satisfaccion"
        
        # Obtener nombres limpios del DF global
        pilar_nombre <- df_indicadores %>% 
          filter(Id_Componente == 0, Id_Pilar == id_pil, Id_Dimension == 0) %>% 
          pull(Pilar) %>% first()
        
        dimension_nombre <- df_indicadores %>% 
          filter(Id_Componente == 0, Id_Pilar == 0, Id_Dimension == id_dim) %>% 
          pull(Dimensión) %>% first()
        
        # Limpieza de strings para nombre de archivo
        clean_pilar <- gsub("[^[:alnum:]]", "_", pilar_nombre)
        clean_dim   <- gsub("[^[:alnum:]]", "_", dimension_nombre)
        
        paste0("Reporte_", componente_nombre, "_", 
               clean_pilar, "_",
               clean_dim, "_",
               format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # 1. Validación inicial
        ids_sel <- id_indicador_seleccionado()
        if (length(ids_sel) == 0) {
          showNotification("Seleccione un indicador para generar el reporte.", 
                           type = "error", duration = 5)
          return(NULL)
        }
        
        showNotification("Generando reporte PDF...", type = "message", id = "pdf_gen")
        
        tryCatch({
          datos <- datos_actuales()
          
          # 2. Preparar Nombres (Tema Visual: Mostrar texto real, no IDs)
          nombres_indicadores <- df_indicadores %>%
            filter(Id_Indicador %in% ids_sel) %>%
            pull(Indicador) %>%
            paste(collapse = " - ") # Unir con guión si hay múltiples
          
          # Corrección de encoding y nombre del componente
          nombre_componente_texto <- if(id_componente() == 1) "Calidad del Servicio Prestado" else "Satisfacción y Experiencia"
          
          # Obtener nombres de Pilar y Dimensión para el Título
          nombre_pilar_texto <- df_indicadores %>%
            filter(Id_Componente == 0, Id_Pilar == id_pilar(), Id_Dimension == 0) %>%
            pull(Pilar) %>% first()
          
          nombre_dimension_texto <- df_indicadores %>%
            filter(Id_Componente == 0, Id_Pilar == 0, Id_Dimension == id_dimension()) %>%
            pull(Dimensión) %>% first()
          
          # 3. Preparar Datos Ranking (Dimensión)
          ranking <- NULL
          if (!is.null(datos$general) && !is.null(datos$general$indicadores)) {
            ranking <- obtener_indicadores(datos, id_componente(), id_pilar(), id_dimension())
          }
          if (!is.null(ranking) && nrow(ranking) > 0) {
            if (length(ids_sel) > 0) {
              ranking <- ranking %>% filter(Id_Indicador %in% ids_sel)
            }
            # Normalizar para asegurar columnas `Indicador` y `Valor`
            if ("Indice" %in% names(ranking) && !("Valor" %in% names(ranking))) {
              ranking <- ranking %>% dplyr::rename(Valor = Indice)
            }
            if (!"Indicador" %in% names(ranking) && "Indicador" %in% names(ranking)) {
              # nothing
            }
            datos_dimension <- ranking %>%
              dplyr::filter(!is.na(Valor)) %>%
              dplyr::mutate(Valor = round(as.numeric(Valor), 1)) %>%
              dplyr::select(Indicador, Valor)
          } else {
            datos_dimension <- data.frame(Indicador = character(), Valor = numeric()) # DF Vacío seguro
          }
          
          # 4. Preparar Datos Filtrados (Canal/Subcanal/Periodo)
          estado_filtros <- isolate(filtros$estado())
          nivel          <- estado_filtros$filtro_nivel
          ver_detalle    <- isTRUE(estado_filtros$filtro_canal_detalle)
          canal_sel_pdf  <- if (ver_detalle) estado_filtros$filtro_canal_selector else NULL
          subcan_pdf     <- if (ver_detalle) estado_filtros$subcanales_checks else NULL
          canales_pdf    <- if (!ver_detalle) estado_filtros$canales_checks else NULL
          
          # A. Datos Canal / Subcanal
          datos_canal <- NULL
          try({
            if (ver_detalle && !is.null(canal_sel_pdf) &&
                !is.null(subcan_pdf) && length(subcan_pdf) > 0) {
              
              if (!is.null(rv_bg$subcanal[[canal_sel_pdf]])) {
                lista_sub  <- rv_bg$subcanal[[canal_sel_pdf]]
                resultados <- data.frame(Subcanal = character(), Valor = numeric(), stringsAsFactors = FALSE)
                
                for (sub_nm in subcan_pdf) {
                  if (!is.null(lista_sub[[sub_nm]])) {
                    val <- tryCatch(extraer_indicadores_subcanal(lista_sub[[sub_nm]], ids_sel), error = function(e) NA)
                    if (!is.na(val))
                      resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Valor = round(as.numeric(val), 1)))
                  }
                }
                # Mantener columna Subcanal (no renombrar a Canal)
                if (nrow(resultados) > 0) datos_canal <- resultados
              }
            } else if (!is.null(rv_bg$canal)) {
              datos_tmp <- obtener_valor_indicador_por_canal(lista_canales = rv_bg$canal, ids_sel)
              if (!is.null(datos_tmp) && nrow(datos_tmp) > 0) {
                if ("Indice" %in% names(datos_tmp) && !("Valor" %in% names(datos_tmp)))
                  datos_tmp <- datos_tmp %>% dplyr::rename(Valor = Indice)
                datos_tmp <- datos_tmp %>% dplyr::mutate(Valor = round(as.numeric(Valor), 1)) %>%
                  dplyr::select(Canal, Valor)
                # Filtrar por canales seleccionados si hay selección parcial
                if (!is.null(canales_pdf) && length(canales_pdf) > 0)
                  datos_tmp <- datos_tmp %>% filter(Canal %in% canales_pdf)
                if (nrow(datos_tmp) > 0) datos_canal <- datos_tmp
              }
            }
          }, silent = TRUE)
          
          # B. Datos Periodo
          datos_periodo <- NULL
          try({
            if (!is.null(rv_bg$historico)) {
              datos_tmp <- calcular_evolucion_indicador_historica(rv_bg$historico, ids_sel)
              if (!is.null(datos_tmp) && nrow(datos_tmp) > 0) {
                if ("Valor" %in% names(datos_tmp)) {
                  datos_periodo <- datos_tmp %>% dplyr::mutate(Valor = round(as.numeric(Valor), 1)) %>% dplyr::select(Periodo, Valor)
                } else if ("Indice" %in% names(datos_tmp)) {
                  datos_periodo <- datos_tmp %>% dplyr::rename(Valor = Indice) %>%
                    dplyr::mutate(Valor = round(as.numeric(Valor), 1)) %>% dplyr::select(Periodo, Valor)
                }
              }
            }
          }, silent = TRUE)
          
          # C. Unificación para el reporte — columnas Canal/Subcanal, Periodo, Valor
          datos_filtrados <- data.frame()
          if (!is.null(datos_canal) && nrow(datos_canal) > 0) {
            datos_filtrados <- dplyr::bind_rows(datos_filtrados, datos_canal %>% dplyr::mutate(Periodo = NA))
          }
          if (!is.null(datos_periodo) && nrow(datos_periodo) > 0) {
            datos_filtrados <- dplyr::bind_rows(datos_filtrados, datos_periodo %>% dplyr::mutate(Canal = NA))
          }
          if (nrow(datos_filtrados) > 0)
            datos_filtrados$Valor <- round(as.numeric(datos_filtrados$Valor), 1)
          
          # 5. Llamada al Generador
          temp_report <- generar_reporte_dimension(
            datos_dimension          = datos_dimension,
            datos_filtrados          = datos_filtrados,
            componente               = nombre_componente_texto,
            pilar_nombre             = nombre_pilar_texto,
            dimension_nombre         = nombre_dimension_texto,
            indicador_seleccionado   = nombres_indicadores,
            nivel_consulta           = estado_filtros$filtro_nivel,
            sectores_seleccionados   = if (!is.null(estado_filtros$sector_checks)) paste(estado_filtros$sector_checks, collapse = ", ") else "Todos",
            entidades_seleccionadas  = if (!is.null(estado_filtros$entidad_checks)) paste(estado_filtros$entidad_checks, collapse = ", ") else "Todas",
            canales_seleccionados    = canales_pdf,
            canal_seleccionado       = canal_sel_pdf,
            subcanales_seleccionados = subcan_pdf,
            detalle_canal            = ver_detalle
          )
          
          # 6. Renderizado
          rmarkdown::render(
            input = temp_report$template,
            output_file = basename(file),
            output_dir = dirname(file),
            params = temp_report$parametros,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          
          removeNotification("pdf_gen")
          showNotification("Reporte generado exitosamente.", type = "message")
          
        }, error = function(e) {
          removeNotification("pdf_gen")
          showNotification(paste("Error generando PDF:", e$message), type = "error", duration = 10)
        })
      }
    )
    
    invisible(NULL)
  })
}
