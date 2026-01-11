library(shinycssloaders)
library(scales)

# ============================================
# 1. PREVIEW DE PILAR (Ranking de dimensiones)
# ============================================

dimension_preview_ui <- function(id) {
  ns <- NS(id)
  
  # Estáticos
  all_sectores <- if(exists("df_sectores")) sort(df_sectores$Sector) else character(0)
  # Allowed years from 2025 to current year (keep in sync with periodos availability)
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  anios_fijos <- as.character(seq(2025, end_year))
  meses_fijos <- sprintf("%02d", 1:12)
  names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  div(
    class = "visualization-container",
    div(
      class = "filter-panel",
      
      # --- FILTRO DE FECHAS ---
      tags$h4("Periodo de Análisis", style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      fluidRow(
        column(6, pickerInput(ns("filtro_anio"), "Año", choices = anios_fijos, multiple = TRUE, 
                              options = list(`actions-box`=TRUE, `none-selected-text`="Todos"))),
        column(6, uiOutput(ns("mes_selector_ui")))
      ),
      actionButton(ns("btn_actualizar_fechas"), "Filtrar por Fechas", icon = icon("sync"), 
                   class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 15px;"),
      
      hr(),
      
      # --- NIVEL DE CONSULTA ---
      tags$h4("Nivel de Consulta", style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      radioButtons(ns("filtro_nivel"), NULL, choices = c("Distrito", "Sector", "Entidad"), selected = "Distrito"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Sector'", ns("filtro_nivel")),
        div(style="margin-bottom: 5px; font-size: 0.9em;", 
            actionLink(ns("sector_select_all"), "Todos"), " | ", 
            actionLink(ns("sector_deselect_all"), "Ninguno")),
        div(class="scrollable-checkbox-group", 
            checkboxGroupInput(ns("sector_checks"), NULL, choices = all_sectores, selected = all_sectores))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Entidad'", ns("filtro_nivel")),
        div(style="margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("entidad_select_all"), "Todos"), " | ",
            actionLink(ns("entidad_deselect_all"), "Ninguno")),
        div(class="scrollable-checkbox-group",
            uiOutput(ns("entidad_checks_ui")))
      ),
      
      actionButton(
        ns("btn_aplicar_nivel"),
        "Aplicar filtrado por nivel",
        icon = icon("filter"),
        class = "btn-primary btn-sm",
        style = "width: 100%; margin-bottom: 15px;"
      ),
      
    ),
    
    div(
      class = "plot-area",
      div(class = "plot-box plot-box-full",
          withSpinner(highchartOutput(ns("plot_ranking_dimensiones"), height = "500px"), 
                      type=4, color="#225495")
      )
    )
  )
}

dimension_preview_server <- function(id, id_componente, id_pilar, rv_bg, signals) {
  moduleServer(id, function(input, output, session) {
    
    # ============================================
    # 1. VALIDACIÓN Y COMUNICACIÓN DE FILTROS
    # ============================================
    
    # Dynamic month selector (per-year pickers)
    output$mes_selector_ui <- renderUI({
      req(input$filtro_anio)
      años <- input$filtro_anio
      meses_fijos <- sprintf("%02d", 1:12)
      names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

      if (length(años) == 0) return(NULL)
      if (length(años) == 1) {
        pickerInput(session$ns("filtro_mes"), "Mes",
                    choices = meses_fijos,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                    selected = meses_fijos)
      } else {
        tagList(lapply(años, function(a) {
          pickerInput(session$ns(paste0("filtro_mes_", a)), label = a,
                      choices = meses_fijos,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `none-selected-text`="Todos"),
                      selected = meses_fijos)
        }))
      }
    })
    
    # Retorno de filtros para el server principal
    filtros_estado <- reactive({
      list(
        # Filtros de fecha (valores actuales)
        btn_actualizar_fechas = input$btn_actualizar_fechas,
        filtro_anio = input$filtro_anio,
        filtro_mes = input$filtro_mes,
        # Filtro de nivel
        filtro_nivel = input$filtro_nivel,
        sector_checks = input$sector_checks,
        entidad_checks = input$entidad_checks,
        # Filtros de canal
        filtro_canal_detalle = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks = input$subcanales_checks
      )
    })
    
    btn_actualizar_click <- reactiveVal(0)
    observeEvent(input$btn_actualizar_fechas, {
      btn_actualizar_click(btn_actualizar_click() + 1)
    })
    
    filtros_evento <- eventReactive(input$btn_actualizar_fechas, {
      años_sel <- input$filtro_anio
      filtro_periodos <- character(0)
      if (!is.null(años_sel) && length(años_sel) > 0) {
        if (length(años_sel) == 1) {
          sel <- input$filtro_mes
          if (is.null(sel) || length(sel) == 0) sel <- sprintf("%02d", 1:12)
          filtro_periodos <- paste0(años_sel, "-", sprintf("%02d", as.integer(sel)))
        } else {
          meses_comb <- unlist(lapply(años_sel, function(a) {
            sel <- input[[paste0('filtro_mes_', a)]]
            if (is.null(sel) || length(sel) == 0) return(character(0))
            paste0(a, "-", sprintf("%02d", as.integer(sel)))
          }))
          filtro_periodos <- meses_comb
        }
      } else {
        filtro_periodos <- NULL
      }

      list(
        filtro_anio = input$filtro_anio,
        filtro_mes = filtro_periodos,
        filtro_nivel = input$filtro_nivel,
        sector_checks = input$sector_checks,
        entidad_checks = input$entidad_checks,
        filtro_canal_detalle = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks = input$subcanales_checks
      )
    }, ignoreNULL = FALSE)
    
    btn_aplicar_nivel_click <- reactiveVal(0)
    observeEvent(input$btn_aplicar_nivel, {
      btn_aplicar_nivel_click(btn_aplicar_nivel_click() + 1)
    })
    
    # ============================================
    # 2. HELPERS UI DINÁMICOS
    # ============================================
    
    entidades_disponibles <- reactive({
      req(exists("df_entidades"))
      if (input$filtro_nivel == "Entidad" && length(input$sector_checks) > 0) {
        ids <- df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector)
        df_entidades %>% filter(Id_Sector %in% ids) %>% pull(Entidad) %>% sort()
      } else {
        sort(df_entidades$Entidad)
      }
    })
    
    output$entidad_checks_ui <- renderUI({
      ent <- entidades_disponibles()
      seleccion_actual <- isolate(input$entidad_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) ent else seleccion_actual[seleccion_actual %in% ent]
      checkboxGroupInput(session$ns("entidad_checks"), NULL, choices = ent, selected = seleccion_filtrada)
    })
    
    # Botones de selección rápida
    observeEvent(input$sector_select_all, { 
      updateCheckboxGroupInput(session, "sector_checks", selected = sort(df_sectores$Sector)) 
    })
    observeEvent(input$sector_deselect_all, { 
      updateCheckboxGroupInput(session, "sector_checks", selected = character(0)) 
    })
    observeEvent(input$entidad_select_all, {
      updateCheckboxGroupInput(session, "entidad_checks", selected = entidades_disponibles())
    })
    observeEvent(input$entidad_deselect_all, {
      updateCheckboxGroupInput(session, "entidad_checks", selected = character(0))
    })
    # ============================================
    # 3. IDS SELECCIONADOS
    # ============================================
    
    ids_seleccionados <- reactive({
      nivel <- input$filtro_nivel
      if (nivel == "Distrito") return(NULL)
      if (nivel == "Sector") {
        req(input$sector_checks)
        return(df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector))
      }
      if (nivel == "Entidad") {
        req(input$entidad_checks)
        return(df_entidades %>% filter(Entidad %in% input$entidad_checks) %>% pull(Id_Entidad))
      }
    })
    
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
      
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(highchart() %>% hc_title(text = "Sin datos de dimensiones"))
      }
      
      ranking_coloreado <- ranking %>%
        mutate(
          Indice = round(Valor, 1),
          Color = case_when(
            Indice >= 98 ~ "#8CBE23",
            Indice >= 90 ~ "#F9D248",
            TRUE ~ "#E3272A"
          )
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
    
    return(list(
      estado = filtros_estado,
      evento = filtros_evento,
      btn_click = btn_actualizar_click,
      nivel_click = btn_aplicar_nivel_click
    ))
  })
}

# ============================================
# 2. VISTA DE DIMENSIÓN (Indicadores + Detalle)
# ============================================
dimension_view_ui <- function(id) {
  ns <- NS(id)
  
  all_sectores <- if(exists("df_sectores")) sort(df_sectores$Sector) else character(0)
  all_canales <- if (exists("df_canales_lista")) df_canales_lista$Canal else obtener_canales()
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  anios_fijos <- as.character(seq(2025, end_year))
  meses_fijos <- sprintf("%02d", 1:12)
  names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  div(
    class = "visualization-container",
    div(
      class = "filter-panel",
      
      # --- FILTRO DE FECHAS ---
      tags$h4("Periodo de Análisis", style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      fluidRow(
        column(6, pickerInput(ns("filtro_anio"), "Año", choices = anios_fijos, multiple = TRUE, 
                              options = list(`actions-box`=TRUE, `none-selected-text`="Todos"))),
        column(6, uiOutput(ns("mes_selector_ui")))
      ),
      actionButton(ns("btn_actualizar_fechas"), "Filtrar por Fechas", icon = icon("sync"), 
                   class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 15px;"),
      
      hr(),
      
      # --- NIVEL DE CONSULTA ---
      tags$h4("Nivel de Consulta", style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      radioButtons(ns("filtro_nivel"), NULL, choices = c("Distrito", "Sector", "Entidad"), selected = "Distrito"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Sector'", ns("filtro_nivel")),
        div(style="margin-bottom: 5px; font-size: 0.9em;", 
            actionLink(ns("sector_select_all"), "Todos"), " | ", 
            actionLink(ns("sector_deselect_all"), "Ninguno")),
        div(class="scrollable-checkbox-group", 
            checkboxGroupInput(ns("sector_checks"), NULL, choices = all_sectores, selected = all_sectores))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Entidad'", ns("filtro_nivel")),
        div(style="margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("entidad_select_all"), "Todos"), " | ",
            actionLink(ns("entidad_deselect_all"), "Ninguno")),
        div(class="scrollable-checkbox-group",
            uiOutput(ns("entidad_checks_ui")))
      ),
      
      actionButton(
        ns("btn_aplicar_nivel"),
        "Aplicar filtrado por nivel",
        icon = icon("filter"),
        class = "btn-primary btn-sm",
        style = "width: 100%; margin-bottom: 15px;"
      ),
      
      hr(),
      
      # --- FILTRO CANAL ---
      tags$h4("Detalle por Canal", style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      checkboxInput(ns("filtro_canal_detalle"), "Mostrar detalle por subcanales", value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("filtro_canal_detalle")),
        selectInput(ns("filtro_canal_selector"), "Seleccione el canal:",
                    choices = all_canales,
                    selected = if(length(all_canales)>0) all_canales[1] else NULL),
        div(style="margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("subcanal_select_all"), "Todos"), " | ",
            actionLink(ns("subcanal_deselect_all"), "Ninguno")),
        div(class="scrollable-checkbox-group",
            uiOutput(ns("subcanales_checks_ui")))
      ),
      
      div(br()),
      
      # --- SELECTOR DE INDICADOR ---
      tags$h4("Seleccione el Indicador", style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      div(style="margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("indicador_select_all"), "Todos"), " | ",
          actionLink(ns("indicador_deselect_all"), "Ninguno")),
      div(class = "scrollable-checkbox-group",
          uiOutput(ns("indicador_selector_ui"))),
      
      hr(),
      
      # --- ACCIONES ---
      downloadButton(ns("descargar_reporte"), "Imprimir PDF", style = "width: 100%; margin-top: 10px;"),
      
      # --- DESCARGA EXCEL DETALLADO (solo para Gestión de Respuestas) ---
      uiOutput(ns("excel_detallado_ui"))
    ),
    
    div(
      class = "plot-area",
      # Ranking Indicadores
      div(class = "plot-box plot-box-full",
          highchartOutput(ns("plot_ranking_indicadores"), height = "400px")),
      
      # Periodo
      div(class = "plot-box plot-box-full",
          withSpinner(highchartOutput(ns("plot_desempeno_periodo"), height = "400px"),
                      type=4, color="#225495")),
      
      # Canal
      div(class = "plot-box plot-box-full",
          withSpinner(highchartOutput(ns("plot_desempeno_canal"), height = "400px"),
                      type=4, color="#225495"))
    )
  )
}

dimension_view_server <- function(id, id_componente, id_pilar, id_dimension, rv_bg, signals) {
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
    # 1. VALIDACIÓN Y COMUNICACIÓN DE FILTROS
    # ============================================
    
    # Dynamic month selector (per-year pickers)
    output$mes_selector_ui <- renderUI({
      req(input$filtro_anio)
      años <- input$filtro_anio
      meses_fijos <- sprintf("%02d", 1:12)
      names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

      if (length(años) == 0) return(NULL)
      if (length(años) == 1) {
        pickerInput(session$ns("filtro_mes"), "Mes",
                    choices = meses_fijos,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                    selected = meses_fijos)
      } else {
        tagList(lapply(años, function(a) {
          pickerInput(session$ns(paste0("filtro_mes_", a)), label = a,
                      choices = meses_fijos,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `none-selected-text`="Todos"),
                      selected = meses_fijos)
        }))
      }
    })
    
    filtros_estado <- reactive({
      list(
        btn_actualizar_fechas = input$btn_actualizar_fechas,
        filtro_anio = input$filtro_anio,
        filtro_mes = input$filtro_mes,
        filtro_nivel = input$filtro_nivel,
        sector_checks = input$sector_checks,
        entidad_checks = input$entidad_checks,
        filtro_canal_detalle = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks = input$subcanales_checks
      )
    })
    
    btn_actualizar_click <- reactiveVal(0)
    observeEvent(input$btn_actualizar_fechas, {
      btn_actualizar_click(btn_actualizar_click() + 1)
    })
    
    filtros_evento <- eventReactive(input$btn_actualizar_fechas, {
      años_sel <- input$filtro_anio
      filtro_periodos <- character(0)
      if (!is.null(años_sel) && length(años_sel) > 0) {
        if (length(años_sel) == 1) {
          sel <- input$filtro_mes
          if (is.null(sel) || length(sel) == 0) sel <- sprintf("%02d", 1:12)
          filtro_periodos <- paste0(años_sel, "-", sprintf("%02d", as.integer(sel)))
        } else {
          meses_comb <- unlist(lapply(años_sel, function(a) {
            sel <- input[[paste0('filtro_mes_', a)]]
            if (is.null(sel) || length(sel) == 0) return(character(0))
            paste0(a, "-", sprintf("%02d", as.integer(sel)))
          }))
          filtro_periodos <- meses_comb
        }
      } else {
        filtro_periodos <- NULL
      }

      list(
        filtro_anio = input$filtro_anio,
        filtro_mes = filtro_periodos,
        filtro_nivel = input$filtro_nivel,
        sector_checks = input$sector_checks,
        entidad_checks = input$entidad_checks,
        filtro_canal_detalle = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks = input$subcanales_checks
      )
    }, ignoreNULL = FALSE)
    
    btn_aplicar_nivel_click <- reactiveVal(0)
    observeEvent(input$btn_aplicar_nivel, {
      btn_aplicar_nivel_click(btn_aplicar_nivel_click() + 1)
    })
    
    
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
          req(input$filtro_anio, input$filtro_mes)
          
          anios <- input$filtro_anio
          meses <- input$filtro_mes
          
          # Generar combinaciones de año-mes en formato YYYY-MM
          periodos <- c()
          for (anio in anios) {
            for (mes in meses) {
              periodos <- c(periodos, paste0(anio, "-", mes))
            }
          }
          
          return(periodos)
        })
        
        descargar_excel_server("excel_detallado", periodos_seleccionados)
      }
    })
    
    # ============================================
    # 2. HELPERS UI DINÁMICOS
    # ============================================
    
    entidades_disponibles <- reactive({
      req(exists("df_entidades"))
      if (input$filtro_nivel == "Entidad" && length(input$sector_checks) > 0) {
        ids <- df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector)
        df_entidades %>% filter(Id_Sector %in% ids) %>% pull(Entidad) %>% sort()
      } else {
        sort(df_entidades$Entidad)
      }
    })
    
    output$entidad_checks_ui <- renderUI({
      ent <- entidades_disponibles()
      seleccion_actual <- isolate(input$entidad_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) ent else seleccion_actual[seleccion_actual %in% ent]
      checkboxGroupInput(session$ns("entidad_checks"), NULL, choices = ent, selected = seleccion_filtrada)
    })
    
    observeEvent(input$sector_select_all, { 
      updateCheckboxGroupInput(session, "sector_checks", selected = sort(df_sectores$Sector)) 
    })
    observeEvent(input$sector_deselect_all, { 
      updateCheckboxGroupInput(session, "sector_checks", selected = character(0)) 
    })
    observeEvent(input$entidad_select_all, { 
      updateCheckboxGroupInput(session, "entidad_checks", selected = entidades_disponibles()) 
    })
    observeEvent(input$entidad_deselect_all, {
      updateCheckboxGroupInput(session, "entidad_checks", selected = character(0))
    })
    observeEvent(input$indicador_select_all, {
      indics <- indicadores_dimension()
      updateCheckboxGroupInput(session, "indicador_seleccionado", selected = as.character(indics$Id_Indicador))
    })
    observeEvent(input$indicador_deselect_all, {
      updateCheckboxGroupInput(session, "indicador_seleccionado", selected = character(0))
    })
    observeEvent(input$subcanal_select_all, {
      req(input$filtro_canal_selector)
      subs <- obtener_subcanales(input$filtro_canal_selector)
      updateCheckboxGroupInput(session, "subcanales_checks", selected = subs)
    })
    observeEvent(input$subcanal_deselect_all, { 
      updateCheckboxGroupInput(session, "subcanales_checks", selected = character(0)) 
    })
    
    # ============================================
    # 4. IDS SELECCIONADOS
    # ============================================
    
    ids_seleccionados <- reactive({
      nivel <- input$filtro_nivel
      if (nivel == "Distrito") return(NULL)
      if (nivel == "Sector") {
        req(input$sector_checks)
        return(df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector))
      }
      if (nivel == "Entidad") {
        req(input$entidad_checks)
        return(df_entidades %>% filter(Entidad %in% input$entidad_checks) %>% pull(Id_Entidad))
      }
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
    
    # Subcanales disponibles (espera señal de canal)
    output$subcanales_checks_ui <- renderUI({
      req(input$filtro_canal_selector)
      req(signals$canal())  # Esperar a que carguen los datos de canal
      
      subs <- tryCatch({
        obtener_subcanales(input$filtro_canal_selector)
      }, error = function(e) character(0))
      
      if(length(subs) == 0) return(div("No hay subcanales disponibles"))
      
      seleccion_actual <- isolate(input$subcanales_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) subs else seleccion_actual[seleccion_actual %in% subs]
      checkboxGroupInput(session$ns("subcanales_checks"), NULL, choices = subs, selected = seleccion_filtrada)
    })
    
    # ============================================
    # 7. GRÁFICA - RANKING INDICADORES
    # ============================================
    
    output$plot_ranking_indicadores <- renderHighchart({
      datos <- datos_actuales()
      id_comp <- id_componente()
      id_pil <- id_pilar()
      id_dim <- id_dimension()
      
      ranking <- obtener_indicadores(datos, id_comp, id_pil, id_dim)
      
      if (is.null(ranking)) {
        return(placeholder_chart("No hay datos para el ranking de indicadores con la selección actual"))
      }
      
      ids_sel <- id_indicador_seleccionado()
      if (!is.null(ids_sel) && length(ids_sel) > 0) {
        ranking <- ranking %>% filter(Id_Indicador %in% ids_sel)
      } else {
        ranking <- ranking[0, ]
      }
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(placeholder_chart("No hay datos para el ranking de indicadores con la selección actual"))
      }
      
      grafico <- hchart(ranking %>%
                          mutate(
                            Valor = round(as.numeric(Valor),1),
                            Color = dplyr::case_when(
                              Valor >= 98 ~ "#8CBE23",  # Verde
                              Valor >= 90 ~ "#F9D248",  # Amarillo
                              TRUE ~ "#E3272A"           # Rojo
                            )
                          ),
                        "bar", hcaes(x = Indicador, y = Valor, color = Color)) %>%
        hc_plotOptions(bar = list(depth = 40, shape = 'cylinder',
                                  dataLabels = list(enabled = TRUE, format = '{y}%'),
                                  colorByPoint = TRUE)) %>%
        hc_title(text = "Ranking de Indicadores") %>%
        hc_legend(enabled = FALSE)
      
      grafico
    })
    
    # ============================================
    # 8. GRÁFICA - DESEMPEÑO POR CANAL
    # ============================================
    
    output$plot_desempeno_canal <- renderHighchart({
      signals$canal()
      req(rv_bg$canal)
      
      ids_sel <<- id_indicador_seleccionado()
      if (is.null(ids_sel) || length(ids_sel) == 0) {
        return(placeholder_chart("Seleccione al menos un indicador para visualizar el desempeño por Canal"))
      }
      req(ids_sel)
      
      ver_detalle <- isTRUE(input$filtro_canal_detalle)
      
      if (!ver_detalle) {
        datos <- obtener_valor_indicador_por_canal(lista_canales = rv_bg$canal, ids_sel)
        
        if (is.null(datos) || nrow(datos) == 0) {
          return(placeholder_chart("La información por Canal no está disponible/no aplica para este Indicador"))
        }
        
        hchart(datos %>% mutate(Valor = round(Indice, 1)), "column",
               hcaes(x = Canal, y = Valor), color = color_azul) %>%
          hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                       dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
          hc_title(text = "Desempeño por Canal") %>%
          hc_yAxis(min = 0, max = 100) %>%
          hc_legend(enabled = FALSE)
      } else {
        req(input$filtro_canal_selector)
        req(input$subcanales_checks)
        signals$subcanal()
        
        canal_sel <- input$filtro_canal_selector
        subcanales_sel <- input$subcanales_checks
        
        if (is.null(rv_bg$subcanal) || is.null(rv_bg$subcanal[[canal_sel]])) {
          return(highchart() %>% hc_title(text = "Cargando subcanales..."))
        }
        
        lista_subcanales <<- rv_bg$subcanal[[canal_sel]]
        resultados <- data.frame(Subcanal = character(), Valor = numeric(), stringsAsFactors = FALSE)
        
        for (sub_nm in subcanales_sel) {
          if (!is.null(lista_subcanales[[sub_nm]])) {
            valor <- extraer_indicadores_subcanal(lista_subcanales[[sub_nm]], ids_sel)
            if (!is.na(valor)) {
              resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Valor = valor))
            }
          }
        }
        
        if (nrow(resultados) == 0) {
          return(placeholder_chart("La información por Canal no está disponible/no aplica para este Indicador"))
        }
        
        hchart(resultados %>% mutate(Valor = round(Valor, 1)), "column",
               hcaes(x = Subcanal, y = Valor, color = Valor)) %>%
          hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                       dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
          hc_colorAxis(stops = color_stops(n = 3, colors = c("#E3242A", "#F9D248", "#225495")),
                       min = 60, max = 100) %>%
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
      
      hchart(datos %>% mutate(Valor = round(Valor, 1)), "column", 
             hcaes(x = Periodo, y = Valor), color = color_azul) %>%
        hc_plotOptions(column = list(depth = 40, shape = 'cylinder',
                                     dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
        hc_title(text = "Evolución Histórica") %>%
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
          
          # 4. Preparar Datos Filtrados (Canal/Periodo)
          nivel <- input$filtro_nivel
          ids <- ids_seleccionados()
          
          # A. Datos Canal
          datos_canal <- NULL
          try({
            ver_detalle <- isTRUE(input$filtro_canal_detalle)
            
            if (ver_detalle && !is.null(input$filtro_canal_selector) &&
                !is.null(input$subcanales_checks) && length(input$subcanales_checks) > 0) {
              
              canal_sel <- input$filtro_canal_selector
              subcanales_sel <- input$subcanales_checks
              
              if (!is.null(rv_bg$subcanal[[canal_sel]])) {
                lista_sub <- rv_bg$subcanal[[canal_sel]]
                resultados <- data.frame(Subcanal = character(), Valor = numeric(), stringsAsFactors = FALSE)
                
                for (sub_nm in subcanales_sel) {
                  if (!is.null(lista_sub[[sub_nm]])) {
                    # Extraer valor promedio para los indicadores seleccionados (si la función lo soporta)
                    # Intentamos extraer un valor representativo
                    val <- tryCatch({ extraer_indicadores_subcanal(lista_sub[[sub_nm]], ids_sel) }, error = function(e) NA)
                    if (!is.na(val)) resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Valor = round(as.numeric(val),1)))
                  }
                }
                
                if (nrow(resultados) > 0) {
                  datos_canal <- resultados %>% dplyr::rename(Canal = Subcanal)
                }
              }
            } else {
              if (!is.null(rv_bg$canal)) {
                datos_tmp <- obtener_valor_indicador_por_canal(lista_canales = rv_bg$canal, ids_sel)
                if (!is.null(datos_tmp) && nrow(datos_tmp) > 0) {
                  # obtener_valor_indicador_por_canal suele devolver 'Canal' y 'Indice'
                  if ("Indice" %in% names(datos_tmp) && !("Valor" %in% names(datos_tmp))) {
                    datos_tmp <- datos_tmp %>% dplyr::rename(Valor = Indice)
                  }
                  datos_canal <- datos_tmp %>% dplyr::mutate(Valor = round(as.numeric(Valor),1)) %>% dplyr::select(Canal = Canal, Valor)
                }
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
                  datos_periodo <- datos_tmp %>% dplyr::mutate(Valor = round(as.numeric(Valor),1)) %>% dplyr::select(Periodo, Valor)
                } else if ("Indice" %in% names(datos_tmp)) {
                  datos_periodo <- datos_tmp %>% dplyr::rename(Valor = Indice) %>% dplyr::mutate(Valor = round(as.numeric(Valor),1)) %>% dplyr::select(Periodo, Valor)
                }
              }
            }
          }, silent = TRUE)
          
          # C. Unificación para el reporte: construir un df que contenga columnas Canal/Periodo y Valor
          datos_filtrados <- data.frame()
          
          if (!is.null(datos_canal) && nrow(datos_canal) > 0) {
            datos_filtrados <- dplyr::bind_rows(datos_filtrados, datos_canal %>% dplyr::mutate(Periodo = NA))
          }
          if (!is.null(datos_periodo) && nrow(datos_periodo) > 0) {
            datos_filtrados <- dplyr::bind_rows(datos_filtrados, datos_periodo %>% dplyr::rename(Periodo = Periodo) %>% dplyr::mutate(Canal = NA))
          }
          
          # Asegurar formato numérico final
          if(nrow(datos_filtrados) > 0) {
            datos_filtrados$Valor <- round(as.numeric(datos_filtrados$Valor), 1)
          }
          
          # 5. Llamada al Generador
          # NOTA: Asegúrate que 'generar_reporte_dimension' acepte estos parámetros corregidos
          temp_report <- generar_reporte_dimension(
            datos_dimension = datos_dimension,
            datos_filtrados = datos_filtrados,
            componente = nombre_componente_texto,       # Texto corregido (UTF-8)
            pilar_nombre = nombre_pilar_texto,          # Texto real
            dimension_nombre = nombre_dimension_texto,  # Texto real
            indicador_seleccionado = nombres_indicadores, # NOMBRE (String), no ID
            nivel_consulta = input$filtro_nivel,
            sectores_seleccionados = if(!is.null(input$sector_checks)) paste(input$sector_checks, collapse=", ") else "Todos",
            entidades_seleccionadas = if(!is.null(input$entidad_checks)) paste(input$entidad_checks, collapse=", ") else "Todas"
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
    
    return(list(
      estado = filtros_estado,
      evento = filtros_evento,
      btn_click = btn_actualizar_click,
      nivel_click = btn_aplicar_nivel_click
    ))
  })
}
