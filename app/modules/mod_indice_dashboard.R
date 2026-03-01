library(shinycssloaders)

# ============================================
# HELPER: Gráfica de error clara (reemplaza spinner infinito)
# ============================================
.error_chart <- function(msg = "No hay datos disponibles para los filtros seleccionados.") {
  highchart() %>%
    hc_title(
      text = paste0("\u26a0 ", msg),
      align = "center",
      style = list(color = "#555555", fontSize = "14px", fontWeight = "bold")
    ) %>%
    hc_xAxis(visible = FALSE) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_plotOptions(series = list(enableMouseTracking = FALSE, showInLegend = FALSE))
}

indice_dashboard_ui <- function(id, id_componente) {
  ns <- NS(id)
  
  all_sectores  <- sort(df_sectores$Sector)
  all_entidades <- sort(df_entidades$Entidad)
  all_canales   <- obtener_canales()
  
  end_year    <- as.integer(format(Sys.Date(), "%Y"))
  anios_fijos <- as.character(seq(2025, end_year))
  meses_fijos <- sprintf("%02d", 1:12)
  names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                          "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  div(
    class = "visualization-container",
    div(
      class = "filter-panel",
      
      # --- FILTRO DE FECHAS ---
      tags$h4("Periodo de Analisis",
              style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      fluidRow(
        column(6,
               pickerInput(ns("filtro_anio"), "Año",
                           choices = anios_fijos, multiple = TRUE,
                           options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"))
        ),
        column(6, uiOutput(ns("mes_selector_ui")))
      ),
      
      hr(),
      
      # --- NIVEL DE CONSULTA ---
      tags$h4("Nivel de Consulta",
              style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      radioButtons(ns("filtro_nivel"), NULL,
                   choices = c("Distrito", "Sector", "Entidad"),
                   selected = "Distrito"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Sector'", ns("filtro_nivel")),
        div(style = "margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("sector_select_all"), "Todos"), " | ",
            actionLink(ns("sector_deselect_all"), "Ninguno")),
        div(class = "scrollable-checkbox-group",
            checkboxGroupInput(ns("sector_checks"), NULL,
                               choices = all_sectores, selected = all_sectores))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Entidad'", ns("filtro_nivel")),
        div(style = "margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("entidad_select_all"), "Todos"), " | ",
            actionLink(ns("entidad_deselect_all"), "Ninguno")),
        div(class = "scrollable-checkbox-group",
            uiOutput(ns("entidad_checks_ui")))
      ),
      
      hr(),
      
      # --- FILTRO CANAL (ahora filtro global) ---
      tags$h4("Filtrar por Canal",
              style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      
      # Canales disponibles (solo visibles cuando NO hay detalle de subcanal)
      conditionalPanel(
        condition = sprintf("input['%s'] == false", ns("filtro_canal_detalle")),
        div(style = "margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("canal_select_all"),    "Todos"), " | ",
            actionLink(ns("canal_deselect_all"),  "Ninguno")),
        div(class = "scrollable-checkbox-group",
            uiOutput(ns("canales_checks_ui")))
      ),
      
      # Detalle por subcanal (opcional)
      checkboxInput(ns("filtro_canal_detalle"),
                    "Filtrar por subcanal (afecta todas las graficas)",
                    value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("filtro_canal_detalle")),
        selectInput(ns("filtro_canal_selector"), "Seleccione el canal:",
                    choices = all_canales,
                    selected = if (length(all_canales) > 0) all_canales[1] else NULL),
        div(style = "margin-bottom: 5px; font-size: 0.9em;",
            actionLink(ns("subcanal_select_all"), "Todos"), " | ",
            actionLink(ns("subcanal_deselect_all"), "Ninguno")),
        div(class = "scrollable-checkbox-group",
            uiOutput(ns("subcanales_checks_ui")))
      ),
      
      hr(),
      
      # --- BOTON UNIFICADO (reemplaza los dos anteriores) ---
      actionButton(ns("btn_aplicar_filtros"), "Aplicar Filtros",
                   icon = icon("filter"),
                   class = "btn-primary btn-sm",
                   style = "width: 100%; margin-bottom: 15px; font-weight: bold;"),
      
      hr(),
      downloadButton(ns("descargar_reporte"), "Imprimir PDF",
                     style = "width: 100%; margin-top: 10px;")
    ),
    
    # --- AREA DE GRAFICAS ---
    div(
      class = "plot-area plot-area-rows",
      div(
        class = "plot-row",
        div(id = ns("box_plot_gauge_global"),
            class = "plot-box plot-box-half plot-box-gauge center-when-alone",
            withSpinner(highchartOutput(ns("plot_gauge_global"), height = "260px"),
                        type = 4, color = "#225495")),
        uiOutput(ns("box_plot_bar_pilar_ui"))
      ),
      div(
        class = "plot-row",
        div(class = "plot-box plot-box-half",
            withSpinner(highchartOutput(ns("plot_bar_periodo"), height = "100%"),
                        type = 4, color = "#225495")),
        div(class = "plot-box plot-box-half",
            withSpinner(highchartOutput(ns("plot_bar_canal"), height = "100%"),
                        type = 4, color = "#225495"))
      ),
      div(
        class = "plot-row plot-row-full",
        uiOutput(ns("box_plot_ranking_entidades_ui"))
      )
    )
  )
}


indice_dashboard_server <- function(id, id_componente_reactive, rv_bg, signals) {
  moduleServer(id, function(input, output, session) {
    
    # ============================================
    # 0. COMUNICACION CON SERVER PRINCIPAL
    # ============================================
    
    output$mes_selector_ui <- renderUI({
      req(input$filtro_anio)
      anios <- input$filtro_anio
      meses_fijos <- sprintf("%02d", 1:12)
      names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
      if (length(anios) == 0) return(NULL)
      if (length(anios) == 1) {
        pickerInput(session$ns("filtro_mes"), "Mes",
                    choices = meses_fijos, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                    selected = meses_fijos)
      } else {
        tagList(lapply(anios, function(a) {
          pickerInput(session$ns(paste0("filtro_mes_", a)), label = a,
                      choices = meses_fijos, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                      selected = meses_fijos)
        }))
      }
    })
    
    filtros_estado <- reactive({
      list(
        btn_actualizar_fechas  = input$btn_aplicar_filtros,
        filtro_anio            = input$filtro_anio,
        filtro_mes             = input$filtro_mes,
        filtro_nivel           = input$filtro_nivel,
        sector_checks          = input$sector_checks,
        entidad_checks         = input$entidad_checks,
        canales_checks         = input$canales_checks,
        filtro_canal_detalle   = input$filtro_canal_detalle,
        filtro_canal_selector  = input$filtro_canal_selector,
        subcanales_checks      = input$subcanales_checks
      )
    })
    
    # Contador unificado - reemplaza btn_actualizar_click + btn_aplicar_nivel_click
    btn_click <- reactiveVal(0)
    observeEvent(input$btn_aplicar_filtros, {
      btn_click(btn_click() + 1)
    })
    
    # Snapshot de filtros al hacer clic - incluye subcanal_ids resueltos
    filtros_evento <- eventReactive(input$btn_aplicar_filtros, {
      anios_sel <- input$filtro_anio
      filtro_periodos <- character(0)
      
      if (!is.null(anios_sel) && length(anios_sel) > 0) {
        if (length(anios_sel) == 1) {
          sel <- input$filtro_mes
          if (is.null(sel) || length(sel) == 0) sel <- sprintf("%02d", 1:12)
          filtro_periodos <- paste0(anios_sel, "-", sprintf("%02d", as.integer(sel)))
        } else {
          meses_comb <- unlist(lapply(anios_sel, function(a) {
            sel <- input[[paste0("filtro_mes_", a)]]
            if (is.null(sel) || length(sel) == 0) return(character(0))
            paste0(a, "-", sprintf("%02d", as.integer(sel)))
          }))
          filtro_periodos <- meses_comb
        }
      } else {
        filtro_periodos <- NULL
      }
      
      # Resolver IDs numericos de subcanal desde los nombres seleccionados
      subcanal_ids <- NULL
      if (isTRUE(input$filtro_canal_detalle) &&
          !is.null(input$filtro_canal_selector) &&
          !is.null(input$subcanales_checks) &&
          length(input$subcanales_checks) > 0 &&
          exists("df_canales")) {
        subcanal_ids <- df_canales %>%
          filter(Canal == input$filtro_canal_selector,
                 Subcanal %in% input$subcanales_checks) %>%
          pull(Id_Subcanal)
      }
      
      # Resolver canales seleccionados (filtro de canal sin subcanal)
      # Si el detalle de subcanal está activo, canales_checks se ignora en favor de subcanal_ids
      canales_sel <- if (isTRUE(input$filtro_canal_detalle)) NULL else input$canales_checks
      
      list(
        filtro_anio           = input$filtro_anio,
        filtro_mes            = filtro_periodos,
        filtro_nivel          = input$filtro_nivel,
        sector_checks         = input$sector_checks,
        entidad_checks        = input$entidad_checks,
        canales_checks        = canales_sel,
        filtro_canal_detalle  = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks     = input$subcanales_checks,
        subcanal_ids          = subcanal_ids
      )
    }, ignoreNULL = FALSE)
    
    # ============================================
    # 1. HELPERS UI DINAMICOS
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
      checkboxGroupInput(session$ns("entidad_checks"), NULL, choices = ent, selected = ent)
    })
    
    output$canales_checks_ui <- renderUI({
      req(signals$canal())
      canales <- tryCatch(obtener_canales(), error = function(e) character(0))
      if (length(canales) == 0) return(div("No hay canales disponibles"))
      seleccion_actual   <- isolate(input$canales_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) canales else seleccion_actual[seleccion_actual %in% canales]
      checkboxGroupInput(session$ns("canales_checks"), NULL, choices = canales, selected = seleccion_filtrada)
    })
    
    output$subcanales_checks_ui <- renderUI({
      req(input$filtro_canal_selector)
      req(signals$canal())
      subs <- tryCatch(obtener_subcanales(input$filtro_canal_selector), error = function(e) character(0))
      if (length(subs) == 0) return(div("No hay subcanales disponibles"))
      seleccion_actual  <- isolate(input$subcanales_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) subs else seleccion_actual[seleccion_actual %in% subs]
      checkboxGroupInput(session$ns("subcanales_checks"), NULL, choices = subs, selected = seleccion_filtrada)
    })
    
    # ============================================
    # 2. BOTONES DE SELECCION RAPIDA
    # ============================================
    
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
    observeEvent(input$subcanal_select_all, {
      req(input$filtro_canal_selector)
      subs <- obtener_subcanales(input$filtro_canal_selector)
      updateCheckboxGroupInput(session, "subcanales_checks", selected = subs)
    })
    observeEvent(input$subcanal_deselect_all, {
      updateCheckboxGroupInput(session, "subcanales_checks", selected = character(0))
    })
    observeEvent(input$canal_select_all, {
      canales <- tryCatch(obtener_canales(), error = function(e) character(0))
      updateCheckboxGroupInput(session, "canales_checks", selected = canales)
    })
    observeEvent(input$canal_deselect_all, {
      updateCheckboxGroupInput(session, "canales_checks", selected = character(0))
    })
    
    # ============================================
    # 3. IDS SELECCIONADOS
    # ============================================
    
    ids_seleccionados <- reactive({
      nivel <- input$filtro_nivel
      if (nivel == "Distrito") return(unique(df_entidades$Id_Entidad))
      if (nivel == "Sector") {
        req(input$sector_checks)
        todos_sectores <- sort(df_sectores$Sector)
        if (length(input$sector_checks) == length(todos_sectores) &&
            all(sort(input$sector_checks) == todos_sectores))
          return(unique(df_entidades$Id_Sector))
        return(df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector))
      }
      if (nivel == "Entidad") {
        req(input$entidad_checks)
        return(df_entidades %>% filter(Entidad %in% input$entidad_checks) %>% pull(Id_Entidad))
      }
    })
    
    # ============================================
    # 4. RESET AL CAMBIAR COMPONENTE
    # ============================================
    
    observeEvent(id_componente_reactive(), {
      req(exists("periodos"), exists("df_sectores"), exists("df_entidades"))
      anios_fijos <- as.character(sort(unique(periodos$Anio)))
      meses_fijos <- sprintf("%02d", 1:12)
      updatePickerInput(session, "filtro_anio", selected = anios_fijos)
      updatePickerInput(session, "filtro_mes", selected = meses_fijos)
      updateRadioButtons(session, "filtro_nivel", selected = "Distrito")
      updateCheckboxGroupInput(session, "sector_checks", selected = sort(df_sectores$Sector))
      updateCheckboxGroupInput(session, "entidad_checks", selected = sort(df_entidades$Entidad))
      updateCheckboxInput(session, "filtro_canal_detalle", value = FALSE)
      canales_disponibles <- obtener_canales()
      if (length(canales_disponibles) > 0) {
        updateSelectInput(session, "filtro_canal_selector", selected = canales_disponibles[1])
        subs <- obtener_subcanales(canales_disponibles[1])
        updateCheckboxGroupInput(session, "subcanales_checks", selected = subs)
      }
      session$sendCustomMessage("removeClass",
                                list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
    }, ignoreInit = TRUE)
    
    # ============================================
    # 5. FUENTES DE DATOS (una reactiva por senal para carga independiente)
    # ============================================
    
    datos_general <- reactive({
      req(signals$general())
      rv_bg
    })
    
    datos_historico <- reactive({
      signals$historico()
      rv_bg
    })
    
    datos_canal_rv <- reactive({
      signals$canal()
      rv_bg
    })
    
    # ============================================
    # 6. GRAFICA - GAUGE GLOBAL
    # ============================================
    
    output$plot_gauge_global <- renderHighchart({
      tryCatch({
        datos <- datos_general()
        req(datos$general)
        id_comp       <- id_componente_reactive()
        indice_global <- obtener_indice_componente(datos$general, id_comp)
        
        if (is.na(indice_global))
          return(.error_chart("No hay datos para el indice global."))
        
        indice_global <- round(indice_global, 1)
        nivel_texto   <- if (indice_global < 90) "Critico" else if (indice_global < 97) "Aceptable" else "Optimo"
        color_nivel   <- if (indice_global < 90) "#E3272A" else if (indice_global < 97) "#F9D248" else "#8CBE23"
        sub_text <- paste0("<span style='font-size:16px;'>El indice es <b style='color:black;'>",
                           indice_global, "%</b> y es <b style='color:", color_nivel, ";'>",
                           nivel_texto, "</b></span>")
        
        highchart() %>%
          hc_chart(type = "gauge") %>%
          hc_pane(startAngle = -150, endAngle = 150, size = "110%") %>%
          hc_yAxis(min = 0, max = 100,
                   plotBands = list(
                     list(from = 0, to = 90, color = "#E3272A"),
                     list(from = 90, to = 97, color = "#F9D248"),
                     list(from = 97, to = 100, color = "#8CBE23")
                   )) %>%
          hc_add_series(name = "Indice", data = list(indice_global),
                        dataLabels = list(enabled = TRUE, format = "{y}",
                                          style = list(fontSize = "24px"),
                                          borderWidth = 0, y = 40)) %>%
          hc_title(text = "Indice Global",
                   style = list(fontWeight = "bold", fontSize = "16px")) %>%
          hc_subtitle(useHTML = TRUE, text = sub_text, align = "center",
                      verticalAlign = "bottom", y = 30)
      }, error = function(e) .error_chart(paste("Error al cargar el indice global:", e$message)))
    })
    
    # ============================================
    # 7. GRAFICA - BARRAS POR PILAR
    # ============================================
    
    output$plot_bar_pilar <- renderHighchart({
      tryCatch({
        datos <- datos_general()
        req(datos$general)
        id_comp      <- id_componente_reactive()
        pilares_data <- obtener_indices_pilares(datos$general, id_comp)
        
        if (is.null(pilares_data) || nrow(pilares_data) == 0)
          return(.error_chart("No hay datos de pilares disponibles."))
        
        hchart(pilares_data %>% mutate(Valor = round(as.numeric(Valor), 1)),
               "bar", hcaes(x = Pilar, y = Valor, color = Valor)) %>%
          hc_plotOptions(bar = list(depth = 40, shape = "cylinder",
                                    dataLabels = list(enabled = TRUE, format = "{y}%"))) %>%
          hc_colorAxis(stops = color_stops(n = 3, colors = c("#E3272A", "#F9D248", "#8CBE23")),
                       min = 85, max = 100) %>%
          hc_title(text = "Indice por Pilar") %>%
          hc_legend(enabled = FALSE)
      }, error = function(e) .error_chart(paste("Error al cargar pilares:", e$message)))
    })
    
    output$box_plot_bar_pilar_ui <- renderUI({
      datos <- datos_general()
      req(datos$general)
      id_comp      <- id_componente_reactive()
      pilares_data <- tryCatch(obtener_indices_pilares(datos$general, id_comp), error = function(e) NULL)
      
      if (is.null(pilares_data)) {
        session$sendCustomMessage("addClass",
                                  list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
        return(NULL)
      }
      
      session$sendCustomMessage("removeClass",
                                list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
      div(
        id = session$ns("box_plot_bar_pilar"),
        class = "plot-box plot-box-half plot-box-pilar",
        withSpinner(highchartOutput(session$ns("plot_bar_pilar"), height = "260px"),
                    type = 4, color = "#225495")
      )
    })
    
    # ============================================
    # 8. GRAFICA - RANKING ENTIDADES
    # ============================================
    
    ranking_entidades_filtrado <- reactive({
      req(rv_bg$ranking_entidades)
      ranking <- obtener_ranking_entidades(rv_bg, id_componente_reactive())
      if (is.null(ranking) || nrow(ranking) == 0) return(NULL)
      
      if (input$filtro_nivel == "Sector") {
        ids_sectores <- ids_seleccionados()
        if (!is.null(ids_sectores) && length(ids_sectores) > 0) {
          ids_entidades <- tryCatch(
            df_entidades %>% filter(Id_Sector %in% ids_sectores) %>% pull(Id_Entidad),
            error = function(e) NULL
          )
          if (!is.null(ids_entidades) && length(ids_entidades) > 0)
            ranking <- ranking %>% filter(Id_Entidad %in% ids_entidades)
        }
      } else if (input$filtro_nivel == "Entidad") {
        ids_entidades <- ids_seleccionados()
        if (!is.null(ids_entidades) && length(ids_entidades) > 0) {
          ents <- tryCatch(
            df_entidades %>% filter(Id_Entidad %in% ids_entidades) %>% pull(Entidad),
            error = function(e) NULL
          )
          if (!is.null(ents) && length(ents) > 0)
            ranking <- ranking %>% filter(Entidad %in% ents)
        }
      }
      
      if (is.null(ranking) || nrow(ranking) == 0) return(NULL)
      ranking
    })
    
    output$box_plot_ranking_entidades_ui <- renderUI({
      div(class = "plot-box plot-box-full plot-box-ranking",
          highchartOutput(session$ns("plot_ranking_entidades"), height = "460px"))
    })
    
    output$plot_ranking_entidades <- renderHighchart({
      tryCatch({
        ranking <- ranking_entidades_filtrado()
        if (is.null(ranking) || nrow(ranking) == 0)
          return(.error_chart("No hay datos de ranking para el periodo seleccionado."))
        
        ranking_coloreado <- ranking %>%
          mutate(
            Indice = as.numeric(round(Valor, 1)),
            Color = case_when(
              Indice >= 98 ~ "#8CBE23",
              Indice >= 90 ~ "#F9D248",
              TRUE ~ "#E3272A"
            )
          )
        
        hchart(ranking_coloreado, "bar", hcaes(x = Entidad, y = Indice, color = Color)) %>%
          hc_plotOptions(bar = list(depth = 40, shape = "cylinder",
                                    dataLabels = list(enabled = TRUE, format = "{y}%"),
                                    colorByPoint = TRUE)) %>%
          hc_title(text = "Ranking de Entidades") %>%
          hc_legend(enabled = FALSE)
      }, error = function(e) .error_chart(paste("Error en ranking:", e$message)))
    })
    
    # ============================================
    # 9. GRAFICA - EVOLUCION HISTORICA
    # ============================================
    
    output$plot_bar_periodo <- renderHighchart({
      tryCatch({
        datos_struct <- datos_historico()
        req(datos_struct$historico)
        datos <- obtener_indice_por_periodo(datos_struct$historico, id_componente_reactive())
        
        if (is.null(datos) || nrow(datos) == 0)
          return(.error_chart("Sin historico disponible para los filtros seleccionados."))
        
        hchart(datos %>% mutate(Indice = round(as.numeric(Indice), 1)),
               "column", hcaes(x = Periodo, y = Indice), color = "#225495") %>%
          hc_plotOptions(column = list(depth = 40, shape = "cylinder",
                                       dataLabels = list(enabled = TRUE, format = "{y}%"))) %>%
          hc_title(text = "Evolución Histórica") %>%
          hc_xAxis(title = list(text = "Periodo")) %>%
          hc_yAxis(min = 0, max = 100) %>%
          hc_legend(enabled = FALSE)
      }, error = function(e) .error_chart(paste("Error en historico:", e$message)))
    })
    
    # ============================================
    # 10. GRAFICA - DESEMPENO POR CANAL
    # Nota: cuando subcanal es filtro global, esta grafica muestra
    # el indice por canal YA filtrado por subcanal en los datos base.
    # El modo "detalle" muestra desglose visual por subcanal.
    # ============================================
    
    output$plot_bar_canal <- renderHighchart({
      tryCatch({
        datos_struct <- datos_canal_rv()
        req(datos_struct$canal)
        
        id_comp <- id_componente_reactive()
        
        ev          <- filtros_evento()
        ver_detalle <- isTRUE(ev$filtro_canal_detalle)
        
        if (!ver_detalle) {
          datos_graf  <- obtener_indices_por_canal(datos_struct$canal, id_comp)
          
          # Filtrar por canales seleccionados (si hay selección parcial)
          canales_sel <- ev$canales_checks
          if (!is.null(canales_sel) && length(canales_sel) > 0) {
            datos_graf <- datos_graf %>% filter(Canal %in% canales_sel)
          }
          
          if (is.null(datos_graf) || nrow(datos_graf) == 0)
            return(.error_chart("Sin datos de canal para la selección actual."))
          
          # Redondear antes del hcaes para que el tooltip muestre "Indice" limpio
          datos_graf <- datos_graf %>% mutate(Indice = round(as.numeric(Indice), 1))
          
          titulo_graf <- "Desempeño por Canal"
          columna_cat <- "Canal"
        } else {
          req(ev$filtro_canal_selector)
          req(ev$subcanales_checks)
          req(signals$subcanal())
          
          canal_sel      <- ev$filtro_canal_selector
          subcanales_sel <- ev$subcanales_checks
          lista_sub      <- datos_struct$subcanal[[canal_sel]]
          
          resultados <- data.frame(Subcanal = character(), Indice = numeric(),
                                   stringsAsFactors = FALSE)
          for (sub_nm in subcanales_sel) {
            if (!is.null(lista_sub[[sub_nm]])) {
              valor <- obtener_indice_componente(lista_sub[[sub_nm]], id_comp)
              if (!is.na(valor))
                resultados <- rbind(resultados, data.frame(Subcanal = sub_nm,
                                                           Indice = round(valor, 1)))
            }
          }
          
          if (nrow(resultados) == 0)
            return(.error_chart("Sin datos de canal disponibles."))
          
          datos_graf  <- resultados %>% arrange(desc(Indice))
          titulo_graf <- paste("Subcanales -", canal_sel)
          columna_cat <- "Subcanal"
        }
        
        if (is.null(datos_graf) || nrow(datos_graf) == 0)
          return(.error_chart("Sin datos de canal disponibles."))
        
        hchart(datos_graf, "column",
               hcaes(x = !!sym(columna_cat), y = Indice, color = Indice)) %>%
          hc_plotOptions(column = list(depth = 40, shape = "cylinder",
                                       dataLabels = list(enabled = TRUE, format = "{y}%"))) %>%
          hc_colorAxis(stops = color_stops(n = 3, colors = c("#E3242A", "#F9D248", "#225495")),
                       min = 60, max = 100) %>%
          hc_title(text = titulo_graf) %>%
          hc_legend(enabled = FALSE)
      }, error = function(e) .error_chart(paste("Error en grafica de canal:", e$message)))
    })
    
    # ============================================
    # 11. REPORTE PDF
    # ============================================
    
    output$descargar_reporte <- downloadHandler(
      filename = function() { paste0("reporte_indice_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content = function(file) {
        showNotification("Generando reporte...", type = "message", duration = NULL, id = "pdf_progress")
        tryCatch({
          req(signals$general())
          datos   <- rv_bg
          id_comp <- id_componente_reactive()
          
          indice_global <- tryCatch(obtener_indice_componente(datos$general, id_comp), error = function(e) NA)
          pilares_data  <- tryCatch(obtener_indices_pilares(datos$general, id_comp), error = function(e) NULL)
          periodo_data  <- tryCatch(
            if (!is.null(datos$historico)) obtener_indice_por_periodo(datos$historico, id_comp) else NULL,
            error = function(e) NULL)
          datos_entidad <- tryCatch(obtener_ranking_entidades(datos, id_comp), error = function(e) NULL)
          
          nivel          <- input$filtro_nivel
          sectores_sel   <- if (nivel %in% c("Sector", "Entidad")) input$sector_checks else NULL
          entidades_sel  <- if (nivel == "Entidad") input$entidad_checks else NULL
          ver_detalle    <- isTRUE(input$filtro_canal_detalle)
          canal_sel      <- if (ver_detalle) input$filtro_canal_selector else NULL
          subcanales_sel <- if (ver_detalle) input$subcanales_checks else NULL
          canales_sel    <- if (!ver_detalle) input$canales_checks else NULL
          
          # Construir datos_canal_pdf según el modo activo
          datos_canal_pdf <- tryCatch({
            if (ver_detalle && !is.null(canal_sel) && !is.null(subcanales_sel) &&
                length(subcanales_sel) > 0 && !is.null(rv_bg$subcanal[[canal_sel]])) {
              # Modo subcanal: extraer índice por subcanal
              lista_sub  <- rv_bg$subcanal[[canal_sel]]
              resultados <- data.frame(Subcanal = character(), Indice = numeric(), stringsAsFactors = FALSE)
              for (sub_nm in subcanales_sel) {
                if (!is.null(lista_sub[[sub_nm]])) {
                  valor <- tryCatch(obtener_indice_componente(lista_sub[[sub_nm]], id_comp), error = function(e) NA)
                  if (!is.na(valor))
                    resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Indice = round(valor, 1)))
                }
              }
              if (nrow(resultados) > 0) resultados else NULL
            } else {
              # Modo canal: obtener índice por canal y filtrar si hay selección
              tmp <- if (!is.null(datos$canal)) obtener_indices_por_canal(datos$canal, id_comp) else NULL
              if (!is.null(tmp) && nrow(tmp) > 0) {
                if ("Valor" %in% names(tmp) && !("Indice" %in% names(tmp)))
                  tmp <- tmp %>% dplyr::rename(Indice = Valor)
                tmp$Indice <- round(as.numeric(tmp$Indice), 1)
                # Filtrar por canales seleccionados si hay selección parcial
                if (!is.null(canales_sel) && length(canales_sel) > 0)
                  tmp <- tmp %>% filter(Canal %in% canales_sel)
                if (nrow(tmp) > 0) tmp else NULL
              } else NULL
            }
          }, error = function(e) NULL)
          
          if (!is.null(pilares_data) && nrow(pilares_data) > 0) {
            if ("Valor" %in% names(pilares_data) && !("Indice" %in% names(pilares_data)))
              pilares_data <- pilares_data %>% dplyr::rename(Indice = Valor)
            if ("Indice" %in% names(pilares_data))
              pilares_data$Indice <- as.numeric(pilares_data$Indice)
          }
          
          if (!is.null(datos_entidad) && nrow(datos_entidad) > 0) {
            if ("Entidad" %in% names(datos_entidad) && !("Nombre_Entidad" %in% names(datos_entidad)))
              datos_entidad <- datos_entidad %>% dplyr::rename(Nombre_Entidad = Entidad)
            if ("Valor" %in% names(datos_entidad) && !("Indice" %in% names(datos_entidad)))
              datos_entidad <- datos_entidad %>% dplyr::rename(Indice = Valor)
            if ("Indice" %in% names(datos_entidad))
              datos_entidad$Indice <- as.numeric(datos_entidad$Indice)
            if (nivel == "Sector" && !is.null(sectores_sel) && length(sectores_sel) > 0) {
              ids <- df_sectores %>% filter(Sector %in% sectores_sel) %>% pull(Id_Sector)
              ent_ids <- df_entidades %>% filter(Id_Sector %in% ids) %>% pull(Id_Entidad)
              datos_entidad <- datos_entidad %>% filter(Id_Entidad %in% ent_ids)
            } else if (nivel == "Entidad" && !is.null(entidades_sel) && length(entidades_sel) > 0) {
              datos_entidad <- datos_entidad %>% filter(Nombre_Entidad %in% entidades_sel)
            }
            if (nrow(datos_entidad) == 0) datos_entidad <- NULL
          }
          
          temp_report <- generar_reporte_indice(
            datos_filtrados          = datos,
            nivel_consulta           = nivel,
            sectores_seleccionados   = sectores_sel,
            entidades_seleccionadas  = entidades_sel,
            canales_seleccionados    = canales_sel,
            canal_seleccionado       = canal_sel,
            subcanales_seleccionados = subcanales_sel,
            detalle_canal            = ver_detalle,
            titulo_ranking           = "Ranking",
            indice_global            = round(indice_global, 1),
            datos_pilar              = pilares_data,
            datos_periodo            = periodo_data,
            datos_canal              = datos_canal_pdf,
            datos_entidad            = datos_entidad
          )
          
          rmarkdown::render(
            temp_report$template,
            output_file = basename(file),
            output_dir = dirname(file),
            params = temp_report$parametros,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          removeNotification("pdf_progress")
          showNotification("Reporte generado", type = "message")
        }, error = function(e) {
          removeNotification("pdf_progress")
          showNotification(paste("Error PDF:", e$message), type = "error")
        })
      }
    )
    
    # ============================================
    # 12. RETORNO AL SERVER PRINCIPAL
    # ============================================
    list(
      estado      = filtros_estado,
      evento      = filtros_evento,
      btn_click   = btn_click,
      nivel_click = btn_click   # alias: server.R usa nivel_click tambien
    )
  })
}