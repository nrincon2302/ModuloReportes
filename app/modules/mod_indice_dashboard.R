library(shinycssloaders)

indice_dashboard_ui <- function(id, id_componente) {
  ns <- NS(id)
  
  # Datos estáticos para los selectores
  all_sectores <- sort(df_sectores$Sector)
  all_entidades <- sort(df_entidades$Entidad)
  all_canales <- obtener_canales()
  
  # Datos para filtros de fecha (desde 2025 hasta año actual)
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  anios_fijos <- as.character(seq(2025, end_year))
  meses_fijos <- sprintf("%02d", 1:12)
  names(meses_fijos) <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  div(
    class = "visualization-container",
    div(
      class = "filter-panel",
      
      # --- FILTRO DE FECHAS ---
      tags$h4("Periodo de Análisis", style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      
      fluidRow(
        column(6, 
               pickerInput(ns("filtro_anio"), "Año", 
                           choices = anios_fijos, 
                           multiple = TRUE, 
                           options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"))
        ),
         column(6,
           uiOutput(ns("mes_selector_ui"))
         )
      ),
      
      actionButton(ns("btn_actualizar_fechas"), "Filtrar por Fechas", 
                   icon = icon("sync"), 
                   class = "btn-primary btn-sm", 
                   style = "width: 100%; margin-bottom: 15px;"),
      
      hr(),
      
      # --- NIVEL DE CONSULTA ---
      tags$h4("Nivel de Consulta", style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
      radioButtons(ns("filtro_nivel"), NULL, 
                   choices = c("Distrito", "Sector", "Entidad"), 
                   selected = "Distrito"),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Sector'", ns("filtro_nivel")),
        div(
          style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("sector_select_all"), "Todos"), " | ",
          actionLink(ns("sector_deselect_all"), "Ninguno")
        ),
        div(class = "scrollable-checkbox-group",
            checkboxGroupInput(ns("sector_checks"), NULL, 
                               choices = all_sectores, 
                               selected = all_sectores))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Entidad'", ns("filtro_nivel")),
        div(
          style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("entidad_select_all"), "Todos"), " | ",
          actionLink(ns("entidad_deselect_all"), "Ninguno")
        ),
        div(class = "scrollable-checkbox-group",
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
        div(
          style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("subcanal_select_all"), "Todos"), " | ",
          actionLink(ns("subcanal_deselect_all"), "Ninguno")
        ),
        div(class = "scrollable-checkbox-group",
            uiOutput(ns("subcanales_checks_ui")))
      ),
      
      hr(),
      downloadButton(ns("descargar_reporte"), "Imprimir PDF", style = "width: 100%; margin-top: 10px;")
    ),
    
    # --- ÁREA DE GRÁFICAS ---
    div(
      class = "plot-area plot-area-rows",
      div(
        class = "plot-row",
        div(id = ns("box_plot_gauge_global"), class = "plot-box plot-box-half plot-box-gauge center-when-alone",
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
    # 0. COMUNICACIÓN CON SERVER PRINCIPAL
    # ============================================
    
    # Dynamic month selector: single year -> one picker; multiple years -> one picker per year
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
    
    # Reactive que devuelve el estado actual de los filtros (válido para UI y debugging)
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
    
    # --- Evento controlado: Exponer un evento que solo cambia cuando se presiona el botón ---
    btn_actualizar_click <- reactiveVal(0)
    observeEvent(input$btn_actualizar_fechas, {
      # Incrementar el contador de clicks para evitar dependencias accidentales
      btn_actualizar_click(btn_actualizar_click() + 1)
    })
    
    # Evento reactivo para recuperar el snapshot de filtros solo cuando se cliquea
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
    # 1. HELPERS UI DINÁMICOS
    # ============================================
    
    # Entidades disponibles según sectores seleccionados
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
    
    # Subcanales disponibles (espera señal de canal)
    output$subcanales_checks_ui <- renderUI({
      req(input$filtro_canal_selector)
      req(signals$canal())  # Esperar a que carguen los datos de canal
      
      subs <- tryCatch({
        obtener_subcanales(input$filtro_canal_selector)
      }, error = function(e) character(0))
      
      if(length(subs) == 0) return(div("No hay subcanales disponibles"))
      
      checkboxGroupInput(session$ns("subcanales_checks"), NULL, choices = subs, selected = subs)
    })
    
    # ============================================
    # 2. BOTONES DE SELECCIÓN RÁPIDA
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
    
    # ============================================
    # 3. REACTIVOS PARA IDS SELECCIONADOS
    # ============================================
    
    ids_seleccionados <- reactive({
      nivel <- input$filtro_nivel
      
      if (nivel == "Distrito") return(unique(df_entidades$Id_Entidad))
      
      if (nivel == "Sector") {
        req(input$sector_checks)
        
        # Obtener todos los sectores disponibles
        todos_sectores <- sort(df_sectores$Sector)
        
        # Si están todos seleccionados, devolvemos los IDs de sector
        if (length(input$sector_checks) == length(todos_sectores) && 
            all(sort(input$sector_checks) == todos_sectores)) {
          return(unique(df_entidades$Id_Sector))
        }
        
        # De lo contrario, devolver los IDs de los sectores seleccionados
        return(df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector))
      }
      
      if (nivel == "Entidad") {
        req(input$entidad_checks)
        
        return(df_entidades %>% filter(Entidad %in% input$entidad_checks) %>% pull(Id_Entidad))
      }
    })
    
    # ============================================
    # 4. FUENTE DE DATOS (Siempre rv_bg) - reactividades por señal
    # ============================================
    observeEvent(id_componente_reactive(), {
      req(exists("periodos"), exists("df_sectores"), exists("df_entidades"))
      anios_fijos <- as.character(sort(unique(periodos$Año)))
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
      
      session$sendCustomMessage("removeClass", list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
    }, ignoreInit = TRUE)
    
    # Creamos reactivas separadas para cada señal para permitir que
    # las gráficas se actualicen de forma independiente cuando cada
    # job/servicio termine.
    datos_general <- reactive({
      req(signals$general())
      rv_bg
    })
    
    datos_historico <- reactive({
      # Dependencia explícita: esta reactiva se invalidará cuando termine el job de historico
      signals$historico()
      rv_bg
    })
    
    datos_canal <- reactive({
      # Dependencia explícita: esta reactiva se invalidará cuando termine el job de canal
      signals$canal()
      rv_bg
    })
    
    # ============================================
    # 5. GRÁFICAS - GAUGE GLOBAL
    # ============================================
    
    output$plot_gauge_global <- renderHighchart({
      datos <- datos_general()
      req(datos$general)
      
      id_comp <- id_componente_reactive()
      indice_global <- obtener_indice_componente(datos$general, id_comp)
      
      if (is.na(indice_global)) return(highchart() %>% hc_title(text = "Sin datos"))
      
      indice_global <- round(indice_global, 1)
      nivel_texto <- if (indice_global < 90) "Crítico" else if (indice_global < 97) "Aceptable" else "Óptimo"
      color_nivel <- if (indice_global < 90) "#E3272A" else if (indice_global < 97) "#F9D248" else "#8CBE23"
      
      sub_text <- paste0("<span style='font-size:16px;'>El índice es <b style='color:black;'>", 
                         indice_global, "%</b> y es <b style='color:", color_nivel, ";'>", nivel_texto, "</b></span>")
      
      highchart() %>%
        hc_chart(type = "gauge") %>%
        hc_pane(startAngle = -150, endAngle = 150, size = "110%") %>%
        hc_yAxis(min = 0, max = 100, 
                 plotBands = list(
                   list(from=0, to=90, color="#E3272A"), 
                   list(from=90, to=97, color="#F9D248"), 
                   list(from=97, to=100, color="#8CBE23")
                 )) %>%
        hc_add_series(name="Índice", data=list(indice_global), 
                      dataLabels=list(enabled=TRUE, format='{y}', 
                                      style=list(fontSize='24px'), borderWidth=0, y=40)) %>%
        hc_title(text = "Índice Global", style = list(fontWeight = "bold", fontSize = "16px")) %>%
        hc_subtitle(useHTML = TRUE, text = sub_text, align = "center", verticalAlign = "bottom", y = 30)
    })
    
    # ============================================
    # 6. GRÁFICAS - BARRAS POR PILAR
    # ============================================
    
    output$plot_bar_pilar <- renderHighchart({
      datos <- datos_general()
      req(datos$general)
      
      id_comp <- id_componente_reactive()
      pilares_data <- obtener_indices_pilares(datos$general, id_comp)
      
      if (is.null(pilares_data)) {
        return(NULL)
      }

      # If pillars exist but after filtering there are no rows, show a friendly message
      if (nrow(pilares_data) == 0) {
        return(
          highchart() %>%
            hc_title(text = 'Sin datos disponibles')
        )
      }
      
      hchart(pilares_data, "bar", hcaes(x = Pilar, y = round(Valor,1), color = Valor)) %>%
        hc_plotOptions(bar = list(depth = 40, shape = 'cylinder', 
                                  dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
        hc_colorAxis(stops = color_stops(n = 3, colors = c('#E3272A', '#F9D248', '#8CBE23')), 
                     min = 85, max = 100) %>%
        hc_title(text = 'Índice por Pilar') %>% 
        hc_legend(enabled = FALSE)
    })
    
    # Render UI for the Pilar box only when there is data to show
    output$box_plot_bar_pilar_ui <- renderUI({
      datos <- datos_general()
      req(datos$general)
      id_comp <- id_componente_reactive()
      pilares_data <- tryCatch(obtener_indices_pilares(datos$general, id_comp), error = function(e) NULL)
      
      if (is.null(pilares_data)) {
        # Ensure gauge is centered when pillar isn't present
        session$sendCustomMessage("addClass", list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
        return(NULL)
      }
      
      # Remove centering class and show the box with the plot output
      session$sendCustomMessage("removeClass", list(id = session$ns("box_plot_gauge_global"), class = "center-when-alone"))
      div(
        id = session$ns("box_plot_bar_pilar"),
        class = "plot-box plot-box-half plot-box-pilar",
        withSpinner(highchartOutput(session$ns("plot_bar_pilar"), height = "260px"),
                    type = 4, color = "#225495")
      )
    })
    
    # ============================================
    # 7. GRÁFICAS - RANKING ENTIDADES
    # ============================================
    ranking_entidades_filtrado <- reactive({
      # Validar que exista ranking_entidades
      req(rv_bg$ranking_entidades)
      
      # Obtener ranking base
      ranking <- obtener_ranking_entidades(rv_bg, id_componente_reactive())
      
      # VALIDACIÓN TEMPRANA: Si no hay datos, retornar NULL inmediatamente
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(NULL)
      }
      
      # Aplicar filtros por nivel si corresponde
      if (input$filtro_nivel == "Sector") {
        ids_sectores <- ids_seleccionados()
        
        # Solo filtrar si hay IDs seleccionados
        if (!is.null(ids_sectores) && length(ids_sectores) > 0) {
          ids_entidades <- tryCatch({
            df_entidades %>% 
              filter(Id_Sector %in% ids_sectores) %>% 
              pull(Id_Entidad)
          }, error = function(e) {
            warning("Error al obtener entidades por sector: ", e$message)
            return(NULL)
          })
          
          if (!is.null(ids_entidades) && length(ids_entidades) > 0) {
            ranking <- ranking %>% filter(Id_Entidad %in% ids_entidades)
          }
        }
        
      } else if (input$filtro_nivel == "Entidad") {
        ids_entidades <- ids_seleccionados()
        
        # Solo filtrar si hay IDs seleccionados
        if (!is.null(ids_entidades) && length(ids_entidades) > 0) {
          ents <- tryCatch({
            df_entidades %>% 
              filter(Id_Entidad %in% ids_entidades) %>% 
              pull(Entidad)
          }, error = function(e) {
            warning("Error al obtener nombres de entidades: ", e$message)
            return(NULL)
          })
          
          if (!is.null(ents) && length(ents) > 0) {
            ranking <- ranking %>% filter(Entidad %in% ents)
          }
        }
      }
      # Si es Distrito, mostrar todas las entidades (sin filtro adicional)
      
      # VALIDACIÓN FINAL: Verificar que aún haya datos después del filtrado
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(NULL)
      }
      
      ranking
    })
    
    output$box_plot_ranking_entidades_ui <- renderUI({
      div(
        class = "plot-box plot-box-full plot-box-ranking",
        highchartOutput(session$ns("plot_ranking_entidades"), height = "460px")
      )
    })
    
    output$plot_ranking_entidades <- renderHighchart({
      ranking <- ranking_entidades_filtrado()
      
      # Manejo robusto de datos vacíos o NULL
      if (is.null(ranking) || nrow(ranking) == 0) {
        return(
          highchart() %>% 
            hc_title(text = "No hay datos disponibles para el período seleccionado",
                     style = list(color = "#666", fontSize = "14px")) %>%
            hc_subtitle(text = "Intente con un rango de fechas diferente",
                        style = list(color = "#999", fontSize = "12px"))
        )
      }
      
      ranking_coloreado <- ranking %>%
        mutate(
          Indice = as.numeric(round(Valor, 1)),
          Color = case_when(
            Indice >= 98 ~ "#8CBE23",  # Verde
            Indice >= 90 ~ "#F9D248",  # Amarillo
            TRUE ~ "#E3272A"           # Rojo
          )
        )
      
      hchart(ranking_coloreado, "bar", hcaes(x = Entidad, y = Indice, color = Color)) %>%
        hc_plotOptions(bar = list(depth = 40, shape = 'cylinder',
                                  dataLabels = list(enabled = TRUE, format = '{y}%'),
                                  colorByPoint = TRUE)) %>%
        hc_title(text = "Ranking de Entidades") %>%
        hc_legend(enabled=FALSE)
    })
    
    # ============================================
    # 8-9. EVOLUCIÓN / CANAL
    # ============================================
    # Asegurar que dependencias a histórico y canal usen las señales
    output$plot_bar_periodo <- renderHighchart({
      datos_struct <- datos_historico()
      req(datos_struct$historico)
      datos <- obtener_indice_por_periodo(datos_struct$historico, id_componente_reactive())
      
      if (is.null(datos) || nrow(datos) == 0) {
        return(highchart() %>% hc_title(text = "Sin histórico disponible"))
      }
      
      hchart(datos %>% mutate(Indice = round(Indice, 1)), 
             "column", 
             hcaes(x = Periodo, y = Indice), 
             color = "#225495") %>%
        hc_plotOptions(column = list(depth = 40, shape = 'cylinder', 
                                     dataLabels = list(enabled = TRUE, format = '{y}'))) %>%
        hc_title(text = "Evolución Histórica") %>%
        hc_xAxis(title = list(text = "Periodo")) %>%
        hc_yAxis(min = 0, max = 100) %>%
        hc_legend(enabled = FALSE)
    })
    
    output$plot_bar_canal <- renderHighchart({
      datos_struct <- datos_canal()
      req(datos_struct$canal)
      
      id_comp <- id_componente_reactive()
      nivel_actual <- input$filtro_nivel
      ver_detalle <- isTRUE(input$filtro_canal_detalle)
      
      # --- VISTA GENERAL: Comparar Canales ---
      channel <<- datos_struct$canal
      if (!ver_detalle) {
        datos_graf <- obtener_indices_por_canal(datos_struct$canal, id_comp)
        titulo_graf <- "Desempeño por Canal"
        columna_cat <- "Canal"
        
        # --- VISTA DETALLE: Subcanales ---
      } else {
        req(input$filtro_canal_selector)
        req(input$subcanales_checks)
        req(signals$subcanal())  # Esperar a que termine el job de subcanales
        
        canal_sel <- input$filtro_canal_selector
        subcanales_sel <- input$subcanales_checks
        
        # Obtener datos del subcanal
        lista_subcanales <- datos_struct$subcanal[[canal_sel]]
        
        # Calcular índice por cada subcanal seleccionado
        resultados <- data.frame(Subcanal = character(), Indice = numeric(), stringsAsFactors = FALSE)
        
        for (sub_nm in subcanales_sel) {
          if (!is.null(lista_subcanales[[sub_nm]])) {
            valor <- obtener_indice_componente(lista_subcanales[[sub_nm]], id_comp)
            if (!is.na(valor)) {
              resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Indice = valor))
            }
          }
        }
        
        datos_graf <- resultados %>% arrange(desc(Indice))
        titulo_graf <- paste("Subcanales -", canal_sel)
        columna_cat <- "Subcanal"
      }
      
      if (is.null(datos_graf) || nrow(datos_graf) == 0) {
        return(highchart() %>% hc_title(text = "Sin datos disponibles"))
      }
      
      hchart(datos_graf, "column", 
             hcaes(x = !!sym(columna_cat), y = round(Indice,1), color = Indice)) %>%
        hc_plotOptions(column = list(depth = 40, shape = 'cylinder', 
                                     dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
        hc_colorAxis(stops = color_stops(n = 3, colors = c("#E3242A", "#F9D248", "#225495")), 
                     min = 60, max = 100) %>%
        hc_title(text = titulo_graf) %>%
        hc_legend(enabled = FALSE)
    })
    
    # ============================================
    # 10. REPORTE PDF
    # ============================================
    output$descargar_reporte <- downloadHandler(
      filename = function() { paste0("reporte_indice_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content = function(file) {
        showNotification("Generando reporte...", type = "message", duration = NULL, id = "pdf_progress")
        tryCatch({
          # datos actuales
          req(signals$general())
          datos <- rv_bg
          id_comp <- id_componente_reactive()
          
          # Construir parámetros visibles actualmente
          indice_global <- tryCatch(obtener_indice_componente(datos$general, id_comp), error = function(e) NA)
          pilares_data <- tryCatch(obtener_indices_pilares(datos$general, id_comp), error = function(e) NULL)
          periodo_data <- tryCatch(if (!is.null(datos$historico)) obtener_indice_por_periodo(datos$historico, id_comp) else NULL, error = function(e) NULL)
          datos_canal <- tryCatch(if (!is.null(datos$canal)) obtener_indices_por_canal(datos$canal, id_comp) else NULL, error = function(e) NULL)
          
          # Generar ranking de entidades de forma consistente con la gráfica
          datos_entidad <- tryCatch(obtener_ranking_entidades(datos, id_comp), error = function(e) NULL)
          
          # parámetros de filtros actuales
          nivel <- input$filtro_nivel
          sectores_sel <- if (nivel %in% c("Sector", "Entidad")) input$sector_checks else NULL
          entidades_sel <- if (nivel == "Entidad") input$entidad_checks else NULL
          canal_sel <- if (isTRUE(input$filtro_canal_detalle)) input$filtro_canal_selector else NULL
          subcanales_sel <- if (isTRUE(input$filtro_canal_detalle)) input$subcanales_checks else NULL
          
          # Normalizar columnas para los templates (esperan nombres como 'Indice' o 'Nombre_Entidad')
          if (!is.null(pilares_data) && nrow(pilares_data) > 0) {
            if ("Valor" %in% names(pilares_data) && !("Indice" %in% names(pilares_data))) {
              pilares_data <- pilares_data %>% dplyr::rename(Indice = Valor)
            }
            if ("Indice" %in% names(pilares_data)) pilares_data$Indice <- as.numeric(pilares_data$Indice)
          }
          
          if (!is.null(datos_entidad) && nrow(datos_entidad) > 0) {
            if ("Entidad" %in% names(datos_entidad) && !("Nombre_Entidad" %in% names(datos_entidad))) {
              datos_entidad <- datos_entidad %>% dplyr::rename(Nombre_Entidad = Entidad)
            }
            if ("Valor" %in% names(datos_entidad) && !("Indice" %in% names(datos_entidad))) {
              datos_entidad <- datos_entidad %>% dplyr::rename(Indice = Valor)
            }
            if ("Indice" %in% names(datos_entidad)) datos_entidad$Indice <- as.numeric(datos_entidad$Indice)
          }
          
          # Si se solicitó detalle por subcanales, construir datos_canal acorde a selección
          if (isTRUE(input$filtro_canal_detalle) && !is.null(canal_sel) && !is.null(subcanales_sel) && length(subcanales_sel) > 0) {
            datos_canal <- NULL
            if (!is.null(rv_bg$subcanal) && !is.null(rv_bg$subcanal[[canal_sel]])) {
              lista_sub <- rv_bg$subcanal[[canal_sel]]
              resultados <- data.frame(Subcanal = character(), Indice = numeric(), stringsAsFactors = FALSE)
              for (sub_nm in subcanales_sel) {
                if (!is.null(lista_sub[[sub_nm]])) {
                  val <- tryCatch(obtener_indice_componente(lista_sub[[sub_nm]], id_comp), error = function(e) NA)
                  if (!is.na(val)) resultados <- rbind(resultados, data.frame(Subcanal = sub_nm, Indice = round(as.numeric(val),1)))
                }
              }
              if (nrow(resultados) > 0) datos_canal <- resultados
            }
          } else {
            if (!is.null(datos_canal) && nrow(datos_canal) > 0) {
              if ("Valor" %in% names(datos_canal) && !("Indice" %in% names(datos_canal))) {
                datos_canal <- datos_canal %>% dplyr::rename(Indice = Valor)
              }
              if ("Indice" %in% names(datos_canal)) datos_canal$Indice <- round(as.numeric(datos_canal$Indice),1)
            }
          }
          
          # Filtrar ranking de entidades según nivel / selección para que el PDF refleje lo mostrado en la UI
          if (!is.null(datos_entidad) && nrow(datos_entidad) > 0) {
            if (nivel == "Sector" && !is.null(sectores_sel) && length(sectores_sel) > 0) {
              ids <- df_sectores %>% filter(Sector %in% sectores_sel) %>% pull(Id_Sector)
              entidades_ids <- df_entidades %>% filter(Id_Sector %in% ids) %>% pull(Id_Entidad)
              datos_entidad <- datos_entidad %>% filter(Id_Entidad %in% entidades_ids)
            } else if (nivel == "Entidad" && !is.null(entidades_sel) && length(entidades_sel) > 0) {
              # entidades_sel contiene nombres de entidades
              datos_entidad <- datos_entidad %>% filter(Entidad %in% entidades_sel)
            }
            if (nrow(datos_entidad) == 0) datos_entidad <- NULL
          }
          
          temp_report <- generar_reporte_indice(
            datos_filtrados = datos,
            nivel_consulta = nivel,
            sectores_seleccionados = sectores_sel,
            entidades_seleccionadas = entidades_sel,
            canal_seleccionado = canal_sel,
            subcanales_seleccionados = subcanales_sel,
            detalle_canal = input$filtro_canal_detalle,
            titulo_ranking = "Ranking",
            indice_global = round(indice_global, 1),
            datos_pilar = pilares_data,
            datos_periodo = periodo_data,
            datos_canal = datos_canal,
            datos_entidad = datos_entidad
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
    # 11. RETORNO DE ESTADO AL SERVER PRINCIPAL
    # ============================================
    list(
      estado = filtros_estado,
      evento = filtros_evento,
      btn_click = btn_actualizar_click,
      nivel_click = btn_aplicar_nivel_click
    )
  })
}
