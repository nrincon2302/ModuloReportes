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

indice_dashboard_local_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "filter-panel",
    downloadButton(ns("descargar_reporte"), "Imprimir PDF",
                   style = "width: 100%;")
  )
}

indice_dashboard_plot_ui <- function(id) {
  ns <- NS(id)

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
}

indice_dashboard_server <- function(id, id_componente_reactive, rv_bg, signals, filtros) {
  moduleServer(id, function(input, output, session) {

    # ============================================
    # 0. FILTROS: ahora vienen del modulo global (mod_filtros.R)
    # ============================================
    # `filtros` es el objeto devuelto por filtros_server(): lista con
    # estado (reactive en vivo), evento (snapshot al pulsar "Aplicar
    # Filtros"), btn_click e ids_seleccionados.
    filtros_evento     <- filtros$evento
    ids_seleccionados  <- filtros$ids_seleccionados

    observeEvent(id_componente_reactive(), {
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
        
        if (is.null(pilares_data) || nrow(pilares_data) == 0 ||
            all(is.na(pilares_data$Valor)))
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
      
      estado_filtros <- filtros$estado()
      if (estado_filtros$filtro_nivel == "Sector") {
        sectores_sel <- estado_filtros$sector_checks
        if (is.null(sectores_sel) || length(sectores_sel) == 0) return(NULL)
        ids_sectores <- df_sectores %>% filter(Sector %in% sectores_sel) %>% pull(Id_Sector)
        ids_entidades <- tryCatch(
          df_entidades %>% filter(Id_Sector %in% ids_sectores) %>% pull(Id_Entidad),
          error = function(e) NULL
        )
        if (!is.null(ids_entidades) && length(ids_entidades) > 0)
          ranking <- ranking %>% filter(Id_Entidad %in% ids_entidades)
      } else if (estado_filtros$filtro_nivel == "Entidad") {
        entidades_sel <- estado_filtros$entidad_checks
        if (is.null(entidades_sel) || length(entidades_sel) == 0) return(NULL)
        ents <- tryCatch(
          df_entidades %>% filter(Entidad %in% entidades_sel) %>% pull(Entidad),
          error = function(e) NULL
        )
        if (!is.null(ents) && length(ents) > 0)
          ranking <- ranking %>% filter(Entidad %in% ents)
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
              Indice >= 97 ~ "#8CBE23",
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
          
          if (is.null(datos_graf) || nrow(datos_graf) == 0)
            return(.error_chart("Sin datos de canal para la selección actual."))
          
          # Filtrar por canales seleccionados (si hay selección parcial)
          canales_sel <- ev$canales_checks
          if (!is.null(canales_sel) && length(canales_sel) > 0) {
            datos_graf <- datos_graf %>% filter(Canal %in% canales_sel)
            if (nrow(datos_graf) == 0)
              return(.error_chart("Ningún canal seleccionado tiene datos para este indicador"))
          }
          
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
          
          estado_filtros <- isolate(filtros$estado())
          nivel          <- estado_filtros$filtro_nivel
          sectores_sel   <- if (nivel %in% c("Sector", "Entidad")) estado_filtros$sector_checks else NULL
          entidades_sel  <- if (nivel == "Entidad") estado_filtros$entidad_checks else NULL
          ver_detalle    <- isTRUE(estado_filtros$filtro_canal_detalle)
          canal_sel      <- if (ver_detalle) estado_filtros$filtro_canal_selector else NULL
          subcanales_sel <- if (ver_detalle) estado_filtros$subcanales_checks else NULL
          canales_sel    <- if (!ver_detalle) estado_filtros$canales_checks else NULL
          
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
    
    invisible(NULL)
  })
}