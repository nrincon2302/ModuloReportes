acciones_mejora_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "visualization-container",
    
    # -------------------------- PANEL DE FILTROS --------------------------
    div(
      class = "filter-panel",
      tags$h4("Filtros de Análisis"),
      
      # --- Filtro Sector ---
      tags$h4("Seleccione el Sector"),
      selectInput(ns("filtro_sector"), NULL, choices = NULL),
      
      # --- Filtro Entidad ---
      tags$h4("Seleccione la Entidad"),
      uiOutput(ns("selector_entidad")),
      
      hr(),
      downloadButton(
        ns("descargar_reporte_mejora"),
        "Imprimir PDF",
        style = "width: 100%; margin-top: 10px;"
      ),
      
    ),
    
    # --------------------------- ÁREA DE TABLA -----------------------------
    div(
      class = "plot-area",
      div(
        class = "plot-box plot-box-full",
        style = "min-height: 500px;",
        DTOutput(ns("tabla_acciones_mejora"))
      )
    )
  )
}


acciones_mejora_server <- function(id, id_componente_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------- ACTUALIZA SECTORES ----------------------------
    observe({
      updateSelectInput(
        session,
        "filtro_sector",
        choices = setNames(df_sectores$Id_Sector, df_sectores$Sector)
      )
    })
    
    # ---------------------------- SELECTOR ENTIDAD ------------------------------
    output$selector_entidad <- renderUI({
      req(input$filtro_sector)
      
      entidades_filtradas <- df_entidades %>%
        filter(Id_Sector == input$filtro_sector)
      
      selectInput(
        session$ns("filtro_entidad"),
        NULL,
        choices = setNames(entidades_filtradas$Id_Entidad, entidades_filtradas$Entidad)
      )
    })
    
    # ---------------------------- DATOS FILTRADOS ------------------------------
    filtered_acciones_data <- reactive({
      req(input$filtro_sector, input$filtro_entidad)
      
      df_indicadores_criticos_por_entidad %>%
        left_join(df_indicadores %>% select(Id_Indicador, Id_Componente, Pilar), by = "Id_Indicador") %>%
        filter(Id_Entidad == input$filtro_entidad, 
               Id_Componente == id_componente_reactive()) %>% 
        select(
          Pilar,
          Indicador,
          `Valor Promedio`,
          Criterios,
          `Acciones Sugeridas`,
          `Herramientas de Apoyo`
        ) %>%
        arrange(`Valor Promedio`)
    })
    
    
    # ---------------------------- TABLA ----------------------------------------
    process_acciones_to_html <- function(df, col = "ACCIONES REQUERIDA") {
      stopifnot(col %in% names(df))
      
      escape_html <- function(x) {
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        x <- gsub('"', "&quot;", x, fixed = TRUE)
        x <- gsub("'", "&#39;", x, fixed = TRUE)
        x
      }
      
      to_ul_if_bullets <- function(text) {
        if (is.na(text)) return(NA_character_)
        text_chr <- as.character(text)
        # Detecta si hay patrones tipo "*", "-" o "•"
        has_bullets <- str_detect(text_chr, "\\*|\\-|•")
        if (!has_bullets) {
          # sin bullets: mantener texto pero convertir saltos de linea a <br>
          return(escape_html(gsub("\r\n|\r|\n", "<br>", text_chr)))
        }
        # dividir por bullets (acepta * - •) y limpiar
        items <- str_split(text_chr, "\\s*(?:\\*|\\-|•)\\s*", simplify = FALSE)[[1]]
        # si el texto empieza con bullet, el primer elemento queda vacío: quitar vacíos
        items <- str_trim(items)
        items <- items[items != ""]
        # si por alguna razón quedaron cero items, mantener el original escapado
        if (length(items) == 0) return(escape_html(gsub("\r\n|\r|\n", "<br>", text_chr)))
        items <- escape_html(items)
        lis <- paste0("<li>", items, "</li>", collapse = "")
        paste0("<ul>", lis, "</ul>")
      }
      
      df %>%
        mutate("{col}" := vapply(.data[[col]], to_ul_if_bullets, FUN.VALUE = character(1)))
    }
    
    
    output$tabla_acciones_mejora <- renderDT({
      
      datos <- filtered_acciones_data()
      
      if (nrow(datos) == 0) {
        return(datatable(
          data.frame(Mensaje = "No hay indicadores críticos para la entidad seleccionada"),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      
      datos_fmt <- datos %>%
        mutate(
          `Valor Promedio` = round(`Valor Promedio`, 1),
          `Herramientas de Apoyo` = stringr::str_replace_all(
            `Herramientas de Apoyo`,
            "(https?://[[:alnum:]/._%?=&-]+)",
            "<a href='\\1' target='_blank' style='color:#0056b3; text-decoration:underline;'>link</a>"
          )
        )
      
      datos_fmt <- process_acciones_to_html(datos_fmt, col = "Acciones Sugeridas")
      
      datatable(
        datos_fmt,
        escape = FALSE,
        options = list(
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            infoEmpty = "Mostrando 0 registros",
            infoFiltered = "(filtrado de _MAX_ registros totales)",
            paginate = list(
              first = "Primero",
              last = "Último",
              `next` = "Siguiente",
              previous = "Anterior"
            )
          ),
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = "display stripe hover"
      )
    })
    
    # ---------------------------- PDF ------------------------------------------
    output$descargar_reporte_mejora <- downloadHandler(
      filename = function() {
        paste0("reporte_acciones_mejora_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        showNotification("Generando reporte PDF...", type = "message", 
                         duration = NULL, id = "pdf_mejora_progress")
        
        tryCatch({
          # Obtener datos filtrados
          datos_base <- filtered_acciones_data()
          
          if (is.null(datos_base) || nrow(datos_base) == 0) {
            removeNotification("pdf_mejora_progress")
            showNotification("No hay datos para generar el reporte", 
                             type = "warning", duration = 5)
            return(NULL)
          }
          
          # Agregar información de entidad y sector
          datos_para_reporte <- datos_base %>%
            mutate(
              Entidad = df_entidades %>% 
                filter(Id_Entidad == input$filtro_entidad) %>% 
                pull(Entidad) %>% 
                first(),
              Sector = df_sectores %>% 
                filter(Id_Sector == input$filtro_sector) %>% 
                pull(Sector) %>% 
                first()
            )
          
          # Preparar entorno de reporte
          info <- preparar_entorno_reporte(
            template = "templates/report_mejora.Rmd",
            logo = "www/logo.png",
            parametros = list(
              entidad = datos_para_reporte[["Entidad"]][[1]],
              datos_acciones = datos_para_reporte
            )
          )
          
          # Renderizar PDF
          rmarkdown::render(
            input = info$template,
            output_file = basename(file),
            output_dir = dirname(file),
            params = info$parametros,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          
          removeNotification("pdf_mejora_progress")
          showNotification("Reporte generado exitosamente", 
                           type = "message", duration = 3)
          
        }, error = function(e) {
          removeNotification("pdf_mejora_progress")
          showNotification(
            paste("Error al generar PDF:", e$message), 
            type = "error", 
            duration = 10
          )
        })
      }
    )
    
  })
}
