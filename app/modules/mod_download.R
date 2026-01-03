library(shiny)
library(openxlsx)
library(dplyr)

# ============================================
# UI
# ============================================
descargar_excel_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "margin-top: 15px;",
    downloadButton(
      ns("btn_descargar_pqrds"),
      "Descargar PQRSD Evaluadas",
      icon = icon("file-excel"),
      class = "btn-success",
      style = "font-weight: bold; width: 100%; margin-bottom: 10px;"
    ),
    downloadButton(
      ns("btn_descargar_extemporaneas"),
      "Descargar Gestiones Extemporáneas",
      icon = icon("file-excel"),
      class = "btn-success",
      style = "font-weight: bold; width: 100%;"
    )
  )
}

# ============================================
# SERVER
# ============================================
descargar_excel_server <- function(id, periodos_seleccionados = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # ============================================
    # BOTÓN 1: DESCARGAR PQRDS
    # ============================================
    output$btn_descargar_pqrds <- downloadHandler(
      filename = function() {
        paste0("PQRDS_por_entidad_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      
      content = function(file) {
        showNotification("Generando Excel de PQRDS...", 
                         type = "message", 
                         duration = NULL, 
                         id = "excel_progress")
        
        tryCatch({
          # Obtener periodos seleccionados (si están disponibles)
          periodos <- NULL
          if (!is.null(periodos_seleccionados)) {
            periodos <- tryCatch(periodos_seleccionados(), error = function(e) NULL)
          }
          
          # Si no hay periodos, continuar sin filtrar (comportamiento anterior)
          if (is.null(periodos) || length(periodos) == 0) {
            showNotification("No hay periodos seleccionados, descargando todos los datos", 
                             type = "message", duration = 3, id = "excel_progress")
          }
          
          # Validar datos
          if (!exists("base_pqrds")) {
            showNotification("No se encontró la base de datos 'base_pqrds'", 
                             type = "warning", duration = 5, id = "excel_progress")
            return(NULL)
          }
          
          if (!exists("df_entidades")) {
            showNotification("No se encontró el catálogo 'df_entidades'", 
                             type = "warning", duration = 5, id = "excel_progress")
            return(NULL)
          }
          
          # Validar columnas
          columnas_requeridas <- c("mod1_gv4_p8", "mod1_gv4_p4", "mod1_gv4_p9",
                                   "mod2_mod2_1_v22", "mod2_mod2_1_v23", 
                                   "mod2_mod2_1_v24", "mod2_mod2_1_v26", "mod2_mod2_1_v25")
          
          columnas_faltantes <- setdiff(columnas_requeridas, names(base_pqrds))
          if (length(columnas_faltantes) > 0) {
            showNotification(
              paste("Faltan columnas:", paste(columnas_faltantes, collapse = ", ")),
              type = "warning", duration = 5, id = "excel_progress"
            )
            return(NULL)
          }
          
          # Procesar datos PQRDS con filtro de periodos (si aplica)
          datos_pqrds <- base_pqrds
          
          # Aplicar filtro de periodo si está disponible
          if (!is.null(periodos) && length(periodos) > 0 && "periodo" %in% names(base_pqrds)) {
            datos_pqrds <- datos_pqrds %>% filter(periodo %in% periodos)
          }
          
          datos_pqrds <- datos_pqrds %>%
            mutate(mod1_gv4_p8 = as.numeric(mod1_gv4_p8)) %>%
            left_join(df_entidades %>% select(Id_Entidad, Entidad), 
                      by = c("mod1_gv4_p8" = "Id_Entidad")) %>%
            mutate(
              Entidad = iconv(Entidad, to = "ASCII//TRANSLIT", sub = ""),
              mod1_gv4_p9 = iconv(mod1_gv4_p9, to = "ASCII//TRANSLIT", sub = ""),
              # Formatear fecha como YYYY-MM-DD
              FECHA_INGRESO = format(as.Date(mod1_p2), "%Y-%m-%d")
            ) %>%
            select(
              Entidad,
              FECHA_INGRESO,
              NO_PETICION = mod1_gv4_p4,
              DEPENDENCIA_RESPUESTA = mod1_gv4_p9,
              COHERENCIA = mod2_mod2_1_v22,
              CLARIDAD = mod2_mod2_1_v23,
              CALIDEZ = mod2_mod2_1_v24,
              OPORTUNIDAD = mod2_mod2_1_v26,
              MANEJO_SISTEMA = mod2_mod2_1_v25
            ) %>%
            filter(!is.na(Entidad))
          
          if (nrow(datos_pqrds) == 0) {
            showNotification("No hay datos de PQRDS para procesar", 
                             type = "warning", duration = 5, id = "excel_progress")
            return(NULL)
          }
          
          # Crear libro de Excel
          wb <- createWorkbook()
          
          # Estilos
          headerStyle <- createStyle(
            fontSize = 12,
            fontColour = "#FFFFFF",
            halign = "center",
            valign = "center",
            fgFill = "#28a745",
            border = "TopBottomLeftRight",
            textDecoration = "bold",
            wrapText = FALSE
          )
          
          bodyStyle <- createStyle(
            fontSize = 10,
            halign = "left",
            valign = "center",
            wrapText = FALSE,
            border = "TopBottomLeftRight",
            borderStyle = "thin"
          )
          
          # Obtener entidades únicas y ordenar
          entidades_unicas <- sort(unique(datos_pqrds$Entidad))
          
          showNotification(paste("Procesando", length(entidades_unicas), "entidades..."), 
                           type = "message", duration = 2)
          
          # Crear una hoja por cada entidad
          for (entidad in entidades_unicas) {
            # Filtrar datos de la entidad específica
            datos_entidad <- datos_pqrds %>%
              filter(Entidad == entidad) %>%
              select(-Entidad)  # Quitar columna Entidad ya que está en el nombre de la hoja
            
            # Validar que haya datos para esta entidad
            if (nrow(datos_entidad) == 0) {
              next  # Saltar si no hay datos
            }
            
            # Limpiar nombre para la hoja (máximo 31 caracteres, solo alfanuméricos)
            nombre_hoja <- iconv(entidad, to = "ASCII//TRANSLIT", sub = "")
            nombre_hoja <- gsub("[^A-Za-z0-9 _-]", "", nombre_hoja)
            nombre_hoja <- trimws(nombre_hoja)
            nombre_hoja <- substr(nombre_hoja, 1, 31)
            
            # Evitar nombres duplicados
            if (nombre_hoja %in% names(wb)) {
              nombre_hoja <- substr(nombre_hoja, 1, 28)
              nombre_hoja <- paste0(nombre_hoja, "_", match(entidad, entidades_unicas))
            }
            
            # Crear hoja
            addWorksheet(wb, nombre_hoja, gridLines = TRUE)
            
            # Escribir datos
            writeData(wb, nombre_hoja, datos_entidad,
                      startRow = 1, startCol = 1,
                      headerStyle = headerStyle,
                      borders = "all",
                      borderStyle = "thin")
            
            # Aplicar estilos
            addStyle(wb, nombre_hoja, headerStyle, 
                     rows = 1, cols = 1:ncol(datos_entidad), gridExpand = TRUE)
            
            if (nrow(datos_entidad) > 0) {
              addStyle(wb, nombre_hoja, bodyStyle, 
                       rows = 2:(nrow(datos_entidad) + 1),
                       cols = 1:ncol(datos_entidad), gridExpand = TRUE)
            }
            
            # Aplicar filtro automático
            addFilter(wb, nombre_hoja, row = 1, cols = 1:ncol(datos_entidad))
            
            # Ajustar anchos
            setColWidths(wb, nombre_hoja, cols = 1:ncol(datos_entidad), widths = "auto")
            
            # Congelar primera fila
            freezePane(wb, nombre_hoja, firstRow = TRUE)
          }
          
          # Guardar archivo
          saveWorkbook(wb, file, overwrite = TRUE)
          
          removeNotification("excel_progress")
          showNotification(
            paste0("Excel de PQRDS generado con ", length(entidades_unicas), " hojas (una por entidad)"), 
            type = "message", 
            duration = 5
          )
          
        }, error = function(e) {
          removeNotification("excel_progress")
          showNotification(
            paste("Error al generar Excel de PQRDS:", e$message), 
            type = "error", 
            duration = 10
          )
        })
      }
    )
    
    # ============================================
    # BOTÓN 2: DESCARGAR GESTIONES EXTEMPORÁNEAS
    # ============================================
    output$btn_descargar_extemporaneas <- downloadHandler(
      filename = function() {
        paste0("Gestiones_Extemporaneas_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      
      content = function(file) {
        showNotification("Generando Excel de Gestiones Extemporáneas...", 
                         type = "message", 
                         duration = NULL, 
                         id = "excel_progress_ext")
        
        tryCatch({
          # Obtener periodos seleccionados (si están disponibles)
          periodos <- NULL
          if (!is.null(periodos_seleccionados)) {
            periodos <- tryCatch(periodos_seleccionados(), error = function(e) NULL)
          }
          
          # Si no hay periodos, continuar sin filtrar
          if (is.null(periodos) || length(periodos) == 0) {
            showNotification("No hay periodos seleccionados, descargando todos los datos", 
                             type = "message", duration = 3, id = "excel_progress_ext")
          }
          
          # Validar datos
          if (!exists("sample_data") || is.null(sample_data)) {
            showNotification("No se encontró la base de datos 'sample_data'", 
                             type = "warning", duration = 5, id = "excel_progress_ext")
            return(NULL)
          }
          
          # Validar columnas
          if (!("tipo_gestion" %in% names(sample_data)) ||
              !("label" %in% names(sample_data)) ||
              !("dependencia" %in% names(sample_data)) ||
              !("entidad" %in% names(sample_data))) {
            showNotification(
              "El archivo sample_data no tiene las columnas esperadas", 
              type = "warning", duration = 5, id = "excel_progress_ext"
            )
            return(NULL)
          }
          
          # Filtrar gestiones extemporáneas con filtro de periodos (si aplica)
          datos_extemporaneas <- sample_data %>%
            filter(tipo_gestion == "Gestion extemporanea")
          
          # Aplicar filtro de periodo si está disponible
          if (!is.null(periodos) && length(periodos) > 0 && "fecha_ingreso" %in% names(datos_extemporaneas)) {
            datos_extemporaneas <- datos_extemporaneas %>%
              filter(periodo %in% periodos) %>%
              select(-periodo)
          }
          
          datos_extemporaneas <- datos_extemporaneas %>%
            mutate(
              Entidad = iconv(entidad, to = "ASCII//TRANSLIT", sub = ""),
              Dependencia = iconv(dependencia, to = "ASCII//TRANSLIT", sub = ""),
              # Formatear fecha como YYYY-MM-DD
              Fecha_Ingreso = format(as.Date(fecha_ingreso), "%Y-%m-%d")
            ) %>%
            select(
              Entidad,
              Fecha_Ingreso,
              Numero_Peticion = label,
              Dependencia
            ) %>%
            filter(!is.na(Entidad))
          
          if (nrow(datos_extemporaneas) == 0) {
            showNotification("No hay gestiones extemporáneas para procesar", 
                             type = "warning", duration = 5, id = "excel_progress_ext")
            return(NULL)
          }
          
          # Crear libro de Excel
          wb <- createWorkbook()
          
          # Estilos
          headerStyle <- createStyle(
            fontSize = 12,
            fontColour = "#FFFFFF",
            halign = "center",
            valign = "center",
            fgFill = "#28a745",
            border = "TopBottomLeftRight",
            textDecoration = "bold",
            wrapText = FALSE
          )
          
          bodyStyle <- createStyle(
            fontSize = 10,
            halign = "left",
            valign = "center",
            wrapText = FALSE,
            border = "TopBottomLeftRight",
            borderStyle = "thin"
          )
          
          # Obtener entidades únicas y ordenar
          entidades_unicas <- sort(unique(datos_extemporaneas$Entidad))
          
          showNotification(paste("Procesando", length(entidades_unicas), "entidades..."), 
                           type = "message", duration = 2)
          
          # Crear una hoja por cada entidad
          for (entidad in entidades_unicas) {
            # Filtrar datos de la entidad específica
            datos_entidad <- datos_extemporaneas %>%
              filter(Entidad == entidad) %>%
              select(-Entidad)  # Quitar columna Entidad
            
            # Validar que haya datos para esta entidad
            if (nrow(datos_entidad) == 0) {
              next  # Saltar si no hay datos
            }
            
            # Limpiar nombre para la hoja (máximo 31 caracteres)
            nombre_hoja <- iconv(entidad, to = "ASCII//TRANSLIT", sub = "")
            nombre_hoja <- gsub("[^A-Za-z0-9 _-]", "", nombre_hoja)
            nombre_hoja <- trimws(nombre_hoja)
            nombre_hoja <- substr(nombre_hoja, 1, 31)
            
            # Evitar nombres duplicados
            if (nombre_hoja %in% names(wb)) {
              nombre_hoja <- substr(nombre_hoja, 1, 28)
              nombre_hoja <- paste0(nombre_hoja, "_", match(entidad, entidades_unicas))
            }
            
            # Crear hoja
            addWorksheet(wb, nombre_hoja, gridLines = TRUE)
            
            # Escribir datos
            writeData(wb, nombre_hoja, datos_entidad,
                      startRow = 1, startCol = 1,
                      headerStyle = headerStyle,
                      borders = "all",
                      borderStyle = "thin")
            
            # Aplicar estilos
            addStyle(wb, nombre_hoja, headerStyle, 
                     rows = 1, cols = 1:ncol(datos_entidad), gridExpand = TRUE)
            
            if (nrow(datos_entidad) > 0) {
              addStyle(wb, nombre_hoja, bodyStyle, 
                       rows = 2:(nrow(datos_entidad) + 1),
                       cols = 1:ncol(datos_entidad), gridExpand = TRUE)
            }
            
            # Aplicar filtro automático
            addFilter(wb, nombre_hoja, row = 1, cols = 1:ncol(datos_entidad))
            
            # Ajustar anchos
            setColWidths(wb, nombre_hoja, cols = 1:ncol(datos_entidad), widths = "auto")
            
            # Congelar primera fila
            freezePane(wb, nombre_hoja, firstRow = TRUE)
          }
          
          # Guardar archivo
          saveWorkbook(wb, file, overwrite = TRUE)
          
          removeNotification("excel_progress_ext")
          showNotification(
            paste0("Excel de Gestiones Extemporáneas generado con ", length(entidades_unicas), " hojas"), 
            type = "message", 
            duration = 5
          )
          
        }, error = function(e) {
          removeNotification("excel_progress_ext")
          showNotification(
            paste("Error al generar Excel de Gestiones Extemporáneas:", e$message), 
            type = "error", 
            duration = 10
          )
        })
      }
    )
  })
}