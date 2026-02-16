library(shiny)
library(openxlsx)
library(dplyr)

# ============================================================================
# HELPERS INTERNOS
# ============================================================================

# Workbook con mensaje de advertencia para cuando no hay datos — evita
# descarga corrupta. El showNotification() amarillo en la UI se sigue usando.
.write_empty_workbook <- function(file,
                                   mensaje = "No hay datos disponibles para los filtros seleccionados.") {
  wb <- createWorkbook()
  addWorksheet(wb, "Sin Datos")
  writeData(wb, "Sin Datos", data.frame(Mensaje = mensaje),
            startRow = 3, startCol = 2, colNames = FALSE)
  addStyle(wb, "Sin Datos",
           createStyle(fontSize = 12, fgFill = "#FFF3CD", fontColour = "#856404",
                       border = "TopBottomLeftRight", borderColour = "#856404",
                       borderStyle = "medium", halign = "center", valign = "center",
                       textDecoration = "bold", wrapText = TRUE),
           rows = 3, cols = 2)
  setColWidths(wb, "Sin Datos", cols = 2, widths = 70)
  setRowHeights(wb, "Sin Datos", rows = 3, heights = 45)
  saveWorkbook(wb, file, overwrite = TRUE)
}

# Limpia un string para usarlo como nombre de hoja Excel (max 31 chars).
.limpiar_nombre_hoja <- function(nombre, max_len = 31L) {
  nombre <- iconv(nombre, to = "ASCII//TRANSLIT", sub = "")
  nombre <- gsub("[^A-Za-z0-9 _-]", "", nombre)
  nombre <- trimws(nombre)
  substr(nombre, 1L, max_len)
}

# Pre-calcula los nombres de hoja para un vector de entidades SIN necesitar
# el workbook. Esto permite crear la hoja General PRIMERO (con sus HYPERLINKs)
# y luego las hojas de entidad — la General queda activa al abrir el archivo.
.precompute_nombres_hojas <- function(entidades_ordenadas) {
  nombres <- setNames(character(length(entidades_ordenadas)), entidades_ordenadas)
  usados  <- character(0L)
  for (i in seq_along(entidades_ordenadas)) {
    ent  <- entidades_ordenadas[i]
    base <- .limpiar_nombre_hoja(ent)
    nm   <- substr(base, 1L, 31L)
    if (nm %in% usados) nm <- substr(paste0(substr(base, 1L, 28L), "_", i), 1L, 31L)
    nombres[ent] <- nm
    usados <- c(usados, nm)
  }
  nombres
}

# Coleccion de estilos compartidos.
.estilos <- function() {
  list(
    header = createStyle(
      fontSize = 11, fontColour = "#FFFFFF", halign = "center", valign = "center",
      fgFill = "#28a745", border = "TopBottomLeftRight",
      textDecoration = "bold", wrapText = FALSE),
    body = createStyle(
      fontSize = 10, halign = "left", valign = "center", wrapText = FALSE,
      border = "TopBottomLeftRight", borderStyle = "thin"),
    title = createStyle(
      fontSize = 13, fontColour = "#FFFFFF", halign = "center", valign = "center",
      fgFill = "#155724", textDecoration = "bold"),
    total_ent = createStyle(
      fontSize = 11, fontColour = "#FFFFFF", fgFill = "#1F6B3A",
      halign = "left", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "medium", textDecoration = "bold"),
    total_cnt = createStyle(
      fontSize = 11, fontColour = "#FFFFFF", fgFill = "#1F6B3A",
      halign = "center", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "medium", textDecoration = "bold"),
    total_pct = createStyle(
      numFmt = "0.00%", fontSize = 11, fontColour = "#FFFFFF", fgFill = "#1F6B3A",
      halign = "center", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "medium", textDecoration = "bold"),
    data_ent = createStyle(
      fontSize = 10, halign = "left", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "thin"),
    data_cnt = createStyle(
      fontSize = 10, halign = "center", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "thin"),
    data_pct = createStyle(
      numFmt = "0.00%", fontSize = 10, halign = "center", valign = "center",
      border = "TopBottomLeftRight", borderStyle = "thin")
  )
}

# ============================================================================
# Hoja "General" para PQRSD Calificadas
#
# CLAVE: se llama ANTES de crear las hojas de entidad para que sea la primera
# hoja del workbook y quede activa al abrir el archivo sin ningun codigo extra.
#
# Todos los valores se calculan en R y se escriben como datos directos — sin
# formulas cruzadas entre hojas — lo que resuelve el problema de "habilitar
# edicion" en Vista Protegida. Los HYPERLINK() si funcionan en Vista Protegida
# porque son navegacion, no calculo.
#
# Estructura de la hoja:
#   Fila 1     : Titulo fusionado (1:13)
#   Fila 2     : (vacia)
#   Fila 3     : Encabezados nivel-1
#                ENTIDAD(f) | MUESTRA(f) | criterio×2cols (×5) | TOTAL INC(f)
#   Fila 4     : Encabezados nivel-2  →  Cumple | Incumple por criterio
#   Filas 5+   : 2 filas por entidad  →  conteos / porcentajes
#   Filas TOTAL: 2 filas              →  TOTAL conteos / TOTAL %
#   Seccion INDICES DE CALIDAD + META
# ============================================================================
.agregar_hoja_general_pqrsd <- function(wb, datos_pqrds, nombres_hojas, est) {

  CRITERIOS <- data.frame(
    label = c("COHERENCIA", "CLARIDAD", "CALIDEZ", "OPORTUNIDAD", "MANEJO DEL SISTEMA"),
    col_r = c("COHERENCIA", "CLARIDAD", "CALIDEZ", "OPORTUNIDAD", "MANEJO_SISTEMA"),
    stringsAsFactors = FALSE
  )
  n_crit <- nrow(CRITERIOS)
  n_col  <- 2L + 2L * n_crit + 1L  # 13 columnas en total

  col_cumple_k   <- function(k) 2L + (k - 1L) * 2L + 1L  # 3, 5, 7, 9, 11
  col_incumple_k <- function(k) col_cumple_k(k) + 1L       # 4, 6, 8, 10, 12
  COL_TOTAL_INC  <- n_col                                   # 13

  FILA_TITULO   <- 1L
  FILA_H1       <- 3L
  FILA_H2       <- 4L
  FILA_DATA_INI <- 5L

  entidades_ord <- sort(unique(datos_pqrds$Entidad))
  n_ent <- length(entidades_ord)

  fila_cnt <- function(i) FILA_DATA_INI + (i - 1L) * 2L
  fila_pct <- function(i) fila_cnt(i) + 1L

  FILA_TOTAL_CNT <- FILA_DATA_INI + n_ent * 2L
  FILA_TOTAL_PCT <- FILA_TOTAL_CNT + 1L
  FILA_IDX_CUM   <- FILA_TOTAL_PCT + 2L
  FILA_IDX_INC   <- FILA_IDX_CUM  + 1L
  FILA_META      <- FILA_IDX_INC  + 2L

  # ── Calcular todos los valores en R ─────────────────────────────────────────
  n_cumple_col <- function(col_r, df) sum(df[[col_r]] == "Cumple", na.rm = TRUE)

  # Peticiones con al menos un criterio incumplido (union, no suma)
  falla_alguno <- function(df) {
    Reduce(`|`, lapply(CRITERIOS$col_r, function(cr) {
      v <- df[[cr]] != "Cumple"; v[is.na(v)] <- FALSE; v
    }))
  }

  ent_data <- lapply(entidades_ord, function(ent) {
    df <- datos_pqrds[datos_pqrds$Entidad == ent, ]
    n  <- nrow(df)
    crit <- lapply(seq_len(n_crit), function(k) {
      nc <- n_cumple_col(CRITERIOS$col_r[k], df)
      ni <- n - nc
      list(cumple = nc, incumple = ni,
           pct_c  = if (n > 0L) nc / n else NA_real_,
           pct_i  = if (n > 0L) ni / n else NA_real_)
    })
    list(n = n, criterios = crit, total_inc = sum(falla_alguno(df)))
  })
  names(ent_data) <- entidades_ord

  muestra_total    <- nrow(datos_pqrds)
  total_inc_global <- sum(falla_alguno(datos_pqrds))

  tot_crit <- lapply(seq_len(n_crit), function(k) {
    nc <- n_cumple_col(CRITERIOS$col_r[k], datos_pqrds)
    ni <- muestra_total - nc
    list(cumple = nc, incumple = ni,
         pct_c  = nc / muestra_total,
         pct_i  = ni / muestra_total)
  })

  # Indices de calidad (replica logica del Excel: G8 = 100% - Q5/F5)
  META       <- 0.88
  indice_cum <- 1 - total_inc_global / muestra_total
  indice_inc <- 1 - indice_cum
  calific    <- if (indice_cum < META) "Malo" else if (indice_cum < 1) "Bueno" else "Excelente"
  calific_hex <- switch(calific,
                        "Excelente" = "#1E8449",
                        "Bueno"     = "#28a745",
                        "Malo"      = "#C0392B")

  # ── Estilos propios de esta hoja ────────────────────────────────────────────
  hyperlink_style <- createStyle(
    fontColour = "#0563C1", textDecoration = "underline",
    halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin")
  calific_style <- createStyle(
    fontSize = 11, fontColour = "#FFFFFF", fgFill = calific_hex,
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin", textDecoration = "bold")
  idx_label_style <- createStyle(
    fontSize = 10, fontColour = "#FFFFFF", fgFill = "#155724",
    halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin", textDecoration = "bold")
  idx_val_style <- createStyle(
    numFmt = "0.00%", fontSize = 12, fontColour = "#FFFFFF", fgFill = "#1F6B3A",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "medium", textDecoration = "bold")
  meta_label_style <- createStyle(
    fontSize = 10, fontColour = "#856404", fgFill = "#FFF3CD",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "medium",
    textDecoration = "bold", wrapText = TRUE)
  meta_val_style <- createStyle(
    numFmt = "0%", fontSize = 12, fontColour = "#FFFFFF", fgFill = "#856404",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "medium", textDecoration = "bold")

  # ── Crear hoja ──────────────────────────────────────────────────────────────
  addWorksheet(wb, "General", gridLines = FALSE)

  # Titulo
  writeData(wb, "General", "RESUMEN DE CALIDAD DE LAS RESPUESTAS",
            startRow = FILA_TITULO, startCol = 1)
  mergeCells(wb, "General", cols = 1:n_col, rows = FILA_TITULO)
  addStyle(wb, "General", est$title, rows = FILA_TITULO, cols = 1)
  setRowHeights(wb, "General", rows = FILA_TITULO, heights = 30)

  # Encabezados nivel-1: ENTIDAD y MUESTRA (fusionados H1:H2)
  writeData(wb, "General", "ENTIDAD", startRow = FILA_H1, startCol = 1)
  mergeCells(wb, "General", cols = 1, rows = FILA_H1:FILA_H2)
  addStyle(wb, "General", est$header, rows = FILA_H1:FILA_H2, cols = 1, gridExpand = TRUE)

  writeData(wb, "General", "MUESTRA", startRow = FILA_H1, startCol = 2)
  mergeCells(wb, "General", cols = 2, rows = FILA_H1:FILA_H2)
  addStyle(wb, "General", est$header, rows = FILA_H1:FILA_H2, cols = 2, gridExpand = TRUE)

  # Criterios: nombre en H1 (fusionado 2 cols), Cumple/Incumple en H2
  for (k in seq_len(n_crit)) {
    cc <- col_cumple_k(k); ci <- col_incumple_k(k)
    writeData(wb, "General", CRITERIOS$label[k], startRow = FILA_H1, startCol = cc)
    mergeCells(wb, "General", cols = cc:ci, rows = FILA_H1)
    addStyle(wb, "General", est$header, rows = FILA_H1, cols = cc:ci, gridExpand = TRUE)
    writeData(wb, "General", "Cumple",   startRow = FILA_H2, startCol = cc)
    writeData(wb, "General", "Incumple", startRow = FILA_H2, startCol = ci)
    addStyle(wb, "General", est$header, rows = FILA_H2, cols = c(cc, ci), gridExpand = TRUE)
  }

  # Total Incumplimiento: encabezado fusionado H1:H2 (sin sub-cols, igual al original)
  writeData(wb, "General", "TOTAL PETICIONES CON INCUMPLIMIENTO",
            startRow = FILA_H1, startCol = COL_TOTAL_INC)
  mergeCells(wb, "General", cols = COL_TOTAL_INC, rows = FILA_H1:FILA_H2)
  addStyle(wb, "General", est$header,
           rows = FILA_H1:FILA_H2, cols = COL_TOTAL_INC, gridExpand = TRUE)
  setRowHeights(wb, "General", rows = c(FILA_H1, FILA_H2), heights = 22)

  # ── Filas de entidades (2 filas: conteos + %) ──────────────────────────────
  for (i in seq_len(n_ent)) {
    ent <- entidades_ord[i]
    d   <- ent_data[[ent]]
    fc  <- fila_cnt(i)
    fp  <- fila_pct(i)
    nh  <- nombres_hojas[ent]

    # Nombre de entidad con HYPERLINK — funciona en Vista Protegida (navegacion)
    ent_safe <- gsub('"', "'", ent)
    writeFormula(wb, "General",
                 x = paste0('=HYPERLINK("#\'', nh, '\'!A1","', ent_safe, '")'),
                 startRow = fc, startCol = 1)
    mergeCells(wb, "General", cols = 1, rows = fc:fp)
    addStyle(wb, "General", hyperlink_style, rows = fc:fp, cols = 1, gridExpand = TRUE)

    # Muestra: fusionada en 2 filas, sin porcentaje (igual al original F5:F6)
    writeData(wb, "General", d$n, startRow = fc, startCol = 2)
    mergeCells(wb, "General", cols = 2, rows = fc:fp)
    addStyle(wb, "General", est$data_cnt, rows = fc:fp, cols = 2, gridExpand = TRUE)

    # Criterios: conteos en fila cnt, porcentajes en fila pct
    for (k in seq_len(n_crit)) {
      cc <- col_cumple_k(k); ci <- col_incumple_k(k); cr <- d$criterios[[k]]
      writeData(wb, "General", cr$cumple,   startRow = fc, startCol = cc)
      writeData(wb, "General", cr$incumple, startRow = fc, startCol = ci)
      addStyle(wb, "General", est$data_cnt, rows = fc, cols = c(cc, ci), gridExpand = TRUE)
      writeData(wb, "General", cr$pct_c, startRow = fp, startCol = cc)
      writeData(wb, "General", cr$pct_i, startRow = fp, startCol = ci)
      addStyle(wb, "General", est$data_pct, rows = fp, cols = c(cc, ci), gridExpand = TRUE)
    }

    # Total Inc: fusionado en 2 filas, solo conteo (igual al original Q5:Q6)
    writeData(wb, "General", d$total_inc, startRow = fc, startCol = COL_TOTAL_INC)
    mergeCells(wb, "General", cols = COL_TOTAL_INC, rows = fc:fp)
    addStyle(wb, "General", est$data_cnt, rows = fc:fp, cols = COL_TOTAL_INC, gridExpand = TRUE)

    setRowHeights(wb, "General", rows = c(fc, fp), heights = 18)
  }

  # ── Fila TOTAL ──────────────────────────────────────────────────────────────
  writeData(wb, "General", "TOTAL", startRow = FILA_TOTAL_CNT, startCol = 1)
  mergeCells(wb, "General", cols = 1, rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT)
  addStyle(wb, "General", est$total_ent,
           rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT, cols = 1, gridExpand = TRUE)

  writeData(wb, "General", muestra_total, startRow = FILA_TOTAL_CNT, startCol = 2)
  mergeCells(wb, "General", cols = 2, rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT)
  addStyle(wb, "General", est$total_cnt,
           rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT, cols = 2, gridExpand = TRUE)

  for (k in seq_len(n_crit)) {
    cc <- col_cumple_k(k); ci <- col_incumple_k(k); tr <- tot_crit[[k]]
    writeData(wb, "General", tr$cumple,   startRow = FILA_TOTAL_CNT, startCol = cc)
    writeData(wb, "General", tr$incumple, startRow = FILA_TOTAL_CNT, startCol = ci)
    addStyle(wb, "General", est$total_cnt,
             rows = FILA_TOTAL_CNT, cols = c(cc, ci), gridExpand = TRUE)
    writeData(wb, "General", tr$pct_c, startRow = FILA_TOTAL_PCT, startCol = cc)
    writeData(wb, "General", tr$pct_i, startRow = FILA_TOTAL_PCT, startCol = ci)
    addStyle(wb, "General", est$total_pct,
             rows = FILA_TOTAL_PCT, cols = c(cc, ci), gridExpand = TRUE)
  }

  writeData(wb, "General", total_inc_global, startRow = FILA_TOTAL_CNT, startCol = COL_TOTAL_INC)
  mergeCells(wb, "General", cols = COL_TOTAL_INC, rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT)
  addStyle(wb, "General", est$total_cnt,
           rows = FILA_TOTAL_CNT:FILA_TOTAL_PCT, cols = COL_TOTAL_INC, gridExpand = TRUE)
  setRowHeights(wb, "General", rows = c(FILA_TOTAL_CNT, FILA_TOTAL_PCT), heights = 22)

  # ── Seccion INDICES DE CALIDAD (replica logica G8/O8/H8/O11 del original) ──
  # Indice Cumplimiento = 100% - TotalInc / Muestra
  writeData(wb, "General", "INDICE CUMPLIMIENTO DE CALIDAD:",
            startRow = FILA_IDX_CUM, startCol = 1)
  mergeCells(wb, "General", cols = 1:3, rows = FILA_IDX_CUM)
  addStyle(wb, "General", idx_label_style, rows = FILA_IDX_CUM, cols = 1)
  writeData(wb, "General", indice_cum, startRow = FILA_IDX_CUM, startCol = 4)
  addStyle(wb, "General", idx_val_style, rows = FILA_IDX_CUM, cols = 4)
  writeData(wb, "General", calific, startRow = FILA_IDX_CUM, startCol = 5)
  addStyle(wb, "General", calific_style, rows = FILA_IDX_CUM, cols = 5)

  # Indice Incumplimiento = 100% - Indice Cumplimiento
  writeData(wb, "General", "INDICE INCUMPLIMIENTO DE CALIDAD:",
            startRow = FILA_IDX_INC, startCol = 1)
  mergeCells(wb, "General", cols = 1:3, rows = FILA_IDX_INC)
  addStyle(wb, "General", idx_label_style, rows = FILA_IDX_INC, cols = 1)
  writeData(wb, "General", indice_inc, startRow = FILA_IDX_INC, startCol = 4)
  addStyle(wb, "General", idx_val_style, rows = FILA_IDX_INC, cols = 4)

  setRowHeights(wb, "General", rows = c(FILA_IDX_CUM, FILA_IDX_INC), heights = 24)

  # META POLITICA PUBLICA
  writeData(wb, "General",
            "META POLITICA PUBLICA DISTRITAL DE SERVICIO A LA CIUDADANIA",
            startRow = FILA_META, startCol = 1)
  mergeCells(wb, "General", cols = 1:9, rows = FILA_META)
  addStyle(wb, "General", meta_label_style, rows = FILA_META, cols = 1)
  writeData(wb, "General", META, startRow = FILA_META, startCol = 10)
  mergeCells(wb, "General", cols = 10:n_col, rows = FILA_META)
  addStyle(wb, "General", meta_val_style, rows = FILA_META, cols = 10)
  setRowHeights(wb, "General", rows = FILA_META, heights = 30)

  # ── Anchos de columna ───────────────────────────────────────────────────────
  setColWidths(wb, "General", cols = 1,             widths = 36)
  setColWidths(wb, "General", cols = 2,             widths = 12)
  setColWidths(wb, "General", cols = 3:12,          widths = 14)
  setColWidths(wb, "General", cols = COL_TOTAL_INC, widths = 20)

  freezePane(wb, "General", firstActiveRow = FILA_DATA_INI)
}

# ============================================================================
# Hoja "General" para Gestiones Extemporaneas
#
# Creada PRIMERO para ser la hoja activa al abrir el archivo.
# Valores calculados en R; HYPERLINK para navegar a cada entidad.
# ============================================================================
.agregar_hoja_general_extemporaneas <- function(wb, datos_ext, nombres_hojas, est) {

  entidades_ord <- sort(unique(datos_ext$Entidad))
  n_ent         <- length(entidades_ord)

  FILA_TITULO   <- 1L
  FILA_HEADER   <- 3L
  FILA_DATA_INI <- 4L
  FILA_TOTAL    <- FILA_DATA_INI + n_ent

  ent_counts    <- table(datos_ext$Entidad)
  muestra_total <- nrow(datos_ext)

  hyperlink_style <- createStyle(
    fontColour = "#0563C1", textDecoration = "underline",
    halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin")

  addWorksheet(wb, "General", gridLines = FALSE)

  # Titulo
  writeData(wb, "General", "RESUMEN DE GESTIONES EXTEMPORANEAS",
            startRow = FILA_TITULO, startCol = 1)
  mergeCells(wb, "General", cols = 1:2, rows = FILA_TITULO)
  addStyle(wb, "General", est$title, rows = FILA_TITULO, cols = 1)
  setRowHeights(wb, "General", rows = FILA_TITULO, heights = 30)

  # Encabezados
  writeData(wb, "General", "ENTIDAD",             startRow = FILA_HEADER, startCol = 1)
  writeData(wb, "General", "TOTAL EXTEMPORANEAS", startRow = FILA_HEADER, startCol = 2)
  addStyle(wb, "General", est$header, rows = FILA_HEADER, cols = 1:2, gridExpand = TRUE)
  setRowHeights(wb, "General", rows = FILA_HEADER, heights = 22)

  # Filas de entidades con HYPERLINK
  for (i in seq_along(entidades_ord)) {
    ent  <- entidades_ord[i]
    nh   <- nombres_hojas[ent]
    fila <- FILA_DATA_INI + (i - 1L)

    ent_safe <- gsub('"', "'", ent)
    writeFormula(wb, "General",
                 x = paste0('=HYPERLINK("#\'', nh, '\'!A1","', ent_safe, '")'),
                 startRow = fila, startCol = 1)
    addStyle(wb, "General", hyperlink_style, rows = fila, cols = 1)

    writeData(wb, "General", as.integer(ent_counts[ent]), startRow = fila, startCol = 2)
    addStyle(wb, "General", est$data_cnt, rows = fila, cols = 2)
    setRowHeights(wb, "General", rows = fila, heights = 18)
  }

  # Fila TOTAL
  writeData(wb, "General", "TOTAL", startRow = FILA_TOTAL, startCol = 1)
  addStyle(wb, "General", est$total_ent, rows = FILA_TOTAL, cols = 1)
  writeData(wb, "General", muestra_total, startRow = FILA_TOTAL, startCol = 2)
  addStyle(wb, "General", est$total_cnt, rows = FILA_TOTAL, cols = 2)
  setRowHeights(wb, "General", rows = FILA_TOTAL, heights = 22)

  setColWidths(wb, "General", cols = 1, widths = 40)
  setColWidths(wb, "General", cols = 2, widths = 24)
  freezePane(wb, "General", firstActiveRow = FILA_DATA_INI)
}

# ============================================================================
# UI
# ============================================================================
descargar_excel_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "margin-top: 15px;",
    downloadButton(ns("btn_descargar_pqrds"), "Descargar PQRSD Evaluadas",
                   icon = icon("file-excel"), class = "btn-success",
                   style = "font-weight: bold; width: 100%; margin-bottom: 10px;"),
    downloadButton(ns("btn_descargar_extemporaneas"), "Descargar Gestiones Extemporaneas",
                   icon = icon("file-excel"), class = "btn-success",
                   style = "font-weight: bold; width: 100%;")
  )
}

# ============================================================================
# SERVER
# ============================================================================
descargar_excel_server <- function(id, periodos_seleccionados = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # BOTON 1: PQRSD CALIFICADAS
    # ========================================================================
    output$btn_descargar_pqrds <- downloadHandler(
      filename = function() {
        paste0("PQRDS_por_entidad_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        showNotification("Generando Excel de PQRDS...",
                         type = "message", duration = NULL, id = "excel_progress")
        tryCatch({

          # Periodos
          periodos <- NULL
          if (!is.null(periodos_seleccionados))
            periodos <- tryCatch(periodos_seleccionados(), error = function(e) NULL)
          if (is.null(periodos) || length(periodos) == 0)
            showNotification("No hay periodos seleccionados, descargando todos los datos",
                             type = "message", duration = 3, id = "excel_progress")

          # Validaciones
          if (!exists("base_pqrds")) {
            showNotification("No se encontro la base de datos 'base_pqrds'",
                             type = "warning", duration = 5, id = "excel_progress")
            .write_empty_workbook(file, "No se encontro la base de datos 'base_pqrds'.")
            return(invisible(NULL))
          }
          if (!exists("df_entidades")) {
            showNotification("No se encontro el catalogo 'df_entidades'",
                             type = "warning", duration = 5, id = "excel_progress")
            .write_empty_workbook(file, "No se encontro el catalogo 'df_entidades'.")
            return(invisible(NULL))
          }
          cols_req  <- c("mod1_gv4_p8", "mod1_gv4_p4", "mod1_gv4_p9",
                         "mod2_mod2_1_v22", "mod2_mod2_1_v23",
                         "mod2_mod2_1_v24", "mod2_mod2_1_v26", "mod2_mod2_1_v25")
          cols_falt <- setdiff(cols_req, names(base_pqrds))
          if (length(cols_falt) > 0) {
            msg <- paste("Faltan columnas:", paste(cols_falt, collapse = ", "))
            showNotification(msg, type = "warning", duration = 5, id = "excel_progress")
            .write_empty_workbook(file, msg)
            return(invisible(NULL))
          }

          # Procesar datos
          datos <- base_pqrds
          if (!is.null(periodos) && length(periodos) > 0 && "periodo" %in% names(datos))
            datos <- datos %>% filter(periodo %in% periodos)

          datos <- datos %>%
            mutate(mod1_gv4_p8 = as.numeric(mod1_gv4_p8)) %>%
            left_join(df_entidades %>% select(Id_Entidad, Entidad),
                      by = c("mod1_gv4_p8" = "Id_Entidad")) %>%
            mutate(
              Entidad       = iconv(Entidad,     to = "ASCII//TRANSLIT", sub = ""),
              mod1_gv4_p9   = iconv(mod1_gv4_p9, to = "ASCII//TRANSLIT", sub = ""),
              FECHA_INGRESO = format(as.Date(mod1_p2), "%Y-%m-%d")
            ) %>%
            select(Entidad, FECHA_INGRESO,
                   NO_PETICION           = mod1_gv4_p4,
                   DEPENDENCIA_RESPUESTA = mod1_gv4_p9,
                   COHERENCIA            = mod2_mod2_1_v22,
                   CLARIDAD              = mod2_mod2_1_v23,
                   CALIDEZ               = mod2_mod2_1_v24,
                   OPORTUNIDAD           = mod2_mod2_1_v26,
                   MANEJO_SISTEMA        = mod2_mod2_1_v25) %>%
            filter(!is.na(Entidad))

          if (nrow(datos) == 0) {
            showNotification("No hay datos de PQRDS para los filtros seleccionados.",
                             type = "warning", duration = 5, id = "excel_progress")
            .write_empty_workbook(file, "No hay datos de PQRSD Calificadas para los filtros seleccionados.")
            return(invisible(NULL))
          }

          # Pre-calcular nombres de hoja ANTES de crear el workbook
          entidades_unicas <- sort(unique(datos$Entidad))
          nombres_hojas    <- .precompute_nombres_hojas(entidades_unicas)

          showNotification(paste("Procesando", length(entidades_unicas), "entidades..."),
                           type = "message", duration = 2)

          wb  <- createWorkbook()
          est <- .estilos()

          # Hoja General PRIMERO → queda activa al abrir el archivo
          .agregar_hoja_general_pqrsd(wb, datos, nombres_hojas, est)

          # Hojas de entidad (creadas despues de General)
          for (i in seq_along(entidades_unicas)) {
            entidad     <- entidades_unicas[i]
            nombre_hoja <- nombres_hojas[entidad]
            datos_ent   <- datos %>% filter(Entidad == entidad) %>% select(-Entidad)
            if (nrow(datos_ent) == 0) next

            addWorksheet(wb, nombre_hoja, gridLines = TRUE)
            writeData(wb, nombre_hoja, datos_ent, startRow = 1, startCol = 1,
                      headerStyle = est$header, borders = "all", borderStyle = "thin")
            addStyle(wb, nombre_hoja, est$header,
                     rows = 1, cols = 1:ncol(datos_ent), gridExpand = TRUE)
            addStyle(wb, nombre_hoja, est$body,
                     rows = 2:(nrow(datos_ent) + 1),
                     cols = 1:ncol(datos_ent), gridExpand = TRUE)
            addFilter(wb, nombre_hoja, row = 1, cols = 1:ncol(datos_ent))
            setColWidths(wb, nombre_hoja, cols = 1:ncol(datos_ent), widths = "auto")
            freezePane(wb, nombre_hoja, firstRow = TRUE)
          }

          saveWorkbook(wb, file, overwrite = TRUE)
          removeNotification("excel_progress")
          showNotification(
            paste0("Excel generado: hoja General + ", length(entidades_unicas), " hojas por entidad."),
            type = "message", duration = 5)

        }, error = function(e) {
          removeNotification("excel_progress")
          showNotification(paste("Error al generar Excel de PQRDS:", e$message),
                           type = "error", duration = 10)
          tryCatch(.write_empty_workbook(file, paste("Error interno:", e$message)),
                   error = function(e2) NULL)
        })
      }
    )

    # ========================================================================
    # BOTON 2: GESTIONES EXTEMPORANEAS
    # ========================================================================
    output$btn_descargar_extemporaneas <- downloadHandler(
      filename = function() {
        paste0("Gestiones_Extemporaneas_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        showNotification("Generando Excel de Gestiones Extemporaneas...",
                         type = "message", duration = NULL, id = "excel_progress_ext")
        tryCatch({

          # Periodos
          periodos <- NULL
          if (!is.null(periodos_seleccionados))
            periodos <- tryCatch(periodos_seleccionados(), error = function(e) NULL)
          if (is.null(periodos) || length(periodos) == 0)
            showNotification("No hay periodos seleccionados, descargando todos los datos",
                             type = "message", duration = 3, id = "excel_progress_ext")

          # Validaciones
          if (!exists("sample_data") || is.null(sample_data)) {
            showNotification("No se encontro la base de datos 'sample_data'",
                             type = "warning", duration = 5, id = "excel_progress_ext")
            .write_empty_workbook(file, "No se encontro la base de datos 'sample_data'.")
            return(invisible(NULL))
          }
          cols_req  <- c("tipo_gestion", "label", "dependencia", "entidad")
          cols_falt <- setdiff(cols_req, names(sample_data))
          if (length(cols_falt) > 0) {
            msg <- paste("Faltan columnas en sample_data:", paste(cols_falt, collapse = ", "))
            showNotification(msg, type = "warning", duration = 5, id = "excel_progress_ext")
            .write_empty_workbook(file, msg)
            return(invisible(NULL))
          }

          # Filtrar extemporaneas
          datos <- sample_data %>% filter(tipo_gestion == "Gestion extemporanea")
          if (!is.null(periodos) && length(periodos) > 0 && "periodo" %in% names(datos))
            datos <- datos %>% filter(periodo %in% periodos) %>% select(-periodo)

          datos <- datos %>%
            mutate(
              Entidad       = iconv(entidad,     to = "ASCII//TRANSLIT", sub = ""),
              Dependencia   = iconv(dependencia, to = "ASCII//TRANSLIT", sub = ""),
              Fecha_Ingreso = format(as.Date(fecha_ingreso), "%Y-%m-%d")
            ) %>%
            select(Entidad, Fecha_Ingreso, Numero_Peticion = label, Dependencia) %>%
            filter(!is.na(Entidad))

          if (nrow(datos) == 0) {
            showNotification("No hay gestiones extemporaneas para los filtros seleccionados.",
                             type = "warning", duration = 5, id = "excel_progress_ext")
            .write_empty_workbook(file, "No hay Gestiones Extemporaneas para los filtros seleccionados.")
            return(invisible(NULL))
          }

          # Pre-calcular nombres de hoja
          entidades_unicas <- sort(unique(datos$Entidad))
          nombres_hojas    <- .precompute_nombres_hojas(entidades_unicas)

          showNotification(paste("Procesando", length(entidades_unicas), "entidades..."),
                           type = "message", duration = 2)

          wb  <- createWorkbook()
          est <- .estilos()

          # Hoja General PRIMERO → queda activa al abrir el archivo
          .agregar_hoja_general_extemporaneas(wb, datos, nombres_hojas, est)

          # Hojas de entidad
          for (i in seq_along(entidades_unicas)) {
            entidad     <- entidades_unicas[i]
            nombre_hoja <- nombres_hojas[entidad]
            datos_ent   <- datos %>% filter(Entidad == entidad) %>% select(-Entidad)
            if (nrow(datos_ent) == 0) next

            addWorksheet(wb, nombre_hoja, gridLines = TRUE)
            writeData(wb, nombre_hoja, datos_ent, startRow = 1, startCol = 1,
                      headerStyle = est$header, borders = "all", borderStyle = "thin")
            addStyle(wb, nombre_hoja, est$header,
                     rows = 1, cols = 1:ncol(datos_ent), gridExpand = TRUE)
            addStyle(wb, nombre_hoja, est$body,
                     rows = 2:(nrow(datos_ent) + 1),
                     cols = 1:ncol(datos_ent), gridExpand = TRUE)
            addFilter(wb, nombre_hoja, row = 1, cols = 1:ncol(datos_ent))
            setColWidths(wb, nombre_hoja, cols = 1:ncol(datos_ent), widths = "auto")
            freezePane(wb, nombre_hoja, firstRow = TRUE)
          }

          saveWorkbook(wb, file, overwrite = TRUE)
          removeNotification("excel_progress_ext")
          showNotification(
            paste0("Excel generado: hoja General + ", length(entidades_unicas), " hojas por entidad."),
            type = "message", duration = 5)

        }, error = function(e) {
          removeNotification("excel_progress_ext")
          showNotification(paste("Error al generar Excel de Gestiones Extemporaneas:", e$message),
                           type = "error", duration = 10)
          tryCatch(.write_empty_workbook(file, paste("Error interno:", e$message)),
                   error = function(e2) NULL)
        })
      }
    )
  })
}