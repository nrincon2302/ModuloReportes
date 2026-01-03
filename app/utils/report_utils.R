# utils/report_utils.R
# Utilidades para generación de reportes PDF con templates RMarkdown

library(rmarkdown)
library(knitr)

# ============================================
# PREPARACIÓN DE ENTORNO DE REPORTE (TEMPLATES)
# ============================================

preparar_entorno_reporte <- function(template, logo = NULL, parametros = list()) {
  if (!file.exists(template)) {
    stop("No se encontró el template de reporte")
  }
  
  temp_dir <- tempfile("reporte_")
  dir.create(temp_dir, recursive = TRUE)
  
  destino_template <- file.path(temp_dir, basename(template))
  file.copy(template, destino_template, overwrite = TRUE)
  
  if (!is.null(logo) && file.exists(logo)) {
    file.copy(logo, file.path(temp_dir, basename(logo)), overwrite = TRUE)
  }
  
  # Copiar tipografías al entorno temporal para que puedan ser referenciadas
  # por el template (especialmente para fuentes OTF personalizadas).
  fuente_dir <- "www/fonts"
  if (dir.exists(fuente_dir)) {
    dir.create(file.path(temp_dir, "fonts"), showWarnings = FALSE)
    fuentes <- list.files(fuente_dir, full.names = TRUE)
    if (length(fuentes) > 0) {
      file.copy(fuentes, file.path(temp_dir, "fonts"), overwrite = TRUE, recursive = FALSE)
    }
  }
  
  list(
    template = destino_template,
    parametros = parametros
  )
}

# ============================================
# GENERACIÓN DE REPORTE PARA ÍNDICE (TEMPLATE)
# ============================================

#' Genera el entorno de render para el reporte de índice utilizando el template
#' `templates/report_indice.Rmd` y adjuntando el logo institucional.
#'
#' @param datos_filtrados data.frame con las métricas calculadas (Indice, Pilar,
#'   Periodo, Canal/Subcanal y Nombre_Entidad)
#' @param nivel_consulta nivel de análisis seleccionado en el dashboard
#' @param sectores_seleccionados vector opcional de sectores filtrados
#' @param entidades_seleccionadas vector opcional de entidades filtradas
#' @param canal_seleccionado canal elegido (si aplica)
#' @param subcanales_seleccionados subcanales elegidos (si aplica)
#' @param detalle_canal bandera para activar vistas por canal/subcanal
#' @param titulo_ranking texto a mostrar sobre la sección de ranking
#'
#' @return lista con la ruta del template temporal y los parámetros listos para
#'   ser consumidos por rmarkdown::render.
#' @export

generar_reporte_indice <- function(
    datos_filtrados,
    nivel_consulta,
    sectores_seleccionados = NULL,
    entidades_seleccionadas = NULL,
    canal_seleccionado = NULL,
    subcanales_seleccionados = NULL,
    detalle_canal = FALSE,
    titulo_ranking = "Ranking de Entidades",
    indice_global = NULL,
    datos_pilar = NULL,
    datos_periodo = NULL,
    datos_canal = NULL,
    datos_entidad = NULL
) {
  parametros <- list(
    datos_filtrados = datos_filtrados,
    nivel_consulta = nivel_consulta,
    sectores_seleccionados = sectores_seleccionados,
    entidades_seleccionadas = entidades_seleccionadas,
    canal_seleccionado = canal_seleccionado,
    subcanales_seleccionados = subcanales_seleccionados,
    detalle_canal = detalle_canal,
    titulo_ranking = titulo_ranking,
    indice_global = indice_global,
    datos_pilar = datos_pilar,
    datos_periodo = datos_periodo,
    datos_canal = datos_canal,
    datos_entidad = datos_entidad
  )
  
  preparar_entorno_reporte(
    template = "templates/report_indice.Rmd",
    logo = "www/logo.png",
    parametros = parametros
  )
}

# ============================================
# GENERACIÓN DE REPORTE PARA DIMENSIÓN (TEMPLATE)
# ============================================

#' Genera el entorno de render para el reporte de dimensión utilizando
#' `templates/report_pilar.Rmd` y adjuntando el logo institucional.
#'
#' @param datos_dimension data.frame con el ranking de indicadores (columna
#'   Indicador y Valor)
#' @param datos_filtrados data.frame opcional con columnas Canal, Periodo o
#'   Criterio para graficar las vistas detalladas.
#' @param componente nombre del componente seleccionado
#' @param pilar_nombre nombre del pilar seleccionado
#' @param dimension_nombre nombre de la dimensión
#' @param indicador_seleccionado indicador activo
#' @param nivel_consulta nivel de análisis
#' @param sectores_seleccionados vector opcional de sectores filtrados
#' @param entidades_seleccionadas vector opcional de entidades filtradas
#'
#' @return lista con la ruta del template temporal y los parámetros listos para
#'   `rmarkdown::render`.
#' @export

generar_reporte_dimension <- function(
    datos_dimension,
    datos_filtrados,
    componente,
    pilar_nombre,
    dimension_nombre,
    indicador_seleccionado,
    nivel_consulta,
    sectores_seleccionados = NULL,
    entidades_seleccionadas = NULL
) {
  parametros <- list(
    componente = componente,
    pilar_nombre = pilar_nombre,
    dimension_nombre = dimension_nombre,
    indicador_seleccionado = indicador_seleccionado,
    nivel_consulta = nivel_consulta,
    sectores_seleccionados = sectores_seleccionados,
    entidades_seleccionadas = entidades_seleccionadas,
    datos_dimension = datos_dimension,
    datos_filtrados = datos_filtrados
  )
  
  preparar_entorno_reporte(
    template = "templates/report_pilar.Rmd",
    logo = "www/logo.png",
    parametros = parametros
  )
}

