# ===================================================
# Librerías requeridas
# ===================================================

# Lectura de bases de datos
library(readxl)
library(readr)

# Conexión a servidor ODK (usando httr en vez de ruODK)
library(httr)
library(jsonlite)

# Generación de gráficas
library(plotly)
library(ggplot2)
library(highcharter)
library(scales)

# Creación del visualizador
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(fresh)
library(htmltools)
library(dplyr)
library(tidyr)
library(purrr)
library(shinydashboard)

# Creación del PDF
library(rmarkdown)
library(knitr)
library(kableExtra)

# Apoyo a la modificación de formatos
library(stringr)

# Funciones auxiliares de ODK
source("utils/odk_helpers.R")

###### Directorio de trabajo
gc()
rm(list = ls())
#readRenviron(file.path(getwd(), ".Renviron"))


# ===========================================================
# FUNCIONES ODK CENTRAL (HTTR en vez de ruODK)
# ============================================================

# Función para obtener envíos de un formulario ODK Central vía OData
pid <- Sys.getenv("ODK_PROJECT_ID")
get_odk_submissions <- function(fid, project_id = pid) {

  # Configuración de credenciales
  base_url <- Sys.getenv("ODK_URL")
  email <- Sys.getenv("ODK_EMAIL")
  password <- Sys.getenv("ODK_PASSWORD")
    
  # Construir URL del endpoint OData
  endpoint <- sprintf(
    "%s/v1/projects/%s/forms/%s.svc/Submissions",
    base_url, project_id, fid
  )
  
  # Intentar obtener los datos con manejo de errores
  tryCatch({
    
    # Hacer petición GET con autenticación básica
    response <- httr::GET(
      url = endpoint,
      httr::authenticate(email, password, type = "basic"),
      httr::add_headers(
        "Accept" = "application/json",
        "Content-Type" = "application/json"
      ),
      httr::timeout(30)
    )
    
    # Verificar status code
    if (httr::status_code(response) != 200) {
      warning(sprintf(
        "Error al obtener datos del formulario %s. Status: %s",
        fid, httr::status_code(response)
      ))
      return(generar_df_vacio(fid))
    }
    
    # Parsear JSON con flatten para expandir estructuras anidadas
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, flatten = TRUE)
    
    # Verificar estructura de respuesta
    if (!"value" %in% names(json_data)) {
      warning(sprintf("Respuesta inesperada para formulario %s", fid))
      return(data.frame())
    }
    
    # Convertir a data.frame (flatten ya debería haber expandido las columnas)
    df <- as.data.frame(json_data$value, stringsAsFactors = FALSE)
    
    # Si está vacío, retornar data.frame vacío
    if (nrow(df) == 0) {
      warning(sprintf("No hay datos en el formulario %s", fid))
      return(generar_df_vacio(fid))
    }
    
    # NORMALIZAR NOMBRES DE COLUMNAS
    df <- normalize_odk_column_names(df)
    
    return(df)
    
  }, error = function(e) {
    warning(sprintf(
      "Error al procesar formulario %s: %s",
      fid, e$message
    ))
    return(generar_df_vacio(fid))
  })
}

# ==========================================================
# CARGA DE BASES DE DATOS DESDE EL MASTER
# ==========================================================

l_canal <- read_excel(path = "info/dic-MOD_tabular.xlsx", sheet = "Canal")

### Entidades
entidades_sector <- read_excel(path = "info/dic-MOD_tabular.xlsx", sheet = "Entidades") %>%
  select(id_entidad, entidad, sector)

l_sectores_id <- read_excel(path = "info/dic-MOD_tabular.xlsx", sheet = "Sector") %>%
  select(id_sector, sector)

l_entidades <- entidades_sector %>%
  left_join(l_sectores_id, by = "sector") %>%
  rename(`No.` = id_sector, SECTOR = sector, Id_Entidad = id_entidad, ENTIDAD = entidad) %>%
    select(`No.`, SECTOR, Id_Entidad, ENTIDAD)

rm(entidades_sector)
rm(l_sectores_id)
  
l_entidades_id <- read_excel(path = "info/dic-MOD_tabular.xlsx", sheet = "Entidades") %>%
  select(id_entidad, entidad) %>%
  rename(Entidad = entidad)

l_complemento_entidades <- read_excel(path = "info/Asignación.xlsx") %>%
  select(-Responsable, -idResponsable)

### Indicadores
l_ind <- read_excel(
  path = "info/Matriz indicadores.xlsx",
  sheet = "V4"
)

### Acciones y Herramientas de Apoyo
l_acciones_original <- read_excel(path = "info/Matriz de indicadores_consolidado.xlsx") %>%
  select(ID_INDICADOR, INDICADOR, `ACCIONES REQUERIDA`, `HERRAMIENTAS DE APOYO`)

l_criterios_sat <- read_excel(path = "info/Matriz Criterios 2.xlsx") %>%
  select(Id_Indicador, IdCriterio, Criterios) %>%
  rename(ID_INDICADOR = Id_Indicador, CRITERIOS = Criterios)

df_joined <- l_acciones_original %>%
  left_join(l_criterios_sat,  by = "ID_INDICADOR")


# LISTO EN FORMATO CERCANO AL FINAL
insumo_acciones_y_criterios <- read_excel("info/Matriz de acciones de mejora.xlsx") %>%
  fill(everything(), .direction = "down") %>%
  rename(Indicador = `Nombre Indicador`, Criterio = `Criterio \r\n(En caso que aplique)`) %>%
  select(Pilar, Dimensión, Indicador, IdCriterio, Criterio, `Acciones de mejora`, `Herramienta de Apoyo`) %>%
  left_join(l_ind %>% select(Indicador, Id_indicador), by = "Indicador") %>%
  rename(ID_INDICADOR = Id_indicador, 
         INDICADOR = Indicador, 
         `ACCIONES REQUERIDA` = `Acciones de mejora`,
         `HERRAMIENTAS DE APOYO` = `Herramienta de Apoyo`,
         CRITERIOS = Criterio) %>%
  select(
    ID_INDICADOR,
    INDICADOR,
    IdCriterio,
    CRITERIOS,
    `ACCIONES REQUERIDA`,
    `HERRAMIENTAS DE APOYO`
  )

l_acciones_1 <- insumo_acciones_y_criterios

l_acciones_2 <- df_joined %>%
  select(
    ID_INDICADOR,
    INDICADOR,
    IdCriterio,
    CRITERIOS,
    `ACCIONES REQUERIDA`,
    `HERRAMIENTAS DE APOYO`
  )

l_acciones <- bind_rows(l_acciones_1, l_acciones_2)

# =========================================================
# CONEXIÓN CON FASTAPI PARA MÓDULOS CON BASE DE DATOS
# =========================================================

backend_url <- Sys.getenv("FASTAPI_URL")
fa_username <- Sys.getenv("FASTAPI_USERNAME")
fa_password <- Sys.getenv("FASTAPI_PASSWORD")

autenticar_fastapi <- function() {
  res_login <- httr::POST(
    url = paste0(backend_url, "/auth/token"),
    body = list(username = fa_username, password = fa_password),
    encode = "form"
  )
  token <- httr::content(res_login)$access_token
}

enviar_acciones_criticas_a_fastapi <- function(df_entidad_acciones) {

  json_payload <- df_entidad_acciones %>%
  transmute(
    entidad = Entidad,
    criterio = Criterios,
    indicador = Indicador,
    accion = `Acciones Sugeridas`,
    insumo = Dimensión
  ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
  list(reportes = .) %>%
  toJSON(auto_unbox = TRUE, pretty = TRUE)

  # Enviar solicitud POST al servidor
  token <- autenticar_fastapi()
  del <- httr::DELETE(
    url = paste0(backend_url, "/reports"),
    httr::add_headers(
      "Authorization" = paste("Bearer", token)
    )
  )
  res <- httr::POST(
    url = paste0(backend_url, "/reports"),
    body = json_payload,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token)
    )
  )

  res$status_code
}


obtener_pqrds <- function() {
  token <- autenticar_fastapi()
  
  res <- httr::GET(
    url = paste0(backend_url, "/pqrds"),
    httr::add_headers("Authorization" = paste("Bearer", token))
  )
  
  # Error HTTP → tibble vacío
  if (httr::http_error(res)) {
    warning("Error al consultar /pqrds")
    return(tibble::tibble())
  }
  
  contenido_raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # Sin contenido → tibble vacío
  if (is.null(contenido_raw) || !nzchar(contenido_raw)) {
    return(tibble::tibble())
  }
  
  # Parseo del JSON
  data <- tryCatch(
    jsonlite::fromJSON(contenido_raw),
    error = function(e) NULL
  )
  
  # JSON inválido o vacío
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  # Convertir a tibble y retornar
  tibble::as_tibble(data)
}


obtener_habilidades <- function() {
  token <- autenticar_fastapi()
  
  res <- httr::GET(
    url = paste0(backend_url, "/habilidades"),
    httr::add_headers("Authorization" = paste("Bearer", token))
  )
  
  # Si falla el request
  if (httr::http_error(res)) {
    warning("Error al consultar /habilidades")
    return(tibble::tibble())
  }
  
  contenido_raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # Si viene vacío
  if (is.null(contenido_raw) || !nzchar(contenido_raw)) {
    return(tibble::tibble())
  }
  
  # Parseo seguro del JSON
  data <- tryCatch(
    jsonlite::fromJSON(contenido_raw),
    error = function(e) NULL
  )
  
  # Validación de estructura
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  # Retorna el dataframe tal cual
  tibble::as_tibble(data)
}
