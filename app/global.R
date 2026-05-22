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

# ---------------------------------------------------------
# Helper de logging ODK: escribe en consola Y en archivo de log
# ---------------------------------------------------------
.log_odk <- function(level = "INFO", msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entrada   <- sprintf("[%s] [ODK-%s] %s", timestamp, level, msg)
  message(entrada)
  tryCatch(
    error = function(e) NULL
  )
}

# Función para obtener envíos de un formulario ODK Central vía OData
pid <- Sys.getenv("ODK_PROJECT_ID")
get_odk_submissions <- function(fid, project_id = pid) {
  
  .log_odk("INFO", paste("Iniciando get_odk_submissions() para formulario:", fid))
  
  # Configuración de credenciales
  base_url <- Sys.getenv("ODK_URL")
  email    <- Sys.getenv("ODK_EMAIL")
  password <- Sys.getenv("ODK_PASSWORD")
  
  if (!nzchar(base_url)) {
    .log_odk("ERROR", "ODK_URL no está definida en las variables de entorno")
    return(generar_df_vacio(fid))
  }
  
  # Construir URL del endpoint OData
  endpoint <- sprintf(
    "%s/v1/projects/%s/forms/%s.svc/Submissions",
    base_url, project_id, fid
  )
  .log_odk("INFO", paste("GET →", endpoint))
  
  # Intentar obtener los datos con manejo de errores
  tryCatch({
    
    # Hacer petición GET con autenticación básica
    response <- httr::GET(
      url = endpoint,
      httr::authenticate(email, password, type = "basic"),
      httr::add_headers(
        "Accept"       = "application/json",
        "Content-Type" = "application/json"
      ),
      httr::timeout(30)
    )
    
    status <- httr::status_code(response)
    .log_odk("INFO", paste("Formulario", fid, "→ HTTP", status))
    
    # Verificar status code
    if (status == 401) {
      .log_odk("ERROR", paste("HTTP 401 en formulario", fid, ": credenciales rechazadas"))
      return(generar_df_vacio(fid))
    }
    if (status == 403) {
      .log_odk("ERROR", paste("HTTP 403 en formulario", fid, ": sin permisos para acceder al proyecto/formulario"))
      return(generar_df_vacio(fid))
    }
    if (status == 404) {
      .log_odk("ERROR", paste("HTTP 404 en formulario", fid, ": formulario o proyecto no encontrado"))
      return(generar_df_vacio(fid))
    }
    if (status != 200) {
      body_raw <- tryCatch(httr::content(response, as = "text", encoding = "UTF-8"), error = function(e) "")
      .log_odk("ERROR", sprintf("HTTP %s inesperado en formulario %s. Cuerpo: %s", status, fid, substr(body_raw, 1, 300)))
      return(generar_df_vacio(fid))
    }
    
    # Parsear JSON con flatten para expandir estructuras anidadas
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    json_data    <- jsonlite::fromJSON(content_text, simplifyVector = TRUE, flatten = TRUE)
    
    # Verificar estructura de respuesta
    if (!"value" %in% names(json_data)) {
      .log_odk("ERROR", paste("Respuesta inesperada para formulario", fid, ": no se encontró el campo 'value'"))
      return(data.frame())
    }
    
    # Convertir a data.frame
    df <- as.data.frame(json_data$value, stringsAsFactors = FALSE)
    
    # Si está vacío, retornar data.frame vacío
    if (nrow(df) == 0) {
      .log_odk("WARN", paste("Formulario", fid, ": sin envíos registrados"))
      return(generar_df_vacio(fid))
    }
    
    # NORMALIZAR NOMBRES DE COLUMNAS
    df <- normalize_odk_column_names(df)
    
    .log_odk("INFO", paste("Formulario", fid, "OK →", nrow(df), "filas,", ncol(df), "columnas"))
    return(df)
    
  }, error = function(e) {
    .log_odk("ERROR", sprintf("Error al procesar formulario %s: %s", fid, e$message))
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

# ---------------------------------------------------------
# Helper de logging: escribe en consola Y en archivo de log
# ---------------------------------------------------------
.log_fastapi <- function(level = "INFO", msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entrada   <- sprintf("[%s] [FASTAPI-%s] %s", timestamp, level, msg)
  message(entrada)  # aparece en la consola / logs de Shiny
  tryCatch(
    error = function(e) NULL  # si no hay permisos de escritura, no rompe nada
  )
}

# ---------------------------------------------------------
# autenticar_fastapi: igual que antes + logging detallado
# ---------------------------------------------------------
autenticar_fastapi <- function() {
  
  .log_fastapi("INFO", paste("Iniciando autenticación →", paste0(backend_url, "/auth/token")))
  
  if (!nzchar(backend_url)) {
    .log_fastapi("ERROR", "FASTAPI_URL no está definida en las variables de entorno")
    return(NULL)
  }
  
  res_login <- tryCatch(
    httr::POST(
      url    = paste0(backend_url, "/auth/token"),
      body   = list(username = fa_username, password = fa_password),
      encode = "form",
      httr::timeout(15)
    ),
    error = function(e) {
      .log_fastapi("ERROR", paste("No se pudo conectar al backend:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(res_login)) return(NULL)
  
  status <- httr::status_code(res_login)
  .log_fastapi("INFO", paste("Respuesta auth → HTTP", status))
  
  if (status == 401) {
    .log_fastapi("ERROR", "HTTP 401: credenciales rechazadas (usuario o contraseña incorrectos)")
    return(NULL)
  }
  if (status == 403) {
    .log_fastapi("ERROR", "HTTP 403: credenciales aceptadas pero sin permisos suficientes")
    return(NULL)
  }
  if (status != 200) {
    body_raw <- tryCatch(httr::content(res_login, as = "text", encoding = "UTF-8"), error = function(e) "")
    .log_fastapi("ERROR", paste("HTTP", status, "inesperado. Cuerpo:", substr(body_raw, 1, 300)))
    return(NULL)
  }
  
  token <- tryCatch(httr::content(res_login)$access_token, error = function(e) NULL)
  
  if (is.null(token) || !nzchar(token)) {
    .log_fastapi("ERROR", "Autenticación OK (HTTP 200) pero no se recibió access_token en la respuesta")
    return(NULL)
  }
  
  .log_fastapi("INFO", "Token obtenido correctamente")
  token
}

# ---------------------------------------------------------
# enviar_acciones_criticas_a_fastapi: sin cambios de lógica,
# se agrega logging en puntos clave
# ---------------------------------------------------------
enviar_acciones_criticas_a_fastapi <- function(df_entidad_acciones) {
  
  json_payload <- df_entidad_acciones %>%
    transmute(
      entidad  = Entidad,
      criterio = Criterios,
      indicador = Indicador,
      accion   = `Acciones Sugeridas`,
      insumo   = Dimensión
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
    list(reportes = .) %>%
    toJSON(auto_unbox = TRUE, pretty = TRUE)
  
  token <- autenticar_fastapi()
  if (is.null(token)) {
    .log_fastapi("ERROR", "No se pudo enviar acciones críticas: autenticación fallida")
    return(NULL)
  }
  
  .log_fastapi("INFO", paste("Eliminando reportes previos →", paste0(backend_url, "/reports")))
  del <- httr::DELETE(
    url = paste0(backend_url, "/reports"),
    httr::add_headers("Authorization" = paste("Bearer", token))
  )
  .log_fastapi("INFO", paste("DELETE /reports → HTTP", httr::status_code(del)))
  
  .log_fastapi("INFO", paste("Enviando nuevos reportes →", paste0(backend_url, "/reports")))
  res <- httr::POST(
    url  = paste0(backend_url, "/reports"),
    body = json_payload,
    httr::add_headers(
      "Content-Type"  = "application/json",
      "Authorization" = paste("Bearer", token)
    )
  )
  .log_fastapi("INFO", paste("POST /reports → HTTP", httr::status_code(res)))
  
  res$status_code
}


# ---------------------------------------------------------
# obtener_pqrds: misma lógica + logging detallado
# ---------------------------------------------------------
obtener_pqrds <- function() {
  
  .log_fastapi("INFO", "Iniciando obtener_pqrds()")
  
  token <- autenticar_fastapi()
  if (is.null(token)) {
    .log_fastapi("WARN", "obtener_pqrds abortado: no se obtuvo token")
    return(tibble::tibble())
  }
  
  url_pqrds <- paste0(backend_url, "/pqrds")
  .log_fastapi("INFO", paste("GET →", url_pqrds))
  
  res <- tryCatch(
    httr::GET(url_pqrds, httr::add_headers("Authorization" = paste("Bearer", token)), httr::timeout(30)),
    error = function(e) {
      .log_fastapi("ERROR", paste("Fallo de red en /pqrds:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(res)) return(tibble::tibble())
  
  status <- httr::status_code(res)
  deny   <- res$headers[["x-deny-reason"]]
  if (!is.null(deny)) .log_fastapi("WARN", paste("Proxy bloqueó la petición. x-deny-reason:", deny))
  
  .log_fastapi("INFO", paste("GET /pqrds → HTTP", status))
  
  # Error HTTP → tibble vacío
  if (httr::http_error(res)) {
    body_raw <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
    .log_fastapi("ERROR", paste("HTTP", status, "en /pqrds. Cuerpo:", substr(body_raw, 1, 300)))
    return(tibble::tibble())
  }
  
  contenido_raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # Sin contenido → tibble vacío
  if (is.null(contenido_raw) || !nzchar(contenido_raw)) {
    .log_fastapi("WARN", "/pqrds devolvió respuesta vacía")
    return(tibble::tibble())
  }
  
  # Parseo del JSON
  data <- tryCatch(
    jsonlite::fromJSON(contenido_raw),
    error = function(e) {
      .log_fastapi("ERROR", paste("JSON inválido en /pqrds:", e$message))
      NULL
    }
  )
  
  # JSON inválido o vacío
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    .log_fastapi("WARN", "/pqrds: datos vacíos o estructura inesperada")
    return(tibble::tibble())
  }
  
  .log_fastapi("INFO", paste("/pqrds OK →", nrow(data), "filas,", ncol(data), "columnas:", paste(names(data), collapse = ", ")))
  
  # Convertir a tibble y retornar
  tibble::as_tibble(data)
}


# ---------------------------------------------------------
# obtener_habilidades: misma lógica + logging detallado
# ---------------------------------------------------------
obtener_habilidades <- function() {
  
  .log_fastapi("INFO", "Iniciando obtener_habilidades()")
  
  token <- autenticar_fastapi()
  if (is.null(token)) {
    .log_fastapi("WARN", "obtener_habilidades abortado: no se obtuvo token")
    return(tibble::tibble())
  }
  
  url_hab <- paste0(backend_url, "/habilidades")
  .log_fastapi("INFO", paste("GET →", url_hab))
  
  res <- tryCatch(
    httr::GET(url_hab, httr::add_headers("Authorization" = paste("Bearer", token)), httr::timeout(30)),
    error = function(e) {
      .log_fastapi("ERROR", paste("Fallo de red en /habilidades:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(res)) return(tibble::tibble())
  
  status <- httr::status_code(res)
  deny   <- res$headers[["x-deny-reason"]]
  if (!is.null(deny)) .log_fastapi("WARN", paste("Proxy bloqueó la petición. x-deny-reason:", deny))
  
  .log_fastapi("INFO", paste("GET /habilidades → HTTP", status))
  
  # Si falla el request
  if (httr::http_error(res)) {
    body_raw <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
    .log_fastapi("ERROR", paste("HTTP", status, "en /habilidades. Cuerpo:", substr(body_raw, 1, 300)))
    return(tibble::tibble())
  }
  
  contenido_raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # Si viene vacío
  if (is.null(contenido_raw) || !nzchar(contenido_raw)) {
    .log_fastapi("WARN", "/habilidades devolvió respuesta vacía")
    return(tibble::tibble())
  }
  
  # Parseo seguro del JSON
  data <- tryCatch(
    jsonlite::fromJSON(contenido_raw),
    error = function(e) {
      .log_fastapi("ERROR", paste("JSON inválido en /habilidades:", e$message))
      NULL
    }
  )
  
  # Validación de estructura
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    .log_fastapi("WARN", "/habilidades: datos vacíos o estructura inesperada")
    return(tibble::tibble())
  }
  
  .log_fastapi("INFO", paste("/habilidades OK →", nrow(data), "filas,", ncol(data), "columnas:", paste(names(data), collapse = ", ")))
  
  # Retorna el dataframe tal cual
  tibble::as_tibble(data)
}