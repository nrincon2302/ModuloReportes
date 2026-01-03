library(dplyr)
library(tidyr)


# ============================================
# 1. MAPEO DE INDICADORES
# ============================================


# Función auxiliar: extrae número tras la letra (c/p/d) o devuelve 0
extract_num <- function(vec, letter) {
  m <- str_match(vec, paste0(letter, "(\\d+)"))
  as.integer(ifelse(is.na(m[,2]), 0, m[,2]))
}

df_indicadores <<- data.frame(
  l_ind,
  Id_Componente = extract_num(l_ind$Id_indicador, "c"),
  Id_Pilar      = extract_num(l_ind$Id_indicador, "p"),
  Id_Dimension  = extract_num(l_ind$Id_indicador, "d"),
  stringsAsFactors = FALSE
)  %>%
  mutate(
    Id_Pilar = ifelse(Id_Componente == 0 & Id_Dimension != 0, 0, Id_Pilar)
  )%>%
  select(
    Id_Indicador = Id_indicador,
    Indicador,
    Componente,
    Pilar,
    Dimensión,
    Id_Componente,
    Id_Pilar,
    Id_Dimension
  )



# ============================================
# 2. MAPEO DE ENTIDADES Y SECTORES
# ============================================


# Función para normalizar nombres (quitar tildes, comas, caracteres especiales)
normalizar_nombre <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub(",", " ", x)  # Reemplazar comas por espacios
  x <- gsub("[^a-zA-Z0-9 ]", "", x)
  x <- trimws(x)
  return(x)
}

# Crear df_sectores y df_entidades
df_sectores <<- l_entidades %>%
  transmute(
    Id_Sector = as.numeric(`No.`),
    Sector    = SECTOR
  ) %>%
  distinct() %>%
  arrange(Id_Sector)

df_entidades <<- l_entidades_id %>%
  mutate(Entidad_norm = normalizar_nombre(Entidad)) %>%
  left_join(
    l_entidades %>% mutate(ENTIDAD_norm = normalizar_nombre(ENTIDAD)),
    by = c("Entidad_norm" = "ENTIDAD_norm")
  ) %>%
  transmute(
    Id_Entidad = id_entidad,
    Entidad = Entidad,
    Id_Sector = as.numeric(`No.`)
  ) %>%
  # Unir el nombre del Sector por Id_Sector
  left_join(df_sectores, by = "Id_Sector") %>%
  
  # ============================================================
# AGREGAR REGISTROS COMPLEMENTO QUE NO EXISTAN EN df_entidades
# ============================================================
{ 
  df_base <- .
  
  # 1. Detectar faltantes por Id_Entidad (sólo para PQRDS)
  faltantes <- l_complemento_entidades %>%
    filter(!(id_entidad %in% df_base$Id_Entidad)) %>%
    transmute(
      Id_Entidad = id_entidad,
      Entidad = Entidad,
      Id_Sector = case_when(
        id_entidad == 75 ~ 14, # ENEL
        id_entidad == 80 ~ 14, # VANTI
        id_entidad == 200 ~ 12, # Grúas y Patios
        id_entidad == 201 ~ 9, # INVEST
        TRUE ~ NA_real_
      )
    ) %>%
    # Pegar nombre del sector
    left_join(df_sectores, by = "Id_Sector")
  
  # 2. Unir con df_base
  bind_rows(df_base, faltantes)
} %>%
  
  # Orden final
  arrange(Id_Sector, Entidad)



# Procesamiento inusual de base de datos PQRDS
procesar_pqrds <- function() {
  base_pqrds <<- base_pqrds %>% 
    # 1. Reemplazar mod1_gv4_p8 por mod1_gv4_v9
    mutate(
      mod1_gv4_p8 = as.numeric(mod1_gv4_v9)
    ) %>%
    
    # 2. Reemplazar mod1_gv4_p7 por Id_Sector usando df_entidades
    left_join(
      df_entidades %>% select(Id_Entidad, Id_Sector),
      by = c("mod1_gv4_p8" = "Id_Entidad")
    ) %>% 
    mutate(
      mod1_gv4_p7 = as.character(Id_Sector),
      mod1_gv4_p8 = as.character(mod1_gv4_p8)
    ) %>% 
    select(-Id_Sector)
  
  sample_data <<- df_pqrds %>%
    select(-any_of(c("Id_Entidad", "Id_Sector"))) %>%
    left_join(
      l_complemento_entidades %>% rename(entidad = Entidad) %>%
        rename(Id_Entidad = id_entidad),
      by = "entidad"
    ) %>%
    left_join(
      df_entidades %>% select(Id_Entidad, Id_Sector),
      by = "Id_Entidad"
    )
  
}

# ============================================
# 3. MAPEO DE CANALES Y SUBCANALES
# ============================================


df_canales <<- l_canal %>%
  rename(Id_Canal = id_canal, Canal = canal, Id_Subcanal = id_subcanal, Subcanal = subcanal) %>%
  filter(!is.na(Canal), !is.na(Subcanal)) %>%
  distinct() %>%
  arrange(Canal, Subcanal)

# Lista de canales únicos
df_canales_lista <<- df_canales %>%
  select(Canal) %>%
  distinct()
cat("✓ Dataframes base cargados correctamente.\n")


# ============================================
# 4. MATRICES DE PQRDS Y HABILIDADES
# ============================================

descarga_matrices_fastapi <- function() {
  suppressWarnings({
    df_habilidades_raw <- obtener_habilidades()
    
    if (is.null(df_habilidades_raw) || nrow(df_habilidades_raw) == 0) {
      df_habilidades <<- generar_df_vacio("habilidades")
    } else {
      df_habilidades <<- df_habilidades_raw %>%
        mutate(
          periodo = sprintf("%04d-%02d", anio, mes),
          Canal = NA,
          Subcanal = NA
        ) %>% 
        select(-id) %>%
        left_join(df_entidades 
                  %>% select(Id_Sector, Id_Entidad) 
                  %>% rename(id_entidad = Id_Entidad),
                  by = "id_entidad")
    }
    
    df_pqrds_raw <- obtener_pqrds()
    
    if (is.null(df_pqrds_raw) || nrow(df_pqrds_raw) == 0) {
      df_pqrds <<- generar_df_vacio("universe")
    } else {
      df_pqrds <<- df_pqrds_raw %>%
        mutate(
          Canal = NA,
          Subcanal = NA
        ) %>% 
        select(-id)
    }
    
    cat("✓ Matrices de FastAPI descargadas.\n")
  })
}


# ============================================
# 5. ESTABLECIMIENTO DE RANGOS TEMPORALES
# ============================================

# Asumimos la historia completa de periodos formato "YYYY-MM"
years <- 2023:as.numeric(format(Sys.Date(), "%Y"))
months <- 1:12
periodos <<- expand.grid(
  Año = years,
  Mes = months
)
periodos$Periodo <- sprintf("%04d-%02d", 
                            periodos$Año,
                            periodos$Mes)

periodos_disponibles <<- periodos$Periodo

