library(dplyr)
library(tidyr)
library(tibble)

# ============================================
# 1. OBTENER ÍNDICE DE COMPONENTE
# Retorna el valor del índice del componente en cuestión
# ============================================
obtener_indice_componente <- function(data, id_componente) {
  # Retornar el índice del componente actual
  indice_comp <- NULL
  
  if (as.numeric(id_componente) == 1) {
    indice_comp <- data$indices %>% filter(Nivel == 0) %>% pull(indice_c1)
  } else {
    indice_comp <- data$indices %>% filter(Nivel == 0) %>% pull(indice_c2)
  }
  
  indice_comp
}

# ============================================
# 2. OBTENER ÍNDICES DE PILARES
# Retorna los índices de los pilares de un componente ordenados
# ============================================
obtener_indices_pilares <- function(data, id_componente) {
  
  indices_pilares <- NULL
  
  # Obtener los pilares según el componente
  if (as.numeric(id_componente) == 1) {
    indices_pilares <- data$indices %>%
      filter(Nivel == 0) %>%
      select(indice_p1, indice_p2, indice_p3)
  } else {
    return (NULL) # No se almacena valor de indice_p4
  }
  
  # Trasponer para obtener el ranking de pilares
  ranking_pilares <- indices_pilares %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Id_Indicador",
      values_to = "Valor"
    ) %>%
    arrange(desc(Valor))
  
  if (nrow(ranking_pilares) == 0) return(NULL)
  
  # Cruzar datos y agregar para que cada Pilar tenga su nombre
  resultado <- ranking_pilares %>%
    inner_join(df_indicadores, by = "Id_Indicador") %>%
    select(Pilar, Valor)
  
  resultado
}

# ============================================
# 3. OBTENER ÍNDICES DE DIMENSIONES
# Retorna los índices de las dimensiones de un pilar ordenados
# ============================================
obtener_indices_dimensiones <- function(data, id_pilar) {
  
  # Obtener las dimensiones según el pilar
  if (is.null(id_pilar)) {
    return (NULL)
  }
  
  prefijo <- paste0("indice_p", id_pilar, "_d")
  indices_dim <- data$indices %>%
    filter(Nivel == 0) %>%
    select(starts_with(prefijo))
  
  # Trasponer para obtener el ranking de pilares
  ranking_dimensiones <- indices_dim %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Id_Indicador",
      values_to = "Valor"
    ) %>%
    arrange(desc(Valor))
  
  if (nrow(ranking_dimensiones) == 0) return(NULL)
  
  # Cruzar datos y agregar para que cada Dimensión tenga su nombre
  resultado <- ranking_dimensiones %>%
    inner_join(df_indicadores, by = "Id_Indicador") %>%
    select(Dimensión, Valor)
  
  resultado
}

# ============================================
# 4. OBTENER ÍNDICE POR PERIODO
# Calcula el promedio histórico filtrando por los IDs seleccionados
# ============================================
obtener_indice_por_periodo <- function(lista_historico, id_componente) {
  
  if (is.null(lista_historico) || length(lista_historico) == 0) return(NULL)
  
  resultados <- data.frame(Periodo = character(), Indice = numeric(), stringsAsFactors = FALSE)
  
  # Iterar sobre cada periodo
  for (periodo_lbl in names(lista_historico)) {
    
    snapshot_mes <- lista_historico[[periodo_lbl]]
    
    # Extraer el valor promedio usando los IDs seleccionados
    valor <- obtener_indice_componente(snapshot_mes, id_componente)
    
    if (!is.na(valor)) {
      resultados <- rbind(resultados, data.frame(Periodo = periodo_lbl, Indice = valor))
    }
  }
  
  if (nrow(resultados) == 0) return(NULL)
  
  resultados %>% arrange(Periodo)
}

# ============================================
# 5. OBTENER ÍNDICES POR CANAL
# Calcula promedio por canal filtrando por los IDs seleccionados
# ============================================
obtener_indices_por_canal <- function(lista_canales, id_componente) {
  
  if (is.null(lista_canales) || length(lista_canales) == 0) return(NULL)
  
  resultados <- data.frame(Canal = character(), Indice = numeric(), stringsAsFactors = FALSE)
  
  for (nombre_canal in names(lista_canales)) {
    
    snapshot_canal <- lista_canales[[nombre_canal]]
    
    # Extraer valor promedio filtrado
    valor <- obtener_indice_componente(snapshot_canal, id_componente)
    
    if (!is.na(valor)) {
      resultados <- rbind(resultados, data.frame(Canal = nombre_canal, Indice = valor))
    }
  }
  
  if (nrow(resultados) == 0) return(NULL)
  
  resultados %>% arrange(desc(Indice))
}

# ============================================
# 6. RANKING DE ENTIDADES
# Filtra las filas resultantes según los IDs seleccionados
# ============================================
obtener_ranking_entidades <- function(data, id_componente) {
  
  if (is.null(data)) return(NULL)
  
  if (id_componente == 1) {
    prefijo <- "ranking_entidades_c1"
  } else {
    prefijo <- "ranking_entidades_c2"
  }
  ranking_entidades <- data$ranking_entidades[[prefijo]] %>%
    rename(Id_Entidad = Nivel)
  
  if (nrow(ranking_entidades) == 0) return(NULL)
  
  # Cruzar datos y agregar para que cada Entidad tenga su nombre
  resultado <- ranking_entidades %>%
    inner_join(df_entidades, by = "Id_Entidad") %>%
    filter(!is.na(Entidad)) %>%
    select(Id_Entidad, Entidad, Valor)
  
  resultado
}


# ============================================
# 7. OBTENER INDICADORES DE DIMENSIONES
# Retorna indicadores de una dimensión rankeados
# ============================================
obtener_indicadores <- function(data, id_componente, id_pilar, id_dimension) {
  
  if (is.null(id_componente) || is.null(id_pilar) || is.null(id_dimension)) {
    return (NULL)
  }
  
  prefijo <- paste0("c", id_componente, "_p", id_pilar, "_d", id_dimension, "_")
  indicadores <- data$general$indicadores %>%
    filter(Nivel == 0) %>%
    select(starts_with(prefijo))
  
  # Trasponer para obtener el ranking de pilares
  ranking_indicadores <- indicadores %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Id_Indicador",
      values_to = "Valor"
    ) %>%
    arrange(desc(Valor))
  
  if (nrow(ranking_indicadores) == 0) return(NULL)
  
  # Cruzar datos y agregar para que cada Dimensión tenga su nombre
  resultado <- ranking_indicadores %>%
    inner_join(df_indicadores, by = "Id_Indicador") %>%
    filter(!is.na(Valor)) %>%
    select(Id_Indicador, Indicador, Valor)
  
  resultado
}


# ============================================
# 8. OBTENER INDICADORES POR CANAL
# Retorna indicadores ponderados de un canal rankeados por canal
# ============================================
obtener_valor_indicador_por_canal <- function(lista_canales, indicadores) {
  
  if (is.null(lista_canales) || length(lista_canales) == 0) return(NULL)
  
  resultados <- data.frame(Canal = character(), Indice = numeric(), stringsAsFactors = FALSE)
  for (nombre_canal in names(lista_canales)) {
    
    snapshot_canal <- lista_canales[[nombre_canal]]$indicadores
    
    # Extraer valor ponderado
    valores_pond <- snapshot_canal %>% filter(Nivel == 0) %>% select(all_of(indicadores))
    valor <- rowMeans(valores_pond, na.rm = TRUE)
    
    if (!is.na(valor)) {
      resultados <- rbind(resultados, data.frame(Canal = nombre_canal, Indice = valor))
    }
  }
  
  if (nrow(resultados) == 0) return(NULL)
  
  resultados %>% arrange(desc(Indice))
}


# ============================================
# 9. OBTENER HISTORICO POR INDICADORES
# Retorna históricos de indicadores seleccionados
# ============================================
calcular_evolucion_indicador_historica <- function(lista_historico, indicadores) {
  
  if (is.null(lista_historico) || length(lista_historico) == 0) return(NULL)
  
  resultados <- data.frame(Periodo = character(), Valor = numeric(), stringsAsFactors = FALSE)
  for (periodo_lbl in names(lista_historico)) {
    snapshot_mes <- lista_historico[[periodo_lbl]]$indicadores
    
    # Extraer valor ponderado
    valores_pond <- snapshot_mes %>% filter(Nivel == 0) %>% select(all_of(indicadores))
    valor <- rowMeans(valores_pond, na.rm = TRUE)
    
    if (!is.na(valor)) {
      resultados <- rbind(resultados, data.frame(Periodo = periodo_lbl, Valor = valor))
    }
  }
  
  if (nrow(resultados) == 0) return(NULL)
  resultados %>% arrange(Periodo)
}


# ============================================
# 10. OBTENER INDICADORES PROMEDIADAS POR SUBCANAL
# Retorna históricos de indicadores seleccionados
# ============================================
extraer_indicadores_subcanal <- function(data_subcanal, indicadores) {
  snapshot_subcanal <- data_subcanal$indicadores
  
  # Extraer valor ponderado
  valores_pond <- snapshot_subcanal %>% filter(Nivel == 0) %>% select(all_of(indicadores))
  valor <- rowMeans(valores_pond, na.rm = TRUE)
  
  valor
}


# ============================================
# HELPERS DE UTILIDAD (Estáticos)
# ============================================
obtener_canales <- function() {
  if (exists("df_canales_lista")) return(df_canales_lista$Canal)
  return(c("Presencial", "Telefónico", "Virtual"))
}

obtener_subcanales <- function(canal) {
  if (exists("df_canales")) {
    df_canales %>% filter(Canal == canal) %>% pull(Subcanal)
  } else {
    character(0)
  }
}
