# ============================================
# FUNCIÓN MAESTRA: GENERADOR OPTIMIZADO
# ============================================

generar_dataframes_con_filtros <- function(
    anios = NULL,
    meses = NULL,
    canal = NULL,
    subcanal = NULL,
    ids_seleccionados = NULL,
    nivel = "Distrito"
) {
  
  # Construir vector de periodos si se especifican años/meses
  periodos_filtrados <- NULL
  
  if (!is.null(anios) && length(anios) > 0) {
    anios <- as.integer(anios)
    
    if (!is.null(meses) && length(meses) > 0) {
      meses <- as.integer(meses)
    } else {
      meses <- 1:12
    }
    
    periodos_filtrados <- expand.grid(Anio = anios, Mes = meses) %>%
      mutate(Periodo = sprintf("%04d-%02d", Anio, Mes)) %>%
      pull(Periodo)
    
  }
  
  # Log de filtros aplicados
  
  # Llamar a calculo_indicadores UNA SOLA VEZ con todos los filtros
  resultado <- calculo_indicadores(
    nivel = nivel,
    periodos = periodos_filtrados,
    canal = canal,
    subcanal = subcanal,
    ids_seleccionados = ids_seleccionados
  )
  
  ans <- list(
    indicadores = resultado$Indicadores,
    indices = resultado$Indices,
    nivel = nivel,
    ids_seleccionados = ids_seleccionados
  )
  
  ans
}

# ============================================
# WRAPPERS ESPECÍFICOS PARA DIFERENTES GRÁFICOS
# ============================================

# 0. STARTUP: todos los periodos, sin filtros de entidad/sector
generar_dataframes_filtrados_startup <- function() {
  
  # Fecha actual
  fecha_actual <- lubridate::floor_date(Sys.Date(), "month")
  ultimos_4 <- seq(from = fecha_actual, by = "-1 month", length.out = 4)
  
  resultados <- list()
  
  # 1. Sin filtros
  resultados$general <- generar_dataframes_con_filtros(
    nivel = "Distrito"
  )
  
  # 2-4. Por canal
  canales <- c("Presencial", "Telefónico", "Virtual")
  resultados$canales <- list()
  for (canal in canales) {
    resultados$canales[[canal]] <- generar_dataframes_con_filtros(
      nivel = "Distrito",
      canal = canal
    )
  }
  
  # 5-8. Últimos 4 periodos
  resultados$historico <- list()
  for (periodo in ultimos_4) {
    periodo_key <- format(as.Date(periodo), "%Y-%m")
    resultados$historico[[periodo_key]] <- generar_dataframes_con_filtros(
      nivel = "Distrito",
      anios = as.integer(format(as.Date(periodo), "%Y")),
      meses = as.integer(format(as.Date(periodo), "%m"))
    )
  }
  
  # 9. Ranking de entidades
  ranking_entidades <- generar_dataframes_con_filtros(
    nivel = "Entidad"
  )$indices %>% filter(Nivel != 0) %>% select(Nivel, indice_c1, indice_c2)
  
  resultados$ranking_entidades_c1 <- ranking_entidades %>%
    select(Nivel, indice_c1) %>%
    filter(!is.na(indice_c1)) %>%
    rename(Valor = indice_c1) %>%
    arrange(desc(Valor))
  
  resultados$ranking_entidades_c2 <- ranking_entidades %>%
    select(Nivel, indice_c2) %>%
    filter(!is.na(indice_c2)) %>%
    rename(Valor = indice_c2) %>%
    arrange(desc(Valor))
  
  # 9. Ranking de entidades
  ranking_entidades <- generar_dataframes_con_filtros(
    nivel = "Entidad"
  )$indices %>% filter(Nivel != 0) %>% select(Nivel, indice_c1, indice_c2)
  
  resultados$ranking_entidades_c1 <- ranking_entidades %>%
    select(Nivel, indice_c1) %>%
    filter(!is.na(indice_c1)) %>%
    rename(Valor = indice_c1) %>%
    arrange(desc(Valor))
  
  resultados$ranking_entidades_c2 <- ranking_entidades %>%
    select(Nivel, indice_c2) %>%
    filter(!is.na(indice_c2)) %>%
    rename(Valor = indice_c2) %>%
    arrange(desc(Valor))
  
  resultados
}

# 1. INDICES GLOBALES
actualizar_dataframes_general <- function(anios = NULL, meses = NULL, canal = NULL, subcanal = NULL, nivel = "Distrito", ids_seleccionados = NULL) {
  generar_dataframes_con_filtros(
    anios = anios, 
    meses = meses, 
    canal = canal, 
    subcanal = subcanal,
    nivel = nivel,
    ids_seleccionados = ids_seleccionados
  )
}

actualizar_ranking_entidades <- function(anios = NULL, meses = NULL, canal = NULL, subcanal = NULL) {
  ranking_entidades <- generar_dataframes_con_filtros(
    anios = anios, 
    meses = meses, 
    canal = canal, 
    subcanal = subcanal,
    nivel = "Entidad"
  )$indices %>% filter(Nivel != 0) %>% select(Nivel, indice_c1, indice_c2)
  
  resultados <- list()
  
  resultados$ranking_entidades_c1 <- ranking_entidades %>%
    select(Nivel, indice_c1) %>%
    filter(!is.na(indice_c1)) %>%
    rename(Valor = indice_c1) %>%
    arrange(desc(Valor))
  
  resultados$ranking_entidades_c2 <- ranking_entidades %>%
    select(Nivel, indice_c2) %>%
    filter(!is.na(indice_c2)) %>%
    rename(Valor = indice_c2) %>%
    arrange(desc(Valor))
  
  resultados
}

# 2. HISTÓRICO POR PERIODO
actualizar_dataframes_historico <- function(anios = NULL, meses = NULL, canal = NULL, subcanal = NULL, nivel = "Distrito", ids_seleccionados = NULL) {
  historicos <- list()
  
  for (anio in anios) {
    for (mes in meses) {
      
      periodo_key <- sprintf("%04d-%02d", as.numeric(anio), as.numeric(mes))
      historicos[[periodo_key]] <- generar_dataframes_con_filtros(
        anios = anio, 
        meses = mes, 
        canal = canal, 
        subcanal = subcanal,
        nivel = nivel,
        ids_seleccionados = ids_seleccionados
      )
    }
  }
  
  historicos
}

# 3. FILTRADO POR CANAL
generar_dataframes_filtrados_canal <- function(anios = NULL, meses = NULL, canal = NULL, subcanal = NULL, nivel = "Distrito", ids_seleccionados = NULL) {
  canales <- list()
  
  for (c in canal) {
    
    canales[[c]] <- generar_dataframes_con_filtros(
      anios = anios, 
      meses = meses, 
      canal = canal, 
      subcanal = subcanal,
      nivel = nivel,
      ids_seleccionados = ids_seleccionados
    )
  }
  
  canales
}

# 4. FILTRADO POR SUBCANAL
generar_dataframes_filtrados_subcanal <- function(anios = NULL, meses = NULL, canal = NULL, subcanal = NULL, nivel = "Distrito", ids_seleccionados = NULL) {
  
  subcanales <- generar_dataframes_con_filtros(
    anios = anios, 
    meses = meses, 
    canal = canal, 
    subcanal = subcanal,
    nivel = nivel,
    ids_seleccionados = ids_seleccionados
  )
}

# ============================================
# CALCULAR LOS INDICADORES CRÍTICOS POR ENTIDAD
# ============================================

generar_indicadores_criticos <- function() {
  # Obtener listado de indicadores críticos por entidad
  df_inds <- calculo_indicadores("Entidad", 
                                 NULL, NULL, NULL,
                                 unique(df_entidades$Id_Entidad))[["Indicadores"]]
  
  df_inds_criticos <- df_inds %>%
    filter(Nivel != 0) %>%
    pivot_longer(
      cols = -Nivel,
      names_to = "Id_Indicador",
      values_to = "Valor"
    ) %>%
    rename(Id_Entidad = Nivel) %>%
    filter(Valor < 90)
  
  # Calcular criterios usando la función original
  df_entidad_acciones <- calculo_criterios() %>%
    left_join(
      l_acciones %>%
        rename(
          Id_Indicador = ID_INDICADOR,
          `Acciones Sugeridas` = `ACCIONES REQUERIDA`,
          `Herramientas de Apoyo` = `HERRAMIENTAS DE APOYO`,
          Id_Criterio = IdCriterio,
          Criterios = CRITERIOS
        ),
      by = "Id_Criterio"
    ) %>%
    filter(Critico == TRUE) %>%
    select(-INDICADOR, -Critico)
  
  # Crear df_indicadores_criticos_por_entidad para compatibilidad
  df_indicadores_criticos_por_entidad <<- df_entidad_acciones %>%
    inner_join(df_inds_criticos, by = c("Id_Entidad", "Id_Indicador")) %>%
    left_join(df_entidades %>% select(Id_Entidad, Entidad), by = "Id_Entidad") %>%
    left_join(df_indicadores %>% select(-Componente, -Pilar), by = "Id_Indicador") %>%
    rename(`Valor Promedio` = Valor) %>%
    select(Id_Entidad, 
           Entidad, 
           Id_Indicador,
           Indicador, 
           Id_Criterio, 
           Criterios, 
           Dimensión, 
           `Valor Promedio`,
           `Acciones Sugeridas`,
           `Herramientas de Apoyo`)
  
  
  # Enviar al backend de FastAPI
  res_fastapi_critico <- enviar_acciones_criticas_a_fastapi(df_indicadores_criticos_por_entidad)
  cat(paste0("✓ ", nrow(df_indicadores_criticos_por_entidad), " Acciones críticas enviadas a FastAPI.\n"))
}
