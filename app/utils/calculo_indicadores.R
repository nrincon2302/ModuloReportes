library(dplyr)
library(purrr)


######## Transformación de variables y Procesos previos

# Función auxiliar para sumar columnas
sumar_cols <- function(df, cols) {
  df[cols] <- lapply(df[cols], \(x) as.numeric(as.character(x)))
  rowSums(df[cols], na.rm = TRUE)
}

# Definición de columnas para p22
cols_p22_1 <- paste0("mod8_gp22_gp22_", 1:12, "_1_p22_", 1:12, "_1")
cols_p22_2 <- paste0("mod8_gp22_gp22_", 1:12, "_1_p22_", 1:12, "_2")
cols_p22_3 <- paste0("mod8_gp22_gp22_", 1:12, "_1_p22_", 1:12, "_3")

base_proceso$p22_1 <- sumar_cols(base_proceso, cols_p22_1)
base_proceso$p22_2 <- sumar_cols(base_proceso, cols_p22_2)
base_proceso$p22_3 <- sumar_cols(base_proceso, cols_p22_3)

# Función para recodificar variables de interés (1, 2, 3 -> 1, 0, 0.5)
recodificar_interes <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- case_when(
        as.character(df[[col]]) == "1" ~ 1,
        as.character(df[[col]]) == "2" ~ 0,
        as.character(df[[col]]) == "3" ~ 0.5,
        TRUE ~ 0
      )
    }
  }
  return(df)
}

# Listas de variables para recodificar
vars_capa <- c("mod2_p8","mod2_p9","mod2_p10","mod2_p11","mod2_p12","mod2_p13",
               "mod2_p14","mod2_p15","mod2_p16","mod2_p17","mod3_mod3_1_mod3_1_1_p18",
               "mod3_mod3_1_mod3_1_1_p19","mod3_mod3_1_mod3_1_1_p20",
               "mod3_mod3_1_mod3_1_1_p21","mod3_mod3_1_mod3_1_1_p22","mod3_mod3_1_mod3_1_1_p23",
               "mod3_mod3_1_mod3_1_1_p24","mod3_mod3_1_mod3_1_2_p25","mod3_mod3_1_mod3_1_2_p26",
               "mod3_mod3_1_mod3_1_2_p27","mod3_mod3_1_mod3_1_2_p28","mod3_mod3_1_mod3_1_2_p29",
               "mod3_mod3_1_mod3_1_2_p30","mod4_mod4_1_p33","mod4_mod4_1_p34","mod4_mod4_1_p35")

vars_esta <- c("mod2_mod2_1_p6","mod2_mod2_1_p7","mod2_mod2_1_p8","mod2_mod2_1_p9",
               "mod2_mod2_1_p10","mod2_mod2_1_p11","mod2_mod2_1_p12","mod3_p14",
               "mod3_p15","mod3_p16","mod4_mod4_1_mod4_1_1_p17","mod4_mod4_1_mod4_1_1_p18",
               "mod4_mod4_1_mod4_1_1_p19","mod4_mod4_1_mod4_1_1_p20","mod4_mod4_1_mod4_1_1_p21",
               "mod4_mod4_1_mod4_1_1_p22","mod4_mod4_1_mod4_1_1_p23","mod4_mod4_1_mod4_1_2_p24",
               "mod4_mod4_1_mod4_1_2_p25","mod4_mod4_1_mod4_1_2_p26","mod4_mod4_1_mod4_1_2_p27",
               "mod4_mod4_1_mod4_1_2_p28","mod4_mod4_1_mod4_1_2_p29","mod4_mod4_1_mod4_1_2_p30",
               "mod4_mod4_1_mod4_1_2_p31","mod5_mod5_1_p39","mod5_mod5_1_p40")

vars_tele <- c("mod2_mod2_1_p10","mod2_mod2_1_p11","mod2_mod2_2_p14","mod2_mod2_2_p15",
               "mod2_mod2_2_p16","mod2_mod2_2_p17","mod2_mod2_3_p18","mod2_mod2_3_p19",
               "mod2_mod2_3_p20","mod2_mod2_3_p21","mod2_mod2_3_p22","mod2_mod2_3_p23",
               "mod2_mod2_3_p24","mod2_mod2_3_gp25_p25","mod2_mod2_3_p26","mod2_mod2_4_p27")

vars_virt <- c("mod2_mod2_1_p5","mod2_mod2_1_p6","mod2_mod2_1_p7","mod2_mod2_1_p8",
               "mod2_mod2_1_p9","mod2_mod2_1_p10","mod2_mod2_1_p11","mod2_mod2_2_mod2_2_1_p12",
               "mod2_mod2_2_mod2_2_1_gp12_p14","mod2_mod2_2_mod2_2_1_gp12_p15",
               "mod2_mod2_2_mod2_2_1_gp12_p16","mod2_mod2_2_mod2_2_2_p17",
               "mod2_mod2_2_mod2_2_2_p18","mod2_mod2_2_mod2_2_2_p19",
               "mod2_mod2_2_mod2_2_2_p20","mod2_mod2_2_mod2_2_2_p21",
               "mod2_mod2_2_mod2_2_2_p22")

# Aplicar recodificaciones
base_CO_capa <- recodificar_interes(base_CO_capa, vars_capa)
base_CO_esta <- recodificar_interes(base_CO_esta, vars_esta)
base_CO_tele <- recodificar_interes(base_CO_tele, vars_tele)
base_CO_virt <- recodificar_interes(base_CO_virt, vars_virt)

# Recodificaciones binarias simples (1 o 0)
binarias_tele <- c("mod2_mod2_1_p12", "mod2_mod2_4_p28", "mod2_mod2_4_p29")
base_CO_tele[binarias_tele] <- lapply(base_CO_tele[binarias_tele], \(x) ifelse(x == "1", 1, 0))

binarias_virt <- c("mod2_mod2_2_mod2_2_1_gp12_p13", "mod2_mod2_3_p23", "mod2_mod2_3_p24", "mod2_mod2_3_p25")
base_CO_virt[binarias_virt] <- lapply(base_CO_virt[binarias_virt], \(x) ifelse(x %in% c("1"), 1, 0))


######## Cálculo de indicadores
calculo_indicadores <- function(nivel, periodos = NULL, canal = NULL, subcanal = NULL, ids_seleccionados = NULL){
  
  # 1. Función interna para preparar los datos (filtrar y agrupar dinámicamente)
  preparar_base <- function(df, col_sector = NULL, col_entidad = NULL) {
    col_target <- if(nivel == "Sector") col_sector else col_entidad
    
    # Filtro común
    df_filt <- df %>% 
      filter(
        if (!is.null(ids_seleccionados)) .data[[col_target]] %in% ids_seleccionados else TRUE,
        if (!is.null(periodos)) periodo %in% periodos else TRUE,
        if (!is.null(canal)) Canal %in% canal else TRUE,
        if (!is.null(subcanal)) Subcanal %in% subcanal else TRUE
      )
    
    # Lógica de agrupamiento
    if(is.null(col_target)) stop(paste("Columna ID no definida para nivel:", nivel))
    
    # Agrupa por Nivel
    df_grouped <- df_filt %>% 
      mutate(Nivel = as.numeric(.data[[col_target]])) %>% 
      filter(!is.na(Nivel)) %>% 
      group_by(Nivel)
    
    return(df_grouped)
  }
  
  preparar_pqrds <- function(df, col_sector = NULL, col_entidad = NULL) {
    col_target <- if(nivel == "Sector") col_sector else col_entidad
    
    # Sólo se filtra por ids ya que el sample es estático en el tiempo y en canal/subcanal
    df_filt <- df %>% 
      filter(
        if (!is.null(ids_seleccionados)) .data[[col_target]] %in% ids_seleccionados else TRUE,
        if (!is.null(periodos)) periodo %in% periodos else TRUE,
        if (!is.null(canal)) Canal %in% canal else TRUE,
        if (!is.null(subcanal)) Subcanal %in% subcanal else TRUE
      )
    
    # Lógica de agrupamiento
    if(is.null(col_target)) stop(paste("Columna ID no definida para nivel:", nivel))
    
    # Agrupa por Nivel
    df_grouped <- df_filt %>% 
      mutate(Nivel = as.numeric(.data[[col_target]])) %>% 
      filter(!is.na(Nivel)) %>% 
      group_by(Nivel)
    
    return(df_grouped)
  }
  
  # 2. Función para añadir fila de Consolidado Ponderado
  anadir_consolidado <- function(df_resumen) {
    # Calcular fila consolidada
    fila_cons <- df_resumen %>% 
      ungroup() %>% 
      summarise(
        # Nivel 0 identificará al "Consolidado"
        Nivel = 0, 
        # Promedio ponderado para todas las columnas numéricas excepto Nivel y num_obs
        across(where(is.numeric) & !any_of(c("Nivel", "ponderador")), 
               ~ weighted.mean(.x, w = ponderador, na.rm = TRUE))
      )
    
    # Unir con la tabla original y eliminar la columna de pesos auxiliar
    bind_rows(df_resumen, fila_cons) %>%
      select(-ponderador)
  }
  
  ponderar_indicadores <- function(df_resumen) {
    # Calcular fila consolidada
    fila_cons <- df_resumen %>% 
      ungroup() %>% 
      summarise(
        # Nivel 0 identificará al "Consolidado"
        Nivel = 0, 
        # Promedio ponderado para todas las columnas numéricas excepto Nivel y ponderador
        across(where(is.numeric) & !any_of(c("Nivel", "num_obs", "ponderador")), 
               ~ weighted.mean(.x, w = ponderador, na.rm = TRUE))
      )
    
    # Unir con la tabla original y eliminar la columna de pesos auxiliar
    bind_rows(df_resumen, fila_cons) %>%
      select(-num_obs)
  }
  
  # 3. Definición de bloques de cálculo
  
  #### Pilar 1: Gestión Efectiva (base_pqrds)
  Ind_p1_1 <- preparar_pqrds(sample_data, "Id_Sector", "Id_Entidad") %>% 
    summarise(
      num_obs = n(),
      c1_p1_d1_01 = 100*(1-(sum(tipo_gestion == "Gestion extemporanea")/n())),
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  Ind_p1_2 <- preparar_base(base_pqrds, "mod1_gv4_p7", "mod1_gv4_p8") %>% 
    summarise(
      num_obs = n(),
      c1_p1_d1_02 = sum(ifelse(mod2_mod2_1_v18 %in% c("0") & mod2_mod2_1_v19 %in% c("0") &
                                 mod2_mod2_1_v20 %in% c("0") & mod2_mod2_1_v16 %in% c("0") &
                                 mod2_mod2_1_v21 %in% c("0"), 1, 0)) / n() * 100
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  #### Pilar 2: Eficiencia Institucional (base_proceso)
  Ind_p2 <- preparar_base(base_proceso, "mod1_c0", "mod1_p1") %>%
    mutate(
      c1_p2_d4_05_calc = (1 - (p22_1 / 240))*100,
      c1_p2_d4_06_calc = (1 - (p22_2 / 240))*100,
      c1_p2_d4_07_calc = (1 - (p22_3 / 240))*100
    ) %>%
    summarise(
      num_obs = n(),
      c1_p2_d2_01 = sum(as.numeric(mod2_gp7_p7), na.rm = T)/sum(as.numeric(mod2_gp6_p6))*100,
      c1_p2_d2_02 = sum(as.numeric(mod3_p9), na.rm = T)/sum(as.numeric(mod3_p8))*100,
      c1_p2_d2_03 = sum(as.numeric(mod3_p12), na.rm = T)/sum(as.numeric(mod3_p10))*100,
      c1_p2_d3_01 = sum(as.numeric(mod4_p13) == 1, na.rm = T)/nrow(base_proceso)*100,
      c1_p2_d3_02 = sum(as.numeric(mod5_p16) == 1, na.rm = T)/nrow(base_proceso)*100,
      c1_p2_d4_01 = sum(as.numeric(mod6_p17) == 1, na.rm = T)/nrow(base_proceso)*100,
      c1_p2_d4_02 = sum(as.numeric(mod6_p18) == 1, na.rm = T)/nrow(base_proceso)*100,
      c1_p2_d4_03 = sum(as.numeric(mod7_p20), na.rm = T)/sum(as.numeric(mod7_p19))*100,
      c1_p2_d4_04 = sum(as.numeric(mod8_p21) == 1, na.rm = T)/nrow(base_proceso)*100,
      c1_p2_d4_05 = mean(c1_p2_d4_05_calc, na.rm = TRUE),
      c1_p2_d4_06 = mean(c1_p2_d4_06_calc, na.rm = TRUE),
      c1_p2_d4_07 = mean(c1_p2_d4_07_calc, na.rm = TRUE)
    )
  
  # Corrección de NaNs específicos para Ind_p2
  vars_nan <- c("c1_p2_d4_05", "c1_p2_d4_06", "c1_p2_d4_07")
  Ind_p2[vars_nan] <- lapply(Ind_p2[vars_nan], function(x) ifelse(is.nan(x), 0, x))
  Ind_p2 <- Ind_p2 %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  #### Pilar 3: Capacidad y Protocolos (Varios Dataframes)
  
  # P3_1: Capacidad (base_CO_capa)
  Ind_p3_1 <- preparar_base(base_CO_capa, "mod1_gp1_c1", "mod1_gp1_p1") %>% 
    summarise(
      num_obs = n(),
      c1_p3_d5_01 = (sum(mod2_p8 + mod2_p9 + mod2_p10 + mod2_p11 + mod2_p12 +
                           mod2_p13 + mod2_p14 + mod2_p15 + mod2_p16 + mod2_p17)/(n()*10)*100),
      c1_p3_d5_02 = (sum(mod3_mod3_1_mod3_1_1_p18 + mod3_mod3_1_mod3_1_1_p19 +
                           mod3_mod3_1_mod3_1_1_p20 + mod3_mod3_1_mod3_1_1_p21 +
                           mod3_mod3_1_mod3_1_1_p22 + mod3_mod3_1_mod3_1_1_p23 +
                           mod3_mod3_1_mod3_1_1_p24 + mod3_mod3_1_mod3_1_2_p25 +
                           mod3_mod3_1_mod3_1_2_p26 + mod3_mod3_1_mod3_1_2_p27 +
                           mod3_mod3_1_mod3_1_2_p28 + mod3_mod3_1_mod3_1_2_p29 +
                           mod3_mod3_1_mod3_1_2_p30)/(n()*13)*100),
      c1_p3_d5_03 = sum(mod3_mod3_2_p32, na.rm = T)/sum(mod3_mod3_2_p31, na.rm = T)*100,
      c1_p3_d5_04 = (sum(mod4_mod4_1_p33 + mod4_mod4_1_p34 + mod4_mod4_1_p35)/(n()*3)*100),
      c1_p3_d5_05 = sum(mod4_mod4_2_p39, na.rm = T)/sum(mod4_mod4_2_p37, na.rm = T)*100,
      c1_p3_d5_06 = (sum(mod4_mod4_3_p40 + mod4_mod4_3_p41)/(n()*2)*100),
      c1_p3_d5_07 = sum(mod4_mod4_4_p43, na.rm = T)/sum(mod4_mod4_4_p42, na.rm = T)*100
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  Ind_p3_2 <- preparar_base(df_habilidades, "Id_Sector", "id_entidad") %>% 
    summarise(
      # Totales por tipo (para ponderaciones)
      total_tecnicas = sum(num_capacitados_tecnicas, na.rm = TRUE),
      total_socio     = sum(num_capacitados_socioemocionales, na.rm = TRUE),
      
      # Promedios ponderados
      c1_p3_d6_01 = sum(pct_habilidades_tecnicas * num_capacitados_tecnicas, na.rm = TRUE) /
        sum(num_capacitados_tecnicas, na.rm = TRUE),
      c1_p3_d6_02 = sum(pct_habilidades_socioemocionales * num_capacitados_socioemocionales, na.rm = TRUE) /
        sum(num_capacitados_socioemocionales, na.rm = TRUE)
    ) %>% 
    mutate(
      ponderador_tecnicas = total_tecnicas / sum(total_tecnicas),
      ponderador_socio    = total_socio     / sum(total_socio)
    ) %>%
    select(-total_tecnicas, -total_socio) %>% 
    summarise(
      Nivel = 0,
      c1_p3_d6_01 = sum(c1_p3_d6_01 * ponderador_tecnicas) / sum(ponderador_tecnicas),
      c1_p3_d6_02 = sum(c1_p3_d6_02 * ponderador_socio) / sum(ponderador_socio),
      ponderador_tecnicas = 1,
      ponderador_socio = 1
    )
  
  # P3_3: Estándares (base_CO_esta)
  Ind_p3_3 <- preparar_base(base_CO_esta, "mod1_gp1_c1", "mod1_gp1_p1") %>% 
    summarise(
      num_obs = n(),
      c1_p3_d7_01 = (sum(mod2_mod2_1_p6 + mod2_mod2_1_p7 + mod2_mod2_1_p8 + 
                           mod2_mod2_1_p9 + mod2_mod2_1_p10 +
                           mod2_mod2_1_p11 + mod2_mod2_1_p12)/(n()*7)*100),
      c1_p3_d7_02 = (1-mean(as.numeric(mod2_mod2_2_p13)/max(as.numeric(mod2_mod2_2_p13), na.rm = T), na.rm = T))*100,
      c1_p3_d7_03 = (sum(mod3_p14 + mod3_p15 + mod3_p16)/(n()*3)*100),
      c1_p3_d7_04 = (sum(mod4_mod4_1_mod4_1_1_p17 + mod4_mod4_1_mod4_1_1_p18 + mod4_mod4_1_mod4_1_1_p19 + 
                           mod4_mod4_1_mod4_1_1_p20 + mod4_mod4_1_mod4_1_1_p21 + mod4_mod4_1_mod4_1_1_p22 +
                           mod4_mod4_1_mod4_1_1_p23 + mod4_mod4_1_mod4_1_2_p24 + mod4_mod4_1_mod4_1_2_p25 +
                           mod4_mod4_1_mod4_1_2_p26 + mod4_mod4_1_mod4_1_2_p27 + mod4_mod4_1_mod4_1_2_p28 +
                           mod4_mod4_1_mod4_1_2_p29 + mod4_mod4_1_mod4_1_2_p30 + mod4_mod4_1_mod4_1_2_p31)/(n()*15)*100),
      c1_p3_d7_05 = (1-mean(as.numeric(mod4_mod4_2_p36)/max(as.numeric(mod4_mod4_2_p36), na.rm = T), na.rm = T))*100,
      c1_p3_d7_06 = (sum(mod5_mod5_1_p39 + mod5_mod5_1_p40)/(n()*2)*100)
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  # P3_4: Telefónico (base_CO_tele)
  Ind_p3_4 <- preparar_base(base_CO_tele, "mod1_gp1_c1", "mod1_gp1_p1") %>% 
    summarise(
      num_obs = n(),
      c1_p3_d7_07 = sum(as.numeric(mod2_mod2_1_p8) == 1, na.rm = T)/sum(!is.na(mod2_mod2_1_p8))*100,
      c1_p3_d7_08 = sum(as.numeric(mod2_mod2_1_p9) == 1, na.rm = T)/sum(!is.na(mod2_mod2_1_p9))*100,
      c1_p3_d7_09 = (sum(mod2_mod2_1_p10 + mod2_mod2_1_p11 + mod2_mod2_1_p12)/(n()*3)*100),
      c1_p3_d7_10 = sum(as.numeric(mod2_mod2_2_p13) == 1, na.rm = T)/sum(!is.na(mod2_mod2_2_p13))*100,
      c1_p3_d7_11 = (sum(mod2_mod2_2_p14 + mod2_mod2_2_p15 + mod2_mod2_2_p16 + mod2_mod2_2_p17)/(n()*4)*100),
      c1_p3_d7_12 = (sum(mod2_mod2_3_p18 + mod2_mod2_3_p19 + mod2_mod2_3_p20 + mod2_mod2_3_p21 +
                           mod2_mod2_3_p22 + mod2_mod2_3_p23 + mod2_mod2_3_p24 + mod2_mod2_3_gp25_p25 +
                           mod2_mod2_3_p26)/(n()*9)*100),
      c1_p3_d7_13 = (sum(mod2_mod2_4_p27 + mod2_mod2_4_p28 + mod2_mod2_4_p29)/(n()*3)*100)
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  # P3_5: Virtual (base_CO_virt)
  Ind_p3_5 <- preparar_base(base_CO_virt, "mod1_gp1_c1", "mod1_gp1_p1") %>% 
    summarise(
      num_obs = n(),
      c1_p3_d7_14 = (sum(mod2_mod2_1_p5 + mod2_mod2_1_p6 + mod2_mod2_1_p7 +
                           mod2_mod2_1_p8 + mod2_mod2_1_p9 + mod2_mod2_1_p10 +
                           mod2_mod2_1_p11)/(n()*7)*100),
      c1_p3_d7_15 = (sum(mod2_mod2_2_mod2_2_1_p12 + mod2_mod2_2_mod2_2_1_gp12_p13 + 
                           mod2_mod2_2_mod2_2_1_gp12_p14 + mod2_mod2_2_mod2_2_1_gp12_p15 +
                           mod2_mod2_2_mod2_2_1_gp12_p16 + mod2_mod2_2_mod2_2_2_p17 +
                           mod2_mod2_2_mod2_2_2_p18 + mod2_mod2_2_mod2_2_2_p19 +
                           mod2_mod2_2_mod2_2_2_p20 + mod2_mod2_2_mod2_2_2_p21)/(n()*11)*100),
      c1_p3_d7_16 = (sum(mod2_mod2_3_p23 + mod2_mod2_3_p24 + mod2_mod2_3_p25)/(n()*3)*100)
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  #### Pilar 4: Percepción Ciudadana (base_encuesta)
  Ind_p4 <- preparar_base(base_encuesta, "mod1_gp1_c3", "mod1_gp1_c4") %>% 
    summarise(
      num_obs = n(),
      c2_p4_d8_01 = ((mean(as.numeric(mod2_g5_g5_1_p5_1), na.rm = T)-1)/4)*100,
      c2_p4_d8_02 = ((mean(as.numeric(mod2_g5_g5_1_p5_2), na.rm = T)-1)/4)*100,
      c2_p4_d8_03 = ((mean(as.numeric(mod2_g5_g5_1_p5_3), na.rm = T)-1)/4)*100,
      c2_p4_d8_04 = ((mean(as.numeric(mod2_g5_g5_1_p5_4), na.rm = T)-1)/4)*100,
      c2_p4_d8_05 = ((mean(as.numeric(mod2_g5_g5_1_p5_5), na.rm = T)-1)/4)*100,
      c2_p4_d8_06 = ((mean(as.numeric(mod2_g5_g5_1_p5_6), na.rm = T)-1)/4)*100,
      c2_p4_d8_07 = ((mean(as.numeric(mod2_g5_g5_1_p5_7), na.rm = T)-1)/4)*100,
      c2_p4_d8_08 = ((mean(as.numeric(mod2_g5_g5_1_p5_8), na.rm = T)-1)/4)*100,
      c2_p4_d8_09 = ((mean(as.numeric(mod2_g5_g5_1_p5_9), na.rm = T)-1)/4)*100,
      c2_p4_d8_10 = ((mean(as.numeric(mod2_g5_g5_1_p5_10), na.rm = T)-1)/4)*100,
      c2_p4_d8_11 = ((mean(as.numeric(mod2_g5_g5_1_p5_11), na.rm = T)-1)/4)*100,
      c2_p4_d8_12 = sum(mod3_p8 %in% c("1"))/n()*100,
      c2_p4_d9_01 = sum(mod3_gp6_p6 %in% c("1"))/n()*100,
      c2_p4_d9_02 = ((mean(as.numeric(mod3_g7_g7_1_p7_1), na.rm = T)-1)/4)*100,
      c2_p4_d9_03 = ((mean(as.numeric(mod3_g7_g7_1_p7_2), na.rm = T)-1)/4)*100,
      c2_p4_d9_04 = ((mean(as.numeric(mod3_g7_g7_1_p7_3), na.rm = T)-1)/4)*100,
      c2_p4_d9_05 = ((mean(as.numeric(mod3_g7_g7_1_p7_4), na.rm = T)-1)/4)*100,
      c2_p4_d9_06 = ((mean(as.numeric(mod3_g7_g7_1_p7_5), na.rm = T)-1)/4)*100,
      c2_p4_d9_07 = ((mean(as.numeric(mod3_g7_g7_1_p7_6), na.rm = T)-1)/4)*100,
      c2_p4_d9_08 = sum(mod3_p9 %in% c("1"))/n()*100
    ) %>% ungroup() %>% 
    mutate(
      ponderador = num_obs / sum(num_obs)
    ) %>% ponderar_indicadores()
  
  #### CALCULO DE INDICES
  # índices de dimensiones 8 y 9 -> pilar 4 -> componente 2
  Indices_D8_D9_P4_C2 <- Ind_p4 %>% filter(Nivel != 0) %>% 
    mutate(
      indice_p4_d8 = rowMeans(select(., starts_with("c2_p4_d8_")), na.rm = TRUE),
      indice_p4_d9 = rowMeans(select(., starts_with("c2_p4_d9_")), na.rm = TRUE)
    ) %>% 
    mutate(
      indice_p4 = rowMeans(select(., starts_with("indice_p4_")), na.rm = TRUE)
    ) %>%
    mutate(
      indice_c2 = rowMeans(select(., c("indice_p4")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p4_d8, indice_p4_d9, indice_p4, indice_c2
    ) %>% anadir_consolidado()
  
  # índices dimensión 7 que son provenientes de diferentes bases
  Indices_D7_5 <- Ind_p3_5 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p3_d7_14_16 = rowMeans(select(., starts_with("c1_p3_d7_")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p3_d7_14_16
    ) %>% anadir_consolidado()
  
  Indices_D7_4 <- Ind_p3_4 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p3_d7_07_13 = rowMeans(select(., starts_with("c1_p3_d7_")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p3_d7_07_13
    ) %>% anadir_consolidado()
  
  Indices_D7_3 <- Ind_p3_3 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p3_d7_01_06 = rowMeans(select(., starts_with("c1_p3_d7_")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p3_d7_01_06
    ) %>% anadir_consolidado()
  
  Indices_D7 <- Indices_D7_3 %>%
    full_join(Indices_D7_4, by = "Nivel") %>% 
    full_join(Indices_D7_5, by = "Nivel") %>%
    mutate(
      indice_p3_d7 = rowMeans(select(., starts_with("indice_p3_d7_")), na.rm = TRUE)
    ) %>%
    select(Nivel, indice_p3_d7)
  
  # índices de dimensión 6
  Indices_D6_tecnico <- Ind_p3_2 %>% filter(Nivel != 0) %>% 
    select(Nivel, c1_p3_d6_01, ponderador_tecnicas) %>% 
    ungroup() %>% 
    summarise(
      Nivel = 0, 
      # Promedio ponderado para todas las columnas numéricas excepto Nivel y num_obs
      across(where(is.numeric) & !any_of(c("Nivel", "ponderador_tecnicas")), 
             ~ weighted.mean(.x, w = ponderador_tecnicas, na.rm = TRUE))
    )
  
  Indices_D6_socio <- Ind_p3_2 %>% filter(Nivel != 0) %>% 
    select(Nivel, c1_p3_d6_02, ponderador_socio) %>% 
    ungroup() %>% 
    summarise(
      Nivel = 0, 
      # Promedio ponderado para todas las columnas numéricas excepto Nivel y num_obs
      across(where(is.numeric) & !any_of(c("Nivel", "ponderador_socio")), 
             ~ weighted.mean(.x, w = ponderador_socio, na.rm = TRUE))
    )
  
  fila_habilidades <- Indices_D6_tecnico %>% inner_join(Indices_D6_socio, by = "Nivel")
  
  Indices_D6 <- Ind_p3_2 %>% filter(Nivel != 0) %>% 
    select(-ponderador_tecnicas, -ponderador_socio) %>%
    bind_rows(fila_habilidades) %>%
    mutate(
      indice_p3_d6 = rowMeans(select(., starts_with("c1_p3_d6_")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, indice_p3_d6
    )
  
  # índices de dimensión 5
  Indices_D5 <- Ind_p3_1 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p3_d5 = rowMeans(select(., starts_with("c1_p3_d5_")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p3_d5
    ) %>% anadir_consolidado()
  
  # índices de dimensiones 5,6,7 -> pilar 3
  Indices_D5_D6_D7_P3 <- Indices_D5 %>%
    full_join(Indices_D6, by = "Nivel") %>%
    full_join(Indices_D7, by = "Nivel") %>%
    mutate(
      indice_p3 = rowMeans(select(., starts_with("indice_p3_d")), na.rm = TRUE)
    )
  
  # índices de dimensiones 2,3,4 -> pilar 2
  Indices_D2_D3_D4_P2 <- Ind_p2 %>% filter(Nivel != 0) %>%
    mutate(
      # Cálculo con ponderación especial (1/10 - 1/180 - 1/180)
      indice_p2_d2 = c1_p2_d2_01 * (9/10) + c1_p2_d2_02 * (9/180) + c1_p2_d2_03 * (9/180)
    ) %>%
    mutate(
      indice_p2_d3 = rowMeans(select(., starts_with("c1_p2_d3_")), na.rm = TRUE)
    ) %>%
    mutate(
      indice_p2_d4 = rowMeans(select(., starts_with("c1_p2_d4_")), na.rm = TRUE)
    ) %>%
    mutate(
      indice_p2 = rowMeans(select(., starts_with("indice_p2_d")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p2_d2, indice_p2_d3, indice_p2_d4, indice_p2
    ) %>% anadir_consolidado()
  
  # índice de dimensión 1 -> pilar 1
  Indices_D1_1 <- Ind_p1_1 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p1_d1_1 = rowMeans(select(., c("c1_p1_d1_01")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p1_d1_1
    ) %>% anadir_consolidado()
  
  Indices_D1_2 <- Ind_p1_2 %>% filter(Nivel != 0) %>%
    mutate(
      indice_p1_d1_2 = rowMeans(select(.,  c("c1_p1_d1_02")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, ponderador, indice_p1_d1_2
    ) %>% anadir_consolidado()
  
  Indices_D1_P1 <- Indices_D1_1 %>%
    full_join(Indices_D1_2, by = "Nivel") %>%
    mutate(
      indice_p1_d1 = rowMeans(select(., starts_with("indice_p1_d1_")), na.rm = TRUE)
    ) %>%
    mutate(
      indice_p1 = rowMeans(select(., c("indice_p1_d1")), na.rm = TRUE)
    ) %>%
    select(
      Nivel, indice_p1_d1, indice_p1
    )
  
  # índice del componente 1
  Indices_D1_D2_D3_D4_D5_D6_D7_P1_P2_P3_C1 <- Indices_D1_P1 %>%
    full_join(Indices_D2_D3_D4_P2, by = "Nivel") %>%
    full_join(Indices_D5_D6_D7_P3, by = "Nivel") %>%
    mutate(
      indice_c1 = rowMeans(select(., c("indice_p1", "indice_p2", "indice_p3")), na.rm = TRUE)
    )
  
  
  # 4. Consolidación de indicadores
  lista_indicadores <- list(Ind_p1_1 %>% select(-ponderador),
                            Ind_p1_2 %>% select(-ponderador), 
                            Ind_p2 %>% select(-ponderador), 
                            Ind_p3_1 %>% select(-ponderador), 
                            Ind_p3_2 %>% select(-ponderador_tecnicas, -ponderador_socio), 
                            Ind_p3_3 %>% select(-ponderador), 
                            Ind_p3_4 %>% select(-ponderador), 
                            Ind_p3_5 %>% select(-ponderador), 
                            Ind_p4 %>% select(-ponderador))
  
  Indica <- lista_indicadores %>% 
    reduce(full_join, by = "Nivel")
  
  # 5. Consolidación de índices
  lista_indices <- list(Indices_D1_D2_D3_D4_D5_D6_D7_P1_P2_P3_C1, Indices_D8_D9_P4_C2)
  
  Indices <- lista_indices %>% 
    reduce(full_join, by = "Nivel")
  
  if (nivel == "Distrito") {
    Indica <- Indica %>% filter(Nivel == 0)
    Indices <- Indices %>% filter(Nivel == 0)
  }
  
  return(list(Indicadores = Indica, Indices = Indices))
}


######## Cálculo de criterios (siempre sobre entidades)
calculo_criterios <- function() {
  # 1. Función interna para preparar los datos (filtrar y agrupar dinámicamente)
  preparar_base <- function(df, col_entidad = NULL) {
    if(is.null(col_entidad)) stop(paste("Columna ID no definida para nivel:", nivel))
    
    # Agrupa por Nivel
    df_grouped <- df %>% 
      mutate(Nivel = as.numeric(.data[[col_entidad]])) %>% 
      filter(!is.na(Nivel)) %>% 
      group_by(Nivel)
    
    return(df_grouped)
  }
  
  # 2. Calcular los criterios de cada indicador
  
  #### Pilar 1: Gestión Efectiva (base_pqrds)
  Ind_p1 <- preparar_base(base_pqrds, "mod1_gv4_p8") %>% 
    summarise(
      c1_p1_d1_01_cr1 = FALSE,
      c1_p1_d1_02_cr1 = sum(ifelse(mod2_mod2_1_v18 %in% c("0"), 1, 0))/n() != 1,
      c1_p1_d1_02_cr2 = sum(ifelse(mod2_mod2_1_v19 %in% c("0"), 1, 0))/n() != 1,
      c1_p1_d1_02_cr3 = sum(ifelse(mod2_mod2_1_v20 %in% c("0"), 1, 0))/n() != 1,
      c1_p1_d1_02_cr4 = sum(ifelse(mod2_mod2_1_v16 %in% c("0"), 1, 0))/n() != 1,
      c1_p1_d1_02_cr5 = sum(ifelse(mod2_mod2_1_v21 %in% c("0"), 1, 0))/n() != 1
    )
  
  #### Pilar 2: Eficiencia Institucional (base_proceso)
  Ind_p2 <- preparar_base(base_proceso, "mod1_p1") %>% 
    mutate(
      c1_p2_d4_05_calc = (1 - (p22_1 / 240)),
      c1_p2_d4_06_calc = (1 - (p22_2 / 240)),
      c1_p2_d4_07_calc = (1 - (p22_3 / 240))
    ) %>%
    summarise(
      c1_p2_d2_01_cr1 = sum(as.numeric(mod2_gp7_p7), na.rm = T)/sum(as.numeric(mod2_gp6_p6)) <= 0.89,
      c1_p2_d2_02_cr1 = sum(as.numeric(mod3_p9), na.rm = T)/sum(as.numeric(mod3_p8)) <= 0.89,
      c1_p2_d2_03_cr1 = sum(as.numeric(mod3_p12), na.rm = T)/sum(as.numeric(mod3_p10)) <= 0.89,
      c1_p2_d3_01_cr1 = sum(as.numeric(mod4_p13) == 1, na.rm = T)/nrow(base_proceso) <= 0.89,
      c1_p2_d3_02_cr1 = sum(as.numeric(mod5_p16) == 1, na.rm = T)/nrow(base_proceso) <= 0.89,
      c1_p2_d4_01_cr1 = sum(as.numeric(mod6_p17) == 1, na.rm = T)/nrow(base_proceso) <= 0.89,
      c1_p2_d4_02_cr1 = sum(as.numeric(mod6_p18) == 1, na.rm = T)/nrow(base_proceso) <= 0.89,
      c1_p2_d4_03_cr1 = sum(as.numeric(mod7_p20), na.rm = T)/sum(as.numeric(mod7_p19)) <= 0.89,
      c1_p2_d4_04_cr1 = sum(as.numeric(mod8_p21) == 1, na.rm = T)/nrow(base_proceso) <= 0.89,
      c1_p2_d4_05_cr1 = mean(c1_p2_d4_05_calc, na.rm = TRUE) >= 5,
      c1_p2_d4_06_cr1 = mean(c1_p2_d4_06_calc, na.rm = TRUE) >= 5,
      c1_p2_d4_07_cr1 = mean(c1_p2_d4_07_calc, na.rm = TRUE) >= 5
    )
  
  #### Pilar 3: Capacidad y Protocolos (Varios Dataframes)
  
  # P3_1: Capacidad (base_CO_capa)
  Ind_p3_1 <- preparar_base(base_CO_capa, "mod1_gp1_p1") %>% 
    summarise(
      c1_p3_d5_01_cr1 = (sum(mod2_p8)/n()) <= 0.89,
      c1_p3_d5_01_cr2 = (sum(mod2_p9)/n()) <= 0.89,
      c1_p3_d5_01_cr3 = (sum(mod2_p10)/n()) <= 0.89,
      c1_p3_d5_01_cr4 = (sum(mod2_p11)/n()) <= 0.89,
      c1_p3_d5_01_cr5 = (sum(mod2_p12)/n()) <= 0.89,
      c1_p3_d5_01_cr6 = (sum(mod2_p13)/n()) <= 0.89,
      c1_p3_d5_01_cr7 = (sum(mod2_p14)/n()) <= 0.89,
      c1_p3_d5_01_cr8 = (sum(mod2_p15)/n()) <= 0.89,
      c1_p3_d5_01_cr9 = (sum(mod2_p16)/n()) <= 0.89,
      c1_p3_d5_01_cr10 = (sum(mod2_p17)/n()) <= 0.89,
      c1_p3_d5_02_cr1 = (sum(mod3_mod3_1_mod3_1_1_p18)/n()) <= 0.89,
      c1_p3_d5_02_cr2 = (sum(mod3_mod3_1_mod3_1_1_p19)/n()) <= 0.89,
      c1_p3_d5_02_cr3 = (sum(mod3_mod3_1_mod3_1_1_p20)/n()) <= 0.89,
      c1_p3_d5_02_cr4 = (sum(mod3_mod3_1_mod3_1_1_p21)/n()) <= 0.89,
      c1_p3_d5_02_cr5 = (sum(mod3_mod3_1_mod3_1_1_p22)/n()) <= 0.89,
      c1_p3_d5_02_cr6 = (sum(mod3_mod3_1_mod3_1_1_p23)/n()) <= 0.89,
      c1_p3_d5_02_cr7 = (sum(mod3_mod3_1_mod3_1_1_p24)/n()) <= 0.89,
      c1_p3_d5_02_cr8 = (sum(mod3_mod3_1_mod3_1_2_p25)/n()) <= 0.89,
      c1_p3_d5_02_cr9 = (sum(mod3_mod3_1_mod3_1_2_p26)/n()) <= 0.89,
      c1_p3_d5_02_cr10 = (sum(mod3_mod3_1_mod3_1_2_p27)/n()) <= 0.89,
      c1_p3_d5_02_cr11 = (sum(mod3_mod3_1_mod3_1_2_p28)/n()) <= 0.89,
      c1_p3_d5_02_cr12 = (sum(mod3_mod3_1_mod3_1_2_p29)/n()) <= 0.89,
      c1_p3_d5_02_cr13 = (sum(mod3_mod3_1_mod3_1_2_p30)/n()) <= 0.89,
      c1_p3_d5_03_cr1 = sum(mod3_mod3_2_p32, na.rm = T)/sum(mod3_mod3_2_p31, na.rm = T) <= 0.89,
      c1_p3_d5_04_cr1 = (sum(mod4_mod4_1_p33)/n()) <= 0.89,
      c1_p3_d5_05_cr1 = sum(mod4_mod4_2_p39, na.rm = T)/sum(mod4_mod4_2_p37, na.rm = T) <= 0.89,
      c1_p3_d5_05_cr2 = (sum(mod4_mod4_1_p35)/n()) <= 0.89,
      c1_p3_d5_06_cr1 = (sum(mod4_mod4_3_p40)/n()) <= 0.89,
      c1_p3_d5_06_cr2 = (sum(mod4_mod4_3_p41)/n()) <= 0.89,
      c1_p3_d5_07_cr1 = sum(mod4_mod4_4_p43, na.rm = T)/sum(mod4_mod4_4_p42, na.rm = T) <= 0.89
    )
  
  # P3_2: Habilidades (df_habilidades)
  Ind_p3_2 <- preparar_base(df_habilidades, "id_entidad") %>% 
    summarise(
      c1_p3_d6_01_cr1 = sum(pct_habilidades_tecnicas*num_capacitados_tecnicas)/sum(num_capacitados_tecnicas) < 90,
      c1_p3_d6_02_cr1 = sum(pct_habilidades_socioemocionales*num_capacitados_socioemocionales)/sum(num_capacitados_socioemocionales) < 90
    )
  
  # P3_3: Estándares (base_CO_esta)
  Ind_p3_3 <- preparar_base(base_CO_esta, "mod1_gp1_p1") %>% 
    summarise(
      c1_p3_d7_01_cr1 = (sum(mod2_mod2_1_p6)/n()) <= 0.89,
      c1_p3_d7_01_cr2 = (sum(mod2_mod2_1_p7)/n()) <= 0.89,
      c1_p3_d7_01_cr3 = (sum(mod2_mod2_1_p8)/n()) <= 0.89,
      c1_p3_d7_01_cr4 = (sum(mod2_mod2_1_p9)/n()) <= 0.89,
      c1_p3_d7_01_cr5 = (sum(mod2_mod2_1_p10)/n()) <= 0.89,
      c1_p3_d7_01_cr6 = (sum(mod2_mod2_1_p11)/n()) <= 0.89,
      c1_p3_d7_01_cr7 = (sum(mod2_mod2_1_p12)/n()) <= 0.89,
      c1_p3_d7_02_cr1 = (sum(mod2_mod2_2_p13)/n()) > 10, # Cumple si es mayor que 10
      c1_p3_d7_03_cr1 = (sum(mod3_p14)/n()) <= 0.89,
      c1_p3_d7_03_cr2 = (sum(mod3_p15)/n()) <= 0.89,
      c1_p3_d7_03_cr3 = (sum(mod3_p16)/n()) <= 0.89,
      c1_p3_d7_04_cr1 = (sum(mod4_mod4_1_mod4_1_1_p17)/n()) <= 0.89,
      c1_p3_d7_04_cr2 = (sum(mod4_mod4_1_mod4_1_1_p18)/n()) <= 0.89,
      c1_p3_d7_04_cr3 = (sum(mod4_mod4_1_mod4_1_1_p19)/n()) <= 0.89,
      c1_p3_d7_04_cr4 = (sum(mod4_mod4_1_mod4_1_1_p20)/n()) <= 0.89,
      c1_p3_d7_04_cr5 = (sum(mod4_mod4_1_mod4_1_1_p21)/n()) <= 0.89,
      c1_p3_d7_04_cr6 = (sum(mod4_mod4_1_mod4_1_1_p22)/n()) <= 0.89,
      c1_p3_d7_04_cr7 = (sum(mod4_mod4_1_mod4_1_1_p23)/n()) <= 0.89,
      c1_p3_d7_04_cr8 = (sum(mod4_mod4_1_mod4_1_2_p24)/n()) <= 0.89,
      c1_p3_d7_04_cr9 = (sum(mod4_mod4_1_mod4_1_2_p25)/n()) <= 0.89,
      c1_p3_d7_04_cr10 = (sum(mod4_mod4_1_mod4_1_2_p26)/n()) <= 0.89,
      c1_p3_d7_04_cr11 = (sum(mod4_mod4_1_mod4_1_2_p27)/n()) <= 0.89,
      c1_p3_d7_04_cr12 = (sum(mod4_mod4_1_mod4_1_2_p28)/n()) <= 0.89,
      c1_p3_d7_04_cr13 = (sum(mod4_mod4_1_mod4_1_2_p29)/n()) <= 0.89,
      c1_p3_d7_04_cr14 = (sum(mod4_mod4_1_mod4_1_2_p30)/n()) <= 0.89,
      c1_p3_d7_04_cr15 = (sum(mod4_mod4_1_mod4_1_2_p31)/n()) <= 0.89,
      c1_p3_d7_05_cr1 = (sum(as.numeric(mod4_mod4_2_p36))/n()) > 15, # Cumple si es mayor que 15
      c1_p3_d7_06_cr1 = (sum(mod5_mod5_1_p39)/n()) <= 0.89,
      c1_p3_d7_06_cr2 = (sum(mod5_mod5_1_p40)/n()) <= 0.89
    )
  
  # P3_4: Telefónico (base_CO_tele)
  Ind_p3_4 <- preparar_base(base_CO_tele, "mod1_gp1_p1") %>% 
    summarise(
      c1_p3_d7_07_cr1 = sum(as.numeric(mod2_mod2_1_p8) == 1, na.rm = T)/sum(!is.na(mod2_mod2_1_p8)) <= 0.89,
      c1_p3_d7_08_cr1 = sum(as.numeric(mod2_mod2_1_p9) == 1, na.rm = T)/sum(!is.na(mod2_mod2_1_p9)) <= 0.89,
      c1_p3_d7_09_cr1 = (sum(mod2_mod2_1_p10)/n()) <= 0.89,
      c1_p3_d7_09_cr2 = (sum(mod2_mod2_1_p11)/n()) <= 0.89,
      c1_p3_d7_09_cr3 = (sum(mod2_mod2_1_p12)/n()) <= 0.89,
      c1_p3_d7_10_cr1 = sum(as.numeric(mod2_mod2_2_p13) == 1, na.rm = T)/sum(!is.na(mod2_mod2_2_p13)) <= 0.89,
      c1_p3_d7_11_cr1 = (sum(mod2_mod2_2_p14)/n()) <= 0.89,
      c1_p3_d7_11_cr2 = (sum(mod2_mod2_2_p15)/n()) <= 0.89,
      c1_p3_d7_11_cr3 = (sum(mod2_mod2_2_p16)/n()) <= 0.89,
      c1_p3_d7_11_cr4 = (sum(mod2_mod2_2_p17)/n()) <= 0.89,
      c1_p3_d7_12_cr1 = (sum(mod2_mod2_3_p18)/n()) <= 0.89,
      c1_p3_d7_12_cr2 = (sum(mod2_mod2_3_p19)/n()) <= 0.89,
      c1_p3_d7_12_cr3 = (sum(mod2_mod2_3_p20)/n()) <= 0.89,
      c1_p3_d7_12_cr4 = (sum(mod2_mod2_3_p21)/n()) <= 0.89,
      c1_p3_d7_12_cr5 = (sum(mod2_mod2_3_p22)/n()) <= 0.89,
      c1_p3_d7_12_cr6 = (sum(mod2_mod2_3_p23)/n()) <= 0.89,
      c1_p3_d7_12_cr7 = (sum(mod2_mod2_3_p24)/n()) <= 0.89,
      c1_p3_d7_12_cr8 = (sum(mod2_mod2_3_gp25_p25)/n()) <= 0.89,
      c1_p3_d7_12_cr9 = (sum(mod2_mod2_3_p26)/n()) <= 0.89,
      c1_p3_d7_13_cr1 = (sum(mod2_mod2_4_p27)/n()) <= 0.89,
      c1_p3_d7_13_cr2 = (sum(mod2_mod2_4_p28)/n()) <= 0.89,
      c1_p3_d7_13_cr3 = (sum(mod2_mod2_4_p29)/n()) <= 0.89
    )
  
  # P3_5: Virtual (base_CO_virt)
  Ind_p3_5 <- preparar_base(base_CO_virt, "mod1_gp1_p1") %>% 
    summarise(
      c1_p3_d7_14_cr1 = (sum(mod2_mod2_1_p5)/n()) <= 0.89,
      c1_p3_d7_14_cr2 = (sum(mod2_mod2_1_p6)/n()) <= 0.89,
      c1_p3_d7_14_cr3 = (sum(mod2_mod2_1_p7)/n()) <= 0.89,
      c1_p3_d7_14_cr4 = (sum(mod2_mod2_1_p8)/n()) <= 0.89,
      c1_p3_d7_14_cr5 = (sum(mod2_mod2_1_p9)/n()) <= 0.89,
      c1_p3_d7_14_cr6 = (sum(mod2_mod2_1_p10)/n()) <= 0.89,
      c1_p3_d7_14_cr7 = (sum(mod2_mod2_1_p11)/n()) <= 0.89,
      c1_p3_d7_15_cr1 = (sum(mod2_mod2_2_mod2_2_1_p12)/n()) <= 0.89,
      c1_p3_d7_15_cr2 = (sum(mod2_mod2_2_mod2_2_1_gp12_p13)/n()) <= 0.89,
      c1_p3_d7_15_cr3 = (sum(mod2_mod2_2_mod2_2_1_gp12_p14)/n()) <= 0.89,
      c1_p3_d7_15_cr4 = (sum(mod2_mod2_2_mod2_2_1_gp12_p15)/n()) <= 0.89,
      c1_p3_d7_15_cr5 = (sum(mod2_mod2_2_mod2_2_1_gp12_p16)/n()) <= 0.89,
      c1_p3_d7_15_cr6 = (sum(mod2_mod2_2_mod2_2_2_p17)/n()) <= 0.89,
      c1_p3_d7_15_cr7 = (sum(mod2_mod2_2_mod2_2_2_p18)/n()) <= 0.89,
      c1_p3_d7_15_cr8 = (sum(mod2_mod2_2_mod2_2_2_p19)/n()) <= 0.89,
      c1_p3_d7_15_cr9 = (sum(mod2_mod2_2_mod2_2_2_p20)/n()) <= 0.89,
      c1_p3_d7_15_cr10 = (sum(mod2_mod2_2_mod2_2_2_p21)/n()) <= 0.89,
      c1_p3_d7_15_cr11 = (sum(mod2_mod2_2_mod2_2_2_p22)/n()) <= 0.89,
      c1_p3_d7_16_cr1 = (sum(mod2_mod2_3_p23)/n()) <= 0.89,
      c1_p3_d7_16_cr2 = (sum(mod2_mod2_3_p24)/n()) <= 0.89,
      c1_p3_d7_16_cr3 = (sum(mod2_mod2_3_p25)/n()) <= 0.89
    )
  
  #### Pilar 4: Percepción Ciudadana (base_encuesta)
  Ind_p4 <- preparar_base(base_encuesta, "mod1_gp1_c4") %>% 
    summarise(
      # TODOS APLICAN SOLO SI PROMEDIO ES MENOR ESTRICTO A 3
      c2_p4_d8_01_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_1), na.rm = T)) < 3.0,
      c2_p4_d8_02_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_2), na.rm = T)) < 3.0,
      c2_p4_d8_03_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_3), na.rm = T)) < 3.0,
      c2_p4_d8_04_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_4), na.rm = T)) < 3.0,
      c2_p4_d8_05_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_5), na.rm = T)) < 3.0,
      c2_p4_d8_06_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_6), na.rm = T)) < 3.0,
      c2_p4_d8_07_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_7), na.rm = T)) < 3.0,
      c2_p4_d8_08_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_8), na.rm = T)) < 3.0,
      c2_p4_d8_09_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_9), na.rm = T)) < 3.0,
      c2_p4_d8_10_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_10), na.rm = T)) < 3.0,
      c2_p4_d8_11_cr1 = (mean(as.numeric(mod2_g5_g5_1_p5_11), na.rm = T)) < 3.0,
      c2_p4_d8_12_cr1 = sum(mod3_p8 %in% c("1"))/n() < 0.35, # Aplica si es menor estricto a 0.35
      c2_p4_d9_01_cr1 = sum(mod3_gp6_p6 %in% c("1"))/n() < 0.35, # Aplica si es menor estricto a 0.35
      c2_p4_d9_02_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_1), na.rm = T)) < 3.0,
      c2_p4_d9_03_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_2), na.rm = T)) < 3.0,
      c2_p4_d9_04_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_3), na.rm = T)) < 3.0,
      c2_p4_d9_05_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_4), na.rm = T)) < 3.0,
      c2_p4_d9_06_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_5), na.rm = T)) < 3.0,
      c2_p4_d9_07_cr1 = (mean(as.numeric(mod3_g7_g7_1_p7_6), na.rm = T)) < 3.0,
      c2_p4_d9_08_cr1 = sum(mod3_p9 %in% c("1"))/n() < 0.35 # Aplica si es menor estricto a 0.35
    )
  
  # 3. Consolidación de resultados
  lista_criterios <- list(Ind_p1, Ind_p2, Ind_p3_1, Ind_p3_2, Ind_p3_3, Ind_p3_4, Ind_p3_5, Ind_p4)
  
  Indica <- lista_criterios %>% 
    reduce(full_join, by = "Nivel")
  
  out <- Indica %>%
    tidyr::pivot_longer(cols = -Nivel, names_to = "Id_Criterio", values_to = "Critico") %>%
    mutate(Nivel = suppressWarnings(as.numeric(Nivel))) %>%
    filter(!is.na(Nivel)) %>%
    left_join(df_entidades %>% select(Id_Entidad, Entidad), by = c("Nivel" = "Id_Entidad")) %>%
    filter(!is.na(Entidad), !is.na(Critico)) %>%
    select(Id_Criterio, Nivel, Critico) %>%
    rename(Id_Entidad = Nivel)
  
  return(out)
}
