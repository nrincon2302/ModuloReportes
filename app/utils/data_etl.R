# --- FUNCIÓN HELPER PARA CREAR COLUMNA 'periodo' ---
# Esta función toma la columna Date (YYYY-MM-DD) y crea periodo (YYYY-MM)
agregar_columna_periodo <- function(df) {
  if (nrow(df) > 0 && "Date" %in% names(df)) {
    df$periodo <- substr(as.character(df$Date), 1, 7) # Extrae "YYYY-MM"
  } else if (nrow(df) > 0 && "EndRecord" %in% names(df)) {
    df$periodo <- substr(as.character(df$EndRecord), 1, 7)
  } else if (nrow(df) > 0) {
    # Si no encuentra fecha, intentar buscar columnas tipo fecha o dejar NA
    warning("No se encontró columna Date/date para crear periodo.")
    df$periodo <- NA
  }
  return(df)
}


# --- FUNCIÓN HELPER PARA ASIGNAR SUBCANAL DE FORMA SEGURA ---
# Permite buscar la columna incluso si tiene guiones o nombres raros
asignar_subcanal <- function(df, col_name) {
  if (nrow(df) > 0 && col_name %in% names(df)) {
    return(as.character(df[[col_name]]))
  } else {
    return(NA_character_) # Retorna NA si la columna no existe
  }
}


descargar_encuestas <- function() {
  suppressWarnings({
    # 1. Descargas
    base_SatExp_pres_raw <- tryCatch({ get_odk_submissions("ddc_SatExp_p4") }, error = function(e) generar_df_vacio("ddc_SatExp_p4"))
    base_SatExp_tele_raw <- tryCatch({ get_odk_submissions("ddc_SatExp_ct") }, error = function(e) generar_df_vacio("ddc_SatExp_ct"))
    base_SatExp_virt_raw <- tryCatch({ get_odk_submissions("ddc_SatExp_cv") }, error = function(e) generar_df_vacio("ddc_SatExp_cv"))
    
    # 2. Procesar fechas
    base_SatExp_pres <<- agregar_columna_periodo(base_SatExp_pres_raw)
    base_SatExp_tele <<- agregar_columna_periodo(base_SatExp_tele_raw)
    base_SatExp_virt <<- agregar_columna_periodo(base_SatExp_virt_raw)
    
    # 3. Unificar
    base_encuesta <<- safe_bind_rows(base_SatExp_pres, base_SatExp_tele)
    base_encuesta <<- safe_bind_rows(base_encuesta, base_SatExp_virt)
    
    # 4. Resto de bases (Descargar y procesar fecha inmediatamente)
    base_proceso_raw <- tryCatch({ get_odk_submissions("ddc_CalProc") }, error = function(e) generar_df_vacio("ddc_CalProc"))
    base_proceso <<- agregar_columna_periodo(base_proceso_raw)
    
    base_pqrds_raw   <- tryCatch({ get_odk_submissions("ddc_pqrsd") }, error = function(e) generar_df_vacio("ddc_pqrsd"))
    base_pqrds   <<- agregar_columna_periodo(base_pqrds_raw)
    procesar_pqrds() # IMPORTANTE PARA ESTANDARIZAR CÓDIGOS DE CRUZAMIENTO !!!!!!!
    
    base_CO_tele_raw <- tryCatch({ get_odk_submissions("ddc_coTel") }, error = function(e) generar_df_vacio("ddc_coTel"))
    base_CO_tele <<- agregar_columna_periodo(base_CO_tele_raw)
    
    base_CO_virt_raw <- tryCatch({ get_odk_submissions("ddc_coVrt") }, error = function(e) generar_df_vacio("ddc_coVrt"))
    base_CO_virt <<- agregar_columna_periodo(base_CO_virt_raw)
    
    base_CO_capa_raw <- tryCatch({ get_odk_submissions("ddc_coPr_CapIns") }, error = function(e) generar_df_vacio("ddc_coPr_CapIns"))
    base_CO_capa <<- agregar_columna_periodo(base_CO_capa_raw)
    
    base_CO_esta_raw <- tryCatch({ get_odk_submissions("ddc_coPr_StdyPrtc") }, error = function(e) generar_df_vacio("ddc_coPr_StdyPrtc"))
    base_CO_esta <<- agregar_columna_periodo(base_CO_esta_raw)
    cat("✓ Bases ODK descargadas y fechas estandarizadas.\n")
  })
}


transformar_bases <- function() {
  # ---------------------------------------------------------
  # A. SATISFACCIÓN Y EXPERIENCIA (SatExp)
  # ---------------------------------------------------------
  
  # 1. SatExp Presencial
  # Canal: Presencial / Subcanal: mod1-gp0-p00
  if(exists("base_SatExp_pres")) {
    base_SatExp_pres$Canal    <<- "Presencial"
    base_SatExp_pres$Subcanal <<- asignar_subcanal(base_SatExp_pres, "mod1_gp0_p00")
  }
  
  # 2. SatExp Telefónico
  # Canal: Telefónico / Subcanal: mod1-gp0-p00
  if(exists("base_SatExp_tele")) {
    base_SatExp_tele$Canal    <<- "Telefónico"
    base_SatExp_tele$Subcanal <<- asignar_subcanal(base_SatExp_tele, "mod1_gp0_p00")
  }
  
  # 3. SatExp Virtual
  # Canal: Virtual / Subcanal: mod1-gp0-p00
  if(exists("base_SatExp_virt")) {
    base_SatExp_virt$Canal    <<- "Virtual"
    base_SatExp_virt$Subcanal <<- asignar_subcanal(base_SatExp_virt, "mod1_gp0_p00")
  }
  
  # --- IMPORTANTE: RE-UNIFICAR BASE_ENCUESTA ---
  # Como modificamos las bases individuales arriba, debemos recrear base_encuesta
  # para que incluya las nuevas columnas Canal y Subcanal.
  base_encuesta <<- safe_bind_rows(base_SatExp_pres, base_SatExp_tele)
  base_encuesta <<- safe_bind_rows(base_encuesta, base_SatExp_virt)
  
  
  # ---------------------------------------------------------
  # B. CIUDADANO Y ORIENTACIÓN (CO)
  # ---------------------------------------------------------
  
  # 1. CO Capacitación (Presencial)
  # Canal: Presencial / Subcanal: mod1-gp3-p7
  if(exists("base_CO_capa")) {
    base_CO_capa$Canal    <<- "Presencial"
    base_CO_capa$Subcanal <<- asignar_subcanal(base_CO_capa, "mod1_gp3_p7")
  }
  
  # 2. CO Estudio (Presencial)
  # Canal: Presencial / Subcanal: mod1-gp3-p5
  if(exists("base_CO_esta")) {
    base_CO_esta$Canal    <<- "Presencial"
    base_CO_esta$Subcanal <<- asignar_subcanal(base_CO_esta, "mod1_gp3_p5")
  }
  
  # 3. CO Telefónico
  # Canal: Telefónico / Subcanal: No registrado (NA)
  if(exists("base_CO_tele")) {
    base_CO_tele$Canal    <<- "Telefónico"
    base_CO_tele$Subcanal <<- NA_character_
  }
  
  # 4. CO Virtual
  # Canal: Virtual / Subcanal: mod2-mod2_2-mod2_2_1-p12 (Lógica especial: si es 1 -> 6)
  if(exists("base_CO_virt")) {
    col_target <<- "mod2_mod2_2_mod2_2_1_p12"
    
    base_CO_virt$Canal <<- "Virtual"
    
    # Verificamos si la columna existe antes de aplicar la lógica
    if (col_target %in% names(base_CO_virt)) {
      # Obtenemos valores originales
      vals <- base_CO_virt[[col_target]]
      # Aplicamos lógica: si es 1 (o "1"), poner 6 (o "6"), sino dejar 7
      # Nota: forzamos a character para consistencia
      base_CO_virt$Subcanal <<- ifelse(vals == 1 | vals == "1", "6",
                                       ifelse(vals == 0 | vals == "0", "7", 
                                              as.character(vals)))
    } else {
      base_CO_virt$Subcanal <<- NA_character_
    }
  }
  
  
  # ---------------------------------------------------------
  # C. OTRAS BASES (Procesos y PQRDS)
  # ---------------------------------------------------------
  # Se agregan como NA para mantener estructura rectangular (tidy data)
  
  if(exists("base_proceso")) {
    base_proceso$Canal    <<- NA_character_
    base_proceso$Subcanal <<- NA_character_
  }
  
  if(exists("base_pqrds")) {
    base_pqrds$Canal    <<- NA_character_
    base_pqrds$Subcanal <<- NA_character_
  }
  
}


proceso_etl <- function() {
  # ===========================================
  # 1. DESCARGA DE DATOS DESDE ODK
  # ===========================================
  descargar_encuestas()
  
  # ===========================================
  # 2. MODIFICACIÓN DE BASES PARA AGREGAR COLUMNAS ÚTILES
  # ===========================================
  transformar_bases()
  cat("✓ Columnas Canal y Subcanal asignadas correctamente.\n")
}
