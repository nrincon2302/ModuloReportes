normalize_odk_column_names <- function(df) {
  if (nrow(df) == 0 || ncol(df) == 0) {
    return(df)
  }
  
  # Guardar los nombres originales para debug
  original_names <- names(df)
  
  # Aplicar transformaciones según la convención de OData
  new_names <- original_names %>%
    # Reemplazar caracteres especiales de OData
    gsub("@odata\\.", "", .) %>%           # Eliminar prefijo @odata.
    gsub("@", "_", .) %>%                  # Reemplazar @ restantes
    gsub("\\$", "_", .) %>%                # Reemplazar $ por _
    gsub("\\.", "_", .) %>%                # Reemplazar . por _
    gsub("-", "_", .) %>%                  # Reemplazar - por _
    gsub("__id$", "", .) %>%               # Eliminar sufijo __id de metadatos
    gsub("_{2,}", "_", .) %>%              # Múltiples _ a uno solo
    gsub("^_|_$", "", .)                   # Eliminar _ al inicio/final
  
  # Identificar y manejar nombres vacíos o duplicados
  empty_idx <- which(new_names == "" | is.na(new_names))
  if (length(empty_idx) > 0) {
    warning(sprintf("Se encontraron %d columnas sin nombre válido, se renombrarán", length(empty_idx)))
    for (i in empty_idx) {
      new_names[i] <- paste0("unnamed_col_", i)
    }
  }
  
  # Manejar duplicados
  if (any(duplicated(new_names))) {
    new_names <- make.unique(new_names, sep = "_")
    warning("Se encontraron nombres duplicados, se han hecho únicos")
  }
  
  # Aplicar los nuevos nombres
  names(df) <- new_names
  
  return(df)
}


safe_bind_rows <- function(df1, df2) {
  # Convertir a data.frame y limpiar rownames
  df1 <- as.data.frame(df1, stringsAsFactors = FALSE)
  df2 <- as.data.frame(df2, stringsAsFactors = FALSE)
  rownames(df1) <- NULL
  rownames(df2) <- NULL
  
  # Eliminar columnas con nombres vacíos
  if (ncol(df1) > 0) {
    empty_cols_1 <- which(names(df1) == "" | is.na(names(df1)))
    if (length(empty_cols_1) > 0) {
      df1 <- df1[, -empty_cols_1, drop = FALSE]
    }
  }
  
  if (ncol(df2) > 0) {
    empty_cols_2 <- which(names(df2) == "" | is.na(names(df2)))
    if (length(empty_cols_2) > 0) {
      df2 <- df2[, -empty_cols_2, drop = FALSE]
    }
  }
  
  # Si alguna está vacía, devolver la otra
  if (nrow(df1) == 0 || ncol(df1) == 0) {
    return(df2)
  }
  if (nrow(df2) == 0 || ncol(df2) == 0) {
    return(df1)
  }
  # Si ambos están vacíos, retornar df2
  if ((nrow(df1) == 0 || ncol(df1) == 0) && (nrow(df2) == 0 || ncol(df2) == 0)) {
    return(df2)
  }
  
  # Columnas comunes
  common_cols <- intersect(names(df1), names(df2))
  
  if (length(common_cols) == 0) {
    warning("No hay columnas comunes para combinar")
    return(df1)
  }
  
  
  # Usar dplyr::bind_rows directamente con selección de columnas
  result <- dplyr::bind_rows(
    df1 %>% select(all_of(common_cols)),
    df2 %>% select(all_of(common_cols))
  )
  
  return(result)
}
