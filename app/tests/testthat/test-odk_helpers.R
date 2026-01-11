library(testthat)

test_that("normalize_odk_column_names normaliza caracteres especiales", {
  df <- data.frame(`@odata.etag` = 1, `meta@id` = 2, `a.b1` = 3, `a-b2` = 4, `a$c` = 5)
  result <- normalize_odk_column_names(df)
  
  expect_named(result, c("X_odata_etag", "meta_id", "a_b1", "a_b2", "a_c"))
})

test_that("normalize_odk_column_names renombra columnas vacías", {
  df <- data.frame(x = 1, y = 2)
  names(df) <- c("", "valida")
  
  expect_warning(result <- normalize_odk_column_names(df), "sin nombre válido")
  expect_named(result, c("unnamed_col_1", "valida"))
})

test_that("normalize_odk_column_names resuelve duplicados", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("dup", "dup")
  
  expect_warning(result <- normalize_odk_column_names(df), "nombres duplicados")
  expect_named(result, c("dup", "dup_1"))
})

test_that("safe_bind_rows combina solo columnas comunes", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(b = 3, c = 4)
  
  result <- safe_bind_rows(df1, df2)
  
  expect_named(result, "b")
  expect_equal(result$b, c(2, 3))
})

test_that("safe_bind_rows advierte si no hay columnas comunes", {
  df1 <- data.frame(a = 1)
  df2 <- data.frame(b = 2)
  
  expect_warning(result <- safe_bind_rows(df1, df2), "No hay columnas comunes")
  expect_equal(result, df1)
})

test_that("safe_bind_rows elimina columnas sin nombre", {
  df1 <- data.frame(a = 1, b = 2)
  names(df1) <- c("", "a")
  df2 <- data.frame(a = 3)
  
  result <- safe_bind_rows(df1, df2)
  
  expect_named(result, "a")
  expect_equal(result$a, c(2, 3))
})
