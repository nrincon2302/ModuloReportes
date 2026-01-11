library(testthat)

test_that("agregar_columna_periodo usa Date si existe", {
  df <- data.frame(Date = "2024-08-15")
  result <- agregar_columna_periodo(df)
  
  expect_equal(result$periodo, "2024-08")
})

test_that("agregar_columna_periodo usa EndRecord si no hay Date", {
  df <- data.frame(EndRecord = "2023-01-05T10:00:00")
  result <- agregar_columna_periodo(df)
  
  expect_equal(result$periodo, "2023-01")
})

test_that("agregar_columna_periodo advierte si no hay columnas de fecha", {
  df <- data.frame(valor = 1)
  
  expect_warning(result <- agregar_columna_periodo(df), "No se encontró columna Date")
  expect_true(is.na(result$periodo))
})

test_that("asignar_subcanal retorna NA si la columna no existe", {
  df <- data.frame(otra = "x")
  
  result <- asignar_subcanal(df, "mod1_gp0_p00")
  
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("asignar_subcanal devuelve el vector de la columna", {
  df <- data.frame(mod1_gp0_p00 = c("A", "B"))
  
  result <- asignar_subcanal(df, "mod1_gp0_p00")
  
  expect_equal(result, c("A", "B"))
})
