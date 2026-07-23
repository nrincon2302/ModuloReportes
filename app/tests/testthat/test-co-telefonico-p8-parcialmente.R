library(dplyr)

calculo_path <- file.path("..", "..", "utils", "calculo_indicadores.R")
calculo_exprs <- parse(calculo_path)

evaluar_asignacion <- function(nombre) {
  for (expr in calculo_exprs) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(as.character(expr[[2]]), nombre)) {
      eval(expr, envir = .GlobalEnv)
      return(invisible(TRUE))
    }
  }
  stop(sprintf("No se encontró la asignación de %s", nombre))
}

evaluar_asignacion("recodificar_interes")
evaluar_asignacion("pct_sum_cols")
evaluar_asignacion("vars_tele")

test_that("p8 telefónico usa la recodificación de cumplimiento parcial", {
  expect_true("mod2_mod2_1_p8" %in% vars_tele)

  datos <- data.frame(
    mod2_mod2_1_p8 = c("1", "3", "2", "9", "", "desconocido", NA_character_)
  )
  resultado <- recodificar_interes(datos, "mod2_mod2_1_p8")$mod2_mod2_1_p8

  expect_equal(resultado[1:3], c(1, 0.5, 0))
  expect_true(all(is.na(resultado[4:7])))
  expect_equal(pct_sum_cols(resultado), 50)
})

test_that("los valores históricos de p8 conservan su resultado", {
  positivos <- recodificar_interes(
    data.frame(mod2_mod2_1_p8 = c("1", "1")),
    "mod2_mod2_1_p8"
  )
  negativos <- recodificar_interes(
    data.frame(mod2_mod2_1_p8 = c("2", "2")),
    "mod2_mod2_1_p8"
  )

  expect_equal(pct_sum_cols(positivos$mod2_mod2_1_p8), 100)
  expect_equal(pct_sum_cols(negativos$mod2_mod2_1_p8), 0)
  expect_true(is.na(pct_sum_cols(c(NA_real_, NA_real_))))
})

test_that("el valor parcial se propaga con las fórmulas agregadas existentes", {
  indicador_p8 <- 50
  canal_telefonico <- mean(c(indicador_p8, rep(100, 6)))
  dimension_7 <- mean(c(100, canal_telefonico, 100))
  pilar_3 <- mean(c(100, 100, dimension_7))
  indice_c1 <- mean(c(100, 100, pilar_3))

  expect_equal(canal_telefonico, 92.8571428571429)
  expect_equal(dimension_7, 97.6190476190476)
  expect_equal(pilar_3, 99.2063492063492)
  expect_equal(indice_c1, 99.7354497354497)
})
