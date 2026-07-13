library(testthat)

test_that("PQRSD excluye No Aplica del denominador por etiqueta", {
  respuestas <- c(
    rep("Cumple", 6),
    rep("No cumple", 2),
    "No Aplica",
    "N/A"
  )

  conteo <- pqrds_contar_etiquetas(respuestas)

  expect_equal(conteo$cumple, 6)
  expect_equal(conteo$incumple, 2)
  expect_equal(conteo$denom, 8)
  expect_equal(conteo$pct_c, 0.75)
})

test_that("PQRSD trata p19 = 7 como oportunidad no evaluable", {
  datos <- data.frame(
    mod2_mod2_1_v18 = c("0", "0", "0"),
    mod2_mod2_1_v19 = c("0", "0", "0"),
    mod2_mod2_1_v20 = c("0", "0", "0"),
    mod2_mod2_1_v16 = c("0", "1", "0"),
    mod2_mod2_1_v21 = c("0", "0", "0"),
    mod2_mod2_1_gp19_p19 = c("7", "2", "1"),
    mod2_mod2_1_v26 = c("No Aplica", "No cumple", "Cumple")
  )

  expect_equal(
    pqrds_pct_cumplimiento_compuesto(
      datos,
      c("mod2_mod2_1_v18", "mod2_mod2_1_v19", "mod2_mod2_1_v20",
        "mod2_mod2_1_v16", "mod2_mod2_1_v21")
    ),
    2 / 3 * 100
  )
})
