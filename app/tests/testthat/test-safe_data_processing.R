library(testthat)

test_that("generar_df_vacio crea un data frame con columnas esperadas", {
  result <- generar_df_vacio("ddc_coPr_CapIns")
  
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), length(cols_base_CO_capa))
  expect_named(result, cols_base_CO_capa)
})

test_that("generar_df_vacio falla con formulario desconocido", {
  expect_error(generar_df_vacio("formulario_inexistente"), "Formulario no reconocido")
})
