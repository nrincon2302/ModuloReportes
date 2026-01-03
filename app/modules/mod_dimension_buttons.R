dimension_buttons_ui <- function(id, dimensiones) {
  ns <- NS(id)
  
  botones_dimensiones <- lapply(names(dimensiones), function(dim_id) {
    actionButton(ns(paste0("dim_btn_", dim_id)),
                 dimensiones[[dim_id]],
                 class = "dimension-button")
  })
  
  div(class = "dimension-buttons-container", botones_dimensiones)
}

dimension_buttons_server <- function(id, dimensiones, active_dimension) {
  moduleServer(id, function(input, output, session) {
    
    lapply(names(dimensiones), function(dim_id) {
      observeEvent(input[[paste0("dim_btn_", dim_id)]], {
        active_dimension(dim_id)
      }, ignoreInit = TRUE)
    })
    
    observe({
      dim <- active_dimension()
      if (!is.null(dim)) {
        shinyjs::removeClass(selector = ".dimension-button", class = "active")
        shinyjs::addClass(paste0("dim_btn_", dim), "active")
      }
    })
    
  })
}