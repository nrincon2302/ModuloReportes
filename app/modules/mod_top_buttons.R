top_buttons_ui <- function(id, pestanas) {
  ns <- NS(id)
  
  botones_lista <- list()
  pilar_keys <- names(pestanas)
  
  for (i in seq_along(pilar_keys)) {
    id_btn <- pilar_keys[i]
    clase_color <- case_when(
      id_btn == "indice" ~ "btn-indice",
      id_btn == "mejora" ~ "btn-mejora",
      TRUE ~ "btn-pilar"
    )
    clase_active <- ifelse(id_btn == "indice", "active", "")
    
    botones_lista[[length(botones_lista) + 1]] <- actionButton(
      ns(paste0("sub_btn_", id_btn)), 
      pestanas[id_btn], 
      class = paste("top-button", clase_color, clase_active)
    )
    
    if (id_btn == "indice" || (i == length(pilar_keys) - 1 && pilar_keys[i+1] == "mejora")) {
      botones_lista[[length(botones_lista) + 1]] <- div(class = "button-separator")
    }
  }
  
  div(class = "top-buttons-container", botones_lista)
}

top_buttons_server <- function(id, pestanas, active_sub_tab, active_dimension) {
  moduleServer(id, function(input, output, session) {
    
    lapply(names(pestanas), function(id_btn) {
      observeEvent(input[[paste0("sub_btn_", id_btn)]], {
        active_sub_tab(id_btn)
        active_dimension(NULL)
      }, ignoreInit = TRUE)
    })
    
    observe({
      req(active_sub_tab())
      shinyjs::removeClass(selector = ".top-button", class = "active")
      shinyjs::addClass(paste0("sub_btn_", active_sub_tab()), "active")
    })
    
  })
}