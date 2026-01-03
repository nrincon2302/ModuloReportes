navigation_ui <- function(id) {
  ns <- NS(id)
  
  # usamos onclick explícito para forzar un input namespaced consistente
  btn_satis_onclick <- sprintf("Shiny.setInputValue('%s', 'satisfaccion', {priority: 'event'})", ns("nav_click"))
  btn_prest_onclick <- sprintf("Shiny.setInputValue('%s', 'prestacion', {priority: 'event'})", ns("nav_click"))
  
  div(
    class = "sidebar-nav",
    tags$img(src = "logo.png", class = "sidebar-logo"),
    div(class = "sidebar-title", "Componentes"),
    # NOTA: dejamos class 'lateral-button' y añadimos onclick que escribe en input nav_click namespaced
    tags$button(
      id = ns("nav_satisfaccion"),
      type = "button",
      class = "lateral-button active",
      onclick = btn_satis_onclick,
      HTML("Satisfacción y<br>Experiencia Ciudadana")
    ),
    tags$button(
      id = ns("nav_prestacion"),
      type = "button",
      class = "lateral-button",
      onclick = btn_prest_onclick,
      HTML("Prestación del<br>Servicio")
    )
  )
}

navigation_server <- function(id, active_main_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Escuchamos el input que setea el onclick (nav_click)
    observeEvent(input$nav_click, {
      val <- input$nav_click
      if (is.null(val)) return()
      
      if (val == "satisfaccion") {
        active_main_tab("satisfaccion")
        # actualizar clases con JS para reflejar estado activo
        shinyjs::runjs(sprintf("
          document.getElementById('%s').classList.add('active');
          document.getElementById('%s').classList.remove('active');
        ", ns("nav_satisfaccion"), ns("nav_prestacion")))
      } else if (val == "prestacion") {
        active_main_tab("prestacion")
        shinyjs::runjs(sprintf("
          document.getElementById('%s').classList.add('active');
          document.getElementById('%s').classList.remove('active');
        ", ns("nav_prestacion"), ns("nav_satisfaccion")))
      }
    }, ignoreNULL = TRUE)
  })
}
