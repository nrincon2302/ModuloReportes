library(shiny)
library(shinyWidgets)

# ============================================
# MODULO GLOBAL DE FILTROS
# Se monta UNA sola vez a nivel de aplicacion (ver ui.R / server.R)
# para que el estado de los filtros persista al cambiar de pestana.
# ============================================

filtros_ui <- function(id) {
  ns <- NS(id)

  all_sectores  <- sort(df_sectores$Sector)
  all_entidades <- sort(df_entidades$Entidad)
  all_canales   <- obtener_canales()
  canales_con_subcanal <- all_canales[all_canales != "Sin Canal"]

  end_year    <- as.integer(format(Sys.Date(), "%Y"))
  anios_fijos <- as.character(seq(2025, end_year))
  meses_fijos <- sprintf("%02d", 1:12)
  names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                          "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

  div(
    class = "filter-panel",

    # --- FILTRO DE FECHAS ---
    tags$h4("Periodo de Analisis",
            style = "color: #225495; margin-top: 0; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
    fluidRow(
      column(6,
             pickerInput(ns("filtro_anio"), "Año",
                         choices = anios_fijos, multiple = TRUE,
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"))
      ),
      column(6, uiOutput(ns("mes_selector_ui")))
    ),

    hr(),

    # --- NIVEL DE CONSULTA ---
    tags$h4("Nivel de Consulta",
            style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
    radioButtons(ns("filtro_nivel"), NULL,
                 choices = c("Distrito", "Sector", "Entidad"),
                 selected = "Distrito"),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'Sector'", ns("filtro_nivel")),
      div(style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("sector_select_all"), "Todos"), " | ",
          actionLink(ns("sector_deselect_all"), "Ninguno")),
      div(class = "scrollable-checkbox-group",
          checkboxGroupInput(ns("sector_checks"), NULL,
                             choices = all_sectores, selected = all_sectores))
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'Entidad'", ns("filtro_nivel")),
      div(style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("entidad_select_all"), "Todos"), " | ",
          actionLink(ns("entidad_deselect_all"), "Ninguno")),
      div(class = "scrollable-checkbox-group",
          uiOutput(ns("entidad_checks_ui")))
    ),

    hr(),

    # --- FILTRO CANAL (filtro global) ---
    tags$h4("Filtrar por Canal",
            style = "color: #225495; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),

    conditionalPanel(
      condition = sprintf("input['%s'] == false", ns("filtro_canal_detalle")),
      div(style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("canal_select_all"),    "Todos"), " | ",
          actionLink(ns("canal_deselect_all"),  "Ninguno")),
      div(class = "scrollable-checkbox-group",
          uiOutput(ns("canales_checks_ui")))
    ),

    checkboxInput(ns("filtro_canal_detalle"),
                  "Filtrar por subcanal (afecta todas las graficas)",
                  value = FALSE),

    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("filtro_canal_detalle")),
      selectInput(ns("filtro_canal_selector"), "Seleccione el canal:",
                  choices = canales_con_subcanal,
                  selected = if (length(canales_con_subcanal) > 0) canales_con_subcanal[1] else NULL),
      div(style = "margin-bottom: 5px; font-size: 0.9em;",
          actionLink(ns("subcanal_select_all"), "Todos"), " | ",
          actionLink(ns("subcanal_deselect_all"), "Ninguno")),
      div(class = "scrollable-checkbox-group",
          uiOutput(ns("subcanales_checks_ui")))
    ),

    hr(),

    actionButton(ns("btn_aplicar_filtros"), "Aplicar Filtros",
                 icon = icon("filter"),
                 class = "btn-primary btn-sm",
                 style = "width: 100%; margin-bottom: 15px; font-weight: bold;")
  )
}

filtros_server <- function(id, id_componente_reactive, signals) {
  moduleServer(id, function(input, output, session) {

    # ============================================
    # 1. SELECTOR DE MES (depende de los anios elegidos)
    # ============================================
    output$mes_selector_ui <- renderUI({
      req(input$filtro_anio)
      anios <- input$filtro_anio
      meses_fijos <- sprintf("%02d", 1:12)
      names(meses_fijos) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
      if (length(anios) == 0) return(NULL)
      if (length(anios) == 1) {
        pickerInput(session$ns("filtro_mes"), "Mes",
                    choices = meses_fijos, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                    selected = meses_fijos)
      } else {
        tagList(lapply(anios, function(a) {
          pickerInput(session$ns(paste0("filtro_mes_", a)), label = a,
                      choices = meses_fijos, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `none-selected-text` = "Todos"),
                      selected = meses_fijos)
        }))
      }
    })

    # ============================================
    # 2. ESTADO EN VIVO (se actualiza con cada click en los inputs)
    # ============================================
    filtros_estado <- reactive({
      list(
        btn_actualizar_fechas  = input$btn_aplicar_filtros,
        filtro_anio            = input$filtro_anio,
        filtro_mes             = input$filtro_mes,
        filtro_nivel           = input$filtro_nivel,
        sector_checks          = input$sector_checks,
        entidad_checks         = input$entidad_checks,
        canales_checks         = input$canales_checks,
        filtro_canal_detalle   = input$filtro_canal_detalle,
        filtro_canal_selector  = input$filtro_canal_selector,
        subcanales_checks      = input$subcanales_checks
      )
    })

    # ============================================
    # 3. SNAPSHOT CONFIRMADO (solo al pulsar "Aplicar Filtros")
    # ============================================
    btn_click <- reactiveVal(0)
    observeEvent(input$btn_aplicar_filtros, {
      btn_click(btn_click() + 1)
    })

    filtros_evento <- eventReactive(input$btn_aplicar_filtros, {
      anios_sel <- input$filtro_anio
      filtro_periodos <- character(0)

      if (!is.null(anios_sel) && length(anios_sel) > 0) {
        if (length(anios_sel) == 1) {
          sel <- input$filtro_mes
          if (is.null(sel) || length(sel) == 0) sel <- sprintf("%02d", 1:12)
          filtro_periodos <- paste0(anios_sel, "-", sprintf("%02d", as.integer(sel)))
        } else {
          meses_comb <- unlist(lapply(anios_sel, function(a) {
            sel <- input[[paste0("filtro_mes_", a)]]
            if (is.null(sel) || length(sel) == 0) return(character(0))
            paste0(a, "-", sprintf("%02d", as.integer(sel)))
          }))
          filtro_periodos <- meses_comb
        }
      } else {
        filtro_periodos <- NULL
      }

      subcanal_ids <- NULL
      if (isTRUE(input$filtro_canal_detalle) &&
          !is.null(input$filtro_canal_selector) &&
          !is.null(input$subcanales_checks) &&
          length(input$subcanales_checks) > 0 &&
          exists("df_canales")) {
        subcanal_ids <- df_canales %>%
          filter(Canal == input$filtro_canal_selector,
                 Subcanal %in% input$subcanales_checks) %>%
          pull(Id_Subcanal)
      }

      canales_sel <- if (isTRUE(input$filtro_canal_detalle)) NULL else input$canales_checks

      list(
        filtro_anio           = input$filtro_anio,
        filtro_mes            = filtro_periodos,
        filtro_nivel          = input$filtro_nivel,
        sector_checks         = input$sector_checks,
        entidad_checks        = input$entidad_checks,
        canales_checks        = canales_sel,
        filtro_canal_detalle  = input$filtro_canal_detalle,
        filtro_canal_selector = input$filtro_canal_selector,
        subcanales_checks     = input$subcanales_checks,
        subcanal_ids          = subcanal_ids
      )
    }, ignoreNULL = FALSE)

    # ============================================
    # 4. HELPERS UI DINAMICOS
    # ============================================
    entidades_disponibles <- reactive({
      req(exists("df_entidades"))
      if (input$filtro_nivel == "Entidad" && length(input$sector_checks) > 0) {
        ids <- df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector)
        df_entidades %>% filter(Id_Sector %in% ids) %>% pull(Entidad) %>% sort()
      } else {
        sort(df_entidades$Entidad)
      }
    })

    output$entidad_checks_ui <- renderUI({
      ent <- entidades_disponibles()
      checkboxGroupInput(session$ns("entidad_checks"), NULL, choices = ent, selected = ent)
    })

    output$canales_checks_ui <- renderUI({
      req(signals$canal())
      canales <- tryCatch(obtener_canales(), error = function(e) character(0))
      if (length(canales) == 0) return(div("No hay canales disponibles"))
      seleccion_actual   <- isolate(input$canales_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) canales else seleccion_actual[seleccion_actual %in% canales]
      checkboxGroupInput(session$ns("canales_checks"), NULL, choices = canales, selected = seleccion_filtrada)
    })

    output$subcanales_checks_ui <- renderUI({
      req(input$filtro_canal_selector)
      req(signals$canal())
      subs <- tryCatch(obtener_subcanales(input$filtro_canal_selector), error = function(e) character(0))
      if (length(subs) == 0) return(div("No hay subcanales disponibles"))
      seleccion_actual  <- isolate(input$subcanales_checks)
      seleccion_filtrada <- if (is.null(seleccion_actual)) subs else seleccion_actual[seleccion_actual %in% subs]
      checkboxGroupInput(session$ns("subcanales_checks"), NULL, choices = subs, selected = seleccion_filtrada)
    })

    # ============================================
    # 5. BOTONES DE SELECCION RAPIDA
    # ============================================
    observeEvent(input$sector_select_all, {
      updateCheckboxGroupInput(session, "sector_checks", selected = sort(df_sectores$Sector))
    })
    observeEvent(input$sector_deselect_all, {
      updateCheckboxGroupInput(session, "sector_checks", selected = character(0))
    })
    observeEvent(input$entidad_select_all, {
      updateCheckboxGroupInput(session, "entidad_checks", selected = entidades_disponibles())
    })
    observeEvent(input$entidad_deselect_all, {
      updateCheckboxGroupInput(session, "entidad_checks", selected = character(0))
    })
    observeEvent(input$subcanal_select_all, {
      req(input$filtro_canal_selector)
      subs <- obtener_subcanales(input$filtro_canal_selector)
      updateCheckboxGroupInput(session, "subcanales_checks", selected = subs)
    })
    observeEvent(input$subcanal_deselect_all, {
      updateCheckboxGroupInput(session, "subcanales_checks", selected = character(0))
    })
    observeEvent(input$canal_select_all, {
      canales <- tryCatch(obtener_canales(), error = function(e) character(0))
      updateCheckboxGroupInput(session, "canales_checks", selected = canales)
    })
    observeEvent(input$canal_deselect_all, {
      updateCheckboxGroupInput(session, "canales_checks", selected = character(0))
    })

    # ============================================
    # 6. IDS SELECCIONADOS (nivel de consulta)
    # ============================================
    ids_seleccionados <- reactive({
      nivel <- input$filtro_nivel
      if (nivel == "Distrito") return(unique(df_entidades$Id_Entidad))
      if (nivel == "Sector") {
        req(input$sector_checks)
        todos_sectores <- sort(df_sectores$Sector)
        if (length(input$sector_checks) == length(todos_sectores) &&
            all(sort(input$sector_checks) == todos_sectores))
          return(unique(df_entidades$Id_Sector))
        return(df_sectores %>% filter(Sector %in% input$sector_checks) %>% pull(Id_Sector))
      }
      if (nivel == "Entidad") {
        req(input$entidad_checks)
        return(df_entidades %>% filter(Entidad %in% input$entidad_checks) %>% pull(Id_Entidad))
      }
    })

    # ============================================
    # 7. RESET AL CAMBIAR DE COMPONENTE (Satisfaccion <-> Prestacion)
    # ============================================
    observeEvent(id_componente_reactive(), {
      req(exists("periodos"), exists("df_sectores"), exists("df_entidades"))
      anios_fijos <- as.character(sort(unique(periodos$Anio)))
      meses_fijos <- sprintf("%02d", 1:12)
      updatePickerInput(session, "filtro_anio", selected = anios_fijos)
      updatePickerInput(session, "filtro_mes", selected = meses_fijos)
      updateRadioButtons(session, "filtro_nivel", selected = "Distrito")
      updateCheckboxGroupInput(session, "sector_checks", selected = sort(df_sectores$Sector))
      updateCheckboxGroupInput(session, "entidad_checks", selected = sort(df_entidades$Entidad))
      updateCheckboxInput(session, "filtro_canal_detalle", value = FALSE)
      canales_disponibles <- obtener_canales()
      if (length(canales_disponibles) > 0) {
        updateSelectInput(session, "filtro_canal_selector", selected = canales_disponibles[1])
        subs <- obtener_subcanales(canales_disponibles[1])
        updateCheckboxGroupInput(session, "subcanales_checks", selected = subs)
      }
    }, ignoreInit = TRUE)

    # ============================================
    # 8. RETORNO AL SERVER PRINCIPAL
    # ============================================
    list(
      estado             = filtros_estado,
      evento             = filtros_evento,
      btn_click          = btn_click,
      nivel_click        = btn_click,      # alias: server.R lo usa tambien
      ids_seleccionados  = ids_seleccionados
    )
  })
}