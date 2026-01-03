# ui.R - RESPONSIVE & OPTIMIZADO
library(shiny)
library(shinyjs)
library(highcharter)

fluidPage(
  useShinyjs(),
  
  # Meta tags para responsive
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler('showOverlay', function(_) {
        document.getElementById('global-overlay').classList.remove('overlay-hidden');
      });
      Shiny.addCustomMessageHandler('hideOverlay', function(_) {
        document.getElementById('global-overlay').classList.add('overlay-hidden');
      });

      Shiny.addCustomMessageHandler('addClass', function(x) {
        var el = document.getElementById(x.id);
        if (el) el.classList.add(x.class);
      });
      Shiny.addCustomMessageHandler('removeClass', function(x) {
        var el = document.getElementById(x.id);
        if (el) el.classList.remove(x.class);
      });
      
      // Mantener conexión activa (anti-timeout)
      setInterval(function() {
        Shiny.setInputValue('keepalive', Math.random());
      }, 30000); // cada 30 segundos

      function reflowHighcharts() {
        if (typeof Highcharts === 'undefined') return;
        if (!Highcharts.charts) return;
        Highcharts.charts.forEach(function(chart) {
          if (chart) chart.reflow();
        });
      }

      window.addEventListener('orientationchange', function() {
        setTimeout(reflowHighcharts, 300);
      });

      window.addEventListener('resize', function() {
        clearTimeout(window.__hc_reflow__);
        window.__hc_reflow__ = setTimeout(reflowHighcharts, 200);
      });
    ")),
    
    tags$style(HTML("
      /* ============================================ */
      /* VARIABLES CSS PARA FÁCIL MANTENIMIENTO */
      /* ============================================ */
      :root {
        --color-rojo: #E3272A;
        --color-azul: #225495;
        --color-amarillo: #F9D248;
        --color-negro: #000000;
        --color-blanco: #FFFFFF;
        --color-gris-claro: #F5F5F5;
        --color-gris-medio: #CCCCCC;
        --color-gris-oscuro: #666666;
        
        --spacing-xs: 5px;
        --spacing-sm: 10px;
        --spacing-md: 15px;
        --spacing-lg: 25px;
        --spacing-xl: 40px;
        
        --border-radius: 10px;
        --transition-speed: 0.25s;
      }
      
      /* ============================================ */
      /* FUENTES */
      /* ============================================ */
      @font-face {
        font-family: 'Museo Sans 500';
        src: url('fonts/MuseoSans-500.otf') format('truetype');
        font-weight: 500;
        font-style: normal;
        font-display: swap; /* Optimización carga fuentes */
      }

      @font-face {
        font-family: 'Museo Sans 700';
        src: url('fonts/MuseoSans-700.otf') format('truetype');
        font-weight: 700;
        font-style: normal;
        font-display: swap;
      }

      @font-face {
        font-family: 'Museo Sans 900';
        src: url('fonts/MuseoSans-900.otf') format('truetype');
        font-weight: 900;
        font-style: normal;
        font-display: swap;
      }

      @font-face {
        font-family: 'Museo Sans Cond 500';
        src: url('fonts/MuseoSansCond-500.otf') format('truetype');
        font-weight: 500;
        font-style: normal;
        font-display: swap;
      }

      @font-face {
        font-family: 'Museo Sans Cond 900';
        src: url('fonts/MuseoSansCond-900.otf') format('truetype');
        font-weight: 900;
        font-style: normal;
        font-display: swap;
      }
      
      /* ============================================ */
      /* BASE STYLES */
      /* ============================================ */
      * {
        box-sizing: border-box;
        margin: 0;
        padding: 0;
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
      }
      
      body {
        background-color: var(--color-rojo);
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
        font-weight: 500;
        overflow-x: hidden;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Museo Sans Cond 900', Arial, sans-serif;
      }
      
      strong, b {
        font-family: 'Museo Sans Cond 900', Arial, sans-serif;
        font-weight: 900;
      }
      
      input, select, textarea, button {
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
      }
      
      label {
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
      }
      
      /* ============================================ */
      /* HEADER RESPONSIVE */
      /* ============================================ */
      .main-header {
        background-color: var(--color-rojo);
        padding: var(--spacing-md) var(--spacing-lg);
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,.2);
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: var(--spacing-md);
        position: sticky;
        top: 0;
        z-index: 1000;
      }

      .main-title {
        color: var(--color-blanco);
        font-size: clamp(18px, 4vw, 28px); /* Responsive font size */
        font-family: 'Museo Sans Cond 900', Arial, sans-serif;
        font-weight: 900;
        margin: 0;
        flex: 1;
        min-width: 200px;
      }

      .reload-btn {
        background: #003366;
        color: white;
        border: none;
        padding: var(--spacing-sm) var(--spacing-md);
        border-radius: 8px;
        font-weight: 600;
        font-size: clamp(12px, 2vw, 14px);
        display: flex;
        gap: 8px;
        align-items: center;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        transition: all var(--transition-speed);
        cursor: pointer;
        white-space: nowrap;
      }
      
      .reload-btn:hover {
        background: #004A99;
        transform: translateY(-1px);
        box-shadow: 0 3px 8px rgba(0,0,0,0.25);
      }
      
      /* ============================================ */
      /* LAYOUT PRINCIPAL RESPONSIVE */
      /* ============================================ */
      .main-container {
        display: flex;
        flex-direction: row;
        min-height: calc(100vh - 80px);
        background-color: var(--color-rojo);
      }

      /* SIDEBAR */
      .sidebar-nav {
        width: 250px;
        background-color: var(--color-blanco);
        padding: var(--spacing-xl);
        display: flex;
        flex-direction: column;
        gap: 12px;
        align-items: center;
        border-right: none;
        overflow-y: auto;
        flex-shrink: 0;
        border-radius: 18px;
        box-shadow: 0 8px 18px rgba(0, 0, 0, 0.08);
        margin: var(--spacing-lg);
        position: sticky;
        top: var(--spacing-lg);
        height: fit-content;
      }

      .sidebar-logo {
        display: block;
        margin: 6px auto 30px auto;
        max-height: 85px;
        max-width: 100%;
        height: auto;
      }
      
      .sidebar-title {
        font-family: 'Museo Sans Cond 900', Arial, sans-serif;
        font-weight: 800;
        color: var(--color-azul);
        margin: 6px 0 30px 0;
        font-size: 20px;
        text-align: center;
      }
      
      .lateral-button {
        background-color: #DDDDDD;
        color: #333333;
        border: none;
        padding: 16px 18px;
        border-radius: 12px;
        font-family: 'Museo Sans Cond 900', Arial, sans-serif;
        font-weight: 700;
        font-size: 17px;
        cursor: pointer;
        transition: all var(--transition-speed);
        box-shadow: 0 5px 12px rgba(0,0,0,.10);
        text-align: center;
        min-height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;
        line-height: 1.05;
        width: 100%;
      }

      .lateral-button:hover {
        background-color: #F5C800;
        transform: translateX(4px);
        box-shadow: 0 8px 16px rgba(0,0,0,.18);
      }

      .lateral-button.active {
        background-color: var(--color-rojo);
        color: var(--color-blanco);
        font-weight: 700;
        transform: translateX(4px) scale(1.08);
        box-shadow: 0 12px 24px rgba(0,0,0,0.20);
        min-height: 90px;
      }

      .lateral-button:not(.active) {
        opacity: 0.92;
        transform: scale(0.99);
      }

      /* CONTENT AREA */
      .content-area {
        flex: 1;
        padding: var(--spacing-lg);
        display: flex;
        flex-direction: column;
        overflow: auto;
        min-width: 0; /* Fix flex overflow */
        background-color: transparent;
      }
      
      .white-box {
        background-color: var(--color-blanco);
        border-radius: 15px;
        padding: var(--spacing-xl);
        box-shadow: 0 6px 12px rgba(0,0,0,0.3);
        display: flex;
        flex-direction: column;
        flex: 1;
        overflow: auto;
        min-height: 0; /* Fix flex overflow */
      }

      /* ============================================ */
      /* VISUALIZATION CONTAINER RESPONSIVE */
      /* ============================================ */
      .visualization-container {
        display: flex;
        gap: var(--spacing-lg);
        flex-wrap: wrap;
        min-height: 0;
      }
      
      .filter-panel {
        background-color: var(--color-gris-claro);
        padding: var(--spacing-lg);
        border-radius: var(--border-radius);
        width: 100%;
        max-width: 280px;
        flex-shrink: 0;
        height: fit-content;
        position: sticky;
        top: 0;
      }
      
      .plot-area {
        flex: 1;
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: var(--spacing-lg);
        min-width: 0;
      }

      .plot-area-rows {
        display: flex;
        flex-direction: column;
        gap: var(--spacing-lg);
      }

      .plot-row {
        display: grid;
        grid-template-columns: repeat(2, minmax(300px, 1fr));
        gap: var(--spacing-lg);
        min-width: 0;
      }

      .plot-row-full {
        grid-template-columns: 1fr;
      }
      
      .plot-box {
        background-color: var(--color-blanco);
        border: 1px solid #EEEEEE;
        border-radius: var(--border-radius);
        padding: var(--spacing-md);
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        min-height: 280px;
        display: flex;
        flex-direction: column;
      }
      
      .plot-box-half {
        grid-column: span 1;
      }
      
      .plot-box-full {
        grid-column: 1 / -1;
        min-height: 300px;
      }

      /* Centrar gauge cuando está solo */
      .center-when-alone {
        grid-column: 1 / -1;
        max-width: 760px;
        margin: 0 auto;
        width: 100%;
      }

      .plot-box-gauge .shiny-spinner-output-container,
      .plot-box-pilar .shiny-spinner-output-container {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 100%;
      }

      .plot-box-gauge .highchart-container {
        margin: 0 auto;
      }
      
      /* ============================================ */
      /* BOTONES SUPERIORES (PILARES) RESPONSIVE */
      /* ============================================ */
      .top-buttons-container {
        display: flex;
        gap: var(--spacing-lg);
        padding: var(--spacing-md) 0;
        margin-bottom: var(--spacing-lg);
        flex-wrap: wrap;
        justify-content: center;
        align-items: center;
      }
      
      .button-separator {
        width: 2px;
        height: 60px;
        background: linear-gradient(to bottom, transparent, var(--color-gris-medio) 20%, var(--color-gris-medio) 80%, transparent);
        margin: 0 var(--spacing-sm);
        flex-shrink: 0;
      }
      
      .top-button {
        border: 2px solid var(--color-gris-medio);
        padding: var(--spacing-sm);
        border-radius: 12px;
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
        font-weight: 700;
        font-size: 15px;
        cursor: pointer;
        transition: all var(--transition-speed);
        box-shadow: 0 3px 6px rgba(0,0,0,.1);
        min-width: 140px;
        min-height: 70px;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        flex-shrink: 0;
        word-break: break-word;
        white-space: normal;
      }
      
      .top-button:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 12px rgba(0,0,0,.2);
      }
      
      .btn-indice {
        background-color: #E8F4FF;
        color: var(--color-azul);
      }
      
      .btn-pilar {
        background-color: #E8F4E8;
        color: var(--color-azul);
      }
      
      .btn-mejora {
        background-color: #FFF8E1;
        color: var(--color-negro);
      }
      
      .top-button.active {
        background-color: var(--color-azul);
        color: var(--color-blanco);
        border-color: var(--color-azul);
        box-shadow: 0 4px 8px rgba(34, 84, 149, 0.4);
      }

      .top-button:not(.active) {
        background-color: #F0F0F0;
        color: var(--color-gris-oscuro);
        border-color: #DDDDDD;
        opacity: 0.9;
        transform: scale(0.95);
      }
      
      /* ============================================ */
      /* BOTONES DE DIMENSIONES RESPONSIVE */
      /* ============================================ */
      .dimension-buttons-container {
        display: flex;
        gap: var(--spacing-md);
        margin-bottom: var(--spacing-lg);
        flex-wrap: wrap;
        justify-content: center;
        padding: var(--spacing-md);
        background-color: var(--color-gris-claro);
        border-radius: var(--border-radius);
      }

      .dimension-button {
        background-color: var(--color-blanco);
        color: var(--color-azul);
        border: 2px solid var(--color-azul);
        padding: 12px 20px;
        border-radius: 8px;
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
        font-weight: 400;
        font-size: 15px;
        cursor: pointer;
        transition: all var(--transition-speed);
        box-shadow: 0 2px 4px rgba(0,0,0,.1);
        min-width: 160px;
        text-align: center;
      }
      
      .dimension-button:hover {
        background-color: #F0F8FF;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,.15);
      }
      
      .dimension-button.active {
        background-color: var(--color-azul);
        color: var(--color-blanco);
        box-shadow: inset 0 2px 4px rgba(0,0,0,.2);
      }

      .dimension-button:not(.active) {
        background-color: #FAFAFA;
        color: #777777;
        border-color: var(--color-gris-medio);
        opacity: 0.9;
        transform: scale(0.96);
      }
      
      /* ============================================ */
      /* FORMS & FILTERS */
      /* ============================================ */
      .filter-panel h4 {
        margin-top: var(--spacing-md);
        margin-bottom: var(--spacing-sm);
        color: var(--color-negro);
        font-family: 'Museo Sans Cond 500', Arial, sans-serif;
        font-weight: 700;
        font-size: 1em;
      }
      
      .filter-panel h4:first-child {
        margin-top: 0;
      }
      
      .scrollable-checkbox-group {
        max-height: 130px;
        overflow-y: auto;
        border: 1px solid var(--color-gris-medio);
        border-radius: 5px;
        padding: var(--spacing-sm);
        background-color: var(--color-blanco);
      }

      /* ============================================ */
      /* OVERLAY DE CARGA */
      /* ============================================ */
      #global-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        background: rgba(255,255,255,0.95);
        backdrop-filter: blur(3px);
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        z-index: 99999;
        gap: 20px;
      }
      
      .overlay-hidden {
        display: none !important;
      }
      
      .loader {
        width: 48px;
        height: 48px;
        border: 5px solid var(--color-azul);
        border-bottom-color: transparent;
        border-radius: 50%;
        animation: spin .8s linear infinite;
      }
      
      @keyframes spin {
        to { transform: rotate(360deg); }
      }

      .loading-text {
        font-size: 18px;
        color: var(--color-azul);
        font-weight: 700;
        text-align: center;
        max-width: 80%;
      }

      .loading-subtext {
        font-size: 14px;
        color: var(--color-gris-oscuro);
        text-align: center;
        max-width: 80%;
      }

      /* ============================================ */
      /* UTILIDADES */
      /* ============================================ */
      .force-remove {
        display: none !important;
      }

      .placeholder-content {
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 300px;
        font-size: 1.2em;
        color: #888888;
        text-align: center;
        padding: var(--spacing-lg);
      }

      /* ============================================ */
      /* MEDIA QUERIES RESPONSIVE */
      /* ============================================ */
      
      /* TABLETS (< 1024px) */
      @media (max-width: 1024px) {
        .main-container {
          flex-direction: column;
        }
        
        .sidebar-nav {
          width: 100%;
          border-right: none;
          border-bottom: 4px solid var(--color-rojo);
          padding: var(--spacing-lg);
          margin: 0;
          border-radius: 0;
          box-shadow: none;
          position: static;
          flex-direction: row;
          flex-wrap: wrap;
          justify-content: center;
          gap: var(--spacing-sm);
        }
        
        .sidebar-logo {
          height: 60px;
          margin: 0;
        }
        
        .sidebar-title {
          width: 100%;
          margin: var(--spacing-sm) 0;
          font-size: 18px;
        }
        
        .lateral-button {
          min-width: 180px;
          min-height: 70px;
          font-size: 15px;
        }
        
        .content-area {
          padding: var(--spacing-md);
        }
        
        .white-box {
          padding: var(--spacing-lg);
        }
        
        .filter-panel {
          max-width: 100%;
          position: static;
        }
        
        .plot-area {
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        }

        .plot-row {
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        }

        .plot-row-full {
          grid-template-columns: 1fr;
        }
      }
      
      /* MÓVILES (< 768px) */
      @media (max-width: 768px) {
        .main-header {
          padding: var(--spacing-sm);
          flex-direction: column;
        }
        
        .main-title {
          font-size: 18px;
          text-align: center;
        }
        
        .reload-btn {
          width: 100%;
          justify-content: center;
        }
        
        .sidebar-nav {
          padding: var(--spacing-md);
        }
        
        .lateral-button {
          min-width: 150px;
          min-height: 60px;
          font-size: 14px;
          padding: var(--spacing-sm);
        }
        
        .content-area {
          padding: var(--spacing-sm);
        }
        
        .white-box {
          padding: var(--spacing-md);
          border-radius: var(--spacing-sm);
        }
        
        .visualization-container {
          flex-direction: column;
          gap: var(--spacing-md);
        }
        
        .filter-panel {
          padding: var(--spacing-md);
        }
        
        .plot-area {
          grid-template-columns: 1fr;
          gap: var(--spacing-md);
        }

        .plot-row {
          grid-template-columns: 1fr;
          gap: var(--spacing-md);
        }

        .plot-row-full {
          grid-template-columns: 1fr;
        }
        
        .plot-box {
          min-height: 250px;
        }
        
        .top-buttons-container {
          gap: var(--spacing-sm);
          padding: var(--spacing-sm) 0;
        }
        
        .top-button {
          min-width: 120px;
          min-height: 60px;
          font-size: 13px;
          padding: 8px;
        }
        
        .button-separator {
          display: none; /* Ocultar separadores en móvil */
        }
        
        .dimension-buttons-container {
          gap: var(--spacing-sm);
          padding: var(--spacing-sm);
        }
        
        .dimension-button {
          min-width: 140px;
          font-size: 14px;
          padding: var(--spacing-sm) var(--spacing-md);
        }
      }
      
      /* MÓVILES PEQUEÑOS (< 480px) */
      @media (max-width: 480px) {
        .main-title {
          font-size: 16px;
        }
        
        .lateral-button {
          min-width: 100%;
          min-height: 50px;
        }
        
        .top-button {
          min-width: 100px;
          font-size: 12px;
        }
        
        .dimension-button {
          min-width: 120px;
          font-size: 13px;
        }
        
        .filter-panel h4 {
          font-size: 0.9em;
        }
        
        .scrollable-checkbox-group {
          max-height: 100px;
        }
      }

      /* ============================================ */
      /* MEJORAS DE RENDIMIENTO */
      /* ============================================ */
      
      /* Usar transform en lugar de top/left para animaciones */
      .top-button:hover,
      .dimension-button:hover,
      .lateral-button:hover {
        will-change: transform;
      }
      
      /* Optimizar highcharts responsive */
      .highcharts-container {
        width: 100% !important;
        height: 100% !important;
      }
      
      /* Reducir renders innecesarios */
      .shiny-plot-output,
      .highcharts-container {
        contain: layout style paint;
      }
    "))
  ),
  
  # Header
  div(
    class = "main-header",
    h1("Seguimiento, acompañamiento y evaluación del servicio a la ciudadanía",
       class = "main-title"),
    actionButton("reload_app",
                 label = tagList(tags$i(class = "fa fa-refresh"), "Actualizar"),
                 class = "reload-btn")
  ),
  
  # Overlay de carga mejorado
  div(
    id = "global-overlay",
    class = "overlay-hidden",
    div(class = "loader"),
    div(class = "loading-text", "Cargando datos del sistema..."),
    div(class = "loading-subtext", "Por favor espere, esto puede tomar unos segundos")
  ),
  
  # Contenedor principal
  div(
    class = "main-container",
    
    # Navegación lateral
    div(
      class = "sidebar-nav",
      tags$img(src = "logo.png", class = "sidebar-logo"),
      div(class = "sidebar-title", "Componentes"),
      actionButton("nav_satisfaccion",
                   HTML("Satisfacción y<br>Experiencia Ciudadana"),
                   class = "lateral-button active"),
      actionButton("nav_prestacion",
                   HTML("Prestación del<br>Servicio"),
                   class = "lateral-button")
    ),
    
    # Área de contenido
    div(
      class = "content-area",
      uiOutput("main_content_wrapper")
    )
  )
)
