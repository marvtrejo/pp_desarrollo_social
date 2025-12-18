# Tarjetas ----
banner_pp <- card(
  layout_column_wrap(
    width = 1/6,
    bloque_info("Año de inicio", "ainicio_srv"),
    bloque_info("Derecho asociado", "derecho_srv"),
    bloque_info("Tipo de apoyos", "apoyos_srv"),
    bloque_info("Tipo de población atendida", "rankpres1_srv"),
    bloque_info("Rank de presupuesto", "rankpres2_srv"),
    bloque_info("Número de evaluaciones", "noeval_srv")
    )
)


# Datos generales ----
resumen_pp <- navset_card_pill(
    nav_panel(tags$div(icon("indent"),
                       " Descripción"),
              textOutput("descr_srv")),
    nav_panel(tags$div(icon("rectangle-list"),
                       "Resumen narrativo"),
              tableOutput("mir_srv")),
  height = "400px",
  full_screen = TRUE
  )

# Gráficas ----
graficas_pp <-   navset_card_pill(
  nav_panel(tags$div(icon("money-bill-trend-up"),
                     "Presupuesto"),
            highchartOutput("hcpres_srv")
            ),
  
  nav_panel(tags$div(icon("users-viewfinder"),
                     "Poblaciones"),
            highchartOutput("hcpob_srv")
            ),
  
  nav_panel(tags$div(icon("timeline"),
                     "Cambios"),
            highchartOutput("hcevprog_srv")
            ),
  
  nav_panel(tags$div(icon("clipboard-check"),
                     "Evaluaciones"),
            highchartOutput("hc_eval_srv")
            ),
  full_screen = TRUE
  )

# Layout ----
pp_layout <- nav_panel(
  title = "Programas sociales",
  layout_sidebar(
    sidebar = sidebar(
      tags$div(
        tags$h4(icon("sliders"),
                tags$strong("Filtros"),
                class = "mt-2 mb-3 text-center"
                ),
        tags$hr(style = "border-top: 4px solid #ccc; margin-top: 0; margin-bottom: 1rem;")
        ),
      uiOutput("selCiclo"),
      uiOutput("selRamo"),
      uiOutput("selPP"),
      open = "open"
      # bg = "#fafafa"
      ),
    
    uiOutput("titulopp_srv"),
    banner_pp,
    layout_column_wrap(
      width = 1/2,
      resumen_pp,
      graficas_pp 
      )
    )
  )