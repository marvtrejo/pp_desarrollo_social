r_tarjeta <- card(
  layout_column_wrap(
    width = 1,
    bloque_info("Número de programas", "numpp_r"),
    tags$div("Presupuesto", br(),
             tags$div(style = paste0(
               "border-top: 4px solid #BFBFBF; width: 100%; margin-top: 0; margin-bottom: 1rem;"
             )),
             uiOutput("presupuesto_r")
             )
    )
  )
    
# Layout ----
ramos_layout <- nav_panel(
  title = "Ramos sociales",
  layout_sidebar(
    sidebar = sidebar(
      tags$div(
        tags$h4(icon("sliders"),
                tags$strong("Filtros"),
                class = "mt-2 mb-3 text-center"
        ),
        tags$hr(style = "border-top: 4px solid #ccc; margin-top: 0; margin-bottom: 1rem;")
      ),
      uiOutput("r_ciclo"),
      uiOutput("r_ramo"),
      open = "open"
      # bg = "#fafafa"
    ),
    
    uiOutput("titulo_r"),
    layout_columns(
      col_widths = c(3, 9),
      r_tarjeta,
      layout_column_wrap(
        width = 1,
        highchartOutput("hcpresupuesto_r"),
        # highchartOutput("hcproppres_r"),
        tags$div(
          dataTableOutput("tabla_ramo"),
          style = "height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 5px;"
        )
      )
    )
  )
)