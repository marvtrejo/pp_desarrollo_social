# Conoce ----
home_conoce <- card(
  card_body(
    tags$h1(style = "text-align: center; font-size: 4rem; font-weight: bold; margin: 0; color: #1a4146;",
            icon("dolly"),
            "Conoce"),
    tags$hr(style = "border-top: 4px solid #ccc; margin: 0.25rem 0 0.75rem 0;"),
    tags$div("contenido", style = "text-align: left; margin-top: 0; margin-bottom: 1rem;"),
    actionButton("go_psociales","Ir a programas sociales"))
)

# Compara ----
home_compara <- card(
  card_body(tags$h1("Compara", 
                    style = "text-align: center; font-weight: bold; margin: 0;color: #1a4146;"),
            tags$hr(style = "border-top: 4px solid #ccc; margin: 0.25rem 0 0.75rem 0;"),
            tags$div("contenido", style = "text-align: left; margin-top: 0; margin-bottom: 1rem;"),
            actionButton("go_mpp","Ir a pobreza en México"))
)

# Analiza ----
home_analiza <- card(
  card_body(tags$h1("Analiza", 
                    style = "text-align: center; font-weight: bold; margin: 0;color: #1a4146;"),
            tags$hr(style = "border-top: 4px solid #ccc; margin: 0.25rem 0 0.75rem 0;"),
            tags$div("contenido", style = "text-align: left; margin-top: 0; margin-bottom: 1rem;"),
            actionButton("go_datos","Ir a datos"))
)


# Layout ----
home_layout<- nav_panel(
  title = "Inicio",
  style = "margin: 0; padding: 0;",
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/icon?family=Material+Icons",
      rel = "stylesheet"
    )
  ),
  br(),
  layout_column_wrap(width = 1/3,
                     home_conoce,
                     home_compara,
                     home_analiza)
)

mpp_layout<- nav_panel(
  title = "Pobreza",
  style = "margin: 0; padding: 0;",
  "Aquí van lo de pobreza"
)
