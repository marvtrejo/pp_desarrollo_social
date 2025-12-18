# Función ----
datos <- function(anio_sel, ramo_sel, pp_sel, nivel) {
  
  df_datos <- if(nivel == "Ramo") {
    inv_fed %>% filter(ciclo %in% anio_sel,
                       id_ramo %in% ramo_sel)
  } else {
    inv_fed %>% filter(ciclo %in% anio_sel,
                       id_ramo %in% ramo_sel,
                       nombre %in% pp_sel)
  }
  
  # df_datos <- inv_fed %>% 
  #   filter(ciclo %in% anio_sel,
  #          id_ramo %in% ramo_sel)
  # 
  # if (nivel == "Programa") {
  #   df <- df_datos %>% filter(nombre %in% pp_sel)
  # }
  
  return(df_datos)
}


# Layout ----
datos_layout <- nav_panel(
  title = "Datos",
  layout_sidebar(
    sidebar = sidebar(
      tags$div(
        tags$h4(icon("sliders"),
                tags$strong("Filtros"),
                class = "mt-2 mb-3 text-center"
        ),
        tags$hr(style = "border-top: 4px solid #ccc; margin-top: 0; margin-bottom: 1rem;")
      ),
      radioButtons(inputId = "nivel",
                   label = HTML('<h5 class="text-center"><strong>Nivel:</strong></h5>'),
                   choices = list("Ramo", "Programa"),
                   selected = "Ramo"),
      uiOutput("selCiclo_datos"),
      conditionalPanel(
        condition = "input.nivel == 'Ramo' || input.nivel == 'Programa'",
        uiOutput("selRamo_datos")
      ),
      conditionalPanel(
        condition = "input.nivel == 'Programa'",
        uiOutput("selPP_datos")
      ),
      # uiOutput("selRamo_datos"),
      # uiOutput("selPP_datos"),
      downloadButton("descargar_datos", "Descargar datos"),
      open = "open",
      bg = "#fafafa"
    ),
    
    tags$div(tags$h2(icon("table"),
                     tags$strong("Datos"),
                     class = "mt-2 mb-3 text-center")
             ),
    dataTableOutput("tabla_datos")
    )
  )