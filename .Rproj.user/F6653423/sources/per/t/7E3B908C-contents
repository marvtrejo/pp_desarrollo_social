# Librerías ----
if (!require(pacman)) install.packages("pacman")

library(pacman)
p_load(readxl,
      tidyverse,
      ggplot2,
      dplyr,
      bslib,
      scales,
      plotly,
      DT,
      shinycssloaders,
      shiny,
      shinyjs,
      bsicons,
      fontawesome,
      tidyr,
      highcharter)


# Datos ----
inv_fed <- read_excel("inv_fed.xlsx", sheet = "inv_fed") %>%
  mutate(across(all_of(c("po_cantidad",
                         "pa_cantidad",
                         "pob_prog",
                         "p_aprobado",
                         "p_ejercido",
                         "cobertura",
                         "eficiencia")
                       ),
                ~ as.numeric(ifelse(as.character(.) %in% c("NA", "ND", "-"), 
                                     NA, 
                                     as.character(.))),
                .names = "{.col}")) %>% 
  mutate(across(all_of(c("po_cantidad",
                         "pa_cantidad",
                         "p_aprobado",
                         "p_ejercido",
                         "cobertura",
                         "eficiencia")
                       ),
                ~ as.character(.),
                .names = "{.col}_txt")) %>% 
  mutate(modcve = paste0(mod, cve),
         ciclo  = factor(ciclo),
         derecho_directo = case_when(
           derecho_directo == "Bienestar económico" ~ "Bienestar Económico",
           derecho_directo == "No discriminación" ~ "No Discriminación",
           TRUE ~ derecho_directo),
         icon_ds = case_when(
           derecho_directo == "No Discriminación" ~ "person-half-dress",
           derecho_directo == "Bienestar Económico" ~ "piggy-bank",
           derecho_directo == "Educación" ~ "graduation-cap",
           derecho_directo == "Trabajo" ~ "briefcase",
           derecho_directo == "Alimentación" ~ "utensils",
           derecho_directo == "Salud" ~ "heart-circle-check",
           derecho_directo == "Vivienda" ~ "person-shelter",
           derecho_directo == "Medio Ambiente Sano" ~ "seedling",
           derecho_directo == "Seguridad Social" ~ "hands-holding-child",
           derecho_directo == "-" ~ "question")
         )

evaluaciones <- read_excel("inv_fed.xlsx", sheet = "evaluaciones") %>% 
  mutate(costo2 = as.character(costo2))


evolucion <- read_excel("inv_fed.xlsx", sheet = "evolucion") %>%
  arrange(id_if, ciclo) %>%
  group_by(id_if) %>%
  mutate(cambio_lag = lag(cambio),
         ciclo_lag = lag(ciclo),
         grupo = cumsum(cambio != cambio_lag | ciclo != ciclo_lag +1 | is.na(cambio_lag))) %>%
  group_by(id_if, cambio, grupo) %>%
  mutate(año_fin = if_else(n()>1, max(ciclo), NA_integer_),
         año_fin = coalesce(año_fin,ciclo)) %>%
  ungroup() %>%
  select(-cambio_lag, -ciclo_lag, -grupo)

deflactores <- read_excel("inv_fed.xlsx", sheet = "deflactores") %>% 
  mutate(ciclo = factor(ciclo),
         deflactor = as.numeric(deflactor))

# Tema ----
invfed_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",  
  primary = "#1a4146", 
  secondary = "#297272",
  base_font = font_google("DM Sans"),      
  heading_font = font_google("Outfit"),   
  font_scale = 1.05,
  bg = "#ffffff",          
  fg = "#2c3e50", 
  border_radius = "0.4rem"
  )

hc_tema <- hc_theme(
  chart = list(style = list(fontFamily = "Outfit, DM Sans, sans-serif")),
  title = list(align = "left",
               x = 0,
               y = 5,
               style = list(fontWeight = "bold",
                            fontSize = "18px")),
  subtitle = list(align = "left",
                  x = 0,
                  y = 25,
                  style = list(fontWeight = "normal",
                               fontSize = "16px")),
  colors = c("#1a4146",
             "#297272",
             "#e8e0c8",
             "#d3a446",
             "#b66d2f")
)


bloque_info <- function(titulo, 
                        outputId, 
                        color = "#297272", 
                        color_sep = "#BFBFBF", 
                        grosor = "4px") {
  separador <- function() {
    tags$div(style = paste0(
      "border-top: ", grosor, " solid ", color_sep, "; width: 100%; margin-top: 0; margin-bottom: 1rem;"
    ))
  }

  tags$div(
    titulo,
    br(),
    separador(),
    tags$h4(
      uiOutput(outputId),
      style = paste0("color: ", color, "; font-weight: bold;")
    )
  )
}

bloque_info3 <- function(icono, presupuesto, porcentaje, variacion) {
  
  pres_num <- as.numeric(presupuesto)
  pres_fmt <- paste0("$", formatC(pres_num, format = "f", big.mark = ",", digits = 2), " MDP")

  var_num <- readr::parse_number(variacion)
  color_var <- if (!is.na(var_num) && var_num < 0) "red" else "#297272"
  
  tags$div(
    style = "text-align: left;",
    
    tags$h4(
      icon(icono), 
      paste0(" ", pres_fmt),
      style = "color:#297272; font-weight:bold; margin:0;"
    ),
    
    tags$p(
      tags$strong("Porcentaje: "), 
      tags$span(porcentaje),
      style = "color:#444; font-size:0.9rem; margin:0;"
    ),
    
    tags$p(
      tags$strong("Variación: "),
      tags$span(variacion, style = paste0("color:", color_var, "; font-weight:bold;")),
      style = "font-size:0.9rem; margin:0;"
    )
  )
}