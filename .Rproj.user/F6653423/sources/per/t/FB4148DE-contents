# Insights
# 
#   Presupuesto total
#   Tipo de poblaciones
#   Tipos de apoyos
#   Por derecho
# 
# Gráficas
#   Poblaciones 
#   Cambios
#   


# Titulo ----
nombre_ramo <- function(anio_sel, ramo_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>% 
    pull(id_ramo) %>% 
    unique() %>% 
    sort()
}

# Datos generales----
  
num_pp <- function(anio_sel, ramo_sel){
  dato<- inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>%
    mutate(indice = row_number())
    
    return(max(dato$indice))
}

## Proporción presupuesto ----

pres_ramo <- function(anio_sel, ramo_sel) {

  anio_sel <- as.numeric(anio_sel)
  
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>% 
    pull(if (anio_sel == 2025) p_aprobado else p_ejercido) %>% 
    sum(na.rm = T)
}

proppres_ramo <- function(anio_sel, ramo_sel){

  anio_sel <- as.numeric(anio_sel)
  
  total <- inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(if (anio_sel == 2025) p_aprobado else p_ejercido) %>% 
    sum(na.rm = T)
  
  presupuesto <- inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>% 
    pull(if (anio_sel == 2025) p_aprobado else p_ejercido) %>% 
    sum(na.rm = TRUE)
  
  porcentaje <- percent(presupuesto/total, 
                        accuracy = 0.01)
  
  return(porcentaje)
}

varpres_ramo <- function(anio_sel, ramo_sel) {
  anio_sel <- as.numeric(anio_sel)
  cicloprevio <- anio_sel -1
  
  previo <- inv_fed %>% 
    filter(ciclo == cicloprevio, id_ramo == ramo_sel) %>% 
    pull(if (anio_sel == 2025) p_aprobado else p_ejercido) %>% 
    sum(na.rm = T)
  
  actual <- pres_ramo(anio_sel, ramo_sel)
  
  variacion <- (actual-previo)/previo
  
  percent(variacion, accuracy = 0.01)
}

  
# Tabla ----
datos_ramo <- function(anio_sel, ramo_sel) {
  df_datos <- inv_fed %>% 
    filter(ciclo == anio_sel,
           id_ramo == ramo_sel) %>% 
    select(nombre)
}

# Gráficas ----
hcpresupuesto_ramo <- function(anio_sel, ramo_sel) {

  num_ramo <- inv_fed %>%
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>%
    pull(no_ramo) %>%
    unique()
  
  df_filtrado <- inv_fed %>%
    filter(no_ramo == num_ramo) %>%
    select(ciclo, p_aprobado, p_ejercido) %>%
    left_join(deflactores, by = "ciclo") %>%
    mutate(p_aprobado = case_when(p_aprobado < 0 ~ NA,
                                  TRUE ~ p_aprobado),
           p_ejercido = case_when(p_ejercido < 0 ~ NA,
                                  TRUE ~ p_ejercido),
           aprobado_real = case_when(ciclo != "2025" ~ p_aprobado/deflactor,
                                     TRUE ~ p_aprobado),
           ejercido_real = p_ejercido/deflactor) %>% 
    group_by(ciclo) %>% 
    summarise(aprobado_total = if (all(is.na(aprobado_real))) NA_real_ else sum(aprobado_real, 
                                                                                na.rm = TRUE),
              ejercido_total = if (all(is.na(ejercido_real))) NA_real_ else sum(ejercido_real, 
                                                                                na.rm = TRUE),
              .groups = "drop") %>% 
    arrange(ciclo)
  
  idx <- which(as.character(df_filtrado$ciclo) == as.character(anio_sel))
  
  hc_pres <- highchart() %>% 
    
    hc_add_series(df_filtrado,
                  type = "column",
                  hcaes(x = ciclo,
                        y = aprobado_total),
                  name = "Aprobado",
                  color = "#bee7e8") %>% 
    
    hc_add_series(df_filtrado,
                  type = "spline",
                  hcaes(x = ciclo,
                        y = ejercido_total),
                  name = "Ejercido",
                  color = "#25566d") %>%
    hc_add_theme(hc_tema) %>%
    hc_title(text = "Presupuesto asignado y ejercido por el ramo",
             style = list(fontSize = "18px",
                          fontWeight = "bold")) %>%
    hc_subtitle(text = "Millones de pesos (MDP) constantes") %>%
    hc_legend(enabled = TRUE) %>% 
    hc_credits(enabled = TRUE,
               text = "Fuente: Elaboración propia con información de CONEVAL. Inventario Federal Histórico 2004 - 2023") %>%
    hc_xAxis(categories = sort(unique(df_filtrado$ciclo)),
             plotBands = list(
               list(
                 from = idx -1 -0.5,
                 to = idx -1 +0.5,
                 color = "#f0f0f0"
               )
             )) %>%
    hc_yAxis(title= list(text = ""),
             labels = list(format = "{value:,.0f}")
    ) %>% 
    hc_tooltip(valueDecimals = 2,
               shared = TRUE,
               headerFormat = "<b>{point.key}</b><br>",
               pointFormat = "<b>{series.name}:</b> {point.y} MDP<br>") %>% 
    hc_exporting(enabled = TRUE)
}

# hcproppres_ramo <- function(anio_sel, ramo_sel) {
#   tot_ejercido <- inv_fed %>% 
#     filter(ciclo == anio_sel) %>% 
#     pull(p_ejercido) %>% 
#     sum(na.rm = T)
#   
#   r_ejercido <- inv_fed %>% 
#     filter(ciclo == anio_sel, id_ramo == ramo_sel) %>% 
#     pull(p_ejercido) %>% 
#     sum(na.rm = TRUE)
#   
#   
#   prop_ramo <- r_ejercido/tot_ejercido
#   prop_resto <- 1 - prop_ramo
#   
#   df <- data.frame(
#     cat = c("Ramo", "Desarrollo social"),
#     porcentaje = c(prop_ramo, prop_resto),
#     grupo = c(1, 2)
#   )
#   
#   hchart(
#     df,
#     type = "column",
#     hcaes(x = porcentaje, y = cat, group = grupo),
#     stacking = "percent")
# }
  

  