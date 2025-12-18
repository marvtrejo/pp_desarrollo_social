# Funciones ----

## Controles ----
anio_sel = 2023

ent_disp <- function(anio_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(id_ramo) %>% 
    unique() %>% 
    sort()
}

pp_disp <- function(anio_sel, ramo_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

## Títulos ----
ramo <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(id_ramo) %>% 
    unique() %>% 
    sort()
}

modcve <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(modcve) %>% 
    unique() %>% 
    sort()
}

nombre <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(nombre) %>% 
    unique() %>% 
    sort()
}

## Año de inicio ----
ainicio <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(inicio) %>% 
    unique() %>% 
    sort()
}

## Derecho social ----
derecho <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(derecho_directo) %>% 
    unique() %>% 
    sort()
}

icon_derecho <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(icon_ds) %>% 
    unique() %>% 
    sort()
}

## Apoyos que entrega ----
apoyos <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(apoyos) %>% 
    unique() %>% 
    sort()
}

## Número de evaluaciones ----
no_eval <- function(anio_sel, ramo_sel, pp_sel){
  id <- inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(id_if) %>% 
    unique() %>% 
    sort()
  
  if (length(id) > 0) {
    id <- id[1]
  } else {
    return("Información no disponible")
  }
  
  resultado <- evaluaciones %>% 
    filter(id_if == id) %>%
    select (ciclo, tipo) %>% 
    arrange (ciclo) 
  
  resultado <- nrow(resultado)
  
  return(resultado)
}

## Descripción ----

descripcion <- function(anio_sel, ramo_sel, pp_sel){
  inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(descripcion) %>% 
    unique() %>% 
    sort()
}

## MIR ----

mir <- function(anio_sel, ramo_sel, pp_sel) {
  df_filtrado <- inv_fed %>% 
    filter(ciclo == anio_sel,
           id_ramo == ramo_sel,
           nombre == pp_sel)
  
  fin <- unique(df_filtrado$fin)
  proposito <- unique(df_filtrado$proposito)
  componentes <- unique(df_filtrado$componente)
  actividades <- unique(df_filtrado$actividad)
  
  if(length(fin) == 0) fin <- "Información no disponible"
  if(length(proposito) == 0) proposito <- "Información no disponible"
  if(length(componentes) == 0) componentes <- "Información no disponible"
  if(length(actividades) == 0) actividades <- "Información no disponible"
  
  tabla_mir <- data.frame(
    Nivel = c("Fin", "Propósito", "Componentes", "Actividades"),
    Objetivo = c(fin, proposito, componentes, actividades),
    stringsAsFactors = FALSE
  )
  
  return(tabla_mir)
}

## Evaluaciones ----
hc_eval <- function(anio_sel, ramo_sel, pp_sel){
  id <- inv_fed %>%
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique() %>%
    sort()
  
  if (length(id) == 0) {
    stop("Información no disponible")
  }
  
  id <- id[1]
  
  resultado <- evaluaciones %>%
    filter(id_if == id) %>%
    arrange(tipo, ciclo)
  
  resultado <- resultado %>%
    mutate(tipo = factor(tipo, levels = unique(tipo)))
  
  resultado <- resultado %>% 
    mutate(tipo_num = as.numeric(tipo) -1) %>% 
    rename(Ciclo = ciclo,
           Título = titulo,
           Costo = costo2)
  
  x <- c("Ciclo", "Título", "Costo")
  y <- str_c("{point.", x, "}")
  tt <- tooltip_table(x, y)
  
  if(nrow(resultado) == 0) {
    return("Información no disponible")
  }
  
  
  
  hchart(resultado,
         "scatter",
         hcaes(x = Ciclo,
               y = tipo_num,
               group = tipo)) %>%
    hc_chart(type = "scatter") %>%
    hc_plotOptions(
      scatter = list(
        marker = list(radius = 14,
                      symbol = "point",
                      lineColor = "black",
                      lineWidth = 2
        ),
        dataLabels = list(enabled = FALSE),
        states = list(inactive = list(opacity = 1))
      )
    ) %>%
    hc_add_theme(hc_tema) %>%
    hc_title(text = "Evaluaciones realizadas al programa") %>%
    hc_xAxis(title = "",
             allowDecimals = FALSE,
             gridLineWidth = 0
    ) %>%
    hc_yAxis(title = "",
             categories = levels(resultado$tipo),
             labels = list(enabled = FALSE),
             # labels = list(style = list(fontWeight = "bold")),
             opposite = FALSE,
             gridLineWidth = 0
    ) %>%
    hc_legend(enabled = TRUE) %>%
    hc_credits(enabled = TRUE,
               text = "Fuente: Elaboración propia con información de CONEVAL. Inventario Federal Histórico 2004 - 2023") %>%
    hc_tooltip(useHTML = TRUE,
               pointFormat = tt,
               crosshairs = FALSE) %>%
    hc_exporting(enabled = TRUE,
                 buttons = list(
                   contextButton = list(
                     menuItems = c("viewFullscreen",
                                   "printChart",
                                   "separator",
                                   "Formato de descarga",
                                   "downloadPNG",
                                   "downloadJPEG",
                                   "downloadPDF",
                                   "downloadSVG")
                     )
                   )
                 ) %>%
    hc_chart(lang = list(
      viewFullscreen = "Pantalla completa",
      printChart = "Imprimir gráfico",
      downloadPNG = "Descargar PNG",
      downloadJPEG = "Descargar JPEG",
      downloadSVG = "Descargar SVG"))
}

## Presupuesto ----

hc_presupuesto <- function(inv_fed, anio_sel, ramo_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, p_aprobado, p_ejercido) %>% 
    left_join(deflactores, by = "ciclo") %>%
    mutate(p_aprobado = case_when(p_aprobado < 0 ~ NA,
                                  TRUE ~ p_aprobado),
           p_ejercido = case_when(p_ejercido < 0 ~ NA,
                                  TRUE ~ p_ejercido),
           aprobado_real = case_when(ciclo != "2025" ~ p_aprobado/deflactor,
                                     TRUE ~ p_aprobado),
           ejercido_real = p_ejercido/deflactor) %>% 
    arrange(ciclo)

  idx <- which(as.character(df_filtrado$ciclo) == as.character(anio_sel))

  hc_pres <- highchart() %>% 

    hc_add_series(df_filtrado,
                  type = "column",
                  hcaes(x = ciclo,
                        y = aprobado_real),
                  name = "Aprobado",
                  color = "#bee7e8") %>% 
    
    hc_add_series(df_filtrado,
                  type = "spline",
                  hcaes(x = ciclo,
                        y = ejercido_real),
                  name = "Ejercido",
                  color = "#25566d") %>%
    hc_add_theme(hc_tema) %>%
    hc_title(text = "Presupuesto asignado y ejercido por el programa",
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

## Poblaciones ----
hc_poblaciones <- function(inv_fed, anio_sel, ramo_sel, pp_sel) {
  id <- inv_fed %>%
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique()
  
  if(length(id) == 0) {
    return(NULL)
  }
  
  df_filtrado <- inv_fed %>%
    filter(id_if %in% id) %>%
    select(ciclo, po_unidad, po_cantidad, pa_unidad, pa_cantidad, pob_prog) %>% 
    filter(!(is.na(po_cantidad) & is.na(pa_cantidad))) %>% 
    arrange(ciclo)
  
  idx <- which(as.character(df_filtrado$ciclo) == as.character(anio_sel))

  
  if(nrow(df_filtrado) == 0) {
    return(NULL)
  }
  
  hc_pob <- highchart() %>% 
     hc_add_series(df_filtrado,
                   type = "column",
                   hcaes(x = ciclo, y = po_cantidad),
                   name = "Objetivo",
                   color = "#F2A490",
                   tooltip = list(
                     pointFormat = "<b>{series.name}:</b> {point.y} {point.po_unidad} <br>")
     ) %>%

     hc_add_series(df_filtrado,
                   type = "spline",
                   hcaes(x = ciclo, y = pa_cantidad),
                   name = "Atendida",
                   color = "#d23642",
                   tooltip = list(
                     pointFormat = "<b>{series.name}:</b> {point.y} {point.pa_unidad}"),
                   states = list(inactive = list(opacity = 1))
     ) %>%
    
    hc_add_series(df_filtrado,
                  type = "spline",
                  hcaes(x = ciclo, y = pob_prog),
                  name = "Programada",
                  color = "#F7C04A",
                  tooltip = list(
                    pointFormat = "<b>{series.name}:</b> {point.y} {point.pob_prog}"),
                  states = list(inactive = list(opacity = 1))
    ) %>%
    
    hc_add_theme(hc_tema) %>% 
    hc_title(text = "Población objetivo y atendida del programa") %>%
    hc_credits(enabled = TRUE,
               text = "Fuente: Elaboración propia con información de CONEVAL. Inventario Federal Histórico 2004 - 2023") %>%
    hc_xAxis(categories = sort(df_filtrado$ciclo),
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
    
    hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      formatter = JS("
    function () {
      let s = '<b>' + this.x + '</b><br>';
      this.points.forEach(function(point) {
        let unidad = point.point.po_unidad || point.point.pa_unidad || '';
          s += '<span style=\"color:' + point.color + '\">' + point.series.name + ':</span> ' +
               Highcharts.numberFormat(point.y, 0) + ' ' + unidad + '<br>';
      });
      return s;
    }
  ")
    ) %>% 
    hc_exporting(enabled = TRUE)
}

## Evolución ----

hc_evprog <- function(anio_sel, ramo_sel, pp_sel){
  
  id <- evolucion %>%
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>%
    pull(id_if) %>%
    unique() %>%
    sort()
  
  if (length(id) == 0) {
    return(NULL)
  }
  
  id <- id[1]
  
  evol_tl<-evolucion %>%
    filter(id_if == id) %>%
    filter(cambio != "Re-sectorización (entra)") %>% 
    select(ciclo, cambio, comentario, año_fin) %>%
    mutate(x = as.Date(paste0(ciclo, "-01-01")),
           cambio = case_when(cambio == "Re-sectorización (sale)" ~ "Re-sectorizado",
                              TRUE ~ cambio)) %>% 
    rename(name = cambio,
           label = ciclo,
           description = comentario)
  
  eventos <- evol_tl %>%
    mutate(evento = case_when(label == año_fin & name != "Sin cambio" ~ 1,
                              label == max(label) & name == "Sin cambio" ~ 1,
                              TRUE ~ 0)) %>% 
    filter(evento == 1) 
  
  id_sc <- evol_tl %>% 
    filter(label <= año_fin & name == "Sin cambio") %>% 
    select(año_fin) %>% 
    unique() %>% 
    mutate(id = row_number())
  
  periodos_sin_cambio <- evol_tl %>%
    filter(label <= año_fin & name == "Sin cambio") %>%
    left_join(id_sc) %>% 
    group_by(id) %>% 
    slice_min(order_by = label,
              with_ties = FALSE) %>% 
    ungroup() %>% 
    transmute(
      from = as.Date(paste0(label, "-03-01")),
      to = as.Date(paste0(año_fin, "-10-31")),
      label = ifelse(label == año_fin,
                     paste(año_fin, "<br>", "Sin cambio"),
                     paste(label, "-", año_fin, "<br>", "Sin cambio"))
      )
  
  
  plotbands <- purrr::pmap(periodos_sin_cambio, function(from, to, label) {
    list(
      from = datetime_to_timestamp(from),
      to = datetime_to_timestamp(to),
      color = "#f5f5f5",
      label = list(
        text = label,
        align = "center",
        verticalAlign = "top",
        style = list(color = "#777", fontSize = "11px", fontWeight = "normal")
      )
    )
  })
  
  
  hchart(eventos,
         type = "timeline",
         hcaes(x = x, name = name, label = label, description = description)) %>%
    hc_add_theme(hc_tema) %>%
    hc_title(text = "Evolución del programa",
             style = list(fontSize = "18px",
                          fontWeight = "bold")) %>%
    hc_subtitle(text = "Cambios con base en la programación presupuestaria",
                style = list(fontSize = "16px")) %>%
    hc_xAxis(
      type = "datetime",
      visible = TRUE,
      title = FALSE,
      plotBands = plotbands
    ) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_credits(enabled = TRUE,
               text = "Fuente: CONEVAL. Evolución de los programas sociales 2008–2023") %>% 

    hc_exporting(enabled = TRUE,
                 buttons = list(
                   contextButton = list(                   
                     menuItems = c("viewFullscreen",
                                   "printChart",
                                   "separator",
                                   "downloadPNG",
                                   "downloadJPEG",
                                   "downloadPDF",
                                   "downloadSVG")
                   )
                 )
    )
}

## Ranking del presupuesto ----

porcentaje_pres <- function(anio_sel, ramo_sel, pp_sel){
  total_ejercido <- inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    pull(p_ejercido) %>% 
    sum(na.rm = T)
  
  pp_ejercido <- inv_fed %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(p_ejercido) %>% 
    sum(na.rm = TRUE)
  
  if(total_ejercido == 0 || is.na(pp_ejercido)) {
    return("Información no disponible")
  }
  
  porcentaje <- percent(pp_ejercido/total_ejercido, 
                        accuracy = 0.001)
  
  return(porcentaje)
}


rank_pp <- function(anio_sel,ramo_sel, pp_sel){
  pres_orden <- inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    arrange(desc(p_ejercido)) %>% 
    mutate(indice = row_number()) 
  
  numero <- pres_orden %>% 
    filter(ciclo == anio_sel, id_ramo == ramo_sel, nombre == pp_sel) %>% 
    pull(indice)
  
  return(numero)

}

no_pp <- function(anio_sel, ramo_sel, pp_sel){
  pres_orden <- inv_fed %>% 
    filter(ciclo == anio_sel) %>% 
    arrange(desc(p_ejercido)) %>% 
    mutate(indice = row_number())
  
  return(max(pres_orden$indice))
}