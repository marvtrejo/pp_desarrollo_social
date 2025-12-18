# 16-E009
# 16-U040
# IF2021-002
# Investigacion noseque SEMARNAT
# Desarrollo Cultural SEP


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
  select(ciclo, cambio, comentario, año_fin) %>%
  mutate(x = as.Date(paste0(ciclo, "-01-01"))) %>% 
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