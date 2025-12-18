source("00_datos.R")
source("00_funcionesPP.R")
source("00_funcionesRamo.R")
source("01_componentesHome.R")
source("01_componentesPP.R")
source("01_componentesRamo.R")
source("01_componentesDatos.R")


ui <- page_navbar(
  id = "monitor_ds",
  fillable = FALSE,
  useShinyjs(),
  window_title = "Monitor Desarrollo Social",
  collapsible = TRUE,
  theme = invfed_theme,
  
  home_layout,
  pp_layout,
  ramos_layout,
  mpp_layout,
  datos_layout,
  
  fluidRow(
    column(12,
           tags$div(style = "font-size: 12px; padding-right:20px",
                    "Elaborado por Marvin Ivan Trejo Mendez"))
  )
)


server <- function(input, output, session) {
  
  # Home ----
  observeEvent(input$go_psociales,{
    updateNavbarPage(
      session = session,
      inputId = "monitor_ds",
      selected = "Programas sociales"
    )
  })
  
  observeEvent(input$go_mpp,{
    updateNavbarPage(
      session = session,
      inputId = "monitor_ds",
      selected = "Pobreza"
    )
  })
  
  observeEvent(input$go_datos,{
    updateNavbarPage(
      session = session,
      inputId = "monitor_ds",
      selected = "Datos"
    )
  })
  
  # Conoce ----
  ## Programas ----
  ### Controles ----
  output$selCiclo <- renderUI({
    selectizeInput(inputId = "selCiclo",
                label = HTML('<h5 class="text-center"><strong>Año:</strong></h5>'),
                choices = unique(inv_fed$ciclo),
                selected = "",
                multiple = FALSE)
  })
  
  output$selRamo <- renderUI({
    req(input$selCiclo)
    selectizeInput(inputId = "selRamo",
                   label = HTML('<h5 class="text-center"><strong>Ramo:</strong></h5>'),
                   choices = ent_disp(input$selCiclo),
                   selected = "",
                   multiple = FALSE,
                   options = list(placeholder = "SELECT")
    )
  })
  
  output$selPP <- renderUI({
    req(input$selCiclo, input$selRamo)
    selectInput(inputId = "selPP",
                label = HTML('<h5 class="text-center"><strong>Programa:</strong></h5>'),
                choices = pp_disp(input$selCiclo,
                                  input$selRamo)
    )
  })
  
  ### Título ----
  output$titulopp_srv <- renderUI({
    req(input$selCiclo, input$selRamo, input$selPP)
    
    tagList(
      h1(
        tags$strong(nombre(input$selCiclo, input$selRamo, input$selPP)),
        style = "font-weight: bold; margin-top: 0.0rem; margin-bottom: 0.0rem;"
      ),
      
     tags$span(
        paste("Ramo", ramo(input$selCiclo, input$selRamo, input$selPP)),
        style = "color: #7F7F7F; font-size: 1.5rem; margin-top: 0.0rem; margin-bottom: 0.0rem;"
      )
    )
  })

  ### Tarjetas ----

  output$ainicio_srv <- renderUI ({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    tags$span(
      icon("calendar-days"),
      ainicio(input$selCiclo, input$selRamo, input$selPP)
    )
  })

  output$derecho_srv <- renderUI ({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    tags$span(
      fa(icon_derecho(input$selCiclo, input$selRamo,input$selPP)),
      derecho(input$selCiclo, input$selRamo,input$selPP)
    )
  })
  
  output$apoyos_srv <- renderUI ({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    tags$span(
      icon("dolly"),
      apoyos(input$selCiclo, input$selRamo,input$selPP)
    )
  })
  
  output$rankpres1_srv <- renderUI ({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    tags$span(
      icon("box-open"),
      porcentaje_pres(input$selCiclo, input$selRamo,input$selPP)
    )
  })
  
  output$rankpres2_srv <- renderUI ({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    no_pp <- no_pp(input$selCiclo, input$selRamo, input$selPP)
    rank_pp <- rank_pp(input$selCiclo, input$selRamo, input$selPP)
    
    tags$span(
      icon("boxes-stacked"),
      paste0(rank_pp, " de ", no_pp)
    )
  })
  
  output$noeval_srv<-renderUI({
    req(input$selCiclo, input$selRamo,input$selPP)
    
    tags$span(
      icon("rectangle-list"),
      no_eval(input$selCiclo, input$selRamo,input$selPP)
    )
  })
  
  ### Datos generales ----
  output$descr_srv<-renderText({
    req(input$selCiclo, input$selRamo,input$selPP)
    descripcion(input$selCiclo, input$selRamo,input$selPP)
  })
  
  output$mir_srv<-renderTable({
    req(input$selCiclo, input$selRamo,input$selPP)
    mir(input$selCiclo, input$selRamo,input$selPP)
  })
  
  ### Gráficas ----
  output$hcpres_srv <- renderHighchart ({
    req(input$selCiclo, input$selRamo, input$selPP)
    hc_presupuesto(inv_fed, input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$hcpob_srv <- renderHighchart ({
    req(input$selCiclo, input$selRamo, input$selPP)
    hc_poblaciones(inv_fed, input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$hcevprog_srv <- renderHighchart({
    req(input$selCiclo, input$selRamo, input$selPP)
    hc_evprog(input$selCiclo, input$selRamo, input$selPP)
  })
  
  output$hc_eval_srv <- renderHighchart ({
    req(input$selCiclo, input$selRamo, input$selPP)
    hc_eval(input$selCiclo, input$selRamo, input$selPP)
  })
  
  ## Ramos ----
  ### Controles ----
  
  output$r_ciclo <- renderUI({
    selectInput(inputId = "r_ciclo",
                label = HTML('<h5 class="text-center"><strong>Año:</strong></h5>'),
                choices = unique(inv_fed$ciclo),
                selected = "",
                multiple = F
    )
  })
  
  output$r_ramo <- renderUI({
    req(input$r_ciclo)
    selectInput(inputId = "r_ramo",
                label = HTML('<h5 class="text-center"><strong>Ramo:</strong></h5>'),
                choices = ent_disp(input$r_ciclo),
                selected = "",
                multiple = F
    )
  })
  
  ### Titulo ----
  output$titulo_r <- renderUI({
    req(input$r_ciclo, input$r_ramo)
    
    tagList(
      h1(
        tags$strong(nombre_ramo(input$r_ciclo, input$r_ramo)),
        style = "font-weight: bold; margin-top: 0.0rem; margin-bottom: 0.0rem;"
      )
    )
  })
  
  ### Tarjetas ----
  
  output$numpp_r <- renderUI ({
    req(input$r_ciclo, input$r_ramo)
    
    tags$span(
      icon("dolly"),
      num_pp(input$r_ciclo, input$r_ramo)
    )
  })
  
  output$presupuesto_r <- renderUI({
    req(input$r_ciclo, input$r_ramo)
    
    presupuesto <- pres_ramo(input$r_ciclo, input$r_ramo)
    porcentaje  <- proppres_ramo(input$r_ciclo, input$r_ramo)
    variacion   <- varpres_ramo(input$r_ciclo, input$r_ramo)
    
    bloque_info3(
      icono = "money-bill-wave",
      presupuesto = presupuesto,
      porcentaje = porcentaje,
      variacion = variacion
    )
  })

  
  ### Gráficas ----

  output$hcpresupuesto_r <- renderHighchart ({
    req(input$r_ciclo, input$r_ramo)
    hcpresupuesto_ramo(input$r_ciclo, input$r_ramo)
  })
  
  ### Tabla ----
  output$tabla_ramo <- renderDataTable ({
    req(input$r_ciclo, input$r_ramo)

    df <- datos_ramo(input$r_ciclo, input$r_ramo)

        datatable(df,options = list(pageLength = 10, scrollX = FALSE))
    
  })
  
  # Datos ----
  ## Controles ----
  
  output$selCiclo_datos <- renderUI({
    selectInput(inputId = "ciclo_datos",
                label = HTML('<h5 class="text-center"><strong>Año:</strong></h5>'),
                choices = unique(inv_fed$ciclo),
                selected = "",
                multiple = TRUE
                )
  })
  
  output$selRamo_datos <- renderUI({
    req(input$ciclo_datos)
    selectInput(inputId = "ramo_datos",
                   label = HTML('<h5 class="text-center"><strong>Ramo:</strong></h5>'),
                   choices = ent_disp(input$ciclo_datos),
                   selected = "",
                   multiple = TRUE
                )
  })
  
  output$selPP_datos <- renderUI({
    req(input$ciclo_datos, input$ramo_datos)
    selectInput(inputId = "pp_datos",
                label = HTML('<h5 class="text-center"><strong>Programa:</strong></h5>'),
                choices = pp_disp(input$ciclo_datos,
                                  input$ramo_datos),
                selected = "",
                multiple = TRUE
                )
  })
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste0("datos_{paste(input$ciclo_datos, collapse = '-')}_{input$ramo_datos}.csv")
    },
    content = function(file) {
      df <- datos(input$ciclo_datos, input$ramo_datos, input$pp_datos, input$nivel)
      write.csv(df$x, file, row.names = FALSE)
    }
  )
  
  ## Tabla ----
  output$tabla_datos <- renderDataTable ({
    req(input$ciclo_datos, input$ramo_datos, input$nivel)
    
    if (input$nivel == "Programa") {
      req(input$pp_datos)
    }
    
    df <- datos(input$ciclo_datos,
                input$ramo_datos,
                input$pp_datos,
                input$nivel)
    
    validate(
      need(nrow(df) > 0, "No hay datos que coincidan con los filtros seleccionados.")
    )
    
    datatable(df,options = list(pageLength = 10, scrollX = FALSE))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
