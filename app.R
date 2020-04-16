library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(purrr)
library(ggplot2)
library(shinycssloaders)

source("funkcija.R")
source("analiza.R")


mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"









ui <- dashboardPagePlus(
  header <- dashboardHeaderPlus(titleWidth = 280, 
                                title = "Projekt knjižničar", 
                                enable_rightsidebar = FALSE),
  sidebar <- dashboardSidebar(width = 280,
                              sidebarMenu(
                                hr(),
                                
                                menuItem("PRIHODI", icon = icon("sort", class = NULL, lib = "font-awesome"),
                                         numericInput(inputId = "parameter_prihodi", label = "intenziteta prihodov", value = 0.025, min = 0.001, max = 1, step = 0.001),
                                         checkboxInput(inputId = "klici_ali", label = "klici dovoljeni", value = TRUE, width = 60),
                                         conditionalPanel(
                                           condition = "input.klici_ali == true",
                                           numericInput(inputId = "parameter_klici", label = "intenziteta klicov",
                                                        value = 0.01, min = 0.001, max = 1, step = 0.001, width = 180)
                                         )
                                ),
                                
                                menuItem("DELOVNI ČAS", icon = icon("clock", lib = "font-awesome"), 
                                         sliderInput(inputId = "cas_obratovanja", label = "Čas obratovanja", 
                                                     value = as.numeric(20000), min = 3600, max = 43200)
                                ),
                                
                                menuItem("OMEJITEV KNJIŽNIČARJA", icon = icon("book", class = NULL, lib = "font-awesome"), 
                                         sliderInput(inputId = "max_knjig", label = "kritično št. knjig", 
                                                     value = 10, min = 1, max = 30)
                                ),
                                
                                menuItem("OMEJITEV IZPOSOJE", icon = icon("id-card", class = NULL, lib = "font-awesome"), 
                                         sliderInput(inputId = "max_izposojenih", label = "najvecje stevilo knjig izposoje",
                                                     value = 8, min = 1, max = 8)
                                ),
                                
                                hr(),
                                
                                actionButton(inputId = "posodobi", label = "",
                                             icon = icon("sync-alt", class = NULL, lib = "font-awesome")),
                                
                                hr(),
                                
                                menuItem("UTEŽI", icon = icon("list-ol", class = NULL, lib = "font-awesome"), 
                                         uiOutput("prvi_drsnik"),
                                         uiOutput("ostali_drsniki")
                                )
                                
                              )
  ),
  
  body <- dashboardBody(
    tags$head(tags$style(".sidebar-menu li { margin-bottom: -2px; }")),
    tags$style(type = "text/css", "
      .irs-slider {width: 30px; height: 20px; top: 20px;}
    "),
    tabsetPanel(type = "pills",
                tabPanel("STANJE V KNJIŽNICI", 
                         withSpinner(plotOutput("graf_stranke_vedno"), type = 2, color = "grey", color.background = 'lightblue'),
                         withSpinner(plotOutput("graf_brezdelje"), type = 2, color = "grey", color.background = 'lightblue')),
                tabPanel("PODATKI", 
                         fluidRow(column(7,
                                         plotOutput("graf_vrste_strank")),
                                  column(5,
                                         wellPanel("Skupno število strank: ", verbatimTextOutput("skupno_strank")),
                                         wellPanel("Skupno število prinešenih knjig: ", verbatimTextOutput("skupno_knjige")),
                                         wellPanel("Čas, ko knjižničar zaključi z delom: ", verbatimTextOutput("zaklepanje")))),
                         
                         hr(),
                         fluidRow(column(2,
                                         wellPanel("Skupni čas čakanja:" , verbatimTextOutput("skupno_cakanje"))),
                                  
                                  column(5,
                                         plotOutput("graf_cakanje"),
                                         "Dolzina najdaljšega čakanje na strežbo: ", verbatimTextOutput("max_cakanje_na_strezbo")),
                                  column(5,
                                         plotOutput("graf_v_knjiznici"),
                                         "Dolžina najdaljšega opravka v knjižnici: ", verbatimTextOutput("max_cas_v_knjiznici"))
                         )
                ),
                tabPanel("PRIMERJAVA",
                         fluidRow(column(6, 
                                         wellPanel(plotOutput("graf_knjige_izposojene"),
                                                   verbatimTextOutput("delezi_izposojene"))),
                                  
                                  column(6,
                                         wellPanel(plotOutput("graf_knjige_vrnjene"),
                                                   verbatimTextOutput("delezi_vrnjene")))
                         ),
                         fluidRow(verbatimTextOutput("utezi_in"))
                )
    )
  ),
  rightsidebar = rightSidebar()
)









server <- function(input, output, session){
  
  output$prvi_drsnik <- renderUI(sliderInput(inputId = "knjige_input1", label = "knjige_input1", value = 1, min = 0, max = 1, step = 0.01))
  col_names <- reactive(paste0("knjige_input", seq_len(input$max_izposojenih - 1) + 1))
  output$ostali_drsniki <- renderUI({
    map(col_names(), ~ sliderInput(.x , .x, value = 0, min = 0, max = 1, step = 0.01))
  })
  
  tabela <- eventReactive(c(input$parameter_prihodi, 
                            input$parameter_klici, 
                            input$posodobi,
                            input$cas_obratovanja, 
                            input$max_knjig,
                            input$klici_ali,
                            input$max_izposojenih),
                          
                          {
                            vsota_utezi <- sum(c(input$knjige_input1, input$knjige_input2, input$knjige_input3, input$knjige_input4, input$knjige_input5, input$knjige_input6, input$knjige_input7, input$knjige_input8)[1:input$max_izposojenih])
                            
                            updateSliderInput(session, inputId = "knjige_input1", value = input$knjige_input1 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input2", value = input$knjige_input2 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input3", value = input$knjige_input3 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input4", value = input$knjige_input4 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input5", value = input$knjige_input5 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input6", value = input$knjige_input6 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input7", value = input$knjige_input7 / vsota_utezi)
                            updateSliderInput(session, inputId = "knjige_input8", value = input$knjige_input8 / vsota_utezi)                          
                            
                            ustvari_skupno_tabelo(input$parameter_prihodi, 
                                                  input$parameter_klici, 
                                                  c(input$knjige_input1, input$knjige_input2, input$knjige_input3, input$knjige_input4, input$knjige_input5, input$knjige_input6, input$knjige_input7, input$knjige_input8)[1:input$max_izposojenih], 
                                                  c(input$knjige_input1, input$knjige_input2, input$knjige_input3, input$knjige_input4, input$knjige_input5, input$knjige_input6, input$knjige_input7, input$knjige_input8)[1:input$max_izposojenih], 
                                                  input$cas_obratovanja, 
                                                  input$max_knjig,
                                                  input$klici_ali,
                                                  input$max_izposojenih)
                          })
  
  output$graf_stranke_vedno <- renderPlot({
    tabela_stranke_prihodi <- naredi_tabelo_stanja_strank(tabela())
    ggplot(data = tabela_stranke_prihodi) +
      geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
      scale_x_continuous(breaks=seq(0, tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1), ((tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1))%/%1000) * 100))
  })
  
  output$graf_brezdelje <- renderPlot({
    tabela_brezdelje <- naredi_tabelo_brez_dela(tabela(), tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1))
    ggplot(data = tabela_brezdelje) + 
      geom_point(mapping = aes(x = cas, y = cas_brez_dela)) +
      geom_line(mapping = aes(x = cas, y = cas_brez_dela)) +
      scale_x_continuous(breaks=seq(0, tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1), ((tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1))%/%1000) * 100))
  })
  
  ######## ZAVIHEK PODATKI
  
  podatki <- reactive({pridobi_podatke(tabela())})
  
  output$skupno_strank <- renderText({as.integer(podatki()[1])})
  output$zaklepanje <- renderText({podatki()[[2]]})
  output$skupno_knjige <- renderText({podatki()[[3]]})
  output$skupno_cakanje <- renderText({podatki()[[4]]})
  output$max_cakanje_na_strezbo <- renderText({podatki()[[5]]})
  output$max_cas_v_knjiznici <- renderText({podatki()[[6]]})
  
  output$graf_vrste_strank <- renderPlot({
    tabela_stranke_opravila <- tabela() %>% group_by(vrsta_opravila) %>% count()
    ggplot(data = tabela_stranke_opravila) +
      geom_bar(width = 1, aes(x = "", y = n, fill=vrsta_opravila), stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=list("#999999", "#E69F00", "#56B4E9", "#6C8029"))
  })
  output$graf_v_knjiznici <- renderPlot({
    tabela_v_knjiznici <- data.frame(tabela()$vrstni_red, tabela()$cas_odhoda - tabela()$cas_prihoda)
    names(tabela_v_knjiznici) <- c("vrstni red", "cas v knjiznici")
    ggplot(data = tabela_v_knjiznici) +
      geom_col(aes(x = `vrstni red`, y = `cas v knjiznici`))
  })
  
  output$graf_cakanje <- renderPlot({
    tabela_cakanje <- data.frame(tabela()$vrstni_red, tabela()$cas_zacetka_strezbe - tabela()$cas_prihoda)
    names(tabela_cakanje) <- c("vrstni red", "cas cakanja")
    ggplot(data = tabela_cakanje) +
      geom_col(aes(x = `vrstni red`, y = `cas cakanja`))
  })
  
  
  #################################### ZAVIHEK PRIMERJAVA
  
  
  output$graf_knjige_izposojene <- renderPlot({
    ggplot(data = izposoja_knjig(tabela())) +
      geom_col(aes(x = st_izposojenih_knjig, y = n))
  })
  
  output$graf_knjige_vrnjene <- renderPlot({
    ggplot(data = vracanje_knjig(tabela())) +
      geom_col(aes(x = st_prinesenih_knjig, y = n))
  })
  
  output$delezi_izposojene <- renderText({
    tabela_knjige_out <- izposoja_knjig(tabela())
    round(tabela_knjige_out$delez, digits = 2)
  })
  
  output$delezi_vrnjene <- renderText({
    tabela_knjige_in <- vracanje_knjig(tabela())
    round(tabela_knjige_in$delez, digits = 2)
  })
  
  output$utezi_in <- renderText({
    c(input$knjige_input1, input$knjige_input2, input$knjige_input3, input$knjige_input4,
      input$knjige_input5, input$knjige_input6, input$knjige_input7, input$knjige_input8)[1:input$max_izposojenih]
  })
}

shinyApp(ui = ui, server = server)