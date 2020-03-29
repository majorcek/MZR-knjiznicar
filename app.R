library(shiny)
library(shinydashboard)
library(purrr)

source("funkcija.R")
source("analiza.R")

ui <- dashboardPage(
  dashboardHeader(title = "Projekt knjižničar"),
  dashboardSidebar(
    sidebarMenu(
      numericInput(inputId = "parameter_prihodi", label = "intenziteta prihodov", value = 0.025, min = 0.001, max = 1, step = 0.001),
      fluidRow(
        column(12, 
               div(style = "display: inline-block; margin-left: 20px; margin-right: 0px;",
                   checkboxInput(inputId = "klici_ali", label = "", value = TRUE, width = "100%")),
               div(style = "display: inline-block;",
                   conditionalPanel(
                     condition = "input.klici_ali == true",
                     numericInput(inputId = "parameter_klici", label = "intenziteta klicov",
                                  value = 0.01, min = 0.001, max = 1, step = 0.001, width = "100%"))))
      ),
      sliderInput(inputId = "cas_obratovanja", label = "Čas obratovanja", 
                  value = as.numeric(20000), min = 3600, max = 43200),
      sliderInput(inputId = "max_knjig", label = "kritično št. knjig", 
                  value = 10, min = 1, max = 30),
      sliderInput(inputId = "max_izposojenih", label = "najvecje stevilo knjig izposoje",
                  value = 8, min = 1, max = 8),
      hr(),
      actionButton(inputId = "posodobi", label = "uporabi nove vrednosti"),
      uiOutput("izbor_knjig")
      
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("ŠTEVILO STRANK V KNJIŽNICI", plotOutput("graf_stranke_vedno")),
      tabPanel("ČAS BREZDELJA", plotOutput("graf_brezdelje")),
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
                               "Dolžina najdaljšega opravka v knjižnici: ", verbatimTextOutput("max_cas_v_knjiznici")))
               
      )
    )
  )
)


server <- function(input,output,session){
  col_names <- reactive(paste0("knjige_input", seq_len(input$max_izposojenih)))
  output$izbor_knjig <- renderUI({
    map(col_names(), ~ sliderInput(.x , .x, min = 0, max = 1, value = 0, step = 0.01))
  })
  output$imena <- renderPrint({print(length(col_names()))})
  
  utezi <- reactive({
    c(input[["knjige_input1"]], input[["knjige_input2"]], input[["knjige_input3"]], input[["knjige_input4"]],
      input[["knjige_input5"]], input[["knjige_input6"]], input[["knjige_input7"]], input[["knjige_input8"]])[1:input[["max_izposojenih"]]]
  })
  
  vsota_utezi <- reactive({sum(utezi())})
  
  observeEvent(input$max_izposojenih,{
    updateSliderInput(session, inputId = "knjige_input1", value = 1)
    updateSliderInput(session, inputId = "knjige_input2", value = 0)
    updateSliderInput(session, inputId = "knjige_input3", value = 0)
    updateSliderInput(session, inputId = "knjige_input4", value = 0)
    updateSliderInput(session, inputId = "knjige_input5", value = 0)
    updateSliderInput(session, inputId = "knjige_input6", value = 0)
    updateSliderInput(session, inputId = "knjige_input7", value = 0)
    updateSliderInput(session, inputId = "knjige_input8", value = 0)
  })
  
  vrednost1 <- reactive({input$knjige_input1})
  vrednost2 <- reactive({input$knjige_input2})
  vrednost3 <- reactive({input$knjige_input3})
  vrednost4 <- reactive({input$knjige_input4})
  vrednost5 <- reactive({input$knjige_input5})
  vrednost6 <- reactive({input$knjige_input6})
  vrednost7 <- reactive({input$knjige_input7})
  vrednost8 <- reactive({input$knjige_input8})
  
  observeEvent(input$posodobi,{
    # 1. posodobimo verjetnost
    updateSliderInput(session, inputId = "knjige_input1", value = vrednost1() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input2", value = vrednost2() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input3", value = vrednost3() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input4", value = vrednost4() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input5", value = vrednost5() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input6", value = vrednost6() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input7", value = vrednost7() / vsota_utezi())
    updateSliderInput(session, inputId = "knjige_input8", value = vrednost8() / vsota_utezi())
  })
  
  
  tabela <- eventReactive(c(input$parameter_prihodi, 
                            input$parameter_klici, 
                            input$posodobi,
                            input$cas_obratovanja, 
                            input$max_knjig,
                            input$klici_ali,
                            input$max_izposojenih),
                          
                          {ustvari_skupno_tabelo(input$parameter_prihodi, 
                                                 input$parameter_klici, 
                                                 utezi(), 
                                                 utezi(), 
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
  
  
  podatki <- reactive({pridobi_podatke(tabela())})
  
  output$skupno_strank <- renderPrint({as.integer(podatki()[1])})
  output$zaklepanje <- renderPrint({podatki()[[2]]})
  output$skupno_knjige <- renderPrint({podatki()[[3]]})
  output$skupno_cakanje <- renderPrint({podatki()[[4]]})
  output$max_cakanje_na_strezbo <- renderPrint({podatki()[[5]]})
  output$max_cas_v_knjiznici <- renderPrint({podatki()[[6]]})

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
}

shinyApp(ui = ui, server = server)