library(shiny)
library(shinydashboard)

source("funkcija.R")
source("analiza.R")

#source("vizualizacija.R")
utezi_knjige_in <- c(0.35, 0.23, 0.17, 0.10, 0.05, 0.04, 0.03, 0.03)
utezi_knjige_out <- c(0.35, 0.23, 0.17, 0.10, 0.05, 0.04, 0.03, 0.03)


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
      
      #numericInput(inputId = "verjetnost1", label = "P(vzame 1 knjigo)", value = 1, min = 0, max = 1, step = 0.01),
      uiOutput("comparison")
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("ŠTEVILO STRANK V KNJIŽNICI", plotOutput("graf_stranke_vedno")),
      tabPanel("ČAS BREZDELJA", plotOutput("graf_brezdelje")),
      tabPanel("PODATKI", 
               fluidRow(column(4,
                               wellPanel("Skupno število strank: ", verbatimTextOutput("skupno_strank"),
                                         "Stranke glede na vrsto opravila"))),
               hr(),
               fluidRow(column(4,
                               wellPanel("Čas, ko knjižničar zaključi z delom: ", verbatimTextOutput("zaklepanje")))),
               hr(),
               fluidRow(column(4,
                               wellPanel("Skupno število prinešenih knjig: ", verbatimTextOutput("skupno_knjige"))),
                        column(4,
                               wellPanel("Skupni čas čakanja:" , verbatimTextOutput("skupno_cakanje"),
                                         "Najdaljše čakanje: ", verbatimTextOutput("max_cakanje"))),
                        column(4,
                               wellPanel("Dolžina najdaljšega opravka v knjižnici: ", verbatimTextOutput("max_cas_knjiznica"))))
      )
    )
  )
)




server <- function(input,output){
  
  output$comparison <- renderUI({
    req(input$slider)
    
    mySliders <- lapply(1:input$slider, function(i) {
      sliderInput(inputId = glue("verjetnost{i}"), label = h3(glue("verjetnost{i}")), value = 0, min = 0, max = 0, step = 0.01) 
    })
    do.call(tabsetPanel, mySliders)
  })
  
  #output$verjetnost1 <- renderUI({numericInput(inputId = "verjetnost2", "Izberi P(k = 2)", value = 0, min = 0, max = 1 - input$verjetnost1)})
  
  tabela <- reactive({ustvari_skupno_tabelo(input$parameter_prihodi, 
                                            input$parameter_klici, 
                                            utezi_knjige_in, 
                                            utezi_knjige_out, 
                                            input$cas_obratovanja, 
                                            input$max_knjig,
                                            input$klici_ali,
                                            8)
  })
  
  output$graf_stranke_vedno <- renderPlot({
    tabela_stranke_prihodi <- naredi_tabelo_stanja_strank(tabela())
    ggplot(data = tabela_stranke_prihodi) +
      geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
      scale_x_continuous(breaks=seq(0, tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1), ((tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1))%/%1000) * 100))
  })
  
  output$graf_brezdelje <- renderPlot({
    tabela_brezdelje <- naredi_tabelo_brez_dela(tabela(), tail(tabela()$cas_odhoda,1)+ + tail(tabela()$cas_knjiznicarja,1))
    ggplot(data = tabela_brezdelje) + 
      geom_point(mapping = aes(x = cas, y = cas_brez_dela)) +
      geom_line(mapping = aes(x = cas, y = cas_brez_dela)) +
      scale_x_continuous(breaks=seq(0, tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1), ((tail(tabela()$cas_odhoda,1) + tail(tabela()$cas_knjiznicarja,1))%/%1000) * 100))
  })
  
  output$skupno_strank <- renderPrint({as.integer(podatki(tabela())[1])})
  output$zaklepanje <- renderPrint({podatki(tabela())[[2]]})
  output$skupno_knjige <- renderPrint({podatki(tabela())[[3]]})
  output$skupno_cakanje <- renderPrint({podatki(tabela())[[4]]})
  output$max_cakanje <- renderPrint({podatki(tabela())[[5]]})
  output$max_cas_knjiznica <- renderPrint({podatki(tabela())[[6]]})
}

shinyApp(ui = ui, server = server)