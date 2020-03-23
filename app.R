library(shiny)
source("funkcija.R")
source("analiza.R")

#source("vizualizacija.R")
utezi_knjige_in <- c(0.35,0.23,0.17,0.10,0.05, 0.04,0.03,0.03)
utezi_knjige_out <- c(0.35,0.23,0.17,0.10,0.05, 0.04,0.03,0.03)


ui <- fluidPage(
  
  # slider za utezi 
  
  numericInput(inputId = "parameter_prihodi", label = "intenziteta prihodov", 
               value = 0.03, min = 0.0001, max = 1, step = 0.0001, width = "150px"),
  numericInput(inputId = "parameter_klici", label = "intenziteta klicov", 
               value = 0.01, min = 0.0001, max = 1, step = 0.0001, width = "150px"),
  sliderInput(inputId = "knjige_uporabnik", label = "najvecje stevilo knjig izposoje", 
              value = 8, min = 1, max = 8, width = "150px"),
  sliderInput(inputId = "cas_obratovanja", label = "Čas obratovanja", value = as.numeric(20000), min = 3600, max = 43200),
  sliderInput(inputId = "max_knjig", label = "kritično št. knjig", value = 10, min = 1, max = 30),
  checkboxInput(inputId = "klici_ali", label = "klici dovoljeni", value = TRUE, width = "150px"),
  
  plotOutput("graf_stranke_vedno"),
  #plotOutput("graf_stranke_ob_dogodkih")
)

server <- function(input,output){
  output$graf_stranke_vedno <- renderPlot({
    tabela <- ustvari_skupno_tabelo(input$parameter_prihodi, 
                                    input$parameter_klici, 
                                    utezi_knjige_in, 
                                    utezi_knjige_out, 
                                    input$cas_obratovanja, 
                                    input$max_knjig,
                                    input$klici_ali)
    tabela_stranke_prihodi <- naredi_tabelo_stanja_strank(tabela)
    graf_stranke <- ggplot(data = tabela_stranke_prihodi) +
      geom_step(mapping = aes(x = cas, y = stevilo_strank_v_knjiznici)) +
      scale_x_continuous(breaks=seq(0, tail(tabela_stranke_prihodi[,1],1), input$cas_obratovanja / 10))
    graf_stranke
  })
  
  #output$graf_stranke_ob_dogodkih <- renderPlot({
   # graf1
  #})
}

shinyApp(ui = ui, server = server)