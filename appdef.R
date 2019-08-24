require(shiny)
require(ggplot2)
require(ggrepel)
require(magrittr)
require(dplyr)
require(plotly)

setwd("C:/Users/Duda/Desktop/TrabalhoFinalAD")
load("mariaeduarda-lessa-ad-ufpe-2019.Rda")

shinyApp(
  ui <- shinyUI(fluidPage(
    fluidPage(
      titlePanel("E-Government Development Index & Democracy Index"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "y",
                      label = "Choose Democracy Index Level:",
                      choices = levels(baseDemEgov$DemLevel),
                      select = "Authoritarian")),
                  mainPanel(plotlyOutput("plot1"))
        )
  ))),
  
  server <- shinyServer(
    function(input, output) {
      filtered_data <- reactive({
        filter(baseDemEgov, DemLevel == input$y)
                 })
      output$plot1 <- renderPlotly({
        p <- ggplot(data = filtered_data(), aes(y = EGDI, x = DemIndex, 
                                            color = EGDILEVEL,label = Country)) +
          theme_replace()+
          scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))
        p <- p + geom_point()
        
        ggplotly(p)
      })
    }
  ))

shinyApp(ui, server)
