# Require or instal packages:
if(require(shiny)==F)install.packages('shiny');require(shiny)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(ggrepel)==F)install.packages('ggrepel');require(ggrepel)
if(require(magrittr)==F)install.packages('magrittr');require(magrittr)
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(plotly)==F)install.packages('plotly');require(plotly)
if(require(shinythemes)==F)install.packages('shinythemes');require(shinythemes)

# !!! Set your own working directory and load database:
setwd("C:/Users/Duda/Desktop/TrabalhoFinalAD")
load("mariaeduarda-lessa-ad-ufpe-2019.Rda")

shinyApp(
  ui <- shinyUI(fluidPage(
    fluidPage(theme = shinytheme("united"),
      titlePanel("E-Government Development Index & Democracy Index"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "y",
                      label = "Choose Democracy Index Level",
                      choices = levels(baseDemEgov$DemLevel),
                      selected = "Authoritarian"),
        selectInput(inputId = "x",
                    label = "Choose Continent",
                    choices = levels(baseDemEgov$Continent),
                    selected = "Africa")),
                  mainPanel(plotlyOutput("plot1"))
        ))
      )
    ),
  
  server <- shinyServer(
    function(input, output) {
      filtered_data <- reactive({
        filter(baseDemEgov, DemLevel == input$y,
               Continent == input$x)
                         })
      output$plot1 <- renderPlotly({
        p <- ggplot(data = filtered_data(), aes(y = EGDI, x = DemIndex, 
                                            color = EGDILEVEL,label = Country)) +
          theme_replace()
        p <- p + geom_point()+
          scale_color_manual(values = c("Very High EGDI" = "#4682B4", 
                                          "High EGDI" = "#3CB371", 
                                        "Middle EGDI" = "#DAA520", "Low EGDI" = "#E9967A"))
        
        ggplotly(p)
      })
    }
  ))

shinyApp(ui, server)
