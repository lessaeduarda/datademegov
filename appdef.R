# Require or instal packages:
if(require(shiny)==F)install.packages('shiny');require(shiny)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(ggrepel)==F)install.packages('ggrepel');require(ggrepel)
if(require(magrittr)==F)install.packages('magrittr');require(magrittr)
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(plotly)==F)install.packages('plotly');require(plotly)
if(require(shinythemes)==F)install.packages('shinythemes');require(shinythemes)

# !!! Set your own working directory and load database !!!
setwd("C:/Users/Duda/Desktop/TrabalhoFinalAD")
load("mariaeduarda-lessa-ad-ufpe-2019.Rda")

shinyApp(
  ui <- shinyUI(fluidPage(
    fluidPage(theme = shinytheme("lumen"),
      titlePanel("Does Democracy Encourage E-Government?"),
      h4( "Data visualization for the relation between The Economist Intelligence Unit's Democracy Index and United Nation's E-Government Development Index, controlled by GDP per capita transformed in log."),
      sidebarLayout(
        sidebarPanel(
        h3("PLOT 1"),
        helpText("Scatterplot of all 157 countries in the dataset, hover over points to see more informations. Also, you can click on EGDI levels displayed on the right side of the graph to select desired options."),
        h3 ("PLOT 2"),
        selectInput(inputId = "x",
                    label = "Choose Continent:",
                    choices = levels(baseDemEgov$Continent),
                    selected = "Africa"),
        h3("PLOT 3"),
        selectInput(inputId = "y",
                    label = "Choose Democracy Index Level:",
                    choices = levels(baseDemEgov$DemLevel),
                    selected = "Authoritarian")),
                  mainPanel(plotlyOutput("plot1"),
                            plotlyOutput("plot2"),
                            plotlyOutput("plot3"))
        ))
      )
    ),
  
  server <- shinyServer(
    function(input, output) {
      filtered_data <- reactive({
        filter(baseDemEgov, Continent == input$x,
               GDPlog)
                         })
      filtered_data2 <- reactive({
        filter(baseDemEgov, DemLevel == input$y,
               GDPlog)
      })

      output$plot1 <- renderPlotly({
        p <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex + GDPlog, 
                                            color = EGDILEVEL,label = Country)) +
          theme_replace()
        p <- p + geom_point()+
          scale_color_manual(values = c("Very High EGDI" = "#4682B4", 
                                          "High EGDI" = "#3CB371", 
                                        "Middle EGDI" = "#DAA520", "Low EGDI" = "#E9967A"))
        
        ggplotly(p)
      })
      output$plot2 <- renderPlotly({
        q <- ggplot(data = filtered_data(), aes(y = EGDI, x = DemIndex + GDPlog, 
                                                color = EGDILEVEL,label = Country)) +
          theme_replace()
        q <- q + geom_point()+
          scale_color_manual(values = c("Very High EGDI" = "#4682B4", 
                                        "High EGDI" = "#3CB371", 
                                        "Middle EGDI" = "#DAA520", "Low EGDI" = "#E9967A"))
        
        ggplotly(q)
      })
      output$plot3 <- renderPlotly({
        r <- ggplot(data = filtered_data2(), aes(y = EGDI, x = DemIndex + GDPlog, 
                                                color = EGDILEVEL,label = Country)) +
          theme_replace()
        r <- r + geom_point()+
          scale_color_manual(values = c("Very High EGDI" = "#4682B4", 
                                        "High EGDI" = "#3CB371", 
                                        "Middle EGDI" = "#DAA520", "Low EGDI" = "#E9967A"))
        
        ggplotly(r)
      })
      
    }
  ))

shinyApp(ui, server)
