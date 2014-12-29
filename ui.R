library(shiny)
library(ggvis)


shinyUI(fluidPage(
  titlePanel("Rolling Stock Return Correlation vs WTI"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine. 
        Returns calculated based on Adjusted Close from yahoo finance."),
    
      textInput("symb", "Symbol", "SPY"),

    
      dateRangeInput("dates", 
        "Date range",
        start = "2013-01-01", 
        end = as.character(Sys.Date())),
   
      actionButton("getstock", "Get Stock"),
      
      br(),
      helpText("Kyle is so, so gay"),
      
      br(),
      selectInput("per","Periodicity:",c("days","weeks","months")),
      
      br(),      
      sliderInput("window",label = ("length of regression window"),
                  min = 5, max = 900, value = 60)
      
    ),
    
    mainPanel(
      plotOutput("line"),
      br(),
      ggvisOutput("ggscatter"),
      br(),
      #plotOutput("scatter"),
      #br(),
      tableOutput("scatterTable")
      )
  )
))