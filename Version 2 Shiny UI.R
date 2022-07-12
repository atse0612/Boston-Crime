## UI Section
library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(textOutput("title")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "daterange", label =  "Target Date Range:",
                     start = "2015-06-15", end = "2019-12-08",
                     min = "2015-06-15", max = "2019-12-08",
                     format = "mm/dd/yy",
                     separator = " - "),
      uiOutput("crimeselect")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Distributions",
                 h3(textOutput("shootingpct")),
                 plotOutput("crimeplot"),
                 plotOutput("timecrime")),
        tabPanel("Heatmap",
                 h3(textOutput("count_text")),
                 plotOutput("crimemap", width = 1000, height = 1000))
      )
      
    )
  )
))