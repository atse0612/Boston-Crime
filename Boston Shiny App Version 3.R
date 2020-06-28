# Shiny App for Boston Crime

## Load the libraries
library(shiny) #to run app
library(shinydashboard) #create dashboard
library(lubridate)
library(DataExplorer)
library(ggplot2)
library(ggvis)
library(leaflet)
library(RColorBrewer) #color palette
library(treemap) # treemap
library(treemapify) # treemap
library(DT) # data table
library(ggmap) # creating maps
library(tidyverse) # tidyr and dplyr are included in this package
library(reshape2) #to melt data

## Changing working directory and reading the dataset
bostoncrime <- read.csv('./Crime Incident Reports (August 2015 - December 11 2019).csv') #dataset file

## Looking at the first few rows
head(bostoncrime)

## Removing the NA values and dropping columns
bostoncrime <- bostoncrime %>%
  select(-c(SHOOTING)) #removing the shooting column as there's no need for it
bostoncrime <- na.omit(bostoncrime) #removing the NA values in the dataset

## Looking at the first few rows again to see if NA Values are present
head(bostoncrime)

## Tidying up the data 
boston_group <- aggregate(list(COUNT=bostoncrime$INCIDENT_NUMBER), 
                          by=list(DISTRICT=bostoncrime$DISTRICT, YEAR=bostoncrime$YEAR), FUN=length)

bosfin <- boston_group %>% spread(DISTRICT, COUNT) # Change Column Nmae

bosfin_df <- bosfin[-1] # retrieving the district column counts 
row.names(bosfin_df) <- bosfin$YEAR # labeling the year rows

bos_year <- boston_group %>% spread(YEAR, COUNT)
bos_melt <- bos_year %>% melt(id.vars="DISTRICT")

## Fixing up the month variables

### Create a variable count starting with value 1
bostoncrime$Count <- 1

### Replace month columns by abbreviated month name

bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "1", "Jan",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "2", "Feb",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "3", "Mar",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "4", "Apr",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "5", "May",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "6", "Jun",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "7", "Jul",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "8", "Aug",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "9", "Sep",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "10", "Oct",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "11", "Nov",bostoncrime$MONTH)
bostoncrime$MONTH <- ifelse(bostoncrime$MONTH == "12", "Dec",bostoncrime$MONTH)

## Creating a Serious Crimes Filter for other plots
boston_serious <- bostoncrime %>% 
  filter(YEAR %in% c(2015, 2016, 2017, 2018, 2019), UCR_PART == "Part One")

## Less Serious Crimes
boston_less_serious <- bostoncrime %>%
  filter(YEAR %in% c(2015, 2016, 2017, 2018, 2019), UCR_PART == "Part Two")

## Setting up the shiny app
ui= dashboardPage ( 
  
  dashboardHeader( title="Crimes in Boston"
  ),
  dashboardSidebar(
## Creating the sidebar menu for the dashboard    
    sidebarMenu(
      menuItem(tabName = "main", "District Crime by Year", icon = icon("chart-bar")),
      menuItem(tabName = "extra1", "Time-Series Graph by Year", icon = icon("chart-bar")),
      menuItem(tabName = "extra2", "Treemap Count of Crime", icon = icon("chart-bar")),
      menuItem(tabName = "extra3", "Crime over Time", icon = icon("chart-bar")),
      menuItem(tabName = "extra4", "Distribution", icon = icon("chart-bar")),
      menuItem(tabName = "extra5", "Crime Table Database", icon = icon("table")),
      menuItem(tabName = "extra6", "Day of Week", icon = icon("chart-bar")),
      menuItem(tabName = "extra7", "Correlation Plot", icon = icon("chart-bar")),
      menuItem(tabName = "extra8", "Less Serious Crimes in Boston", icon = icon("chart-bar"))
    )
  ),

## Creating the body of the dashboard  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box (width=12,
                       color = "navajowhite1", 
                       selectInput("DISTRICT", "DISTRICT:", 
                                   choices=colnames(bosfin_df)),
                       hr(),
                       helpText("Boston Crime Incidents by Month"),
                  ),
                  box(width = 12,
                      title = "Number of Crime Incidents by District",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsPlot")
                      )
                  ),
          ), 
          column (7,
                  box(width = 12,
                      title = "Stacked Chart by District",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsGroupByYearLineGraph")
                      )
                  ),
          ),          
          
          
        ),
      ),
      
      tabItem(
        tabName = "extra1",
        fluidRow(
          box(width = 16,
              title = "Crime by Year",
              color = "navajowhite1", ribbon = TRUE, title_side = "top right",
              height = "1000",
              plotOutput("plot1") # first plot
          ),
        ),
      ),
      
      tabItem(
        tabName = "extra2",
        fluidRow(
          box(width = 16,
              title = "Treemap Counts",
              color = "navajowhite1", ribbon = TRUE, title_side = "top right",
              plotOutput("plot2")
              
          ),
          
        ),
      ),
      
      tabItem(
        tabName = "extra3",
        fluidRow(
          column (6,
                  box(width = 14,
                      title = "Serious Crime Rates by Hour",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot3")
                      )
                  ),
                  box(width = 14,
                      title = "Top Streets of Crime by Opportunity",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot5")
                      )
                  ),
          ),
          column (6,
                  box(width = 14,
                      title = "Summary by Month",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot4")
                      )
                  ),
                  box(width = 14,
                      title = "Summary by Day According to Street",
                      color = "navajowhite1", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot6")
                      )
                  ),
          )
          
        ),
      ),
      
      tabItem(
        tabName = "extra4",
        fluidRow(
          box(width = 15,title = "Hourly Distribution",
              
              plotOutput("plot7")
          ),
          
        ),
      ),
      tabItem(
        tabName = "extra6",
        fluidRow(
          box(width = 16,
              title = "Day of Week",
              color = "navajowhite1", ribbon = TRUE, title_side = "top right",
              plotOutput("plot8")
              
          ),
          
        ),
      ),    
      tabItem(
        tabName = "extra7",
        fluidRow(
          box(width = 20,
              title = "Correlation Plot",
              color = "navajowhite1", ribbon = TRUE, title_side = "top right",
              plotOutput("plot9")
              
          ),
          
        ),
      ), 
      tabItem(
        tabName = "extra8",
        fluidRow(
          box(width = 16,
              title = "Less Serious Crimes in Boston",
              color = "navajowhite1", ribbon = TRUE, title_side = "top right",
              plotOutput("plot10")
              
          ),
          
        ),
      ),    
      
      tabItem(
        tabName = "extra5",
        fluidRow(
          box(title = "Boston Crime Database", color = "orange", ribbon = TRUE, 
              width = 12,status = "success", 
              height = "575",solidHeader = T, dataTableOutput("crimetable")),
      
     
          
        ),
      )
    ),
  ),
)



# Creating the function for the shiny app
server=shinyServer(function(input, output, session ) {
  
  
  # Rendering the plot for District variable
  output$crimeIncidentsPlot <- renderPlot({
    
    # Rendering barplot with dropdown menu
    barplot(bosfin_df[,input$DISTRICT],
            main=paste("District:", input$DISTRICT),
            ylab="Number of crimes",
            xlab="Year",
            names.arg = c(2015:2019),
            col=brewer.pal(n = 5, name = "Greys"),
            border = "darkblue" 
    ) # 
    
  })
# Stacked Bar Plot of Crimes by Year
  output$crimeIncidentsGroupByYearLineGraph <- renderPlot({
    
    # Render a stacked bar plot by district
    ggplot(bos_melt , aes(x=DISTRICT, y=value,group=variable,
                          color=variable, fill = variable)) +
      geom_bar(position="stack", stat="identity") #stacked bar plot
      
  })

## Year Series Line Plot
  output$plot1<- renderPlot({
    bostoncrime  %>% separate(OCCURRED_ON_DATE, c("Date", "Time"), sep = " ") %>% 
      mutate(Date = ymd(Date)) %>% 
      ggplot(aes(Date))+
      geom_freqpoly(color="salmon", size = 1.5)
      
  })

## Treemap of Crimes
  output$plot2<- renderPlot({
    dd_aggr2 <- aggregate(Count~ OFFENSE_CODE_GROUP, data = bostoncrime, FUN = sum)
    # Plot the graph
    ggplot(data=dd_aggr2, aes(area = Count, fill = OFFENSE_CODE_GROUP, label = OFFENSE_CODE_GROUP)) +
      geom_treemap() + 
      geom_treemap_text(fontface="italic", color = "black", place = "center", grow=TRUE)
  })
  
## Top Group of Serious Crimes Per Day
  output$plot3<- renderPlot({
    ggplot(data = filter(boston_serious)) +
      geom_bar(mapping = aes(x = HOUR, fill = OFFENSE_CODE_GROUP)) +
      xlab("Hour") +
      ylab("Count") +
      labs(fill="Offense Code Group")
    
  })
  ## Orginally a line graph in Tableau, and only barplot work on the app, and change of color
  output$plot4<- renderPlot({
    # Plot the graph 
    ggplot(bostoncrime, aes(x=MONTH),border = "seagreen")+
      geom_bar(stat="Count", width=0.75, fill="sienna")+ 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
      labs(x = "Month", y = "Count") + 
      theme_minimal()+ theme(axis.title.x=element_blank()) + 
      theme(axis.title.y=element_blank()) +
      theme(axis.text.y = element_text(size= 6),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
  })

# Top Street Count only with a different shade of color from Tableau 
  output$plot5<- renderPlot({
    # Plot the graph 
    bostoncrime  %>% count(STREET) %>% arrange(-n) %>% head(20) %>% 
      ggplot(aes(reorder(STREET,n), n))+
      geom_col(fill = "lightcyan4")+
      coord_flip()+
      labs(x = NULL, y = NULL)
    })
  
  
# Weekday by Street  
  output$plot6<- renderPlot({
    # Plot the graph 
    bostoncrime %>% 
      filter(STREET %in% (bostoncrime %>% 
                            count(STREET) %>% 
                            arrange(-n) %>% head(10) %>% pull(STREET))) %>% 
      ggplot(aes(DAY_OF_WEEK, fill = STREET))+
      geom_bar(position = "fill")+
      coord_flip()+
      scale_fill_ordinal()+
      labs(x="Count", y = "Day of Week")
  })
  # Data table originated from Tableau
  output$crimetable <- renderDT({datatable(bostoncrime, 
                                          options = list(searching = TRUE, 
                                                         pageLength = 50,
                                                         lengthMenu = c(50, 100, 200), 
                                                         scrollX = T))})
  ## Create hourly distribution similar to Tableau
  output$plot7 <- renderPlot({
    ggplot(bostoncrime) +
      geom_bar(aes(x=HOUR), fill="orangered") +
      xlab("Hour") +
      ylab("Count")})
    
  

## Day of Week Chart as similar to Tableau
  output$plot8 <- renderPlot({
    ggplot(bostoncrime, aes(x=DAY_OF_WEEK),border = "seagreen")+
      geom_bar(stat="Count", width=0.75, fill="burlywood")+ 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
      labs(x = "Day of Week", y = "Count") + 
      theme_minimal()+ theme(axis.title.x=element_blank()) + 
      theme(axis.title.y=element_blank()) +
      theme(axis.text.y = element_text(size= 6),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
  })

## Correlation Plot (New Plot)
  output$plot9 <- renderPlot({plot_correlation(bostoncrime)})    

## Less Serious Crimes in Boston
  output$plot10<- renderPlot({
    ggplot(data = filter(boston_less_serious)) +
      geom_bar(mapping = aes(x = HOUR, fill = OFFENSE_CODE_GROUP)) +
      xlab("Hour") +
      ylab("Count") +
      labs(fill="Offense Code Group")
    
  })
    
})


## Running the app command
shinyApp(ui=ui, server=server)

