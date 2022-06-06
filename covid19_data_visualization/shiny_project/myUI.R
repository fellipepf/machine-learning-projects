
# load packages
library(shiny)
library(shinyjs)

library(ggplot2)
library(dbplyr)
library(tidyverse)

library(tibbletime)
library(lubridate)

library(sp)  #read shape file
library(sf)  #read shape file
library(choroplethrMaps) #load world map
library(rgdal)
library(choroplethr) #load world map
library(plotly) # add feature to ggplot
library(ggrepel) #add label at the end of the line
library(gganimate)  #create animation
library(gifski)
library(av)

library(flexdashboard)  #add gauges indicator
library(shinydashboard) #add gauges indicator
library(geobr)
library(gridExtra) #organise ggplot side by side
library(grid)
library(devtools)




source("load_data.R")

#----------------------------------------------- UI CODE ----------------------------------------
myUI <- fluidPage(
  
  titlePanel("COVID-19"),
  sidebarLayout(
    
    sidebarPanel(
      h3("Ireland"),
      h4("Map with counties"),
      radioButtons("ireland_map_opt", "Plot Type",
                   c("New Confirmed Cases"        = "new_confirmed_cases",
                     "Cumulative Confirmed Cases" = "cumulative_cases")
      ),
      
      useShinyjs(),
      
      hr(),
      h4("Line chart"),
      minDate <- min(ireland_covid_county$date),
      maxDate <- max(ireland_covid_county$date),
      sliderInput("date_cases", "Year:",
                  min = minDate, 
                  max = maxDate, 
                  value = c(maxDate), 
                  timeFormat="%Y-%m-%d",
                  animate = animationOptions(interval = 200, loop = FALSE)),
      
      hr(),
      h3("Brazil"),
      h4("Bump Chart"),
      radioButtons("bump_chart_input", "Select input type:",
                   c("Cumulative Confirmed Cases" = "cumulative_cases",
                     "Cumulative Deaths"          = "cumulative_deaths")
      ),
      
      h4("Line Chart"),
      radioButtons("br_linechart_cases", "Cases - Total or Daily",
                   c("New Confirmed Cases"        = "new_confirmed_cases",
                     "Cumulative Confirmed Cases" = "cumulative_cases")
      ),
      radioButtons("br_linechart_deaths", "Deaths - Total or Daily",
                   c("New Deaths"                 = "new_deaths",
                     "Cumulative Deaths"          = "cumulative_deaths")
      ),
      
      
      hr(),
      h4("World"),
      minDate <- min(world_covid$date),
      maxDate <- max(world_covid$date),
      sliderInput("date_world_spread", "Covid spread:",
                  min = minDate, 
                  max = maxDate, 
                  value = c(maxDate), 
                  timeFormat="%Y-%m-%d",
                  animate = animationOptions(interval = 200, loop = FALSE)),
      
      h3("Select countries to display in gauges"),
      selectInput('gauge1_opt', 'Gauge 1: ', unique(world_covid$location)),
      selectInput('gauge2_opt', 'Gauge 2: ', unique(world_covid$location)),
      selectInput('gauge3_opt', 'Gauge 3: ', unique(world_covid$location)),
      selectInput('gauge4_opt', 'Gauge 4: ', unique(world_covid$location)),
      
      #   selectInput('x_axis', 'X', names(datasetForInput), names(datasetForInput)[[1]]),
      #  selectInput('y_axis', 'Y', names(datasetForInput), names(datasetForInput)[[2]]),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Ireland", plotOutput("mapCasesByCounty"),
                           plotlyOutput("lineChartIreland"),
                           plotOutput("barChartIrelandByDate")
                  ),
                  
                  tabPanel("Brazil", 
                           plotOutput("brazil_map"),
                           plotOutput("bump_chart_br_states"),
                           plotOutput("brazil_linechart_cases"),
                           plotOutput("brazil_linechart_deaths")
                  ),
                  
                  tabPanel("World", 
                           plotOutput("world_covid"),
                           plotOutput("world_covid_spread"),
                           dashboardBody(
                             column(6,box(flexdashboard::gaugeOutput("gauge1"),width=12,title="Vaccine Gauge")),
                             column(6,box(flexdashboard::gaugeOutput("gauge2"),width=12,title="Vaccine Gauge")),
                             column(6,box(flexdashboard::gaugeOutput("gauge3"),width=12,title="Vaccine Gauge")),
                             column(6,box(flexdashboard::gaugeOutput("gauge4"),width=12,title="Vaccine Gauge")),
                             
                           ),
                           plotOutput("world_lineChart")
                  ),                
                  
                  tabPanel("World - animated", plotOutput("world_covid_lineplot")), 
                  
                  tabPanel("Sources", plotOutput("sources"))
      )
    )
  )
)
