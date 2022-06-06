
# Fellipe Paes Ferreira

# set working directory
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#install packages
install.packages("shinyjs")
install.packages("tibbletime")
install.packages("rgdal")
install.packages("rgdal", repos="http://R-Forge.R-project.org")
install.packages(c("sf", "tmap"))
#brew install gdal

install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("tmap")
install.packages("sp")


install.packages("tidyverse")
install.packages("gganimate")
install.packages("gifski")
install.packages("av")

install.packages("devtools")
install.packages("geobr")  #brazilian map

# if not work use the commands below
utils::remove.packages('geobr')
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

install.packages("ggrepel")  #include label at end of line chart
install.packages("shinydashboard")
install.packages("flexdashboard")


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

#-------------------- 
#run this first

#ireland map
ireland_map <- st_read("maps/counties.shp")
ireland_map$NAME_TAG

#ireland dataset
# https://data.gov.ie/dataset/covid19countystatisticshpscireland1/resource/9ec1cba9-a9a9-4890-a4cc-980f4676197b
ireland_covid_county <- read.csv("datasets/Covid19CountyStatisticsHPSCIreland.csv" )
ireland_covid_county

#make sure the name of column is correct
colnames(ireland_covid_county)[1] <- 'OBJECTID'


# convert string to Date
ireland_covid_county$TimeStamp <- as.Date(ireland_covid_county$TimeStamp)

#chaging name of column
ireland_covid_county <- ireland_covid_county %>%
    rename(date = TimeStamp)


#ireland dataset does not have daily covid cases just the total value. 
# this function calculate daily covid cases 
calculateIrelandCountyDailyCases <- function (){
  codCountyList <- unique(ireland_covid_county$ORIGID)
  
  for (cod in codCountyList){
    valuesByCounty <- subset(ireland_covid_county, ORIGID == cod)
    
    for (i in 2:nrow(valuesByCounty) ){
      codLine <- valuesByCounty$OBJECTID[i]
      
      totalCaseToday = valuesByCounty$ConfirmedCovidCases[i]
      totalCaseYesterday = valuesByCounty$ConfirmedCovidCases[i-1]
      
      caseDailyCalc = (totalCaseToday - totalCaseYesterday)
      
      # probably missing data 
      if (caseDailyCalc < 0){
        caseDailyCalc = 0
      }

      ireland_covid_county$dailyCasesCalc[ ireland_covid_county$OBJECTID == codLine ] <- caseDailyCalc 
      
    }
  }
  ireland_covid_county
}
ireland_covid_county = calculateIrelandCountyDailyCases()

last_day = max(ireland_covid_county$date)
last_day

values_last_day = ireland_covid_county %>% filter(date == last_day )
values_last_day



#world covid dataset
world_covid <- read.csv("datasets/owid-covid-data.csv")
unique(world_covid$location)

str(world_covid)

# convert string to Date
world_covid$date <- as.Date(world_covid$date)

world_last_day = max(world_covid$date)
world_last_day


#brazil dataset
br_dataset <- read.csv("datasets/Brazil.csv")
str(br_dataset)

br_dataset$data <- as.Date(br_dataset$data)

map_states <- read_state(year=2019)
map_states



#----------------------------------------------- UI CODE ----------------------------------------
ui <- fluidPage(
  
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

#----------------------------------------------- SERVER CODE ----------------------------------------
server <- function(input, output) {
  

  
#------ Ireland ------ 
  output$mapCasesByCounty <- renderPlot({
    
    ireland_map_input_opt = input$ireland_map_opt
    print(ireland_map_input_opt)
    
    if (ireland_map_input_opt == "cumulative_cases"){
      type_irebarchart = "ConfirmedCovidCases" 
    }else{
      type_irebarchart = "dailyCasesCalc"
    }
    
    map_and_values = inner_join(values_last_day,ireland_map, by = c("CountyName" = "NAME_TAG" ) )
    
    p <- ggplot(map_and_values)+
      geom_sf( aes(fill = !!sym(type_irebarchart),  geometry = geometry))+
      geom_text(aes(label = CountyName, x = Long, y = Lat), colour= "white") +
      labs(x = "Longitude", y = "Latitude", title = "Ireland ConfirmedCovidCases") 
    print(p)
    })
  
  #barchart
  output$barChartIreland <- renderPlot({
    last_day_barchart = max(ireland_covid_county$date)
    last_day_barchart
    
    values_last_day_barchart = ireland_covid_county %>% filter(date == last_day_barchart )
    values_last_day_barchart
    
    
  ggplot(values_last_day_barchart, mapping = aes(reorder(CountyName, ConfirmedCovidCases), ConfirmedCovidCases, label = ConfirmedCovidCases))+
    geom_bar(stat="identity")+
    geom_text()+
    coord_flip()+
    labs(title='ConfirmedCovidCases - ', x = "Conties", y = "ConfirmedCovidCases") 
  })
  
  #line chart
  output$lineChartIreland <- renderPlotly({
    
    
    top10_counties <-  arrange(values_last_day, desc(ConfirmedCovidCases) )
    top10_counties <- top10_counties[1:10, "CountyName"]
    
    top10_values = ireland_covid_county %>% filter(CountyName  %in% top10_counties )
    top10_values
    
    
    date =  format(as.Date(top10_values$TimeStamp), "%d")
    top10_values$ID <- seq.int(nrow(top10_values))
    
    g <- ggplot(top10_values,
           aes(x = date, y = ConfirmedCovidCases, group = CountyName, color = CountyName)) +
      geom_line() +
     # scale_color_viridis_d() +
      labs(x = "Day of Month", y = "Total Cases", title = "Total cases Ireland - Top 10 counties -  ") +
      geom_text_repel(
        data = subset(top10_values, date == max(date)),
        aes(label = CountyName),
        size = 3,
        nudge_x = 45,
        segment.color = NA
      )+
      theme_bw() +
      theme(legend.title=element_blank())
    #theme(legend.position = "top")
    
    ggplotly(g)
    })
  
  
  output$barChartIrelandByDate <- renderPlot({
    dateSelected = input$date_cases[1]
    values_last_day = ireland_covid_county %>% filter(date == dateSelected )
    
    ggplot(values_last_day, mapping = aes(reorder(CountyName, ConfirmedCovidCases), ConfirmedCovidCases, label = ConfirmedCovidCases,  fill = CountyName))+
      geom_bar(stat="identity")+
      geom_text()+
      coord_flip()+
      labs(title= paste('ConfirmedCovidCases - ',dateSelected), x = "Conties", y = "ConfirmedCovidCases") 
  })
  
#------ Brazil ------ 
  #https://covid.saude.gov.br/
  
  output$brazil_map <- renderPlot({
    
    covid_dataset_states_br = br_dataset %>% filter( regiao != "Brasil", is.na(codmun), municipio == "",populacaoTCU2019 >1  )
    covid_dataset_states_br
    
    
    
    last_date = max(covid_dataset_states_br$data)
    last_info_br = covid_dataset_states_br %>% filter(data  == last_date)
    last_info_br
    
    
    all_map_covid = inner_join(map_states, last_info_br, by=c("abbrev_state"="estado"))
    
    map1 <- ggplot(data=all_map_covid) +
      geom_sf( aes( fill=casosAcumulado,  geometry = geom)) +
      scale_fill_gradient2(mid = "white",high = "firebrick4") +
      geom_sf_label(aes(label = abbrev_state),
                    label.padding = unit(0.5, "mm"),
                    size = 3) +
      labs(fill = "",
           title = "Total Cases",
           subtitle = paste("Date: ",last_date))+
      xlab("Longitude") + 
      ylab("Latitude") +
      theme_minimal()
    
    
    map2 <- ggplot(data=all_map_covid) +
      geom_sf( aes( fill=obitosAcumulado,  geometry = geom)) +
      scale_fill_gradient2(mid = "white",high = "firebrick4") +
      geom_sf_label(aes(label = abbrev_state),
                    label.padding = unit(0.5, "mm"),
                    size = 3) +
      labs(fill = "",
           title = "Total Deaths",
           subtitle = paste("Date: ",last_date))+
      xlab("Longitude") + 
      ylab("Latitude") +
      theme_minimal()
    
    #show the graphs side by side
    grid.arrange(map1,map2, ncol=2)
  })
    
  
  output$bump_chart_br_states <- renderPlot({
    
    bump_chart_input = input$bump_chart_input
    
    if (bump_chart_input == "cumulative_cases"){
      input_type = "casosAcumulado" 
    }else{
      input_type = "obitosAcumulado"
    }
    
    print(input_type)
    
    df_by_state <- br_dataset %>% 
      filter(regiao != "Brasil", is.na(codmun) )
    df_by_state
    
    minDate = min(df_by_state$data)
    maxDate = max(df_by_state$data)
    
    totalMonths = seq(from=minDate, to=maxDate, by='month')
    lasDaysMonths <- c()
    
    
    for (i in 1:length(totalMonths)){
      res = ceiling_date(as.Date(totalMonths[i]), "month") -1
      lasDaysMonths=c(lasDaysMonths,res)
    }
    class(lasDaysMonths) <- "Date"
    
    df_by_state <- df_by_state %>% 
      filter(data %in%  lasDaysMonths)
    
    df.rankings <- df_by_state %>% 
      group_by(data) %>% 
      
      #the variable input_type is dynamic. To use this variable with filter in dplyr 
      # have to use this function and operator: !!sym() 
      arrange(data, !!sym(input_type) , estado ) %>% 
      mutate(ranking = row_number(),
             month = ((interval(minDate, data) %/% months(1)  )+1)  )%>% 
      as.data.frame()
    
    print(df.rankings)
    
    show.top.n <- 27
    months_available <- max(df.rankings$month)
    
    ggplot(data = df.rankings, aes(x = month, y = ranking, group = estado)) +
      geom_line(aes(color = estado, alpha = 1), size = 1) +
      geom_point(aes(color = estado, alpha = 1), size = 4) +
      geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(breaks = 1:show.top.n) +
      scale_x_continuous(breaks = 1:months_available, minor_breaks = 1:months_available, expand = c(.05, .05)) +
      geom_text(data = df.rankings %>% filter(month == "1"),
                aes(label = estado, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
      geom_text(data = df.rankings %>% filter(month == "12"),
                aes(label = estado, x = months_available +0.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
      coord_cartesian(ylim = c(1,show.top.n)) + 
      theme(legend.position = "none") +
      labs(x = "COVID-19 Deaths by Month for each state of Brazil",
           y = "Rank",
           title = "Bump Chart of covid total deaths by month for each Brazilian State ",
           subtitle = "States ranked by total deaths by month") 
  
  })
  output$brazil_linechart_cases <- renderPlot({
    
    option_cases = input$br_linechart_cases
    
    if (option_cases == "new_confirmed_cases"){
      cases_type = "new_cases_smoothed" 
      title_label_cases = "COVID-19 Daily cases in Brazil"
    }else{
      cases_type = "total_cases"
      title_label_cases = "COVID-19 Total cases in Brazil"
    }
    
    world_covid$date <- as.Date(world_covid$date)
    df_countr_br <- world_covid %>% 
      filter(location == "Brazil")
    
    rect2 <- data.frame(xmin=as.Date("2020-12-01"), xmax=as.Date("2021-01-05"), ymin=-Inf, ymax=Inf)
    ggplot(df_countr_br, aes(date)) + 
      geom_line(aes(y = !!sym(cases_type)  , colour = "Daily Cases"), show.legend = FALSE) +
      geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="grey20",
                alpha=0.5,
                inherit.aes = FALSE)+
      labs(title = title_label_cases,
           x ="Date", y = "Cases")
    
  })
  
  output$brazil_linechart_deaths<- renderPlot({
    
    option_deaths = input$br_linechart_deaths
    
    if (option_deaths == "new_deaths"){
      deaths_type = "new_deaths_smoothed" 
      title_label = "COVID-19 Daily deaths in Brazil"
    }else{
      deaths_type = "total_deaths"
      title_label = "COVID-19 Total deaths in Brazil"
    }
    
    
    world_covid$date <- as.Date(world_covid$date)
    df_countr_br <- world_covid %>% 
      filter(location == "Brazil")
    
    rect1 <- data.frame(xmin=as.Date("2020-11-20"), xmax=as.Date("2021-03-01"), ymin=-Inf, ymax=Inf)
    rect2 <- data.frame(xmin=as.Date("2020-12-01"), xmax=as.Date("2021-01-05"), ymin=-Inf, ymax=Inf)
    ggplot(df_countr_br, aes(date)) + 
      geom_line(aes(y = !!sym(deaths_type)  , colour = "Daily Deaths"), show.legend = FALSE) +
      geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="grey20",
                alpha=0.5,
                inherit.aes = FALSE)+
      geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                color="yellow",
                alpha=0.5,
                inherit.aes = FALSE)+
      labs(title= title_label,
           x ="Date", y = "Deaths")
    
  })
#------------------------------------------ World ------------------------------------------
  output$world_covid <- renderPlot({
    world_values_last_day = world_covid %>% filter(date == "2021-01-27" )
    #world_values_last_day
    
    plotdata <- world_values_last_day %>%
      rename(region = location) %>%
      mutate(region = tolower(region)) %>%
      mutate(value = total_cases) %>%
      mutate(region = recode(region,
                             "united states"    = "united states of america",
                             "congo, dem. rep." = "democratic republic of the congo",
                             "congo, rep."      = "republic of congo",
                             "korea, dem. rep." = "south korea",
                             "korea. rep."      = "north korea",
                             "tanzania"         = "united republic of tanzania",
                             "serbia"           = "republic of serbia",
                             "slovak republic"  = "slovakia",
                             "yemen, rep."      = "yemen"))
    
    
    
    country_choropleth(plotdata)+
      scale_fill_brewer(palette="YlOrBr") +
      
      labs(title = "Word cases of covid",
           subtitle = "for the month",
           caption = "source: https://www.gapminder.org",
           fill = "cases")
  })
  
  
  output$world_covid_spread <- renderPlot({
    
    spread_date_selected = input$date_world_spread[1]
    
    #keep colluns to use
    df_world_spread = subset(world_covid, select = c(iso_code,continent, location, date, total_cases,total_deaths) )
    
    df_world_spread$total_cases <- 1
    
    df_world_spread <- df_world_spread %>%
      
      rename(region = location) %>%
      mutate(region = tolower(region)) %>%
      mutate(value = total_cases) %>%
      mutate(region = recode(region,
                             "united states"    = "united states of america",
                             "congo, dem. rep." = "democratic republic of the congo",
                             "congo, rep."      = "republic of congo",
                             "korea, dem. rep." = "south korea",
                             "korea. rep."      = "north korea",
                             "tanzania"         = "united republic of tanzania",
                             "serbia"           = "republic of serbia",
                             "slovak republic"  = "slovakia",
                             "yemen, rep."      = "yemen"))
    print(spread_date_selected)
    plotdata_spread <- df_world_spread %>% filter(date == spread_date_selected )
    
    country_choropleth(plotdata_spread)+
      scale_fill_brewer(palette="RdYlGn") +
      labs(title = "Covid spread over the World",
           subtitle = "for the month",
           caption = "source: https://www.gapminder.org",
           fill = "cases")
    
    
    
  })
  ### line chart
  
  output$world_covid_lineplot <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')

    countries = c("Brazil", "Israel", "Italy","Ireland")
    vaccine <- world_covid %>%
      filter(location %in% countries) %>%
      drop_na(total_vaccinations_per_hundred)
    
    minDate = min(vaccine$date)
    maxDate = max(vaccine$date)
    
    #endpoint layer
    label_subset <- vaccine %>% 
      group_by(location) %>%
      filter(total_vaccinations_per_hundred == max(total_vaccinations_per_hundred, na.rm=TRUE)) %>%
      ungroup()
    
    #if there's a stall you get doubles
    label_subset <- label_subset %>% group_by(location) %>% filter(date==max(date)) %>% ungroup()
    
  
  p<-  ggplot(data = vaccine, aes(date, total_vaccinations_per_hundred, colour = location)) + geom_point() + geom_line() + 
      
      scale_x_date(limits = c(minDate,maxDate)) +
      geom_label_repel(
        data = label_subset,
        aes(label = location),
        size = 3

      )+
      geom_text(aes(label=location, group=location))+
      
      transition_reveal(date)
  #, width = 200, height = 200
  animate(p, duration = 5, fps = 20, renderer = gifski_renderer())
    anim_save("outfile.gif") 
    
    # Return a list containing the filename
    list(src = "outfile.gif", contentType = 'image/gif'

    )
  }, deleteFile = TRUE)
  
  
  countries = c("Brazil", "Israel", "Italy","Ireland")
  world_last_day = max(world_covid$date)
  vaccine_gauge <- world_covid %>%
    filter(location %in% countries, date == "2021-01-27") %>%
    drop_na(total_vaccinations_per_hundred) 
  
  print(countries)
  output$gauge1 <- renderGauge({
    country1 <- vaccine_gauge %>%
      filter(location == countries[1])
   
    gauge(country1$total_vaccinations_per_hundred, min = 0, max = 100, symbol = '%', label = paste(country1$location),
          gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39), colors = c("#CC6699")
          ))
  })
  
  output$gauge2 <- renderGauge({
    country2 <- vaccine_gauge %>%
      filter(location == countries[2]) 
    gauge(country2$total_vaccinations_per_hundred, min = 0, max = 100, symbol = '%', label = paste(country2$location),
          gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39), colors = c("#CC6699")
    ))
  })
  
  output$gauge3 <- renderGauge({
    country <- vaccine_gauge %>%
      filter(location == countries[3]) 
    gauge(country$total_vaccinations_per_hundred, min = 0, max = 100, symbol = '%', label = paste(country$location),gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39), colors = c("#CC6699")
    ))
  })
  
  output$gauge4 <- renderGauge({
    country <- vaccine_gauge %>%
      filter(location == countries[4]) 
    gauge(country$total_vaccinations_per_hundred, min = 0, max = 100, symbol = '%', label = paste(country$location),gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39), colors = c("#CC6699")
    ))
  })
  
  
  #line chart
  output$world_lineChart <- renderPlot({
    
    
    world_last_day = max(world_covid$date)
    vaccine_countries <- world_covid %>%
      filter(date == "2021-01-27")
    
    top10_countries <-  arrange(vaccine_countries, desc(population) )
    top10_countries <- top10_countries[1:10, "location"]
    
    countries = c("Brazil", "Israel", "Italy","Ireland", "Spain", "United States")
    
    top10_values_vaccine = world_covid %>% filter(location  %in% countries )
    
    top10_values_vaccine <- top10_values_vaccine[!(is.na(top10_values_vaccine$total_vaccinations_per_hundred)) ,]
    
    date =  format(as.Date(top10_values_vaccine$TimeStamp), "%d")

    ggplot(top10_values_vaccine,
                aes(x = date, y = total_vaccinations_per_hundred, group = location, color = location)) +
      geom_line() +
     
      labs(x = "Day of Month", y = "Total Cases", title = "Top 10 counties - Vaccine") +
      geom_text_repel(
        data = subset(top10_values_vaccine, date == max(date)),
        aes(label = location),
        size = 3,
        nudge_x = 45,
        segment.color = NA
      )
      #theme_bw() +
     # theme(legend.title=element_blank())
    
    
    
  })
  
  
  
  
  
  #############################
  


  
}

shinyApp(ui , server )
 