


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
