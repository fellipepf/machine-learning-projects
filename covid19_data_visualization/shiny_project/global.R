#ireland map
ireland_map <- st_read("maps/counties.shp")
ireland_map$NAME_TAG

#ireland dataset
# https://data.gov.ie/dataset/covid19countystatisticshpscireland1/resource/9ec1cba9-a9a9-4890-a4cc-980f4676197b
ireland_covid_county <- read.csv("datasets/Covid19CountyStatisticsHPSCIreland.csv" )
ireland_covid_county