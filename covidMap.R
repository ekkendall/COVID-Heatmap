
library(dplyr)
library(leaflet)
library(tigris)


##add state data
states <- states(cb=T)
states %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)

##add county data
countyData <-counties(cb=T)
countyData %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)

##combine state data so state names are present
stateInfo <- data.frame(states@data[["STATEFP"]], states@data[["NAME"]])
colnames(stateInfo) <- c("STATEFP", "state")


##input data from New York TImes
nytDataFull <- read.csv(
  url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"),
  stringsAsFactors = F)


##Data from unknown counties was removed
nytData <- na.omit(nytDataFull)

##find latest date
minDate <- min(nytData$date)
maxDate <- max(nytData$date)

##New york city combines 5 counties so need to add them back to map
newyorkcity <- nytDataFull %>% filter(county == "New York City") %>% filter(date == maxDate)
newyorkcity$fips[is.na(newyorkcity$fips)] <- "36061"
newyorkC <- newyorkcity

newyorkcity <- nytDataFull %>% filter(county == "New York City") %>% filter(date == maxDate)
newyorkcity$fips[is.na(newyorkcity$fips)] <- "36047"
kingsC <- newyorkcity

newyorkcity <- nytDataFull %>% filter(county == "New York City") %>% filter(date == maxDate)
newyorkcity$fips[is.na(newyorkcity$fips)] <- "36081"
queensC <- newyorkcity

newyorkcity <- nytDataFull %>% filter(county == "New York City") %>% filter(date == maxDate)
newyorkcity$fips[is.na(newyorkcity$fips)] <- "36005"
bronxC <- newyorkcity

newyorkcity <- nytDataFull %>% filter(county == "New York City") %>% filter(date == maxDate)
newyorkcity$fips[is.na(newyorkcity$fips)] <- "36085"
richmondC <- newyorkcity



dataToMap <- nytData %>% filter(date == maxDate)
dataToMap <- rbind(dataToMap, newyorkC)
dataToMap <- rbind(dataToMap, kingsC)
dataToMap <- rbind(dataToMap, queensC)
dataToMap <- rbind(dataToMap, bronxC)
dataToMap <- rbind(dataToMap, richmondC)

dataWithState <- merge.data.frame(dataToMap, stateInfo, by="state")
colnames(dataWithState) <- c("state",
                             "date",
                             "NAME",
                             "GEOID",
                             "cases",
                             "deaths",
                             "STATEFP")

##need to get rid of leading zero on fips code and then merge
countyData@data[["GEOID"]] <- as.numeric(countyData@data[["GEOID"]])
fullMapData <- geo_join(countyData, dataWithState, by="GEOID")

##transform data to log scale for heatmap
logCases <- log(fullMapData$cases)
logDeaths <- log(fullMapData$deaths)
logDeaths[logDeaths == "-Inf"] <- NA


fullMapData$cases[is.na(fullMapData$cases)] <- "0"
fullMapData$deaths[is.na(fullMapData$deaths)] <- "0"

##set heatmap scale
pal1 <- colorNumeric("Reds", domain=logDeaths, na.color = "white")
pal3 <- colorNumeric("Reds", domain=logCases, na.color = "white")

##add pop up labels for county
popup_Cases <- paste0("<b>", fullMapData$NAME, "</b>", "<br>",
                      "Number of cases: ", as.character(fullMapData$cases),"<br>",
                      "Number of deaths: ", as.character(fullMapData$deaths),"<br>",
                      "<i>", "<small>", "Last updated ", as.character(maxDate), "</small>", "</i>"
)

popup_Deaths <- paste0("<b>", fullMapData$NAME, "</b>", "<br>",
                       "Number of cases: ", as.character(fullMapData$cases),"<br>",
                       "Number of deaths: ", as.character(fullMapData$deaths),"<br>",
                       "<i>", "<small>", "Last updated ", as.character(maxDate), "</small>", "</i>"
)


##make map!!
map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  
##state lines
  addPolygons(data=states,
              fillColor = "white",
              color = "grey",
              opacity = 1,
              weight = 0.5
              ) %>% 
##add cases
  addPolygons(data = fullMapData , 
              fillColor = ~pal3(logCases), 
              fillOpacity = 0.75, 
              weight = 0.2, 
              smoothFactor = 0.2,
              popup = ~popup_Cases,
              opacity = 0.3,
              color = "grey",
              dashArray = "3",
              highlight = highlightOptions(weight = 5,
                                           color = "gray",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              group = "COVID-19 Cases") %>%

##add deaths
addPolygons(data = fullMapData, 
            fillColor = ~pal1(logDeaths), 
            fillOpacity = 0.75, 
            weight = 0.2, 
            smoothFactor = 0.2,
            opacity = 0.3,
            color = "gray",
            dashArray = "3",
            popup = ~popup_Deaths,
            highlight = highlightOptions(weight = 5,
                                         color = "gray",
                                         dashArray = "",
                                         fillOpacity = 0.7,
                                         bringToFront = TRUE),
            group = "COVID-19 Deaths") %>%

  
  addLayersControl(baseGroups = c("COVID-19 Cases", "COVID-19 Deaths"),
                   options = layersControlOptions(collapsed = F))
  


  
  


