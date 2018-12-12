# make leaflet map for state level data

# One Leaflet with at least three layers – one of which must be a choropleth. 
# The User must be able to choose which layers to “see” except for the choropleth
# which can be the base layer.

# ref: https://rstudio.github.io/leaflet/choropleths.html

# libraries
library(leaflet)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(sp)
library(geojsonio)

# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# import state level results by candidate
df      <- read.csv('2016-president-by-candidate.csv', stringsAsFactors = FALSE)
df$NAME <- df$state
df$points_diff <- (df$trump_pct - df$clinton_pct) * 100

# import US State map data (https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html)
us.map <- readOGR(dsn='cb_2017_us_state_5m', layer='cb_2017_us_state_5m', stringsAsFactors = FALSE)

# remove outer islands
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
# merge the data
electionmap <- merge(us.map, df, by=c("NAME"))

# merge with state center lat and long (taken from: https://www.kaggle.com/washimahmed/usa-latlong-for-state-abbreviations)
centers <- read.csv('statelatlong.csv', stringsAsFactors = FALSE)
centers$NAME <- centers$City
electionmap  <- merge(electionmap, centers, by=c("NAME"))


###### MAPPING #######
# set up colors for chloropleth
pct.bins <-c(0, 0.35, 0.45, 0.50, 0.55, 0.65, 1.0)
pct.pal  <- c("#5499de","#b2d4e6", "#f9ccb9",
             "#F4A582", "#D6604D", "#B2182B")
pct.pal  <- colorBin(pct.pal, bins=pct.bins)

# format labels
labels <- sprintf("<strong>%s</strong><br/>Trump pct: %s <br/>Clinton pct: %s <br/>Point Differential: %s pts", 
                  electionmap$state, round(electionmap$trump_pct, 2), round(electionmap$clinton_pct, 2),
                  round(electionmap$points_diff,1)
                  ) %>% lapply(htmltools::HTML)

text_for_markers <- paste0("<strong>State: </strong>",
                           electionmap$State,
                           "<br><strong>Total Votes: </strong>",
                           electionmap$total_votes)
# set up plot
n_map <- leaflet(data = electionmap) %>%

addTiles() %>%
addPolygons(fillColor = ~pct.pal(electionmap$trump_pct), weight = 2, opacity = 1, color = "gray",
            dashArray = "3", fillOpacity = 0.75, 
            highlight = highlightOptions(weight = 5, color = "#666",dashArray = "",
                                         fillOpacity = 0.85, bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                         textsize = "15px",direction = "auto")) %>%
addLegend(pal = pct.pal, 
          values = ~electionmap$trump_pct, 
          opacity = 0.7, 
          title = '% of Votes for Trump',
          position = 'bottomright') %>%
  
addPopups(
  lng = -107.2903, lat = 43.0760,
  popup = "WY: Largest Margin for Trump",
  options = popupOptions(closeButton = TRUE))  %>%

  addPopups(
  lng = -77.0369, lat = 38.9072,
  popup = "DC: Largest Margin for Clinton",
  options = popupOptions(closeButton = TRUE)) %>%
  
  addPopups(
    lng = -85.6024, lat = 44.3148,
    popup = "Michigan: Smallest Margin",
    options = popupOptions(closeButton = TRUE)) %>%
 
addMarkers(lat=electionmap$Latitude, lng=electionmap$Longitude,
           popup=text_for_markers, group = "Number of Votes") %>%
  
# add layers control
addLayersControl(overlayGroups = c("Number of Votes"),
    options = layersControlOptions(collapsed = FALSE))

n_map

n_map$
library(htmlwidgets)
saveWidget(n_map, file="leaflet_map.html")


# change to electoral college votes ... 



