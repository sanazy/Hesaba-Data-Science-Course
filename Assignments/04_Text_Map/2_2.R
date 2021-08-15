#install.packages('sf')
#install.packages('plotly')
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(sf)
library(ggmap)
library(plotly)

########################################################################
# load iran equakes
equake <- read.csv("data/iran.csv") %>%
  select(time, latitude, longitude, depth, mag)

# load iran polygons
iran_sf <- readRDS("data/gadm36_IRN_2_sf.rds")

# find city name for each equake
spatialPoints = sf::st_as_sf(equake,
                             coords = c ( "longitude" , "latitude" ),
                             crs= 4326)
geoIndex = sf::st_intersects(spatialPoints, iran_sf)
geoIndex = lapply(geoIndex, 
                  FUN = function(x){ ifelse( is.null(x[ 1 ]), NA ,x[1 ])}) %>%
  unlist()
equake[["city" ]] = iran_sf[["NAME_2"]][geoIndex]

########################################################################
# extract number of equakes in each city of Iran
stat <- equake %>%
  group_by(city) %>%
  dplyr::summarise(count = n())

eqIran = merge(iran_sf, stat, by. = "NAME_2", by.y = "city" , all.x = T)

# print list of cities without any equakes
setdiff(iran_sf[["NAME_2"]], stat$city)

# interactive plot using plotly
p <- ggplot(eqIran) + 
  geom_sf(aes(label1 = NAME_2, fill = count),  color = "white")

ggplotly(p, tooltip = "all") %>%
  highlight("plotly_hover") 
