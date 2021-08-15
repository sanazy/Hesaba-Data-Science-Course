library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggmap)
library(ggplot2)
library(lubridate)

###############################################################
# load csv and extract quarter of each equakes
equake <- read.csv('data/iran.csv') %>%
  select( time, latitude, longitude, depth, mag ) %>%
  mutate( year = lubridate::year(time),
          quarter = lubridate::quarter(time))

# frequency of equakes in each quarter
freqs <- equake %>%
  dplyr::count(quarter)
View(freqs)

###############################################################
# Iran lon-lat boundries
iran_bb <- c(left = 44.1092252948,
             bottom = 25.0782370061,
             right = 63.3166317076,
             top = 39.7130026312)

# get Iran map
Map <- get_map(iran_bb, zoom = 5) 

# show equakes for each quarter in Iran map
ggmap(Map) +
  geom_point(data = equake, 
             aes(x = longitude, y = latitude, color = mag, size = mag), 
             alpha = 0.7) +
  scale_color_distiller(palette = "Spectral") + 
  facet_wrap(. ~ quarter)
