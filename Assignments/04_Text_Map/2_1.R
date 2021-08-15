#install.packages('ggmap')
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(tidytext)
library(ggmap)
library(ggplot2)
library(lubridate)

###############################################################
# load csv, extract time and sort by date
equake <- read.csv('data/iran.csv') %>%
  mutate(date = as.Date(time),
         t = ymd_hms(time), 
         time2 = format(t, format = "%H:%M:%S")) %>%
  select(date, time2, latitude, longitude, mag) %>%
  arrange(date)
head(equake)

# indices of large equakes
large_inds <- which(equake$mag > 7)

# empty dataframe
large_quakes = data.frame()

# keep 4 rows data before each large equakes
for (ind in large_inds){
  prev = ind - 4
  large_quakes <- rbind(large_quakes, equake[prev:ind,])
}
  
###############################################################
# Iran lon-lat boundries
iran_bb <- c(left = 44.1092252948,
             bottom = 25.0782370061,
             right = 63.3166317076,
             top = 39.7130026312)

# get Iran map
Map <- get_map(iran_bb, zoom = 5) 

# show large equakes and precursors in Iran map
ggmap(Map) +
  geom_point(data = large_quakes, 
             aes(x = longitude, y = latitude, color = mag, size = mag),
             alpha = 0.7) +
  scale_color_distiller(palette = "Spectral")
