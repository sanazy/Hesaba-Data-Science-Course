# load libraries
library(engsoccerdata)
library(tidyr)
library(dplyr)
library(lubridate)

# Load Spain dataset
fdb = as_tibble(spain)

df <- spain %>%
  mutate(day = day(Date),
         weekday = weekdays(Date), # new column with name of weekdays
         hloose = ifelse(hgoal < vgoal, 1, 0)) %>%
  filter(day == 13, 
         weekday == 'Friday', 
         hloose == 1)

# View Result
View(df)

ans_1 <- df
save(ans_1, file = "2740430299_10.RData")
load("2740430299_10.RData")
