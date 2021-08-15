# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load Spain dataset
fdb = as_tibble(spain)

# Select range of Seasons
min_season <- 2001
max_season <- 2010

# Black Cats 
black_cats <- vector()

# Good teams
team1s = c('Real Madrid', 'FC Barcelona', 
           'Athletic Bilbao', 'Athletic Madrid',
           'Valencia CF')

# Find Champions for each Season
for (season in c(min_season:max_season)){
  # Seperate hosts
  df1 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(team1 = home, 
              team2 = visitor,
              GF    = hgoal, 
              GA    = vgoal) 
  
  # Seperate guests
  df2 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(team1 = visitor, 
              team2 = home, 
              GF  = vgoal, 
              GA  = hgoal) 
  
  # Bind 2 dataframes
  df3 <- bind_rows(df1, df2) %>%
    filter(team1 %in% team1s, 
           !team2 %in% team1s,
           GF < GA)

  black_cats <- union(df3$team2, black_cats)
}

Black_Cats <- data.frame(black_cats)
View(Black_Cats)

ans_1 <- Black_Cats
save(ans_1, file = "2740430299_04.RData")
load("2740430299_04.RData")

