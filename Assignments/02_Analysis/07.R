# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load Spain dataset
fdb = as_tibble(spain)

# Create empty vectors
counts <- vector()
seasons <- vector()
relegates <- vector()

# Exclude 3 seasons due to lack of information 
min_season <- min(fdb$Season)
max_season <- max(fdb$Season)
exclude <- 1936:1938 # exclude these seasons
all_seasons <- min_season: max_season
condition <- all_seasons[!all_seasons %in% exclude]

# Find Champions for each Season
for (season in condition){
  
  # Seperate hosts
  df1 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(date = Date,
              team = home, 
              GF  = hgoal, 
              GA  = vgoal) 
  
  # Seperate guests
  df2 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(date = Date,
              team = visitor, 
              GF  = vgoal, 
              GA  = hgoal) 
  
  # Bind 2 dataframes
  df3 <- bind_rows(df1, df2) %>%
    group_by(team) %>%
    mutate(MP = 1,
           W  = ifelse(GF > GA, 1, 0),
           D  = ifelse(GF == GA, 1, 0),
           L  = ifelse(GF < GA, 1, 0),
           GD = GF - GA,
           Pts = W*3 + D*1) %>%
    summarise(MP = sum(MP),
              W  = sum(W),
              D  = sum(D),
              L  = sum(L),
              F = sum(GF),
              A = sum(GA),
              G = sum(GD),
              P = sum(Pts)) %>%
    arrange(P)

  # Name of season champion 
  relegate <- as.character(df3[1,1])
  
  start <- head(df1$date, n=1) # start date of season
  end   <- tail(df1$date, n=1) # end date of season
  # range dates from start to end by each week
  range <- seq(from=ymd(start), to=ymd(end), by='7 days') 
  
  # set number of sequentional champions in weeks
  counter <- 0
  
  # iterate over weeks to detect sequential championships
  for (i in c(2:length(range))){
    df4 <- bind_rows(df1, df2) %>%
      filter(between(date, start, range[i])) %>%
      group_by(team) %>%
      mutate(MP = 1,
             W  = ifelse(GF > GA, 1, 0),
             D  = ifelse(GF == GA, 1, 0),
             Pts = W*3 + D*1) %>%
      summarise(MP = sum(MP),
                P = sum(Pts)) %>%
      arrange(P)
    
    if (df4$team[1] == relegate){
      counter <- counter + 1
    }
    else {
      counter <- 0
    }
  }
  
  relegates <- c(relegates, relegate) # save name of champions
  counts <- c(counts, counter) # number of sequential weeks of championships
  seasons <- c(seasons, season) # season
}

# create a dataframe from all outputs
df <- data.frame(relegates, seasons, counts) %>%
  arrange(desc(counts))
View(df)

ans_1 <- paste('Fastest Relegation from League: ', df$relegates[1])
save(ans_1, file = "2740430299_07.RData")
load("2740430299_07.RData")
