# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load Spain dataset
fdb = as_tibble(spain)

############################      Part B      #############################

# create empty vectors
diffs <- vector()
champs <- vector()
seasons <- vector()

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
    summarise(team = home, 
              GF  = hgoal, 
              GA  = vgoal) 
  
  # Seperate guests
  df2 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(team = visitor, 
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
    arrange(desc(P))
  
  champ <- as.character(df3[1,1])
  # difference of points between first team and second team in table
  diff <- df3$P[1] - df3$P[2]  
  
  champs <- c(champs, champ) # save name of champions
  diffs  <- c(diffs, diff)
  seasons <- c(seasons, season)
}

# create a dataframe from all outputs
df <- data.frame(champs, seasons, diffs) %>%
  arrange(desc(diffs))
View(df)

ans_1 <- paste('Powerful championship: ', df$champs[1])
save(ans_1, file = "2740430299_05-b.RData")
load("2740430299_05-b.RData")
