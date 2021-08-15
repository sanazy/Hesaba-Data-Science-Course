# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load Spain dataset
fdb = as_tibble(spain)

# Create empty vectors
num_l <- vector()
num_d <- vector()
teams_l <- vector()
teams_d <- vector()
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
    mutate(D  = ifelse(GF == GA, 1, 0),
           L  = ifelse(GF < GA, 1, 0)) %>%
    summarise(D = max(rle(D)$lengths[rle(D)$values == 1]),
              L = max(rle(L)$lengths[rle(L)$values == 1]))

  df4 <- df3 %>%
    arrange(desc(D))

  df5 <- df3 %>%
    arrange(desc(L))

  num_d <- c(num_d, df4$D[1])
  teams_d <- c(teams_d, df4$team[1])
  
  num_l <- c(num_l, df5$L[1])
  teams_l <- c(teams_l, df5$team[1])
  
  seasons <- c(seasons, season)
}

df_d <- data.frame(seasons, teams_d, num_d) %>%
  arrange(desc(num_d)) %>%
  filter(num_d == max(num_d))
name_d <- c("Season", "Team", "No. Seq Draw")
colnames(df_d) <- name_d
View(df_d)

df_l <- data.frame(seasons, teams_l, num_l) %>%
  arrange(desc(num_l)) %>%
  filter(num_l == max(num_l))
name_l <- c("Season", "Team", "No. Seq Lost")
colnames(df_l) <- name_l
View(df_l)

ans_1 <- df_d
ans_2 <- df_l
save(ans_1, ans_2, file = "2740430299_06.RData")
load("2740430299_06.RData")