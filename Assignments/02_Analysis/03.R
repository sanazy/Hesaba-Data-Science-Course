# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load Spain dataset
fdb = as_tibble(spain)

# create empty dataframes
df_hchamps <- data.frame()
df_fchamps <- data.frame()

# Exclude 3 seasons due to lack of information 
min_season <- min(fdb$Season)
max_season <- max(fdb$Season)
exclude <- 1936:1938 # exclude these seasons
all_seasons <- min_season: max_season
condition <- all_seasons[!all_seasons %in% exclude]

# Find Champions for each Season
for (season in c(min_season:max_season)){
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
  
  # Bind 2 dataframes for Half Season Championship
  half_Season = nrow(df1) / 2
  df3 <- bind_rows(df1[1:half_Season,], 
                   df2[1:half_Season,]) %>%
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
  
  # Append data to new dataframe
  df_hchamps <- bind_rows(df3[1,], df_hchamps)
  
  # Bind 2 dataframes for Full Season Championship
  df4 <- bind_rows(df1, df2) %>%
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
  
  # Append data to new dataframe
  df_fchamps <- bind_rows(df4[1,], df_fchamps)
}

# Remove Nan columns
df_hchamps <- df_hchamps %>% 
  drop_na(team)

df_fchamps <- df_fchamps %>% 
  drop_na(team)

# View results
View(df_hchamps)
View(df_fchamps)

# Length of all same full and half champions
nom <- nrow(df_fchamps) - length(which(df_hchamps$team != df_fchamps$team))
denom <- nrow(df_fchamps)

# Calculate Percentage
ans_1 <- paste('Percentage of same Half and Full Season Champions: ',
               (nom/denom) * 100,
               '%')

save(ans_1, file = "2740430299_03.RData")
load("2740430299_03.RData")
