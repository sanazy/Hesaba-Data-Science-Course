# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)

############### team with most wins from out of home games ##################

# Load Spain dataset
fdb = as_tibble(spain)

# create empty dataframe
df_champs <- data.frame()

# Exclude 3 seasons due to lack of information 
min_season <- min(fdb$Season)
max_season <- max(fdb$Season)
exclude <- 1936:1938 # exclude these seasons
all_seasons <- min_season: max_season
condition <- all_seasons[!all_seasons %in% exclude]

# Find Champions for each Season
for (season in condition){
  
  df1 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(team = visitor, 
              GF  = vgoal, 
              GA  = hgoal) 
  
  df3 <- df1 %>%
    group_by(team) %>%
    mutate(MP = 1,
           W  = ifelse(GF > GA, 1, 0),
           D  = ifelse(GF == GA, 1, 0),
           L  = ifelse(GF < GA, 1, 0),
           GD = GF - GA,
           Pts = W*3 + D*1) %>%
    summarise(Season = season,
              MP = sum(MP),
              W  = sum(W),
              D  = sum(D),
              L  = sum(L),
              F = sum(GF),
              A = sum(GA),
              G = sum(GD),
              P = sum(Pts)) %>%
    arrange(desc(W))
  
  df_champs <- bind_rows(df3[1,], df_champs)
}

df_top <- df_champs %>%
  arrange(desc(P))

View(df_top[1,])

ans_1 <- df_top[1,]
save(ans_1, file = "2740430299_11-c.RData")
load("2740430299_11-c.RData")

