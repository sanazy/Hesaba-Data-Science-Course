# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load Spain dataset
fdb = as_tibble(spain)

# set specific season
season = 2012

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

View(df3)

ans_1 <- df3
save(ans_1, file = "2740430299_09.RData")
load("2740430299_09.RData")