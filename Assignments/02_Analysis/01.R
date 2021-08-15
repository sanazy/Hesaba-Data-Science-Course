# Package Installations
packages = c("scales", "readr", "plotly", "editrules", "stringr", "jsonlite",
             "devtools", "tidyr", "data.table", "rjson", "scales", "dplyr", 
             "gridExtra", "shiny", "ggplot2", "lubridate")
x = sapply(packages, function(x) if (!require(x, character.only = T)) 
  
install.packages(x))
devtools::install_github('jalapic/engsoccerdata')

# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)

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
  
  df_champs <- bind_rows(df3[1,], df_champs)
}

# Top Teams with most championships
df_top <- df_champs %>%
  group_by(team) %>%
  mutate(num = 1) %>%
  summarise(num = sum(num)) %>%
  arrange(desc(num))

# Barplot
ggplot(df_top, aes(reorder(team, -num), num, fill=team)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x='Name of Teams', y='Number of Championships')

names <- c("Team Name", "No. Championships")
colnames(df_top) <- names
View(df_top)

ans_1 <- df_top
save(ans_1, file = "2740430299_01.RData")
load("2740430299_01.RData")
