# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load Spain dataset
fdb = as_tibble(spain)

# create epty dataframes
df_tops <- data.frame()
df_bots <- data.frame()
df_seasons <- data.frame()

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
              GF   = hgoal) 
  
  # Seperate guests
  df2 <- spain %>%
    filter(tier == 1,
           Season == season) %>%
    summarise(team = visitor, 
              GF   = vgoal) 
  
  # Bind 2 dataframes for finding boring and intersting teams
  df3 <- bind_rows(df1, df2) %>%
    group_by(team) %>%
    summarise(GF = sum(GF)) %>%
    arrange(desc(GF))
  
  # Append top and bottom team into new dataframes
  df_tops <- bind_rows(head(df3, n=1), df_tops)
  df_bots <- bind_rows(tail(df3, n=1), df_bots)
  
  # Append Number of goals in each season into new dataframe
  df4 <- df3 %>%
    summarise(Season = season,
              GF = sum(GF))
  
  df_seasons <- bind_rows(df4, df_seasons)
}

# Find Exciting Team by number of goals
exciting_teams <- df_tops %>%
  group_by(team) %>%
  summarise(GF = sum(GF)) %>%
  arrange(desc(GF))

# Find Boring Team by number of goals
boring_teams <- df_bots %>%
  group_by(team) %>%
  summarise(GF = sum(GF)) %>%
  arrange(GF)

# Find Exciting season with most number of goals
exciting_seasons <- df_seasons %>%
  arrange(desc(GF))

# Find Boring season with least number of goals
boring_seasons <- df_seasons %>%
  filter(GF > 0) %>%
  arrange(GF)

########################     Dataframes      #####################
View(exciting_teams)
View(boring_teams)
View(exciting_seasons)
View(boring_seasons)

########################     Outputs       ########################
ans_1 <- paste('Most Exciting Team: ', exciting_teams[1,1])
ans_2 <- paste('Most Boring Team: ', boring_teams[1,1])
ans_3 <- paste('Most Exciting Season: ', exciting_seasons[1,1])
ans_4 <- paste('Most Boring Season: ', boring_seasons[1,1])

save(ans_1, ans_2, ans_3, ans_4, file = "2740430299_02.RData")
load("2740430299_02.RData")

########################     Plots       #########################
# Barplot of boring teams
ggplot(boring_teams[1:10,], 
       aes(x = reorder(as.character(team), GF), 
           y = GF)) + 
  geom_bar(stat = "identity", 
           fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = 'Boring Teams', 
       y = 'Number of Goals') +
  geom_text(aes(label = GF), vjust = 1.6, color = "white", size = 3.5) 

# New window
dev.new()

# Barplot of boring seasons
ggplot(boring_seasons[1:10,], 
       aes(x = reorder(as.character(Season),GF), 
           y = GF)) + 
  geom_bar(stat = "identity", 
           fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = 'Boring Seasons', 
       y = 'Number of Goals') +
  geom_text(aes(label = GF), vjust = 1.6, color = "white", size = 3.5) 
