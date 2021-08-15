# Load libraries
library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

#############   teams with most consecutive games without lost ##################

# Load Spain dataset
fdb = as_tibble(spain)

# Create empty vectors
num_nl <- vector()
teams_nl <- vector()
  
# Seperate hosts
df1 <- spain %>%
  filter(tier == 1) %>%
  summarise(team = home, 
            GF  = hgoal, 
            GA  = vgoal) 

# Seperate guests
df2 <- spain %>%
  filter(tier == 1) %>%
  summarise(team = visitor, 
            GF  = vgoal, 
            GA  = hgoal) 

# Bind 2 dataframes
df3 <- bind_rows(df1, df2) %>%
  group_by(team) %>%
  mutate(NL  = ifelse(GF >= GA, 1, 0)) %>%
  summarise(NL = max(rle(NL)$lengths[rle(NL)$values == 1])) %>%
  arrange(desc(NL))

View(df3)

ans_1 <- df3
save(ans_1, file = "2740430299_11-a.RData")
load("2740430299_11-a.RData")

# Barplot
ggplot(df3[1:5,], aes(reorder(team, -NL), NL, fill=team)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x='Name of Teams', y='Number of Consecutive not Loosing Games') +
  ggtitle("Top 5 Teams with most Consecutive not Loosing Games")


