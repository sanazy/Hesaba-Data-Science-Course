library(engsoccerdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# Load Spain dataset
fdb = as_tibble(spain)

# Filter rows with 2 differnece in half time goals for hosts
df1 <- spain %>% 
  separate(HT, c('h_hg', 'h_vg'), sep='-', convert = T) %>%
  filter(h_hg - h_vg == 2) 
  
# Filter those with 2 dif in half time and win at the end for hosts 
df11 <- filter(df1, hgoal > vgoal)

# Filter rows with 2 differnece in half time goals for guests
df2 <- spain %>% 
  separate(HT, c('h_hg', 'h_vg'), sep='-', convert = T) %>%
  filter(h_vg - h_hg == 2) 

# Filter those with 2 dif in half time and win at the end for guests
df22 <- filter(df2, hgoal < vgoal) 

# Average probabileties for winnig the game when there is 2 diff in goals at half time
a <- nrow(df11) / nrow(df1)
b <- nrow(df22) / nrow(df2)
prob <- ((a + b) / 2) * 100

ans_1 <- paste('Probability of winning: ', prob, '%')
save(ans_1, file = "2740430299_08.RData")
load("2740430299_08.RData")

