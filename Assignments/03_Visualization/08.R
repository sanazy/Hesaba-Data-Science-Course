# load libraries
#install.packages('ggpubr') 
library(ggpubr)
library(ggplot2)
library(tidyr)
library(dplyr)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

df <- data %>%
  drop_na(battery_mah, weight) %>%
  summarise(battery_mah, weight)
View(df)

##########################    Graphs     ############################  
ggplot(data = df, aes(x = battery_mah, y = weight)) + 
  geom_point() + 
  labs(x='battery_mah', y='weight') + 
  geom_smooth(method = lm, formula = y ~ x) +
  stat_cor(method = "pearson")
