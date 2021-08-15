# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

df <- data %>%
  drop_na(year, dim_thickness, audio_jack) %>%
  filter(year == 2017) %>%
  summarise(device, dim_thickness, audio_jack)
View(df)

##########################    Graphs     ############################  
ggplot(data = df, aes(x = audio_jack, y = dim_thickness)) + 
  geom_boxplot(aes(colour = audio_jack)) 
