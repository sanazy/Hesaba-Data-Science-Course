# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

# density of water
water_den = 997

# Extract density of all phones
df1 <- data %>%
  drop_na(weight, dim_length, dim_breadth, dim_thickness) %>%
  mutate(density = (weight*10^6)/(dim_length*dim_breadth*dim_thickness))

# find phones with density less than water
df2 <- df1 %>%  
  filter(density < water_den) # desity of water
View(df2)

##########################    Graphs     ############################  
ggplot(df1, aes(x = density)) +
  geom_histogram(bins = 30, color = 'black', 
                 fill = 'lightblue', linetype = 'dashed') +
  geom_vline(xintercept = water_den, 
             color = "red", size = 1) +
  annotate(geom = "text", x = 700, y = 550, size = 5,
           label = 'Water Density', color = 'red')
