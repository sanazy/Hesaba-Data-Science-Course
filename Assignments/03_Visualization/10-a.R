# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

df <- data %>%
  drop_na(company, price) %>%
  group_by(company) %>%
  summarise(price = mean(price)) %>%
  arrange(desc(price)) %>%
  slice(1:10)
View(df)

##########################    Graphs     ############################  
ggplot(df, aes(x = '', y = -price,
               fill = reorder(company, -price))) + 
  geom_bar(width = 1, stat = 'identity', color='black') +
  coord_polar('y', start=0) +
  labs(title = 'Top 10 Companies with Highest Mean Price for Phones') +
  theme_void()

