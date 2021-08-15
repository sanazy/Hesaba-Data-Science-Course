# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

# mean price of each no. simcards
df <- data %>%
  drop_na(sim_no, LTE, price) %>%
  group_by(sim_no, LTE) %>%
  summarise(mean_price = mean(price))
View(df)

##########################    Graphs     ############################  
# bar graph
ggplot(df, aes(x = sim_no, y = mean_price, fill = LTE)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x='Number of Simcards', y='Mean Price') + 
  geom_text(aes(label = round(mean_price, 1)),
            position = position_dodge(width = 1))
