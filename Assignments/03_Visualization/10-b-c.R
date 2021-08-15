# load libraries
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(data.table) 

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

# power of all phones
df <- data %>%
  drop_na(display_size, sim_no, battery_mah, bt_v, wlan, year) %>%
  mutate(power = battery_mah * bt_v) 

df1 <- df %>%  
  group_by(sim_no, wlan) %>%
  summarise(mean_power = mean(power))

df2 <- df %>%
  summarise(year, power, display_size)

##########################   Plots   ############################
g1 <- ggplot(data = df1, aes(x = sim_no, y = mean_power, fill = wlan)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x='Number of Simcards', y='Mean Power (mWh)') + 
  geom_text(aes(label = round(mean_power,1)),
            position = position_dodge(width = 1))
  
g2 <- ggplot(data = df2, aes(x = display_size, y = power, color = year)) + 
  geom_point() +
  labs(x='Display Size', y='Power (mWh)') + 
  scale_color_gradient(low = 'red', high = 'blue')

grid.arrange(g1, g2,
             ncol = 2, nrow = 1)