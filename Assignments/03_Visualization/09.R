# load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# load data
data <- readRDS('phone_data.rds')

flagships = c('iPhone', 'iPhone 3G', 'iPhone 3GS', 'iPhone 4', 'iPhone 4s',
              'iPhone 5', 'iPhone 5s', 'iPhone 6 Plus', 'iPhone 6s Plus',
              'iPhone 7 Plus', 'iPhone X')

# Apple flagship phones
df <- data %>%
  subset(device %in% flagships) %>%
  summarise(device, price, year)
View(df)

# graph
ggplot(data = df, aes(x = device, y = price, fill = year)) + 
  theme(axis.text.x = element_text(angle = 45)) +
  geom_tile()







