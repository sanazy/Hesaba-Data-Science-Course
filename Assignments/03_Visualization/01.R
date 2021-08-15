# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

##########################   Dataframes   ############################
# load data
data <- readRDS('phone_data.rds')

# extract top 20 companies with most manufactured phones
df <- data %>%
  drop_na(company) %>%
  group_by(company) %>%
  mutate(num = 1) %>%
  summarise(num = sum(num)) %>%
  arrange(desc(num)) %>%
  slice(1:20)
View(df)

##########################    Graphs     ############################
ggplot(df, aes(reorder(company, -num), num, fill=company)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x='Name of Companies', y='Number of Manufactured Phones') +
  geom_text(aes(label = num), vjust = 1.6, color = 'white', size = 3.5)
