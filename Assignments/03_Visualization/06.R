# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

# extract tenderizerness of all phones
df <- data %>%
  drop_na(weight, dim_length, dim_breadth, dim_thickness) %>%
  mutate(tenderizer = (weight * dim_thickness) / (dim_length * dim_breadth)) %>%
  summarise(device, tenderizer) %>%
  arrange(desc(tenderizer)) %>%
  slice(1:10)
View(df)

# bar plot
ggplot(df, aes(reorder(device, -tenderizer), tenderizer)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x='Name of Phones', y='Tenderizerness')