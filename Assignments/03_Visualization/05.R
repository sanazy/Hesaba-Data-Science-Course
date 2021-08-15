# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

df <- data %>%
  drop_na(display_size, px_row, px_col) %>%
  mutate(ppi = round(sqrt(px_row^2 + px_col^2) / display_size))

df1 <- df %>%
  drop_na(year) %>%
  group_by(year) %>%
  summarise(mean_ppi = mean(ppi))
View(df1)

df2 <- df %>%
  filter(ppi == max(ppi)) %>%
  summarise(company, device, ppi)
View(df2)

##########################    Graphs     ############################  
# histogram
g1 <- ggplot(data = df, aes(x = ppi)) + 
  labs(x = 'PPI') +
  geom_histogram(aes(y = ..density..), bins = 30,
                 color = 'black', fill = 'lightblue', linetype = 'dashed') +
  geom_density(alpha = 0.2, fill = '#FF6666') 

# scatter
g2 <- ggplot(data = df1, aes(x = year, y = mean_ppi)) + 
  labs(x = 'Year', y = 'Mean PPI') +
  geom_line(linetype = 'dashed', color = 'lightblue', size = 1.2) +
  geom_point(color = '#FF6666', size = 3)

# grid
grid.arrange(g1, g2, ncol = 2, nrow = 1)