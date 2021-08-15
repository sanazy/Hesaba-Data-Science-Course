# load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(data.table) 

##########################   Dataframes   ############################  
# load data
data <- readRDS('phone_data.rds')

# determine month names
months = c('January', 'February', 'March', 'April', 
           'May', 'June', 'July', 'August', 'September', 
           'October', 'November', 'December')
month_match <- paste(months, collapse = "|")

# Extract month name from announced column and make a new column as date
data1 <- as_tibble(as.data.frame(data[data$announced %like% month_match, ]))

# dataframe with extracted date
df <- data1 %>%
  drop_na(announced) %>%
  mutate(month_name = str_extract(announced, month_match),
         month = match(month_name, month.name),
         date = make_date(year, month))

# length, width, thickness over time dataframe 
df1 <- df %>%
  group_by(date) %>%
  drop_na(date, dim_length, dim_breadth, dim_thickness, cam_px) %>%
  summarise(dim_length = mean(dim_length), 
            dim_breadth = mean(dim_breadth), 
            dim_thickness = mean(dim_thickness), 
            cam_px = mean(cam_px)) %>%
  arrange(date)
View(df1)

##########################    Graphs     ############################  
g1 <- ggplot(data = df1, aes(x = date, y = dim_length)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) 

g2 <- ggplot(data = df1, aes(x = date, y = dim_breadth)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

g3 <- ggplot(data = df1, aes(x = date, y = dim_thickness)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

g4 <- ggplot(data = df1, aes(x = date, y = cam_px)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

grid.arrange(g1, g2, g3, g4,
             ncol = 2, nrow = 2)