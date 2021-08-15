# load libraries
library(tidyr)
library(dplyr)
library(data.table) 

# load data
data <- readRDS('mobile_data.rds')

# determine not-phones features
no_phone = c('iPad', 'Tab', 'tab', 'Watch', 'watch')
no_phone_match <- paste(no_phone, collapse = "|")

# dataframe with extracted phones from device column
data1 <- as_tibble(as.data.frame(data[!(data$device %like% no_phone_match), ]))

# dataframe with excluded remaining watches and tablets
df <- data1 %>%
  drop_na(display_size, sim_no) %>%
  filter(display_size < 7, sim_no >= 1) 

# save data of all phones
saveRDS(df, 'phone_data.rds')