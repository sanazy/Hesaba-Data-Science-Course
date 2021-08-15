# load libraries
library(tidyverse)
library(dplyr)
 
# Load csv
df <- read.csv(file = 'data/murder_suicide.csv')

# Check how many unique values are in each column 
rapply(df, function(x) length(unique(x)))

# Drop constant columns
drops <- c('AgeSubstitutionFlag', 'CurrentDataYear')
df <- df[ , !(names(df) %in% drops)]

# Check type of each column
str(df)

# Change character columns into factor
df$Sex                 <- as.numeric(factor(df$Sex))
df$Autopsy             <- as.numeric(factor(df$Autopsy))
df$MaritalStatus       <- as.numeric(factor(df$MaritalStatus))
df$InjuryAtWork        <- as.numeric(factor(df$InjuryAtWork))
df$MethodOfDisposition <- as.numeric(factor(df$MethodOfDisposition))

# Adapt and Replace Education1989Revision column with Education2003Revision
from_edu = c(0,1,2,3,4,5,6,7,8,
             9,10,11,12,
             13,14,15,16,
             17,99) 
to_edu = c(1,1,1,1,1,1,1,1,1,
           2,2,2,2,
           3,4,5,6,
           7,9)
df$Education1989Revision <- mapvalues(df$Education1989Revision, 
                                      from=from_edu, 
                                      to=to_edu)

# Substitute sum of 2 education column as new Education
df <- df %>%
  mutate(Education = Education2003Revision + Education1989Revision)


# Keep only these columns
cols <- c('ResidentStatus','Education','MonthOfDeath','Sex','Age',
          'MaritalStatus','DayOfWeekOfDeath','MethodOfDisposition',
          'Autopsy','InjuryAtWork','Race','MannerOfDeath')
df <- df[ , (names(df) %in% cols)]

# Change into MannerOfDeath column 
df$MannerOfDeath <- df$MannerOfDeath-2
df$MannerOfDeath = as.character(df$MannerOfDeath)

# Find number of suicide(0) and homicide(1) in data
df %>% 
  group_by(MannerOfDeath) %>% 
  summarise(MannerOfDeath = count(MannerOfDeath))

# Summary of each column 
summary(df)

# Count rows with non-sense column values
nrow(subset(df, Age==999))
nrow(subset(df, Education==99))
nrow(subset(df, DayOfWeekOfDeath==9))

# Drop rows with non-sense column values
df <- subset(df, DayOfWeekOfDeath!=9 & Education!=99 & Age!=999)

# Head of dataframe
head(df)

# Write into CSV
write.csv(df, file = 'data/data.csv', row.names=FALSE)

