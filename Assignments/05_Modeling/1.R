#install.packages('lsr')
#install.packages('ggcorrplot')
#install.packages('psych')

# load libraries
library(psych)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(lsr)
library(dplyr)

# load csv
df <- read.csv(file = 'data/data.csv')

#---------------    Correlation plot      --------------
# Correlation Matrix
M <- cor(df)

# Correlation plot
ggcorrplot(M)

#---------------    Pair plot      --------------
# Keep only these columns
cols <- c('ResidentStatus','Education','Age',
          'MaritalStatus','MethodOfDisposition',
          'MannerOfDeath')
df <- df[ , (names(df) %in% cols)]

# Pairplot
pairs(df)

pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = FALSE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
