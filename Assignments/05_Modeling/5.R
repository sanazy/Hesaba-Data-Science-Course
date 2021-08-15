#install.packages('h2o')

# Load libraries
library(h2o)
library(ggplot2)
library(ROCit)
library(tidyverse)

# Initialize H2O
h2o.init()

# Import data
df = h2o.importFile(path = 'data/data.csv',
                    destination_frame = 'df')

# Split data with a ratio of 0.8
df.split <- h2o.splitFrame(data = df, 
                           ratios = 0.8, 
                           seed = 1)

# Separate test and train
df.train <- df.split[[1]]
df.test <- df.split[[2]]

# Define x and y
x = setdiff(colnames(df), 'MannerOfDeath')
y = 'MannerOfDeath'

# Train model
df.glm <- h2o.glm(x = x, # Vector of predictor variable names
                 y = y, # Name of response/dependent variable
                 training_frame = df.train, # Training data
                 seed = 1234,        # Seed for random numbers
                 family = "binomial",   # Outcome variable
                 lambda_search = TRUE,  # Optimum regularization lambda
                 alpha = 0.5,           # Elastic net regularization
                 nfolds = 5             # N-fold cross validation
)

# Predict labels 
df.fit = h2o.predict( object = df.glm, newdata = df.test)
  
# bind labels and output of model
bind <- as.data.frame(h2o.cbind(df.test$MannerOfDeath, df.fit$p1))

# extract class and score
class <- bind$MannerOfDeath
score <- bind$p1

# calculate accuracy and cutoff
measure <- measureit(score = score, class = class)

measure
######################     Plot    ######################     
# Make a tibble out of parameters
df1 <- tibble(
  Accuracy = measure$ACC,
  Cutoff   = measure$Cutoff,
)

# Plot Accuracy vs Cutoff
ggplot(df1, aes(x = Cutoff, y = Accuracy)) +
  geom_point(colour = "aquamarine3", size = 1) + 
  geom_label(data = df1 %>% filter(Accuracy == max(Accuracy)),
             aes(label = round(Cutoff,3)),
             vjust = 1) 




