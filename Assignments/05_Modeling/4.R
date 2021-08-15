#install.packages('h2o')
#install.packages('cvms')
#install.packages('precrec')

# Load libraries
library(h2o)
library(cvms)
library(tibble)
library(broom)
library(precrec)
library(ggplot2)

# Inititialize H2O
h2o.init()

# Import data
df = h2o.importFile(path = 'data/data.csv',
                    destination_frame = 'df')

# Split data with a ratio of 0.8
df.split <- h2o.splitFrame(data = df, 
                           ratios = 0.8, 
                           seed = 1)

# Create training set from 1st data set in split
df.train <- df.split[[1]]

# Create testing set from 2st data set in split
df.test <- df.split[[2]]

# Define x and y for separate column 
x = setdiff(colnames(df), 'MannerOfDeath')
y = 'MannerOfDeath'

# Train a model
model <- h2o.glm(x = x, # Vector of predictor variable names
                 y = y, # Name of response/dependent variable
                 training_frame = df.train, # Training data
                 seed = 1234,        # Seed for random numbers
                 family = "binomial",   # Outcome variable
                 lambda_search = TRUE,  # Optimum regularization lambda
                 alpha = 0.5,           # Elastic net regularization
                 nfolds = 5             # N-fold cross validation
)

# Prediction on test data
pred = h2o.predict(object = model, newdata = df.test)

##################      Plots       #####################
#-------------     Confusion Matrix   --------------------------
bind1 <- as.data.frame(h2o.cbind(df.test$MannerOfDeath, 
                                 pred$predict))
table <- table(bind1)
cfm   <- tidy(table)

plot_confusion_matrix(cfm,
                      targets_col = "MannerOfDeath", 
                      predictions_col = "predict",
                      counts_col = "n",
                      palette = "Greens")

#---------       ROC and Precision-Recall plots   ---------------
# bind label and output of model
bind2 <- as.data.frame(h2o.cbind(df.test$MannerOfDeath, pred$p1))

# extract class and score
class <- bind2$MannerOfDeath
score <- bind2$p1

sscurves <- evalmod(scores = score, labels = class)

# plot
autoplot(sscurves)
