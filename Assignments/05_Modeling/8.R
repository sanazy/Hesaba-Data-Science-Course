#install.packages('h2o')

# Load libraries
library(h2o)

# Initialize H2O
h2o.init()

# Import File
df = h2o.importFile( path = 'data/data.csv',
                     destination_frame = 'df' )

df$MannerOfDeath <- as.factor(df$MannerOfDeath)

# Split with a ratio of 0.8
df.split <- h2o.splitFrame(data = df, ratios = 0.8, seed = 1234)

# Create training set from 1st data set in split
df.train <- df.split[[1]]

# Create testing set from 2st data set in split
df.test <- df.split[[2]]

# Separate features and target columns
x = setdiff( colnames(df), 'MannerOfDeath' )
y = 'MannerOfDeath'

# GBM hyperparameters
gbm_params1 <- list(learn_rate = c(0.001, 0.01, 0.1),
                    max_depth = c(3, 6),
                    ntrees = c(1, 100))

# Train and validate a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = df.train,
                      seed = 1234,
                      nfolds = 5,
                      stopping_metric = 'logloss',
                      hyper_params = gbm_params1)

# Get the grid results, sorted by logloss
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "logloss",
                             decreasing = FALSE)
print(gbm_gridperf1)

# Grab the top GBM model, chosen by logloss
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

# Evaluate the model performance on a test set
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = df.test)
# Print Performance of best GBM
print(best_gbm_perf1)

# Print logloss of best model
h2o.logloss(best_gbm_perf1)

# Look at the hyperparameters for the best model
print(best_gbm1@model[["model_summary"]])

# Save model
h2o.saveModel(object = best_gbm1, 'data/models')
