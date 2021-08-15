# specify the packages of interest
#packages = c("shinythemes", "shiny", "DT", "tidyverse",
#             "dplyr", "ggplot2", "h2o", "ggridges")

# load or install & load all
#package.check <- lapply(
#  packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

library(shinythemes)
library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(h2o)
library(ggridges)

# initiate h2o
h2o.init()

# load dataset
data <- read.csv(file = 'data/murder_suicide.csv')

# load saved model
saved_model <- h2o.loadModel('data/glm_model')