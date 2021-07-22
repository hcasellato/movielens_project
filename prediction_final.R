### Code Summary: ##############################################################
# This script has the intent of using the LIBMF method to predict the movies in 
# the validation set.
# This script do not require the prediction_base.R script.
#  
### Basic data sets: ###########################################################
# Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes

repo <- "http://cran.us.r-project.org"
if(!require(tidyverse))      install.packages("tidyverse",      repos = repo)
if(!require(caret))          install.packages("caret",          repos = repo)
if(!require(data.table))     install.packages("data.table",     repos = repo)
if(!require(recosystem))     install.packages("recosystem",     repos = repo)


library(tidyverse)
library(caret)
library(data.table)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
# Check if the edx and validation datasets exists:
if(!exists("edx") & !exists("validation")){
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::",
                               "\t",
                               readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")),
                            "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  # if using R 4.0 or later:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1,
                                    list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
}
gc()

### Modeling: ##################################################################
# Modeling the data using the recosystem package, LIBMF method.
# Also creating the RMSE function

# RMSE function
rmse <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

set.seed(2021, sample.kind = "Rounding")

# Translating the train and test sets to a recosystem set
edx_data <- with(edx, data_memory(user_index = userId, 
                                  item_index = movieId, 
                                  rating     = rating))

validation_data <- with(validation,  data_memory(user_index = userId, 
                                                 item_index = movieId, 
                                                 rating     = rating))

# Creating a recommender model:
r_v <- Reco()

# Tuning parameters:
# This process can take a while =(
tune_v <- r$tune(edx_data, opts = list(dim    = c(10, 20, 30),
                                       lrate    = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, 
                                       niter    = 10))

# Training model:
r_train_v <- r$train(edx_data, opts = c(tune$min, nthread = 1, niter = 30))

# Prediction model:
r_predict_v <- r$predict(validation_data, out_memory())

# RMSE results:
result_v <- rmse(validation$rating, r_predict_v)
result_v