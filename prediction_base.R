### Code Summary: ##############################################################
#
#
#
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
gc()

### Partitioning: ##############################################################
# Test and train data partitioning
# Also creating rmse function

# Data sets
test_index <- createDataPartition(edx$rating, times = 1,
                                  p = 0.3, list = FALSE)
train_set <- edx[-test_index]
test_set  <- edx[test_index]

# Making sure all movies and users appear in both data sets
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# RMSE function
rmse <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

### Modeling: ##################################################################
# 

set.seed(2021, sample.kind = "Rounding")

train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

rm(edx, validation, test_index, train_set)
gc()

r <- Reco()

tune <- r$tune(train_data, opts = list(dim      = c(10, 20, 30),
                                       lrate    = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, 
                                       niter    = 10))

r_train <- r$train(train_data, opts = c(tune$min, nthread = 1, niter = 30))

r_predict <- r$predict(test_data, out_memory())

result <- rmse(test_set$rating, r_predict)