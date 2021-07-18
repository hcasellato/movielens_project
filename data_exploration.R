### Code Summary: ##############################################################
# This script has the intent of exploring the data set and find insights for
# further analysis.
#  
### Basic data sets: ###########################################################
# Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes

repo <- "http://cran.us.r-project.org"
if(!require(tidyverse))      install.packages("tidyverse",      repos = repo)
if(!require(caret))          install.packages("caret",          repos = repo)
if(!require(data.table))     install.packages("data.table",     repos = repo)

library(tidyverse)
library(caret)
library(data.table)

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

### Data Exploration: ##########################################################
# Quite self explanatory, but this piece of code has the intent of exploring the
# data set to understand its content.
#

# Data set structure:
head(edx)

# Data set dimensions:
dim(edx)

# Searching for NAs in ratings:
sum(is.na(edx$rating))

# Number of distinct users:
n_distinct(edx$userId)

# Number of distinct movies:
n_distinct(edx$movieId)

# Distribution of votes per user:
# Also its mean and median values
d_user <- edx %>% group_by(userId) %>%
                  summarize(count = n())

mean(d_user$count)
median(d_user$count)

# Distribution of votes per movie:
# Also its mean and median values
d_movie <- edx %>% group_by(movieId, title) %>%
                   summarize(count = n())

mean(d_movie$count)
median(d_movie$count)

# Distribution of votes per genre:
# Also its mean and median values
# ATTENTION: This d_genres is commented out because the process is unnecessarily
# slow. The file was already downloaded when cloned the git repository.

# d_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
#                     group_by(genres) %>%
#                     summarize(count = n())

# This will set the working directory where the current project is located:
setwd(getwd())
d_genres <- read.csv("./d_genres.csv")

mean(d_genres$count)
median(d_genres$count)




