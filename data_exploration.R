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
# Also its mean value
# ATTENTION: This d_genres is commented out because the process is unnecessarily
# slow. The file was already downloaded when cloned the git repository.

# d_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
#                     group_by(genres) %>%
#                     summarize(count = n())

# This will set the working directory where the current project is located:
setwd(getwd())

# And this will load the file:
d_genres <- read.csv("./d_genres.csv")[,2:3]

mean(d_genres$count)

# Distribution of ratings:
# Also its mean and median values

d_rating <- edx %>% group_by(rating) %>%
                    summarize(count = n())

mean(d_rating$count)
median(d_rating$count)

### Data Visualization: ########################################################
# Now, the distributions already made can be visualized and maybe give an 
# insight on what method is better pursuing.
#

## Histograms:
# Users
h_user <- d_user %>% ggplot(aes(count)) +
          scale_x_log10() +
          geom_histogram(bins = 50, fill = "azure4") +
          ggtitle("Distribution of Ratings per User") +
          ylab("Number of Users") +
          xlab("Number of Ratings") +
          theme_bw(base_size = 12, base_family = "times")
h_user

# Movies
h_movie <- d_movie %>% ggplot(aes(count)) +
           scale_x_log10() +
           geom_histogram(bins = 50, fill = "azure4") +
           ggtitle("Distribution of Ratings per Movie") +
           ylab("Number of Movie") +
           xlab("Number of Ratings") +
           theme_bw(base_size = 12, base_family = "times")
h_movie

# Ratings
h_rating <- d_rating %>% ggplot(aes(x = rating, y = count)) +
            geom_col(fill = "azure4") +
            ggtitle("Distribution of Ratings given") +
            ylab("Number of Ratings") +
            xlab("Ratings") +
            theme_bw(base_size = 12, base_family = "times")
h_rating

# Uncomment the rm() function if the distributions are no longer necessary! 
# rm(d_genres,d_movie,d_rating,d_user)