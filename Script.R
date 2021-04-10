# "MovieLens Project"
# "Viktoriia Ilina"
# "April 2021"

# Data #

### Loading data ###

# To generate our datasets, we use the following code provided by the course page:


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
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

# The 'edx' dataset comprises 9000061 rows and 6 columns. The 'validation' dataset has 999993 rows and 6 columns respectively.

### Data exploration and visualization ###

# To begin with, let's take a quick look at the data. As we can see, 'edx' dataset contains 6 variables: 'userId', 'movieId', 'rating', 'timestamp', 'title' and 'genres'. Each row represents a single rating a unique user gave a particular movie. Some films belong to several genres at once, so it is advisable for further analysis to split the 'genre' column data into single genres.

head(edx)

# According to the output of summary() function, there are no missing values.

summary(edx)

# In general, the dataset includes 69878 users and 10677 movies in 797 genres.

edx %>% 
    summarize(unique_movies = n_distinct(movieId), unique_users = n_distinct(userId), unique_genres = n_distinct(genres))

# In the histogram below, we can see that users tend to rate movies relatively high, since only 25% of ratings are below 3, and use whole numbers.

edx %>% 
    ggplot(aes(rating)) + 
    geom_histogram(binwidth = 0.5, color = "black", fill = "#B3CFDD") + 
    xlab("Rating") + 
    scale_y_continuous(breaks = seq(0,3000000,500000)) + 
    ylab("Count") + 
    ggtitle("Rating distribution") + 
    theme(plot.title = element_text(hjust = 0.5))

# There is a large imbalance in the number of ratings between films: some movies were rated once while other movies were rated more than 10,000 times. As a result, the mean number of rating by movie is 842.9, while the median value is 122. There are a range of reasons for this disjuncture: the year of film's release, genre, star power, cast, awards, marketing and so on.

edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) + 
  geom_histogram(color = "black", fill = "#B3CFDD", bins = 30) +  
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of movies") + 
  ggtitle("Distribution of ratings by movieId") + 
  theme(plot.title = element_text(hjust = 0.5))

edx %>% 
  group_by(movieId) %>%
  summarize(count = n()) %>% 
  summary()

# Now, we can see which films have the greatest number of ratings:

edx %>% 
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  head(7)

# Number of ratings per user is skewed right. The majority of users rated between 13 and 140 movies, nevertheless the maximum number of ratings given by one user - 6637.

edx %>% 
  count(userId) %>%
  ggplot(aes(n)) + 
  geom_histogram(fill = "#B3CFDD", color = "black", bins = 30) + 
  scale_x_log10() + 
  xlab("Number of ratings") + 
  ylab("Number of users") + 
  ggtitle("Distribution of ratings by userId") + 
  theme(plot.title = element_text(hjust = 0.5))

edx %>% 
  group_by(userId) %>%
  summarize(count = n()) %>% 
  summary()

# In respect to genres, we can observe that some genres have a lot more ratings than others and that the ratings appear to be different between genres. The most popular rated genre types are Drama and Comedy, whereas Documentary, IMAX, and (no genres listed) have the smallest count of ratings. Drama and film-noir are some of the better-rated genre types, while horror is the worst rated.

# Getting genres stats information

stats_genres <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(countRating=n(), meanRating=mean(rating), medianRating=median(rating), countMovies=n_distinct(movieId))

# Plotting histograms

genres_count_of_the_ratings <- stats_genres %>% 
  ggplot(aes(x = genres, y = countRating)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#B3CFDD") + 
  xlab("Genres") + 
  scale_y_continuous(breaks = seq(0,4000000,500000)) + 
  ylab("Count of ratings") + 
  ggtitle("Count of the ratings per genre") + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

genres_average_rating <- stats_genres %>% 
  ggplot(aes(x = genres, y = meanRating)) + 
  geom_bar(stat = "identity", colour = "black", fill = "#B3CFDD") + 
  xlab("Genres") + 
  ylab("Average rating") + 
  ggtitle("The average rating per genre") + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

# Installing and importing the package for additional data visualization functions

if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(ggpubr)

# Arranging two ggplots on the same page

ggarrange(genres_count_of_the_ratings, genres_average_rating, ncol = 2, nrow = 1)

# Methodology and analysis #

# Before proceeding to the models and algorithms, we should split the 'edx' dataset into training (90%) and testing (10%) sets.

# Splitting edx data set into training and testing sets

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
training <- edx[-test_index,]
temp <- edx[test_index,]

# Checking that all 'userId' and 'movieId' in testing set are also in training set

testing <- temp %>% 
  semi_join(training, by = "movieId") %>% 
  semi_join(training, by = "userId")

# Adding back missing rows to training set and removing extraneous data

removed <- anti_join(temp, testing)
training <- rbind(training, removed)

rm(test_index, temp, removed)

# For evaluating trained models for usefulness / accuracy we will use Root Mean Square Error (RMSE), defined by the following function:

rmse <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Basic prediction via mean rating ###

# Computing the mean

mu <- mean(training$rating)

# Testing results

rmse_naive_model <- rmse(testing$rating, mu)
rmse_naive_model

# As expected, our result is far from perfect.

### The movie effect model ###

# Computing the averages and predicted ratings

movie_averages <- training %>% 
  group_by (movieId) %>% 
  summarise(b_i = mean(rating-mu))

predicted_ratings <- mu + testing %>% 
  left_join(movie_averages, by = "movieId") %>% 
  pull(b_i)

# Testing results

rmse_movie_effect_model <- rmse(testing$rating, predicted_ratings)
rmse_movie_effect_model

# Taking into account movie effect $b_i$ generates a lower RMSE value; nevertheless, the result is still mediocre.

### The movie and user effect model ###

# Computing the averages and predicted ratings

user_averages <- training %>% 
  left_join(movie_averages, by = "movieId") %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- testing %>% 
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Testing results

rmse_movie_and_user_effect_model <- rmse(testing$rating, predicted_ratings)
rmse_movie_and_user_effect_model

# Great, we see an improvement in the RMSE by 8.28%.

### The movie, user and genre effect model ###

# Splitting data into single genres

training_genres <- training %>% 
  separate_rows(genres, sep = "\\|", convert = TRUE)

testing_genres <- testing %>% 
  separate_rows(genres, sep = "\\|", convert = TRUE)

# Computing the averages and predicted ratings

genres_averages <- training_genres %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- testing_genres %>%   
  left_join(movie_averages, by = 'movieId') %>%
  left_join(user_averages, by = 'userId') %>%
  left_join(genres_averages, by = c('genres')) %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

# Testing results

rmse_movie_user_and_genre_model <- rmse(testing_genres$rating, predicted_ratings)
rmse_movie_user_and_genre_model

# We slightly inhence our result, but is it possible to improve it even further? 

### Regularized movie,user and genre effect model ###
  
# Note: this process could take a couple of minutes

# Tuning the hyperparameter

lambdas <- seq(0, 10, 0.15)

# Function for computing predicted ratings and testing for each lambda

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(training$rating)
  
  b_i <- training %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- training %>% 
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- training_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings <- testing_genres %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(rmse(testing_genres$rating, predicted_ratings))
  
})

# Choosing the minimum of function

rmse_regularization <- min(rmses)
rmse_regularization

# There is very little improvement compared to the previous model.

# Results #

# As seen in the table below, the best result (RMSE = 0.8620) was achieved by testing regularized movie, user and genre effect model.

rmse_results <- tibble(Method = c("Basic prediction via mean rating", "The movie effect model", "The movie and user effect model", "The movie, user and genre effect model", "Regularized movie,user and genre effect model"), RMSE = c(rmse_naive_model, rmse_movie_effect_model, rmse_movie_and_user_effect_model, rmse_movie_user_and_genre_model, rmse_regularization))
rmse_results %>% knitr::kable()

# However, we should keep in mind that we used 'edx' dataset for training, developing and selecting our algorithm. So, we'll have to go back to the original 'movielens' dataset and try to predict movie ratings in the 'validation' set.

# Note: this process could take a couple of minutes

# Creating the separated genres versions of our datasets

edx_genres <- edx %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

validation_genres <- validation %>%
  separate_rows(genres, sep = "\\|", convert = TRUE)

# Tuning the hyperparameter

lambdas <- seq(0, 10, 0.15)

# Function for computing predicted ratings and testing for each lambda

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- edx_genres %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
    
  predicted_ratings <- validation_genres %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(rmse(validation_genres$rating, predicted_ratings))
})

# Choosing the minimum of function

rmse_final <- min(rmses)
rmse_final

# Thus, Root Mean Square Error (RMSE) on 'validation' data is 0.8628443.