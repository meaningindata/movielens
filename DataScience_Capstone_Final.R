
##################################################################################################
# github location
#
# https://github.com/meaningindata/movielens
##################################################################################################


##################################################################################################
# Install required packages if missing
##################################################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dslabs)) install.packages("dslabs")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(lubridate)) install.packages("lubridate")

##################################################################################################
# Load Libraries
##################################################################################################
library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(dplyr)
library(ggplot2)
library(dslabs)
library(ggrepel)
library(lubridate)




###########################################################################################
# Download MovieLens file
###########################################################################################
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)




##################################################################################################
# Some custom functions
##################################################################################################

# Get current major and minor version numbers of local R install
R_Major <- as.numeric(R.version$major)
R_Minor <- as.numeric(R.version$minor)

# Create a function to set the seed based on currently running R version
# setSeed(s) : s = seed to be set to
##################################################################################################
setSeed <- function(s){
  if (R_Major >= 3 & R_Minor > 3.5) {
    set.seed(s, sample.kind="Rounding")
  } else {
    set.seed(s)
  }
}

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




###########################################################################################
# Import Ratings file
###########################################################################################
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

###########################################################################################
# Import Movies file
###########################################################################################
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         titlenoyear = str_remove(title, "[/(]\\d{4}[/)]$"),
         genres = as.character(genres),
         year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"),regex("\\d{4}"))),
         decade = floor(year/10) * 10
  )

# The year released needs to be extracted from the title as shown above


###########################################################################################
# Join Movies and Ratings into a working dataset
###########################################################################################
movielens <- left_join(ratings, movies, by = "movieId")



#Structure of movielens dataset
str(movielens)



###########################################################################################
# Create Validation Set representing 10% of the MovieLens Data
###########################################################################################
setSeed(1)

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

rm(dl, ratings, test_index, temp, movielens, removed)




###############################################################################
# Calculate summary statistics
###############################################################################
edx_summary <- summary(edx$rating)

###############################################################################
# Save mean as variable mu for future access
###############################################################################
mu <- round(as.numeric(edx_summary["Mean"]),1)




##############################################################################
# Create a summary dataset containing Rating counts
##############################################################################
count_by_rating <- edx %>% group_by(rating) %>% summarize(total=n())

count_by_rating %>% ggplot(aes(rating, total/10^6)) + geom_bar(stat="identity") + ggtitle("Distribution by Rating") +
  xlab("Rating") + ylab("Count in Millions")




##############################################################################
# Create a dataset containing some overall statistics of the contents
##############################################################################
content_summary <- edx %>%
  summarize(users=n_distinct(userId), movies=n_distinct(movieId), ratings=n(), min_rating=min(rating), max_rating=max(rating))



##############################################################################
# Inspect Ratings By Movie
##############################################################################
setSeed(1)

# generate a dataset of ratings by movie
ratings_by_movie <- edx %>% group_by(movieId, title) %>% summarize(count = n(), avg=mean(rating), b_movie = avg - mu)

# plot this as a histogram
ratings_by_movie %>% ggplot(aes(avg)) + geom_histogram(bins=30) + ggtitle("Average Ratings by Movie") + xlab("Rating") + ylab("Number of Movies")


setSeed(1)

# Display the top 10 movies by their number of ratings
ratings_by_movie %>% 
  arrange(desc(count)) %>% 
  head(10) %>% 
  select(MovieID="movieId", Title="title", Ratings="count", Avg="avg")

# calculate and store the average number of ratings by movie
avg_ratings_per_movie <- round(mean(ratings_by_movie$count),0)


setSeed(1)

# display the movies with the least number of ratings
ratings_by_movie %>% 
  arrange(count, avg) %>% 
  head(10) %>% 
  select(MovieID="movieId", Title="title", Ratings="count", Avg="avg")


##############################################################################
# Inspect Ratings by Genre
##############################################################################

# generate a dataset of ratings by genre separating the genres column into rows based on the piped value contained therein
ratings_by_genre <- edx %>% 
  separate_rows(genres, sep="\\|") %>% 
  select(movieId, genres, rating) %>% 
  group_by(genres) %>% 
  summarize(avg = mean(rating), movies = n_distinct(movieId), ratings = n())

# generate a plot showing the average rating by genre as well as the number of ratings by genre as a label
ratings_by_genre %>% filter(genres != "(no genres listed)") %>% 
  ggplot(data=.) + 
  geom_point(aes(reorder(genres, avg), avg, size=ratings), show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Genres") + 
  ylab("Average Rating") +
  ggtitle("Average Ratings and Number of Ratings by Genre") + 
  geom_label_repel(aes(genres, avg, label=format(ratings, big.mark=","))) + 
  geom_hline(yintercept = round(as.numeric(edx_summary["Mean"]),1), linetype="dashed", color="red") +
  geom_text(aes(17,3.5, label="Overall Average : 3.5", vjust=-.5), color="red")


ratings_by_genre %>% filter(genres != "(no genres listed)") %>% 
  ggplot(data=.) + 
  geom_bar(aes(reorder(genres, -movies), movies), show.legend = FALSE, stat="identity") + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Genres") + 
  ylab("Number of Movies") +
  ggtitle("Number of Movies by Genre")

# calculate and store the average number of movies by genre
avg_movies_per_genre <- round(mean(ratings_by_genre$movies),1)
median_movies_per_genre <- round(median(ratings_by_genre$movies),1)


#generate a datset of ratings by genres as they are assigned to the movies
#genres can have one or more assigned to a movie
ratings_by_genres <- edx %>%
  group_by(genres) %>%
  summarize(ratings = n(), avg=mean(rating), movies = n_distinct(movieId))

# calculate the number of genres combinations
number_of_genre_groups <- nrow(ratings_by_genres)

# calculate the top 10 genre groupings by number of movies within them
top_10_genres <- ratings_by_genres %>% arrange(desc(movies)) %>% head(10)

# calculate the bottom 10 genres groupings by the number of movies with them
bottom_10_genres <- ratings_by_genres %>% arrange(movies) %>% head(10)

#calculate the number of genre combinations that have only onr (1) movie assigned
genre_groups_with_one_movie <- ratings_by_genres %>% filter(movies == 1)


# display top 10 genres
top_10_genres

# display bottom 10 genres
bottom_10_genres



##############################################################################
# Inspect Ratings by Decade
##############################################################################

# generate a dataset of ratings by decade
ratings_by_decade <- edx %>%
  group_by(decade = as.factor(decade)) %>%
  summarize(movies = n_distinct(movieId), ratings=n(), avg=mean(rating))

# plot average ratings by decade and the number of ratings by decade against the average rating
ratings_by_decade %>% 
  ggplot(data=.) + 
  geom_point(aes(decade, avg, size=ratings), show.legend = FALSE, stat="identity") + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  scale_x_discrete(name="Decade") +
  ylab("Average Rating") +
  ggtitle("Average Ratings and Number of Ratings by Decade") + 
  geom_label_repel(aes(decade, avg, label=format(ratings, big.mark=","))) + 
  geom_hline(yintercept = round(as.numeric(edx_summary["Mean"]),1), linetype="dashed", color="red") +
  geom_text(aes(3,3.5, label="Overall Average : 3.5", vjust=-.5), color="red")


# plot number of movies by decade into a bar chart
ratings_by_decade %>% 
  ggplot(data=.) + 
  geom_bar(aes(decade, movies), show.legend = FALSE, stat="identity") + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  scale_x_discrete(name="Decade") + 
  ylab("Number of Movies") +
  ggtitle("Number of Movies by Decade")



##############################################################################
# Inspect Ratings by Year
##############################################################################

# generate a dataset of ratings by year
ratings_by_year <- edx %>%
  group_by(year = as.factor(year)) %>%
  summarize(movies=n_distinct(movieId), ratings=n(), avg=mean(rating))

# plot rating by year into a bar chart
ratings_by_year %>% 
  ggplot(data=.) + 
  geom_bar(aes(year, movies), show.legend = FALSE, stat="identity") + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  scale_x_discrete(name="Year", breaks=seq(1910,2010, 5)) + 
  ylab("Number of Movies") +
  ggtitle("Number of Movies by Year")

# display the top 10 years with the most movies and show the number of ratings and the average rating
ratings_by_year %>% arrange(desc(movies)) %>% head(10)



##############################################################################
# Inspect Ratings by User
##############################################################################

setSeed(1)

# generate dataset of ratings by user
rating_by_users <- edx %>% group_by(userId) %>% summarize(ratings = n(), avg = mean(rating), movies=n_distinct(movieId))

# plot ratings per user by number of users
rating_by_users %>% ggplot(aes(avg)) + geom_histogram(bins=30) + ggtitle("Average Ratings by User") + xlab("Rating") + ylab("Number of users")


# plot the number of ratings by user
rating_by_users %>% ggplot(aes(ratings)) + geom_histogram(bins=150) + ggtitle("Total Ratings by User") + xlab("Ratings") + ylab("Number of users")

# display the top 10 users with the most ratings
rating_by_users %>% arrange(desc(ratings)) %>% select(userId, ratings, avg) %>% head(10)

#calculate the average nuumber of ratings per user
avg_ratings_per_user <- mean(rating_by_users$ratings)

# create a dataset of users have more than 1,000 ratings
over_1000_ratings <- rating_by_users %>% filter(ratings > 999)

# create a dataset of those users that have more than 1,000 ratings
ratio_1000_raters <- round(nrow(over_1000_ratings) / nrow(rating_by_users) * 100, 0)

# calculate the percentage of ratings these user represent
ratio_1000_ratings <- round(sum(over_1000_ratings$ratings) / sum(rating_by_users$ratings) * 100, 0)

# create a dataset of the user with less than 1,000 ratings
under_1000_raters <- rating_by_users %>% filter(ratings < 1000)




##############################################################################
# Build Model
##############################################################################

# Create a traing set and a test set from the edx dataset for use in training our model

setSeed(1)
test_index <- createDataPartition(y=edx$rating, times=1, p=0.1, list=FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from the test set back into training set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)


##############################################################################
# Start Building Model
##############################################################################

# calculate the overall average of the training set and assign it to mu
mu <- mean(train_set$rating)



##############################################################################
# Set Model baseline
##############################################################################

setSeed(1)

#create baseline RMSE using only the overall average
baseline_rmse <- RMSE(train_set$rating, mu)
rmse_results <- tibble(Method = "Baseline", RMSE = baseline_rmse)
knitr::kable(rmse_results)




##############################################################################
# Add Movie effect to model
##############################################################################

setSeed(1)

# create movie effect model
model_movie <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_movie = mean(rating - mu))

# apply it to the test set we created from the full training set
prediction <- test_set %>%
  left_join(model_movie, by="movieId") %>%
  mutate(pred = mu + b_movie) %>%
  .$pred

# calculate the RMSE
rmse_movie <- RMSE(prediction, test_set$rating)
rmse_movie

rmse_results <- bind_rows(rmse_results, tibble(Method="+ Movie Effect", RMSE=rmse_movie))
knitr::kable(rmse_results)



##############################################################################
# Add User effect to model
##############################################################################

setSeed(1)

# add user effect to model
model_user <- train_set %>% 
  left_join(model_movie, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_user = mean(rating - mu - b_movie))

# apply model to test set
prediction <- test_set %>%
  left_join(model_movie, by="movieId") %>%
  left_join(model_user, by="userId") %>%
  mutate(pred = mu + b_movie + b_user) %>%
  .$pred

# calculate RMSE
rmse_user <- RMSE(prediction, test_set$rating)
rmse_user

rmse_results <- bind_rows(rmse_results, tibble(Method="+ User Effect", RMSE=rmse_user))
knitr::kable(rmse_results)



##############################################################################
# Add Decade effect to model
##############################################################################

setSeed(1)

# add decade to our model
model_decade <- train_set %>% 
  left_join(model_movie, by="movieId") %>% 
  left_join(model_user, by="userId") %>%
  group_by(decade) %>% 
  summarize(b_decade = mean(rating - mu - b_movie - b_user))

# apply it to the test set
prediction <- test_set %>%
  left_join(model_movie, by="movieId") %>%
  left_join(model_user, by="userId") %>%
  left_join(model_decade, by="decade") %>%
  mutate(pred = mu + b_movie + b_user + b_decade) %>%
  .$pred

# calculate the RMSE
rmse_decade <- RMSE(prediction, test_set$rating)
rmse_decade

rmse_results <- bind_rows(rmse_results, tibble(Method="+ Decade Effect", RMSE=rmse_decade))
knitr::kable(rmse_results)



##############################################################################
# Add Year effect to model
##############################################################################

setSeed(1)

# add year to model
model_year <- train_set %>% 
  left_join(model_movie, by="movieId") %>% 
  left_join(model_user, by="userId") %>%
  left_join(model_decade, by="decade") %>%
  group_by(year) %>% 
  summarize(b_year = mean(rating - mu - b_movie - b_user - b_decade))

# apply to test set
prediction <- test_set %>%
  left_join(model_movie, by="movieId") %>%
  left_join(model_user, by="userId") %>%
  left_join(model_decade, by="decade") %>%
  left_join(model_year, by="year") %>%
  mutate(pred = mu + b_movie + b_user + b_decade + b_year) %>%
  .$pred

#calculate RMSE
rmse_year <- RMSE(prediction, test_set$rating)
rmse_year

rmse_results <- bind_rows(rmse_results, tibble(Method="+ Year Effect", RMSE=rmse_year))
knitr::kable(rmse_results)



##############################################################################
# Add Genres effect to model
##############################################################################

setSeed(1)

# add genres to model
model_genres <- train_set %>% 
  left_join(model_movie, by="movieId") %>% 
  left_join(model_user, by="userId") %>%
  left_join(model_decade, by="decade") %>%
  left_join(model_year, by="year") %>%
  group_by(genres) %>% 
  summarize(b_genres = mean(rating - mu - b_movie - b_user - b_decade - b_year))

# apply to test set
prediction <- test_set %>%
  left_join(model_movie, by="movieId") %>%
  left_join(model_user, by="userId") %>%
  left_join(model_decade, by="decade") %>%
  left_join(model_year, by="year") %>%
  left_join(model_genres, by="genres") %>%
  mutate(pred = mu + b_movie + b_user + b_decade + b_year + b_genres) %>%
  .$pred

# calculate RMSE
rmse_genres <- RMSE(prediction, test_set$rating)
rmse_genres

rmse_results <- bind_rows(rmse_results, tibble(Method="+ Genres Effect", RMSE=rmse_genres))
knitr::kable(rmse_results)


##############################################################################
# Regularization
##############################################################################

setSeed(1)

# display the top 10 movies based on their error 
model_movie %>%
  mutate(abs_b_movie = abs(b_movie)) %>%
  arrange(desc(abs_b_movie)) %>%
  left_join(movies, by="movieId") %>%
  select(Title="title", Error = abs_b_movie) %>%
  head(10)



##############################################################################
# Find best penalization parameter for regularization using cross-validation
##############################################################################

setSeed(1)

# check for lambdas between 3 and 6 using .25 increments
lambdas <- seq(3,6, .25)

rmses <- sapply(lambdas, function(l){
  
  b_movie <- train_set %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - mu)/(n() + l))
  
  b_user <- train_set %>%
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - mu - b_movie)/(n() + l))
  
  b_decade <- train_set %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    group_by(decade) %>%
    summarize(b_decade =sum(rating - mu - b_movie - b_user)/(n() + l) )
  
  b_year <- train_set %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    left_join(b_decade, by="decade") %>%
    group_by(year) %>%
    summarize(b_year =sum(rating - mu - b_movie - b_user - b_decade)/(n() + l) )
  
  b_genres <- train_set %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    left_join(b_decade, by="decade") %>%
    left_join(b_year, by="year") %>%
    group_by(genres) %>%
    summarize(b_genres =sum(rating - mu - b_movie - b_user - b_decade - b_year)/(n() + l) )
  
  
  prediction <- test_set %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user, by="userId") %>%
    left_join(b_decade, by="decade") %>%
    left_join(b_year, by="year") %>%
    left_join(b_genres, by="genres") %>%
    mutate(pred = mu + b_movie + b_user + b_decade + b_year + b_genres) %>%
    .$pred
  
  return(RMSE(prediction, test_set$rating))
})

# plot lambas versu rmse results
qplot(lambdas, rmses)

# identify the lowest RMSE value
lowest_rmse <- min(rmses)

# identity which lambda produced the lowest RMSE
lowest_lambda <- lambdas[which.min(rmses)]

##############################################################################
# Apply regularization to model using the best lambda value
##############################################################################

rmse_results <- bind_rows(rmse_results, tibble(Method="Final Regularized Model", RMSE=lowest_rmse))
knitr::kable(rmse_results)




##############################################################################
# Apply final model to initial training and validation sets
##############################################################################

lambda <- lowest_lambda

b_movie <- edx %>%
  group_by(movieId) %>%
  summarize(b_movie = sum(rating - mu)/(n() + lambda))

b_user <- edx %>%
  left_join(b_movie, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_user = sum(rating - mu - b_movie)/(n() + lambda))

b_decade <- edx %>%
  left_join(b_movie, by="movieId") %>%
  left_join(b_user, by="userId") %>%
  group_by(decade) %>%
  summarize(b_decade =sum(rating - mu - b_movie - b_user)/(n() + lambda) )

b_year <- edx %>%
  left_join(b_movie, by="movieId") %>%
  left_join(b_user, by="userId") %>%
  left_join(b_decade, by="decade") %>%
  group_by(year) %>%
  summarize(b_year =sum(rating - mu - b_movie - b_user - b_decade)/(n() + lambda) )

b_genres <- edx %>%
  left_join(b_movie, by="movieId") %>%
  left_join(b_user, by="userId") %>%
  left_join(b_decade, by="decade") %>%
  left_join(b_year, by="year") %>%
  group_by(genres) %>%
  summarize(b_genres =sum(rating - mu - b_movie - b_user - b_decade - b_year)/(n() + lambda) )


prediction <- validation %>%
  left_join(b_movie, by="movieId") %>%
  left_join(b_user, by="userId") %>%
  left_join(b_decade, by="decade") %>%
  left_join(b_year, by="year") %>%
  left_join(b_genres, by="genres") %>%
  mutate(pred = mu + b_movie + b_user + b_decade + b_year + b_genres) %>%
  .$pred

rmse_final <- RMSE(prediction, validation$rating)
rmse_final


##############################################################################
# CODE COMPLETE
##############################################################################


