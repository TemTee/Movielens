library(tidyverse)
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
library(gmodels)
library(stringr)
library(data.table)
library(psych)



#   ************************       Read in the dataset from example code     ***********************


getwd()
edx_capstone <- as.data.frame (read_rds("~/R/datasets/edx.rds"))
head(edx_capstone)
str(edx_capstone)

###          split validation set for evaluation and 
#           extract validation set for raw data evaluation

set.seed(1, sample.kind = "Rounding")
split_index <- createDataPartition(y = edx_capstone$rating, times = 1, p = 0.1, list = FALSE)
capstone_dataset <- edx_capstone [-split_index, ]
temp <- edx_capstone [split_index, ] 
str(capstone_dataset)
str(temp)

# ensuring user and movie ids in validation dataset is also in capstone dataset
validation <- temp %>% 
  semi_join(capstone_dataset, by = "userId") %>% 
  semi_join(capstone_dataset, by = "movieId")

###        reduce data size to reduce run time on my machine

set.seed(1, sample.kind = "Rounding")
sample_split_index <- createDataPartition(y = capstone_dataset$rating, times = 1, p = 0.07, list = FALSE)
excess_data <- capstone_dataset [-sample_split_index, ]

###     subset of the data for quick run time

edx <- capstone_dataset [sample_split_index, ] 
str(edx)
str(excess_data)


#   ********************************       Tidying the data        ********************************


prop.table(table(is.na(edx))) * 100

###     Create a sample regex to separate year from title


x <- c(" in this year (01991) the spring sprang  .  ", 
       "To all numbers, 1999", 
       "I would like to order room ,(2005)", 
       "Mid month sales amounted to (1999)",
       "Make 05782 hay while the sun shines",
       "if at 1st you don't succeed, try  (1998) times",
       "Yearly budget should be around  (1971)",
       "Code me (428015), to strength (4012)",
       "  Come thou o friend  , free thy pride 2000, to (1985)  ")
x

str_match (x, "\\s+\\([0-9]{4}\\)$")


###      Test the regex on title variable  

title_tidy <- edx
title_tidy <- title_tidy %>% extract(title, 
                                     into = c("title", "year"),"(.*)\\s+(\\([0-9]{4}\\))$")
head(title_tidy)
year_reg <- title_tidy$year
pattern <- "[^0-9{4}$]"
head(str_remove_all(year_reg,pattern))

### remove all special characters in year variable

year_reg <- str_remove_all(year_reg,pattern)

prop.table(table(is.na(year_reg)))
title_tidy$year <- as.integer(year_reg)
head(title_tidy)
str(title_tidy)


##################################      End of test tidy        ##################################



###              incorporate the test tidy code into the edx data

edx <- edx %>% extract(title, 
                       into = c("title", "year"),"(.*)\\s+(\\([0-9]{4}\\))$")

year_reg <- edx$year
pattern <- "[^0-9{4}$]"
year_reg <- str_remove_all(year_reg,pattern)

####      parse year as integer

edx$year <- as.integer(year_reg)
head(edx)
str(edx)

identical(edx$year, title_tidy$year)
rm(year_reg)
rm(title_tidy)



#           *********************    Exploring and Manipulating the data    ********************************



###         extract rating year, movie age and date from timestamp and remove timestamp variable

edx <- edx %>% mutate(date = date(as_datetime(timestamp,origin = "1970/01/01")),
                      rating = as.double (rating),
                      rating_year = year(as_datetime(date)),
                      movie_year = year,
                      movie_age = rating_year - movie_year) %>%
  select(1,2,3,date,rating_year,movie_year,title,genres,movie_age,-timestamp)

str(edx)
head(edx)

# check movie age is correct with no negative value

edx %>% filter(movie_age < 0)
summary(edx[c("date", "movie_year","rating_year")])

# 9 movies appear to have negative age, this is can be corrected manually

edx$movie_age <- 
  ifelse(edx$movie_age < 0,(-1*edx$movie_age), edx$movie_age)

edx %>% filter(movie_age < 0)

###        summary of the distribution


summary(edx[c("userId", "movieId", "rating","rating_year","movie_year")])

describe(edx)


###           Visualize the data        ############################

###       number of ratings given and distribution

sort(table(edx$rating), descending = True)

edx %>% group_by(rating) %>% count() %>% arrange(desc(n)) %>% 
  ggplot(aes(rating,n)) +
  geom_col(fill = "aquamarine4", color = "black") +
  xlab("Rating") +
  ylab("Count")

edx %>% group_by(rating) %>% count() %>% arrange(desc(n)) %>% 
  ggplot(aes(reorder(rating,n), n)) +
  geom_col(fill = "aquamarine4", color = "black") +
  xlab("Rating") +
  ylab("Count")

# number of distinct users in the dataset

n_distinct(edx$userId) 

# distribution of users that rated movie shows that not many users rated many movies

edx %>% group_by(userId) %>% summarise(n = n()) %>% 
  ggplot(aes(n))+
  geom_histogram(bins = 50, fill = "aquamarine4", color ="black")+
  ylab("Count")+
  xlab("Number of UserId")


###     Average rating per user show users rate most movies between 2.5 and 4.5

edx %>% group_by(userId) %>% summarise(avg_userId_ratings = mean(rating)) %>% 
  ggplot(aes(userId,avg_userId_ratings))+
  geom_point(show.legend = F, color = "aquamarine4")+
  xlab("Users")+
  ylab("Mean Rating")

edx %>% group_by(userId) %>% summarise(avg_userId_ratings = mean(rating)) %>% 
  ggplot(aes(avg_userId_ratings))+
  geom_histogram(fill = "aquamarine4", color = "black")+
  xlab("Users")+
  ylab("Mean Rating")


###    number of movies in the dataset 

edx %>% group_by(movieId) %>% distinct(movieId) %>% nrow()

#   the distribution in the dataset

edx %>% group_by(movieId) %>% summarise(n = n()) %>% 
  ggplot(aes(n))+
  geom_histogram(bins = 50, fill = "aquamarine4", color ="black")+
  ylab("Count")+
  xlab("Number of Movies Rated")

###       Average rating per movie shows that most of the movies were rated between 2.5 and 4.5

edx %>% group_by(movieId) %>% 
  summarise(movie_mn = mean(rating)) %>% 
  ggplot(aes(movie_mn)) +
  geom_histogram(fill = "cadetblue", color ="black")+
  ylab("count of movieId")+
  xlab("Average rating of movie")

edx %>% group_by(movieId) %>% summarise(n = mean(rating)) %>% 
  ggplot(aes(n,movieId, color = "blue"))+
  geom_point(show.legend = F)+
  ylab("Movies")+
  xlab("Rating")


###   Visuzlize rating by year and age

edx %>% group_by(rating_year) %>% summarise(n=n()) %>% arrange(desc(n))

edx %>% group_by(rating_year) %>% summarise(n = mean(rating)) %>% 
  ggplot(aes(factor(rating_year),n))+
  geom_point()+
  geom_line()+
  ylab("Average Rating")+
  xlab("Rating Year")

edx %>% group_by(movie_year) %>% summarise(n=n()) %>% arrange(desc(n))

edx %>% group_by(movie_year) %>% summarise(n = mean(rating)) %>% 
  ggplot(aes(movie_year,n))+
  geom_point()+
  geom_line()+
  ylab("Average Rating")+
  xlab("Movie Year")


# the scatterplot below show that most older movies received high ratings


edx %>% group_by(movie_age) %>% summarise(n = mean(rating)) %>% 
  ggplot(aes(movie_age,n))+
  geom_point(color = "aquamarine4")+
  geom_jitter(color = "aquamarine4")+
  ylab("Average Rating")+
  xlab("Movie Age")

###      There are 19 distinct genre in dataset

genre <- edx %>% separate_rows(genres, sep = "[\\|$]") %>%
  group_by(genres) %>% count() %>% pull(genres)

genre

edx %>% separate_rows(genres, sep = "[\\|$]") %>%
  group_by(genres) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  ggplot(aes(reorder(genres,n),n)) +
  geom_col(fill = "aquamarine4")+
  coord_flip()+
  xlab("Genre")+
  ylab("Count")


# Average distributions of ratings by genres shows that Film Noir has the hightest average rating
# compared to Drama whose count was the highest in previous chart

edx %>% separate_rows(genres, sep = "[\\|$]") %>%
  group_by(genres) %>%
  summarise(n = mean(rating)) %>% arrange(desc(n)) %>% 
  ggplot(aes(reorder(genres,n),n)) +
  geom_col(fill = "aquamarine4")+
  coord_flip()+
  xlab("Genre")+
  ylab("Count")


# Most rated movies by count of number of ratings received and average movie rating

edx %>% group_by(movieId, title) %>% summarise(n=n()) %>% arrange(desc(n))

edx %>% group_by(movieId, title) %>% summarise(n=mean(rating)) %>% arrange(desc(n))

# Most and least rated movies by user average

edx %>% group_by(movieId, title) %>% summarise(n=n()) %>% arrange(n)

edx %>% group_by(movieId, title) %>% summarise(n=mean(rating)) %>% arrange(n)

# Note that the above most and least rated are biased owing to the fact that some of the most
# rated movies were rated once and give a 5 star rating. Hence this should be put into consideration
# when building the model.

###### visualize correlation of the variables

pairs.panels(edx[,c(1:3,9)])


#   ********************************       Build the Model          ******************************



######  spliting data into train_set (75%) and test_set (25%)

set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.25, list = FALSE)
train_set <- edx [-test_index, ]
test_set <- edx [test_index, ]


#######   Training the model using movieId, movie age and userId as predictors

# Because the model requires a larger memory space, so the below code couldnot be run
# on my machine and the residual was unable to be retrieved. Hence, using the RMSE
# from caret package, the averages of userId, movieId and movie_age, i will use the LSE method
# to determine the model with lowest RMSE.

* y <- train_set$rating

* fit <- lm(y ~ as.factor(movieId) + as.factor(userId) + movie_age, data = train_set)

* predict <- predict(fit, test_set)


#   ***************** Check the the model with the least RMSE      **********************

# write a function to calculate the rmse. 
# rmse = sqrt of the mean of the squared of error
# error = actual value - predicted value
# bias = mean(actual - mean of actuals)
# predicted rating = average of predictor


mu <- mean(train_set$rating)


# find the average of movie biasness on the model

avg_movie_rating <- train_set %>%
  group_by(movieId) %>%
  summarize(movie_bias = mean(rating - mu))

# the movie effect prediction

predict_rating <- test_set %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  mutate(predictions = mu + movie_bias) %>%
  pull(predictions)

RMSE(predict_rating, test_set$rating, na.rm = TRUE)


# find the average of user biasness on the model

avg_user_rating <- train_set %>% left_join(avg_movie_rating, by = "movieId") %>% 
  group_by(userId) %>%
  summarize(user_bias = mean(rating - mu - movie_bias))

# the user effect prediction

predict_rating <- test_set %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_user_rating, by = "userId") %>%
  mutate(predictions = mu + movie_bias + user_bias) %>%
  pull(predictions)

RMSE(predict_rating, test_set$rating, na.rm = TRUE)


# find the average of age biasness on the model

avg_age_rating <- train_set %>% left_join(avg_user_rating, by = "userId") %>% 
  group_by(movie_age) %>%
  summarize(age_bias = mean(rating - mu - user_bias))

# the age effect prediction

predict_rating <- test_set %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_user_rating, by = "userId") %>%
  left_join(avg_age_rating, by = "movie_age") %>%
  mutate(predictions = mu + movie_bias + user_bias + age_bias) %>%
  pull(predictions)

RMSE(predict_rating, test_set$rating, na.rm = TRUE)







avg_genre_rating <- train_set %>% left_join(avg_age_rating, by = "movie_age") %>% 
  group_by(genres)%>% 
  summarize(genre_bias = mean(rating - mu - age_bias))


# make the predictions using the averages of each predictor variable

predict_rating <- test_set %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_user_rating, by = "userId") %>%
  left_join(avg_age_rating, by = "movie_age") %>%
  mutate(predictions = mu + movie_bias + user_bias + age_bias) %>%
  pull(predictions)

# 
RMSE(predict_rating, test_set$rating, na.rm = TRUE)



#       *************   Regularizing      *********************
#   *********************   Improve the model by regularising     **************************


lambdas <- seq(0,20,0.5)

reg <-function(l, train_set, test_set){
  
  mu <- mean(edx$rating)
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarize(movie_bias = sum(rating - mu)/(n()+l))
  
  user_bias <- train_set %>%
    left_join(movie_bias, by="movieId") %>%
    group_by(userId) %>%
    summarize(user_bias = sum(rating - movie_bias-mu)/(n()+l))
  
  predict_rating <- test_set %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    mutate(pred = mu + movie_bias + user_bias) %>%
    pull(pred)
  return(RMSE(predict_rating, test_set$rating, na.rm = TRUE))
  
}

rmses <- sapply(lambdas, reg, train_set = train_set, test_set = test_set)
qplot(lambdas, rmses) 

lamd <- lambdas[which.min(rmses)]


##############          Improve the model          ########################################


mu <- mean(train_set$rating)

movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarize(movie_bias = sum(rating - mu)/(n()+lamd))

user_bias <- train_set %>%
  left_join(movie_bias, by="movieId") %>%
  group_by(userId) %>%
  summarize(user_bias = sum(rating - mu - movie_bias)/(n()+lamd))


predict_rating <- test_set %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  mutate(pred = mu + movie_bias + user_bias) %>%
  pull(pred)

RMSE(predict_rating, test_set$rating, na.rm = TRUE)

