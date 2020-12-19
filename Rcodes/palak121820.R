# BA- Final Project _ Team -1

##############################Data import and EDA ###################################


# 1. Reading CSV 
rawmovies = "https://raw.githubusercontent.com/abhishek-gupta-nyu/themoviedb/master/final/final.csv"
movies = read.csv(rawmovies, header=TRUE,stringsAsFactors= FALSE)
head(movies)
summary(movies)
str(movies)
dim(movies)

# 1. Setting working directory : In case reading file locally
#setwd("C:/Users/palak/Dropbox/NYU courses/Fall 2020/Business analytics/BA Project _Final")

# 2. Reading CSV
#df_movies<- read.csv(file = 'Movies_combine.csv',header = TRUE, stringsAsFactors = FALSE)
#head(df_movies)

#3.Initialing required libraries
library(plyr)
library(readr)
library(ggplot2)
library(scales)
library(ggthemes)
library(GGally)
library(dplyr)
library(mlbench)
library(dplyr)
library(tidyverse)
library(dataQualityR)
library(ggplot2)
library(ROSE)
library(rpart)
library(gbm)
library(mlbench)
library(caret)

#4. Checking Data quality
# Script loads a file and produces a data quality report for
# numerical variables: "dq_num.csv"
# categorical variables: "dq_cat.cs"

library(dataQualityR)   #dataquality package

checkDataQuality(data = movies, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("dq_num.csv")
dq_cat<-read.csv("dq_cat.csv")
View(dq_num)   
View(dq_cat) 

#5.EDA : Analysis of different variables and plotting visualizations
# a) genre 
# top 6 genre by num, most of them are missing
movies %>% group_by(genre) %>% 
  summarise(n=n()) %>%
  head()

# The number of genres in each movie is listed in this table, sorted in descend order.

tibble(count = str_count(movies$genre, fixed("|")), genre = movies$genre) %>% 
  group_by(count, genre) %>%
  summarise(n = n()) %>%
  arrange(-count) %>% 
  head()

# b) Ratings 
# counting number of each ratings/ vote average

movies %>% group_by(vote_average) %>% summarize(n=n())

# ploting number of ratings or vote average

movies %>% group_by(vote_average) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=vote_average, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("vote_average Distribution", subtitle = "What ratings are most common") + 
  xlab("vote_average") +
  ylab("Count") +
  theme_economist()


# c) movies distribution using id 

movies %>% group_by(id) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "Assymetric distribution ") +
  xlab("Ratings") +
  ylab("Number of Movies") + 
  theme_economist()

# d) Vote_count distribution

movies %>% group_by(	
  vote_count) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + stat_bin(bins = 10)+
  ggtitle("Vote_count distribution", 
          subtitle = "Vote_count distribution of movies ") +
  xlab("Vote_count") +
  ylab("Number of Movies") + 
  theme_economist()



# 1. Movie names vs. genre ( to know the movie by type) { need to clean genre column}
# table(df_movies$genre)
# unique(df_movies$genre)
ggplot(data=movies, aes(x=genre)) + 
  geom_bar(alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Genre", y = "Number of Movies")

# 2. Plotting revenue distribution of the movies
ggplot(movies, aes(x=revenue))+ geom_histogram (alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) + 
  labs(x= "Revenue, Millions $", y = "Number of Movies")

# 3. Plotting Budget distribution of the movies

movies$budget = as.numeric(movies$budget) # converting budget from factor to numeric data
ggplot(movies, aes(x=budget))+ geom_histogram (alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) + 
  labs(x= "Budget, Millions $", y = "Number of Movies")

# 4. 

genre_df <- movies%>%
  select(id, title, runtime, original_language,genre)

genre_df %>% 
  filter(!is.na(genre) == TRUE) %>%
  ggplot(aes(x = genre, y = runtime)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# 5. 

top10 <- head(movies[order(movies$revenue, decreasing = TRUE), c("original_title", "revenue")], n = 10)
top10

top10$original_title <- reorder(top10$original_title, as.numeric(top10$revenue))

# Formating in Billions
top10$revenue <- paste(format(round(top10$revenue / 1e9, 2), trim = TRUE), "B")

ggplot(top10, aes(original_title, revenue)) +
  geom_col(position = "dodge", aes(fill = revenue)) +
  coord_flip() +
  labs(x = "Movie Name", y = "Revenues in USD", title = "Top 10 Movie with Most Revenues All Time")



top10 <- head(movies[order(movies$vote_average, decreasing = TRUE), c("original_title", "vote_average")], n = 10)
top10

top10$original_title <- reorder(top10$original_title, as.numeric(top10$vote_average))

# Formating in Billions
top10$vote_average <- paste(format(round(top10$vote_average / 1e9, 2), trim = TRUE), "B")

ggplot(top10, aes(original_title, vote_average)) +
  geom_col(position = "dodge", aes(fill = vote_average)) +
  coord_flip() +
  labs(x = "Movie Name", y = "ratings", title = "Top 10 Movie with highest ratings All Time")
