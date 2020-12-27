# BA- Final Project _ Team -1
# Plots file : 1. Before imputation plots and 2. After imputation plots

# 1. Reading CSV 
rawmovies = "https://raw.githubusercontent.com/abhishek-gupta-nyu/themoviedb/master/final/final.csv"
movies = read.csv(rawmovies, header=TRUE,stringsAsFactors= FALSE)
head(movies)
summary(movies)
str(df_movies)
dim(df_movies)

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
library(plotly)

############################## After imputation plots ###############

# 1. Reading CSV 
rawmovies_new = "https://raw.githubusercontent.com/ajithgh/Predicting-Movie-success/main/Final%20Data%20Sets/Imputed%20file%20SunNight.csv"
movies_new = read.csv(rawmovies_new, header=TRUE,stringsAsFactors= FALSE)
head(movies_new)
summary(movies_new)
str(movies_new)
dim(movies_new)

#2. Checking Data quality
# Script loads a file and produces a data quality report for
# numerical variables: "dq_num.csv"
# categorical variables: "dq_cat.cs"

checkDataQuality(data = movies_new, 
                 out.file.num ="dq_num1.csv", 
                 out.file.cat= "dq_cat1.csv")
dq_num1<-read.csv("dq_num1.csv")
dq_cat1<-read.csv("dq_cat1.csv")
View(dq_num1)   
View(dq_cat1) 

#3. Basis analysis and plotting

###3a. Genre vs. budget
movies_new %>% group_by(genre) %>% 
  summarise(n=n()) %>%
  head()
head(movies_new$budget)


df_1 = movies_new
library(dplyr)
df_1 <- df_1 %>% group_by(genre) %>% summarise(me = mean(budget))
df_1<- df_1[-c(1), ]


P = 239/255
ggplot(aes(x = reorder(genre,-me), y = me), data = df_1) + geom_bar(stat = "identity",fill="tomato3")+
  labs(title="Movie budgets by genre", 
       subtitle="Adventure topped the list") +
  xlab("Genre") +
  ylab("Budget ($)")+ scale_y_continuous(labels = dollar) + theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(axis.text=element_text(size=20),
          axis.title=element_text(size=24,face="bold"))+
  theme(plot.title = element_text(size = 35, face = "bold"))+
  theme(plot.subtitle = element_text(size = 24)) +
  theme(plot.background = element_rect(fill = rgb(P,P,P))) +
  theme(panel.background = element_rect(fill = rgb(P,P,P)))

##################  Genre box plots by IMDB ratings#################
df_4 = movies_new
df_4 = df_4[!(is.na(df_4$genre) | df_4$genre==""), ]

df_4 %>%
  ggplot(aes(reorder(genre, Imdb_Rating, median, order = TRUE), y = Imdb_Rating, fill = genre)) + 
  geom_boxplot() + 
  coord_flip() + 
  geom_label(data = typeLabelCount, aes(x = genre, y = 10, label = count),  hjust = 0, size = 8) + 
  ggtitle("Box plot of Imdb ratings by popular movie genres") + 
  guides(fill=FALSE) + 
  ylim(0, 11) +
  labs(x = "Popular movie genre", y = "IMDB rating")+ theme_classic()+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"))+
  theme(plot.title = element_text(size = 35, face = "bold"))+
  theme(plot.subtitle = element_text(size = 24)) +
  theme(plot.background = element_rect(fill = rgb(P,P,P))) +
  theme(panel.background = element_rect(fill = rgb(P,P,P)))

################ Top 10 movies by genre ##########################

top10 <- head(df_movies[order(df_movies$revenue, decreasing = TRUE), c("original_title", "revenue")], n = 11)
top10
top10$original_title <- reorder(top10$original_title, as.integer(top10$revenue))

# Formating in Billions

top10$revenue <- as.integer(top10$revenue)
str(top10)
top10<- top10[-c(1), ]
top10$revenue = round(top10$revenue/10^9,2)
view(top10)

ggplot(top10, aes(y= revenue,x = reorder(original_title,-revenue))) +
  geom_bar(stat="identity",fill="tomato3",width = 0.5)+
  scale_y_continuous(labels = dollar)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(x = "Movie", y = "Revenue (Billions)", title = "Top 10 Movies by Revenue ")+
  theme(plot.background = element_rect(fill = rgb(P,P,P))) +
  theme(panel.background = element_rect(fill = rgb(P,P,P)))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"))+
  theme(plot.title = element_text(size = 35, face = "bold"))+
  theme(plot.subtitle = element_text(size = 24)) 
  
################ scatter plot of revenue vs. budget##########################

scatterPlot <- movies_new %>% 
  ggplot(aes(x = cast_size, y = crew_size)) + 
  geom_point(alpha= 1, colour = "tomato3", size = 4) + 
  labs(y = "crew size", 
       x = "cast size",
       title = "Cast size vs. Crew size") +
  theme_classic()+
  theme(plot.background = element_rect(fill = rgb(P,P,P))) +
  theme(panel.background = element_rect(fill = rgb(P,P,P)))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"))+
  theme(plot.title = element_text(size = 35, face = "bold"))+
  theme(plot.subtitle = element_text(size = 24)) 

#############################################################################
movies %>% group_by(vote_average) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=vote_average, y=count)) + 
  geom_line(colour = "tomato3") +
  geom_point(size = 4,colour = "tomato3") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Ratings Distribution", subtitle = "What ratings are most common") + 
  xlab("Ratings") +
  ylab("Count") +
  theme_classic()+
  theme(plot.background = element_rect(fill = rgb(P,P,P))) +
  theme(panel.background = element_rect(fill = rgb(P,P,P)))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"))+
  theme(plot.title = element_text(size = 35, face = "bold"))+
  theme(plot.subtitle = element_text(size = 24))



