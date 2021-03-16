#Individual Project 5
# Author: Aaron Bone
# Version: 1.0
# Date: Spring 2021
# Summary: The goal of this project is to use R and TidyVerse to clean up a data set and 
# explore it. Essentially you are re-creating your Pandas analysis in R.

# imports
library(datasets)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(psych)

# reading the dataset
f50 <- read.csv("Future50.csv", header=TRUE, sep=",")

#column names
colnames(f50)

# first 5 rows
head(f50)

# last 5 rows
tail(f50)

# datatypes

sapply(f50, class)

# summary data of the dataset
summary(f50)

#dropping all rows without complete data
full<-f50[complete.cases(f50), ]

# replacing special characters
YOY_Sales <- as.numeric(sub("%", "",f50$YOY_Sales,fixed=TRUE))/100
YOY_Units <- as.numeric(sub("%", "",f50$YOY_Units,fixed=TRUE))/100
f50[["YOY_Sales"]] <- YOY_Sales
f50[["YOY_Units"]] <- YOY_Units

# correlation heatmap
cor(f50[,c("Sales","YOY_Sales","Units","YOY_Units","Unit_Volume")])

f50.cor = cor(f50[,c("Sales","YOY_Sales","Units","YOY_Units","Unit_Volume")])

corrplot(f50.cor)

#Distributions
hist(f50$Sales, main = "Sales",col = "pink")
hist(f50$YOY_Sales, main = "YOY_Sales",col = "red")

hist(f50$Units, main = "Units",col = "blue")
hist(f50$YOY_Units, main = "YOY_Units",col = "purple")

# Scatterplot

ggplot(data=full, mapping=aes(x=YOY_Sales, y=YOY_Units, color="restaurants")) +
  geom_point() + ggtitle("YOY_Sales by YOY_Units")

ggplot(data=full, mapping=aes(x=Sales, y=Units, color="restaurants")) +
  geom_point() + ggtitle("Sales by Units")

# Sales Boxplot

boxplot(Rank~Sales,data=f50, main="Sales Data",
        xlab="Sales", ylab="Rank")
# Bar Graphs

citystate = sapply(f50$Location, strsplit, ",")
citystate = data.frame(matrix(unlist(citystate), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
colnames(citystate) = c("City", "State")

count(citystate, vars = City)

citycount <- table(citystate$City)
barplot(citycount, main = "City Count", xlab = "", col = c("red"), horiz = TRUE)

statecount <- table(citystate$State)
barplot(statecount, main = "State Count", xlab = "", col = c("blue"), horiz = TRUE)

topcity <- head(citycount)
topstate <- head(statecount)

barplot(topcity, main = "City Count", xlab = "", col = c("red"), horiz = TRUE)
barplot(topstate, main = "City Count", xlab = "", col = c("blue"), horiz = TRUE)

# Pie Chart

piechartprep <- count(f50, Franchising)
lbls <- c("No","Yes")
pie(piechartprep[["n"]], labels = lbls, main="Pie Chart of Franchising")

