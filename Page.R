## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$spotify_streams)
sd(data$spotify_streams)
summary(data$spotify_streams)
mean(data$star_rating)
sd(data$star_rating)
summary(data$star_rating)

# album
table(data$Album)

# mood_emotions
table(data$Mood_Emotions, data$Album)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
ggplot(data, aes(x =Album, y = star_rating)) +
  geom_boxplot() +
  labs(title = "Album Star Ratings",
       x = "Album",
       y = "Star_rating") +
  theme(axis.text.x=  element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme_minimal()
anova <- aov(star_rating ~ Album, data = data)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
# natural log of spotify streams
data$lnstreams <- log(data$spotify_streams)

linear_plot <- plot(data$star_rating, data$lnstreams)
print(linear_plot)
# add x line and y line for means
meany <- mean(data$lnstreams)
meanx <- mean(data$star_rating)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(lnstreams ~ star_rating, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
plot(data$star_rating,residuals(linear_relationship))
abline(h = 0, col = "black")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$Album, data$Mood_Emotions)
chisq.test(data$Album,data$Mood_Emotions)
