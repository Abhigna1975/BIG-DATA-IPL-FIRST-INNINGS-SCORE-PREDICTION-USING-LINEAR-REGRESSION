# Exploratory Data Analysis of IPL Data
# Authors: Arun Rao (Leader), Sumanth Goud Bairi, Abhigna Margam
# Date: 2022-11-11

# Load Libraries and Data
library(dplyr)
library(ggplot2)

# Load Dataset
matches <- read.csv(file.choose())
deliveries <- read.csv(file.choose())

# Basic Data Inspection

# Inspect Match Data
head(matches)
str(matches)
summary(matches)

# Inspect Deliveries Data
head(deliveries)
str(deliveries)
summary(deliveries)

# Data Cleaning

# Check for Missing Values
sum(is.na(matches))
sum(is.na(deliveries))

# Handle Missing Values
matches <- na.omit(matches)

# Analysis

# Distribution of Matches by Season
matches_by_season <- matches %>%
  group_by(season) %>%
  summarise(Matches = length(unique(id)))



# Average Runs per Over
runs_per_over <- deliveries %>%
  group_by(over) %>%
  filter(is_super_over == 0) %>%
  summarise(Runs = mean(total_runs) * 6)

print(runs_per_over)

ggplot(runs_per_over, aes(over, Runs, fill = as.factor(over))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 1:20) +
  guides(fill = FALSE) +
  xlab("Over") +
  ylab("Total runs scored") +
  ggtitle("Average number of runs scored in each over of the innings")

# Total Number of Wickets per Over
wickets_per_over <- deliveries %>%
  group_by(over) %>%
  filter(is_super_over == 0) %>%
  summarise(Wickets = sum(dismissal_kind %in% c("bowled", "caught", "lbw", "stumped", "caught and bowled", "hit wicket")))

print(wickets_per_over)

ggplot(wickets_per_over, aes(over, Wickets, fill = as.factor(over))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 1:20) +
  guides(fill = FALSE) +
  xlab("Over") +
  ylab("Total wickets taken") +
  ggtitle("Total number of wickets taken in each over of the innings")
