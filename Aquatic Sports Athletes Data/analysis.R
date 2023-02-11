# Nina Tran
# Homework 2: R Script 
# AHC 695 Intro to Statistics for HCI
# Instructor: James Waller 
# 1/26/2023  

# required packages in the following 
library(tidyverse)
library(readxl)
library(ggplot2)
library(psych)

# open the file called Rio2016_AquaticAthletes.xlsx 
my_data <- read_xlsx("Rio2016_AquaticAthletes.xlsx")

# 1. DESCRIBING YOUR DATASET 
# what does each row represent? 
# Each row represents information about these athletes, including name, height, 
# gender, age, country, what sport they play, and whether they received a medal. 

# How many athletes are there in your dataset? 
# There are 1438 observations (athletes)
total_atheletes <- nrow(distinct(my_data))
total_atheletes

# What does each column represent? 
# Each column represents ........... 

# For each column (variable), state whether it is a categorical variable 
# or a numerical variable. 
summary(my_data)

# The column 1 'id' is a categorical variable. 
# The column 2 'name' is a categorical variable. 
# The column 3 'gender' is a categorical variable. 
# The column 4 'age' is a numerical variable. 
# The column 5 'height_cm' is a numerical variable. 
# The column 6 'country' is a categorical variable. 
# The column 7 'sport' is a categorical variable. 
# The column 8 'medalled' is a categorical variable. 

# For categorical variables (except Name and ID) list how many levels there are.
df_gender <- as.factor(my_data$gender)
df_gender 
# 2 levels 

df_age <- as.factor(my_data$age)
df_age
# 29 levels 

df_height <- as.factor(my_data$height_cm)
df_height
# 60 levels 

df_country <- as.factor(my_data$country)
df_country
# 174 levels 

df_sport <- as.factor(my_data$sport)
df_sport 
# 4 levels 

df_medal <- as.factor(my_data$medalled)
df_medal
# 2 levels 

# 2. COUNTRY REPRESENTATION 
# What are the five most represented countries (countries with the most athletes), 
most_athletes <- my_data %>% 
  count(country) %>% 
  top_n(5, wt = n) %>% 
  arrange(-n)
most_athletes

# The five most represented countries are United States, Australia, Italy, 
# Brazil, and China. 

# How many athletes are from each, and all together what percent of all athletes 
# are from these five countries? 
# United States - 85  
# Australia - 83 
# Italy - 81
# Brazil - 80 
# China - 78 

athletes_percent <- most_athletes %>%
  group_by(country, n) %>% 
  summarise(percent = 100*(sum(n)) / total_atheletes)
athletes_percent

# 3. AGE OF ATHLETES 
# What is the range of the ages (include minimum, maximum, range), and what is 
# the mean age and standard deviation? 
range_ages <- range(my_data$age)
range_ages
# range 13 41 

min_ages <- min(my_data$age)
min_ages
# min 13 
  
max_ages <- max(my_data$age)
max_ages
# max 41 

mean_ages <- mean(my_data$age)
mean_ages 
# mean 23.43 

sd_ages <- sd(my_data$age)
sd_ages
# standard deviation 4.36 

median_ages <- median(my_data$age)
median_ages

# summary info 
summary_ages <- summary(my_data)
summary_ages

# b. Include a histogram of all athlete’s ages and give a description – is it symmetrical
# or is it skewed either left or right? Does it have multiple modes. You can add any 
# other comments about the histogram. 

# Create a histogram of all athletes' ages
age_hist <- my_data %>% 
  ggplot(aes(x= age)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age Group", y = "Number of Athletes") + 
  theme_classic()
age_hist

# How to find the mode
Mode <- function(x){
  y <- data.frame(table(x))
  y[y$Freq == max(y$Freq),1]
}

age_mode <- Mode(my_data$age)
age_mode

# It is skewed right because the mean is greater than the median. It has the highest peak on this histogram. 
# So, there are no multiple modes. The mode is 21. 

# 4. SIZE OF DIFFERENT SPORTS 
# Give a review of the four aquatic sports in the data set – what are they  

sports_type <- unique(my_data$sport)
sports_type
# The four aquatic sports are swimming, diving, water polo, and synchronized swimming. 

# how many athletes are there competing in each (also give percentages). 
athletes_compete <- my_data %>%
  count(sport, gender) %>% 
  mutate(percent = n / sum(n) * 100)
athletes_compete

# Diving has 136 athletes (9.46%)
# Swimming has 942 athletes (65.5%) 
# Synchronized Swimming has 102 (7.09%)
# Water Polo has 258 athletes (17.9%)

# Which has the most athletes, which has the least?
# Swimming has the most athletes 
# Synchronized Swimming has the least athletes. 

# Insert a bar chart showing the number of athletes in each sport. Y label should say “Number of
# Athletes”.
# Give a visual gender breakdown of each sport as a proportional stacked bar chart. 

athletes_bar <- athletes_compete %>% 
  ggplot(aes(x = sport, y = n, fill = gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Types of Aquatic Sport", y = "Number of Athletes") + 
  theme_classic()
athletes_bar

# Comment on the gender breakdown of each sport – are all sports about 50-50 or are there any 
# sports that stand out one way or another? (you do not have calculate the exact percentages 
# yourself, although you can) 

# Synchronized Swimming has no male athletes. However, Swimming has more male athletes (35.1%) than 
# female athletes (31.1%). Also, Water Polo have more male athletes (10.7%) than female athletes (7.23%).   

# 5. HEIGHT OF WOMEN ACROSS THE SPORTS 
# I’m curious if the height of athletes depends on the sport, so I want you to plot four boxplots on
# one graph, each representing the distribution of women’s heights in inches
# , and give the median and IQR for each in a table. Use the data (graph and table) 
# to compare height distribution across sports – center, dispersion, skew. 

# Bonus – explain why I decided to look only at women instead of collapsing across 
# gender – that is, why not make 4 boxplots of all athletes in each sport?

# Because gender can impact height, which can skew the data. Depending on 
# the proportion of men:women in the data set, that can skew the data to the right or left 
# and higher or lower. 

# Do some data handling first in R 
# Some rows do not have height information (N/A). These should be filtered out. 

# Filter the data to only inlcude women 
women_df <- my_data %>% 
  drop_na(height_cm) %>% 
  filter(gender == "F") 
women_df

# The height_cm gives heights in centimeters, but I want inches. Create a new column 
# ('height_in'). 
height_dist <- women_df %>% 
  mutate(height_in = height_cm / 2.54) %>% 
  count(sport, height_in, height_cm) 
height_dist

# Create a 4-boxplot graph of the distribution of women's heights in each sport 
height_box <- height_dist %>% 
  ggplot(aes(x = sport, y = height_in)) + 
  geom_boxplot() + 
  labs(x = "Types of Aquatic Sports", y = "Women's Heights (Inches)") + 
  theme_classic()
height_box 
