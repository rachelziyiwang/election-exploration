#Analyzing election results

#Set up
library(tidyverse)
raw_data <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")

#Basic dataframe exploration
num_cols <- ncol(data)
num_rows <- nrow(data)
num_states <- length(unique(data$state))
#The number of timestamps varies for each state
num_timestamps <- length(unique(data$timestamp))

#How many reported timestamps exist for each state?
timestamps_by_state <- data %>% 
  group_by(state) %>% 
  count()

#Formatting: split out state name from electoral votes
data <- raw_data %>% 
  separate(state, into = c("state", "electoral_votes"), " \\(") %>% 
  mutate(electoral_votes = parse_number(electoral_votes))

#When did Biden take the lead in Georgia?
biden_lead_georgia_time <- data %>% 
  filter(state == "Georgia") %>% 
  filter(leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  pull(timestamp)

#What is the difference in votes in each state?


#How do total votes change over time (by candidate)?