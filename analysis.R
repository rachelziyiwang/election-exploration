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
#Add Biden and Trump vote columns
data <- raw_data %>% 
  separate(state, into = c("state", "electoral_votes"), " \\(") %>% 
  mutate(electoral_votes = parse_number(electoral_votes)) %>% 
  mutate(biden_votes = if_else(leading_candidate_name == "Biden",
                               leading_candidate_votes, 
                               trailing_candidate_votes)) %>% 
  mutate(trump_votes = total_votes_count - biden_votes)

#When did Biden take the lead in Georgia?
biden_lead_georgia_time <- data %>% 
  filter(state == "Georgia") %>% 
  filter(leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  pull(timestamp)

#What is the earliest time in each state that Biden is ahead?
biden_lead_all_states_time <- data %>% 
  group_by(state) %>% 
  filter(leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  select(state, timestamp)

#What is the difference in votes in each state?
vote_diff <- data %>% 
  group_by(state) %>% 
  filter(timestamp == max(timestamp)) %>% 
  mutate(vote_diff = biden_votes - trump_votes) %>% 
  mutate(pct_diff = vote_diff / total_votes_count)

vote_diff_plot <- ggplot(vote_diff) +
  geom_col(mapping = aes(x = vote_diff, 
                         y = reorder(state, vote_diff),
                         fill = leading_candidate_name)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(y = "State", x = "Vote Difference", fill = "Candidate", 
       title = "Vote Difference at Most Recent Time Stamp")

vote_pct_plot <- ggplot(vote_diff) +
  geom_col(mapping = aes(x = pct_diff, y = reorder(state, pct_diff)))

#How do total votes change over time (by candidate)?