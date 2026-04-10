# Packages ################################
library(hoopR) #basketball datasets
library(dplyr) #data manipulation
library(ggplot2) #graphing and visualization
library(tidyverse) #data manipulation
library(psych) #descriptive statistics
library(ggiraph) #interactive plots

# Save dataset from hoopR package ########################
nba_team_box <- hoopR::load_nba_team_box(2021:hoopR::most_recent_nba_season()) #save data from 2021 to current

# Atlanta Hawks ##########################################################

# Atlanta Hawks team ID = 1 

hawks <- nba_team_box %>% #select from nba_team_box data 
  filter(team_id==1) %>% #select values where team = 1 
  mutate(game_date = as.Date(game_date)) %>% #convert game date variable to date type 
  arrange(game_date) %>% #order the dataframe by game 
  mutate(season = as.factor(season)) %>% #convert season variable to factor 
  mutate(team_home_away = recode(team_home_away, #recode home_away variable so values have capital letters 
                                 home = 'Home',
                                 away = 'Away'))

# create dataframe with season average points 

hawks_avg <- hawks %>%
  group_by(season) %>% #summarize by season
  summarize(mean_score = round(mean(team_score),0)) #average team scores
