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

# graph hawks points by season 
hawks_plot <- ggplot(data = hawks, aes(x = season, y = team_score)) +
  geom_jitter_interactive(alpha = .7, 
                          aes(color = team_winner,
                              tooltip = paste0(`team_home_away`,
                                               ' vs. ', `opponent_team_abbreviation`,
                                               '\nFinal Score: ',`team_score`, '-', `opponent_team_score`))) +
  geom_point_interactive(data = hawks_avg,
                         aes(x = season, y = mean_score,
                             tooltip = paste0('Season Average: ', `mean_score`)),
                         color = 'black', shape = 17, size = 3) +
  theme_classic() +
  labs(x = 'Season',
       y = 'Hawks Score',
       color = 'Outcome',
       title = 'Atlanta Hawks Points Per Game 2021-2026') +
  theme(text = element_text(family = 'serif'),
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = 'bottom') +
  scale_color_manual(values = c('#C8102E', '#fdb927'), labels = c('Loss', 'Win'))

hawks_plot

