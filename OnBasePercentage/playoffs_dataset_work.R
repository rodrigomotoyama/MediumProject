library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(janitor)

df_playoffs_teams <- read_csv('mlb_playoffs_teams_2010_2019.csv', col_names = F)
colnames(df_playoffs_teams)  <- c("year", "score", "team")
df_playoffs_teams$year <- df_playoffs_teams$year %>% str_sub(end = 4)
list_teams <- df_playoffs_teams$team %>% str_split(fixed('vs.'))
team1 <- list_teams[[1]][1] %>% str_split(fixed(' ('))
team2 <- list_teams[[1]][2] %>% str_split(fixed(' ('))
df_playoffs_teams$team1[1] <- team1[[1]][1]
team2[[1]][1]

df_playoffs_teams
