library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(janitor)

# df_playoffs_teams <- read_csv('mlb_playoffs_teams_2010_2019.csv', col_names = F)
# colnames(df_playoffs_teams)  <- c("year", "score", "team")
# df_playoffs_teams$year <- df_playoffs_teams$year %>% str_sub(end = 4)
# list_teams <- df_playoffs_teams$team %>% str_split(fixed('vs.'), simplify = T)
# list_teams
# team1 <- list_teams[[1]][1] %>% str_split(fixed(' ('))
# team2 <- list_teams[[1]][2] %>% str_split(fixed(' ('))
# df_playoffs_teams$team1[1] <- team1[[1]][1]
# team2
# team2[[1]][1]
# 
# df_playoffs_teams
# list_teams %>% str_detect(pattern = "^ ")
# test <- list_teams %>% str_split(fixed(" (|vs. "), simplify = T)
# test %>% str_replace(fixed("*"), fixed(""))
# test[5]
# list_teams
# test
# # df_playoffs_teams %>% select(year, team) %>% 
# #   group_by(year) %>% 
# #   mutate(team1 = df_playoffs_teams$team %>% str_split(fixed('vs.'), simplify = T))
# teams_playoff <- df_playoffs_teams$team %>% 
#   str_replace("\\*", "") %>% 
#   str_split(" \\(|vs. ", simplify = T)
# df_playoffs_teams$team1 <- ''
# df_playoffs_teams$team2 <- ''
# df_playoffs_teams
# for (i in 1:86) {
#   df_playoffs_teams$team1[i] <-  teams_playoff[i,1]
#   df_playoffs_teams$team2[i] <-  teams_playoff[i,3]
#   
# }
# team_by_game
# teams_playoff[3,1]
# 
# df_playoffs_teams

df_playoffs_teams <- read_csv('mlb_playoffs_teams_2010_2019.csv', col_names = F)
colnames(df_playoffs_teams)  <- c("year", "score", "matchup")
df_playoffs_teams$series <- df_playoffs_teams$year %>% str_sub(start = 6)
df_playoffs_teams$year <- df_playoffs_teams$year %>% str_sub(end = 4)
teams_playoff <- df_playoffs_teams$matchup %>% 
  str_replace("\\*", "") %>% 
  str_split(" \\(|vs. ", simplify = T)

for (i in 1:86) {
  df_playoffs_teams$winning_team[i] <-  teams_playoff[i,1]
  df_playoffs_teams$losing_team2[i] <-  teams_playoff[i,3]
}
  
df_playoffs_teams

playoff_teams <- df_playoffs_teams %>% 
  mutate(series = year %>% str_sub(start = 6),
         year = year %>% str_sub(end = 4),
         matchup_clean = str_remove(matchup, "\\*")) %>%
  separate(matchup_clean, sep = " vs\\. ", into = c("team1", "team2")) %>% 
  mutate(winning_team = 
           str_remove(team1, " \\([:digit:]{2,3}-[:digit:]{2,3}, [AN]L\\)"),
         losing_team = 
           str_remove(team2, " \\([:digit:]{2,3}-[:digit:]{2,3}, [AN]L\\)")) %>% 
  select(year, series, winning_team, losing_team)

playoff_teams










