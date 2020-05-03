library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(janitor)
library(gridExtra)
playoff_teams <- read_csv('mlb_playoffs_teams_2010_2019.csv', 
                              col_names = c("year", "score", "matchup"))%>% 
  mutate(series = year %>% str_sub(start = 6),
         year = year %>% str_sub(end = 4),
         matchup_clean = str_remove(matchup, "\\*")) %>%
  separate(matchup_clean, sep = " vs\\. ", into = c("team1", "team2")) %>% 
  mutate(winning_team = 
           str_remove(team1, " \\([:digit:]{2,3}-[:digit:]{2,3}, [AN]L\\)"),
         losing_team = 
           str_remove(team2, " \\([:digit:]{2,3}-[:digit:]{2,3}, [AN]L\\)")) %>% 
  select(year, series, winning_team, losing_team) %>% gather(final, team, ends_with('team')) %>% 
  arrange(desc(year)) %>% 
  mutate(final = final %>% str_sub(end = 1) %>% str_to_upper())

teams_abreviation <- read_csv('currentAbbreviationTeamMLB.csv', col_names = c('team')) %>% 
  separate(team, sep = " - ", into = c("name", "abb")) %>% 
  mutate(abb = abb %>% str_sub(start = -3))

playoff_teams_abb <- playoff_teams %>% left_join(teams_abreviation, by = c('team' = 'name'))















