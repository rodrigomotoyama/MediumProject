library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)
#library(gridExtra)
library(grid)
library(gt)
MVP_MLB_2010_2019 <- function(dataFrame, year, column = "player"){
  #print('passou')
  for (j in seq(1:length(dataFrame[,as.character(column)]))){
    #print('passou')
    if (year == 2010 & dataFrame[j,"player"] %in% c('Joey Votto', 'Josh Hamilton')){
      dataFrame[j, 'MVP'] <- T
      #print(dataFrame)
    }
    else if (year == 2011 & dataFrame[j,"player"] %in% c('Ryan Braun', 'Justin Verlander')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2012 & dataFrame[j,"player"] %in% c('Buster Posey', 'Miguel Cabrera')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2013 & dataFrame[j,as.character(column)] %in% c('Andrew McCutchen', 'Miguel Cabrera')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2014 & dataFrame[j,as.character(column)] %in% c('Clayton Kershaw', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2015 & dataFrame[j,as.character(column)] %in% c('Bryce Harper', 'Josh Donaldson')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2016 & dataFrame[j,as.character(column)] %in% c('Kris Bryant', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2017 & dataFrame[j,as.character(column)] %in% c('Giancarlo Stanton', 'Jose Altuve')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2018 & dataFrame[j,as.character(column)] %in% c('Christian Yelich', 'Mookie Betts')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (year == 2019 & dataFrame[j,as.character(column)] %in% c('Cody Bellinger', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else{
      dataFrame[j, 'MVP'] <- F
    }  
  }
  return(dataFrame)
}
################################## MVP FUNCTION #############################################
combined_mlb_df <- tibble()
for (i in 2010:2019){
  path_file <- paste('mlb-player-stats-Batters-', as.character(i), '.csv', sep='')
  mlb_df <- read_csv(file = path_file)%>% clean_names()
  df <- paste('mlb_batting_', as.character(i), sep='')
  mlb_df <- cbind(year=i, mlb_df)
  year <- i
  # MVP <- mlb_df %>% filter
  mlb_df <- MVP_MLB_2010_2019(mlb_df, year)
  combined_mlb_df <- rbind(combined_mlb_df, mlb_df)
  assign(df, mlb_df)
  # print(as.character(i))
}
combined_mlb_df

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
  mutate(final = final %>% str_sub(end = 1) %>% str_to_upper(), year = as.double(year))

teams_abreviation <- read_csv('currentAbbreviationTeamMLB.csv', col_names = c('team')) %>% 
  separate(team, sep = " - ", into = c("name", "abb")) %>% 
  mutate(abb = abb %>% str_sub(start = -3))

combined_mlb_df <- playoff_teams %>% left_join(teams_abreviation, by = c('team' = 'name')) %>% 
  right_join(combined_mlb_df, by = c('abb' = 'team', 'year' = 'year')) %>% 
  mutate(series = if_else(series %>% is.na(), "Eliminated", series),
         final = if_else(final %>% is.na(), "-", final)) %>% select(!team) %>% 
  group_by(player) %>%  mutate(age = age - (max(year) - year))

playoff_teams_abb %>% group_by(player) %>%  mutate(age = age - (max(year) - year)) %>% 
  filter(player == 'Ichiro Suzuki')






combined_mlb_df %>% arrange(player)

mlb_df_300_ab <- combined_mlb_df %>% 
  filter(ab>300) %>% 
  mutate(pa = ab+bb+sh+sf+hbp) %>% 
  mutate(bb_pct = round((bb+hbp)/pa, digits = 3), 
         so_pct = round(so/pa, digits = 3),
         babip = round(h/(ab+sf-so), digits = 3)) 





mlb_df_300_ab
arrange_stat_first_n <- function(dataFrame, year, n){
  dataFrame  %>% filter(year == year) %>% arrange(desc(obp)) %>% head(n)
}
mlb_df_top_obp <- tibble()
for (i in seq(2010:2019)){
  mlb_df_top_obp_year <- arrange_stat_first_n(mlb_df_300_ab, i, 5)
  mlb_df_top_obp <- rbind(mlb_df_top_obp, mlb_df_top_obp_year)
}

# mlb_df_top_obp_year <- arrange_stat_first_n(mlb_df_300_ab, 2010, 'obp', 5)
mlb_df_300_ab  %>% filter(year == 2010) %>% arrange(desc(obp)) %>% head(5)

team_obp <- mlb_df_300_ab %>% select(year, team, avg, obp) %>% 
  group_by(year, team) %>%
  summarise(obp = mean(obp), avg = mean(avg)) %>%
  arrange(desc(obp), desc(avg))
WS_mathches <- read_csv(file = 'WSwinners.csv')%>% clean_names()
team_obp_ws <- merge(team_obp, WS_mathches, all = TRUE, by = c('year', 'team')) %>% 
  mutate(champion = if_else(is.na(champion)|champion == F, F, T))
team_obp_ws %>% arrange(desc(avg), desc(obp))
for (i in 2010:2019){
  chart <- team_obp_ws %>% filter(year == i) %>%  ggplot() +
  geom_point(aes(x = as.factor(team), y = obp, color = champion))
  name <- paste('vis', as.character(i), sep = '')
  assign(name, chart)
  name
}
team_obp_ws %>% filter(year == 2010) %>%  ggplot() +
  geom_point(aes(x = as.factor(team), y = avg, color = champion))+
    
vis2010
vis2011
vis2012
vis2013
vis2014
vis2015


