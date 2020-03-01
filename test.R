##x-special/nautilus-clipboard
#copy
#file:///home/rodrigo/%C3%81rea%20de%20Trabalho/Studying/mlb-player-stats-Batters-2019.csv


library(dplyr)
library(tidyr)
library(tibble)
library(abjData)
library(readr)
library(stringr)
library(janitor)
pnud_min %>% View()
abjutils::calc_dig()
pnud_min %>% glimpse() #funcao da gatinha
typeof(pnud_min)
class(pnud_min)

pnud_min %>% 
  select(ano, muni, uf) %>% 
  filter(uf=='AC')
pnud_min %>% 
  select(ano, regiao, uf, idhm) %>% 
  filter(uf %in% c('SP', 'MG') | idhm > .5, ano == 2010)

pnud_min %>% 
  filter(str_detect(muni, '^[HG]|S$'))

qq <- pnud_min %>% 
  pull(uf)
pnud_min %>% 
   mutate(uf=1)%>%
  mutate(uf=qq)
class(qq)
qq


pnud_min


mlb_batting_2019 <- read_csv(file = "mlb-player-stats-Batters-2019.csv")%>% clean_names()
mlb_batting_2019_350 <- mlb_batting_2019 %>% filter(ab>300) 
mlb_batting_2019 %>% glimpse()
mlb_batting_2019 %>%filter(ab>400) %>%  select(player, ab, obp, slg) %>%
  arrange(desc(slg))
mlb_batting_2019_350 %>% group_by(team) %>% 
  summarise(n=n(), avg_obp = mean(obp), avg_slg = mean(slg))%>% 
  arrange(desc(avg_slg), desc(avg_obp))
mlb_batting_2019_350 %>% count(team, sort=T)
pnud_min %>% 
  filter(ano == 2010) %>% 
  count(regiao, sort = TRUE) %>% 
  mutate(prop = n / sum(n), prop = scales::percent(prop))
pnud_min %>% 
  select(uf, muni, ano, starts_with('idhm_')) %>% 
  gather(qq, idhm, starts_with('qq')) #%>%
  # arrange(desc(idhm))
pnud_min %>% 
  select(muni, uf, ano, starts_with('idhm_')) %>% 
  gather(tipo_idhm, idhm, starts_with('idhm_')) %>% 
  spread(ano, idhm)

# mlb_batting_2019_350 %>% 
#   select(player, year, obp, slg) %>% 
MVP_MLB_2012_2019 <- function(dataFrame, column = "player"){
  #print('passou')
  for (j in seq(1:length(dataFrame[,as.character(column)]))){
    #print('passou')
    if (i == 2012 & dataFrame[j,"player"] %in% c('Buster Posey', 'Miguel Cabrera')){
      dataFrame[j, 'MVP'] <- T
      #print(dataFrame)
    }
    else if (i == 2013 & dataFrame[j,as.character(column)] %in% c('Andrew McCutchen', 'Miguel Cabrera')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2014 & dataFrame[j,as.character(column)] %in% c('Clayton Kershaw', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2015 & dataFrame[j,as.character(column)] %in% c('Bryce Harper', 'Josh Donaldson')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2016 & dataFrame[j,as.character(column)] %in% c('Kris Bryant', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2017 & dataFrame[j,as.character(column)] %in% c('Giancarlo Stanton', 'Jose Altuve')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2018 & dataFrame[j,as.character(column)] %in% c('Christian Yelich', 'Mookie Betts')){
      dataFrame[j, 'MVP'] <- T
    }
    else if (i == 2019 & dataFrame[j,as.character(column)] %in% c('Cody Bellinger', 'Mike Trout')){
      dataFrame[j, 'MVP'] <- T
    }
    else{
      dataFrame[j, 'MVP'] <- F
    }  
  }
  return(dataFrame)
}
combined_mlb_df <- tibble()
for (i in 2012:2019){
  path_file <- paste('mlb-player-stats-Batters-', as.character(i), '.csv', sep='')
  mlb_df <- read_csv(file = path_file)%>% clean_names()
  df <- paste('mlb_batting_', as.character(i), sep='')
  mlb_df <- cbind(year=i, mlb_df)
  # MVP <- mlb_df %>% filter
  mlb_df <- MVP_MLB_2012_2019(mlb_df)
  combined_mlb_df <- rbind(combined_mlb_df, mlb_df)
  assign(df, mlb_df)
  # print(as.character(i))
}
combined_mlb_df_ab_300 <- combined_mlb_df %>% filter(ab>300)
combined_mlb_df_ab_300 %>% filter(year == 2019, player %in% c('Mike Trout', 'Bryan Reynolds')) %>% 
  select(player, pos, team, year, ab, hr, avg, obp, slg, ops, MVP) %>% arrange(desc(avg))
avg_by_year <- combined_mlb_df_ab_300 %>% 
  group_by(year) %>% 
  summarise(league_avg = mean(avg))
mlb_df_avg <- merge(combined_mlb_df_ab_300, avg_by_year, by = "year")
mlb_df_avg %>% filter(year==2017, avg > 0.280) %>% arrange(desc(avg)) %>% 
  select(player, ab, avg, obp, slg, hr, rbi, MVP)
mlb_df_avg %>% filter(player == 'Giancarlo Stanton') %>% arrange(desc(avg)) %>% 
  select(player, year, ab, avg, obp, slg, hr, rbi, MVP)
mlb_df_avg
first_table <- mlb_df_avg %>% select(player, year, ab, avg, obp, slg, hr, rbi, run, league_avg) %>% 
  filter(year == 2019, player %in% c('Mike Trout', 'Bryan Reynolds')) %>%
  arrange(desc(avg))
first_table
first_table[1, 'player'] <- 'Player A'
first_table[2, 'player'] <- 'Player B'

