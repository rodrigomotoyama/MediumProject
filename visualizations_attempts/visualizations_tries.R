library(dplyr)
library(tidyr)
library(tibble)
library(abjData)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)
library(grid)
library(gridExtra)

combined_mlb_df <- tibble()
for (i in 2012:2019){
  path_file <- paste('mlb-player-stats-Batters-', as.character(i), '.csv', sep='')
  mlb_df <- read_csv(file = path_file)%>% clean_names()
  df <- paste('mlb_batting_', as.character(i), sep='')
  mlb_df <- cbind(year=i, mlb_df)
  # MVP <- mlb_df %>% filter
  # mlb_df <- MVP_MLB_2012_2019(mlb_df)
  combined_mlb_df <- rbind(combined_mlb_df, mlb_df)
  assign(df, mlb_df)
  # print(as.character(i))
}
mlb_df_300_ab <- combined_mlb_df %>% 
  filter(ab>300) %>% 
  mutate(pa = ab+bb+sh+sf+hbp) %>% 
  mutate(bb_pct = round((bb+hbp)/pa, digits = 3), 
         so_pct = round(so/pa, digits = 3),
         babip = round(h/(ab+sf-so), digits = 3))
trout_df_300_ab <- mlb_df_300_ab %>% 
  filter(player == 'Mike Trout')
harper_trout_df_300_ab <- mlb_df_300_ab %>% 
  filter(player %in% c(
                       # 'Aaron Judge', 'Cody Bellinger', 
                       # 'Ketel Marte', 
                       'Mike Trout',
                       # 'Mookie Betts', 'Christian Yelich',
                       'Paul Goldschmidt', 'Jose Altuve',
                       'Miguel Cabrera'
                       )
         ) %>% 
  group_by(player)

ggplot(harper_trout_df_300_ab)+
  geom_point(mapping = aes(year, rbi, color = player))
ggplot(mlb_df_300_ab)+
  geom_point(aes(x = r, y = slg))

ggplot(mlb_df_300_ab)+
  geom_boxplot(aes(x = as.factor(team), y = slg))+
  labs(x = "team", y = "slugging")
ggplot(mlb_df_300_ab)+
  geom_boxplot(aes(x = as.factor(team), y = bb_pct))

mlb_df_team_year <- mlb_df_300_ab %>% 
  mutate(team_year = paste(team, year, sep='_'))

ggplot(mlb_df_team_year %>% filter(year == 2012))+
  geom_boxplot(aes(x = as.factor(team_year), y = slg))+
  labs(x = "team per year", y = "slugging")

team_stats <- mlb_df_team_year %>% 
  group_by(team_year) %>% 
  summarise(avg = mean(avg), obp = mean(obp), slg = mean(slg), run = sum(r), hitsPerRuns = sum(h)/sum(r))

team_stats %>% ggplot()+
  geom_point(mapping = aes(x = run, y = obp))

plot_slg <- team_stats %>% ggplot()+
  geom_point(mapping = aes(x = run, y = slg, colour = 'slg'))+
  labs(x = "runs", y = "avg/obp/slg")
plot_obp <- team_stats %>% ggplot()+
  geom_point(mapping = aes(x = run, y = obp, colour = 'obp'))+
  labs(x = "runs", y = "avg/obp/slg")
plot_avg <- team_stats %>% ggplot()+
  geom_point(mapping = aes(x = run, y = avg, colour = 'avg'))+
  labs(x = "runs", y = "avg/obp/slg")
grid.arrange(plot_slg, plot_obp, plot_avg)
team_stats %>% ggplot()+
  geom_point(mapping = aes(x = run, y = hitsPerRuns))










