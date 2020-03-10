library(dplyr)
library(tidyr)
library(tibble)
library(abjData)
library(readr)
library(stringr)
library(janitor)
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

mlb_df_300_ab <- combined_mlb_df %>% 
  filter(ab>300) %>% 
  mutate(pa = ab+bb+sh+sf+hbp) %>% 
  mutate(bb_pct = round((bb+hbp)/pa, digits = 3), 
         so_pct = round(so/pa, digits = 3),
         babip = round(h/(ab+sf-so), digits = 3)) 
