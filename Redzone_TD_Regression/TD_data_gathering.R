#devtools::install_github("mrcaseb/nflfastR")
#install.packages("Rcpp")
# load libraries
library(nflfastR)
library(tidyverse)
library(nflscrapR)

# pull data for each season
games_2013 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2013.rds'))
games_2014 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds'))
games_2015 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds'))
games_2016 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds'))
games_2017 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds'))
games_2018 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
games_2019 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

# combine data into one df
pbp <- rbind(games_2013, games_2014, games_2015, games_2016, games_2017, games_2018, games_2019)

# filter to only pass plays in the regular season that resulted in TDs
passTDs <- pbp %>% filter(season_type == 'REG' & !is.na(posteam) & pass_attempt == 1 & td_team == posteam)

# add a flag for if the play was in the redzone
passTDs <- passTDs %>% mutate(redzone = if_else(yardline_100 < 21, 1, 0))

# get counts of TDs in the redzone and not
TD_in_RZ <- passTDs %>% group_by(redzone) %>% summarise(count = n())
TD_in_RZ

# get counts of TDs by receiver by redzone per season
TD_per_Season <- passTDs %>% group_by(receiver_id, receiver, posteam, season, redzone) %>% summarise(count = n())

# write to csv
write.csv(TD_per_Season, 'TD_per_Season.csv')

# testing
test <- passTDs %>% filter(receiver_id == "32013030-2d30-3032-3335-3036f327363a")
