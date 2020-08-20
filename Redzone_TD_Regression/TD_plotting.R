library(tidyverse)
library(ggplot2)

# set working directory
setwd("~/Fun/Fantasy Articles/WR RZ TD Regression")

# read in data
TDs_per_Season <- read.csv('TD_per_Season.csv')
player_names <- read.csv('NFLfastR_to_PlayerProfiler_Names.csv')
draft_years <- read.csv('WR_Draft_Year.csv')

# combine rows for players who played for multiple teams in one season
TDs_per_Season <- TDs_per_Season %>% group_by(receiver_id, receiver, season, redzone) %>% summarise(count = sum(count))

# create the dataframe for current season and past season TD totals
TDs <- TDs_per_Season %>% distinct(receiver_id, season, .keep_all = TRUE)
drops <- c("redzone","count")
TDs <- TDs[ , !(names(TDs) %in% drops)]
TDs <- TDs %>% drop_na()

# add columns for current season and previous season redzone/non-rz TDs
TDs$rz_td <- 0
TDs$non_rz_td <- 0
TDs$total_td <- 0
TDs$ls_rz_td <- 0
TDs$ls_non_rz_td <- 0
TDs$ls_total_td <- 0

# loop through each player and grab their TD counts
for (i in 1:nrow(TDs)) {
  x <- subset(TDs_per_Season, receiver_id == TDs$receiver_id[i] & season == TDs$season[i] & redzone == 1)$count
  if (length(x) > 0) {
    TDs$rz_td[i] <- x
  }
  x <- subset(TDs_per_Season, receiver_id == TDs$receiver_id[i] & season == TDs$season[i] & redzone == 0)$count
  if (length(x) > 0) {
    TDs$non_rz_td[i] <- x
  }
  x <- subset(TDs_per_Season, receiver_id == TDs$receiver_id[i] & season == TDs$season[i] - 1 & redzone == 1)$count
  if (length(x) > 0) {
    TDs$ls_rz_td[i] <- x
  }
  x <- subset(TDs_per_Season, receiver_id == TDs$receiver_id[i] & season == TDs$season[i] - 1 & redzone == 0)$count
  if (length(x) > 0) {
    TDs$ls_non_rz_td[i] <- x
  }
}

# add rz and non-rz TDs for total TDs
TDs$total_td <- TDs$rz_td + TDs$non_rz_td
TDs$ls_total_td <- TDs$ls_rz_td + TDs$ls_non_rz_td

# filter out 2013 since we don't have last season data for it (2012)
TDs <- TDs %>% filter(season > 2013)

# join names table for cleaner, playerprofiler names and positions
names_slim <- player_names[, c("NFR_receiver_id", "PP_name", "Position")]
TDs <- merge(x=TDs, y=names_slim, by.x="receiver_id", by.y="NFR_receiver_id", all.x=TRUE)

# filter to only WRs for this analysis
TDs_WRs <- TDs %>% filter(Position == 'WR')

# add draft year to TDs in order to filter out rookie seasons since there isn't previous year data
TDs_WRs <- merge(x=TDs_WRs, y=draft_years, by.x="PP_name", by.y="Name", all.x=TRUE)
#write.csv(TDs_WRs, 'WR_Rec_TDs_since_2014.csv')
TDs_WRs <- TDs_WRs %>% filter(season != Draft_Year)

# plot last season and next season total TDs for WRs
ggplot(TDs_WRs, aes(ls_total_td, total_td)) +
  geom_count() +
  scale_size_area() +
  geom_smooth() +
  labs(y = "Next Season TDs", x = "Last Season TDs", title = "WR Year Over Year Receiving TDs", 
       subtitle = "2014 - 2019", caption = "Data from nflfastR & playerprofiler.com")

# plot last season and next season redzone TDs for WRs
ggplot(TDs_WRs, aes(ls_rz_td, rz_td)) +
  geom_count() +
  scale_size_area() +
  geom_smooth() +
  labs(y = "Next Season Redzone TDs", x = "Last Season Redzone TDs", title = "WR Year Over Year Redzone Receiving TDs", 
       subtitle = "2014 - 2019", caption = "Data from nflfastR & playerprofiler.com")
ggsave("YoY_RZ_TDs.png")

rsq_total <- summary(lm(total_td ~ ls_total_td, data=TDs_WRs))$r.squared 
rsq_rz <- summary(lm(rz_td ~ ls_rz_td, data=TDs_WRs))$r.squared
rsq_non_rz <- summary(lm(non_rz_td ~ ls_non_rz_td, data=TDs_WRs))$r.squared
