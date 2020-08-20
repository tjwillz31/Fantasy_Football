library(tidyverse)
library(ggplot2)

# set working directory
setwd("~/Fun/Fantasy Articles/WR RZ TD Regression")

# read in data
WR_TDs <- read.csv('WR_Rec_TDs_since_2014.csv')
draft_years <- read.csv('WR_Draft_Year.csv')
raw_stats <- read.csv('PP_TD_Opp.csv')

# create an initial DF for yearly stats
stats <- data.frame(name = character(), season = integer(), games = integer(), rz_snaps = integer(), 
                    rz_snap_share = double(), rz_target_share = double(), targets_5 = integer(), targets_10 = integer(),
                    rz_rec = integer())

# add a row for each player season
for (i in 1:nrow(raw_stats)) {
  for (j in 1:7) {
    if (!is.na(raw_stats[i, j + 44])) {
      stats <- stats %>% add_row(name = raw_stats$Name[i], 
                                 season = j + 2012,
                                 games = raw_stats[i, j + 44],
                                 rz_snaps = raw_stats[i, j + 37],
                                 rz_snap_share = raw_stats[i, j + 16],
                                 rz_target_share = raw_stats[i, j + 9],
                                 targets_5 = raw_stats[i, j + 30],
                                 targets_10 = raw_stats[i, j + 23],
                                 rz_rec = raw_stats[i, j + 2])
    } 
  }
}

# add columns for last year's stats for each metric
stats$ls_games <- 0
stats$ls_rz_snaps <- 0
stats$ls_rz_snap_share <- 0
stats$ls_rz_target_share <- 0
stats$ls_targets_5 <- 0
stats$ls_targets_10 <- 0
stats$ls_rz_rec <- 0

# loop through each player and add last year's stats where possible
for (i in 1:nrow(stats)) {
  x <- subset(stats, name == stats$name[i] & season == stats$season[i] - 1)
  if (nrow(x) > 0) {
    stats$ls_games[i] <- x[[3]]
    stats$ls_rz_snaps[i] <- x[[4]]
    stats$ls_rz_snap_share[i] <- x[[5]]
    stats$ls_rz_target_share[i] <- x[[6]]
    stats$ls_targets_5[i] <- x[[7]]
    stats$ls_targets_10[i] <- x[[8]]
    stats$ls_rz_rec[i] <- x[[9]]
  }
}

# add draft year in order to filter out rookie seasons since there isn't previous year data
#stats <- merge(x=stats, y=draft_years, by.x="name", by.y="Name", all.x=TRUE)
#stats <- stats %>% filter(season != Draft_Year)

# filter to include only seasons with 8 or more games in the current and previous season
corr_stats <- stats %>% filter(games > 7 & ls_games > 7)

# filter to later seasons for metrics that have only been tracked since then
corr_stats18 <- corr_stats %>% filter(season > 2017) 
corr_stats19 <- corr_stats %>% filter(season > 2018) 

# check year to year correlations of each metric
rsq_rz_snaps <- summary(lm(rz_snaps ~ ls_rz_snaps, data=corr_stats19))$r.squared
rsq_rz_snap_share <- summary(lm(rz_snap_share ~ ls_rz_snap_share, data=corr_stats19))$r.squared
rsq_rz_target_share <- summary(lm(rz_target_share ~ ls_rz_target_share, data=corr_stats))$r.squared
rsq_targets_5 <- summary(lm(targets_5 ~ ls_targets_5, data=corr_stats18))$r.squared
rsq_targets_10 <- summary(lm(targets_10 ~ ls_targets_10, data=corr_stats18))$r.squared
rsq_rz_rec <- summary(lm(rz_rec ~ ls_rz_rec, data=corr_stats))$r.squared

# plot last season and next season redzone target share
ggplot(corr_stats, aes(ls_rz_target_share, rz_target_share)) +
  geom_count() +
  scale_size_area() +
  geom_smooth() +
  labs(y = "Next Season Redzone Target Share", x = "Last Season Redzone Target Share", 
       title = "WR Year Over Year Redzone Receiving Target Share", 
       subtitle = "2014 - 2019", caption = "Data from playerprofiler.com")
ggsave("YoY_RZ_TS.png")

# create the slim df for TDs
WR_TDs_slim <- WR_TDs[, c("PP_name", "season", "rz_td", "non_rz_td", "total_td")]

# join TD stats to opportunity stats
stats <- merge(x=stats, y=WR_TDs_slim, by.x=c("name", "season"), by.y=c("PP_name", "season"), all.x=TRUE)
stats <- stats %>% filter(season > 2013)
stats$rz_td[is.na(stats$rz_td)] <- 0
stats$non_rz_td[is.na(stats$non_rz_td)] <- 0
stats$total_td[is.na(stats$total_td)] <- 0
write.csv(stats, "WR_TDs_and_Opp.csv")

# create a new dataframe for seasons with 8 or more games for predicting same season RZ TDs using RZ Opp stats
mod_stats <- stats %>% filter(games > 7 & !is.na(rz_target_share) & !is.na(rz_rec))

# split 2019 data into its own set for model application and then train/test split the remaining data
mod_stats_19 <- mod_stats %>% filter(season == 2019)
mod_stats <- mod_stats %>% filter(season < 2019)
set.seed(12493)
sample <- sample.int(n = nrow(mod_stats), size = floor(.75*nrow(mod_stats)), replace = F)
train <- mod_stats[sample, ]
test  <- mod_stats[-sample, ]

# train a model using redzone target share and redzone receptions
lm <- lm(rz_td ~ rz_target_share + rz_rec, data = train)
summary(lm)

lm_ts <- lm(rz_td ~ rz_target_share, data = train)
summary(lm_ts)

lm_rec <- lm(rz_td ~ rz_rec, data = train)
summary(lm_rec)

preds <- predict(lm, newdata = test)
rsq_lm <- (cor(preds, test$rz_td))^2
rmse_lm <- sqrt(mean((preds - test$rz_td)^2))

# apply best model to 2019 stats
preds19 <- predict(lm, newdata = mod_stats_19)
mod_stats_19$predicted_rz_td <- preds19
mod_stats_19$differential <- mod_stats_19$rz_td - mod_stats_19$preds

buys <- mod_stats_19 %>% slice_min(diff, n = 10) %>% select("name", "rz_target_share", "rz_rec", "rz_td", "predicted_rz_td", "differential")
sells <- mod_stats_19 %>% slice_max(diff, n = 10) %>% select("name", "rz_target_share", "rz_rec", "rz_td", "predicted_rz_td", "differential")
