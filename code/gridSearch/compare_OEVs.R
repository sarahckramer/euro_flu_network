
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Read in syn+ data:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso <- iliiso[, c(1, count.indices + 1)]

# Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Scale syn+ and syn data:
scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
scalings <- scalings[count.indices, ]
# note: these are the "old" scalings
for (i in 2:dim(iliiso)[2]) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:286, i] <- iliiso[1:286, i] * 1.3
    iliiso[287:495, i] <- iliiso[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[1:286, i] <- syn.dat[1:286, i] * 1.3
    syn.dat[287:495, i] <- syn.dat[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  } else {
    iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[, i] <- syn.dat[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

# Set oev_base and oev_denom:
oev_base <- 1e4
oev_denom <- 10

# Source relevant functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

# List seasons:
seasons <- c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")

# Initiate lists to store OEVs:
oev.new.alt = oev.new = oev.old = vector('list', length(seasons))

# Loop through seasons and calculate both "old" and "new" type of OEV:
for (season.index in 1:length(seasons)) {
  season <- seasons[season.index]
  
  # Get weeks for current season:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  
  # Reduce data to current season:
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # Replace leading/lagging NAs:
  for (count.index in 1:length(countries)) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # # Calculate OEVs used in network model:
  obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base, oev_denom, tmp_exp = 2.0)
  
  # Calculate OEVs used in individual country models:
  obs_vars.indiv <- calc_obsvars(obs = obs_i, oev_base, oev_denom)
  
  # And calculate OEVs after "scaling" posprops:
  #pos_i <- pos_i * 100000
  obs_vars.alt <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base = 0.5, oev_denom = 1.0, tmp_exp = 2.0)
  
  # Store results in lists:
  oev.new[[season.index]] <- obs_vars
  oev.old[[season.index]] <- obs_vars.indiv
  oev.new.alt[[season.index]] <- obs_vars.alt
}

# Format lists for plotting:
oev.alt.df = oev.new.df = oev.old.df = NULL
for (ix in 1:length(seasons)) {
  colnames(oev.new[[ix]]) <- countries
  oev.new[[ix]] <- melt(oev.new[[ix]])
  oev.new[[ix]]$season <- seasons[ix]
  oev.new.df <- rbind(oev.new.df, oev.new[[ix]])
  
  colnames(oev.old[[ix]]) <- countries
  oev.old[[ix]] <- melt(oev.old[[ix]])
  oev.old[[ix]]$season <- seasons[ix]
  oev.old.df <- rbind(oev.old.df, oev.old[[ix]])
  
  colnames(oev.new.alt[[ix]]) <- countries
  oev.new.alt[[ix]] <- melt(oev.new.alt[[ix]])
  oev.new.alt[[ix]]$season <- seasons[ix]
  oev.alt.df <- rbind(oev.alt.df, oev.new.alt[[ix]])
}

names(oev.new.df) = names(oev.old.df) = names(oev.alt.df) = c('time', 'country', 'value', 'season')

# Combine:
oev.new.df$model <- 'New'; oev.old.df$model <- 'Old'; oev.alt.df$model <- 'Alt'
oev.df <- rbind(oev.old.df, oev.alt.df)

# # Plot new and old by country:
# p1 <- ggplot(data = oev.new.df) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
#   geom_line(aes(x = time, y = value, group = season, colour = season)) +
#   facet_wrap(~ country, scales = 'free_y') + theme_bw()
# print(p1)
# 
# p2 <- ggplot(data = oev.old.df) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
#   geom_line(aes(x = time, y = value, group = season, colour = season)) +
#   facet_wrap(~ country, scales = 'free_y') + theme_bw()
# print(p2)

# Only for season?
p1 <- ggplot(data = oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p2 <- ggplot(data = oev.old.df[oev.old.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p3 <- ggplot(data = oev.alt.df[oev.alt.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
# print(p1); print(p2)
# absolute values remain similar, but structure over time changes quite a bit

# Save plots to pdf:
#pdf('code/gridSearch/plots/OEV_comp.pdf', width = 12, height = 7)
#print(p2); print(p3)
# dev.off()

# Now plot by country, comparing new to old:
oev.df <- oev.df[oev.df$time < 33, ]
oev.df$group <- paste(oev.df$season, oev.df$model, sep = '_'); oev.df$group <- factor(oev.df$group)
p1 <- ggplot(data = oev.df, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p1)

# # Try denominators:
# oev.df$value[oev.df$model == 'New'] <- oev.df$value[oev.df$model == 'New'] * 2 / 5
# p1 <- ggplot(data = oev.df, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
#   facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
# print(p1)
# # even dividing by 2 or 5 could help, but may make peak OEV way too low

### Try calculating first, then scaling:
# Read in data fresh:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

iliiso <- iliiso[, c(1, count.indices + 1)]
test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Replace -1 with NA:
for (i in 2:dim(iliiso)[2]) {
  iliiso[, i][iliiso[, i] < 0] <- NA
  syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

# Calculate OEV with unscaled data:
oev.new = oev.old = vector('list', length(seasons))
for (season.index in 1:length(seasons)) {
  season <- seasons[season.index]
  
  # Get weeks for current season:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  
  # Reduce data to current season:
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # Replace leading/lagging NAs:
  for (count.index in 1:length(countries)) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # Calculate OEVs used in network model:
  obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base = 0.5, oev_denom = 1.0, tmp_exp = 2.0)
  
  # Calculate OEVs used in individual country models:
  obs_vars.indiv <- calc_obsvars(obs = obs_i, oev_base, oev_denom)
  
  # Store results in lists:
  oev.new[[season.index]] <- obs_vars
  oev.old[[season.index]] <- obs_vars.indiv
}

# Compile and check unscaled:
oev.new.df = oev.old.df = NULL
for (ix in 1:length(seasons)) {
  colnames(oev.new[[ix]]) <- countries
  oev.new[[ix]] <- melt(oev.new[[ix]])
  oev.new[[ix]]$season <- seasons[ix]
  oev.new.df <- rbind(oev.new.df, oev.new[[ix]])
  
  colnames(oev.old[[ix]]) <- countries
  oev.old[[ix]] <- melt(oev.old[[ix]])
  oev.old[[ix]]$season <- seasons[ix]
  oev.old.df <- rbind(oev.old.df, oev.old[[ix]])
}
names(oev.new.df) = names(oev.old.df) = c('time', 'country', 'value', 'season')
oev.new.df$model <- 'New'; oev.old.df$model <- 'Old'
oev.df.unscaled <- rbind(oev.new.df, oev.old.df)

oev.df.unscaled <- oev.df.unscaled[oev.df.unscaled$time < 33, ]
oev.df.unscaled$group <- paste(oev.df.unscaled$season, oev.df.unscaled$model, sep = '_'); oev.df.unscaled$group <- factor(oev.df.unscaled$group)
oev.df.unscaled <- oev.df.unscaled[!is.na(oev.df.unscaled$value), ]
p2 <- ggplot(data = oev.df.unscaled, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p2)
# # these have a similar range between countries as the scaled versions, but some are quite low, and I think scaling would make sense
# # relationship between old and new changes for some countries: AT, BE, NL especially

# Scale:
oev.df.scaled <- oev.df.unscaled
for (country in countries) {
  if (country == 'FR') {
    oev.df.scaled$value[oev.df.scaled$country == country & oev.df.scaled$season %in% seasons[1:4]] <-
      oev.df.scaled$value[oev.df.scaled$country == country & oev.df.scaled$season %in% seasons[1:4]] * 1.3
    oev.df.scaled$value[oev.df.scaled$country == country & oev.df.scaled$season %in% seasons[5:8]] <-
      oev.df.scaled$value[oev.df.scaled$country == country & oev.df.scaled$season %in% seasons[5:8]] * scalings$gamma[which(countries == country)]
  } else {
    oev.df.scaled$value[oev.df.scaled$country == country] <- oev.df.scaled$value[oev.df.scaled$country == country] * scalings$gamma[which(countries == country)]
  }
}

# Replot:
p3 <- ggplot(data = oev.df.scaled, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p3)
# okay, obviously the same relationships between old and new, but between-country changes might be interesting

p1 <- ggplot(data = oev.df.unscaled[oev.df.unscaled$model == 'New', ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Unscaled') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 4) + theme_bw() + scale_y_log10()
p2 <- ggplot(data = oev.df.scaled[oev.df.scaled$model == 'New', ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Scaled') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 4) + theme_bw() + scale_y_log10()
grid.arrange(p1, p2)
# scalings only slightly increase how close different countries are, but they do change the ordering of the countries

# And plot OEV_old and OEV_new, comparing pre- and post-scaling:
oev.df.scaled$scaling <- 'Post'
oev.df$scaling <- 'Pre'
oev.df <- rbind(oev.df, oev.df.scaled)

oev.df$model[oev.df$model == 'Alt'] <- 'New'

oev.df$group <- paste(oev.df$group, oev.df$scaling, sep = '_'); oev.df$group <- factor(oev.df$group)

p1 <- ggplot(data = oev.df[oev.df$model == 'Old', ], aes(x = time, y = value, group = group, colour = scaling)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = 'Scaling Timing')
p2 <- ggplot(data = oev.df[oev.df$model == 'New', ], aes(x = time, y = value, group = group, colour = scaling, lty = scaling)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = 'Scaling Timing')
print(p2)
# scaling before or after makes no difference; obviously

dev.off()












