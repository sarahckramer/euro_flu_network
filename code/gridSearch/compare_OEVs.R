
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

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
for (i in 2:21) {
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
oev_base <- 1e5
oev_denom <- 1

# Source relevant functions:
source('code/functions/Fn_initializations.R')
source('code/functions/calc_obsvars.R')
source('code/functions/replaceLeadingLaggingNAs.R')

# List seasons:
seasons <- c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")

# Initiate lists to store OEVs:
oev.new = oev.old = vector('list', length(seasons))

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
  
  # Calculate OEVs used in network model:
  obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base, oev_denom, tmp_exp = 2.0)
  
  # Calculate OEVs used in individual country models:
  obs_vars.indiv <- calc_obsvars(obs = obs_i, oev_base, oev_denom)
  
  # Store results in lists:
  oev.new[[season.index]] <- obs_vars
  oev.old[[season.index]] <- obs_vars.indiv
}

# Format lists for plotting:
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

# Plot new and old by country:
p1 <- ggplot(data = oev.new.df) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
print(p1)

p2 <- ggplot(data = oev.old.df) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
print(p2)

# Only for season?
p1 <- ggplot(data = oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p2 <- ggplot(data = oev.old.df[oev.old.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
print(p1); print(p2)
# absolute values remain similar, but structure over time changes quite a bit

# Save plots to pdf:
pdf('code/gridSearch/plots/OEV_comp.pdf', width = 12, height = 7)
print(p1); print(p2)
dev.off()





















