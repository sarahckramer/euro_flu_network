
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set strain.
strain <- 'A(H3)'

# Read in necessary data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
syn.dat <- read.csv(paste0('data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
pos.dat <- read.csv(paste0('data/by_subtype/posprop_', strain, '.csv'))
test.dat <- read.csv('data/testRates_010820.csv')

# syn.dat.unscaled <- syn.dat.unscaled[, c(1, count.indices + 1)]
# test.dat <- test.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

if (strain == 'A(H1)') {
  seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
} else if (strain == 'A(H3)') {
  seasons <- c('2011-12', '2013-14', '2014-15', '2016-17') # H3
} else if (strain == 'B') {
  seasons <- c('2010-11', '2011-12', '2012-13', '2014-15', '2015-16', '2017-18') # B
} else {
  print('AAAAGGGGGHHHHHH')
}

# We'll set the oev_base/oev_denom as we go

# Source relevant functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

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
  obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, 0, 2.0, tmp_exp = 2.0)
  
  # Calculate OEVs used in individual country models:
  obs_vars.indiv <- calc_obsvars(obs = obs_i, 1e4, 10)

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

# Combine:
oev.new.df$model <- 'New'; oev.old.df$model <- 'Old'
oev.df <- rbind(oev.old.df, oev.new.df)

### PLOTS ###

# Only for season?
p1 <- ggplot(data = oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'New') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p2 <- ggplot(data = oev.old.df[oev.old.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'Old') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()

# Save plots to pdf:
pdf(paste0('results/explore_oev/OEV_comp_', strain, '.pdf'), width = 12, height = 7)
print(p2)
print(p1)

# Now plot by country, comparing new to old:
oev.df <- oev.df[oev.df$time < 33, ]
oev.df$group <- paste(oev.df$season, oev.df$model, sep = '_'); oev.df$group <- factor(oev.df$group)
p1 <- ggplot(data = oev.df, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p1)
# dev.off()

# What about the order at which the countries are weighted?
p1 <- ggplot(oev.old.df[oev.old.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Old') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 3) + theme_bw() + scale_y_log10()
p2 <- ggplot(oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'New') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 3) + theme_bw() + scale_y_log10()
grid.arrange(p1, p2, ncol = 1)

dev.off()

rm(list = ls())










