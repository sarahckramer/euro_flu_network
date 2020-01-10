
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set strain.
strain <- 'A(H1)'
# scaling.index <- 3

# Read in necessary data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
syn.dat <- read.csv(paste0('data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
pos.dat <- read.csv(paste0('data/by_subtype/posprop_', strain, '.csv'))
test.dat <- read.csv('data/testRates_010820.csv')
test.dat.unscaled <- read.csv('data/testCounts_052719.csv')
load('data/by_subtype/scalings_noCutoff.RData')

test.dat.unscaled <- test.dat.unscaled[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Get relevant seasons:
if (strain == 'A(H1)') {
  seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
} else if (strain == 'A(H3)') {
  seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
} else if (strain == 'B') {
  seasons <- c('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18') # B
} else {
  print('AAAAGGGGGHHHHHH')
}

# Source relevant functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

# Initiate lists to store OEVs:
obs.list = oev.list = oev.list.unscaled = oev.old.list = vector('list', length(seasons))

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
  test_i_unscaled <- test.dat.unscaled[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # Replace leading/lagging NAs:
  for (count.index in 1:length(countries)) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  test_i_unscaled[test_i_unscaled == 0 & !is.na(test_i_unscaled)] <- NA
  
  # Calculate OEVs used in network model:
  obs_vars <- 1e5 + calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, 0, 2.0, tmp_exp = 2.0)
  # obs_vars[obs_vars < 1e4 & !is.na(obs_vars)] <- 1e4
  # obs_vars <- obs_vars + 1e5
  
  obs_vars2 <- 1e5 + calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i_unscaled, posprops = pos_i, 0, 2.0, tmp_exp = 2.0)
  
  obs_vars_old <- calc_obsvars(obs = obs_i, 1e5, 10)
  
  # # Calculate OEVs used in individual country models:
  # obs_vars.indiv <- calc_obsvars(obs = obs_i, 1e4, 10)
  
  # Store results in lists:
  obs.list[[season.index]] <- obs_i
  oev.list[[season.index]] <- obs_vars
  oev.old.list[[season.index]] <- obs_vars_old
  oev.list.unscaled[[season.index]] <- obs_vars2
}

# Format to plot:
obs.df = oev.df = oev.df2 = oev.old.df = NULL
for (ix in 1:length(seasons)) {
  colnames(obs.list[[ix]]) <- countries
  obs.list[[ix]] <- as.matrix(obs.list[[ix]])
  rownames(obs.list[[ix]]) <- 1:dim(obs.list[[ix]])[1]
  obs.list[[ix]] <- melt(obs.list[[ix]])
  obs.list[[ix]]$season <- seasons[ix]
  obs.df <- rbind(obs.df, obs.list[[ix]])
  
  colnames(oev.list[[ix]]) <- countries
  oev.list[[ix]] <- melt(oev.list[[ix]])
  oev.list[[ix]]$season <- seasons[ix]
  oev.df <- rbind(oev.df, oev.list[[ix]])
  
  colnames(oev.list.unscaled[[ix]]) <- countries
  oev.list.unscaled[[ix]] <- melt(oev.list.unscaled[[ix]])
  oev.list.unscaled[[ix]]$season <- seasons[ix]
  oev.df2 <- rbind(oev.df2, oev.list.unscaled[[ix]])
  
  colnames(oev.old.list[[ix]]) <- countries
  oev.old.list[[ix]] <- melt(oev.old.list[[ix]])
  oev.old.list[[ix]]$season <- seasons[ix]
  oev.old.df <- rbind(oev.old.df, oev.old.list[[ix]])
}
names(obs.df) = names(oev.df) = names(oev.df2) = names(oev.old.df) = c('time', 'country', 'value', 'season')

# Plot!:
p1 <- ggplot(data = oev.df, aes(x = country, y = log(value))) + geom_boxplot(fill = 'lightblue2') + theme_bw() +
  labs(x = '', y = 'Log(OEV)', title = paste0('OEV (Tests Scaled): ', strain))
p2 <- ggplot(data = oev.df2, aes(x = country, y = log(value))) + geom_boxplot(fill = 'lightblue2') + theme_bw() +
  labs(x = '', y = 'Log(OEV)', title = paste0('OEV (Tests Unscaled): ', strain))
p3 <- ggplot(data = oev.old.df, aes(x = country, y = log(value))) + geom_boxplot(fill = 'lightblue2') + theme_bw() +
  labs(x = '', y = 'Log(OEV)', title = paste0('OEV (Old): ', strain))

pdf(paste0('results/explore_oev/OEV_boxplots_', strain, '.pdf'), width = 12, height = 8)
print(p1)
print(p2)
print(p3)
dev.off()

rm(list = ls())















