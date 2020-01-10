
### Look through a range of oev_base and oev_denom to see which produce "best-looking" OEVs for all subtypes ###

# Load packages:
library(ggplot2)

# Set ranges to explore:
oev_bases <- c(0.0, 0.01, 0.1, 0.25) #c(0.0, 0.05, 0.1, 0.3)#, 0.5, 0.6)
oev_denoms <- c(1, 2, 10, 100) #c(1, 10, 100, 400) #c(3, 10, 25, 100)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set strain.
strain <- 'B'

# Read in data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
syn.dat <- read.csv(paste0('data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
pos.dat <- read.csv(paste0('data/by_subtype/posprop_', strain, '.csv'))
test.dat <- read.csv('data/testRates_010820.csv')

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

# Read in necessary functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

# Save to pdf:
pdf(paste0('results/explore_oev/OEV_rat_', strain, '_gridSearch.pdf'), width = 12, height = 7)

# Plot out data:
obs.list <- vector('list', length(seasons))
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
  
  # Replace leading/lagging NAs:
  for (count.index in 1:length(countries)) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
  }
  
  # Store results in list:
  obs.list[[season.index]] <- obs_i
}
obs.df <- NULL
for (ix in 1:length(seasons)) {
  colnames(obs.list[[ix]]) <- countries
  obs.list[[ix]] <- as.matrix(obs.list[[ix]])
  rownames(obs.list[[ix]]) <- 1:dim(obs.list[[ix]])[1]
  obs.list[[ix]] <- melt(obs.list[[ix]])
  obs.list[[ix]]$season <- seasons[ix]
  obs.df <- rbind(obs.df, obs.list[[ix]])
}
names(obs.df) <- c('time', 'country', 'value', 'season')

p1 <- ggplot(data = obs.df) + labs(x = 'Weeks Since Season Start', y = 'Syn+ (Scaled)') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
print(p1)

# Loop through bases/denoms:
for (o1 in oev_bases) {
  for (o2 in oev_denoms) {
    oev.list = oev.ratio = vector('list', length(seasons))
    
    # Loop through all seasons:
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
      obs_vars <- 1e5 + calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base = o1, oev_denom = o2, tmp_exp = 2.0)
      # obs_vars_old <- calc_obsvars(obs = obs_i, 1e4, 10)
      # obs_vars[obs_vars == 0 & !is.na(obs_vars)] <- NA # allow zeros to be set to min - preserve more points, and this is what would be done with a baseline anyway
      # obs_vars[obs_vars < 1e4 & !is.na(obs_vars)] <- 1e4
      
      # Store results in lists:
      oev.list[[season.index]] <- obs_vars
      oev.ratio[[season.index]] <- sqrt(obs_vars) / obs_i
    }
    
    # Remove Infs in oev.ratio (this is just where obs_i are 0):
    for (i in 1:length(oev.ratio)) {
      oev.ratio[[i]][oev.ratio[[i]] == Inf & !is.na(oev.ratio[[i]])] <- NA
    }
    
    # Format lists for plotting:
    oev.df = rat.df = NULL
    for (ix in 1:length(seasons)) {
      colnames(oev.list[[ix]]) <- countries
      oev.list[[ix]] <- melt(oev.list[[ix]])
      oev.list[[ix]]$season <- seasons[ix]
      oev.df <- rbind(oev.df, oev.list[[ix]])
      
      colnames(oev.ratio[[ix]]) <- countries
      oev.ratio[[ix]] <- as.matrix(oev.ratio[[ix]])
      rownames(oev.ratio[[ix]]) <- 1:dim(oev.ratio[[ix]])[1]
      oev.ratio[[ix]] <- melt(oev.ratio[[ix]])
      oev.ratio[[ix]]$season <- seasons[ix]
      rat.df <- rbind(rat.df, oev.ratio[[ix]])
    }
    names(oev.df) = names(rat.df) = c('time', 'country', 'value', 'season')
    
    # Plot!:
    p2 <- ggplot(data = oev.df) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = paste(o1, o2, sep = '_')) + geom_line(aes(x = time, y = value, group = season, colour = season)) +
      facet_wrap(~ country, scales = 'free_y') + theme_bw()
    p3 <- ggplot(data = rat.df) + labs(x = 'Weeks Since Season Start', y = 'Sqrt(OEV)/Syn+') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
      facet_wrap(~ country, scales = 'free_y') + theme_bw() + geom_abline(intercept = c(0.1, 0.5), slope = 0)# + scale_y_continuous(limits = c(0, 1))
    p4 <- ggplot(data = rat.df) + labs(x = 'Weeks Since Season Start', y = 'Sqrt(OEV)/Syn+') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
      facet_wrap(~ country, scales = 'free_y') + theme_bw() + geom_abline(intercept = c(0.1, 0.5), slope = 0) + scale_y_continuous(limits = c(0, 1))
    
    print(p2)
    print(p3)
    print(p4)
    
  }
}

dev.off()

rm(list = ls())

