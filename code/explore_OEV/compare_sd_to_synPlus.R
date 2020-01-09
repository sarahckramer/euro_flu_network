
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set strain.
strain <- 'B'
# scaling.index <- 3

# Read in necessary data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
syn.dat <- read.csv(paste0('data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
pos.dat <- read.csv(paste0('data/by_subtype/posprop_', strain, '.csv'))
test.dat <- read.csv('data/testRates_010820.csv')
# test.dat <- read.csv('data/testCounts_052719.csv')
load('data/by_subtype/scalings_noCutoff.RData')

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

# Source relevant functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

# Initiate lists to store OEVs:
obs.list = oev.list = oev.old.list = oev.ratio = vector('list', length(seasons))

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
  obs_vars <- calc_obsvars(obs = obs_i, 1e4, 10)# calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, 0, 2.0, tmp_exp = 2.0)
  obs_vars_old <- calc_obsvars(obs = obs_i, 1e4, 10)
  
  # # Calculate OEVs used in individual country models:
  # obs_vars.indiv <- calc_obsvars(obs = obs_i, 1e4, 10)
  
  # Store results in lists:
  obs.list[[season.index]] <- obs_i
  oev.list[[season.index]] <- obs_vars
  oev.old.list[[season.index]] <- obs_vars_old
  oev.ratio[[season.index]] <- sqrt(obs_vars) / obs_i # want ~0.1-0.5ish
}

# # Look at ratios - want 10-50%-ish of observed values:
# for (i in 1:length(oev.ratio)) {
#   matplot(oev.ratio[[i]], pch = 20, col = viridis(12))
# }
# # quite high early and late, but what's it doing mid-season?
# 
# for (i in 1:length(oev.ratio)) {
#   matplot(oev.ratio[[i]], pch = 20, col = viridis(12), ylim = c(0, 1))
# }
# # FR gets low in final season (H1), but that's about it; NL not so bad; only ES consistently low
# # H3 doesn't look way worse? - eh, last two seasons have ES, FR, NL looking bad
# # 

# Remove Infs in oev.ratio (this is just where obs_i are 0); do we want to look at what the OEV values here are?
for (i in 1:length(oev.ratio)) {
  oev.ratio[[i]][oev.ratio[[i]] == Inf & !is.na(oev.ratio[[i]])] <- NA
}

# And plot (OEV, data, and ratios):
# Format lists for plotting:
obs.df = oev.df = oev.old.df = rat.df = NULL
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
  
  colnames(oev.old.list[[ix]]) <- countries
  oev.old.list[[ix]] <- melt(oev.old.list[[ix]])
  oev.old.list[[ix]]$season <- seasons[ix]
  oev.old.df <- rbind(oev.old.df, oev.old.list[[ix]])
  
  colnames(oev.ratio[[ix]]) <- countries
  oev.ratio[[ix]] <- as.matrix(oev.ratio[[ix]])
  rownames(oev.ratio[[ix]]) <- 1:dim(oev.ratio[[ix]])[1]
  oev.ratio[[ix]] <- melt(oev.ratio[[ix]])
  oev.ratio[[ix]]$season <- seasons[ix]
  rat.df <- rbind(rat.df, oev.ratio[[ix]])
}
names(obs.df) = names(oev.df) = names(oev.old.df) = names(rat.df) = c('time', 'country', 'value', 'season')

# Plots:
p1 <- ggplot(data = obs.df) + labs(x = 'Weeks Since Season Start', y = 'Syn+ (Scaled)') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p2 <- ggplot(data = oev.df) + labs(x = 'Weeks Since Season Start', y = 'OEV') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p3 <- ggplot(data = oev.old.df) + labs(x = 'Weeks Since Season Start', y = 'OEV (Old)') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p4 <- ggplot(data = rat.df) + labs(x = 'Weeks Since Season Start', y = 'Sqrt(OEV)/Syn+') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + geom_abline(intercept = c(0.1, 0.5), slope = 0)# + scale_y_continuous(limits = c(0, 1))
p5 <- ggplot(data = rat.df) + labs(x = 'Weeks Since Season Start', y = 'Sqrt(OEV)/Syn+') + geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + geom_abline(intercept = c(0.1, 0.5), slope = 0) + scale_y_continuous(limits = c(0, 1))

# Save plots to pdf:
pdf(paste0('results/explore_oev/OEV_rat_', strain, '_Old.pdf'), width = 12, height = 7)
print(p1)
print(p2)
# print(p3)
print(p4)
print(p5)
dev.off()

# these rates seem better matched by "old" OEV form so far - except still really high early on with FR (and ES) b/c scaled case counts are really low
# similar for H3 and H1; seems normal to go higher before and after peak
# looks better for 1e4 than 1e5 I think, but still not bad - just especially more high before and after peak; but still looks fine if limit to between 0 and 1
# honestly, "new" version as-is doesn't even look that bad - maybe it's partially that less time is spent within this range? Or the uncertain early values?

# Scaled tests: look at lot more similar by country, and none are too low! (Maybe even too high)
# Comparing countries, they are on a similar comparative scale as "old" OEV
    # But should this be? Shouldn't some countries look worse by comparison?
# Also: Might still need to set some minimum, for very early/late in the season - maybe like 1e4? (or even 1e5)












############################################################################################################################################

# Concern that OEV way too high early on, but otherwise would get too low near peak
# What should sqrt(OEV) look like when obs are zero?
# Scaled test numbers are a much smaller range - low in DE (~0.5), high in LU (~40), but mostly 1-5ish, with a few more like 8-15








