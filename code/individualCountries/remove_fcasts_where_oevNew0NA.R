
# In individual country models, there are fewer times when OEV is 0/NA, since we only take syn+, and not test
# numbers, etc., into account; remove forecasts where the network wouldn't produce them, for fairer comparison

# Read in all results:
# m <- read.csv(list.files(pattern = 'Met_pro.csv')) # m is already read in
o <- read.csv(list.files(pattern = 'OP'))
d <- list()
logScore.files <- list.files(pattern = 'logScores')
for (i in 1:length(logScore.files)) {
  d[[i]] <- read.csv(logScore.files[i])
}
rm(i)

# List countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# # Get observations:
# iliiso <- read.csv('../../data/WHO_data_05-09-19.csv')
# iliiso <- iliiso[, c(1, count.indices + 1)]
# # here it doesn't matter if they're scaled or not b/c we only care whether or not they are NAs
# 
# test.dat <- read.csv('../../data/testCounts_052719.csv')
# syn.dat <- read.csv('../../data/synDatCounts_060519.csv')
# pos.dat <- read.csv('../../data/posProp_060519.csv')
# 
# test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
# pos.dat <- pos.dat[, c(1, count.indices + 1)]
# 
# # Replace -1 with NA:
# for (i in 2:13) {
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
#   syn.dat[, i][syn.dat[, i] < 0] <- NA
#   pos.dat[, i][pos.dat[, i] < 0] <- NA
#   test.dat[, i][test.dat[, i] < 0] <- NA
# }

# Or, for individual "strains":
iliiso <- read.csv(paste0('../../data/by_subtype/WHO_data_', strain, '_SCALED.csv'))

test.dat <- read.csv('../../data/testRates_010820.csv')
syn.dat <- read.csv(paste0('../../data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
pos.dat <- read.csv(paste0('../../data/by_subtype/posprop_', strain, '.csv'))

# test.dat <- test.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# # And rename the columns:
# names(iliiso) = names(syn.dat) = names(pos.dat) = names(test.dat)

# Load required functions:
source('../../cluster/functions/Fn_initializations.R')
source('../../cluster/functions/replaceLeadingLaggingNAs.R')
source('../../cluster/functions/calc_obsvars.R')

# Get mini-dataset that can be matched with larger ones later:
m.mini <- unique(m.store[, c(1, 7:8)])
m.mini$oev = m.mini$obs <- NA

# Loop through seasons to get matching observations:
seasons <- levels(m.store$season)

for (season in seasons) {
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  nsn <- tmp$nsn
  
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # replace leading/lagging NAs:
  for (count.index in 1:n) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # Variance of syndromic+ data:
  oev_base <- 0; oev_denom <- 2.0
  obs_vars <- 1e5 + calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
                                       oev_base, oev_denom, tmp_exp = 2.0)
  
  # Match obs_i/obs_vars to appropriate season/country/fc_start:
  for (count.index in 1:n) {
    for (fc in unique(m.mini$fc_start)) {
      m.mini$obs[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_i[fc - 39, count.index]
      m.mini$oev[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_vars[fc - 39, count.index]
      
    }
  }
  
}

# Remove from m.mini where obs is NA, or oev NA or 0:
m.mini <- m.mini[!is.na(m.mini$oev) & m.mini$oev != 0, ]
# honestly, this doesn't remove much

# Merge m.mini with all important data frames:
m.store <- merge(m.store, m.mini, by = c('season', 'country', 'fc_start'))
o <- merge(o, m.mini, by = c('season', 'country', 'fc_start'))
for (i in 1:length(d)) {
  d[[i]] <- merge(d[[i]], m.mini, c('season', 'country', 'fc_start'))
}

# Remove relevant columns to put these into correct format:
m.store$obs <- NULL; m.store$oev <- NULL
o$obs <- NULL; o$oev <- NULL
for (i in 1:length(d)) {
  d[[i]]$obs <- NULL; d[[i]]$oev <- NULL
}

# Put correct column order back:
m.store <- m.store[, c(1, 4:8, 3, 2, 9:78)]
o <- o[, c(1, 4, 6:8, 3, 9:11, 2, 12:13, 18, 14:17, 19:20, 25, 21:24)]

# Go ahead and remove unnecessary columns from m:
m.store <- m.store[, c(1:9, 12:13, 15:32, 39, 43, 47, 51, 53:56, 60:78)]

# Save new results files:
write.csv(m.store, file = 'outputMet_pro_PROC.csv', row.names = FALSE)
write.csv(o, file = 'outputOP_PROC.csv', row.names = FALSE)

for (i in 1:length(d)) {
  d.temp <- d[[i]]
  write.csv(d.temp, file = paste0('PROC_', logScore.files[i]), row.names = FALSE)
}

# Clean up!
rm(list = ls())

