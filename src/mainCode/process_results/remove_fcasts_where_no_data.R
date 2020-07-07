
### In individual country models, forecasts are not performed if data is NA (b/c nothing to forecast on) ###
### To make for a fair comparison, remove forecasts made for a country when these conditions were met ###

# Read in all results:
m <- m.store #read.csv(list.files(pattern = 'Met_pro.csv'))
o <- read.csv(list.files(pattern = 'OP_'))
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

# Change country labels in O:
o$country <- countries[o$country + 1]
o$country <- factor(o$country)

# # Get observations:
iliiso <- read.csv(paste0('../../../data/WHO_data_', strain, '_SCALED.csv'))

# Load required functions:
source('../../../src/mainCode/functions/Fn_initializations.R')
source('../../../src/mainCode/functions/replaceLeadingLaggingNAs.R')
source('../../../src/mainCode/functions/calc_obsvars.R')

# Get mini-dataset that can be matched with larger ones later:
m.mini <- unique(m[, c(1:2, 32)]) # want: season, country, fc_start
m.mini$oev = m.mini$obs <- NA

# Loop through seasons to get matching observations:
seasons <- levels(m$season)

for (season in seasons) {
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  nsn <- tmp$nsn
  
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  
  # replace leading/lagging NAs:
  for (count.index in 1:n) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
  }
  
  # Variance of syndromic+ data:
  obs_vars <- calc_obsvars(obs = as.matrix(obs_i), oev_base = 1e5, oev_denom = 10)
  
  # Match obs_i/obs_vars to appropriate season/country/fc_start:
  for (count.index in 1:n) {
    for (fc in unique(m.mini$fc_start)) {
      m.mini$obs[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_i[fc - 39, count.index]
      m.mini$oev[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_vars[fc - 39, count.index]
    }
  }
  
}

# Get metrics file for forecasts where no obs:
m.temp <- merge(m, m.mini, by = c('season', 'country', 'fc_start'))
m.temp <- m.temp[is.na(m.temp$obs), ]
write.csv(m.temp, file = paste0('../../by_subtype/network_', strain, '/outputMet_obsNA.csv'), row.names = FALSE)
# write.csv(m.temp, file = 'outputMet_obsNA.csv', row.names = FALSE)
rm(m.temp)

# Remove from m.mini where obs is NA, or oev NA or 0:
m.mini <- m.mini[!is.na(m.mini$obs) & !is.na(m.mini$oev) & m.mini$oev != 0, ]

# Merge m.mini with all important data frames:
m <- merge(m, m.mini, by = c('season', 'country', 'fc_start')) # 9304
o <- merge(o, m.mini, by = c('season', 'country', 'fc_start'))
for (i in 1:length(d)) {
  d[[i]] <- merge(d[[i]], m.mini, c('season', 'country', 'fc_start'))
}

# Remove relevant columns to put these into correct format:
m$obs <- NULL; m$oev <- NULL
o$obs <- NULL; o$oev <- NULL
for (i in 1:length(d)) {
  d[[i]]$obs <- NULL; d[[i]]$oev <- NULL
}

# Clean up!
rm(m.store, m.mini, iliiso, obs_i, obs_vars, tmp, weeks, nsn, count.index, fc, season, seasons)

