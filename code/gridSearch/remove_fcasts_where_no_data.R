
# In individual country models, forecasts are not performed if data is NA, or if obs_vars is either 0 or NA
# To make for a fair comparison, remove forecasts made for a country when these conditions were met

# Read in all results:
m <- read.csv('results/original/outputMet_111819_pro.csv')
o <- read.csv('results/original/outputOP_111819.csv')
d <- read.csv('results/original/logScores_pt_ot.csv')
e <- read.csv('results/original/logScores_pi.csv')
e.wks <- read.csv('results/original/logScores_1-4wk.csv')

# List countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get observations:
iliiso <- read.csv('data/WHO_data_05-09-19.csv')
iliiso <- iliiso[, c(1, count.indices + 1)]
# here it doesn't matter if they're scaled or not b/c we only care whether or not they are NAs

test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Replace -1 with NA:
for (i in 2:13) {
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

# # And rename the columns:
# names(iliiso) = names(syn.dat) = names(pos.dat) = names(test.dat)

# Load required functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')
source('cluster/functions/calc_obsvars.R')

# Get mini-dataset that can be matched with larger ones later:
m.mini <- unique(m[, c(1, 7:8)])
m.mini$oev = m.mini$obs <- NA

# Loop through seasons to get matching observations:
seasons <- levels(m$season)

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
  oev_base <- 1e4; oev_denom <- 10
  obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
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
m.mini <- m.mini[!is.na(m.mini$obs) & !is.na(m.mini$oev) & m.mini$oev != 0, ]

# Merge m.mini with all important data frames:
m <- merge(m, m.mini, by = c('season', 'country', 'fc_start')) # 9304
o <- merge(o, m.mini, by = c('season', 'country', 'fc_start'))
d <- merge(d, m.mini, by = c('season', 'country', 'fc_start'))
e <- merge(e, m.mini, by = c('season', 'country', 'fc_start'))
e.wks <- merge(e.wks, m.mini, by = c('season', 'country', 'fc_start'))

# Remove relevant columns to put these into correct format:
d$obs <- NULL; e$obs <- NULL; e.wks$obs <- NULL
d$oev <- NULL; e$oev <- NULL; e.wks$oev <- NULL

# Save new results files:
write.csv(m, file = 'results/original/outputMet_110819_pro_PROC.csv', row.names = FALSE)
write.csv(o, file = 'results/original/outputOP_110819_PROC.csv', row.names = FALSE)
write.csv(d, file = 'results/original/logScores_pt_ot_PROC.csv', row.names = FALSE)
write.csv(e, file = 'results/original/logScores_pi_PROC.csv', row.names = FALSE)
write.csv(e.wks, file = 'results/original/logScores_1-4wk_PROC.csv', row.names = FALSE)











