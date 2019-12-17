
# *** Code to compare set-up and outputs to those from python code *** #

### Set up model:
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)

# Read in model functions:
source('cluster/SIRS_network.R')
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/Fn_checkxnobounds.R')
source('cluster/functions/Util.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')
source('cluster/EAKF_network.R')

# Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

# Parameter ranges:
init_parms = vector('list', 5)
for (i in 1:5) {
  parms_temp <- read.table(paste0('python/initial_parms/parms', i - 1, '.txt'), header = FALSE, sep = '\t')
  init_parms[[i]] <- parms_temp
}; rm(parms_temp)

# Parameters for the filters:
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

# seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18') # ADD '2018-19'
seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
# seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
# seasons <- c('2010-11', '2011-12', '2012-13', '2014-15', '2015-16', '2017-18') # B

oev_base <- 1e4 #oevBase_list[ceiling((task.index - 26) / 26) %% 2 + 1]
oev_denom <- 10.0 #oevDenom_list[ceiling((task.index - 78) / 78) %% 3 + 1]
lambda <- 1.02 #lambdaList[ceiling((task.index - 26) / 26) %% 3 + 1]

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 2

# Specify countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set population sizes and # of countries used:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in humidity data:
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# ah <- read.csv('data/ah_MEAN_120519.csv')
# AH <- rbind(ah, ah)

# Read in influenza data:
# iliiso <- read.csv('data/WHO_data_05-09-19_SCALED.csv') # in same order as "countries" vector
# iliiso <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv')
iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')

# Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')

# syn.dat <- read.csv('data/synDatCounts_060519_SCALED.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(all).csv')

syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
pos.dat <- read.csv('data/by_subtype/posprop_A(H1).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')
# 
# syn.dat <- read.csv('data/by_subtype/synDatCounts_B_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_B.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Scale data:
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
# # # note: these are the "old" scalings
# scalings <- read.csv('data/by_subtype/scalings_frame_A(all).csv')
scalings <- read.csv('data/by_subtype/scalings_frame_A(H1).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_A(H3).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_B.csv')

for (i in 2:13) {
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

# Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL
outputOPParams <- NULL
outputDist <- NULL
outputEns <- NULL

### Main fitting code:
for (season in seasons) {
  
  # Get commuting data:
  load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
  t.comm <- comm.by.year[[which(seasons == season)]]
  t.comm <- t.comm[countries, countries]
  
  # Get population counts:
  N <- t.comm; n <- length(countries) # w/ commuting
  diag(N) <- unlist(lapply(1:n, function(ix) {
    pop.size$pop[ix] - rowSums(N)[ix]
  }))
  # population and commuting data are COUNTS, not RATES
  
  # Get observations for current season:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # Replace any leading or lagging NAs:
  for (count.index in 1:n) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
    # test_i[, count.index] <- replaceLeadLag(test_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # Variance of syndromic+ data:
  obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
                                 oev_base, oev_denom, tmp_exp = 2.0)
  
  # Get the first and last date of the simulation:
  clim_start <- as.numeric(start_date - as.Date(paste('20',
                                                      substr(season, gregexpr('-', season)[[1]][1]-2,
                                                             gregexpr('-', season)[[1]][1]-1),
                                                      '-01-01', sep=''))) + 1 - 6
  
  ### Number of days in the year at the beginning of the week:
  clim_end <- as.numeric(end_date - as.Date(paste('20',
                                                  substr(season, gregexpr('-', season)[[1]][1]-2,
                                                         gregexpr('-', season)[[1]][1]-1),
                                                  '-01-01', sep=''))) + 1
  tm.ini <- clim_start - 1 # the end of the former week
  tm.range <- clim_start:clim_end
  
  
  for (run in 1:num_runs) {
    ntrn <- 10 # this is how I set it in python, so whatever
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    updates = FALSE, do.reprobing = FALSE)
    
    # outputMetrics <- rbind(outputMetrics, cbind(season, run, oev_base, oev_denom, lambda, scalings$gamma, res$metrics))
    # outputOP <- rbind(outputOP, cbind(season, run, oev_base, oev_denom, lambda, res$opStates))
    # outputOPParams <- rbind(outputOPParams, cbind(season, run, oev_base, oev_denom, lambda, res$trainParams))
    # outputDist = rbind(outputDist, cbind(season, run, oev_base, oev_denom, lambda, res$dist))
    # outputEns = rbind(outputEns, cbind(season, run, oev_base, oev_denom, lambda, res$ensembles))
    # outputVars = rbind(outputVars, cbind(season, run, lambda, res$vars))
  }
  
}

colnames(outputMetrics)[6] <- 'scaling'

load('data/by_subtype/scalings_by_subtype_120219.RData')
outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- scalings.new[[2]][13]#1.3

# Read in and compare xprior after initiating model run:
res.py <- read.table(file = 'python/results/xprior_ens_0_2010-11.txt', header = FALSE, sep = ',')
res.py <- res.py[, 1:20]

res <- as.data.frame(res)

all.equal(res, res.py)






