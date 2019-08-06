
### Fit Network Model to Observed Data ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Read in model functions
source('code/SIRS_network.R')
source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')
source('code/functions/replaceLeadingLaggingNAs.R')

### Read in filter function
source('code/EAKF_network.R')

### Seasons:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
param.bound <- cbind(theta_low, theta_up)

### Initial state variable values
S0_low <- 0.50; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.001 # proportion of population

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs
lambda <- 1.03 # inflation factor for the ensemble filters c(1.00, 1.01, 1.02, 1.03, 1.05, 1.075?)
oev_base <- 1e5; oev_denom <- 10.00

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 3

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Read in humidity data
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso <- iliiso[, c(1, count.indices + 1)]
# iliiso.raw <- iliiso

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# syn.dat.raw <- syn.dat

### Scale data:
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
# iliiso.scale <- iliiso
# syn.dat.scale <- syn.dat

### Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL
outputOPParams <- NULL
outputDist <- NULL
outputEns <- NULL

### Load commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')

season.index <- 1

### Loop through seasons:
# for (season.index in 1:length(seasons)) {
season <- seasons[season.index]

# Get commuting data:
t.comm <- comm.by.year[[season.index]]
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

matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)

# Replace any leading or lagging NAs:
for (count.index in 1:n) {
  obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
  syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
  pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  # test_i[, count.index] <- replaceLeadLag(test_i[, count.index])
}
### QUESTION! It's possible leaving this step out would allow there to be a bit more ensemble noise going into the outbreak, since the model wouldn't
###           be fitting everything down to zero.

# Replace 0s in test_i w/ NA (b/c can't divide by 0!):
test_i[test_i == 0 & !is.na(test_i)] <- NA

# Plot:
par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(syn_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(pos_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(test_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

# Variance of syndromic+ data:
obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base, oev_denom, tmp_exp = 2.0)
matplot(obs_vars, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
### QUESTION: OEV is way higher for a couple of countries than most of the others - is this fair?
# Scaled syn+ - also scale syn? Otherwise not sure the OEV would be appropriate for the rates we're fitting to
# but not test#, b/c this should tell us something about error
# but maybe do all of this somehow BEFORE scaling? or do we have to fit to counts, not rates?
# AT DE PT pretty large, Luxembourg and Slovakia greatest; very small are FR, ES, SE (just for 2010-11 season)

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

# Set ntrn:
ntrn <- 30 # final ntrn

# Run!:
for (run in 1:num_runs) {
  res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                  updates = FALSE, do.reprobing = FALSE)
  
  outputMetrics <- rbind(outputMetrics, cbind(season, run, oev_base, oev_denom, lambda, scalings$gamma, res$metrics))
  outputOP <- rbind(outputOP, cbind(season, run, oev_base, oev_denom, lambda, res$opStates))
  outputOPParams <- rbind(outputOPParams, cbind(season, run, oev_base, oev_denom, lambda, res$trainParams))
  outputDist = rbind(outputDist, cbind(season, run, oev_base, oev_denom, lambda, res$dist))
  outputEns = rbind(outputEns, cbind(season, run, oev_base, oev_denom, lambda, res$ensembles))
}

# }

colnames(outputMetrics)[6] <- 'scaling'
# I actually think all the other colnames are fine as-is...

outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- 1.3
# FR has an alternative scaling for earlier

write.csv(outputMetrics, file = 'outputMetrics_check.csv', row.names = FALSE)


# Different countries have different oev at different times, and this doesn't always match up with
# what the ensemble members are doing
# Scale data to have similar AR? I guess rather scale to be similar to AR in synthetic outbreaks

