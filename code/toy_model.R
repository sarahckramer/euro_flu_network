
### Fit Network Model to Observed Data ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Read in model functions
source('cluster/SIRS_network.R')
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/Fn_checkxnobounds.R')
source('cluster/functions/Util.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

### Read in filter function
source('cluster/EAKF_network.R')

### Seasons:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
S0_low <- 0.55; S0_up <- 0.85
sd_low <- 0.05; sd_up <- 0.18
I0_low <- 0; I0_up <- 0.00005

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs
lambda <- 1.02 # inflation factor for the ensemble filters c(1.00, 1.01, 1.02, 1.03, 1.05, 1.075?)
oev_base <- 1e4; oev_denom <- 10.00

# 1e4/10/1.1 (tends toward D = 8-10); 1e4/10/1.05 (doesn't get the blips as well, but looks better near end); 1e4/1/1.05 (gets some trailing up after outbreak...; fit R0diff much smaller (~0.2 vs. ~0.6));
# 1e5/10/1.05 - has trounle keeping early values toward 0
# 1e4/10/1.02 actually almost looks better, although not much change (R0diff ~ 0.34)
# old OEV, 1e4/10/1.02: doesn't have early upticks, but totally misses outbreak in SK completely
# old OEV, 1e5/10/1.02: maybe actually better, but still can't get SK
# old OEV, 1e4/1/1.02: also misses SK; doesn't do well with CZ either
# 1e4/10/1.02, keep NAs: very similar but somehow less uptick at the end of the outbreak? actually, not necessarily

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 1

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Read in humidity data
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza data
# iliiso <- read.csv('data/WHO_data_05-09-19_SCALED.csv') # in same order as "countries" vector
iliiso <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv') # in same order as "countries" vector

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')

# syn.dat <- read.csv('data/synDatCounts_060519_SCALED.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')

syn.dat <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
pos.dat <- read.csv('data/by_subtype/posprop_A(all).csv')
# ADD 18-19 - change these four files

test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
scalings <- read.csv('data/by_subtype/scalings_frame_A(all).csv')

for (i in 2:13) {
  # iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  # syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

### Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL
outputOPParams <- NULL
outputDist <- NULL
outputEns <- NULL

### Load commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
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

### Set up parameter bounds:
param.bound <- cbind(c(S0_low, sd_low, rep(I0_low, n), theta_low),
                     c(S0_up, sd_up, rep(I0_up, n), theta_up))

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

# matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)

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
# par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
# matplot(syn_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
# matplot(pos_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
# matplot(test_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
# par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

# Variance of syndromic+ data:
obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
                               oev_base, oev_denom, tmp_exp = 2.0)
# obs_vars <- calc_obsvars(obs = as.matrix(obs_i), oev_base, oev_denom)
# matplot(obs_vars, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
### QUESTION: OEV is way higher for a couple of countries than most of the others - is this fair?
# Scaled syn+ - also scale syn? Otherwise not sure the OEV would be appropriate for the rates we're fitting to
# but not test#, b/c this should tell us something about error
# but maybe do all of this somehow BEFORE scaling? or do we have to fit to counts, not rates?
# AT DE PT pretty large, Luxembourg and Slovakia greatest; very small are FR, ES, SE (just for 2010-11 season)

# obs_vars <- obs_vars / 10

print(obs_vars[1:4, ])
print(obs_i[which(obs_vars == 0 | is.na(obs_vars), arr.ind = TRUE)])

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
                  updates = TRUE, do.reprobing = FALSE)
  
  outputMetrics <- rbind(outputMetrics, cbind(season, run, oev_base, oev_denom, lambda, scalings$gamma, res$metrics))
  outputOP <- rbind(outputOP, cbind(season, run, oev_base, oev_denom, lambda, res$opStates))
  outputOPParams <- rbind(outputOPParams, cbind(season, run, oev_base, oev_denom, lambda, res$trainParams))
  outputDist = rbind(outputDist, cbind(season, run, oev_base, oev_denom, lambda, res$dist))
  outputEns = rbind(outputEns, cbind(season, run, oev_base, oev_denom, lambda, res$ensembles))
}

# }

colnames(outputMetrics)[6] <- 'scaling'
# I actually think all the other colnames are fine as-is...

load('data/by_subtype/scalings_by_subtype_120219.RData')
outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- scalings.new[[1]][13]
# FR has an alternative scaling for earlier

# write.csv(outputMetrics, file = 'code/checks/outputMetrics_1e5_remove.csv', row.names = FALSE)


# Different countries have different oev at different times, and this doesn't always match up with
# what the ensemble members are doing

