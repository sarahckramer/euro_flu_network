
# This is just to plot out preliminary results
# Eventually run in cluster and get full output files

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Read in model functions
source('/Users/sarahkramer/Desktop/Lab/spatial_transmission/network_model/SIRS_network.R')
source('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/code/Fn_initializations.R')
source('/Users/sarahkramer/Desktop/Lab/spatial_transmission/network_model/functions/Fn_checkxnobounds.R')

### Read in filter function
source('/Users/sarahkramer/Desktop/Lab/spatial_transmission/network_model/synthetic/EAKF_rFC_Synth.R')

### Headers for output functions:




### Ensemble member numbers kept:
to.keep <- c(10, 38, 40, 88)

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 # "data" are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8;
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2;
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)
param.bound <- cbind(theta_low, theta_up)

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs
lambda <- 1.05 # inflation factor for the ensemble filters
oev_denom <- 5 # denominator for observation error variance
num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 1

### Specify the country for which we are performing a forecast
# countries <- c('BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IS', 'IT',
#                'LU', 'NL', 'PL', 'PT', 'SK', 'SI', 'ES', 'SE', 'UK')
# count.indices <- c(2:20) # just those countries w/ train data, too

countries <- c('BE', 'FR', 'DE')
count.indices <- c(2, 6:7)

### Set population sizes and # of countries used
pop.size <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/matrices/comm_mat_by_year_ADJ_04-16.RData')
t.comm <- apply(simplify2array(comm.by.year.adj), 1:2, mean); rm(comm.by.year.adj)
t.comm <- t.comm[countries, countries]

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

### Read in random train data
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/train_04-30.RData')
t.rand <- t.rand[countries, countries]

### Read in humidity data
ah <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/data/ah_04-30_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza "data":
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/by_country_COUNTS_ERR.RData')

### Format data for fitting:
to.fit <- vector('list', length(to.keep))
for (i in 1:length(to.keep)) {
  newI.temp <- NULL
  for (j in 1:length(countries)) {
    newI.temp <- rbind(newI.temp, newI.ERR[[j]][i, ])
  }
  to.fit[[i]] <- newI.temp
}
rm(newI.ERR, newI.temp)

### Set important values for fitting:
tm.ini <- 270 - 1
tm.range <- 270:600

### Loop through ensemble members of interest:
ens.index <- 1
# for (ens.index in 1:4) {
  
  # Initialize output data frames (here?):
  
  
  
  # Get truth parameter values:
  load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/all_params.RData')
  true.params <- c(all.params[[4]][to.keep[ens.index]], all.params[[3]][to.keep[ens.index]],
                   all.params[[1]][to.keep[ens.index]], all.params[[2]][to.keep[ens.index]])
  print(true.params)
  
  # Get "data":
  obs_i <- t(to.fit[[ens.index]])
  matplot(obs_i, type = 'b', pch = 20, lty = 1, col = viridis(length(countries)), cex = 0.75,
          xlab = 'Weeks from Outbreak Start', ylab = 'Syn+ Counts')
  
  # Calculate "observational" error variance:
  tmp <- matrix(0, nrow = nrow(obs_i), ncol = ncol(obs_i))
  for (ix in 1:length(countries)) {
    for (jx in 4:length(obs_i[, ix])) {
      tmp[jx, ix] <- mean(obs_i[(jx - 3):(jx - 1), ix])
    }
  }
  obs_vars <- (1e5 + (tmp ** 2) / 5) / oev_denom
  
  # Set initial time and time range:
  tm.ini <- 270 - 1
  tm.range <- 270:600
  
  # Fit through end of outbreak:
  ntrn <- 39 # 4 weeks from "end" of outbreak
  
  # Model fitting!
  pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/run_ens10_lam105_oevd5.pdf',
      width = 10, height = 8)
  for (run in 1:num_runs) {
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn, obs_vars, tm.ini, tm.range)
    
    
    
  }
  dev.off()
  
  
  
  
# }


















