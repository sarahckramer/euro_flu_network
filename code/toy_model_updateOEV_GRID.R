
### Fit Network Model to Observed Data ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Set directory (if necessary):
# setwd('/Users/sarahkramer/Desktop/Lab/spatial_transmission/EuropeanNetwork/')
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network.R')

source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/replaceLeadingLaggingNAs.R')
source('code/functions/calc_obsvars.R')

### Read in filter function
source('code/EAKF_network.R')

### Global variables
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
S0_low <- 0.30; S0_up <- 1.00 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population
# QUESTION: upper bound of 0.001 instead?

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

oevBase_list <- c(1e4, 1e5, 1e6)
oevFact_list <- c(0.1, 0.2, 0.5, 1.0)
oevDenom_list <- c(1.0, 5.0, 10.0, 20.0, 100.0)
tmpExpList <- c(1.0, 1.5, 2.0)
lambdaList <- c(1.00, 1.01, 1.03, 1.05)

cmd_args = commandArgs(trailingOnly = T)
task_index=as.numeric(cmd_args[1]) # 1:720
# task.index <- 319

oev_base <- oevBase_list[ceiling(task.index / 240)]
oev_fact <- oevFact_list[ceiling((task.index - 60) / 60) %% 4 + 1]
oev_denom <- oevDenom_list[ceiling((task.index - 12) / 12) %% 5 + 1]
tmp_exp <- tmpExpList[ceiling((task.index - 4) / 4) %% 3 + 1]
lambda <- lambdaList[ceiling(task.index - 1) %% 4 + 1]

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 3

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- 1:21

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso.raw <- iliiso

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

syn.dat.raw <- syn.dat

### Scale data:
scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
for (i in 2:22) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:283, i] <- iliiso[1:283, i] * 1.3
    iliiso[284:495, i] <- iliiso[284:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[1:283, i] <- syn.dat[1:283, i] * 1.3
    syn.dat[284:495, i] <- syn.dat[284:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  } else {
    iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[, i] <- syn.dat[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}
iliiso.scale <- iliiso
syn.dat.scale <- syn.dat

### Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL

### Loop through seasons:
seasons <- c('2010-11', '2011-12')#, '2012-13')#, '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
for (s.index in 1:length(seasons)) {
  season <- seasons[s.index]
  pdf(paste('outputs/plots', oev_base, oev_fact, oev_denom, tmp_exp, lambda, season, '060519.pdf', sep = '_'),
      width = 10, height = 7)
  
  ### Load commuting data:
  load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
  t.comm <- comm.by.year[[s.index]][countries, countries]
  
  ### Set country populations:
  N <- t.comm; n <- length(countries) # w/ commuting
  diag(N) <- unlist(lapply(1:n, function(ix) {
    pop.size$pop[ix] - rowSums(N)[ix]
  }))
  
  ### Get observations for current season:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  
  ### Replace any leading or lagging NAs:
  for (count.index in count.indices) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
    # test_i[, count.index] <- replaceLeadLag(test_i[, count.index])
  }
  
  ### Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  ### Plot:
  par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  matplot(syn_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  matplot(pos_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  matplot(test_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  
  ### Variance of syndromic+ data:
  obs_vars <- calc_obsvars_nTest(syn_i, test_i, pos_i, oev_base, oev_fact, oev_denom, tmp_exp)
  # obs_vars <- calc_obsvars(obs_i, oev_base, oev_denom, oev_denom_tmp)
  matplot(obs_vars, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  
  ### Get the first and last date of the simulation:
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
  
  ### Set ntrn:
  ntrn <- 40
  
  ### Fit to data:
  for (run in 1:num_runs) {
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    updates = TRUE)
    print(table(res[[1]][, 8]))
    print(table(res[[1]][, 9]))
    
    par(mfrow = c(3, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
    for (param.index in 1:5) {
      plot(res[[3]][, param.index], pch = 20, type = 'b',
           xlab = 'Time Since Outbreak Start', ylab = names(res[[3]])[param.index])
    }
    
    resMet.temp <- cbind(season, run, oev_base, oev_fact, oev_denom, tmp_exp, lambda,
                         scalings$gamma, res[[1]])
    names(resMet.temp)[8] <- 'scaling'
    outputMetrics <- rbind(outputMetrics, resMet.temp)
    
    resOP.temp <- cbind(season, run, oev_base, oev_fact, oev_denom, tmp_exp, lambda,
                        res[[2]], res[[3]])
    outputOP <- rbind(outputOP, resOP.temp)
  }
  
  dev.off()
}

write.csv(outputMetrics, file = paste('outputs/outputMet', oev_base, oev_fact, oev_denom,
                                      tmp_exp, lambda, '060519.csv', sep = '_'), row.names = FALSE)
write.csv(outputOP, file = paste('outputs/outputOP', oev_base, oev_fact, oev_denom,
                                 tmp_exp, lambda, '060519.csv', sep = '_'), row.names = FALSE)


