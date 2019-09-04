
### Code to run forecasts for individual countries ###

### Read in libraries
library("truncnorm"); library("tgp"); # for lhs
library("MASS"); # for multivariate normal distribution
require(plyr); # for function count

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/individualCountries/SIRS_indiv.R')
source('code/functions/Fn_initializations.R')
source('code/individualCountries/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')
source('code/functions/replaceLeadingLaggingNAs.R')

### Read in filter function
source('code/individualCountries/EAKF_indiv.R')

### Seasons:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40
N <- 1e5 # population - since no longer split into compartments by commuting!

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)
param.bound <- cbind(theta_low, theta_up)

### Initial state variable values
S0_low <- 0.50; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.001 # proportion of population

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

oevBaseList <- c(1e4, 1e5)
oevDenomList <- c(1.0, 10.0, 50.0)
lambdaList <- c(1.00, 1.02, 1.05)

# task.index <- 1:18
cmd_args = commandArgs(trailingOnly = T)
task.index=as.numeric(cmd_args[1]) # 1:144 # 1:240 # 1:480

oev_base <- oevBaseList[ceiling((task.index - 9) / 9) %% 2 + 1]
oev_denom <- oevDenomList[ceiling((task.index - 3) / 3) %% 3 + 1]
lambda <- lambdaList[ceiling(task.index - 1) %% 3 + 1]
print(paste(oev_base, oev_denom, lambda, sep = '_'))

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 3

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso <- iliiso[, c(1, count.indices + 1)]

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

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

### Loop through countries to generate forecasts:
for (count.index in 1:length(countries)) {
  country <- countries[count.index]
  # print(country)
  gamma <- scalings$gamma[count.index]

  # Get country-specific humidity:
  AH.count <- AH[, count.index]

  # # Get country-specific influenza data:
  # iliiso.count <- iliiso[, count.index + 1]

  # Initiate output data frames:
  outputMetrics <- NULL
  outputOP <- NULL
  outputDist <- NULL
  outputEns <- NULL

  # Loop through seasons:
  for (season in seasons) {
    print(paste(country, season, sep=' '))

    # Get observations for current season:
    tmp <- Fn_dates(season)
    weeks <- tmp$weeks + 3
    start_date <- tmp$start_date
    end_date <- tmp$end_date
    nsn <- tmp$nsn

    obs_i <- iliiso[weeks, count.index + 1]
    syn_i <- syn.dat[weeks, count.index + 1]
    test_i <- test.dat[weeks, count.index + 1]
    pos_i <- pos.dat[weeks, count.index + 1]

    # Check if any data for this country/season:
    if (any(obs_i > 0 & !is.na(obs_i))) {

      # Replace any leading or lagging NAs:
      obs_i <- replaceLeadLag(obs_i)
      syn_i <- replaceLeadLag(syn_i)
      pos_i <- replaceLeadLag(pos_i)

      # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
      test_i[test_i == 0 & !is.na(test_i)] <- NA

      # # Plot:
      # par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
      # plot(obs_i, pch = 20, type = 'b', lty = 1, cex = 0.75)
      # plot(syn_i, pch = 20, type = 'b', lty = 1, cex = 0.75)
      # plot(pos_i, pch = 20, type = 'b', lty = 1, cex = 0.75)
      # plot(test_i, pch = 20, type = 'b', lty = 1, cex = 0.75)
      # # par(mfrow = c(6, 5), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

      # Calculate OEV:
      obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base, oev_denom, tmp_exp = 2.0)
      # print(obs_vars[1:3, ])

      # Get the first and last date of the simulation
      clim_start <- as.numeric(start_date - as.Date(paste('20',
                                                          substr(season, gregexpr('-', season)[[1]][1]-2,
                                                                 gregexpr('-', season)[[1]][1]-1),
                                                          '-01-01', sep=''))) + 1 - 6
      # Number of days in the year at the beginning of the week
      clim_end <- as.numeric(end_date - as.Date(paste('20',
                                                      substr(season, gregexpr('-', season)[[1]][1]-2,
                                                             gregexpr('-', season)[[1]][1]-1),
                                                      '-01-01', sep=''))) + 1
      tm.ini <- clim_start - 1 # the end of the former week
      tm.range <- clim_start:clim_end

      # Now loop through all forecast start weeks!:
      for (ntrn in 5:30) {
        if (!is.na(obs_i[ntrn]) & !is.na(obs_vars[ntrn])) { # make sure there's data at this point

          for (run in 1:num_runs) {
            res <- EAKF_rFC(num_ens = num_ens, tmstep = tmstep, param.bound = param.bound, obs_i = obs_i, ntrn = ntrn, obs_vars = obs_vars,
                            tm.ini = tm.ini, tm.range = tm.range)

            outputMetrics <- rbind(outputMetrics, cbind(country, season, run, gamma, oev_base, oev_denom, lambda, res$metrics))
            outputOP <- rbind(outputOP, cbind(country, season, run, gamma, oev_base, oev_denom, lambda, res$op))
            outputDist <- rbind(outputDist, cbind(country, season, run, gamma, oev_base, oev_denom, lambda, res$dist))
            outputEns <- rbind(outputEns, cbind(country, season, run, gamma, oev_base, oev_denom, lambda, res$ensembles))

            # # Plot forecast results:
            # plot(obs_i, pch = 20, xlab = '', ylab = 'Syn+')
            # lines((1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])], res$op[, 11][res$op[, 4] == 'train'], col = 'steelblue2', lwd = 2.0)
            # lines((ntrn + 1):nsn, res$op[, 11][res$op[, 4] == 'fcast'], col = 'coral', lwd = 2.0)

          }

        }
      }

    }

  }

  write.csv(outputMetrics, file = paste('code/individualCountries/outputs/outputMet_', country, '_', oev_base, '_', oev_denom, '_', lambda, '.csv', sep = ''), row.names = FALSE)
  write.csv(outputOP, file = paste('code/individualCountries/outputs/outputOP_', country, '_', oev_base, '_', oev_denom, '_', lambda,'.csv', sep = ''), row.names = FALSE)
  write.csv(outputDist, file = paste('code/individualCountries/outputs/outputDist_', country, '_', oev_base, '_', oev_denom, '_', lambda,'.csv', sep = ''), row.names = FALSE)
  write.csv(outputEns, file = paste('code/individualCountries/outputs/outputEns_', country, '_', oev_base, '_', oev_denom, '_', lambda,'.csv', sep = ''), row.names = FALSE)

}

### Consolidate output ###
metrics.all <- NULL
output.all <- NULL
dist.all <- NULL
ens.all <- NULL

for (i in countries) {
    a <- read.csv(file = paste0('code/individualCountries/outputs/outputMet_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    metrics.all <- rbind(metrics.all, a)
}
print('Metrics compiled')
for (i in countries) {
    b <- read.csv(file = paste0('code/individualCountries/outputs/outputOP_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    output.all <- rbind(output.all, b)
}
print('OP compiled')
for (i in countries) {
    c <- read.csv(file = paste0('code/individualCountries/outputs/outputDist_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    dist.all <- rbind(dist.all, c)
}
print('Dist compiled')
for (i in countries) {
    d <- read.csv(file = paste0('code/individualCountries/outputs/outputEns_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    ens.all <- rbind(ens.all, d)
}
print('Ens compiled')

write.csv(metrics.all, file = paste0('code/individualCountries/outputs/outputMet_', oev_base, '_', oev_denom, '_', lambda, '.csv'), row.names = FALSE)
write.csv(output.all, file = paste0('code/individualCountries/outputs/outputOP_', oev_base, '_', oev_denom, '_', lambda, '.csv'), row.names = FALSE)
write.csv(dist.all, file = paste0('code/individualCountries/outputs/outputDist_', oev_base, '_', oev_denom, '_', lambda, '.csv'), row.names = FALSE)
write.csv(ens.all, file = paste0('code/individualCountries/outputs/outputEns_', oev_base, '_', oev_denom, '_', lambda, '.csv'), row.names = FALSE)
print('Compiled files written.')

# And delete old files:
for (i in countries) {
  file.remove(paste0('code/individualCountries/outputs/outputMet_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
  file.remove(paste0('code/individualCountries/outputs/outputOP_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
  file.remove(paste0('code/individualCountries/outputs/outputDist_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
  file.remove(paste0('code/individualCountries/outputs/outputEns_', i, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
}
print('Done.')

