
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
# source('code/individualCountries/EAKF_indiv_R0mn.R')

### Seasons:
# seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
# seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
# seasons <- c('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18') # B

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40
N <- 1e5 # population - since no longer split into compartments by commuting!

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up)

# D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8
# D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2
# theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)
# theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)

param.bound <- cbind(theta_low, theta_up)

### Initial state variable values
# S0_low <- 0.55; S0_up <- 0.85
# S0_low <- 0; S0_up <- 1.0
S0_low <- 0.3; S0_up <- 0.9
I0_low <- 0; I0_up <- 0.00005

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

# oevBaseList <- c(1e4, 1e5)
# oevDenomList <- c(1.0, 10.0, 50.0)
# lambdaList <- c(1.00, 1.02, 1.05)
# 
# # task.index <- 1:18
# cmd_args = commandArgs(trailingOnly = T)
# task.index=as.numeric(cmd_args[1]) # 1:144 # 1:240 # 1:480

oev_base <- 0.0 #0.3 or 1e4 #oevBaseList[ceiling((task.index - 9) / 9) %% 2 + 1]
oev_denom <- 2.0 #1.0 or 10.0 #oevDenomList[ceiling((task.index - 3) / 3) %% 3 + 1]
lambda <- 1.05 #lambdaList[ceiling(task.index - 1) %% 3 + 1]

# oev_denom <- 100
# lamdba <- 1.05

print(paste(oev_base, oev_denom, lambda, sep = '_'))

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 2

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza data
# iliiso <- read.csv('data/WHO_data_05-09-19_SCALED.csv') # in same order as "countries" vector
# iliiso <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
iliiso <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')
print(dim(iliiso))

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testRates_010820.csv')

# syn.dat <- read.csv('data/synDatCounts_060519_SCALED.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(all).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(H1).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')

syn.dat <- read.csv('data/by_subtype/synDatCounts_B_SCALED.csv')
pos.dat <- read.csv('data/by_subtype/posprop_B.csv')

# test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

### Scale data:
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
# note: these are the "old" scalings
# scalings <- read.csv('data/by_subtype/scalings_frame_A(all).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_A(H1).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_A(H3).csv')
scalings <- read.csv('data/by_subtype/scalings_frame_B.csv')

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
      obs_vars <- 1e5 + calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, oev_base, oev_denom, tmp_exp = 2.0)
      # obs_vars <- calc_obsvars(obs = as.matrix(obs_i), oev_base, oev_denom)
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
print('Forecasting completed.')

### Consolidate output ###
metrics.all <- NULL
output.all <- NULL
dist.all <- NULL
ens.all <- NULL

for (country in countries) {
    a <- read.csv(file = paste0('code/individualCountries/outputs/outputMet_', country, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    metrics.all <- rbind(metrics.all, a)
}
for (country in countries) {
    b <- read.csv(file = paste0('code/individualCountries/outputs/outputOP_', country, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    output.all <- rbind(output.all, b)
}
for (country in countries) {
    c <- read.csv(file = paste0('code/individualCountries/outputs/outputDist_', country, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    dist.all <- rbind(dist.all, c)
}
for (country in countries) {
    d <- read.csv(file = paste0('code/individualCountries/outputs/outputEns_', country, '_', oev_base, '_', oev_denom, '_', lambda, '.csv'))
    ens.all <- rbind(ens.all, d)
}
print('Results compiled.')

write.csv(metrics.all, file = paste0('code/individualCountries/outputs/outputMet_', oev_base, '_', oev_denom, '_', lambda, '_B_OEVnew.csv'), row.names = FALSE)
write.csv(output.all, file = paste0('code/individualCountries/outputs/outputOP_', oev_base, '_', oev_denom, '_', lambda, '_B_OEVnew.csv'), row.names = FALSE)
write.csv(dist.all, file = paste0('code/individualCountries/outputs/outputDist_', oev_base, '_', oev_denom, '_', lambda, '_B_OEVnew.csv'), row.names = FALSE)
write.csv(ens.all, file = paste0('code/individualCountries/outputs/outputEns_', oev_base, '_', oev_denom, '_', lambda, '_B_OEVnew.csv'), row.names = FALSE)

print('Done.')