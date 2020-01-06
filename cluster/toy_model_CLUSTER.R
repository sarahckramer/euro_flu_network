
### Fit Network Model to Observed Data and Run Forecasts ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
# library(viridis)

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network.R')
source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')
source('code/functions/replaceLeadingLaggingNAs.R')

### Read in filter function
source('code/EAKF_network.R')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
# S0_low <- 0.55; S0_up <- 0.85
# S0_low <- 0; S0_up <- 1.0
S0_low <- 0.3; S0_up <- 0.9
sd_low <- 0.05; sd_up <- 0.18
I0_low <- 0; I0_up <- 0.00005

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

# seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18') # ADD '2018-19'
seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
# seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
# seasons <- c('2010-11', '2011-12', '2012-13', '2014-15', '2015-16', '2017-18') # B

#oevBase_list <- c(0.1, 0.3, 0.5)
#oevDenom_list <- c(0.2, 1.0, 5.0) #c(1.0, 2.0, 5.0, 10.0, 20.0, 50.0)
#lambdaList <- c(1.00, 1.02, 1.05)
#multList <- c(1.0, 2.0, 5.0, 10.0)
ntrnList <- 5:30
# 1:1404, if lambda held at 1.02 for now - just exploring OEV form!

cmd_args = commandArgs(trailingOnly = T)
task.index=as.numeric(cmd_args[1])

season <- seasons[ceiling(task.index / 26)]
oev_base <- 1e4#0.5 #oevBase_list[ceiling((task.index - 78) / 78) %% 3 + 1]
oev_denom <- 10#2.5 #oevDenom_list[ceiling((task.index - 26) / 26) %% 3 + 1]
lambda <- 1.05 #lambdaList[ceiling((task.index - 26) / 26) %% 3 + 1]
ntrn <- ntrnList[ceiling(task.index - 1) %% 26 + 1]
print(paste(season, oev_base, oev_denom, lambda, ntrn, sep = '_'))

# # Check:
# check <- unique(as.data.frame(cbind(season, oev_base, oev_denom, lambda, ntrn)))
# print(dim(check))

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 2

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# ah <- read.csv('data/ah_MEAN_120519.csv')
# AH <- rbind(ah, ah)

### Read in influenza data
# iliiso <- read.csv('data/WHO_data_05-09-19_SCALED.csv') # in same order as "countries" vector
# iliiso <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv')
iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')

# syn.dat <- read.csv('data/synDatCounts_060519_SCALED.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(all).csv')

syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
pos.dat <- read.csv('data/by_subtype/posprop_A(H1).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_B_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_B.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# syn.dat.raw <- syn.dat

### Scale data: # ADD: new scalings
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
# # # note: these are the "old" scalings
# scalings <- read.csv('data/by_subtype/scalings_frame_A(all).csv')
scalings <- read.csv('data/by_subtype/scalings_frame_A(H1).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_A(H3).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_B.csv')

for (i in 2:13) {
  # iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  # syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}
# iliiso.scale <- iliiso
# syn.dat.scale <- syn.dat

# # "new" scalings:
# load('data/scalings_temp_08-26-19_MEANS.RData') # names are in same order
# for (i in 2:21) {
#   if (names(iliiso)[i] == 'France') {
#     iliiso[1:286, i] <- iliiso[1:286, i] * new.scalings.mean[[i - 1]][1]
#     iliiso[287:495, i] <- iliiso[287:495, i] * new.scalings.mean[[i - 1]][2]
#     syn.dat[1:286, i] <- syn.dat[1:286, i] * new.scalings.mean[[i - 1]][1]
#     syn.dat[287:495, i] <- syn.dat[287:495, i] * new.scalings.mean[[i - 1]][2]
#   } else {
#     iliiso[, i] <- iliiso[, i] * new.scalings.mean[[i - 1]]
#     syn.dat[, i] <- syn.dat[, i] * new.scalings.mean[[i - 1]]
#   }
#   
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
#   syn.dat[, i][syn.dat[, i] < 0] <- NA
#   pos.dat[, i][pos.dat[, i] < 0] <- NA
#   test.dat[, i][test.dat[, i] < 0] <- NA
# }

### Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL
outputOPParams <- NULL
outputDist <- NULL
outputEns <- NULL

### Main fitting code:

# Get commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
t.comm <- comm.by.year[[which(c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18') == season)]]
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
# param.bound <- cbind(theta_low, theta_up)

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
### QUESTION! It's possible leaving this step out would allow there to be a bit more ensemble noise going into the outbreak, since the model wouldn't
###           be fitting everything down to zero.

# Replace 0s in test_i w/ NA (b/c can't divide by 0!):
test_i[test_i == 0 & !is.na(test_i)] <- NA

# Variance of syndromic+ data:
#obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
#                               oev_base, oev_denom, tmp_exp = 2.0)
obs_vars <- calc_obsvars(obs = as.matrix(obs_i), oev_base, oev_denom)

# Set minimum OEV value:
obs_vars[obs_vars < 1e4 & !is.na(obs_vars)] <- 1e4

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

# Fit!:
# for (ntrn in 5:30) {
# print(ntrn)
# ntrn <- 10
outputVars <- NULL
for (run in 1:num_runs) {
  res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                  updates = FALSE, do.reprobing = FALSE)
  
  outputMetrics <- rbind(outputMetrics, cbind(season, run, oev_base, oev_denom, lambda, scalings$gamma, res$metrics))
  outputOP <- rbind(outputOP, cbind(season, run, oev_base, oev_denom, lambda, res$opStates))
  outputOPParams <- rbind(outputOPParams, cbind(season, run, oev_base, oev_denom, lambda, res$trainParams))
  outputDist = rbind(outputDist, cbind(season, run, oev_base, oev_denom, lambda, res$dist))
  outputEns = rbind(outputEns, cbind(season, run, oev_base, oev_denom, lambda, res$ensembles))
  outputVars = rbind(outputVars, cbind(season, run, lambda, res$vars))
}
# }

#print(head(outputMetrics))
#print(head(outputOP))
#print(head(outputOPParams))
#print(head(outputDist))
#print(head(outputEns))

colnames(outputMetrics)[6] <- 'scaling'
# I actually think all the other colnames are fine as-is...

load('data/by_subtype/scalings_by_subtype_120219.RData')
outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- scalings.new[[2]][13]#1.3
# outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- new.scalings.mean[[6]][1]
# FR has an alternative scaling for earlier

### Save results:
print('Finished with loop; writing files...')

write.csv(outputMetrics, file = paste('outputs/obs/outputMet', season, ntrn, '.csv', sep = '_'), row.names = FALSE)
write.csv(outputOP, file = paste('outputs/obs/outputOP', season, ntrn, '.csv', sep = '_'), row.names = FALSE)
write.csv(outputOPParams, file = paste('outputs/obs/outputOPParams', season, ntrn, '.csv', sep = '_'), row.names = FALSE)
write.csv(outputDist, file = paste('outputs/obs/outputDist', season, ntrn, '.csv', sep = '_'), row.names = FALSE)
write.csv(outputEns, file = paste('outputs/obs/outputEns', season, ntrn, '.csv', sep = '_'), row.names = FALSE)
# write.csv(outputVars, file = paste('outputs/obs/outputVars', season, ntrn, '120219.csv', sep = '_'), row.names = FALSE)

# write.csv(outputMetrics, file = paste('outputs/obs/outputMet', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)
# write.csv(outputOP, file = paste('outputs/obs/outputOP', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)
# write.csv(outputOPParams, file = paste('outputs/obs/outputOPParams', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)
# write.csv(outputDist, file = paste('outputs/obs/outputDist', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)
# write.csv(outputEns, file = paste('outputs/obs/outputEns', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)
# # write.csv(outputVars, file = paste('outputs/obs/outputEns', season, oev_base, oev_denom, lambda, ntrn, '.csv', sep = '_'), row.names = FALSE)

print('Done.')



