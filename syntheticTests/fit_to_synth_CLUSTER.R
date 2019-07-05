
### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network.R')
source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')

### Read in filter function
source('code/EAKF_rFC_Synth.R')

### Headers for output functions:
metrics_header <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'pkwk',
                    'obs_pkwk', 'delta_pkwk_mean', 'peak_intensity', 'obs_peak_int', 'intensity_err',
                    'corr', 'rmse', 'pi_acc', 'pt_acc')
output_header <- c('outbreak','run','oev_base', 'oev_denom','lambda', 'week', 'L', 'L_sd',
                   'D', 'D_sd', 'R0max', 'R0max_sd', 'R0min', 'R0min_sd', 'airScale', 'airScale_sd')

### Ensemble member numbers kept (for now):
to.keep <- c(3, 5, 13, 19, 26)

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 # "data" are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
param.bound <- cbind(theta_low, theta_up)

### Initial state variable values
S0_low <- 0.30; S0_up <- 1.00 # proportion of population
I0_low <- 0; I0_up <- 0.001 # proportion of population

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

lambdaList <- c(1.00, 1.01, 1.03, 1.05)
oevBase_list <- c(1e4, 1e5)
oevDenom_list <- c(5, 10, 20)
# tmpExpList <- c(1.0, 1.5, 2.0)

cmd_args = commandArgs(trailingOnly = T)
task_index=as.numeric(cmd_args[1]) # 1:24

lambda <- lambdaList[ceiling(task_index / 6)]
oev_base <- oevBase_list[ceiling((task_index - 3) / 3) %% 2 + 1]
oev_denom <- oevDenom_list[(task_index - 1) %% 3 + 1]

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 2

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

### Read in influenza "data":
load(paste0('data/synth_06-28_RATES_wError_', oev_base, '_', oev_denom, '.RData'))

### Initialize output data frames
outputMetrics <- NULL
outputOP <- NULL
outputS = outputI = outputS_sd = outputI_sd = outputAlps = vector('list', length(to.keep) * num_runs)

### Load commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
# t.comm <- t.comm[countries, countries]

### Set country populations:
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))
# population and commuting data are COUNTS, not RATES

### Set important values for fitting: #!!!
tm.ini <- 273 - 1 #270 - 1
tm.range <- 273:600 #270:600

### Loop through synthetic runs:
pos <- 1
for (outbreak in 1:length(to.keep)) {
  
  # Get true parameter values:
  load('syntheticTests/syntheticData/params_06-26.RData')
  true.params <- select.parms[to.keep[outbreak], ]
  print(true.params)
  
  ### Get observations for current run:
  obs_i <- synth.runs.RATES[[to.keep[outbreak]]]
  nsn <- dim(obs_i)[1]
  
  ### Variance of syndromic+ data:
  obs_vars <- calc_obsvars(obs_i, oev_base, oev_denom)
  
  ### Set ntrn:
  ntrn <- nsn
  
  ### Fit to data: #!!!
  for (run in 1:num_runs) {
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    do.reprobing = TRUE)
    
    outputMetrics <- rbind(outputMetrics, cbind(outbreak, run, oev_base, oev_denom, lambda, res[[1]]))
    outputOP <- rbind(outputOP, cbind(outbreak, run, oev_base, oev_denom, lambda, 1:ntrn, res[[4]]))
    
    outputS[[pos]] <- res[[5]]; outputS_sd[[pos]] <- res[[6]] # in proportion of population
    outputI[[pos]] <- res[[2]]; outputI_sd[[pos]] <- res[[3]] # in rate per 100,000
    outputAlps[[pos]] <- res[[7]]
    pos <- pos + 1
  }
  
}

names(outputMetrics) <- metrics_header

outputOP <- outputOP[, c(1:7, 12, 8, 13, 9, 14, 10, 15, 11, 16)]
names(outputOP) <- output_header

write.csv(outputMetrics, file = 'outputs/outputMet.csv', row.names = FALSE)
write.csv(outputOP, file = 'outputs/outputOP.csv', row.names = FALSE)

outputsS <- list(outputS, outputS_sd); outputsI <- list(outputI, outputI_sd)

save(outputsS, file = 'outputs/outputS.RData')
save(outputsI, file = 'outputs/outputI.RData')
save(outputAlps, file = 'outputs/outputAlps.RData')














