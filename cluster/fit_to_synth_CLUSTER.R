
### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
# library(viridis)

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network2.R')
source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')

### Read in filter function
source('code/EAKF_rFC_Synth.R')

### Headers for output functions:
metrics_header <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'pkwk',
                    'obs_pkwk', 'delta_pkwk_mean', 'peak_intensity', 'obs_peak_int', 'intensity_err',
                    'onset', 'onsetObs', 'delta_onset',
                    'corr', 'rmse', 'pi_acc', 'pt_acc')
output_header <- c('outbreak','run','oev_base', 'oev_denom','lambda', 'week', 'L', 'L_sd', 'D', 'D_sd',
                   'R0max', 'R0max_sd', 'R0min', 'R0min_sd', 'airScale', 'airScale_sd', 'nu', 'nu_sd')

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 # "data" are weekly
wk_start <- 40

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8; airScale_low <- 0.75; nu_low <- 0
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2; airScale_up <- 1.25; nu_up <- 0.20
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low, nu_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up, nu_up)
param.bound <- cbind(theta_low, theta_up)

### Initial state variable values
S0_low <- 0.50; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

outbreak_list <- c(11, 14, 26, 29)
lambdaList <- c(1.01, 1.03)
oevBase_list <- c(1e4, 1e5)
oevDenom_list <- c(5, 10)
# oevDenom_list <- c(1, 2, 50)
# oevDenom_list <- c(1, 5, 10)
# tmpExpList <- c(1.0, 1.5, 2.0)

cmd_args = commandArgs(trailingOnly = T)
task_index=as.numeric(cmd_args[1]) # 1:32 #1:24
# task_index <- 1:864#899

# outbreak <- outbreak_list[ceiling(task_index / 24)]
# lambda <- lambdaList[ceiling((task_index - 6) / 6) %% 4 + 1]
# oev_base <- oevBase_list[ceiling((task_index - 3) / 3) %% 2 + 1]
# oev_denom <- oevDenom_list[(task_index - 1) %% 3 + 1]

outbreak <- outbreak_list[ceiling(task_index / 8)]
lambda <- lambdaList[ceiling((task_index - 4) / 4) %% 2 + 1]
oev_base <- oevBase_list[ceiling((task_index - 2) / 2) %% 2 + 1]
oev_denom <- oevDenom_list[(task_index - 1) %% 2 + 1]

print(paste0('Outbreak ', outbreak))
print(paste(oev_base, oev_denom, lambda, sep = '_'))

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
# ah <- read.csv('data/ah_05-07_formatted.csv')
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza "data":
synth.runs.TRUE <- readRDS('data/synth_0807_zto20_RATES.rds')
# load('data/synth_0807_zto20_RATES.RData')
# synth.runs.TRUE <- synth.runs.RATES
print('Truth read in.')
load(paste0('data/synth_0807_RATES_wError_', oev_base, '_', oev_denom, '.RData'))

### Initialize output data frames
outputMetrics <- NULL
outputOP <- NULL
outputS = outputI = outputS_sd = outputI_sd = outputAlps = vector('list', num_runs)

### Load commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

### Get symmetric version of the commuting matrix, for multiplying by prop. random travel:
t.comm.sym <- t.comm
t.comm.sym.lower <- which(lower.tri(t.comm.sym), arr.ind = TRUE)
avg.vals <- c()
for (i in 1:dim(t.comm.sym.lower)[1]) {
  index <- t.comm.sym.lower[i, ]
  avg.vals <- c(avg.vals, (t.comm.sym[index[1], index[2]] + t.comm.sym[index[2], index[1]]) / 2)
}
t.comm.sym[t.comm.sym.lower] <- avg.vals
t.comm.sym.upper <- cbind(t.comm.sym.lower[, 2], t.comm.sym.lower[, 1])
t.comm.sym[t.comm.sym.upper] <- avg.vals
print(isSymmetric(t.comm.sym))
rm(t.comm.sym.lower, t.comm.sym.upper, i, avg.vals)

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
# pdf(paste0('outputs/fit_to_synth1_', task_index, '.pdf'), width = 10, height = 8)
# pos <- 1

# Get true parameter values:
load('data/params_0807_zto20.RData')
true.params <- select.parms[outbreak, ]
print(true.params)

### Get observations for current run:
obs_i <- synth.runs.RATES[[outbreak]]
nsn <- dim(obs_i)[1]

### Get TRUE case rates for current run:
obs_TRUE <- t(synth.runs.TRUE[[outbreak]])

### Variance of syndromic+ data:
obs_vars <- calc_obsvars(obs_i, oev_base, oev_denom)

### Set ntrn:
ntrn <- nsn

### Fit to data: #!!!
for (run in 1:num_runs) {
  res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                  do.reprobing = FALSE)
  
  outputMetrics <- rbind(outputMetrics, cbind(outbreak, run, oev_base, oev_denom, lambda, res[[1]]))
  outputOP <- rbind(outputOP, cbind(outbreak, run, oev_base, oev_denom, lambda, 1:ntrn, res[[4]]))
  
  outputS[[run]] <- res[[5]]; outputS_sd[[run]] <- res[[6]] # in proportion of population
  outputI[[run]] <- res[[2]]; outputI_sd[[run]] <- res[[3]] # in rate per 100,000
  outputAlps[[run]] <- res[[7]]
  # pos <- pos + 1
}

# dev.off()

names(outputMetrics) <- metrics_header

outputOP <- outputOP[, c(1:7, 13, 8, 14, 9, 15, 10, 16, 11, 17, 12, 18)]
names(outputOP) <- output_header

write.csv(outputMetrics, file = paste0('outputs/synth/outputMet', task_index, '.csv'), row.names = FALSE)
write.csv(outputOP, file = paste0('outputs/synth/outputOP', task_index, '.csv'), row.names = FALSE)

outputsS <- list(outputS, outputS_sd); outputsI <- list(outputI, outputI_sd)

save(outputsS, file = paste0('outputs/synth/outputS', task_index, '.RData'))
save(outputsI, file = paste0('outputs/synth/outputI', task_index, '.RData'))
save(outputAlps, file = paste0('outputs/synth/outputAlps', task_index, '.RData'))

print('Done.')














