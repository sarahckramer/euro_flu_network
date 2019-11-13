
### Run synthetic code for 20 years ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); #library(ggplot2); library(gridExtra); library(viridis)

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network.R')
source('code/functions/Util.R')
source('code/synth_functions.R')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Set parameters
num_ens <- 50 # (100 x 100, or 200 x 50)
tm_strt <- 273; tm_end <- 273 + 365 * 20 - 1; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end # should be length 7300 days, or 7300 / 365 = 20 years

### Parameters for the filters
discrete <- TRUE # run the model stochastically

### Choose input parameters:
cmd_args = commandArgs(trailingOnly = T)
task.index=as.numeric(cmd_args[1])
range.to.use <- (1:50) + 50 * (task.index - 1)

load('code/runs20yr/inputs/init_parms_10000.RData')
parms <- parms[, range.to.use]

print(range.to.use)
print(dim(parms))

### Specify the countries in the model
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))
# note: this now results in more home-home people than before, since there are fewer countries to commute to

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])
for (i in 1:4) {
  AH <- rbind(AH, AH)
}
AH <- AH[1:7665, ] # 21 years

### Run model!
print('Get initial states...')
init.states <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'dist')
print('Begin running model...')
res <- run_model(parms, init.states[[1]], init.states[[2]], AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, r0.mn = FALSE) # time: 50 ~ 30minutes
print('Model run!')
res.rates <- res[[1]]

### Save raw outputs:
# save(res.rates, file = paste0('outputs/synth/resRates_20yr_FULL_', task.index,'.RData'))

### Remove the first 10 years for all runs:
for (i in 1:num_ens) {
  res.rates[[i]] <- res.rates[[i]][, 521:1043]
}

### Save outputs with last 10 years only:
save(res.rates, file = paste0('outputs/synth/resRates_20yr_last10_', task.index,'.RData'))







