
### Run synthetic code for 20 years ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); #library(ggplot2); library(gridExtra); library(viridis); library(miceadds)
# sessionInfo()

### Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/')

### Read in model functions
source('code/SIRS_network_multistrain.R')
source('code/functions/Util.R')
source('code/functions/synth_functions.R')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Set parameters
num_ens <- 50 # (100 x 100, or 200 x 50)
tm_strt <- 1; tm_end <- 365 * 20 + 2; tm_step <- 1#; t <- 273 is first of October; adding 2 makes it the whole week - avoids NAs at the end
tm.range <- tm_strt:tm_end # should be length 7300 days, or 7300 / 365 = 20 years

### Parameters for the filters
discrete <- TRUE # run the model stochastically

### Choose input parameters:
cmd_args = commandArgs(trailingOnly = T)
task.index=as.numeric(cmd_args[1])
range.to.use <- (1:50) + 50 * (task.index - 1)

# load('code/runs20yr/inputs/init_parms_10000_LHS.RData')
load('code/runs20yr/inputs/init_parms_10000_LHS_noAH.RData')
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

# ### Read in humidity data
# # ah <- read.csv('data/ah_Europe_07142019.csv')
# ah <- read.csv('data/ah_MEAN_120519.csv')
# # AH <- rbind(ah[, count.indices], ah[, count.indices])
# AH <- rbind(ah, ah)
# for (i in 1:4) {
#   AH <- rbind(AH, AH)
# }
# AH <- AH[1:7316, ] # 20 years
# # print(head(AH))

### Get subtypes by season
Vtype <- read.csv('data/subtypes_seeding.csv')

### Run model!
print('Get initial states...')
init.states1 <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'lhs')
init.states2 <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'lhs')
init.states <- list(list(init.states1[[1]], init.states2[[1]]),
                    list(init.states1[[2]], init.states2[[2]]))
print('Begin running model...')
# res <- run_model(parms, init.states[[1]], init.states[[2]], AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, r0.mn = FALSE, multi = TRUE) # time: 50 ~ 30minutes
res <- run_model_noAH(parms, init.states[[1]], init.states[[2]], num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, multi = TRUE)
print('Model run!')
res.rates1 <- res[[1]]; res.rates2 <- res[[2]]
res.s1 <- res[[3]]; res.s2 <- res[[4]]
res.r1 <- res[[5]]; res.r2 <- res[[6]]

# matplot(t(res.rates1[[1]]), pch = 20, col = viridis(n), type = 'b', lty = 1)
# matlines(t(res.rates2[[1]]), pch = 20, col = inferno(n), type = 'b', lty = 1)
# abline(v = seq(0, 52*19, by = 52), lwd = 2.0, lty = 2)
# matplot(t(res.rates1[[2]]), pch = 20, col = viridis(n), type = 'b', lty = 1)
# matlines(t(res.rates2[[2]]), pch = 20, col = inferno(n), type = 'b', lty = 1)
# abline(v = seq(0, 52*19, by = 52), lwd = 2.0, lty = 2)
# matplot(t(res.rates1[[3]]), pch = 20, col = viridis(n), type = 'b', lty = 1)
# matlines(t(res.rates2[[3]]), pch = 20, col = inferno(n), type = 'b', lty = 1)
# abline(v = seq(0, 52*19, by = 52), lwd = 2.0, lty = 2)
# matplot(t(res.rates1[[4]]), pch = 20, col = viridis(n), type = 'b', lty = 1)
# matlines(t(res.rates2[[4]]), pch = 20, col = inferno(n), type = 'b', lty = 1)
# abline(v = seq(0, 52*19, by = 52), lwd = 2.0, lty = 2)
# matplot(t(res.rates1[[5]]), pch = 20, col = viridis(n), type = 'b', lty = 1)
# matlines(t(res.rates2[[5]]), pch = 20, col = inferno(n), type = 'b', lty = 1)
# abline(v = seq(0, 52*19, by = 52), lwd = 2.0, lty = 2)
# # it looks like Rs are fine

### Save raw outputs:
# save(res.rates, file = paste0('outputs/synth/resRates_20yr_FULL_', task.index,'.RData'))

### Remove the first 10 years for all runs:
for (i in 1:num_ens) {
  res.rates1[[i]] <- res.rates1[[i]][, 521:1043]
  res.rates2[[i]] <- res.rates2[[i]][, 521:1043]
  res.s1[[i]] <- res.s1[[i]][, 521:1043]
  res.s2[[i]] <- res.s2[[i]][, 521:1043]
  res.r1[[i]] <- res.r1[[i]][, 521:1043]
  res.r2[[i]] <- res.r2[[i]][, 521:1043]
}
# this is still okay, I think - want those last 9 years that use the strains that actually circulated

### Save outputs with last 9 years only:
res.list <- list(res.rates1, res.rates2, res.s1, res.s2, res.r1, res.r2)
save(res.list, file = paste0('outputs/synth/resList_20yr_last10_', task.index,'.RData'))

print('Done.')





