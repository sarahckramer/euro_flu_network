
# This is just to plot out preliminary results
# Eventually run in cluster and get full output files


# Questions for Sen: Any issues with parameters not fitting well? What modifications did you make to the model? I0 randomly initiated everywhere or only in one/some locations?


# [x] Try reinitiating/reprobing
# [x] Check Sen's paper
# [x] Check Sen's code - different start conditions (more narrow S, D up to 12, R0mn lowest at 1.0)
# [x] Try with various amounts of error added
    # [] And various error formats (tmp_exp)?
# See list in Word doc

# Meeting w/ Jeff:
    # How does this make sense? Not being able to fit something it made itself?
    # Changing multiplier of air travel has very little impact, but commuting seems to play a role; right now though, patterns are dominated by AH-forcing; R0mx/D have stronger impact on pattern when no AH
        # Add in proportional random travel to commuting (like Sen)? (In Sen's model, transmission is very sensitive to random movement rate - seems important)
        # Country-level AH weighted by population density (SE)? Remove IS and maybe SE?
    # How to tell how much error to add? Just so it vaguely looks like observations?
    # obs_var using (j-2):j, or just j, since everything here is "known"? (note: for when adding error, not fitting)
    # Still initiate S and I by country, then normal dist/split proportionally for each individual compartment - is that okay, or should everything be drawn from LHS?
    # Still having most trouble with later peaks - fit seems to have already "settled in," and harder to get it to move upward again

# Notes from meeting w/ Jeff:
    # [x] Also look at incidence relative to the TRUE newI, not just error-laden observations
    # [x] Look at beta, R0, Re, S (truth)
    # [x] Re-check humidity data; humidity data by population density?
    # [x] Remove IS
    # [] Proportional random movement (~0-5%)
    # [x] For fitting: draw all S and I from LHS
    # [x] Use j-2 and j for first two points
    # [] Recode EAKF to loop through each country (like Sen), rather than using matrices?


### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

### Read in model functions
source('code/SIRS_network.R')
source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/calc_obsvars.R')

### Read in filter function
source('syntheticTests/EAKF_rFC_Synth.R')

### Headers for output functions:
metrics_header <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'pkwk',
                    'obs_pkwk', 'delta_pkwk_mean', 'peak_intensity', 'obs_peak_int', 'intensity_err',
                    'corr', 'rmse', 'pi_acc', 'pt_acc')
output_header <- c('outbreak','run','oev_base', 'oev_denom','lambda', 'week', 'L', 'L_sd',
                   'D', 'D_sd', 'R0max', 'R0max_sd', 'R0min', 'R0min_sd', 'airScale', 'airScale_sd')

### Ensemble member numbers kept (for now):
to.keep <- c(1, 2, 8)#, 10, 15, 18) #!!!

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
lambda <- 1.03 # inflation factor for the ensemble filters c(1.00, 1.01, 1.02, 1.03, 1.05, 1.075?)
oev_base <- 1e5; oev_denom <- 20.00
# 1e4/5/1.00 combo doesn't look bad...; oev_base of 1e3 seems too low to handle early low error observations...
# most similar to observed data seem to be 1e4/10, 1e4/20, and 1e5/20 (even 1e5/10, although a lot of error), based on visual similarity
# OEVs look more like those calculated from Aim1 if denominator is 10 (although I know this isn't a great test)

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 2

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Read in influenza "data":
load('syntheticTests/syntheticData/synth_07-11_RATES.RData')
synth.runs.TRUE <- synth.runs.RATES
load('syntheticTests/syntheticData/synth_07-11_RATES_wError_1e5_10.RData')
# synth.runs.RATES <- synth.runs.RATES[to.keep]
# use rates b/c for observed data we used scaled data, which are meant to represent rates per 100,000 population

### Initialize output data frames
outputMetrics <- NULL
outputOP <- NULL
outputS = outputI = outputS_sd = outputI_sd = outputAlps = vector('list', length(to.keep) * num_runs)

### Load commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

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
  # outbreak <- 1
  
  # Get true parameter values:
  load('syntheticTests/syntheticData/params_07-11.RData')
  true.params <- select.parms[to.keep[outbreak], ]
  print(true.params)
  
  ### Get observations for current run:
  obs_i <- synth.runs.RATES[[to.keep[outbreak]]]
  nsn <- dim(obs_i)[1]
  
  ### Get TRUE case rates for current run:
  obs_TRUE <- t(synth.runs.TRUE[[to.keep[outbreak]]])
  
  ### Plot:
  par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.9)
  # par(mfrow = c(5, 5), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # for (i in 1:n) {
  #   plot(obs_i[, i], type = 'b', pch = 20, col = 'coral', cex = 0.7, main = countries[i], xaxt = 'n', xlab = '', ylab = 'Syn+')
  # }
  
  ### Variance of syndromic+ data:
  obs_vars <- calc_obsvars(obs_i, oev_base, oev_denom)
  # matplot(obs_vars, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.9)
  
  # tm.ini <- clim_start - 1 # the end of the former week
  # tm.range <- clim_start:clim_end
  
  ### Set ntrn:
  ntrn <- nsn
  
  ### Fit to data: #!!!
  for (run in 1:num_runs) {
    par(mfrow = c(3, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    do.reprobing = TRUE)

    print(table(res[[1]][, 10]))
    print(table(res[[1]][, 11]))
    print('')
    
    true.params <- true.params[c(2, 1, 4:5, 3)]
    true.params[1] <- true.params[1] * 365
    
    par(mfrow = c(3, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
    for (param.index in 1:5) {
      plot(res[[4]][1:30, param.index], pch = 20, type = 'b', cex = 0.8,
           xlab = 'Time Since Outbreak Start', ylab = names(res[[4]])[param.index],
           ylim = c(min(unlist(c(res[[4]][, param.index], true.params[param.index]))),
                    max(unlist(c(res[[4]][, param.index], true.params[param.index])))))#,
           # ylim = c(param.bound[param.index, 1], param.bound[param.index, 2]))
      abline(h = true.params[param.index], lwd = 2, lty = 1, col = 'blue')
    }
    
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

# write.csv(outputMetrics, file = 'syntheticTests/outputs/outputMet2.csv', row.names = FALSE)
# write.csv(outputOP, file = 'syntheticTests/outputs/outputOP2.csv', row.names = FALSE)

outputsS <- list(outputS, outputS_sd); outputsI <- list(outputI, outputI_sd)

# save(outputsS, file = 'syntheticTests/outputs/outputS2.RData')
# save(outputsI, file = 'syntheticTests/outputs/outputI2.RData')
# save(outputAlps, file = 'syntheticTests/outputs/outputAlps2.RData')














