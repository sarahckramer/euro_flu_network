
### Fit Network Model to Observed Data ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)
library(viridis)

# ### Set directory (if necessary):
# setwd('/Users/sarahkramer/Desktop/Lab/spatial_transmission/EuropeanNetwork/')

### Read in model functions
source('code/SIRS_network.R')

source('code/functions/Fn_initializations.R')
source('code/functions/Fn_checkxnobounds.R')
source('code/functions/Util.R')
source('code/functions/replaceLeadingLaggingNAs.R')
source('code/functions/calc_obsvars.R')

### Read in filter function
source('code/EAKF_network.R')

# ### Specify headers for output file
# metrics_header <- c('season','run','oev','lambda','scaling','rescale','country','fc_start',
#                     'obs_pkwk','pkwk_mode','delta_pkwk_mode','pkwk_mean','delta_pkwk_mean',
#                     'leadpkwk_mode','leadpkwk_mean','pkwk_sd','obs_peak_int','peak_intensity',
#                     'intensity_err','peak_intensity_sd','totAttackObs','tot_attack','delta_AR',
#                     'obs_1week','obs_2week','obs_3week','obs_4week','fcast_1week','fcast_2week',
#                     'fcast_3week','fcast_4week','delta_1w','delta_2w','delta_3w','delta_4w',
#                     'rms','corr','rms_fcast','corr_fcast','mape','wape','smape','onset',
#                     'onset_sd','onsetObs')
# output_header <- c('season','run','oev','lambda','rescale','results','fc_start',
#                    'country','time','time_date','week','estimate','est_sd','S','S_sd','I','I_sd',
#                    'L','L_sd','D','D_sd','R0max','R0max_sd','R0min','R0min_sd','observed',
#                    'observed_sd')
# dist_header <- c('season','run','oev','lambda','rescale','fc_start','metric', 'week',
#                  'prob','country')

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
I0_low <- 0; I0_up <- 0.001 # proportion of population
# QUESTION: upper bound of 0.001 instead?

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs
lambda <- 1.03 # inflation factor for the ensemble filters c(1.00, 1.01, 1.02, 1.03, 1.05)

oev_base <- 1e5
oev_denom <- 1.0
# oev_denom <- 1.0 # denominator for observation error variance c(1, 5, 10, 50) (less for old scalings?: c(0.25, 0.5, 1, 5))
tmp_exp <- 2.0

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 1
# a little too high starting around time 19 (2010-11)
# try: (oev-denom 20 or 5 -> 10) w/ & w/o (lambda = 1.05)
# oev-denom = 20 w/o: decent, but definitely needs to be lower starting around 18ish
# but changing 5 to 10 did even better
# combine? no, too low; w/ lambda = 1.05? - no, still too low
# go back to 10 and 10 - still okay? - somehow no?? still seems too low, although starts to recorrect around 20, then gets too high; try with 1.05? - still no
# 1e6/10/5/1.05 - some countries fine, others not (DE having trouble? too little var. in ensemble?)
# 1e6/10/5/1.03 - something happens at tt = 16 - not enough variation in ensemble for some countries? (DE I think)
# 1e6/10/1/1.05 - works until about tt = 17, then stops paying attention to increases
# 1e6/10/1/1.03 - same-ish
# 1e5/10/1/1.03 - alright until about 19, then obs error too big
# try lambda 1.01 (not much better), 1.00 (same - seems something else has to change)
# 1e5/5OR1/1/1.03: 1 seems to be better, I think
# 1e5/1/5 or 10/1.03: fits well, but inferred params are unrealistic

# Different countries have different oev at different times, and this doesn't always match up with
# what the ensemble members are doing
# Scale data to have similar AR? I guess rather scale to be similar to AR in synthetic outbreaks

# make sure to try balancing by vir tests; check for bugs; can try reinit early on; other seasons!

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

# ### Initialize output data frame
# outputMetrics <- NULL
# outputOP <- NULL
# outputDist <- NULL
# outputEns <- NULL

### Loop through seasons:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
# for (s.index in 1:length(seasons)) {
s.index <- 1
  season <- seasons[s.index]
  
  ### Load commuting data:
  load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
  t.comm <- comm.by.year[[s.index]][countries, countries]
  # t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
  # t.comm <- t.comm[countries, countries]
  
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
  
  # par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  # matplot(syn_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  # matplot(pos_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  # matplot(test_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
  
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
  obs_vars <- calc_obsvars_nTest(obs_i, syn_i, test_i, pos_i, oev_base, oev_fact, oev_denom, tmp_exp)
  # obs_vars.orig <- calc_obsvars_nTest(syn_i, test_i, pos_i, oev_base, oev_fact, oev_denom, tmp_exp)
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
  ntrn <- 31
  
  ### Fit to data:
  for (run in 1:num_runs) {
    par(mfrow = c(4, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    updates = FALSE, do.reprobing = FALSE)
    print(table(res[[1]][, 8]))
    print(table(res[[1]][, 9]))
    # print(res)
    print('')
    par(mfrow = c(3, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
    for (param.index in 1:5) {
      plot(res[[3]][, param.index], pch = 20, type = 'b',
           xlab = 'Time Since Outbreak Start', ylab = names(res[[3]])[param.index])
    }
    
  }
  
  
  # 11/17
  
# }











# ######################################################################
# 
# ### Choose seasons
# seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17')
# 
# ### Main loop
# # for running EAKF experimentally:
# season <- '2015-16'; ntrn <- 30; run <- 1
# # for(season in seasons) {
# 
# tmp <- Fn_dates(season)
# weeks <- tmp$weeks + 3
# start_date <- tmp$start_date
# end_date <- tmp$end_date
# nsn <- tmp$nsn
# obs_i <- iliiso[weeks, ] # extract relevant data
# # obs_i.raw <- iliiso.raw[weeks, ]; obs_i.raw[obs_i.raw < 0] <- NA
# 
# # if(any(obs_i > 0 & !is.na(obs_i))) {
# 
# # print(season); #matplot(obs_i, type = 'b', pch = 20, lty = 1, col = viridis(4))
# 
# ### Replace any leading or lagging NAs
# for (i in 1:dim(obs_i)[2]) {
#   if (any(!is.na(obs_i[, i]))) {
#     start.index <- 1
#     while (obs_i[start.index, i] < 1 | is.na(obs_i[start.index, i])) {
#       start.index <- start.index + 1
#     }
#     start.index <- start.index - 1
#     
#     end.index <- length(obs_i[, i])
#     while (obs_i[end.index, i] < 1 | is.na(obs_i[end.index, i])) {
#       end.index <- end.index - 1
#     }
#     end.index <- end.index + 1
#     
#     if (start.index > 0 & end.index < (length(obs_i[, i]) + 1)) {
#       obs_i[c(1:start.index, end.index:length(obs_i[, i])), i] <- 0
#     } else if(start.index > 0 & end.index > length(obs_i[, i])) {
#       obs_i[1:start.index, i] <- 0
#     } else if(end.index < (length(obs_i[, i]) + 1) & start.index < 1) {
#       obs_i[end.index:length(obs_i[, i]), i] <- 0
#     }
#     
#   }
# }
# 
# par(mfrow = c(1, 1))
# matplot(obs_i, type = 'b', pch = 20, lty = 1, col = viridis(n))#, main = countries[1])
# # abline(v = ntrn, col = 'grey40', lty = 2)
# # in 15-16, no data: Greece, Sweden, UK
# 
# ### Variance of ILI+ data
# obs_vars <- calc_obsvars(obs_i, oev_denom)
# 
# ### Get the first and last date of the simulation
# clim_start <- as.numeric(start_date - as.Date(paste('20',
#                                                     substr(season, gregexpr('-', season)[[1]][1]-2,
#                                                            gregexpr('-', season)[[1]][1]-1),
#                                                     '-01-01', sep=''))) + 1 - 6
# ### Number of days in the year at the beginning of the week
# clim_end <- as.numeric(end_date - as.Date(paste('20',
#                                                 substr(season, gregexpr('-', season)[[1]][1]-2,
#                                                        gregexpr('-', season)[[1]][1]-1),
#                                                 '-01-01', sep=''))) + 1
# tm.ini <- clim_start - 1 # the end of the former week
# tm.range <- clim_start:clim_end
# 
# ### Identify scalings of relevance to this season
# any.data <- c()
# for (ix in 1:n) {
#   if(!all(is.na(obs_i[, ix]))) {
#     any.data <- c(any.data, ix)
#   }
# }
# # scalings.new.seas <- scalings.new
# # if (season %in% c('2014-15', '2015-16', '2016-17', '2017-18')) {
# #   scalings.new.seas[[6]] <- scalings.new.seas[[6]][2]
# # } else {
# #   scalings.new.seas[[6]] <- scalings.new.seas[[6]][1]
# # }
# # scalings.new.seas <- unlist(scalings.new.seas[any.data])
# 
# # par(mfrow = c(2, 1))
# ### Now run forecasts
# for (ntrn in c(20)) { # for (ntrn in 5:30) {
#   #   if (any(!is.na(obs_i[ntrn, ]))) {
#   #     # for (run in 1:num_runs) {
#   #
#   res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn, obs_vars,
#                   tm.ini, tm.range)
#   
#   outputMetrics = rbind(outputMetrics, cbind(season, run, oev_denom, lambda, scalings.new.seas, rescale, res$metrics))
#   outputDist = rbind(outputDist, cbind(season, run, oev_denom, lambda, rescale, res$dist))
#   outputEns = rbind(outputEns, cbind(season, run, oev_denom, lambda, rescale, res$ensembles))
#   
#   for(i in 1:2) {
#     temp_res = buildOutputNew(res[[i]], 'long') # reorder columns
#     temp_res = convertDaysToDate(temp_res, start_date) #convert 'time' to date
#     obs_output = NULL
#     obs_sd = NULL
#     
#     if(names(res)[i] == "fcast"){
#       if(dim(obs_i)[1] == nsn){
#         
#         for (j in 1:n) {
#           if(any(!is.na(obs_i[, j]))) {
#             obs_output <- c(obs_output, obs_i[(ntrn+1):nsn, j])
#             obs_sd <- c(obs_sd, obs_vars[(ntrn+1):nsn, j])
#           }
#         }
#         
#       }
#       
#       else{
#         if(ntrn+1 <= length(obs_i)) {
#           
#           for (j in 1:n) {
#             if(any(!is.na(obs_i[, j]))) {
#               obs_output <- c(obs_output, obs_i[(ntrn+1):(dim(obs_i)[1]), j])
#               obs_sd <- c(obs_sd, obs_vars[(ntrn+1):(dim(obs_vars)[1]), j])
#             }
#           }
#           
#         }
#         
#         for (j in 1:n) {
#           if(any(!is.na(obs_i[, j]))) {
#             obs_output <- c(obs_output, rep(-1, nsn - dim(obs_i)[1]))
#             obs_sd <- c(obs_sd, rep(-1, nsn - dim(obs_i)[1]))
#           }
#         }
#         
#       }
#     } else {
#       for (j in 1:n) {
#         if(any(!is.na(obs_i[, j]))) {
#           obs_output <- c(obs_output, obs_i[1:ntrn, j][!is.na(obs_i[1:ntrn, j])])
#           obs_sd <- c(obs_sd, obs_vars[1:ntrn, j][!is.na(obs_i[1:ntrn, j])])
#         }
#       }
#     }
#     
#     outputOP = rbind(outputOP, cbind(season, run, oev_denom, lambda, rescale, names(res)[i], temp_res, obs_output, obs_sd))
#   }
#   
#   
#   #
#   #
#   #
#   #
#   #
#   #     # } # end of all runs
#   #   }
# } # end of all ntrn values
# 
# # }
# # } # end of all seasons

# dimnames(outputMetrics) = list(c(), as.list(metrics_header))
# dimnames(outputOP)[[2]] = as.list(output_header)
# dimnames(outputDist) = list(c(), as.list(dist_header))
# dimnames(outputEns)[[2]][1:7] <- as.list(c('season', 'run', 'oev', 'lambda',
#                                            'rescale', 'country', 'metric'))
# 
# # write.csv(outputMetrics, file = '/Users/Sarah/Desktop/forecasts/results_5-01/outputMet.csv', row.names=F)
# # write.csv(outputOP, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputOP',countries[count.index],'.csv',sep=''), row.names=F)
# # write.csv(outputDist, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputDist',countries[count.index],'.csv',sep=''), row.names=F)
# # write.csv(outputEns, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputEns', countries[count.index], '.csv', sep=''), row.names=F)
# 
# outputMetrics <- as.data.frame(outputMetrics)
# print(length(unlist(outputMetrics$delta_pkwk_mean)[unlist(outputMetrics$delta_pkwk_mean) %in% -1:1]) / length(unlist(outputMetrics$delta_pkwk_mean)))
# print(length(unlist(outputMetrics$peak_intensity)[unlist(outputMetrics$peak_intensity) >= 0.875 * unlist(outputMetrics$obs_peak_int) &
#                                                     unlist(outputMetrics$peak_intensity) <= 1.125 * unlist(outputMetrics$obs_peak_int)]) / length(unlist(outputMetrics$peak_intensity)))
# delta_onset <- unlist(outputMetrics$onset) - unlist(outputMetrics$onsetObs)
# print(length(delta_onset[delta_onset %in% -1:1 & !is.na(delta_onset)]) / length(delta_onset))



############################################################################################
