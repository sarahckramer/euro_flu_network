
# *** Code to compare set-up and outputs to those from python code *** #

### Set up model:
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); require(plyr)

# Read in model functions:
source('cluster/SIRS_network.R')
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/Fn_checkxnobounds.R')
source('cluster/functions/Util.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')
source('cluster/EAKF_network.R')

# Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

# Parameter ranges:
init_parms = vector('list', 5)
for (i in 1:5) {
  parms_temp <- read.table(paste0('python/initial_parms/parms', i - 1, '.txt'), header = FALSE, sep = '\t')
  init_parms[[i]] <- parms_temp
}; rm(parms_temp)

# Parameters for the filters:
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs

# seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18') # ADD '2018-19'
seasons <- c('2010-11', '2012-13')#, '2013-14', '2014-15', '2015-16', '2017-18') # H1
# seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
# seasons <- c('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18') # B

oev_base <- 0.0 #oevBase_list[ceiling((task.index - 26) / 26) %% 2 + 1]
oev_denom <- 2.0 #oevDenom_list[ceiling((task.index - 78) / 78) %% 3 + 1]
lambda <- 1.05 #lambdaList[ceiling((task.index - 26) / 26) %% 3 + 1]

num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 1

# Specify countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set population sizes and # of countries used:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in humidity data:
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# ah <- read.csv('data/ah_MEAN_120519.csv')
# AH <- rbind(ah, ah)

# Read in influenza data:
# iliiso <- read.csv('data/WHO_data_05-09-19_SCALED.csv') # in same order as "countries" vector
# iliiso <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv')
iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
# iliiso <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')

# Read in syndromic/virologic counts:
test.dat <- read.csv('data/testRates_010820.csv')

# syn.dat <- read.csv('data/synDatCounts_060519_SCALED.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(all).csv')

syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
pos.dat <- read.csv('data/by_subtype/posprop_A(H1).csv')

# syn.dat <- read.csv('data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')
# 
# syn.dat <- read.csv('data/by_subtype/synDatCounts_B_SCALED.csv')
# pos.dat <- read.csv('data/by_subtype/posprop_B.csv')

# test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

# Scale data:
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
# # # note: these are the "old" scalings
# scalings <- read.csv('data/by_subtype/scalings_frame_A(all).csv')
scalings <- read.csv('data/by_subtype/scalings_frame_A(H1).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_A(H3).csv')
# scalings <- read.csv('data/by_subtype/scalings_frame_B.csv')

# Initialize output data frame
outputMetrics <- NULL
outputOP <- NULL
outputOPParams <- NULL
outputDist <- NULL
outputEns <- NULL

### Main fitting code:
for (season in seasons) {
# season <- '2010-11'  

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
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # Variance of syndromic+ data:
  obs_vars <- 1e5 + calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
                                       oev_base, oev_denom, tmp_exp = 2.0)
  # obs_vars[obs_vars < 1e3 & !is.na(obs_vars)] <- 1e3
  
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
  
  ntrn <- 20
  run <- 1
  
  for (run in 1:num_runs) {
    # ntrn <- 10
    res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i, ntrn, obs_vars, tm.ini, tm.range,
                    updates = FALSE, do.reprobing = FALSE)
    
    outputMetrics <- rbind(outputMetrics, cbind(season, run, oev_base, oev_denom, lambda, scalings$gamma, res$metrics))
    outputOP <- rbind(outputOP, cbind(season, run, oev_base, oev_denom, lambda, res$opStates))
    outputOPParams <- rbind(outputOPParams, cbind(season, run, oev_base, oev_denom, lambda, res$trainParams))
    outputDist = rbind(outputDist, cbind(season, run, oev_base, oev_denom, lambda, res$dist))
    outputEns = rbind(outputEns, cbind(season, run, oev_base, oev_denom, lambda, res$ensembles))
    # outputVars = rbind(outputVars, cbind(season, run, lambda, res$vars))
  }
  
}

colnames(outputMetrics)[6] <- 'scaling'

load('data/by_subtype/scalings_noCutoff_threeOverPointOne.RData')
outputMetrics[outputMetrics[, 'country'] == 'FR' & outputMetrics[, 'season'] %in% seasons[1:4], 'scaling'] <- scalings.new[[1]][13]#1.3

# Read in and compare xprior after initiating model run:
# res.py <- read.table(file = 'python/results/xprior_ens_0_2010-11.txt', header = FALSE, sep = ',')
# res.py <- res.py[, 1:20]
# 
# res <- as.data.frame(res)
# 
# all.equal(res, res.py)

write.csv(outputMetrics, file = 'python/results/outputMet_Rcomp_20.csv', row.names = FALSE)
write.csv(outputOP, file = 'python/results/outputOP_Rcomp_20.csv', row.names = FALSE)
write.csv(outputOPParams, file = 'python/results/outputOPParams_Rcomp_20.csv', row.names = FALSE)
write.csv(outputDist, file = 'python/results/outputDist_Rcomp_20.csv', row.names = FALSE)
write.csv(outputEns, file = 'python/results/outputEns_Rcomp_20.csv', row.names = FALSE)

rm(list = ls())

outputMetrics <- read.csv('python/results/outputMet_Rcomp_20.csv')
outputOP <- read.csv('python/results/outputOP_Rcomp_20.csv')

a <- read.csv('python/results/outputMet_updated2.csv')
b <- read.csv('python/results/outputOP_updated2.csv')

b <- b[b$fc_start == 59, ]
op <- outputOP[outputOP$run == 1, ]

countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
b$country <- factor(b$country)
levels(b$country) <- countries

p1 <- ggplot(data = b, aes(x = week, y = Est, colour = result, group = season)) + geom_line() + facet_wrap(~ country) + theme_classic() +
  geom_point(data = op, size = 1.1)
p1

d <- merge(b, op, by = c('country', 'result', 'week', 'season', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'time'))
all.equal(d$Est.x, d$Est.y)
all.equal(d$Est.x[d$result == 'train'], d$Est.y[d$result == 'train'])
all.equal(d$Est.x[d$result == 'fcast'], d$Est.y[d$result == 'fcast'])
# these all say all equal
# at 20, mean relative difference of ~0.0004; worse for fcast part since accumulation has happened (~0.0016)

which(d$Est.x == d$Est.y)
which(d$S.x == d$S.y)
which(d$I.x == d$I.y)
# never exactly equal...
# 20: some actually are exactly equal here? but these are weeks that weren't caught before, so maybe tolerance changed

# op <- op[op$result == 'fcast', ]

all.equal(op$S, b$S)
all.equal(op$I, b$I)
all.equal(op$Est, b$Est)
all.equal(op$S_sd, b$S_sd)
all.equal(op$I_sd, b$I_sd)
all.equal(op$Est_sd, b$Est_sd)
# all equal at fc_start 44
# all diff at fc_start 59 - but within 0.0003ish (0.003 for I_sd/Est_sd)

a <- a[a$fc_start == 59, ]
outputMetrics <- outputMetrics[outputMetrics$run == 1, ]
all.equal(a$obs_pkwk, outputMetrics$obs_pkwk) # same
all.equal(a$pkwk_mean, outputMetrics$pkwk_mean) # same
all.equal(a$pkwk_sd, outputMetrics$pkwk_sd) # 0.00028
all.equal(a$obs_peak_int, outputMetrics$obs_peak_int) # same
all.equal(a$peak_intensity, outputMetrics$peak_intensity) # 6.5e-5
all.equal(a$peak_intensity_sd, outputMetrics$peak_intensity_sd) # 0.00019
all.equal(a$obs_1week, outputMetrics$obs_1week) # these are equal now!
all.equal(a$obs_2week, outputMetrics$obs_2week)
all.equal(a$obs_3week, outputMetrics$obs_3week)
all.equal(a$obs_4week, outputMetrics$obs_4week)
all.equal(a$fcast_1week, outputMetrics$fcast_1week) # this and next 3 have small errors (~0.0002-4, more as further)
all.equal(a$fcast_2week, outputMetrics$fcast_2week)
all.equal(a$fcast_3week, outputMetrics$fcast_3week)
all.equal(a$fcast_4week, outputMetrics$fcast_4week)
all.equal(a$onsetObs5, outputMetrics$onsetObs5)
all.equal(a$onset5, outputMetrics$onset5)
all.equal(a$onset5_sd, outputMetrics$onset5_sd) # all onsets same
all.equal(a$corr, outputMetrics$corr)
all.equal(a$rmse, outputMetrics$rmse)
all.equal(a$corr_fcast, outputMetrics$corr_fcast)
all.equal(a$rmse_fcast, outputMetrics$rmse_fcast)
# small errors on those last 4, but not seeing anything extreme
  # are they all handling NAs correctly? - doesn't look to be a problem!

outputDist <- read.csv('python/results/outputDist_Rcomp_20.csv')
outputEns <- read.csv('python/results/outputEns_Rcomp_20.csv')
outputOPParams <- read.csv('python/results/outputOPParams_Rcomp_20.csv')

d <- read.csv('python/results/outputDist_updated2.csv')
e <- read.csv('python/results/outputEns_updated2.csv')
op <- read.csv('python/results/outputOPParams_updated2.csv')

# # Stuff should no longer equal old:
# b <- read.csv('python/results/outputOP_updated.csv')
# o.old <- read.csv('python/results/outputOP1.csv')
# o.old2 <- read.csv('python/results/outputOP_allZeros.csv')
# all.equal(o.old, o.old2)
# all.equal(b, o.old)
# all.equal(b, o.old2)

# opparams: these we'll check at 20
all.equal(op[, 3:12], outputOPParams[, 9:18])
# minor differences, mostly <0.0001 with L as exception
plot(op$L); lines(outputOPParams$L)

d <- d[d$fc_start == 59, ]
# d <- d[d$season == '2012-13', ]
# d <- d[, c(2:5)]
levels(d$metric)
levels(outputDist$metric)
outputDist <- outputDist[outputDist$metric %in% levels(outputDist$metric)[c(5:8, 11, 14:15)], ]
outputDist$metric <- factor(outputDist$metric)
levels(outputDist$metric) <- c('nextweek1', 'nextweek2', 'nextweek3', 'nextweek4', 'onset5', 'pi', 'pw')
outputDist <- outputDist[outputDist$run == 1, ]
# outputDist <- outputDist[, c(7:10)]
d$run <- NULL; outputDist$run <- NULL
d.new <- merge(d, outputDist, by = c('fc_start', 'metric', 'bin', 'country', 'season'))
all.equal(d.new$value.x, d.new$value.y) # 0.9765098 - that's not great - something in labeling the bins is wrong here
# at time point 5 all are equal here
# 0.0308 - not great, but might be just rounding error now?
all.equal(d.new$value.x[d.new$metric == 'nextweek1'], d.new$value.y[d.new$metric == 'nextweek1'])
all.equal(d.new$value.x[d.new$metric == 'nextweek2'], d.new$value.y[d.new$metric == 'nextweek2'])
all.equal(d.new$value.x[d.new$metric == 'nextweek3'], d.new$value.y[d.new$metric == 'nextweek3'])
all.equal(d.new$value.x[d.new$metric == 'nextweek4'], d.new$value.y[d.new$metric == 'nextweek4'])
# 0.01-0.05, depending
all.equal(d.new$value.x[d.new$metric == 'onset5'], d.new$value.y[d.new$metric == 'onset5']) # all equal!
all.equal(d.new$value.x[d.new$metric == 'pi'], d.new$value.y[d.new$metric == 'pi']) # 0.044
all.equal(d.new$value.x[d.new$metric == 'pw'], d.new$value.y[d.new$metric == 'pw']) # 0.026

all.equal(outputDist$metric, d$metric)
all.equal(outputDist$bin, d$bin)
all.equal(outputDist$country, d$country)
all.equal(outputDist$value, d$value)
# all equal except maybe a one-off with weeks for PT/onset

e <- e[e$fc_start == 59, ]
outputEns <- outputEns[outputEns$metric != 'ar', ]; outputEns$metric <- factor(outputEns$metric)
all.equal(outputEns[, 9:308], e[, 4:303])
# only a few ensemble members off (10^-7-^-8 range)
# yep, all pretty small-scale differences

### Look at adjustments to x/obs_ens:
for (i in 0:29) {
  pre <- read.table(paste0('python/results/obsens_PRE_', i, '.txt'), sep = ',')
  post <- read.table(paste0('python/results/obsens_POST_', i, '.txt'), sep = ',')
  
  # # Check 1: Empty compartments stay 0
  # all.zero.pre <- sapply(1:437, function(ix) {
  #   all(pre[ix, ] == 0)
  # })
  # all.zero.post <- sapply(1:437, function(ix) {
  #   all(post[ix, ] == 0)
  # })
  # print(all(all.zero.pre == all.zero.post)) # good!
  
  # Check 2: How often are we adjusted below 0? Where? (Note we only have ES saved)
  below.zero.pre <- sapply(1:12, function(ix) {
    any(pre[ix, ] < 0)
  })
  # print(length(below.zero.pre[below.zero.pre]))
  # 6 71 58 62 70 61 62 66 64 47 8 6 19 4 7 14 4 2 2 8 4 5 9 12 10 37 5 16 74 29
  # so seems to be most common early, before outbreak gets going (although some other high spots as well)
  print(which(below.zero.pre)) # almost exclusively I and newI; some S (17, 117, 125, 56, 84, 125, 126, 101, 123, 57, 121, 123, 136); never parameters!
  # note that this is only for one season/subtype/country, though
  # S tend to be commuting compartments, which have far fewer people than diagonal/main compartments
  # for obs: early and late only, but then ES (the only country we have files for) always has something
  
  # # Check 3: After fxn call, all are at least 0
  # below.zero.post <- sapply(1:437, function(ix) {
  #   any(post[ix, ] < 0)
  # })
  # print(length(below.zero.post[below.zero.post])) # good!
  
}

# Compare isolated, too:
a <- read.csv('python/results/outputOP_A(H1)_ISOLATED.csv')
a <- a[a$run == 0 & a$season == '2010-11' & a$fc_start == 69 & a$result == 'fcast', ]
# ggplot(data = a, aes(x = time, y = Est, col = result, group = fc_start)) + geom_line() + facet_wrap(~country) + theme_classic()

b <- read.csv('cluster/individualCountries/outputs/outputOP_1e+05_10_AH1_OEVold.csv')
b <- b[b$run == 0 & b$season == '2010-11' & b$fc_start == 69 & b$result == 'fcast', ]
ggplot(data = b, aes(x = time, y = Est, col = result, group = fc_start)) + geom_line() + facet_wrap(~country) + theme_classic()

ggplot(data = a, aes(x = time, y = Est)) + geom_line(col = 'steelblue2') + facet_wrap(~country, scales = 'free_y') + theme_classic() +
  geom_point(data = b, col = 'salmon')
# look good now! what's the difference with the cluster code??

















