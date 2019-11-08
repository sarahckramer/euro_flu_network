
### Home directories:
dir_home_code <- '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/code/'
dir_home_data <- '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/data/'

### Read in libraries
library("truncnorm"); library("tgp"); # for lhs
library("MASS"); # for multivariate normal distribution
require(plyr); # for function count

### Read in model functions
source(paste(dir_home_code,"EpiModelsTrackIncidence_SEED.R",sep="")) # SIRS model
source(paste(dir_home_code,"Fn_checkxnobounds.R",sep="")) # function to check DA aphysicality
source(paste(dir_home_code,'Util.R',sep=""));#buildOutput and ConvertDaysToDate functions; simple utilities

### Read in filter function
source(paste(dir_home_code,'Fn_EAKF_rFC.R',sep=""))
# Eventually try any other filters? Any other models?

### Specify headers for output file
metrics_header <- c('country','season','run','scaling','oev','lambda','fc_start','obs_pkwk',
                    'pkwk_mode','delta_pkwk_mode','pkwk_mean','delta_pkwk_mean','leadpkwk_mode',
                    'leadpkwk_mean','pkwk_sd','obs_peak_int','peak_intensity','intensity_err',
                    'peak_intensity_sd','totAttackObs','tot_attack','delta_AR','obs_1week',
                    'obs_2week','obs_3week','obs_4week','fcast_1week','fcast_2week',
                    'fcast_3week','fcast_4week','delta_1w','delta_2w','delta_3w','delta_4w',
                    'rms','corr','onset3','onset4','onset5','onset6','onset3_sd','onset4_sd',
                    'onset5_sd','onset6_sd','onsetObs3','onsetObs4','onsetObs5','onsetObs6',
                    'rms_fcast','corr_fcast','mape','wape','smape')
output_header <- c('country','season','run','scaling','oev','lambda','result','fc_start',
                   'time','time_date','week','estimate','est_sd','S','S_sd','I','I_sd','L',
                   'L_sd','D','D_sd', 'R0max','R0max_sd','R0min','R0min_sd','observed',
                   'observed_sd')
dist_header <- c('country','season','run','scaling','oev','lambda','fc_start','metric',
                 'week','prob')

### Read in list of variables
if(!exists("phi")) {
  phi <- read.csv(paste(dir_home_data, 'phi.csv', sep=''), header=F)
  params <- phi[,1:4]
  susceps = infects = NULL
  for(i in 5:31) {
    susceps <- append(susceps, phi[,i])
    infects <- append(infects, phi[,27+i])
    # What is the 59th column?
  }
}

### Global variables
N <- 1e5 # population
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8;
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2;
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)
param.bound <- cbind(theta_low, theta_up)

# Values slightly different in Jeff's code: 2-7 days (D), 2-10 years (L)

### Parameters for the filters
discrete <- FALSE # run the SIRS model continuously
metricsonly <- FALSE # save all outputs
lambda <- 1.03 # inflation factor for the ensemble filters
oev_denom <- 10 #c(1,10,100) # denominator for observation error variance
num_ens <- 300 # use 300 for ensemble filters, 10000 for particle filters
num_runs <- 5

### Specify the country for which we are performing a forecast
countries <- c('Australia', 'Austria', 'Bangladesh', 'Belarus', 'Belgium', 'Bhutan', 'Bolivia',
               'Brazil', 'Bulgaria', 'Cambodia', 'Canada', 'Chile', 'Colombia', 'Croatia',
               'Cuba', 'Czechia', 'Denmark', 'Ecuador', 'Estonia', 'Finland', 'France', 'Georgia',
               'Germany', 'Greece', 'Honduras', 'Hungary', 'Iceland', 'Indonesia', 'Ireland',
               'Israel', 'Italy', 'Kazakhstan', 'Kenya', 'Kyrgyzstan', 'Latvia', 'Lithuania',
               'Luxembourg', 'Madagascar', 'Mexico', 'Morocco', 'Netherlands', 'New Zealand',
               'Norway', 'Oman', 'Pakistan', 'Paraguay', 'Peru', 'Poland', 'Portugal',
               'Republic of Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Singapore',
               'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Thailand', 'Turkey',
               'Ukraine', 'United Kingdom', 'United States of America', 'Uzbekistan')

counts.temperate.north <- c(2, 4:5, 9, 11, 14, 16:17, 19:24, 26:27, 29:32, 34:37, 39:41,
                            43, 48:53, 55:58, 61:65) # removed Switzerland
counts.temperate.south <- c(1, 12, 42)
counts.tropics <- c(3, 6:8, 10, 13, 15, 18, 25, 28, 33, 38, 44:47, 54, 60)

load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/data/scalings_4-19.RData')
#load('/Users/Sarah/Dropbox/spatial_model/forecasts/data/scalings_4-16_ALT.RData')

for(count.index in c(counts.temperate.north, counts.temperate.south)) {
  
  if(count.index %in% counts.temperate.north) {
    wk_start <- 40 # season starts at week 40
    source(paste(dir_home_code,'Fn_initializations.R',sep="")) # source of some funtions for initializations
  } else {
    wk_start <- 14
    source(paste(dir_home_code,'Fn_initializations_SOUTH.R',sep=""))
  }
  
  ### Read in humidity data
  ah <- read.csv(paste(dir_home_data, 'ah_4-17_formatted.csv', sep=''), header=T)
  AH <- matrix(c(ah[,count.index], ah[,count.index]), 365*2, 1) # basically have the data twice, since seasons cross years
  
  ### Read in influenza data
  iliiso <- read.csv(paste(dir_home_data, 'WHO_dat_ALL_05-12_NOPAN.csv', sep=''), header=T)
  iliiso <- as.matrix(iliiso[,count.index + 1], dim(iliiso)[1], dim(iliiso)[2])
  iliiso[iliiso == -1] <- NA
  
  ### Set scaling
  if(count.index != 21) {
    gamma <- new.scalings[[count.index]]
  } else {
    gammas <- new.scalings[[count.index]]
  }
  
  ### Choose season and get observations
  seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17')
  # Focus just on seasonal for now
  
  ### Initialize output data frame
  outputMetrics <- NULL
  outputOP <- NULL
  outputDist <- NULL
  outputEns <- NULL
  
  ### Main loop
  for(season in seasons) {
    
    if(count.index == 21) {
      if(season %in% c('2010-11', '2011-12', '2012-13', '2013-14')) {
        gamma <- gammas[1]
      } else {
        gamma <- gammas[2]
      }
    }
    
    tmp <- Fn_dates(season)
    weeks <- tmp$weeks + 3
    start_date <- tmp$start_date
    end_date <- tmp$end_date
    nsn <- tmp$nsn
    obs_i <- iliiso[weeks, 1] * gamma # extract relevant data AND multiply by scaling
    
    #if(!all(is.na(obs_i))) {
    if(any(obs_i > 0 & !is.na(obs_i))) {
      
      print(paste(countries[count.index], season, gamma, sep=' '))
      plot(obs_i, type='b', main=paste(countries[count.index], season, sep='_'))
      
      #for(oev_denom in oev_denoms) {
      #  for(lambda in lambdas) {
      # print(paste0(oev_denom, ', ', lambda))
      
      ### Replace any leading or lagging NAs
      start.index <- 1
      while(obs_i[start.index] < 1 | is.na(obs_i[start.index])) {
        start.index <- start.index + 1
      }
      start.index <- start.index - 1
      end.index <- length(obs_i)
      while(obs_i[end.index] < 1 | is.na(obs_i[end.index])) {
        end.index <- end.index - 1
      }
      end.index <- end.index + 1
      if(start.index > 0 & end.index < (length(obs_i)+1)) {
        obs_i[c(1:start.index, end.index:length(obs_i))] <- 0
      } else if(start.index > 0 & end.index > length(obs_i)) {
        obs_i[1:start.index] <- 0
      } else if(end.index < (length(obs_i)+1) & start.index < 1) {
        obs_i[end.index:length(obs_i)] <- 0
      }
      
      ### Variance of ILI+ data
      tmp <- rep(0, length(obs_i))
      for(i in 4:length(obs_i)) {
        tmp[i] <- mean(obs_i[(i-3):(i-1)], na.rm=T)
      }
      #obs_vars <- (1e4 + (tmp^2)/50) # OEV - here using 10 as denominator
      obs_vars <- (1e5 + (tmp^2)/5)/oev_denom
      check.flag <- FALSE
      if(length(obs_vars[is.na(obs_vars)]) > 0) {
        print('obs_vars contains NAs!')
        check.flag <- TRUE
        #print(obs_i)
        print(obs_vars)
      }
      for(i in which(is.na(obs_vars))) {
        obs_before <- obs_i[i-4]
        k <- i
        while(is.na(obs_before)) {
          k <- k - 1
          obs_before <- obs_i[k]
        }
        obs_after <- obs_i[i]
        j <- i
        while(is.na(obs_after)) {
          j <- j + 1
          obs_after <- obs_i[j]
        }
        
        art.mean <- mean(c(obs_before, obs_after))
        obs_vars[i] <- (1e5 + (art.mean^2)/5)/oev_denom
      }
      if(check.flag) {
        print(obs_vars)
      }
      
      ### Get the first and last date of the simulation
      clim_start <- as.numeric(start_date - as.Date(paste('20',
                                                          substr(season, gregexpr('-', season)[[1]][1]-2,
                                                                 gregexpr('-', season)[[1]][1]-1),
                                                          '-01-01', sep=''))) + 1 - 6
      ### Number of days in the year at the beginning of the week
      clim_end <- as.numeric(end_date - as.Date(paste('20',
                                                      substr(season, gregexpr('-', season)[[1]][1]-2,
                                                             gregexpr('-', season)[[1]][1]-1),
                                                      '-01-01', sep=''))) + 1
      tm.ini <- clim_start - 1 # the end of the former week
      tm.range <- clim_start:clim_end
      
      ### Choose filter function
      #fn <- "EAKF_rFC" # can't get this to function properly - just manually added function name
      #ntrn <- 15 # number of observations for the training period; could be 1 to 51 weeks
      
      #par(mfrow=c(4,6))
      for(ntrn in (5:30)) {
        
        if(!is.na(obs_i[ntrn])) {
          #plot(obs_i, type='l', main=paste(season,'_Wk:',ntrn,'_',gamma,sep=''))
          
          for(run in 1:num_runs) {
            
            res <- EAKF_rFC(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn, obs_vars,
                            tm.ini, tm.range)
            
            ### Extract and append results
            #out=res$train[,2:9]; out[,'newI']=out[,'newI'];
            #fcast=res$fcast[,2:5]; fcast[,'newI']=fcast[,'newI'];
            
            #outputMetrics <- rbind(outputMetrics, res$metrics)
            outputMetrics = rbind(outputMetrics, cbind(countries[count.index], season, run, gamma, oev_denom, lambda, res$metrics))
            outputDist = rbind(outputDist, cbind(countries[count.index], season, run, gamma, oev_denom, lambda, res$dist))
            outputEns = rbind(outputEns, cbind(countries[count.index], season, run, gamma, oev_denom, lambda, res$ensembles))
            
            for(i in 1:2) {
              temp_res = buildOutput(res[[i]], 'long') # reorder columns
              temp_res = convertDaysToDate(temp_res, start_date) #convert 'time' to date
              obs_output = NULL
              obs_sd = NULL
              
              if(names(res)[i] == "fcast"){
                if(length(obs_i) == nsn){
                  obs_output = obs_i[(ntrn+1):nsn]
                  obs_sd = obs_vars[(ntrn+1):nsn]
                }
                
                else{
                  if(ntrn+1 <= length(obs_i)) {
                    obs_output = obs_i[(ntrn+1):length(obs_i)]
                    obs_sd = obs_vars[(ntrn+1):length(obs_vars)]
                  }
                  
                  obs_output = c(obs_output, rep(-1, nsn-length(obs_i)))
                  obs_sd = c(obs_sd, rep(-1, nsn-length(obs_vars)))
                }
              } else{
                obs_output = obs_i[1:ntrn][!is.na(obs_i[1:ntrn])]
                obs_sd = obs_vars[1:ntrn][!is.na(obs_i[1:ntrn])]
              }
              
              outputOP = rbind(outputOP, cbind(countries[count.index], season, run, gamma, oev_denom, lambda, names(res)[i], temp_res, obs_output, obs_sd))#cbind(row_prefix_2, datatypes, oev_denom, wk_start, lambda, off_type, temp_res, obs_output))
            }
            
            ### Plot results
            #plot(obs_i, type='l', main=paste(season,'_Wk:',ntrn,sep=''))
            #lines((1:ntrn)[!is.na(obs_i)[1:ntrn]],out[,8], col='blue', type='o')
            #lines(c(rep(NA,ntrn), fcast[,4]), col='red')
          }
        }
      }
      #  }
      #}
    }
  }
  
  dimnames(outputMetrics) = list(c(), as.list(metrics_header))
  dimnames(outputOP)[[2]] = as.list(output_header)
  dimnames(outputDist) = list(c(), as.list(dist_header))
  dimnames(outputEns)[[2]][1:7] <- as.list(c('country', 'season', 'run', 'scaling',
                                             'oev', 'lambda', 'metric'))
  
  #dev.off()
  
  write.csv(outputMetrics, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputMet',countries[count.index],'.csv',sep=''), row.names=F)
  write.csv(outputOP, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputOP',countries[count.index],'.csv',sep=''), row.names=F)
  write.csv(outputDist, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputDist',countries[count.index],'.csv',sep=''), row.names=F)
  write.csv(outputEns, file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputEns', countries[count.index], '.csv', sep=''), row.names=F)
  
}

# Note: France seasons seem to start around week 38?
# Assess forecast accuracy separately for pre- and post-peak
# Figure out how to handle pandemic season
# Am I starting too early? What if I remove some of the 0's from the early seasons?

### Consolidate output ###
metrics.all <- NULL
output.all <- NULL
dist.all <- NULL
ens.all <- NULL

for(i in c(counts.temperate.north, counts.temperate.south)) {
  a <- read.csv(file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputMet',countries[i],'.csv',sep=''))
  metrics.all <- rbind(metrics.all, a)
}
for(i in c(counts.temperate.north, counts.temperate.south)) {
  b <- read.csv(file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputOP',countries[i],'.csv',sep=''))
  output.all <- rbind(output.all, b)
}
for(i in c(counts.temperate.north, counts.temperate.south)) {
  c <- read.csv(file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputDist',countries[i],'.csv',sep=''))
  dist.all <- rbind(dist.all, c)
}
for(i in c(counts.temperate.north, counts.temperate.south)) {
  d <- read.csv(file = paste('/Users/Sarah/Desktop/forecasts/results_5-01/outputEns',countries[i],'.csv',sep=''))
  ens.all <- rbind(ens.all, d)
}

write.csv(metrics.all, '/Users/Sarah/Desktop/forecasts/results_5-01/OEV_10/outputMet_TEMPERATE_ALTSCALE.csv', row.names=F)
write.csv(output.all, '/Users/Sarah/Desktop/forecasts/results_5-01/OEV_10/outputOP_TEMPERATE_ALTSCALE.csv', row.names=F)
write.csv(dist.all, '/Users/Sarah/Desktop/forecasts/results_5-01/OEV_10/outputDist_TEMPERATE_ALTSCALE.csv', row.names=F)
write.csv(ens.all, '/Users/Sarah/Desktop/forecasts/results_5-01/OEV_10/outputEns_TEMPERATE_ALTSCALE.csv', row.names=F)

# Clear environment
rm(list=ls())

