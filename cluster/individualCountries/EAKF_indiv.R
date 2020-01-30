## R Function to run Ensemble Adjustment Kalman Filter with inflation and then forecast
## Based on Anderson JL (2001) Mon Weather Rev 129: 2884-2903.
##          and Karspeck AR, Anderson JL (2007) J Climate 20: 4638-4658.
##          and Shaman J, Karspeck A (2012) Proc Natl Acad Sci USA 109: 20425-20430.
## nsn: number of weeks in a season, b/c it's run retrospectively, we have the whole time series
## ntrn: number of weeks for the training process, beyond which we do forecast
##  More information in 'Comparison of filtering methods for the modeling and retrospective forecasting of influenza epidemics' (PLoS Compute Biol)
##  by Wan Yang, Alicia Karspeck, and Jeffrey Shaman, 2014

EAKF_rFC<-function(num_ens, tmstep, param.bound, obs_i=obs_i,ntrn=1, 
                    obs_vars,tm.ini=273, tm.range=273:500){

  num_times <- floor(length(tm.range) / tmstep)
  nfc <- nsn - ntrn # number of weeks for forecasting
  tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep)
  
  theta_low <- param.bound[, 1]; theta_up <- param.bound[, 2]
  
  So <- matrix(0, 6, num_ens) # S, I, 4 params
  xprior <- array(0, c(7, num_ens, ntrn + 1)) # add newI for each compartment, too
  xpost <- array(0, c(7, num_ens, ntrn))
  fcast <- array(0, c(3, num_ens, nfc))
  
  ### Set initial conditions based on input parameters
  param.bound <- cbind(c(S0_low, I0_low, theta_low), c(S0_up, I0_up, theta_up))
  parms <- t(lhs(num_ens, param.bound))
  
  So[1, ] <- parms[1, ] * N
  So[2, ] <- parms[2, ] * N
  So[3:6, ] <- parms[3:6, ]
  
  ### Calculate the reproductive number at time t BT1 and the transmission rate
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- AH.count[beta.range]
  AHpt <- as.matrix(AHpt, length(AHpt), 1)
  ones1 <- matrix(1, length(AHpt), 1) # matrix of 1 to make the dimension match
  # b <- log(So[5, ] - So[6, ]); b <- ones1 %*% b # expand b to a matrix with each col for each ensemble member
  b <- log(So[6, ]); b <- ones1 %*% b
  a <- -180
  ones2 <- matrix(1, 1, num_ens)
  AHpt <- AHpt %*% ones2
  BT1 <- exp(a * AHpt + b) + ones1 %*% (So[5, ] - So[6, ])
  beta <- BT1 / (ones1 %*% So[4, ])
  tcurrent <- tm.ini
  
  # integrate forward 1 step foreward
  Sr_tmp <- propagateParSIR(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                            S0 = So[1,], I0=So[2,], N, D=So[4,], L=So[3,], beta, realdata=T)
  xprior[1,, 1] <- tail(Sr_tmp$S, 1) 
  xprior[2,, 1] <- tail(Sr_tmp$I, 1)
  xprior[3:6,, 1] <- So[3:6,]
  xprior[7,, 1] <- tail(Sr_tmp$newI, 1)
  
  ####  Define the mapping operator H
  H=7;   #  observing the 7th variable (new infections)
  
  ### Training process:
  for (tt in 1:ntrn) {
    
    if (!is.na(obs_i[tt]) & !is.na(obs_vars[tt]) & obs_vars[tt] > 0) {
      
      # Inflate all states and parameters:
      inflat <- diag(x = rep(lambda, 7), 7, 7)
      xmn <- rowMeans(xprior[,, tt])
      xprior[,, tt] <- inflat %*% (xprior[,, tt] - xmn %*% matrix(1, 1, num_ens)) + xmn %*% matrix(1, 1, num_ens)
      
      # Ensure no xprior[,, tt] below zero:
      xprior[,, tt][xprior[,, tt] < 0] <- 0
      
      # Get the variance of the ensemble:
      obs_var <- obs_vars[tt]
      prior_var <- var(xprior[H,, tt])
      post_var = prior_var * obs_var / (prior_var + obs_var)
      
      if (prior_var == 0 | is.na(prior_var)) {
        post_var <- 0
        prior_var <- 1e-3
      }
      
      # print(prior_var); print(post_var)
      
      # Compute the prior and post means:
      prior_mean <- mean(xprior[H,, tt])
      post_mean <- post_var * (prior_mean / prior_var + obs_i[tt] / obs_var)
      # print(prior_mean); print(post_mean)
      
      # Compute alpha and adjust distribution to conform to posterior moments
      alp <- sqrt(obs_var / (obs_var + prior_var))
      dy <- post_mean + alp * ((xprior[H,, tt]) - prior_mean) - xprior[H,, tt];
      
      # Getting the covariance of the prior state space and observations (which could be part of state space, e.g. infections)
      rr <- NULL
      for (j in 1:dim(xprior[,, tt])[1]) {
        C <- cov(xprior[j,, tt], xprior[H,, tt]) / prior_var  # covariance/variance of x.obs
        rr <- append(rr, C)
      }
      dx <- rr %*% t(dy)
      
      # Get the new ensemble and save prior and posterior
      xnew <- xprior[,, tt] + dx
      
      # Corrections to data aphysicalities
      xnew[1:6, ] <- Fn_checkxnobounds(xnew[1:6, ])
      xnew[xnew < 0] <- 0
      
      xpost[,, tt] <- xnew;
      
      # Integrate forward one time step
      b <- log(xpost[6,, tt]); b <- ones1 %*% b # expand b to a matrix with each col for each particle
      a <- -180
      BT1 <- exp(a * AHpt + b) + ones1 %*% (xpost[5,, tt] - xpost[6,, tt])
      beta <- BT1 / (ones1 %*% xpost[4,, tt])
      
      tcurrent <- tm.ini + tmstep * tt
      Sr_tmp <- propagateParSIR(tcurrent + dt, tcurrent + tmstep, dt, xpost[1,, tt], xpost[2,, tt], N,
                                D = xpost[4,, tt], L = xpost[3,, tt], beta, realdata=T)
      xprior[1,, tt+1] = tail(Sr_tmp$S, 1)
      xprior[2,, tt+1] = tail(Sr_tmp$I, 1)
      xprior[3:6,, tt+1] = xpost[3:6,, tt]
      xprior[7,, tt+1] = tail(Sr_tmp$newI, 1)
      
    } else { # run forward without fitting

      #  Don't use xpost for propagating - use xprior that resulted from previous integration
      #  Integrate forward one time step
      b <- log(xprior[6,, tt]); b <- ones1 %*% b # expand b to a matrix with each col for each particle
      a <- -180
      BT1 <- exp(a * AHpt + b) + ones1 %*% (xprior[5,, tt] - xprior[6,, tt])
      beta <- BT1 / (ones1 %*% xprior[4,, tt]); 
      tcurrent <- tm.ini + tmstep * tt;
      Sr_tmp <- propagateParSIR(tcurrent + dt, tcurrent + tmstep, dt, xprior[1,, tt], xprior[2,, tt], N,
                                D = xprior[4,, tt], L = xprior[3,, tt], beta, realdata = T)
      xprior[1,,tt+1]=tail(Sr_tmp$S,1);
      xprior[2,,tt+1]=tail(Sr_tmp$I,1);
      xprior[3:6,,tt+1]=xprior[3:6,,tt];
      xprior[7,,tt+1]=tail(Sr_tmp$newI,1);
      # !!! So in this case, xpost is not updated at all?
      
    }
    
  }
  # End of training
  
  ### Forecast:
  b <- log(xpost[6,, ntrn]); b <- ones1 %*% b # expand b to a matrix with each col for each particle
  a <- -180
  BT1 <- exp(a * AHpt + b) + ones1 %*% (xpost[5,, ntrn] - xpost[6,, ntrn])
  
  beta <- BT1 / (ones1 %*% xpost[4,, ntrn]) 
  tcurrent <- tm.ini + tmstep * ntrn
  Sr_tmp <- propagateParSIR(tcurrent + dt, tcurrent + tmstep * nfc, dt, xpost[1,, ntrn], xpost[2,, ntrn], N,
                            D = xpost[4,, ntrn], L = xpost[3,, ntrn], beta, realdata = T)
  fcast[1,, ] <- t(Sr_tmp$S[tmstep * (1:nfc) + 1, ]) # num_ens, time 
  fcast[2,, ] <- t(Sr_tmp$I[tmstep * (1:nfc) + 1, ])
  fcast[3,, ] <- t(Sr_tmp$newI[tmstep * (1:nfc) + 1, ] - Sr_tmp$newI[tmstep * (0:(nfc - 1)) + 1, ])
  # get weekly incidence. (Note: newI in the SIRS function is cumulative new cases)
  
  # calculate the mean of ensemble
  xprior_mean = xpost_mean = xsd = matrix(0, 7, ntrn)
  for (tt in 1:ntrn) {
    xprior_mean[ ,tt] <- apply(xprior[,, tt], 1, mean, na.rm = T)
    xpost_mean[ ,tt] <- apply(xpost[,, tt], 1, mean, na.rm = T)
    xsd[ ,tt] <- apply(xpost[,, tt], 1, sd, na.rm = T)
  }
  
  obs.na <- which(is.na(obs_i))[which(is.na(obs_i)) %in% 1:(dim(xpost_mean)[2])]
  xpost_mean[, obs.na] <- NA
  
  fcast_mean = fcast_sd = matrix(0, 3, nfc)
  for (tt in 1:nfc) {
    fcast_mean[ ,tt] <- apply(fcast[,, tt], 1, mean, na.rm = T)
    fcast_sd[ ,tt] <- apply(fcast[,, tt], 1, sd, na.rm = T)
  }
  
  #### Metrics for comparison
  Y <- c(xpost_mean[7, ], fcast_mean[3, ]) # newI
  Y.ens <- matrix(NA, nrow = num_ens, ncol = nsn)
  Y.ens[, 1:ntrn] <- xpost[7,, 1:ntrn]; Y.ens[, (ntrn + 1):(ntrn + nfc)] <- fcast[3,, 1:nfc]
  
  Y[is.na(obs_i)] <- NA
  Y.ens[, which(is.na(obs_i))] <- NA
  # for (ensmem in 1:num_ens) {
  #   Y.ens[ensmem, ][is.na(obs_i)] <- NA
  # }
  
  pkwks <- rep(0, num_ens)
  obs_pkwk <- which.max(obs_i)
  for (i in 1:num_ens){
    # pkwks[i] <- which.max(c(xpost[7, i, 1:ntrn], fcast[3, i, 1:nfc]))
    pkwks[i] <- which.max(Y.ens[i, ])
  }
  pkwk_mode <- MODE(pkwks)[1]
  pkwk_mode_perc <- MODE(pkwks)[2] / num_ens
  leadpkwk_mode <- pkwk_mode-ntrn
  pkwk_var <- var(pkwks)
  deno_obs_i <- ifelse(obs_i[ntrn + 1]==0, 1, obs_i[ntrn + 1])
  rdiff_next_newI <- (fcast_mean[3, 1] - deno_obs_i) / deno_obs_i
  # corr <- cor(Y, head(obs_i, nsn)); rms <- sqrt(mean((Y - head(obs_i, nsn))^2))
  delta_sum_newI <- sum(Y, na.rm = TRUE) - sum(head(obs_i, nsn))
  delta_pkwk_mode <- pkwk_mode - obs_pkwk
  
  corr <- cor(Y, head(obs_i, nsn), use='pairwise.complete.obs');
  rms <- sqrt(mean((Y - head(obs_i, nsn))**2, na.rm = T));
  
  corr.fcast <- cor(fcast_mean[3,], obs_i[(ntrn + 1):nsn], use='pairwise.complete.obs')
  rms.fcast <- sqrt(mean((fcast_mean[3,] - obs_i[(ntrn + 1):nsn]) ^ 2, na.rm=T))
  
  obs_adj <- obs_i[(ntrn + 1):nsn]
  obs_adj[obs_adj == 0 & !is.na(obs_adj)] <- 1
  mape <- sum(abs((obs_adj - fcast_mean[3,]) / obs_adj), na.rm = T) *
    100 / length((ntrn + 1):nsn) # should it be where obs_i NOT NA?
  wape <- (sum(abs(obs_i[(ntrn + 1):nsn] - fcast_mean[3,]), na.rm = T) /
    sum(obs_i[(ntrn + 1):nsn], na.rm=T)) * 100 / length((ntrn + 1):nsn)
  smape <- sum(abs(fcast_mean[3,] - obs_i[(ntrn + 1):nsn]) /
                 ((fcast_mean[3,] + obs_i[(ntrn + 1):nsn]) / 2), na.rm = T) *
    100 / length((ntrn + 1):nsn)
  
  ## check the mean as well
  pkwk_mean <- which.max(Y)
  delta_pkwk_mean <- pkwk_mean - obs_pkwk
  leadpkwk_mean <- pkwk_mean - ntrn
  
  ## look at intensity, AR, eventually onset
  obs_pkwk_1  <-  obs_pkwk + wk_start - 1
  
  obs_peak_int  <-  max(obs_i, na.rm = T)
  peak_intensity <- max(Y, na.rm = T)
  intensity_err <- peak_intensity - obs_peak_int
  ili_peaks  <-  rep(0, num_ens)
  for (i in 1:num_ens){
    # ili_peaks[i]  <-  max(c(xpost[7, i, 1:ntrn], fcast[3, i, 1:nfc]))
    ili_peaks[i] <- max(Y.ens[i, ], na.rm = TRUE)
  }
  peak_intensity_var <- var(ili_peaks)
  
  totAttackObs <- sum(obs_i[!is.na(obs_vars)], na.rm = T)
  tot_attack <- sum(Y[!is.na(obs_i) & !is.na(obs_vars)]) # summing predicted cases only where actual data is NOT NA - otherwise we are adding in "extra" compared to the real data
  delta_sum_newI <- tot_attack - totAttackObs
  totalARs <- rep(0, num_ens)
  for (i in 1:num_ens) {
    totalARs[i] <- sum(c(xpost[7, i, 1:ntrn][!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])], fcast[3, i, 1:nfc][!is.na(obs_i[(ntrn + 1):nsn]) & !is.na(obs_vars[(ntrn + 1):nsn])]))
  }
  ar_var <- var(totalARs)
  
  # accuracy in predicting newI in the following weeks
  if(dim(fcast_mean)[2] > 0 & !is.na(obs_i[ntrn + 1])){ # if there is a forecast
    deno_obs_i <- ifelse(obs_i[ntrn + 1] == 0, 1, obs_i[ntrn + 1])
    rdiff_next_newI <- (fcast_mean[3, 1] - deno_obs_i) / deno_obs_i
  } else {
    rdiff_next_newI <- NA
  }
  
  # newI at the 2nd following week
  if(dim(fcast_mean)[2] > 1 & !is.na(obs_i[ntrn + 2])){ # if the forecast is made for more than 1 week
    deno_obs_i<-ifelse(obs_i[ntrn + 2] == 0, 1, obs_i[ntrn + 2])
    rdiff_next_newI2 <- (fcast_mean[3, 2] - deno_obs_i) / deno_obs_i
  } else {
    rdiff_next_newI2  <-  NA
  }
  
  # newI at the 3rd following week
  if (dim(fcast_mean)[2] > 2 & !is.na(obs_i[ntrn + 3])){ # if the forecast is made for more than 2 weeks
    deno_obs_i <- ifelse(obs_i[ntrn + 3] == 0, 1, obs_i[ntrn + 3])
    rdiff_next_newI3 <- (fcast_mean[3, 3] - deno_obs_i) / deno_obs_i
  } else{
    rdiff_next_newI3 <- NA
  }
  
  # newI at the 4th following week
  if (dim(fcast_mean)[2] > 3 & !is.na(obs_i[ntrn + 4])){ # if the forecast is made for more than 3 weeks
    deno_obs_i <- ifelse(obs_i[ntrn + 4] == 0, 1, obs_i[ntrn + 4])
    rdiff_next_newI4 <- (fcast_mean[3, 4] - deno_obs_i) / deno_obs_i
  } else{
    rdiff_next_newI4 <- NA
  }
  
  onsetObs3 <- findOnset(obs_i, 300)$onset
  onsetObs4 <- findOnset(obs_i, 400)$onset
  onsetObs5 <- findOnset(obs_i, 500)$onset
  onsetObs6 <- findOnset(obs_i, 600)$onset
  
  # onset3 <- findOnset(Y, 300)$onset
  # onset4 <- findOnset(Y, 400)$onset
  # onset5 <- findOnset(Y, 500)$onset
  # onset6 <- findOnset(Y, 600)$onset
  
  onsets3 = onsets4 = onsets5 = onsets6 = rep(0,num_ens)
  peakWeeks = peakIntensities = rep(NA, num_ens)
  nextILI  <-  matrix(NA, nrow=4, ncol=num_ens)
  for (i in 1:num_ens){
    # yy <- c(xpost[7,i,1:ntrn], fcast[3,i,1:nfc]); # trajectory for that particle
    yy <- Y.ens[i, ]
    
    onsets3[i] <- findOnset(yy, 300)$onset
    onsets4[i] <- findOnset(yy, 400)$onset
    onsets5[i] <- findOnset(yy, 500)$onset
    onsets6[i] <- findOnset(yy, 600)$onset
    
    # find peak week as predicted by this ensemble member
    peakWeeks[i]  <- which.max(yy);
    if(!is.na(peakWeeks[i]))
      peakWeeks[i] <- peakWeeks[i] + wk_start - 1
    
    # find peak intensity as predicted by this ensemble member
    peakIntensities[i] <- max(yy, na.rm=T)
    
    # find forecasts of this ensemble member for the next 4 weeks; row_i = ith week ahead
    for(j in 1:min(nfc, 4)){
      nextILI[j, i] <- fcast[3, i, j]
    }
    
  } # end of ensemble loop
  
  #calculate prob. distribution for onsets
  # col1 = week#, col2=prob.dist
  onsets3DistNA <- matrix(c(-1, round(length(onsets3[is.na(onsets3)]) / length(onsets3), 4)), nrow=1, ncol=2)
  onsets3 <- onsets3[!is.na(onsets3)];
  onsets3Dist <- matrix(NA, nrow=length(unique(onsets3)), ncol=2)
  row <- 1
  for(i in sort(unique(onsets3))){
    onsets3Dist[row, 1] <- i;
    onsets3Dist[row, 2] <- round(length(onsets3[onsets3 == i]) / 300, 4)
    row <- row+1
  }
  if (onsets3DistNA[, 2] > max(onsets3Dist[, 2])) {
    onset3 <- NA
  } else {
    onset3 <- findOnset(Y, 300)$onset
  }
  onsets3Dist <- rbind(onsets3Dist, onsets3DistNA)
  
  onsets4DistNA <- matrix(c(-1, round(length(onsets4[is.na(onsets4)]) / length(onsets4), 4)), nrow = 1, ncol = 2)
  onsets4 <- onsets4[!is.na(onsets4)];
  onsets4Dist <- matrix(NA, nrow=length(unique(onsets4)), ncol = 2)
  row<-1
  for(i in sort(unique(onsets4))){
    onsets4Dist[row, 1] <- i
    onsets4Dist[row, 2] <- round(length(onsets4[onsets4 == i]) / 300, 4)
    row <- row+1
  }
  if (onsets4DistNA[, 2] > max(onsets4Dist[, 2])) {
    onset4 <- NA
  } else {
    onset4 <- findOnset(Y, 400)$onset
  }
  onsets4Dist <- rbind(onsets4Dist, onsets4DistNA)
  
  onsets5DistNA <- matrix(c(-1, round(length(onsets5[is.na(onsets5)]) / length(onsets5), 4)), nrow = 1, ncol = 2)
  #print(length(onsets5))
  onsets5 <- onsets5[!is.na(onsets5)];
  onsets5Dist <- matrix(NA, nrow=length(unique(onsets5)), ncol=2)
  row <- 1
  for(i in sort(unique(onsets5))){
    onsets5Dist[row, 1] <- i;
    onsets5Dist[row, 2] <- round(length(onsets5[onsets5==i])/300, 4)
    row<-row+1
  }
  if (onsets5DistNA[, 2] > max(onsets5Dist[, 2])) {
    onset5 <- NA
  } else {
    onset5 <- findOnset(Y, 500)$onset
  }
  onsets5Dist <- rbind(onsets5Dist, onsets5DistNA)
  
  onsets6DistNA <- matrix(c(-1, round(length(onsets6[is.na(onsets6)]) / length(onsets6), 4)), nrow = 1, ncol = 2)
  onsets6 <- onsets6[!is.na(onsets6)]
  onsets6Dist <- matrix(NA, nrow = length(unique(onsets6)), ncol = 2)
  row <- 1
  for(i in sort(unique(onsets6))){
    onsets6Dist[row, 1] <- i;
    onsets6Dist[row, 2] <- round(length(onsets6[onsets6 == i]) / 300, 4)
    row<-row+1
  }
  if (onsets6DistNA[, 2] > max(onsets6Dist[, 2])) {
    onset6 <- NA
  } else {
    onset6 <- findOnset(Y, 600)$onset
  }
  onsets6Dist <- rbind(onsets6Dist, onsets6DistNA)
  
  # calculate onset errors
  delta_onset3 <- onset3 - onsetObs3
  delta_onset4 <- onset4 - onsetObs4
  delta_onset5 <- onset5 - onsetObs5
  delta_onset6 <- onset6 - onsetObs6
  
  #calculate prob. distribution for peak weeks
  # col1 = week#, col2=prob.dist
  peakWeeksDistNA <- matrix(c(-1, round(length(peakWeeks[is.na(peakWeeks)]) / length(peakWeeks), 4)), nrow = 1, ncol = 2)
  
  peakWeeks <- peakWeeks[!is.na(peakWeeks)]
  peakWeeksDist <- matrix(NA, nrow=length(unique(peakWeeks)), ncol = 2)
  row <- 1
  for(i in sort(unique(peakWeeks))){
    peakWeeksDist[row, 1] <- i;
    peakWeeksDist[row, 2] <- round(length(peakWeeks[peakWeeks == i]) / length(peakWeeks), 4)
    row<-row + 1
  }
  peakWeeksDist <- rbind(peakWeeksDist, peakWeeksDistNA)
  
  #calculate prob. distribution for peak intensities
  # we bin in increments of 1e3 upto 1e4. An extra bin for >1e4
  # CHANGE BINS to be size 500
  reqLimits <- seq(0, 14000, by = 500)
  peakIntensities <- peakIntensities[!is.na(peakIntensities)]
  peakIntensitiesDist <- matrix(NA, nrow = length(reqLimits), ncol = 2)
  
  row <- 1
  for(i in 2:length(reqLimits)){
    peakIntensitiesDist[row, 1] <- reqLimits[i]
    peakIntensitiesDist[row, 2] <- round(length(peakIntensities[peakIntensities >= reqLimits[i - 1] & peakIntensities < reqLimits[i]]) / length(peakIntensities), 4)
    row<-row+1    
  }
  # > 1e4
  peakIntensitiesDist[row, 1] <- 1e5
  peakIntensitiesDist[row, 2] <- round(length(peakIntensities[peakIntensities >= max(reqLimits)]) / length(peakIntensities), 4)
  
  # calculate prob. distribution for next 4 week forecasts
  # we bin in increments of 1e3 upto 1e4. An extra bin for >1e4
  nextILIDist <- matrix(NA, nrow = length(reqLimits), ncol = nrow(nextILI) + 1)
  row <- 1
  for(i in 2:length(reqLimits)){
    nextILIDist[row, 1] <- reqLimits[i]
    for(j in 1:nrow(nextILI)){
      values <- nextILI[j, ]
      values <- values[!is.na(values)]
      nextILIDist[row, j + 1] <- round(length(values[values >= reqLimits[i - 1] & values < reqLimits[i]]) / length(values), 4)
    }
    row <- row + 1
  }
  # > 1e4
  nextILIDist[row, 1] <- 1e5
  for(j in 1:nrow(nextILI)){
    values <- nextILI[j, ]
    values <- values[!is.na(values)]
    nextILIDist[row, j + 1] <- round(length(values[values >= max(reqLimits)]) / length(values), 4)
  }
  
  ## Calculate onset variances
  onset3_var <- var(onsets3, na.rm = T) 
  onset4_var <- var(onsets4, na.rm = T) 
  onset5_var <- var(onsets5, na.rm = T) 
  onset6_var <- var(onsets6, na.rm = T) 
  
  ### Output results:
  tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep) # time
  fc_start <- ntrn + wk_start - 1 # the time of forecast
  
  out1 <-cbind(rep(fc_start, length((1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])])),
               tstep[(1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])]],
               (1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])] + 40 - 1,
               rep('train', length((1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])])),
               matrix(t(xpost_mean)[(1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])], ], ncol = 7),
               matrix(t(xsd)[(1:ntrn)[!is.na(obs_i[1:ntrn]) & !is.na(obs_vars[1:ntrn])], ], ncol = 7));
  out2 <-cbind(rep(fc_start, nfc),
               tstep[(ntrn+1):nsn],
               ((ntrn + 1):nsn) + 40 - 1,
               rep('fcast', nfc),
               t(fcast_mean),
               t(fcast_sd))
  na.mat <- matrix(NA, nrow = nfc, ncol = 4)
  out2 <- cbind(out2[, 1:6], na.mat, out2[, 7:9], na.mat, out2[, 10])
  out3 <-t(c(fc_start,
             obs_pkwk_1, pkwk_mode + wk_start - 1, delta_pkwk_mode, pkwk_mean + wk_start - 1, delta_pkwk_mean, leadpkwk_mode, leadpkwk_mean, sqrt(pkwk_var),
             obs_peak_int, peak_intensity, intensity_err, sqrt(peak_intensity_var),
             totAttackObs, tot_attack, delta_sum_newI, sqrt(ar_var),
             obs_i[ntrn + 1], obs_i[ntrn + 2], obs_i[ntrn + 3], obs_i[ntrn + 4],
             fcast_mean[3, 1], fcast_mean[3, 2], fcast_mean[3, 3], fcast_mean[3, 4],
             rdiff_next_newI, rdiff_next_newI2, rdiff_next_newI3, rdiff_next_newI4,
             onsetObs3, onsetObs4, onsetObs5, onsetObs6, onset3, onset4, onset5, onset6,
             delta_onset3, delta_onset4, delta_onset5, delta_onset6,
             sqrt(onset3_var), sqrt(onset4_var), sqrt(onset5_var), sqrt(onset6_var),
             corr, rms, corr.fcast, rms.fcast, mape, wape, smape))
  # unlike networked model, no info on ends or durations here
  out4 <-cbind(rep(fc_start, ntrn),
               tstep[1:ntrn],
               t(xprior_mean))
  # This probably still has values where data is NA, but we're not using this so far, so it probably doesn't matter; also note that we'd have to remove
  # one FORWARD from missing data
  out5  <- formatDistNEW(onsets3Dist, onsets4Dist, onsets5Dist, onsets6Dist,peakWeeksDist, peakIntensitiesDist, nextILIDist)
  out5  <- cbind(rep(fc_start, nrow(out5)), out5)
  out6 = cbind(c('pi', '1week', '2week', '3week', '4week'), rep(fc_start, 5), rbind(peakIntensities, nextILI))
  
  colnames(out1) = colnames(out2) = c('fc_start', 'time', 'week', 'result',
                                      'S', 'I', 'L', 'D', 'R0max', 'R0diff', 'Est',
                                      'S_sd', 'I_sd', 'L_sd', 'D_sd', 'R0max_sd', 'R0diff_sd', 'Est_sd')
  out1 <- rbind(out1, out2)
  colnames(out3) = c('fc_start',
                     'obs_pkwk', 'pkwk_mode', 'delta_pkwk_mode', 'pkwk_mean', 'delta_pkwk_mean', 'leadpkwk_mode', 'leadpkwk_mean', 'pkwk_sd',
                     'obs_peak_int', 'peak_intensity', 'intensity_err', 'peak_intensity_sd',
                     'totAttackObs', 'tot_attack', 'delta_AR', 'AR_sd',
                     'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week',
                     'fcast_1week', 'fcast_2week', 'fcast_3week', 'fcast_4week',
                     'delta_1w', 'delta_2w', 'delta_3w', 'delta_4w',
                     'onsetObs3', 'onsetObs4', 'onsetObs5', 'onsetObs6', 'onset3', 'onset4', 'onset5', 'onset6',
                     'delta_onset3', 'delta_onset4', 'delta_onset5', 'delta_onset6', 'onset3_sd','onset4_sd','onset5_sd','onset6_sd',
                     'corr', 'rmse', 'corr_fcast', 'rmse_fcast', 'mape', 'wape', 'smape')
  colnames(out5) <- c('fc_start', 'metric', 'week', 'prob')
  rownames(out6) <- NULL
  colnames(out6) <- c('metrics', 'fc_start', 1:300)
  
  # check that obs_i always NA/0 when obs_vars = 0
  # need to also exclude where obs_vars is NA in network model? since those 0s in obs_i aren't "real" 0s?
  # 
  
  out <- list(op = out1, metrics = out3, trainprior = out4, dist = out5, ensembles = out6);
}
