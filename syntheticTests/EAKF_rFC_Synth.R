
### QUESTIONS:
# Should tm.ini be 3-4 weeks into the outbreak and not actually at time 1?
# Initiate EAKF here with distribution about a single S0 value, or just LHS as below?
# Line 171?
# Are obs_vars for actual data ever NA? When?
# Line 196: Correct to update obs_ens with dy.full?
### Line 197: set <0 to 0 - is this needed?
# What happens if we remove reinit/change reinit to just setting to 0? (Line 211)
### xpost is already accounting for differences in population sizes, right?
### Check that iterating variables (i, j, etc.) aren't being reused!
# Try: Lower OEV? Higher lambda? Rewrite from scratch looking step-by-step? Try with no travel first?

EAKF_rFC <- function(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn = 1, obs_vars,
                     tm.ini = 273, tm.range = 273:500){
  
  if (! exists("phi")){
    phi <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/data/phi.csv', header=F)
    params=phi[,1:4];
    susceps=infects=NULL; 
    for (i in 5:31){
      susceps=append(susceps,phi[,i])
      infects=append(infects,phi[,27+i])
    }
  }
  
  num_times <- floor(length(tm.range) / tmstep)
  nsn <- length(obs_i[, 1]) # length of season
  nfc <- nsn - ntrn # number of weeks for forecasting
  
  theta_low <- param.bound[, 1]; theta_up <- param.bound[, 2]
  
  ##########################################################################################
  # Determine which compartments need to be monitored
  # Others are always zero
  pos.comp <- which(t.comm > 0, arr.ind = TRUE)
  destinations <- vector('list', n)
  pos.in.vector <- c()
  count.by.pos <- c()
  main.comp.val <- c()
  # rows.by.country <- vector('list', n)
  # start <- 1
  for (i in 1:n) {
    dests <- as.vector(pos.comp[, 2][pos.comp[, 1] == i])
    dests <- sort(c(dests, i))
    destinations[[i]] <- dests
    pos.in.vector <- c(pos.in.vector, dests + n * (i - 1))
    count.by.pos <- c(count.by.pos, rep(i, length(dests)))
    main.comp.val <- c(main.comp.val, rep(i + n * (i - 1), length(dests)))
    # rows.by.country[[i]] <- start:(start + length(dests) - 1)
    # start <- start + length(dests)
  }; rm(dests)
  # num_comp <- length(unlist(rows.by.country))
  pos.in.vector <- sort(pos.in.vector)
  count.by.pos <- countries[count.by.pos]
  count.by.pos <- as.data.frame(cbind(count.by.pos, pos.in.vector, main.comp.val))
  names(count.by.pos) <- c('country', 'pos', 'main')
  # Probably best to keep large matrices, as it's easier to draw from SIR results
  # Just don't use the zeros in updating everything
  ##########################################################################################
  
  So <- matrix(0, n ** 2 * 2 + 4, num_ens) # last 4 rows are parameters
  xprior <- array(0, c(dim(So)[1] + n ** 2, num_ens, ntrn + 1)) # add newI for each compartment, too
  xpost <- array(0, c(dim(So)[1] + n ** 2, num_ens, ntrn))
  fcast <- array(0, c(n ** 2 * 3, num_ens, nfc)) # fcast: S, I, newI
  
  obsprior <- array(NA, c(n, num_ens, ntrn + 1))
  obspost <- array(NA, c(n, num_ens, ntrn))
  
  rnd <- cbind(t(do.call('rbind', replicate(n ** 2 * 2,
                                            ceiling(27 * 1e4 * runif(num_ens)),
                                            simplify = FALSE))),
               ceiling(1e4 * runif(num_ens))) # determines which values to choose from phi
  
  S0.indices <- (1:((dim(So)[1] - 4) / 2)) # where S0 are stored
  I0.indices <- S0.indices + S0.indices[length(S0.indices)] # where I0 are stored
  newI.indices <- S0.indices + S0.indices[length(S0.indices)] * 2 # where newI are stored (in fcast) (only individual compartments)
  param.indices <- (dim(So)[1] - 3):(dim(So)[1]) # where the epi parameters are stored
  
  So[S0.indices, ] <- susceps[rnd[, S0.indices]] / 5
  So[I0.indices, ] <- infects[rnd[, I0.indices]] / 5
  So[param.indices, ] <- t(params[rnd[, dim(rnd)[2]], 1:4])
  So[param.indices[1:2], ] <- So[param.indices[1:2], ] * 365
  
  ## Calculate the reproductive number at time t BT1 and the transmission rate
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- AH[beta.range, ]
  AHpt <- as.matrix(AHpt, length(AHpt), n)
  b <- log(So[param.indices[3], ] - So[param.indices[4], ])
  a <- -180
  
  beta <- lapply(1:num_ens, function(ix) {
    (exp(a * AHpt + b[ix]) + So[param.indices[4], ix]) / So[param.indices[2], ix]
  })
  tcurrent <- tm.ini
  
  # Draw S0 and I0 from So
  S0.temp <- lapply(1:num_ens, function(ix) {
    N * (matrix(So[S0.indices, ix], ncol = n, byrow = TRUE) / 100000)
  }) # list of S0 for all 300 ensembles
  I0.temp <- lapply(1:num_ens, function(ix) {
    N * (matrix(So[I0.indices, ix], ncol = n, byrow = TRUE) / 100000)
  }) # list of S0 for all 300 ensembles
  D.temp <- So[param.indices[2], ]; L.temp <- So[param.indices[1], ]
  
  # integrate 1 step forward
  Sr_tmp <- sapply(1:num_ens, function(ix) {
    propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     realdata = TRUE)
  })
  
  Sr_tmp_S <- lapply(1:num_ens, function(ix) {
    as.vector(tail(Sr_tmp[1, ix]$S, 1))
  })
  Sr_tmp_S <- t(matrix(unlist(Sr_tmp_S), ncol = n ** 2, byrow = T))
  
  Sr_tmp_I <- lapply(1:num_ens, function(ix) {
    as.vector(tail(Sr_tmp[2, ix]$I, 1))
  })
  Sr_tmp_I <- t(matrix(unlist(Sr_tmp_I), ncol = n ** 2, byrow = T))
  
  Sr_tmp_newI <- lapply(1:num_ens, function(ix) {
    as.vector(tail(Sr_tmp[3, ix]$newI, 1))
  }) # this is right - it's cumulative, but only a week has passed, so it's all the new cases
  Sr_tmp_newI <- t(matrix(unlist(Sr_tmp_newI), ncol = n ** 2, byrow = T))
  
  xprior[S0.indices, , 1] <- Sr_tmp_S
  xprior[I0.indices, , 1] <- Sr_tmp_I
  xprior[newI.indices + 4, , 1] <- Sr_tmp_newI
  xprior[param.indices, , 1] <- So[param.indices, ]
  
  ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
  obs_ens <- lapply(1:n, function(ix) {
    colSums(Sr_tmp_newI[1:n + n * (ix - 1), ])
  })
  obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F)) # each row is a single country
  obsprior[,, 1] <- obs_ens
  
  alps <- matrix(NA, nrow = n, ncol = ntrn) # record ratios of obs_var to obs+prior var
  
  #### Begin looping through observations
  #### Training process
  par(mfrow = c(2, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  for (tt in 1:ntrn) {
    # Update state variables and parameters, then integrate forward
    
    # inflate all states and parameters
    inflat <- diag(x = rep(lambda, n ** 2 * 3 + 4), n ** 2 * 3 + 4, n ** 2 * 3 + 4)
    inflat.obs <- diag(x = rep(lambda, n), n, n)
    xmn <- rowMeans(xprior[,, tt]); obs_ens.mn <- rowMeans(obs_ens)
    xprior[,, tt] <- inflat %*% (xprior[,, tt] - xmn %*% matrix(1, 1, num_ens)) + xmn %*% matrix(1, 1, num_ens)
    obs_ens <- inflat.obs %*% (obsprior[,, tt] - obs_ens.mn %*% matrix(1, 1, num_ens)) + obs_ens.mn %*% matrix(1, 1, num_ens)
    obsprior[,, tt] <- obs_ens
    
    ####  Get the variance of the ensemble
    obs_var <- obs_vars[tt, ];
    prior_var <- unlist(lapply(1:n, function(ix) { # for each country (ix)
      var(obsprior[ix,, tt])
    }))
    post_var <- prior_var * (obs_var / (prior_var + obs_var))
    
    prior_mean <- unlist(lapply(1:n, function(ix) {
      mean(obsprior[ix,, tt])
    }))
    post_mean <- post_var * (prior_mean / prior_var + as.vector(obs_i[tt, ]) / obs_var)
    # post_mean[is.na(post_mean)] <- 0
    
    #### Compute alpha and adjust distribution to conform to posterior moments
    alp <- sqrt(obs_var / (obs_var + prior_var))
    alps[, tt] <- alp
    
    dy <- lapply(1:n, function(ix) {
      as.numeric(post_mean[ix]) + alp[ix] * (obsprior[ix,, tt] - prior_mean[ix]) - obsprior[ix,, tt]
    })
    dy <- t(matrix(unlist(dy), ncol = n)) # each row is for a single country; columns are ensemble members
    dy.full <- dy; dy <- dy[!is.na(obs_var[1:n]), ] # only keeping for countries that have non-NA obs_var at this point
    
    rr <- NULL
    
    to.adjust <- c(pos.in.vector, pos.in.vector + n ** 2, param.indices,
                   pos.in.vector + n ** 2 * 2 + 4) # don't want to include compartments with no people here
    
    for (j in to.adjust) {
      C <- unlist(lapply((1:n)[!is.na(obs_var[1:n])], function(ix) { # for each country with data at this time point...
        cov(xprior[j,, tt], obsprior[ix,, tt]) / prior_var[ix] # calculate covariance between state/param of interest (j) and obs in this country (ix)
      }))
      rr <- rbind(rr, C)
    }; rr[is.na(rr)] <- 0
    dx <- rr %*% dy # updates for each state/param (rows) for each ensemble member (columns)
    
    ###  Get the new ensemble and save prior and posterior
    xnew <- xprior[,, tt]
    xnew[to.adjust, ] <- xprior[to.adjust,, tt] + dx
    obs_ens <- obs_ens + dy.full
    obs_ens[obs_ens < 0] <- 0
    if (any(obs_ens < 0)) {
      print('Obs_Ens < 0')
    }
    
    # Corrections to data aphysicalities
    xnew[c(S0.indices, I0.indices, param.indices), ] <-
      Fn_checkxnobounds(xnew[c(S0.indices, I0.indices, param.indices), ],
                        S0.indices, I0.indices, param.indices)
    
    # Also correct any newI's < 0:
    ### Where do these zeroes tend to occur? ###
    if (any(xnew[newI.indices + 4, ] < 0)) {
      neg.vals <- which(xnew[newI.indices + 4, ] < 0, arr.ind = TRUE)
      rnd.new <- (do.call('rbind', replicate(n ** 2, ceiling(27 * 1e4 * runif(num_ens)), simplify = FALSE)))
      # xnew[newI.indices + 4, ][neg.vals] <- 0 # Set all to 0
      xnew[newI.indices + 4, ][neg.vals] <- (infects[rnd.new[neg.vals]] / 5 / 100000) * melt(t(N))$value[neg.vals[, 1]]
      # infects/5 are values per 100,000 - so divide by 100,000 to get proportion to multiply by N for that compartment
    }
    if (any(xnew[newI.indices + 4, ] < 0)) {
      print('!!!')
    }
    
    # # Finally, reduce S and I in "empty" compartments to zero
    # to.zero <- (1:(dim(xpost)[1]))[!((1:dim(xpost)[1]) %in% to.adjust)]
    # xnew[to.zero, ] <- 0
    
    xpost[,, tt] <- xnew
    obspost[,, tt] <- obs_ens
    
    #  Integrate forward one time step
    b <- log(xpost[param.indices[3],, tt] - xpost[param.indices[4],, tt])
    a <- -180
    beta <- lapply(1:num_ens, function(ix) {
      (exp(a * AHpt + b[ix]) + xpost[param.indices[4], ix, tt]) /
        xpost[param.indices[2], ix, tt]
    })
    tcurrent <- tm.ini + tmstep * tt
    
    # Draw S0 and I0 from xpost
    # Note: These are already "standardized" by population size, so no longer any need to multiply by N or divide by 100,000, right?
    S0.temp <- lapply(1:num_ens, function(ix) {
      matrix(xpost[S0.indices, ix, tt], ncol = n, byrow = TRUE)
    })
    I0.temp <- lapply(1:num_ens, function(ix) {
      matrix(xpost[I0.indices, ix, tt], ncol = n, byrow = TRUE)
    })
    D.temp <- xpost[param.indices[2], , tt]; L.temp <- xpost[param.indices[1], , tt]
    
    Sr_tmp <- sapply(1:300, function(ix) {
      propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                       S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                       D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                       realdata = TRUE)
    })
    
    Sr_tmp_S <- lapply(1:num_ens, function(ix) {
      as.vector(tail(Sr_tmp[1, ix]$S, 1))
    })
    Sr_tmp_S <- t(matrix(unlist(Sr_tmp_S), ncol = n ** 2, byrow = T))
    
    Sr_tmp_I <- lapply(1:num_ens, function(ix) {
      as.vector(tail(Sr_tmp[2, ix]$I, 1))
    })
    Sr_tmp_I <- t(matrix(unlist(Sr_tmp_I), ncol = n ** 2, byrow = T))
    
    Sr_tmp_newI <- lapply(1:num_ens, function(ix) {
      as.vector(tail(Sr_tmp[3, ix]$newI, 1))
    }) # okay too - total new cases in the 7 days we run this
    Sr_tmp_newI <- t(matrix(unlist(Sr_tmp_newI), ncol = n ** 2, byrow = T))
    
    xprior[S0.indices, , tt + 1] <- Sr_tmp_S
    xprior[I0.indices, , tt + 1] <- Sr_tmp_I
    xprior[newI.indices + 4, , tt + 1] <- Sr_tmp_newI
    xprior[param.indices, , tt + 1] <- xpost[param.indices, , tt]
    
    ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
    obs_ens <- lapply(1:n, function(ix) {
      colSums(Sr_tmp_newI[1:n + n * (ix - 1), ])
    })
    obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F))
    obsprior[,, tt + 1] <- obs_ens
    
    # Plot training progress:
    matplot(obs_i, type = 'b', pch = 4, lty = 2, col = viridis(n), cex = 0.75,
            xlab = 'Weeks from Outbreak Start', ylab = 'Syn+ Counts', main = tt)
    
    obs.prior.toPlot <- t(apply(obsprior[,, 1:(tt + 1)], c(1, 3), mean))
    matlines(obs.prior.toPlot, type = 'b', pch = 20, lty = 1, cex = 0.75, col = viridis(n))
    
  } # end of training
  
  # Print true parameter values:
  # print(c(all.params[[1]][to.keep[ens.index]], all.params[[2]][to.keep[ens.index]],
  #         all.params[[3]][to.keep[ens.index]], all.params[[4]][to.keep[ens.index]]))
  
  # Plot trend of parameters/states/alps over time:
  mean.param.vals <- apply(xpost[param.indices,, ], c(1, 3), mean)
  param.labs <- c('L', 'D', 'R0mx', 'R0mn')
  par(mfrow = c(4, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  for (i in 1:4) {
    plot(mean.param.vals[i, ], type = 'b', pch = 20, xlab = 'Time Since Outbreak Start', ylab = param.labs[i])
    abline(h = true.params[i], lty = 2)
  }
  
  spost <- matrix(0, n, ntrn)
  for (i in 1:n) {
    spost[i, ] <- colSums(apply(xpost[S0.indices[(1:n) + n * (i - 1)],, ], c(1, 3), mean)) / pop.size$pop[i] * 100
  }
  par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  matplot(t(spost), type = 'b', pch = 20, lty = 1, col = viridis(n), xlab = 'Time Since Outbreak Start', ylab = '%S')
  
  par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  matplot(t(alps), type = 'b', pch = 20, lty = 1, col = viridis(n))
  
  # #### Forecast
  # b <- log(xpost[param.indices[3],, ntrn] - xpost[param.indices[4],, ntrn])
  # a <- -180
  # beta <- lapply(1:num_ens, function(ix) {
  #   (exp(a * AHpt + b[ix]) + xpost[param.indices[4], ix, ntrn]) /
  #     xpost[param.indices[2], ix, ntrn]
  # })
  # 
  # tcurrent <- tm.ini + tmstep * ntrn
  # 
  # # Draw S0 and I0 from xpost
  # S0.temp <- lapply(1:num_ens, function(ix) {
  #   N * (matrix(xpost[S0.indices, ix, ntrn], ncol = n, byrow = TRUE) / 100000)
  # })
  # I0.temp <- lapply(1:num_ens, function(ix) {
  #   N * (matrix(xpost[I0.indices, ix, ntrn], ncol = n, byrow = TRUE) / 100000)
  # })
  # D.temp <- xpost[param.indices[2], , ntrn]; L.temp <- xpost[param.indices[1], , ntrn]
  # 
  # Sr_tmp <- sapply(1:300, function(ix) {
  #   propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep * nfc, dt,
  #                    S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
  #                    D = D.temp[ix], L = L.temp[ix], beta[[ix]],
  #                    realdata = TRUE)
  # })
  # 
  # Sr_tmp_S <- lapply(1:length(S0.indices), function(ix) {
  #   matrix(unlist(lapply(1:num_ens, function(jx) {
  #     Sr_tmp[1, jx]$S[tmstep * (1:nfc) + 1, ix]
  #   })), nrow = num_ens, byrow = TRUE)
  # })
  # 
  # Sr_tmp_I <- lapply(1:length(I0.indices), function(ix) {
  #   matrix(unlist(lapply(1:num_ens, function(jx) {
  #     Sr_tmp[2, jx]$I[tmstep * (1:nfc) + 1, ix]
  #   })), nrow = num_ens, byrow = TRUE)
  # })
  # 
  # Sr_tmp_newI <- lapply(1:length(newI.indices), function(ix) {
  #   matrix(unlist(lapply(1:num_ens, function(jx) {
  #     Sr_tmp[3, jx]$newI[tmstep*(1:nfc)+1, ix] - Sr_tmp[3, jx]$newI[tmstep*(0:(nfc-1))+1, ix]
  #   })), nrow = num_ens, byrow = TRUE)
  # })
  # 
  # for (i in 1:length(S0.indices)) {
  #   fcast[S0.indices[i],, ] <- Sr_tmp_S[[i]]
  #   fcast[I0.indices[i],, ] <- Sr_tmp_I[[i]]
  #   fcast[newI.indices[i],, ] <- Sr_tmp_newI[[i]]
  # }
  # 
  # ### Also calculate total newI for each COUNTRY, and call these "obsfcast"
  # obs.fcast <- vector('list', n)
  # for (i in 1:n) {
  #   obs.fcast[[i]] <- Reduce('+', Sr_tmp_newI[1:n + n * (i - 1)])
  # }
  # obsfcast <- array(NA, c(n, num_ens, nfc))
  # for (i in 1:n) {
  #   obsfcast[i,, ] <- obs.fcast[[i]]
  # }
  # rm(obs.fcast)
  # 
  # # Calculate ensemble means
  # xprior_mean = xpost_mean = xsd = matrix(0, n ** 2 * 3 + 4, ntrn)
  # obs_sd <- matrix(0, n, ntrn)
  # for (tt in 1:ntrn) {
  #   xprior_mean[, tt] <- apply(xprior[,, tt], 1, mean, na.rm = TRUE) # why only through ntrn and not ntrn + 1?
  #   xpost_mean[, tt] <- apply(xpost[,, tt], 1, mean, na.rm = TRUE)
  #   xsd[, tt] <- apply(xpost[,, tt], 1, sd, na.rm = TRUE)
  #   obs_sd[, tt] <- apply(obspost[,, tt], 1, sd, na.rm = TRUE)
  # }
  # obsprior_mean = obspost_mean = matrix(0, n, ntrn)
  # for (tt in 1:ntrn) {
  #   obsprior_mean[, tt] <- apply(obsprior[,, tt], 1, mean, na.rm = TRUE)
  #   obspost_mean[, tt] <- apply(obspost[,, tt], 1, mean, na.rm = TRUE)
  # }
  # 
  # if (length(which(is.na(obs_i), arr.ind = T)) > 0) {
  #   obs.na <- which(is.na(obs_i), arr.ind = T)[(which(is.na(obs_i), arr.ind = T) %in% 1:(dim(xpost_mean)[2]))[1:(length(which(is.na(obs_i), arr.ind = T)) / 2)], ]
  #   obs.na <- matrix(obs.na, nrow = length(obs.na) / 2)
  #   if (length(obs.na) > 0) {
  #     for (i in 1:dim(obs.na)[1]) {
  #       obspost_mean[obs.na[i, 2], obs.na[i, 1]] <- NA
  #     }
  #   }
  # }
  # 
  # fcast_mean = fcast_sd = matrix(0, (n ** 2) * 3, nfc)
  # obs_pred_sd <- matrix(0, n, nfc)
  # for (tt in 1:nfc) {
  #   fcast_mean[, tt] <- apply(fcast[,, tt], 1, mean, na.rm = TRUE)
  #   fcast_sd[, tt] <- apply(fcast[,, tt], 1, sd, na.rm = TRUE)
  #   obs_pred_sd[, tt] <- apply(obsfcast[,, tt], 1, sd, na.rm = TRUE)
  # }
  # 
  # #### Reduce to individual country level
  # rows.by.country <- matrix(1:(n ** 2), ncol = n, byrow = TRUE)
  # 
  # xprior_mean_c = xpost_mean_c = fcast_mean_c = obs_pred = xsd_c = fcast_sd_c = NULL
  # for (i in 1:n) {
  #   xprior_mean_c <- rbind(xprior_mean_c, colSums(xprior_mean[S0.indices, ][rows.by.country[i, ], ]))
  #   xpost_mean_c <- rbind(xpost_mean_c, colSums(xpost_mean[S0.indices, ][rows.by.country[i, ], ]))
  #   fcast_mean_c <- rbind(fcast_mean_c, colSums(fcast_mean[S0.indices, ][rows.by.country[i, ], ]))
  #   xsd_c <- rbind(xsd_c, colSums(xsd[S0.indices, ][rows.by.country[i, ], ]))
  #   fcast_sd_c <- rbind(fcast_sd_c, colSums(fcast_sd[S0.indices, ][rows.by.country[i, ], ]))
  # }
  # for (i in 1:n) {
  #   xprior_mean_c <- rbind(xprior_mean_c, colSums(xprior_mean[I0.indices, ][rows.by.country[i, ], ]))
  #   xpost_mean_c <- rbind(xpost_mean_c, colSums(xpost_mean[I0.indices, ][rows.by.country[i, ], ]))
  #   fcast_mean_c <- rbind(fcast_mean_c, colSums(fcast_mean[I0.indices, ][rows.by.country[i, ], ]))
  #   xsd_c <- rbind(xsd_c, colSums(xsd[I0.indices, ][rows.by.country[i, ], ]))
  #   fcast_sd_c <- rbind(fcast_sd_c, colSums(fcast_sd[I0.indices, ][rows.by.country[i, ], ]))
  # }
  # xprior_mean_c <- rbind(xprior_mean_c, xprior_mean[param.indices, ])
  # xpost_mean_c <- rbind(xpost_mean_c, xpost_mean[param.indices, ])
  # xsd_c <- rbind(xsd_c, xsd[param.indices, ])
  # for (i in 1:n) {
  #   xprior_mean_c <- rbind(xprior_mean_c, colSums(xprior_mean[newI.indices + 4, ][rows.by.country[i, ], ]))
  #   xpost_mean_c <- rbind(xpost_mean_c, colSums(xpost_mean[newI.indices + 4, ][rows.by.country[i, ], ]))
  #   fcast_mean_c <- rbind(fcast_mean_c, colSums(fcast_mean[newI.indices, ][rows.by.country[i, ], ]))
  #   obs_pred <- rbind(obs_pred, colSums(fcast_mean[newI.indices, ][rows.by.country[i, ], ]))
  #   xsd_c <- rbind(xsd_c, colSums(xsd[newI.indices + 4, ][rows.by.country[i, ], ]))
  #   fcast_sd_c <- rbind(fcast_sd_c, colSums(fcast_sd[newI.indices, ][rows.by.country[i, ], ]))
  # }
  # 
  # #### Metrics for comparison
  # Y <- cbind(xpost_mean_c[(1:n) + (n * 2) + 4, ], fcast_mean_c[(1:n) + (n * 2), ]) # newI
  # Y.obs <- cbind(obspost_mean, obs_pred)
  # 
  # # only need to keep results from countries that actually had outbreaks
  # any.data <- c()
  # for (ix in 1:n) {
  #   if(!all(is.na(obs_i[, ix]))) {
  #     any.data <- c(any.data, ix)
  #   }
  # }
  # Y <- Y[any.data, ]; Y.obs <- Y.obs[any.data, ]
  # 
  # par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # matplot(obs_i, type = 'b', pch = 20, lty = 2, col = 'black',
  #         ylim=c(0, min(max(Y.obs, na.rm = T) + 100, 1e10)), main = ntrn)
  # abline(v = ntrn, lwd = 3, lty = 2, col = 'gray80')
  # matlines(t(Y.obs), lty = 1, col = viridis(n), lwd = 2)
  # 
  # par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # matplot(t(xpost_mean_c[(1:n)[any.data] ,]), type = 'b', pch = 20, lty = 2, col = viridis(n),
  #         xlab = 'Time (Weeks)', ylab = '# Susceptible')
  # Re.mat <- t(matrix(1, nrow = n, ncol = 1) %*% t(xpost_mean[param.indices[4] ,])) +
  #   t(matrix(1, nrow = n, ncol = 1) %*%
  #       t(xpost_mean[param.indices[3] ,] - xpost_mean[param.indices[4] ,])) *
  #   exp(a * AHpt[seq(1, ntrn * 7, by = 7), ])
  # matplot(Re.mat, type = 'b', pch = 20, lty = 2, col = viridis(n),
  #         xlab = 'Time (Weeks)', ylab = 'R_eff')
  # abline(h = 1.0, col = 'black', lwd = 2, lty = 2)
  # matplot(t(alps), type = 'b', pch = 20, lty = 2, col = viridis(n), ylim = c(0, 1),
  #         xlab = 'Time (Weeks)', ylab = 'alp')
  # 
  # Y.obs <- cbind(obspost_mean, obs_pred)
  # par(mfrow = c(6, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # for (i in any.data) {
  #   plot(obs_i[, i], type = 'b', pch = 20, lty = 2, col = 'black', main = countries[i])
  #   lines(t(Y.obs)[, i], lwd = 3, col = 'plum')
  # }
  # 
  # params.post <- t(xpost_mean[param.indices, ])
  # par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # plot(params.post[, 1], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'L')
  # abline(h = parms[1], lty = 2, col = 'red')
  # plot(params.post[, 2], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'D')
  # abline(h = parms[2], lty = 2, col = 'red')
  # plot(params.post[, 3], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0max')
  # abline(h = parms[3], lty = 2, col = 'red')
  # plot(params.post[, 4], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0min')
  # abline(h = parms[4], lty = 2, col = 'red')
  # 
  # params.var <- t(xsd[param.indices, ])
  # plot(params.var[, 1], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'L')
  # plot(params.var[, 2], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'D')
  # plot(params.var[, 3], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0max')
  # plot(params.var[, 4], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0min')
  # 
  # #### Metrics for comparison
  # Y <- cbind(obspost_mean, obs_pred)
  # 
  # # Peak Timing #
  # pkwks <- matrix(0, length(any.data), num_ens)
  # obs_pkwk <- c()
  # for (i in 1:length(any.data)) {
  #   obs_pkwk <- c(obs_pkwk, which.max(obs_i[, i]))
  #   
  #   for (j in 1:num_ens){
  #     pkwks[i, j] <- which.max(c(obspost[i, j, 1:ntrn], obsfcast[i, j, 1:nfc]))
  #   }
  # }
  # 
  # pkwk_mean = pkwk_mode = pkwk_mode_perc = pkwk_var = c()
  # for (i in 1:length(any.data)) {
  #   pkwk_mean <- c(pkwk_mean, which.max(Y[i, ]))
  #   pkwk_mode <- c(pkwk_mode, MODE(pkwks[i, ])[1])
  #   pkwk_mode_perc <- c(pkwk_mode_perc, MODE(pkwks[i, ])[2] / num_ens)
  #   pkwk_var <- c(pkwk_var, var(pkwks[i, ]))
  # }
  # 
  # obs_pkwk_1 <- obs_pkwk + wk_start - 1
  # 
  # leadpkwk_mean <- pkwk_mean - ntrn
  # leadpkwk_mode <- pkwk_mode - ntrn
  # 
  # delta_pkwk_mean <- pkwk_mean - obs_pkwk
  # delta_pkwk_mode <- pkwk_mode - obs_pkwk
  # 
  # # Peak Intensity #
  # obs_peak_int = peak_intensity = c()
  # ili_peaks <- matrix(0, length(any.data), num_ens)
  # for (i in 1:length(any.data)) {
  #   obs_peak_int <- c(obs_peak_int, max(obs_i[, i], na.rm = T))
  #   peak_intensity <- c(peak_intensity, max(Y[i, ], na.rm = T))
  #   
  #   for (j in 1:num_ens) {
  #     ili_peaks[i, j] <- max(c(obspost[i, j, ], obsfcast[i, j, ]), na.rm = T)
  #   }
  # }
  # intensity_err <- peak_intensity - obs_peak_int
  # 
  # peak_intensity_var <- c()
  # for (i in 1:length(any.data)) {
  #   peak_intensity_var <- c(peak_intensity_var, var(ili_peaks[i, ]))
  # }
  # 
  # # Correlation/RMSE #
  # corr = rms = corr.fcast = rms.fcast = c()
  # for (i in 1:length(any.data)) {
  #   corr <- c(corr, cor(Y[i, ], head(obs_i[, i], nsn), use = 'pairwise.complete.obs'))
  #   corr.fcast <- c(corr.fcast, cor(obs_pred[i, ], obs_i[(ntrn + 1):nsn, i],
  #                                   use = 'pairwise.complete.obs'))
  #   
  #   rms <- c(rms, sqrt(mean((Y[i, ] - head(obs_i[, i], nsn)) ** 2, na.rm = TRUE)))
  #   rms.fcast <- c(rms.fcast, sqrt(mean((obs_pred[i, ] - obs_i[(ntrn + 1):nsn, i]) ** 2,
  #                                       na.rm = TRUE)))
  # }
  # 
  # obs_adj <- obs_i[(ntrn + 1):nsn, ]
  # obs_adj[obs_adj == 0 & !is.na(obs_adj)] <- 1
  # mape = wape = smape = c()
  # for (i in 1:length(any.data)) {
  #   mape <- c(mape, sum(abs((obs_adj[, i] - obs_pred[i, ]) / obs_adj[, i]), na.rm = T) *
  #               100 / length((ntrn + 1):nsn))
  #   wape <- c(wape, (sum(abs(obs_i[(ntrn + 1):nsn, i] - obs_pred[i, ]), na.rm = T) /
  #                      sum(obs_i[(ntrn + 1):nsn, i], na.rm = T)) *
  #               100 / length((ntrn + 1):nsn))
  #   smape <- c(smape, sum(abs(obs_pred[i, ] - obs_i[(ntrn + 1):nsn, i]) /
  #                           ((obs_pred[i, ] + obs_i[(ntrn + 1):nsn, i]) / 2), na.rm = T) *
  #                100 / length((ntrn + 1):nsn))
  # }
  # wape[wape == Inf] <- NA
  # 
  # # Attack Rate #
  # totAttackObs = tot_attack = c()
  # for (i in 1:length(any.data)) {
  #   totAttackObs <- c(totAttackObs, sum(head(obs_i[, i], nsn), na.rm = T))
  #   tot_attack <- c(tot_attack, sum(Y[i, ][!is.na(obs_i[, i])], na.rm = T))
  # }
  # delta_sum_newI <- tot_attack - totAttackObs
  # 
  # # Next 4 Weeks #
  # if (dim(obs_pred)[2] > 0 & any(!is.na(obs_i[ntrn + 1, ]))) {
  #   deno_obs_i <- unlist(ifelse(obs_i[ntrn + 1, ] == 0, 1, obs_i[ntrn + 1, ]))
  #   rdiff_next_newI <- (obs_pred[, 1] - deno_obs_i) / deno_obs_i
  # } else {
  #   rdiff_next_newI <- rep(NA, length(any.data))
  # }
  # 
  # if (dim(obs_pred)[2] > 1 & any(!is.na(obs_i[ntrn + 2, ]))) {
  #   deno_obs_i <- unlist(ifelse(obs_i[ntrn + 2, ] == 0, 1, obs_i[ntrn + 2, ]))
  #   rdiff_next_newI2 <- (obs_pred[, 2] - deno_obs_i) / deno_obs_i
  # } else {
  #   rdiff_next_newI2 <- rep(NA, length(any.data))
  # }
  # 
  # if (dim(obs_pred)[2] > 2 & any(!is.na(obs_i[ntrn + 3, ]))) {
  #   deno_obs_i <- unlist(ifelse(obs_i[ntrn + 3, ] == 0, 1, obs_i[ntrn + 3, ]))
  #   rdiff_next_newI3 <- (obs_pred[, 3] - deno_obs_i) / deno_obs_i
  # } else {
  #   rdiff_next_newI3 <- rep(NA, length(any.data))
  # }
  # 
  # if (dim(obs_pred)[2] > 3 & any(!is.na(obs_i[ntrn + 4, ]))) {
  #   deno_obs_i <- unlist(ifelse(obs_i[ntrn + 4, ] == 0, 1, obs_i[ntrn + 4, ]))
  #   rdiff_next_newI4 <- (obs_pred[, 4] - deno_obs_i) / deno_obs_i
  # } else {
  #   rdiff_next_newI4 <- rep(NA, length(any.data))
  # }
  # 
  # # Onset #
  # onsetObs = onset = c()
  # for (i in 1:length(any.data)) {
  #   onsetObs <- c(onsetObs, findOnset(obs_i[, i], 500)$onset)
  #   onset <- c(onset, findOnset(Y[, i], 500)$onset)
  # }
  # 
  # onsets = matrix(0, length(any.data), num_ens)
  # for (i in 1:length(any.data)) {
  #   for (j in 1:num_ens) {
  #     yy <- c(obspost[i, j, 1:ntrn], obsfcast[i, j, 1:nfc])
  #     onsets[i, j] <- findOnset(yy, 500)$onset
  #   }
  # }
  # onset_var <- c()
  # for (i in 1:length(any.data)) {
  #   onset_var <- c(onset_var, var(onsets[i, ], na.rm = T))
  # }
  # 
  # #### OUTPUT DATA
  # tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep)
  # fc_start <- ntrn + wk_start - 1
  # 
  # out1 = out2 = out4 = out5 = out6 = NULL
  # 
  # for (i in 1:length(any.data)) {
  #   xpost_mean_c.temp <- xpost_mean_c[c(any.data[i], any.data[i] + n), (1:ntrn)[!is.na(obs_i[1:ntrn, i])]]
  #   xsd_c.temp <- xsd_c[c(any.data[i], any.data[i] + n), (1:ntrn)[!is.na(obs_i[1:ntrn, i])]]
  #   
  #   fcast_mean_c.temp <- fcast_mean_c[c(any.data[i], any.data[i] + n), ]
  #   fcast_sd_c.temp <- fcast_sd_c[c(any.data[i], any.data[i] + n), ]
  #   
  #   out1.temp <- cbind(rep(fc_start, length((1:ntrn)[!is.na(obs_i[1:ntrn, i])])),
  #                      tstep[(1:ntrn)[!is.na(obs_i[1:ntrn, i])]],
  #                      (1:ntrn)[!is.na(obs_i[1:ntrn, i])] + 39,
  #                      t(obspost_mean[i, ])[(1:ntrn)[!is.na(obs_i[1:ntrn, i])]],
  #                      t(obs_sd[i, ])[(1:ntrn)[!is.na(obs_i[1:ntrn, i])]],
  #                      t(xpost_mean_c.temp), t(xsd_c.temp),
  #                      t(xpost_mean_c[(n * 2 + 1):(n * 2 + 4), (1:ntrn)[!is.na(obs_i[1:ntrn, i])]]),
  #                      t(xsd_c[(n * 2 + 1):(n * 2 + 4), (1:ntrn)[!is.na(obs_i[1:ntrn, i])]]))
  #   out2.temp <- cbind(rep(fc_start, nfc), tstep[(ntrn + 1):nsn], ((ntrn + 1):nsn) + 39,
  #                      obs_pred[i, ], obs_pred_sd[i, ], t(fcast_mean_c.temp),
  #                      t(fcast_sd_c.temp))
  #   out4.temp <- cbind(rep(fc_start, ntrn), tstep[1:ntrn], obsprior_mean[i, ],
  #                      t(xprior_mean_c[c(any.data[i], any.data[i] + n, (n * 2 + 1):(n * 2 + 4)), ]))
  #   
  #   out1.temp <- cbind(rep(countries[any.data][i], length((1:ntrn)[!is.na(obs_i[1:ntrn, i])])),
  #                      out1.temp)
  #   out2.temp <- cbind(rep(countries[any.data][i], nfc), out2.temp)
  #   out4.temp <- cbind(rep(countries[any.data][i], ntrn), out4.temp)
  #   
  #   out1 <- rbind(out1, out1.temp); out2 <- rbind(out2, out2.temp); out4 <- rbind(out4, out4.temp)
  # }
  # colnames(out1) <- c('country', 'fc_start', 'time', 'week', 'Est', 'Est_sd', 'S', 'I', 'S_sd', 'I_sd',
  #                     'L', 'D', 'R0max', 'R0min', 'L_sd', 'D_sd', 'R0max_sd', 'R0min_sd')
  # colnames(out2) <- c('country', 'fc_start', 'time', 'week', 'Est', 'Est_sd', 'S', 'I', 'S_sd', 'I_sd')
  # colnames(out4) <- c('country', 'fc_start', 'time', 'Est', 'S', 'I', 'L', 'D', 'R0max', 'R0min')
  # 
  # out3 <- cbind(countries[any.data], rep(fc_start, length(any.data)), obs_pkwk_1, pkwk_mode + wk_start - 1,
  #               delta_pkwk_mode, pkwk_mean + wk_start - 1, delta_pkwk_mean, leadpkwk_mode, leadpkwk_mean,
  #               sqrt(pkwk_var), obs_peak_int, peak_intensity, intensity_err, sqrt(peak_intensity_var),
  #               totAttackObs, tot_attack, delta_sum_newI, matrix(obs_i[ntrn + 1, ]), matrix(obs_i[ntrn + 2, ]),
  #               matrix(obs_i[ntrn + 3, ]), matrix(obs_i[ntrn + 4, ]), obs_pred[, 1], obs_pred[, 2],
  #               obs_pred[, 3], obs_pred[, 4], rdiff_next_newI, rdiff_next_newI2, rdiff_next_newI3,
  #               rdiff_next_newI4, rms, corr, rms.fcast, corr.fcast, mape, wape, smape, onset, sqrt(onset_var),
  #               onsetObs)
  # colnames(out3) <- c('country', 'fc_start','obs_pkwk','pkwk_mode','delta_pkwk_mode','pkwk_mean',
  #                     'delta_pkwk_mean','leadpkwk_mode','leadpkwk_mean','pkwk_sd','obs_peak_int',
  #                     'peak_intensity','intensity_err','peak_intensity_sd','totAttackObs',
  #                     'tot_attack','delta_AR','obs_1week','obs_2week','obs_3week','obs_4week',
  #                     'fcast_1week','fcast_2week','fcast_3week','fcast_4week','delta_1w',
  #                     'delta_2w','delta_3w','delta_4w','rms','corr','rms_fcast','corr_fcast',
  #                     'mape','wape','smape','onset','onset_sd','onsetObs')
  # 
  # if(metricsonly==F){
  #   out=list(train=out1,fcast=out2,metrics=out3,trainprior=out4);
  # }else{
  #   out=list(metrics=out3);
  # }
  
}





