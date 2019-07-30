
EAKF_rFC <- function(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn = 1, obs_vars,
                     tm.ini = 273, tm.range = 273:500, updates = FALSE, do.reprobing = FALSE){
  
  if (!exists('updates')) {
    updates <- FALSE
  }
  if (!exists('do.reprobing')) {
    do.reprobing <- FALSE
  }
  
  to.check <- c(7, 10, 18, 19, 20) # difficult to look at 21 countries at once - assess visually for these 5
  
  num_times <- floor(length(tm.range) / tmstep)
  nfc <- nsn - ntrn # number of weeks for forecasting
  
  theta_low <- param.bound[, 1]; theta_up <- param.bound[, 2]
  
  ##########################################################################################
  # Determine which compartments need to be monitored
  # Others are always zero
  pos.comp <- which(t.comm > 0, arr.ind = TRUE)
  pos.in.vector <- c()
  for (i in 1:n) {
    dests <- as.vector(pos.comp[, 2][pos.comp[, 1] == i])
    dests <- sort(c(dests, i))
    pos.in.vector <- c(pos.in.vector, dests + n * (i - 1))
  }; rm(dests)
  pos.in.vector <- sort(pos.in.vector)
  ##########################################################################################
  
  # So <- matrix(0, n ** 2 * 2 + 5, num_ens) # last 5 rows are parameters
  xprior <- array(0, c(n ** 2 * 3 + 5, num_ens, ntrn + 1)) # add newI for each compartment, too
  xpost <- array(0, c(n ** 2 * 3 + 5, num_ens, ntrn))
  # fcast <- array(0, c(n ** 2 * 3, num_ens, nfc)) # fcast: S, I, newI
  
  obsprior <- array(NA, c(n, num_ens, ntrn + 1))
  obspost <- array(NA, c(n, num_ens, ntrn))
  
  ### Where each state/param is stored:
  S0.indices <- 1:(n ** 2) # where S0 are stored
  I0.indices <- S0.indices + S0.indices[length(S0.indices)] # where I0 are stored
  newI.indices <- S0.indices + S0.indices[length(S0.indices)] * 2 # where newI are stored (in fcast) (only individual compartments)
  param.indices <- (max(newI.indices) + 1):(max(newI.indices) + 5) # where the epi parameters are stored
  
  ### Set initial conditions based on input parameters
  param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
                       c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))
  parms <- t(lhs(num_ens, param.bound))
  
  S0.temp = I0.temp = vector('list', num_ens)
  for (i in 1:num_ens) {
    S0.temp[[i]] <- matrix(parms[1:(n ** 2), i], nrow = n, ncol = n, byrow = T) * N
    I0.temp[[i]] <- matrix(parms[1:(n ** 2) + (n ** 2), i], nrow = n, ncol = n, byrow = T) * N
  }
  parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
  
  ### Calculate the reproductive number at time t BT1 and the transmission rate
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- AH[beta.range, ]
  AHpt <- as.matrix(AHpt, length(AHpt), n)
  b <- log(parms[3, ] - parms[4, ])
  a <- -180
  
  beta <- lapply(1:num_ens, function(ix) {
    (exp(a * AHpt + b[ix]) + parms[4, ix]) / parms[2, ix]
  })
  tcurrent <- tm.ini
  
  ### Create vectors of initial parameters:
  D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[5, ]
  
  # integrate 1 step forward
  Sr_tmp <- sapply(1:num_ens, function(ix) {
    propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     airScale = airScale.temp[ix], realdata = TRUE)
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
  xprior[newI.indices, , 1] <- Sr_tmp_newI
  xprior[param.indices, , 1] <- parms
  # Since these are values for individual compartments, and not countries overall, I know I need all of them
  
  ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
  obs_ens <- lapply(1:n, function(ix) {
    colSums(Sr_tmp_newI[1:n + n * (ix - 1), ]) # adding up all new infecteds LIVING in each country
  })
  obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F)) # each row is a single country
  
  # Convert to rate per 100,000
  for (i in 1:n) {
    obs_ens[i, ] <- obs_ens[i, ] / pop.size$pop[i] * 100000
  }
  
  obsprior[,, 1] <- obs_ens # standardized per 100,000
  
  alps <- matrix(NA, nrow = n, ncol = ntrn) # record ratios of obs_var to obs+prior var
  
  ### Begin looping through observations
  ### Training process
  to.adjust <- c(pos.in.vector, pos.in.vector + n ** 2, pos.in.vector + n ** 2 * 2, param.indices)
  
  for (tt in 1:ntrn) {
    # Update state variables and parameters, then integrate forward
    print(tt)
    
    if (updates) {
      print('Inflating variances...')
    }
    
    # inflate all states and parameters
    inflat <- diag(x = rep(lambda, n ** 2 * 3 + 5), n ** 2 * 3 + 5, n ** 2 * 3 + 5)
    inflat.obs <- diag(x = rep(lambda, n), n, n)
    xmn <- rowMeans(xprior[,, tt]); obs_ens.mn <- rowMeans(obs_ens)
    x <- inflat %*% (xprior[,, tt] - (xmn %*% matrix(1, 1, num_ens))) + (xmn %*% matrix(1, 1, num_ens))
    obs_ens <- inflat.obs %*% (obsprior[,, tt] - (obs_ens.mn %*% matrix(1, 1, num_ens))) + (obs_ens.mn %*% matrix(1, 1, num_ens))
    # so we subtract the mean from the prior, inflate, then add the mean back in?
    # QUESTION: Any xprior < 0?
    
    # CHECK: No obsprior should be <0, right??
    not.to.adjust <- (1:dim(xprior)[1])[!((1:dim(xprior)[1]) %in% to.adjust)]
    if (any(xprior[not.to.adjust,, tt] != 0)) {
      print('Empty compartment(s) w/ > 0!')
    }
    
    ### FIX 1: Don't allow obsprior to be <0 - set to 0?
    x[which(x < 0, arr.ind = T)] <- 0
    obs_ens[which(obs_ens < 0, arr.ind = T)] <- 0
    xprior[,, tt] <- x
    obsprior[,, tt] <- obs_ens
    
    if (updates) {
      print('Looping through observations...')
    }
    
    # Loop through observations:
    for (loc in 1:n) { # for (loc in n:1) { # to test for sensitivity to loop order
      
      # Check that data point is not NA, and obs_var not 0:
      obs_var <- obs_vars[tt, loc]
      
      if (!is.na(obs_i[tt, loc]) & !is.na(obs_var) & obs_var > 0) { # otherwise, don't fit with this point
        prior_var <- var(obs_ens[loc, ])
        post_var <- prior_var * (obs_var / (prior_var + obs_var))
        
        if (prior_var == 0) {
          post_var <- 0
          prior_var <- 1e-3
        }
        
        prior_mean <- mean(obs_ens[loc, ])
        post_mean <- post_var * (prior_mean / prior_var + obs_i[tt, loc] / obs_var)
        
        # Compute alpha and adjust distribution to conform to posterior moments:
        alp <- sqrt(obs_var / (obs_var + prior_var))
        alps[loc, tt] <- alp
        if (updates) {
          print(paste0(countries[loc], ':  ', round(alp, 3)))
        }
        # print(paste0(countries[loc], ':  ', round(alp, 3)))#, '  ', obs_var / prior_var))
        
        dy <- post_mean + alp * (obs_ens[loc, ] - prior_mean) - obs_ens[loc, ] # no NAs, since this is still in the if-loop
        
        # Get covariance of the prior state space and the observations, and loop over each state variable:
        rr <- NULL
        for (j in 1:dim(x)[1]) { # so here, we're not doing only "to.adjust" - QUESTION
          C <- cov(x[j, ], obs_ens[loc, ]) / prior_var # this will be 0 for empty compartments
          rr <- append(rr, C)
        }
        dx <- rr %*% t(dy)
        
        # Get adjusted ensemble and obs_ens:
        x <- x + dx # QUESTION: then using the updated x and obs_ens to update further - isn't this a little not genuine?
        obs_ens[loc, ] <- obs_ens[loc, ] + dy
        
        # if (any(obs_ens < 0)) {
        #   print(countries[loc])
        # }
        
        x[which(x < 0, arr.ind = TRUE)] <- 0 # try - set this to 1.0 instead of 0, like in Fn_checkxnobounds?
        x <- Fn_checkxnobounds(x, S0.indices, I0.indices, param.indices) # this alone sets the "empty" compartments to 1.0; also, nothing to check newI? (okay, b/c makes sure don't go below 0)
        obs_ens[loc, obs_ens[loc, ] < 0] <- 0 # so we do ensure these aren't wild as we go, though
        
      }
      # CHECK: Any points where all obs_var either NA or 0?
      
    }
    
    xnew <- x
    
    if (updates) {
      print(rowMeans(xprior[param.indices,, tt]))
      print(rowMeans(xnew[param.indices, ]))
      
      print(rowMeans(obs_ens[to.check, ]))
      print(obs_i[tt, to.check])
    }
    
    # Store posteriors:
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
    D.temp <- xpost[param.indices[2], , tt]; L.temp <- xpost[param.indices[1], , tt];
    airScale.temp <- xpost[param.indices[5],, tt]
    
    Sr_tmp <- sapply(1:300, function(ix) {
      propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                       S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                       D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                       airScale = airScale.temp[ix], realdata = TRUE)
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
    xprior[newI.indices, , tt + 1] <- Sr_tmp_newI
    xprior[param.indices, , tt + 1] <- xpost[param.indices, , tt]
    
    ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
    obs_ens <- lapply(1:n, function(ix) {
      colSums(Sr_tmp_newI[1:n + n * (ix - 1), ])
    })
    obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F))
    for (i in 1:n) {
      obs_ens[i, ] <- obs_ens[i, ] / pop.size$pop[i] * 100000
    }
    obsprior[,, tt + 1] <- obs_ens
    
    # for (i in 1:n) {
    #   print(all.equal(colSums(xprior[newI.indices,, tt + 1][(1:n) + (i - 1) * n, ]), obs_ens[i, ]))
    # }
    
    # Plot training progress:
    if (tt > 1) {
      # par(mfrow = c(4, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
      obs.red.toPlot <- obs_i[, to.check] # DE, IT, ES, SE, UK
      obs.post.toPlot <- t(apply(obspost[,, 1:tt], c(1, 3), mean))
      matplot(obs.red.toPlot, type = 'b', pch = 4, lty = 2,
              col = c("#FF0000FF", "#0000FFFF", "#00FF00FF", "#00FFFFFF", "#FF00FFFF"), cex = 0.75,
              xlab = 'Weeks from Outbreak Start', ylab = 'Syn+ Counts', main = tt)
      matlines(obs.post.toPlot[, to.check], type = 'b', pch = 20, lty = 1, cex = 0.8,
               col = c("#FF0000FF", "#0000FFFF", "#00FF00FF", "#00FFFFFF", "#FF00FFFF"))
      
      # print(obs.post.toPlot[, to.check])
      # print(obs_i[1:tt, to.check])
      # Failure might occur sometimes b/c model cases go to infinity
      
      if (tt == ntrn) {
        par(mfrow = c(4, 5), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
        for (i in (1:n)[!is.na(obs_i[tt, ])]) {
          obs.post.toPlot.ind <- colMeans(obspost[i,, 1:tt])
          plot(obs_i[, i], type = 'b', pch = 4, lty = 2, col = 'gray40', cex = 0.75,
               xlab = 'Wks from Start', ylab = 'Syn+ Counts', main = countries[i], 
               ylim = c(0, max(max(obs_i[, i], na.rm = T), max(obs.post.toPlot.ind, na.rm = T))))
          lines(obs.post.toPlot.ind, type = 'b', pch = 20, lty = 1, cex = 0.8, col = 'coral')
        }
      }
    }
    
  } # end of training
  
  # For now, we just want to return: PT, PI, corr, rmse, newI, S, and params!
  
  # Get parameters (and parameter sd) over time:
  params.post <- xpost[param.indices,, 1:tt]
  params.post_mean <- t(apply(params.post, c(1, 3), mean))
  params.post_sd <- t(apply(params.post, c(1, 3), sd))
  # QUESTION: SD continues to increase near end of outbreak - okay?
  
  params.post_df <- as.data.frame(cbind(params.post_mean, params.post_sd))
  names(params.post_df) <- c('L', 'D', 'R0mx', 'R0mn', 'airScale', 'L_sd', 'D_sd', 'R0mx_sd', 'R0mn_sd', 'airScale_sd')
  
  # Get newI over time:
  obspost_mean <- t(apply(obspost[,, 1:tt], c(1, 3), mean))
  obspost_mean <- as.data.frame(obspost_mean)
  names(obspost_mean) <- countries
  
  # Also get sd of "observations":
  obspost_sd <- t(apply(obspost[,, 1:tt], c(1, 3), sd))
  obspost_sd <- as.data.frame(obspost_sd)
  names(obspost_mean) <- countries
  
  # Calculate country-level S and I over time:
  s.post <- xpost[S0.indices,, ]
  i.post <- xpost[I0.indices,, ]
  
  s.post.by.count <- array(0, c(n, num_ens, ntrn)) # last dimension should eventually be nsn instead!
  i.post.by.count <- array(0, c(n, num_ens, ntrn)) # last dimension should eventually be nsn instead!
  for (i in 1:n) {
    country.vals <- (1:n) + n * (i - 1)
    s.post.temp <- s.post[country.vals,, ]
    i.post.temp <- i.post[country.vals,, ]
    
    for (j in 1:num_ens) {
      s.post.by.count[i, j, ] <- colSums(s.post.temp[, j, ])
      i.post.by.count[i, j, ] <- colSums(i.post.temp[, j, ])
    }
    
  }
  
  # Get S/I and respective sd over time:
  s.post_mean <- t(apply(s.post.by.count, c(1, 3), mean))
  s.post_sd <- t(apply(s.post.by.count, c(1, 3), sd))
  i.post_mean <- t(apply(i.post.by.count, c(1, 3), mean))
  i.post_sd <- t(apply(i.post.by.count, c(1, 3), sd))
  
  for (i in 1:n) {
    s.post_mean[, i] <- s.post_mean[, i] / rowSums(N)[i]
    s.post_sd[, i] <- s.post_sd[, i] / rowSums(N)[i]
    i.post_mean[, i] <- i.post_mean[, i] / rowSums(N)[i]
    i.post_sd[, i] <- i.post_sd[, i] / rowSums(N)[i]
  }
  
  s.post_mean <- as.data.frame(s.post_mean); names(s.post_mean) <- countries
  s.post_sd <- as.data.frame(s.post_sd); names(s.post_sd) <- countries
  i.post_mean <- as.data.frame(i.post_mean); names(i.post_mean) <- countries
  i.post_sd <- as.data.frame(i.post_sd); names(i.post_sd) <- countries
  
  oStates <- as.data.frame(cbind(melt(t(s.post_mean)), melt(t(s.post_sd)), melt(t(i.post_mean)), melt(t(i.post_sd))))
  oStates <- oStates[, c(1:3, 6, 9, 12)]
  names(oStates) <- c('country', 'week', 'S', 'S_sd', 'I', 'I_sd')
  oStates$week <- oStates$week + wk_start - 1
  
  # Calculate accuracy metrics:
  pt = pt.obs = pi = pi.obs = ot = ot.obs = corrs = rmses = c()
  for (i in 1:n) {
    obs_temp <- obs_i[1:tt, i]
    pred_temp <- obspost_mean[, i]
    
    if (!all(is.na(obs_temp))) {
      ot.obs <- c(ot.obs, findOnset(obs_temp, 500)$onset)
      ot <- c(ot, findOnset(pred_temp, 500)$onset)
      
      pt <- c(pt, which(pred_temp == max(pred_temp, na.rm = TRUE)))
      pt.obs <- c(pt.obs, which(obs_temp == max(obs_temp, na.rm = TRUE)))
      
      pi <- c(pi, max(pred_temp, na.rm = TRUE))
      pi.obs <- c(pi.obs, max(obs_temp, na.rm = TRUE))
      
      corrs <- c(corrs, cor(obs_temp, pred_temp, use = 'pairwise.complete.obs'))
      rmses <- c(rmses, sqrt(mean((obs_temp - pred_temp) ** 2, na.rm = TRUE)))
      
    } else {
      pt <- c(pt, NA); pt.obs <- c(pt.obs, NA)
      pi <- c(pi, NA); pi.obs <- c(pi.obs, NA)
      ot <- c(ot, NA); ot.obs <- c(ot.obs, NA)
      corrs <- c(corrs, NA); rmses <- c(rmses, NA)
    }
  }
  
  m <- as.data.frame(cbind(countries, pt, pt.obs, pt - pt.obs, pi, pi.obs, pi - pi.obs, ot, ot.obs, ot - ot.obs, corrs, rmses))
  m$pt.acc = m$pi.acc = NA
  for (i in 1:n) {
    if (!is.na(pt[i])) {
      if (pt[i] %in% c(pt.obs[i] - 1, pt.obs[i], pt.obs[i] + 1)) {
        m$pt.acc[i] <- 'y'
      } else {
        m$pt.acc[i] <- 'n'
      }
      
      if (pi[i] >= (0.875 * pi.obs[i]) & pi[i] <= (1.125 * pi.obs[i])) {
        m$pi.acc[i] <- 'y'
      } else {
        m$pi.acc[i] <- 'n'
      }
    } else {
      m$pt.acc[i] <- NA
      m$pi.acc[i] <- NA
    }
  }
  m$pt.acc <- factor(m$pt.acc); m$pi.acc <- factor(m$pi.acc)
  
  # Melt alps to data frame to return:
  rownames(alps) <- countries
  alps <- as.data.frame(melt(alps))
  names(alps) <- c('country', 'week', 'value')
  
  # Return results:
  res.list <- list(m, params.post_df, oStates, alps)
  
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
  # print('Forecast completed!')
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
  
  # #### Metrics for comparison
  # Y <- cbind(xpost_mean_c[(1:n) + (n * 2) + 4, ], fcast_mean_c[(1:n) + (n * 2), ]) # newI
  # Y.obs <- cbind(obspost_mean, obs_pred)
  # # use obspost_mean and obspred
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
  # par(mfrow = c(2, 3), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # for (i in any.data) {
  #   plot(obs_i[, i], type = 'b', pch = 20, lty = 2, col = 'black', main = countries[i])
  #   lines(t(Y.obs)[, i], lwd = 3, col = 'plum')
  # }
  # 
  # params.post <- t(xpost_mean[param.indices, ])
  # par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # plot(params.post[, 1], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'L')
  # plot(params.post[, 2], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'D')
  # plot(params.post[, 3], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0max')
  # plot(params.post[, 4], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0min')
  # 
  # params.var <- t(xsd[param.indices, ])
  # plot(params.var[, 1], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'L')
  # plot(params.var[, 2], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'D')
  # plot(params.var[, 3], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0max')
  # plot(params.var[, 4], type = 'b', pch = 20, xlab = 'Training Time', ylab = 'R0min')
  # 
  # #### Metrics for comparison
  # # For this section, just use the scaled data/model outputs; everything can be
  # # unscaled later
  # # For this reason, also include scalings in outputs
  # 
  # Y <- cbind(obspost_mean, obs_pred)
  # Y <- Y[any.data, ]
  # 
  # # Reduce results to countries where there are data
  # obs_i <- obs_i[, any.data]
  # obspost <- obspost[any.data,, ]
  # obsfcast <- obsfcast[any.data,, ]
  # obs_pred <- Y[, (ntrn + 1):nsn]#obs_pred[any.data, ]
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
  # # note that we're assuming that leading/lagging NAs are actually 0
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
  # # Use MEM values, but remember to scale them appropriately
  # 
  # if (scale.type == 'new') {
  #   load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/data/onset_vals_RAW.RData')
  #   for (i in 1:n) {
  #     if (i == 6) {
  #       onset.vals[[i]][1] <- onset.vals[[i]][1] * scalings.new[[i]][1]
  #       onset.vals[[i]][2] <- onset.vals[[i]][2] * scalings.new[[i]][2]
  #     } else {
  #       onset.vals[[i]] <- onset.vals[[i]] * scalings.new[[i]]
  #     }
  #   }
  #   onset.vals <- onset.vals[any.data]
  # } else {
  #   onset.vals <- vector('list', n)
  #   for (i in 1:n) {
  #     onset.vals[i] <- 500
  #   }
  # }
  # 
  # onsetObs <- c()
  # for (i in 1:length(any.data)) {
  #   on.temp <- onset.vals[[i]]
  #   if (length(on.temp) > 1) {
  #     # France
  #     if (season %in% c('2014-15', '2015-16', '2016-17', '2017-18')) {
  #       on.temp <- on.temp[2]
  #     } else {
  #       on.temp <- on.temp[1]
  #     }
  #   }
  #   onsetObs <- c(onsetObs, findOnset(obs_i[, i], on.temp)$onset)
  # }
  # 
  # # Distributions #
  # # Onset:
  # onsets = matrix(0, length(any.data), num_ens)
  # # peakWeeks = peakIntensities = matrix(NA, length(any.data), num_ens) # already have these!
  # nextILI = array(NA, c(length(any.data), num_ens, 4))
  # 
  # pkwks <- pkwks + wk_start - 1
  # 
  # for (i in 1:length(any.data)) {
  #   for (j in 1:num_ens) {
  #     yy <- c(obspost[i, j, 1:ntrn], obsfcast[i, j, 1:nfc])
  #     on.temp <- onset.vals[[i]]
  #     if (length(on.temp) > 1) {
  #       # France
  #       if (season %in% c('2014-15', '2015-16', '2016-17', '2017-18')) {
  #         on.temp <- on.temp[2]
  #       } else {
  #         on.temp <- on.temp[1]
  #       }
  #     }
  #     onsets[i, j] <- findOnset(yy, on.temp)$onset
  #     for (k in 1:min(nfc, 4)) {
  #       nextILI[i, j, k] <- obsfcast[i, j, k]
  #     }
  #   }
  # }
  # 
  # onsetsDistNA <- NULL
  # for (i in 1:length(any.data)) {
  #   onsetsDistNA <- rbind(onsetsDistNA, c(-1, round(length(onsets[i, ][is.na(onsets[i, ])]) /
  #                                                     length(onsets[i, ]), 4),
  #                                         countries[any.data][i]))
  # }
  # 
  # onset_var <- c()
  # for (i in 1:length(any.data)) {
  #   onset_var <- c(onset_var, var(onsets[i, ], na.rm = T))
  # }
  # 
  # onsets.old <- onsets
  # onsets <- vector('list', length(any.data))
  # for (i in 1:length(any.data)) {
  #   onsets[[i]] <- onsets.old[i, ][!is.na(onsets.old[i, ])]
  # }
  # 
  # onsetsDist <- NULL
  # for (i in 1:length(any.data)) {
  #   onsetsDist.temp <- NULL
  #   
  #   onsets.temp <- onsets[[i]]
  #   for (j in sort(unique(onsets.temp))) {
  #     onsetsDist.temp <- rbind(onsetsDist.temp,
  #                              c(j, round(length(onsets.temp[onsets.temp == j]) / 300, 4)))
  #   }
  #   onsetsDist.temp <- cbind(onsetsDist.temp, rep(countries[any.data][i],
  #                                                 dim(onsetsDist.temp)[1]))
  #   
  #   onsetsDist <- rbind(onsetsDist, onsetsDist.temp)
  # }
  # 
  # onset <- c()
  # for (i in 1:length(any.data)) { # to find forecasted onset
  #   onsetsDist.temp <- onsetsDist[onsetsDist[, 3] == countries[any.data][i], 1:2]
  #   if (as.numeric(onsetsDistNA[onsetsDistNA[, 3] == countries[any.data][i], 2]) > max(as.numeric(onsetsDist.temp[, 2]))) {
  #     onset <- c(onset, NA)
  #   } else {
  #     on.temp <- onset.vals[[i]]
  #     if (length(on.temp) > 1) {
  #       # France
  #       if (season %in% c('2014-15', '2015-16', '2016-17', '2017-18')) {
  #         on.temp <- on.temp[2]
  #       } else {
  #         on.temp <- on.temp[1]
  #       }
  #     }
  #     onset <- c(onset, findOnset(Y[i, ], on.temp)$onset)
  #   }
  # }
  # 
  # onsetsDist <- rbind(onsetsDist, onsetsDistNA)
  # 
  # # Peak timing:
  # peakWeeksDistNA <- NULL
  # for (i in 1:length(any.data)) {
  #   peakWeeksDistNA <- rbind(peakWeeksDistNA, c(-1, round(length(pkwks[i, ][is.na(pkwks[i, ])]) /
  #                                                           length(pkwks[i, ]), 4),
  #                                               countries[any.data][i]))
  # }
  # # QUESTION: shouldn't this be NA if no onset???
  # 
  # pkwks.old <- pkwks
  # pkwks <- vector('list', length(any.data))
  # for (i in 1:length(any.data)) {
  #   pkwks[[i]] <- pkwks.old[i, ][!is.na(pkwks.old[i, ])]
  # }
  # 
  # peakWeeksDist <- NULL
  # for (i in 1:length(any.data)) {
  #   peakWeeksDist.temp <- NULL
  #   pkwks.temp <- pkwks[[i]]
  #   for (j in sort(unique(pkwks.temp))) {
  #     peakWeeksDist.temp <- rbind(peakWeeksDist.temp,
  #                                 c(j, round(length(pkwks.temp[pkwks.temp == j]) / 300, 4)))
  #   }
  #   peakWeeksDist.temp <- cbind(peakWeeksDist.temp, rep(countries[any.data][i],
  #                                                       dim(peakWeeksDist.temp)[1]))
  #   peakWeeksDist <- rbind(peakWeeksDist, peakWeeksDist.temp)
  # }
  # 
  # peakWeeksDist <- rbind(peakWeeksDist, peakWeeksDistNA)
  # 
  # # Peak intensity:
  # reqLimits <- seq(0, 1e4, by = 1e3)
  # ili_peaks.old <- ili_peaks
  # ili_peaks <- vector('list', length(any.data))
  # for (i in 1:length(any.data)) {
  #   ili_peaks[[i]] <- ili_peaks.old[i, ][!is.na(ili_peaks.old[i, ])]
  # }
  # 
  # peakIntensitiesDist <- NULL
  # for (i in 1:length(any.data)) {
  #   peakIntensitiesDist.temp <- NULL
  #   ili_peaks.temp <- ili_peaks[[i]]
  #   for (j in 2:length(reqLimits)) {
  #     peakIntensitiesDist.temp <- rbind(peakIntensitiesDist.temp,
  #                                       c(reqLimits[j], round(length(ili_peaks.temp[ili_peaks.temp >= reqLimits[j - 1] & ili_peaks.temp < reqLimits[j]]) / length(ili_peaks.temp), 4)))
  #   }
  #   peakIntensitiesDist.temp <- rbind(peakIntensitiesDist.temp, c(1e5, round(length(ili_peaks.temp[ili_peaks.temp >= max(reqLimits)]) / length(ili_peaks.temp), 4)))
  #   peakIntensitiesDist.temp <- cbind(peakIntensitiesDist.temp, rep(countries[any.data][i],
  #                                                                   dim(peakIntensitiesDist.temp)[1]))
  #   peakIntensitiesDist <- rbind(peakIntensitiesDist, peakIntensitiesDist.temp)
  # }
  # 
  # # Next 4 weeks:
  # nextILIDist <- NULL
  # for (i in 1:length(any.data)) {
  #   nextILIDist.temp <- NULL
  #   nextILI.temp <- nextILI[i,, ]
  #   
  #   for (k in 1:4) {
  #     values <- nextILI.temp[, k]
  #     values <- values[!is.na(values)]
  #     for (j in 2:length(reqLimits)) {
  #       nextILIDist.temp <- rbind(nextILIDist.temp, c(reqLimits[j], round(length(values[values >= reqLimits[j - 1] & values < reqLimits[j]]) / length(values), 4), k))
  #     }
  #     nextILIDist.temp <- rbind(nextILIDist.temp, c(1e5, round(length(values[values >= max(reqLimits)]) / length(values), 4), k))
  #   }
  #   
  #   nextILIDist.temp <- cbind(nextILIDist.temp, rep(countries[any.data][i],
  #                                                   dim(nextILIDist.temp)[1]))
  #   nextILIDist <- rbind(nextILIDist, nextILIDist.temp)
  # }
  # 
  # #### OUTPUT DATA
  # # Output the prediction from the last iteration
  # obsprior_mean <- obsprior_mean[any.data, ]
  # obspost_mean <- obspost_mean[any.data, ]
  # obs_sd <- obs_sd[any.data, ]
  # 
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
  #   out6 <- rbind(out6, cbind(rep(countries[any.data][i], 5), c('pi', '1week', '2week', '3week', '4week'),
  #                             rep(fc_start, 5), rbind(ili_peaks[[i]], t(nextILI[i,, ]))))
  # }
  # colnames(out1) <- c('country', 'fc_start', 'time', 'week', 'Est', 'Est_sd', 'S', 'I', 'S_sd', 'I_sd',
  #                     'L', 'D', 'R0max', 'R0min', 'L_sd', 'D_sd', 'R0max_sd', 'R0min_sd')
  # colnames(out2) <- c('country', 'fc_start', 'time', 'week', 'Est', 'Est_sd', 'S', 'I', 'S_sd', 'I_sd')
  # colnames(out4) <- c('country', 'fc_start', 'time', 'Est', 'S', 'I', 'L', 'D', 'R0max', 'R0min')
  # colnames(out6) <- c('country', 'metrics', 'fc_start', 1:300)
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
  # onsetsDist <- cbind('onset', onsetsDist)
  # peakWeeksDist <- cbind('pw', peakWeeksDist)
  # peakIntensitiesDist <- cbind('pi', peakIntensitiesDist)
  # out5 <- rbind(onsetsDist, peakWeeksDist, peakIntensitiesDist)
  # for (i in c('1', '2', '3', '4')) {
  #   out5 <- rbind(out5, cbind(paste0('nextweek', i), nextILIDist[nextILIDist[, 3] == i, c(1:2, 4)]))
  # }
  # out5 <- cbind(rep(fc_start, nrow(out5)), out5)
  # 
  # if(metricsonly==F){
  #   out=list(train=out1,fcast=out2,metrics=out3,trainprior=out4,dist=out5,ensembles=out6);
  # }else{
  #   out=list(metrics=out3);
  # }
  
}





