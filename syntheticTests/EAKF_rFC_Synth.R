
### QUESTIONS:
# Should tm.ini be 3-4 weeks into the outbreak and not actually at time 1?
# I don't think so; tm.ini is just the beginning of October - when does the beginning
# of October happen in our simulation? tm.ini should correspond to the correct date
# at time 1 of the simulation
# Simulations also start at 1st of October (t 273)
# What happens if we remove reinit/change reinit to just setting to 0? (Line 211)
# xpost is already accounting for differences in population sizes, right?
# Check that iterating variables (i, j, etc.) aren't being reused!
# Maybe make a set of slides that goes through this whole process - would help with understanding
# it, and with explaining it to others
# Are we like one week ahead/behind or something?
# Question for Sen: Was alp consistently lower than for single-state forecasts?

EAKF_rFC <- function(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn = 1, obs_vars,
                     tm.ini = 273, tm.range = 273:500, do.reprobing = FALSE){
  
  num_times <- floor(length(tm.range) / tmstep)
  nfc <- nsn - ntrn # number of weeks for forecasting
  
  theta_low <- param.bound[, 1]; theta_up <- param.bound[, 2]
  
  if (!exists('do.reprobing')) {
    do.reprobing <- FALSE
  }
  
  ##########################################################################################
  # Determine which compartments need to be monitored
  # Others are always zero
  pos.comp <- which(t.comm > 0, arr.ind = TRUE)
  # destinations <- vector('list', n)
  pos.in.vector <- c()
  # count.by.pos <- c()
  # main.comp.val <- c()
  # rows.by.country <- vector('list', n)
  # start <- 1
  for (i in 1:n) {
    dests <- as.vector(pos.comp[, 2][pos.comp[, 1] == i])
    dests <- sort(c(dests, i))
    # destinations[[i]] <- dests
    pos.in.vector <- c(pos.in.vector, dests + n * (i - 1))
    # count.by.pos <- c(count.by.pos, rep(i, length(dests)))
    # main.comp.val <- c(main.comp.val, rep(i + n * (i - 1), length(dests)))
    # rows.by.country[[i]] <- start:(start + length(dests) - 1)
    # start <- start + length(dests)
  }; rm(dests)
  # num_comp <- length(unlist(rows.by.country))
  pos.in.vector <- sort(pos.in.vector)
  # count.by.pos <- countries[count.by.pos]
  # count.by.pos <- as.data.frame(cbind(count.by.pos, pos.in.vector, main.comp.val))
  # names(count.by.pos) <- c('country', 'pos', 'main')
  # Probably best to keep large matrices, as it's easier to draw from SIR results
  # Just don't use the zeros in updating everything
  ##########################################################################################
  
  # So <- matrix(0, n ** 2 * 2 + 4, num_ens) # last 4 rows are parameters
  xprior <- array(0, c(n ** 2 * 3 + 5, num_ens, ntrn + 1)) # add newI for each compartment, too
  xpost <- array(0, c(n ** 2 * 3 + 5, num_ens, ntrn))
  # fcast <- array(0, c(n ** 2 * 3, num_ens, nfc)) # fcast: S, I, newI
  
  obsprior <- array(NA, c(n, num_ens, ntrn + 1))
  obspost <- array(NA, c(n, num_ens, ntrn))
  
  # Where each state/param is stored:
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
  parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ] # L, D, R0mx, R0mn, airScale
  
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
  
  ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
  obs_ens <- lapply(1:n, function(ix) {
    colSums(Sr_tmp_newI[1:n + n * (ix - 1), ])
  })
  obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F)) # each row is a single country
  
  # Convert to rate per 100,000
  for (i in 1:n) {
    obs_ens[i, ] <- obs_ens[i, ] / pop.size$pop[i] * 100000
  }
  
  obsprior[,, 1] <- obs_ens
  
  alps <- matrix(NA, nrow = n, ncol = ntrn) # record ratios of obs_var to obs+prior var
  
  #### Begin looping through observations
  #### Training process
  to.adjust <- c(pos.in.vector, pos.in.vector + n ** 2, pos.in.vector + n ** 2 * 2, param.indices)
  for (tt in 1:ntrn) {
    
    # Update state variables and parameters, then integrate forward
    print(tt)
    
    # inflate all states and parameters
    inflat <- diag(x = rep(lambda, n ** 2 * 3 + 5), n ** 2 * 3 + 5, n ** 2 * 3 + 5)
    inflat.obs <- diag(x = rep(lambda, n), n, n)
    xmn <- rowMeans(xprior[,, tt]); obs_ens.mn <- rowMeans(obs_ens)
    x <- inflat %*% (xprior[,, tt] - (xmn %*% matrix(1, 1, num_ens))) + (xmn %*% matrix(1, 1, num_ens))
    obs_ens <- inflat.obs %*% (obsprior[,, tt] - (obs_ens.mn %*% matrix(1, 1, num_ens))) + (obs_ens.mn %*% matrix(1, 1, num_ens))
    
    # Compartments w/o people should always be zero - alert if not:
    not.to.adjust <- (1:dim(xprior)[1])[!((1:dim(xprior)[1]) %in% to.adjust)]
    if (any(x[not.to.adjust, ] != 0)) {
      print('Empty compartment(s) w/ > 0!')
    }
    
    ### FIX 1: Don't allow obsprior to be <0 - set to 0?
    # QUESTION: Don't do this yet? Sen hadn't corrected for aphysicalities yet
    # And yes, some do dip below 0 already
    x[which(x < 0, arr.ind = T)] <- 0 # CHECK
    obs_ens[which(obs_ens < 0, arr.ind = T)] <- 0 # CHECK
    xprior[,, tt] <- x
    obsprior[,, tt] <- obs_ens
    
    # Loop through observations:
    for (loc in 1:n) { # for (loc in n:1) { # to test for sensitivity to loop order
      # Get variances:
      obs_var <- obs_vars[tt, loc]
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
      # POTENTIALLY CONDITION ON THIS!
      alps[loc, tt] <- alp
      print(paste0(countries[loc], ':  ', round(alp, 3), '  ', obs_var / prior_var))
      
      dy <- post_mean + alp * (obs_ens[loc, ] - prior_mean) - obs_ens[loc, ] # no NAs in synthetic data, so don't have to worry about that
      
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
      
      # print(any(x < 0)) # QUESTION: Is this and the next line a problem?
      # if (any(obs_ens < 0)) {
      #   print(countries[loc])
      # }
      # for (div.check.count in 1:n) {
      #   if (any(obs_ens[div.check.count, ] < 0)) {
      #     print(countries[div.check.count])
      #   }
      # } # if yes, they're reducing themselves below 0, obviously
      
      x[which(x < 0, arr.ind = TRUE)] <- 0 # try - set this to 1.0 instead of 0, like in Fn_checkxnobounds?
      x <- Fn_checkxnobounds(x, S0.indices, I0.indices, param.indices) # this alone sets the "empty" compartments to 1.0; also, nothing to check newI? (okay, b/c makes sure don't go below 0)
      obs_ens[loc, obs_ens[loc, ] < 0] <- 0 # so we do ensure these aren't wild as we go, though
    }
    
    xnew <- x
    
    # # Finally, reduce S and I in "empty" compartments to zero
    # to.zero <- (1:(dim(xpost)[1]))[!((1:dim(xpost)[1]) %in% to.adjust)]
    # if (any(xnew == 1.0)) {
    #   print ('1s in xnew!')
    # }
    # # xnew[to.zero, ] <- 0
    
    # REPROBING
    # do.reprobing controls whether reprobing is considered at all; condition for reprobing is inside if-loop
    if (do.reprobing) {
      
      if (any(alps[, tt] > 0.9)) {
      # if (any(alps[, tt] > 0.95)) {
      # if (tt %% 2 == 0) { # even weeks only - so every other week
      # if (tt %in% c(1:43)) { # dummy condition - reprobe every week
        
        print('Reprobing...')
        
        rpnum <- ceiling(0.05 * num_ens) # 2%? 5%?
        rpid <- sample(1:num_ens, rpnum)
        
        parms.reprobe <- t(lhs(rpnum, param.bound))
        
        S0.reprobe = I0.reprobe = vector('list', rpnum)
        for (ir in 1:rpnum) {
          S0.reprobe[[ir]] <- matrix(parms.reprobe[1:(n ** 2), ir], nrow = n, ncol = n, byrow = T) * N
          I0.reprobe[[ir]] <- matrix(parms.reprobe[1:(n ** 2) + (n ** 2), ir], nrow = n, ncol = n, byrow = T) * N
        }
        
        # to get matrices in proper format for adding back to xnew, need to transpose before unlisting
        S0.reprobe <- lapply(1:rpnum, function(ix) {
          t(S0.reprobe[[ix]])
        })
        I0.reprobe <- lapply(1:rpnum, function(ix) {
          t(I0.reprobe[[ix]])
        })
        
        S0.reprobe <- matrix(unlist(S0.reprobe), ncol = rpnum, byrow = F)
        I0.reprobe <- matrix(unlist(I0.reprobe), ncol = rpnum, byrow = F)
        parms.reprobe <- parms.reprobe[(dim(parms.reprobe)[1] - 4):(dim(parms.reprobe)[1]), ]
        # S0.indices go row by row - so full first row, then on to second row, etc.
        
        xnew[S0.indices, rpid] <- S0.reprobe # dim 400 6
        xnew[I0.indices, rpid] <- I0.reprobe
        xnew[param.indices, rpid] <- parms.reprobe
        
        # xnew[xnew < 0] <- 0
        xnew[which(xnew < 0, arr.ind = TRUE)] <- 0
        xnew <- Fn_checkxnobounds(xnew, S0.indices, I0.indices, param.indices)
        
      } # end of all conditional loops
      
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
    D.temp <- xpost[param.indices[2],, tt]; L.temp <- xpost[param.indices[1],, tt];
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
    
    # Plot training progress:
    if (tt %in% c(10, 15, 20, 25, 30, 40)) {
      matplot(obs_i, type = 'b', pch = 4, lty = 2, col = viridis(n), cex = 0.75,
              xlab = 'Weeks from Outbreak Start', ylab = 'Syn+ Counts', main = tt)
      obs.prior.toPlot <- t(apply(obsprior[,, 1:(tt + 1)], c(1, 3), mean))
      matlines(obs.prior.toPlot, type = 'b', pch = 20, lty = 1, cex = 0.75, col = viridis(n))
    }
    # matplot(obs_i, type = 'b', pch = 4, lty = 2, col = viridis(n), cex = 0.75,
    #         xlab = 'Weeks from Outbreak Start', ylab = 'Syn+ Counts', main = tt)
    # obs.prior.toPlot <- t(apply(obsprior[,, 1:(tt + 1)], c(1, 3), mean))
    # matlines(obs.prior.toPlot, type = 'b', pch = 20, lty = 1, cex = 0.75, col = viridis(n))
    
  } # end of training
  
  ### For now, we just want to return: PT, PI, corr, rmse, newI, S, and params!
  
  # Get parameters (and parameter sd) over time:
  params.post <- xpost[param.indices,, 1:tt]
  params.post_mean <- t(apply(params.post, c(1, 3), mean))
  params.post_sd <- t(apply(params.post, c(1, 3), sd))
  
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
  
  # Calculate accuracy metrics:
  pt = pt.obs = pi = pi.obs = ot = ot.obs = corrs = rmses = c()
  for (i in 1:n) {
    obs_temp <- obs_TRUE[1:tt, i]
    pred_temp <- obspost_mean[, i]
    
    wk_start <- 1
    ot.obs <- c(ot.obs, findOnset(obs_temp, 500)$onset)
    ot <- c(ot, findOnset(pred_temp, 500)$onset)
    
    if (!all(is.na(obs_temp))) {
      pt <- c(pt, which(pred_temp == max(pred_temp, na.rm = TRUE)))
      pt.obs <- c(pt.obs, which(obs_temp == max(obs_temp, na.rm = TRUE)))
      
      pi <- c(pi, max(pred_temp, na.rm = TRUE))
      pi.obs <- c(pi.obs, max(obs_temp, na.rm = TRUE))
      
      corrs <- c(corrs, cor(obs_temp, pred_temp, use = 'pairwise.complete.obs'))
      rmses <- c(rmses, sqrt(mean((obs_temp - pred_temp) ** 2, na.rm = TRUE)))
      
    } else {
      pt <- c(pt, NA)
      pt.obs <- c(pt.obs, NA)
      pi <- c(pi, NA)
      pi.obs <- c(pi.obs, NA)
      corrs <- c(corrs, NA)
      rmses <- c(rmses, NA)
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
  
  # Calculate country-level S over time:
  s.post <- xpost[S0.indices,, ]
  
  s.post.by.count <- array(0, c(n, num_ens, nsn))
  for (i in 1:n) {
    country.vals <- (1:n) + n * (i - 1)
    s.post.temp <- s.post[country.vals,, ]
    
    for (j in 1:num_ens) {
      s.post.by.count[i, j, ] <- colSums(s.post.temp[, j, ])
    }
    
  }
  
  s.post_mean <- t(apply(s.post.by.count, c(1, 3), mean))
  s.post_sd <- t(apply(s.post.by.count, c(1, 3), sd))
  
  for (i in 1:n) {
    s.post_mean[, i] <- s.post_mean[, i] / rowSums(N)[i]
    s.post_sd[, i] <- s.post_sd[, i] / rowSums(N)[i]
  }
  
  s.post_mean <- as.data.frame(s.post_mean); names(s.post_mean) <- countries
  s.post_sd <- as.data.frame(s.post_sd); names(s.post_sd) <- countries
  
  # Return all relevant values:
  res.list <- list(m, obspost_mean, obspost_sd, params.post_df, s.post_mean, s.post_sd, alps)
  return(res.list)
}



