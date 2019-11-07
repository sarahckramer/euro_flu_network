
EAKF_rFC <- function(num_ens, tmstep, param.bound, obs_i = obs_i, ntrn = 1, obs_vars,
                     tm.ini = 273, tm.range = 273:500, updates = FALSE, do.reprobing = FALSE){
  
  if (!exists('updates')) {
    updates <- FALSE
  }
  if (!exists('do.reprobing')) {
    do.reprobing <- FALSE
  }
  
  num_times <- floor(length(tm.range) / tmstep)
  nfc <- nsn - ntrn # number of weeks for forecasting
  tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep)
  
  # theta_low <- param.bound[, 1]; theta_up <- param.bound[, 2]
  
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
  # fc.met = fc.op = fc.dist = fc.ens = NULL
  fcast <- array(0, c(n ** 2 * 3, num_ens, nfc)) # fcast: S, I, newI;
  
  obsprior <- array(NA, c(n, num_ens, ntrn + 1))
  obspost <- array(NA, c(n, num_ens, ntrn))
  
  ### Where each state/param is stored:
  S0.indices <- 1:(n ** 2) # where S0 are stored
  I0.indices <- S0.indices + S0.indices[length(S0.indices)] # where I0 are stored
  newI.indices <- S0.indices + S0.indices[length(S0.indices)] * 2 # where newI are stored (in fcast) (only individual compartments)
  param.indices <- (max(newI.indices) + 1):(max(newI.indices) + 5) # where the epi parameters are stored
  
  # Do we continue to generate S0 as a distribution even when forecasting, or do we use LHS?
  # Dist:
  parms <- t(lhs(num_ens, param.bound))
  S0.temp = I0.temp = vector('list', num_ens)
  for (i in 1:num_ens) {
    S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
    
    diag(S0.temp[[i]]) <- rnorm(n, mean = parms[1, i], sd = parms[2, i])
    diag(I0.temp[[i]]) <- parms[(1:n) + 2, i]
    
    # ensure all between 0 and 1:
    while (any(diag(S0.temp[[i]]) > 1)) {
      diag(S0.temp[[i]])[which(diag(S0.temp[[i]]) > 1)] <- rnorm(length(which(diag(S0.temp[[i]]) > 1)),
                                                                 mean = parms[1, i], sd = parms[2, i])
    }
    while (any(diag(S0.temp[[i]]) < 0)) {
      diag(S0.temp[[i]])[which(diag(S0.temp[[i]]) < 0)] <- rnorm(length(which(diag(S0.temp[[i]]) < 0)),
                                                                 mean = parms[1, i], sd = parms[2, i])
    }
    
    # now same process for non-home-home compartments
    S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
      rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.025)
    })
    
    while (any(S0.temp[[i]] > 1)) {
      to.redraw <- which(S0.temp[[i]] > 1, arr.ind = TRUE)
      for (ix.r in dim(to.redraw)[1]) {
        S0.temp[[i]][to.redraw[ix.r, 1], to.redraw[ix.r, 2]] <-
          rnorm(1, mean = S0.temp[[i]][to.redraw[ix.r, 2], to.redraw[ix.r, 2]], sd = 0.025)
      }
    }
    while (any(S0.temp[[i]] < 0)) {
      to.redraw <- which(S0.temp[[i]] < 0, arr.ind = TRUE)
      for (ix.r in dim(to.redraw)[1]) {
        S0.temp[[i]][to.redraw[ix.r, 1], to.redraw[ix.r, 2]] <-
          rnorm(1, mean = S0.temp[[i]][to.redraw[ix.r, 2], to.redraw[ix.r, 2]], sd = 0.025)
      }
    }
    
    # Finish!
    S0.temp[[i]] <- t(S0.temp[[i]])
    S0.temp[[i]] <- S0.temp[[i]] * N
    
    I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
    I0.temp[[i]] <- I0.temp[[i]] * N
    
  }
  # QUESTION: I0 could still just be LHS; seed just "main" compartments?
  parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
  
  # # LHS:
  # ### Set initial conditions based on input parameters
  # param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
  #                      c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))
  # parms <- t(lhs(num_ens, param.bound))
  # 
  # S0.temp = I0.temp = vector('list', num_ens)
  # for (i in 1:num_ens) {
  #   S0.temp[[i]] <- matrix(parms[1:(n ** 2), i], nrow = n, ncol = n, byrow = T) * N
  #   I0.temp[[i]] <- matrix(parms[1:(n ** 2) + (n ** 2), i], nrow = n, ncol = n, byrow = T) * N
  # }
  # parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
  
  ### Calculate the reproductive number at time t BT1 and the transmission rate
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- AH[beta.range, ]
  AHpt <- as.matrix(AHpt, length(AHpt), n)
  a <- -180
  
  # R0diff:
  b <- log(parms[4, ])
  beta <- lapply(1:num_ens, function(ix) {
    (exp(a * AHpt + b[ix]) + (parms[3, ix] - parms[4, ix])) / parms[2, ix]
  })
  
  # # R0mn:  
  # b <- log(parms[3, ] - parms[4, ])
  # beta <- lapply(1:num_ens, function(ix) {
  #   (exp(a * AHpt + b[ix]) + parms[4, ix]) / parms[2, ix]
  # })

  tcurrent <- tm.ini
  
  ### Create vectors of initial parameters:
  D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[5, ]
  
  # integrate 1 step forward
  Sr_tmp <- sapply(1:num_ens, function(ix) {
    propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     airScale = airScale.temp[ix], realdata = TRUE,
                     prohibAir = FALSE)
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
        
        if (length(x[S0.indices, ][which(x[S0.indices, ] < 0, arr.ind = TRUE)]) > 0) {
          print('Some S0 reduced below 0.')
        }
        x[which(x < 0, arr.ind = TRUE)] <- 0 # try - set this to 1.0 instead of 0, like in Fn_checkxnobounds?
        x <- Fn_checkxnobounds(x, S0.indices, I0.indices, param.indices) # this alone sets the "empty" compartments to 1.0; also, nothing to check newI? (okay, b/c makes sure don't go below 0)
        obs_ens[loc, obs_ens[loc, ] < 0] <- 0 # so we do ensure these aren't wild as we go, though
        
        # # Alternatively, replace these with a random draw from I0?
        # x[S0.indices, ][which(x[S0.indices, ] < 0, arr.ind = TRUE)] <- runif(length(x[which(x < 0, arr.ind = TRUE)]), 0.5, 0.9) * pop.size$pop[loc]
        # x[I0.indices, ][which(x[I0.indices, ] < 0, arr.ind = TRUE)] <- runif(length(x[which(x < 0, arr.ind = TRUE)]), 0, 0.001) * pop.size$pop[loc]
        # x[S0.indices, ][which(x[S0.indices, ] < 0, arr.ind = TRUE)] <- runif(length(x[which(x < 0, arr.ind = TRUE)]), 0, 0.001) * pop.size$pop[loc]
        
        # Could be an interesting idea, but need to get the correct pop size by compartment, not country; unless we only want to correct obs_ens this way and not x?
        
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
    a <- -180
    
    b <-log(xpost[param.indices[4],, tt])
    beta <- lapply(1:num_ens, function(ix) {
      (exp(a * AHpt + b[ix]) + (xpost[param.indices[3], ix, tt] - xpost[param.indices[4], ix, tt])) /
        xpost[param.indices[2], ix, tt]
    })
    
    # b <- log(xpost[param.indices[3],, tt] - xpost[param.indices[4],, tt])
    # beta <- lapply(1:num_ens, function(ix) {
    #   (exp(a * AHpt + b[ix]) + xpost[param.indices[4], ix, tt]) /
    #     xpost[param.indices[2], ix, tt]
    # })
    
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
                       airScale = airScale.temp[ix], realdata = TRUE,
                       prohibAir = FALSE)
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
    
  } # end of training
  
  ### Forecast
  a <- -180
  
  b <-log(xpost[param.indices[4],, ntrn])
  beta <- lapply(1:num_ens, function(ix) {
    (exp(a * AHpt + b[ix]) + (xpost[param.indices[3], ix, ntrn] - xpost[param.indices[4], ix, ntrn])) /
      xpost[param.indices[2], ix, ntrn]
  })
  
  # b <- log(xpost[param.indices[3],, ntrn] - xpost[param.indices[4],, ntrn])
  # beta <- lapply(1:num_ens, function(ix) {
  #   (exp(a * AHpt + b[ix]) + xpost[param.indices[4], ix, ntrn]) /
  #     xpost[param.indices[2], ix, ntrn]
  # })
  
  tcurrent <- tm.ini + tmstep * ntrn
  
  # Draw S0 and I0 from xpost
  # Note: These are already "standardized" by population size, so no longer any need to multiply by N or divide by 100,000, right? (S, I, newI are counts)
  S0.temp <- lapply(1:num_ens, function(ix) {
    matrix(xpost[S0.indices, ix, ntrn], ncol = n, byrow = TRUE)
  })
  I0.temp <- lapply(1:num_ens, function(ix) {
    matrix(xpost[I0.indices, ix, ntrn], ncol = n, byrow = TRUE)
  })
  D.temp <- xpost[param.indices[2],, ntrn]; L.temp <- xpost[param.indices[1],, ntrn];
  airScale.temp <- xpost[param.indices[5],, ntrn]
  
  Sr_tmp <- sapply(1:300, function(ix) {
    propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep * nfc, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     airScale = airScale.temp[ix], realdata = TRUE,
                     prohibAir = FALSE)
  })
  
  Sr_tmp_S <- lapply(1:num_ens, function(ix) {
    t(Sr_tmp[1, ix]$S[tmstep * (1:nfc) + 1, ])
  })
  Sr_tmp_I <- lapply(1:num_ens, function(ix) {
    t(Sr_tmp[2, ix]$I[tmstep * (1:nfc) + 1, ])
  })
  Sr_tmp_newI <- lapply(1:num_ens, function(ix) {
    t(Sr_tmp[3, ix]$newI[tmstep * (1:nfc) + 1, ] - Sr_tmp[3, ix]$newI[tmstep * (0:(nfc - 1)) + 1, ])
  })
  
  for (ensmem in 1:num_ens) {
    fcast[S0.indices, ensmem, ] <- Sr_tmp_S[[ensmem]]
    fcast[I0.indices, ensmem, ] <- Sr_tmp_I[[ensmem]]
    fcast[newI.indices, ensmem, ] <- Sr_tmp_newI[[ensmem]]
  }
  
  ### Also calculate total newI for each COUNTRY, and call these "obsfcast"
  obs_ens <- lapply(1:num_ens, function(jx) {
    matrix(unlist(lapply(1:n, function(ix) {
      colSums(Sr_tmp_newI[[jx]][1:n + n * (ix - 1), ])
    })), nrow = n, byrow = T)
  })
  
  obsfcast <- array(NA, c(n, num_ens, nfc))
  for (ensmem in 1:num_ens) {
    obsfcast[, ensmem, ] <- obs_ens[[ensmem]]
  }
  
  for (i in 1:n) {
    obsfcast[i,, ] <- obsfcast[i,, ] / pop.size$pop[i] * 100000
  }
  
  rm(obs_ens)
  
  ### Plot fit and forecast:
  par(mfrow = c(4, 3), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  for (i in (1:n)[!is.na(obs_i[tt, ])]) {
    obs.post.toPlot.ind <- colMeans(obspost[i,, 1:tt])
    obs.fcast.toPlot.ind <- colMeans(obsfcast[i,, 1:nfc])
    plot(obs_i[, i], type = 'b', pch = 4, lty = 2, col = 'gray40', cex = 0.75,
         xlab = 'Wks from Start', ylab = 'Syn+ Counts', main = countries[i], 
         ylim = c(0, max(max(obs_i[, i], na.rm = T), max(obs.post.toPlot.ind, na.rm = T))))
    lines(obs.post.toPlot.ind, type = 'b', pch = 20, lty = 1, cex = 0.8, col = 'steelblue2')
    lines((ntrn + 1):(ntrn + nfc), obs.fcast.toPlot.ind, type = 'b', pch = 20, lty = 1, cex = 0.8, col = 'coral')
  }
  
  ### Calculate S and I by country (prior, post, and fcast):
  s.post <- xpost[S0.indices,, ]; s.prior <- xprior[S0.indices,, ]
  i.post <- xpost[I0.indices,, ]; i.prior <- xprior[I0.indices,, ]
  s.fcast <- fcast[S0.indices,, ]; i.fcast <- fcast[I0.indices,, ]
  
  s.post.by.count = i.post.by.count = array(0, c(n, num_ens, ntrn))
  s.prior.by.count = i.prior.by.count = array(0, c(n, num_ens, ntrn + 1))
  s.fcast.by.count = i.fcast.by.count = array(0, c(n, num_ens, nfc))
  for (i in 1:n) {
    country.vals <- (1:n) + n * (i - 1)
    s.post.temp <- s.post[country.vals,, ]
    s.prior.temp <- s.prior[country.vals,, ]
    s.fcast.temp <- s.fcast[country.vals,, ]
    i.post.temp <- i.post[country.vals,, ]
    i.prior.temp <- i.prior[country.vals,, ]
    i.fcast.temp <- i.fcast[country.vals,, ]
    
    for (j in 1:num_ens) {
      s.post.by.count[i, j, ] <- colSums(s.post.temp[, j, ]) / pop.size$pop[i] * 100000
      s.prior.by.count[i, j, ] <- colSums(s.prior.temp[, j, ]) / pop.size$pop[i] * 100000
      s.fcast.by.count[i, j, ] <- colSums(s.fcast.temp[, j, ]) / pop.size$pop[i] * 100000
      i.post.by.count[i, j, ] <- colSums(i.post.temp[, j, ]) / pop.size$pop[i] * 100000
      i.prior.by.count[i, j, ] <- colSums(i.prior.temp[, j, ]) / pop.size$pop[i] * 100000
      i.fcast.by.count[i, j, ] <- colSums(i.fcast.temp[, j, ]) / pop.size$pop[i] * 100000
    }
    
  }
  
  ### Calculate ensemble means and sds (prior, post, and fcast; S, I, newI):
  obsprior_mean <- t(apply(obsprior[,, 1:(ntrn + 1)], c(1, 3), mean))
  obspost_mean <- t(apply(obspost[,, 1:ntrn], c(1, 3), mean))
  obsfcast_mean <- t(apply(obsfcast[,, 1:nfc], c(1, 3), mean))
  obspost_sd <- t(apply(obspost[,, 1:ntrn], c(1, 3), sd))
  obsfcast_sd <- t(apply(obsfcast[,, 1:nfc], c(1, 3), sd))
  
  sprior_mean <- t(apply(s.prior.by.count[,, 1:(ntrn + 1)], c(1, 3), mean))
  spost_mean <- t(apply(s.post.by.count[,, 1:ntrn], c(1, 3), mean))
  sfcast_mean <- t(apply(s.fcast.by.count[,, 1:nfc], c(1, 3), mean))
  spost_sd <- t(apply(s.post.by.count[,, 1:ntrn], c(1, 3), sd))
  sfcast_sd <- t(apply(s.fcast.by.count[,, 1:nfc], c(1, 3), sd))
  
  iprior_mean <- t(apply(i.prior.by.count[,, 1:(ntrn + 1)], c(1, 3), mean))
  ipost_mean <- t(apply(i.post.by.count[,, 1:ntrn], c(1, 3), mean))
  ifcast_mean <- t(apply(i.fcast.by.count[,, 1:nfc], c(1, 3), mean))
  ipost_sd <- t(apply(i.post.by.count[,, 1:ntrn], c(1, 3), sd))
  ifcast_sd <- t(apply(i.fcast.by.count[,, 1:nfc], c(1, 3), sd))
  
  statesprior <- cbind(melt(sprior_mean), melt(iprior_mean), melt(obsprior_mean)); statesprior <- statesprior[, c(1:3, 6, 9)];
  names(statesprior) <- c('week', 'country', 'S', 'I', 'newI');
  statesprior$week <- statesprior$week + wk_start - 1; statesprior$country <- countries[statesprior$country]
  
  statespost <- cbind(melt(spost_mean), melt(spost_sd), melt(ipost_mean), melt(ipost_sd), melt(obspost_mean), melt(obspost_sd));
  statespost <- statespost[, c(1:3, 6, 9, 12, 15, 18)];
  names(statespost) <- c('week', 'country', 'S', 'S_sd', 'I', 'I_sd', 'newI', 'newI_sd');
  statespost$week <- statespost$week + wk_start - 1; statespost$country <- countries[statespost$country]
  
  statesfcast <- cbind(melt(sfcast_mean), melt(sfcast_sd), melt(ifcast_mean), melt(ifcast_sd), melt(obsfcast_mean), melt(obsfcast_sd));
  statesfcast <- statesfcast[, c(1:3, 6, 9, 12, 15, 18)]; names(statesfcast) <- c('week', 'country', 'S', 'S_sd', 'I', 'I_sd', 'newI', 'newI_sd');
  statesfcast$week <- statesfcast$week + wk_start + ntrn - 1; statesfcast$country <- countries[statesfcast$country]
  
  ### Get parameter means and sds over time (prior and post):
  params.prior_mean <- t(apply(xprior[param.indices,, 1:(ntrn + 1)], c(1, 3), mean))
  params.post_mean <- t(apply(xpost[param.indices,, 1:ntrn], c(1, 3), mean))
  params.post_sd <- t(apply(xpost[param.indices,, 1:ntrn], c(1, 3), sd))
  # QUESTION: SD continues to increase near end of outbreak - okay?
  params.post_df <- as.data.frame(cbind(params.post_mean, params.post_sd))
  names(params.post_df) <- c('L', 'D', 'R0mx', 'R0diff', 'airScale', 'L_sd', 'D_sd', 'R0mx_sd', 'R0diff_sd', 'airScale_sd')
  
  ### Calculate metrics to return:
  Y <- rbind(obspost_mean, obsfcast_mean)
  Y.ens <- array(NA, c(n, num_ens, nsn))
  Y.ens[,, 1:ntrn] <- obspost; Y.ens[,, (ntrn + 1):(ntrn + nfc)] <- obsfcast
  
  obs_pkwk = pkwk_mode = leadpkwk_mode = pkwk_mean = leadpkwk_mean = pkwk_var = delta_pkwk_mode = delta_pkwk_mean = 
    obs_peak_int = peak_intensity = intensity_err = peak_intensity_var = totAttackObs = tot_attack = ar_err = ar_var =
    onsetObs3 = onsetObs4 = onsetObs5 = onsetObs6 = onset3 = onset4 = onset5 = onset6 = onset3_var = onset4_var = onset5_var = onset6_var =
    endObs3 = endObs4 = endObs5 = endObs6 = end3 = end4 = end5 = end6 = end3_var = end4_var = end5_var = end6_var =
    dur3_var = dur4_var = dur5_var = dur6_var =
    obs1wk = obs2wk = obs3wk = obs4wk = fcast1wk = fcast2wk = fcast3wk = fcast4wk = rdiff_next_newI = rdiff_next_newI2 = rdiff_next_newI3 = rdiff_next_newI4 =
    corr = rmse = corr_fcast = rmse_fcast = mape = wape = smape = rep(NA, n)
  
  peakWeeks = peakIntensities = totalARs = onsets3 = onsets4 = onsets5 = onsets6 = ends3 = ends4 = ends5 = ends6 = matrix(NA, n, num_ens)
  nextILI <- obsfcast[,, 1:4] #array(NA, c(n, num_ens, 4))
  
  for (i in 1:n) {
    
    if (!all(is.na(obs_i[, i]))) {
      
      # distributions:
      for (ensmem in 1:num_ens) {
        yy <- Y.ens[i, ensmem, ]
        
        peakWeeks[i, ensmem] <- which.max(yy)
        peakIntensities[i, ensmem] <- max(yy)
        totalARs[i, ensmem] <- sum(yy[!is.na(obs_i[, i])])
        
        onsets3[i, ensmem] <- findOnset(yy, 300)$onset
        ends3[i, ensmem] <- findOnset(yy, 300)$end
        onsets4[i, ensmem] <- findOnset(yy, 400)$onset
        ends4[i, ensmem] <- findOnset(yy, 400)$end
        onsets5[i, ensmem] <- findOnset(yy, 500)$onset
        ends5[i, ensmem] <- findOnset(yy, 500)$end
        onsets6[i, ensmem] <- findOnset(yy, 600)$onset
        ends6[i, ensmem] <- findOnset(yy, 600)$end
      }
      
      # point metrics:
      obs_pkwk[i] <- which.max(obs_i[, i])
      
      pkwk_mode[i] <- MODE(peakWeeks[i, ])[1]
      leadpkwk_mode[i] <- pkwk_mode[i] - ntrn
      delta_pkwk_mode[i] <- pkwk_mode[i] - obs_pkwk[i]
      
      pkwk_mean[i] <- which.max(Y[, i])
      leadpkwk_mean[i] <- pkwk_mean[i] - ntrn
      delta_pkwk_mean[i] <- pkwk_mean[i] - obs_pkwk[i]
      
      pkwk_var[i] <- var(peakWeeks[i, ], na.rm = TRUE)
      
      obs_peak_int[i] <- max(obs_i[, i], na.rm = TRUE)
      peak_intensity[i] <- max(Y[, i])
      intensity_err[i] <- peak_intensity[i] - obs_peak_int[i]
      peak_intensity_var[i] <- var(peakIntensities[i, ], na.rm = TRUE)
      
      totAttackObs[i] <- sum(obs_i[, i], na.rm = TRUE)
      tot_attack[i] <- sum(Y[!is.na(obs_i[, i]), i]) # sum predicted cases only where actual data NOT NA - otherwise adding in "extra" compared to observations
      ar_err[i] <- tot_attack[i] - totAttackObs[i]
      ar_var[i] <- var(totalARs[i, ], na.rm = TRUE)
      
      onsetObs3[i] <- findOnset(obs_i[, i], 300)$onset
      onsetObs4[i] <- findOnset(obs_i[, i], 400)$onset
      onsetObs5[i] <- findOnset(obs_i[, i], 500)$onset
      onsetObs6[i] <- findOnset(obs_i[, i], 600)$onset
      
      onset3_var[i] <- var(onsets3[i, ], na.rm = TRUE)
      onset4_var[i] <- var(onsets4[i, ], na.rm = TRUE)
      onset5_var[i] <- var(onsets5[i, ], na.rm = TRUE)
      onset6_var[i] <- var(onsets6[i, ], na.rm = TRUE)
      
      endObs3[i] <- findOnset(obs_i[, i], 300)$end
      endObs4[i] <- findOnset(obs_i[, i], 400)$end
      endObs5[i] <- findOnset(obs_i[, i], 500)$end
      endObs6[i] <- findOnset(obs_i[, i], 600)$end
      
      end3_var[i] <- var(ends3[i, ], na.rm = TRUE)
      end4_var[i] <- var(ends4[i, ], na.rm = TRUE)
      end5_var[i] <- var(ends5[i, ], na.rm = TRUE)
      end6_var[i] <- var(ends6[i, ], na.rm = TRUE)
      
      # continuous error metrics:
      corr[i] <- cor(Y[, i], obs_i[, i], use = 'pairwise.complete.obs')
      rmse[i] <- sqrt(mean((Y[, i] - obs_i[, i]) ** 2, na.rm = TRUE))
      
      corr_fcast[i] <- cor(obsfcast_mean[, i], obs_i[(ntrn + 1):nsn, i], use = 'pairwise.complete.obs')
      rmse_fcast[i] <- sqrt(mean((obsfcast_mean[, i] - obs_i[(ntrn + 1):nsn, i]) ** 2, na.rm = TRUE))
      
      obs_adj <- obs_i[(ntrn + 1):nsn, i]
      obs_adj[obs_adj == 0 & !is.na(obs_adj)] <- 1.0
      mape[i] <- sum(abs((obs_adj - obsfcast_mean[, i]) / obs_adj), na.rm = TRUE) * 100 / nfc
      wape[i] <- sum(abs(obs_i[(ntrn + 1):nsn, i] - obsfcast_mean[, i]), na.rm = TRUE) / sum(obs_i[(ntrn + 1):nsn, i], na.rm = TRUE) * 100 - nfc
      smape[i] <- sum(abs(obsfcast_mean[, i] - obs_i[(ntrn + 1):nsn, i]) / ((obsfcast_mean[, i] + obs_i[(ntrn + 1):nsn, i]) / 2), na.rm = TRUE) * 100 / nfc
      
      # 1-4 weeks ahead:
      if (dim(obsfcast_mean)[2] > 0 & !is.na(obs_i[ntrn + 1, i])) {
        obs1wk[i] <- obs_i[ntrn + 1, i]
        fcast1wk[i] <- obsfcast_mean[1, i]
        deno_obs_i <- ifelse(obs_i[ntrn + 1, i] == 0, 1.0, obs_i[ntrn + 1, i])
        rdiff_next_newI[i] <- (fcast1wk[i] - deno_obs_i) / deno_obs_i
      }
      
      if (dim(obsfcast_mean)[2] > 1 & !is.na(obs_i[ntrn + 2, i])) {
        obs2wk[i] <- obs_i[ntrn + 2, i]
        fcast2wk[i] <- obsfcast_mean[2, i]
        deno_obs_i <- ifelse(obs_i[ntrn + 2, i] == 0, 1.0, obs_i[ntrn + 2, i])
        rdiff_next_newI2[i] <- (fcast2wk[i] - deno_obs_i) / deno_obs_i
      }
      
      if (dim(obsfcast_mean)[2] > 2 & !is.na(obs_i[ntrn + 3, i])) {
        obs3wk[i] <- obs_i[ntrn + 3, i]
        fcast3wk[i] <- obsfcast_mean[3, i]
        deno_obs_i <- ifelse(obs_i[ntrn + 3, i] == 0, 1.0, obs_i[ntrn + 3, i])
        rdiff_next_newI3[i] <- (fcast3wk[i] - deno_obs_i) / deno_obs_i
      }
      
      if (dim(obsfcast_mean)[2] > 3 & !is.na(obs_i[ntrn + 4, i])) {
        obs4wk[i] <- obs_i[ntrn + 4, i]
        fcast4wk[i] <- obsfcast_mean[4, i]
        deno_obs_i <- ifelse(obs_i[ntrn + 4, i] == 0, 1.0, obs_i[ntrn + 4, i])
        rdiff_next_newI4[i] <- (fcast4wk[i] - deno_obs_i) / deno_obs_i
      }
      
      
      
    }
    
  }
  
  # Calculate durations and durationObs:
  # ends3[ends3 < wk_start & !is.na(ends3)] <- ends3[ends3 < wk_start & !is.na(ends3)] + nsn # convert to make workable with onsets
  # ends4[ends4 < wk_start & !is.na(ends4)] <- ends4[ends4 < wk_start & !is.na(ends4)] + nsn
  # ends5[ends5 < wk_start & !is.na(ends5)] <- ends5[ends5 < wk_start & !is.na(ends5)] + nsn
  # ends6[ends6 < wk_start & !is.na(ends6)] <- ends6[ends6 < wk_start & !is.na(ends6)] + nsn
  
  durations3 <- ends3 - onsets3
  durations4 <- ends4 - onsets4
  durations5 <- ends5 - onsets5
  durations6 <- ends6 - onsets6
  
  for (i in 1:n) {
    dur3_var[i] <- var(durations3[i, ], na.rm = TRUE)
    dur4_var[i] <- var(durations4[i, ], na.rm = TRUE)
    dur5_var[i] <- var(durations5[i, ], na.rm = TRUE)
    dur6_var[i] <- var(durations6[i, ], na.rm = TRUE)
  }
  
  # endObs3[endObs3 < wk_start & !is.na(endObs3)] <- endObs3[endObs3 < wk_start & !is.na(endObs3)] + nsn
  # endObs4[endObs4 < wk_start & !is.na(endObs4)] <- endObs4[endObs4 < wk_start & !is.na(endObs4)] + nsn
  # endObs5[endObs5 < wk_start & !is.na(endObs5)] <- endObs5[endObs5 < wk_start & !is.na(endObs5)] + nsn
  # endObs6[endObs6 < wk_start & !is.na(endObs6)] <- endObs6[endObs6 < wk_start & !is.na(endObs6)] + nsn
  
  durObs3 <- endObs3 - onsetObs3
  durObs4 <- endObs4 - onsetObs4
  durObs5 <- endObs5 - onsetObs5
  durObs6 <- endObs6 - onsetObs6
  
  # Calculate distributions for onsets:
  onsets3DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      onsets3DistNA[, j + 1] <- round(length(onsets3[j, ][is.na(onsets3[j, ])]) / 300, 4)
    }
  }
  onsets3Dist <- matrix(NA, nrow = length(unique(melt(onsets3)[, 3])[!is.na(unique(melt(onsets3)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(onsets3)[, 3]))) {
    onsets3Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        onsets3Dist[row, j + 1] <- round(length(onsets3[j, ][onsets3[j, ] == i & !is.na(onsets3[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (onsets3DistNA[, j + 1] <= max(onsets3Dist[, j + 1])) {
        onset3[j] <- findOnset(Y[, j], 300)$onset
      }
    }
  }
  onsets3Dist <- rbind(onsets3Dist, onsets3DistNA)
  
  onsets4DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      onsets4DistNA[, j + 1] <- round(length(onsets4[j, ][is.na(onsets4[j, ])]) / 300, 4)
    }
  }
  onsets4Dist <- matrix(NA, nrow = length(unique(melt(onsets4)[, 3])[!is.na(unique(melt(onsets4)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(onsets4)[, 3]))) {
    onsets4Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        onsets4Dist[row, j + 1] <- round(length(onsets4[j, ][onsets4[j, ] == i & !is.na(onsets4[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (onsets4DistNA[, j + 1] <= max(onsets4Dist[, j + 1])) {
        onset4[j] <- findOnset(Y[, j], 400)$onset
      }
    }
  }
  onsets4Dist <- rbind(onsets4Dist, onsets4DistNA)
  
  onsets5DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      onsets5DistNA[, j + 1] <- round(length(onsets5[j, ][is.na(onsets5[j, ])]) / 300, 4)
    }
  }
  onsets5Dist <- matrix(NA, nrow = length(unique(melt(onsets5)[, 3])[!is.na(unique(melt(onsets5)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(onsets5)[, 3]))) {
    onsets5Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        onsets5Dist[row, j + 1] <- round(length(onsets5[j, ][onsets5[j, ] == i & !is.na(onsets5[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (onsets5DistNA[, j + 1] <= max(onsets5Dist[, j + 1])) {
        onset5[j] <- findOnset(Y[, j], 500)$onset
      }
    }
  }
  onsets5Dist <- rbind(onsets5Dist, onsets5DistNA)
  
  onsets6DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      onsets6DistNA[, j + 1] <- round(length(onsets6[j, ][is.na(onsets6[j, ])]) / 300, 4)
    }
  }
  onsets6Dist <- matrix(NA, nrow = length(unique(melt(onsets6)[, 3])[!is.na(unique(melt(onsets6)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(onsets6)[, 3]))) {
    onsets6Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        onsets6Dist[row, j + 1] <- round(length(onsets6[j, ][onsets6[j, ] == i & !is.na(onsets6[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (onsets6DistNA[, j + 1] <= max(onsets6Dist[, j + 1])) {
        onset6[j] <- findOnset(Y[, j], 600)$onset
      }
    }
  }
  onsets6Dist <- rbind(onsets6Dist, onsets6DistNA)
  # can we assume that, if there's a predicted onset, there will also be a predicted end? but maybe most predict an onset, but many don't predict an end yet?
  # what can happen is that there's a specific week that has more ensmems for onset than NA, but b/c end is so distributed, NA is the MODE there
  
  # Calculate distributions for ends:
  ends3DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      ends3DistNA[, j + 1] <- round(length(ends3[j, ][is.na(ends3[j, ])]) / 300, 4)
    }
  }
  ends3Dist <- matrix(NA, nrow = length(unique(melt(ends3)[, 3])[!is.na(unique(melt(ends3)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(ends3)[, 3]))) {
    ends3Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        ends3Dist[row, j + 1] <- round(length(ends3[j, ][ends3[j, ] == i & !is.na(ends3[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (ends3DistNA[, j + 1] <= max(ends3Dist[, j + 1])) {
        end3[j] <- findOnset(Y[, j], 300)$end
      }
    }
  }
  ends3Dist <- rbind(ends3Dist, ends3DistNA)
  
  ends4DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      ends4DistNA[, j + 1] <- round(length(ends4[j, ][is.na(ends4[j, ])]) / 300, 4)
    }
  }
  ends4Dist <- matrix(NA, nrow = length(unique(melt(ends4)[, 3])[!is.na(unique(melt(ends4)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(ends4)[, 3]))) {
    ends4Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        ends4Dist[row, j + 1] <- round(length(ends4[j, ][ends4[j, ] == i & !is.na(ends4[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (ends4DistNA[, j + 1] <= max(ends4Dist[, j + 1])) {
        end4[j] <- findOnset(Y[, j], 400)$end
      }
    }
  }
  ends4Dist <- rbind(ends4Dist, ends4DistNA)
  
  ends5DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      ends5DistNA[, j + 1] <- round(length(ends5[j, ][is.na(ends5[j, ])]) / 300, 4)
    }
  }
  ends5Dist <- matrix(NA, nrow = length(unique(melt(ends5)[, 3])[!is.na(unique(melt(ends5)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(ends5)[, 3]))) {
    ends5Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        ends5Dist[row, j + 1] <- round(length(ends5[j, ][ends5[j, ] == i & !is.na(ends5[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (ends5DistNA[, j + 1] <= max(ends5Dist[, j + 1])) {
        end5[j] <- findOnset(Y[, j], 500)$end
      }
    }
  }
  ends5Dist <- rbind(ends5Dist, ends5DistNA)
  
  ends6DistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      ends6DistNA[, j + 1] <- round(length(ends6[j, ][is.na(ends6[j, ])]) / 300, 4)
    }
  }
  ends6Dist <- matrix(NA, nrow = length(unique(melt(ends6)[, 3])[!is.na(unique(melt(ends6)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(ends6)[, 3]))) {
    ends6Dist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        ends6Dist[row, j + 1] <- round(length(ends6[j, ][ends6[j, ] == i & !is.na(ends6[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      if (ends6DistNA[, j + 1] <= max(ends6Dist[, j + 1])) {
        end6[j] <- findOnset(Y[, j], 600)$end
      }
    }
  }
  ends6Dist <- rbind(ends6Dist, ends6DistNA)
  
  # Now calculate onset and end error, as well as duration and duration error:
  # end3[end3 < wk_start & !is.na(end3)] <- end3[end3 < wk_start & !is.na(end3)] + nsn
  # end4[end4 < wk_start & !is.na(end4)] <- end4[end4 < wk_start & !is.na(end4)] + nsn
  # end5[end5 < wk_start & !is.na(end5)] <- end5[end5 < wk_start & !is.na(end5)] + nsn
  # end6[end6 < wk_start & !is.na(end6)] <- end6[end6 < wk_start & !is.na(end6)] + nsn
  
  duration3 <- end3 - onset3
  duration4 <- end4 - onset4
  duration5 <- end5 - onset5
  duration6 <- end6 - onset6
  
  delta_onset3 <- onset3 - onsetObs3
  delta_onset4 <- onset4 - onsetObs4
  delta_onset5 <- onset5 - onsetObs5
  delta_onset6 <- onset6 - onsetObs6
  
  delta_end3 <- end3 - endObs3
  delta_end4 <- end4 - endObs4
  delta_end5 <- end5 - endObs5
  delta_end6 <- end6 - endObs6
  
  delta_dur3 <- duration3 - durObs3
  delta_dur4 <- duration4 - durObs4
  delta_dur5 <- duration5 - durObs5
  delta_dur6 <- duration6 - durObs6
  
  # Calculate prob. distribution for peak weeks:
  peakWeeksDistNA <- matrix(c(-1, rep(NA, n)), nrow = 1, ncol = n + 1)
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      peakWeeksDistNA[, j + 1] <- round(length(peakWeeks[j, ][is.na(peakWeeks[j, ])]) / 300, 4)
    }
  }
  peakWeeksDist <- matrix(NA, nrow = length(unique(melt(peakWeeks)[, 3])[!is.na(unique(melt(peakWeeks)[, 3]))]), ncol = n + 1)
  row <- 1
  for (i in sort(unique(melt(peakWeeks)[, 3]))) {
    peakWeeksDist[row, 1] <- i
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        peakWeeksDist[row, j + 1] <- round(length(peakWeeks[j, ][peakWeeks[j, ] == i & !is.na(peakWeeks[j, ])]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  peakWeeksDist <- rbind(peakWeeksDist, peakWeeksDistNA)
  
  # Calculate prob. distribution for peak intensities
  # we bin in increments of 1e3 upto 1e4. An extra bin for >1e4
  reqLimits = seq(0, 1e4, by=1e3)
  peakIntensitiesDist <- matrix(NA, nrow = length(reqLimits), ncol = n + 1)
  row <- 1
  for (i in 2:length(reqLimits)) {
    peakIntensitiesDist[row, 1] <- reqLimits[i]
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        peakIntensitiesDist[row, j + 1] <- round(length(peakIntensities[j, ][peakIntensities[j, ] >= reqLimits[i - 1] & peakIntensities[j, ] < reqLimits[i]]) / 300, 4)
      }
    }
    
    row <- row + 1
  }
  # >1e4:
  peakIntensitiesDist[row, 1] <- 1e5
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      peakIntensitiesDist[row, j + 1] <- round(length(peakIntensities[j, ][peakIntensities[j, ] >= max(reqLimits)]) / 300, 4)
    }
  }
  
  # Calculate prob. distribution for next 4 weeks:
  nextILIDist <- array(NA, c(length(reqLimits), n + 1, dim(nextILI)[3]))
  row <- 1
  for (i in 2:length(reqLimits)) {
    nextILIDist[row, 1, ] <- reqLimits[i]
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        
        for (k in 1:dim(nextILI)[3]) { # depending on where we are in the season, could be fewer than 4 weeks left
          values <- nextILI[j,, k] # don't think these can possibly be NA, right?
          nextILIDist[row, j + 1, k] <- round(length(values[values >= reqLimits[i - 1] & values < reqLimits[i]]) / 300, 4)
        }
        
      }
    }
    
    row <- row + 1
  }
  # >1e4:
  nextILIDist[row, 1, ] <- 1e5
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      
      for (k in 1:dim(nextILI)[3]) {
        values <- nextILI[j,, k]
        nextILIDist[row, j + 1, k] <- round(length(values[values >= max(reqLimits)]) / 300, 4)
      }
      
    }
  }
  
  
  # pkwk_mode_perc=MODE(peakWeeks)[2]/num_ens
  
  ### Output data:
  tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep)
  fc_start <- ntrn + wk_start - 1
  
  out1 <- cbind(fc_start, 'train', rep(tstep[1:ntrn], n), statespost)
  out1 <- out1[, c(1, 3:4, 2, 5:6, 8, 10, 7, 9, 11)]
  
  out2 <- cbind(fc_start, 'fcast', rep(tstep[(ntrn + 1):nsn], n), statesfcast)
  out2 <- out2[, c(1, 3:4, 2, 5:6, 8, 10, 7, 9, 11)]
  
  out3 <- cbind(fc_start, countries, obs_pkwk + wk_start - 1, pkwk_mode + wk_start - 1, delta_pkwk_mode, pkwk_mean + wk_start - 1, delta_pkwk_mean,
                leadpkwk_mode, leadpkwk_mean, sqrt(pkwk_var), obs_peak_int, peak_intensity, intensity_err, sqrt(peak_intensity_var),
                totAttackObs, tot_attack, ar_err, sqrt(ar_var), obs1wk, obs2wk, obs3wk, obs4wk, fcast1wk, fcast2wk, fcast3wk, fcast4wk,
                rdiff_next_newI, rdiff_next_newI2, rdiff_next_newI3, rdiff_next_newI4, onsetObs3 + wk_start - 1, onsetObs4 + wk_start - 1,
                onsetObs5 + wk_start - 1, onsetObs6 + wk_start - 1, onset3 + wk_start - 1, onset4 + wk_start - 1, onset5 + wk_start - 1,
                onset6 + wk_start - 1, delta_onset3, delta_onset4, delta_onset5, delta_onset6, sqrt(onset3_var), sqrt(onset4_var),
                sqrt(onset5_var), sqrt(onset6_var), endObs3 + wk_start - 1, endObs4 + wk_start - 1, endObs5 + wk_start - 1, endObs6 + wk_start - 1,
                end3 + wk_start - 1, end4 + wk_start - 1, end5 + wk_start - 1, end6 + wk_start - 1, delta_end3, delta_end4, delta_end5,
                delta_end6, sqrt(end3_var), sqrt(end4_var), sqrt(end5_var), sqrt(end6_var), durObs3, durObs4, durObs5, durObs6,
                duration3, duration4, duration5, duration6, delta_dur3, delta_dur4, delta_dur5, delta_dur6, sqrt(dur3_var), sqrt(dur4_var),
                sqrt(dur5_var), sqrt(dur6_var), corr, rmse, corr_fcast, rmse_fcast, mape, wape, smape)
  
  out4 <- cbind(fc_start, tstep[1:ntrn], 1:ntrn, params.post_df)
  
  out5 <- rbind(cbind('onset3', onsets3Dist), cbind('onset4', onsets4Dist), cbind('onset5', onsets5Dist), cbind('onset6', onsets6Dist),
                cbind('pw', peakWeeksDist), cbind('pi', peakIntensitiesDist))
  for (i in 1:4) {
    out5 <- rbind(out5, cbind(paste0('nextweek', i), nextILIDist[,, i]))
  }
  out5 <- as.data.frame(cbind(fc_start, out5))
  names(out5) <- c('fc_start', 'metric', 'bin', countries)
  out5 <- melt(out5, id.vars = c('fc_start', 'metric', 'bin'))
  names(out5)[4] <- 'country'
  
  out6 <- rbind(cbind(fc_start, countries, 'pi', peakIntensities), cbind(fc_start, countries, 'ar', totalARs))
  for (i in 1:4) {
    out6 <- rbind(out6, cbind(fc_start, countries, paste0(i, 'week'), nextILI[,, i]))
  }
  out6 <- as.data.frame(out6)
  names(out6) <- c('fc_start', 'country', 'metric', 1:300)
  
  colnames(out1) = colnames(out2) = c('fc_start', 'time', 'week', 'result', 'country', 'S', 'I', 'Est', 'S_sd', 'I_sd', 'Est_sd')
  out1$result <- as.character(out1$result); out2$result <- as.character(out2$result)
  out1 <- rbind(out1, out2)
  out1$result <- factor(out1$result)
  
  colnames(out3) = c('fc_start', 'country', 'obs_pkwk', 'pkwk_mode', 'delta_pkwk_mode', 'pkwk_mean', 'delta_pkwk_mean',
                     'leadpkwk_mode', 'leadpkwk_mean', 'pkwk_sd', 'obs_peak_int', 'peak_intensity', 'intensity_err', 'peak_intensity_sd',
                     'totAttackObs', 'tot_attack', 'delta_AR', 'AR_sd', 'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week',
                     'fcast_1week', 'fcast_2week', 'fcast_3week', 'fcast_4week', 'delta_1w', 'delta_2w', 'delta_3w', 'delta_4w',
                     'onsetObs3', 'onsetObs4', 'onsetObs5', 'onsetObs6', 'onset3', 'onset4', 'onset5', 'onset6', 'delta_onset3',
                     'delta_onset4', 'delta_onset5', 'delta_onset6', 'onset3_sd','onset4_sd','onset5_sd','onset6_sd', 'endObs3',
                     'endObs4', 'endObs5', 'endObs6', 'end3', 'end4', 'end5', 'end6', 'delta_end3', 'delta_end4', 'delta_end5', 'delta_end6',
                     'end3_sd', 'end4_sd', 'end5_sd', 'end6_sd', 'durationObs3', 'durationObs4', 'durationObs5', 'durationObs6',
                     'duration3', 'duration4', 'duration5', 'duration6', 'delta_dur3', 'delta_dur4', 'delta_dur5', 'delta_dur6',
                     'duration3_sd', 'duration4_sd', 'duration5_sd', 'duration6_sd', 'corr', 'rmse', 'corr_fcast', 'rmse_fcast',
                     'mape', 'wape', 'smape')
  colnames(out4)[1:3] <- c('fc_start', 'time', 'week')
  
  out <- list(opStates = out1, metrics = out3, trainParams = out4, dist = out5, ensembles = out6)
  # out <- list(train = out1, fcast = out2, metrics = out3, trainParams = out4, dist = out5, ensembles = out6)
  
  return(out)
}

