
format_model_results <- function(m.res, num.ens, tmstep, tm.strt, tm.end, n, pop.dat) {
  
  # Calculate weekly incidence by compartment:
  nt <- floor((length(tm.strt:tm.end) + 1) / 7)
  newI <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m.res[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
    })), nrow = num_ens, byrow = TRUE)
  }) # each ensemble member has its own row
  
  # Aggregate to the country level:
  newI.c <- vector('list', n)
  for (i in 1:n) {
    newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
  }
  newI.c.COUNT <- newI.c
  
  # Standardize results to per 100,000 population:
  for (i in 1:n) {
    newI.c[[i]] <- (newI.c[[i]] / pop.dat$pop[i]) * 100000
  }
  
  # Create lists by ens_mem:
  synth.runs.RATES = synth.runs.COUNTS = vector('list', num.ens)
  for (ix in 1:length(synth.runs.RATES)) {
    newI.ens = newI.ens.count = NULL
    for (country in 1:n) {
      newI.ens <- rbind(newI.ens, newI.c[[country]][ix, ])
      newI.ens.count <- rbind(newI.ens.count, newI.c.COUNT[[country]][ix, ])
    }
    
    synth.runs.RATES[[ix]] <- newI.ens
    synth.runs.COUNTS[[ix]] <- newI.ens.count
  }
  
  # Return results:
  return(list(synth.runs.RATES, synth.runs.COUNTS))
  
}

format_model_results_multi <- function(m.res, num.ens, tmstep, tm.strt, tm.end, n, pop.dat) {
  
  # Calculate weekly incidence by compartment:
  nt <- floor((length(tm.strt:tm.end) + 1) / 7)
  
  newI1 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[3, jx]$newI[[1]][tmstep * (1:nt) + 1, ix] - m.res[3, jx]$newI[[1]][tmstep * (0:(nt - 1)) + 1, ix]
    })), nrow = num_ens, byrow = TRUE)
  }) # each ensemble member has its own row
  newI2 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[3, jx]$newI[[2]][tmstep * (1:nt) + 1, ix] - m.res[3, jx]$newI[[2]][tmstep * (0:(nt - 1)) + 1, ix]
    })), nrow = num.ens, byrow = TRUE)
  })
  
  # Get S and R, too, but also at weekly level, to conserve space:
  S1 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[1, jx]$S[[1]][tmstep * (1:nt) + 1, ix]
    })), nrow = num.ens, byrow = TRUE)
  })
  S2 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[1, jx]$S[[2]][tmstep * (1:nt) + 1, ix]
    })), nrow = num.ens, byrow = TRUE)
  })
  
  R1 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[4, jx]$R[[1]][tmstep * (1:nt) + 1, ix]
    })), nrow = num.ens, byrow = TRUE)
  })
  R2 <- lapply(1:(n ** 2), function(ix) {
    matrix(unlist(lapply(1:num.ens, function(jx) {
      m.res[4, jx]$R[[2]][tmstep * (1:nt) + 1, ix]
    })), nrow = num.ens, byrow = TRUE)
  })
  
  # Aggregate to the country level:
  newI.c1 = newI.c2 = Sc1 = Sc2 = Rc1 = Rc2 = vector('list', n)
  for (i in 1:n) {
    newI.c1[[i]] <- Reduce('+', newI1[(i * n - n + 1):(i * n)])
    newI.c2[[i]] <- Reduce('+', newI2[(i * n - n + 1):(i * n)])
    Sc1[[i]] <- Reduce('+', S1[(i * n - n + 1):(i * n)])
    Sc2[[i]] <- Reduce('+', S2[(i * n - n + 1):(i * n)])
    Rc1[[i]] <- Reduce('+', R1[(i * n - n + 1):(i * n)])
    Rc2[[i]] <- Reduce('+', R2[(i * n - n + 1):(i * n)])
  }
  # newI.c.COUNT <- newI.c
  # we don't really need to save the raw counts
  
  # Standardize results to per 100,000 population:
  for (i in 1:n) {
    newI.c1[[i]] <- (newI.c1[[i]] / pop.dat$pop[i]) * 100000
    newI.c2[[i]] <- (newI.c2[[i]] / pop.dat$pop[i]) * 100000
    Sc1[[i]] <- (Sc1[[i]] / pop.dat$pop[i]) * 100000
    Sc2[[i]] <- (Sc2[[i]] / pop.dat$pop[i]) * 100000
    Rc1[[i]] <- (Rc1[[i]] / pop.dat$pop[i]) * 100000
    Rc2[[i]] <- (Rc2[[i]] / pop.dat$pop[i]) * 100000
  }
  
  # Create lists by ens_mem:
  # synth.runs.RATES = synth.runs.COUNTS = vector('list', num.ens)
  synth.runs1 = synth.runs2 = synth.s1 = synth.s2 = synth.r1 = synth.r2 = vector('list', num.ens)
  for (ix in 1:length(synth.runs1)) {
    newI.ens1 = newI.ens2 = s.ens1 = s.ens2 = r.ens1 = r.ens2 = NULL
    for (country in 1:n) {
      newI.ens1 <- rbind(newI.ens1, newI.c1[[country]][ix, ])
      newI.ens2 <- rbind(newI.ens2, newI.c2[[country]][ix, ])
      s.ens1 <- rbind(s.ens1, Sc1[[country]][ix, ])
      s.ens2 <- rbind(s.ens2, Sc2[[country]][ix, ])
      r.ens1 <- rbind(r.ens1, Rc1[[country]][ix, ])
      r.ens2 <- rbind(r.ens2, Rc2[[country]][ix, ])
    }
    
    synth.runs1[[ix]] <- newI.ens1
    synth.runs2[[ix]] <- newI.ens2
    synth.s1[[ix]] <- s.ens1
    synth.s2[[ix]] <- s.ens2
    synth.r1[[ix]] <- r.ens1
    synth.r2[[ix]] <- r.ens2
  }
  
  # Return results:
  return(list(synth.runs1, synth.runs2, synth.s1, synth.s2, synth.r1, synth.r2))
  
}

allocate_S0I0 <- function(in.parms, num.ens, n, N, s0.method = NULL) {
  
  # Store country-level S0:
  s0.by.count <- NULL
  
  # Get initial conditions:
  S0.temp = I0.temp = vector('list', num.ens)
  for (i in 1:num.ens) {
    S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
    
    if (s0.method == 'dist') {
      diag(S0.temp[[i]]) <- rnorm(n, mean = in.parms[1, i], sd = in.parms[2, i])
      diag(I0.temp[[i]]) <- in.parms[(1:n) + 2, i]
      
      while (any(diag(S0.temp[[i]]) > 1)) {
        diag(S0.temp[[i]])[which(diag(S0.temp[[i]]) > 1)] <- rnorm(length(which(diag(S0.temp[[i]]) > 1)),
                                                                   mean = in.parms[1, i], sd = in.parms[2, i])
      }
      while (any(diag(S0.temp[[i]]) < 0)) {
        diag(S0.temp[[i]])[which(diag(S0.temp[[i]]) < 0)] <- rnorm(length(which(diag(S0.temp[[i]]) < 0)),
                                                                   mean = in.parms[1, i], sd = in.parms[2, i])
      }
      
    } else if (s0.method == 'lhs') {
      diag(S0.temp[[i]]) <- in.parms[1:n, i]
      diag(I0.temp[[i]]) <- in.parms[(1:n) + n, i]
    } else {
      print('No S0 method specified.')
      break
    }
    
    s0.by.count <- rbind(s0.by.count, diag(S0.temp[[i]]))
    
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
    
    if (any(S0.temp[[i]] < 0)) {
      print('Deal with negatives, too!')
      print(i)
    }
    
    S0.temp[[i]] <- t(S0.temp[[i]])
    S0.temp[[i]] <- S0.temp[[i]] * N
    
    I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
    I0.temp[[i]] <- I0.temp[[i]] * N
  }
  
  return(list(S0.temp, I0.temp, s0.by.count))
}

run_model <- function(in.parms, S0.in, I0.in, in.ah, num.ens, n, N, tm.range, tmstep, tm.strt, tm.end, dt, pop.dat, r0.mn = FALSE, multi = FALSE) {
  
  # Get parameters:
  in.parms <- in.parms[(dim(in.parms)[1] - 4):(dim(in.parms)[1]), ]
  
  # print(dim(in.parms))
  # in.parms[4, ] <- in.parms[3, ] - in.parms[4, ] # option to "replace" R0diff with R0mn, then use the original code
  
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- in.ah[beta.range, ]
  AHpt <- as.matrix(AHpt, length(AHpt), n)
  a <- -180
  
  if (r0.mn) { # use R0mn
    b <- log(in.parms[3, ] - in.parms[4, ])
    beta <- lapply(1:num.ens, function(ix) {
      (exp(a * AHpt + b[ix]) + in.parms[4, ix]) / in.parms[2, ix]
    })
  } else { # use R0diff
    b <- log(in.parms[4, ])
    beta <- lapply(1:num_ens, function(ix) {
      (exp(a * AHpt + b[ix]) + (in.parms[3, ix] - in.parms[4, ix])) / in.parms[2, ix]
    })
  }
  
  D.temp <- in.parms[2, ]; L.temp <- in.parms[1, ]; airScale.temp <- in.parms[5, ]
  S0.temp <- S0.in; I0.temp <- I0.in
  
  # Run model:
  if (multi) {
    S0.temp1 <- S0.in[[1]]; S0.temp2 <- S0.in[[2]]
    I0.temp1 <- I0.in[[1]]; I0.temp2 <- I0.in[[2]]
    
    m <- sapply(1:num.ens, function(ix) {
      propagateToySIRS_multi(tm_strt = tm.strt, tm_end = tm.end, dt,
                       S01 = S0.temp1[[ix]], I01 = I0.temp1[[ix]],
                       S02 = S0.temp2[[ix]], I02 = I0.temp2[[ix]],
                       N, D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                       airScale = airScale.temp[ix], realdata = TRUE,
                       prohibAir = FALSE)
    })
    
    # Re-format results:
    res.list <- format_model_results_multi(m, num.ens, tmstep, tm.strt, tm.end, n, pop.dat)
    # res.list <- list(res.list, s0.by.count)
    
  } else {
    m <- sapply(1:num.ens, function(ix) {
      propagateToySIRS(tm_strt = tm.strt, tm_end = tm.end, dt,
                       S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                       D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                       airScale = airScale.temp[ix], realdata = TRUE,
                       prohibAir = FALSE)
      })
    
    # Re-format results:
    res.list <- format_model_results(m, num.ens, tmstep, tm.strt, tm.end, n, pop.dat)
    # res.list <- list(res.list, s0.by.count)
  
  }
  
  # Return results:
  return(res.list)
  
}

### Analysis ###

calc_metrics <- function(m) {
  
  df <- NULL
  
  for (run in 1:length(m)) {
    out.temp <- m[[run]]
    
    for (count.index in 1:length(countries)) {
      ot.temp <- findOnset(out.temp[count.index, ], 500)$onset + 40 - 1
      pt.temp <- which.max(out.temp[count.index, ]) + 40 - 1
      df <- rbind(df, c(countries[count.index], run, ot.temp, pt.temp))
    }
    
  }
  
  df <- as.data.frame(df)
  names(df) <- c('country', 'run', 'ot', 'pt')
  df$ot <- as.numeric(as.character(df$ot)); df$pt <- as.numeric(as.character(df$pt))
  df$pt[is.na(df$ot)] <- NA
  
  return(df)
}


check_realistic <- function(m) {
  
  df <- calc_metrics(m)
  is.onset = is.real = c()
  
  for (run in 1:length(unique(df$run))) {
    if (length(which(is.na(df$ot[df$run == run]))) <= 2) {
      is.onset <- c(is.onset, T)
      
      if (length(which(!(df$pt[df$run == run] %in% 52:64) & !is.na(df$pt[df$run == run]))) < 2) { # if fewer than 2 (so, 0 or 1) have PT outside the range
      # if (length(which(!(df$pt[df$run == run] %in% 50:66) & !is.na(df$pt[df$run == run]))) < 2) { # if fewer than 2 (so, 0 or 1) have PT outside the range
        # for full model, change to fewer than 4 (0, 1, 2, 3)
        is.real <- c(is.real, T)
      } else {
        is.real <- c(is.real, F)
      }
      
      # if (length(which(df$pt[df$run == run] %in% 52:64 & !is.na(df$pt[df$run == run]))) >= 11) {
      #   # for full model, change to greater than or equal to 17
      #   # actually, might want to make this 1 lower than the non-NAs, or else expectation might be too high
      #   is.real <- c(is.real, T)
      # } else {
      #   is.real <- c(is.real, F)
      # }
      
    } else {
      is.onset <- c(is.onset, F)
      is.real <- c(is.real, F)
    }
    
  }
  
  df$onset = df$real =  NA
  for (run in 1:length(is.onset)) {
    df$onset[df$run == run] <- is.onset[run]
    df$real[df$run == run] <- is.real[run]
  }
  
  return(list(df, is.onset, is.real))
}


attach_lat_long <- function(df, countries) {
  library(maps)
  data("world.cities")
  country.names <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
                     'Hungary', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
                     'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                     'United Kingdom')
  
  if (length(country.names) != length(countries)) {
    country.names <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy', 'Luxembourg',
                       'Netherlands', 'Poland', 'Slovakia', 'Spain')
  }
  
  world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
                                 world.cities$capital == 1, ]
  world.cities$country.etc <- factor(world.cities$country.etc)
  
  if (length(levels(world.cities$country.etc)) != length(countries)) {
    world.cities <- world.cities[world.cities$country.etc != 'UK', ]
    world.cities$country.etc <- factor(world.cities$country.etc)
  }
  
  levels(world.cities$country.etc) <- countries
  world.cities <- world.cities[, c('country.etc', 'lat', 'long')]
  df <- merge(df, world.cities, by.x = 'country', by.y = 'country.etc')
  return(df)
}

calculate_geo_patterns <- function(df) {
  
  # Get correlations and p-values:
  df$corr = df$pval = NA
  for (run in unique(df$run)) {
    corr.dat <- cor.test(df$long[df$run == run], df$pt[df$run == run], method = 'kendall')
    df$corr[df$run == run] <- corr.dat$estimate
    df$pval[df$run == run] <- corr.dat$p.value
  }
  
  # Categorize by pattern:
  df$pattern <- ifelse(df$corr > 0, 'westToEast', 'eastToWest')
  
  df$pattern2 <- df$pattern
  df$pattern2[df$pval > 0.05] <- 'noPatt'
  
  df$sig <- ifelse(df$pval < 0.05, 'yes', 'no')
  
  df$pattern <- factor(df$pattern)
  df$pattern2 <- factor(df$pattern2)
  df$sig <- factor(df$sig)
  
  df$patternSig <- paste(df$pattern, df$sig, sep = '_')
  df$patternSig <- factor(df$patternSig)
  # df$patternSig <- factor(df$patternSig, levels = levels(df$patternSig)[c(2, 1, 3, 4)])
  
  # Return:
  return(df)
}

# Sum preserving rounding function (from package "miceadds," which won't install on cluster)
# https://www.rdocumentation.org/packages/miceadds/versions/3.6-21/source
sumpreserving.rounding <- function (data, digits=0, preserve=TRUE )
  
{
  
  ism <- ( is.matrix(data) ) | ( is.data.frame(data) )
  
  if (ism) {
    
    DD <- ncol(data)
    
    err.r <- data.r <- matrix(0, nrow=nrow(data), ncol=DD)
    
    data.r[, 1] <- round(data[, 1], digits)
    
    err.r[, 1] <- data[, 1] - data.r[, 1]
    
    for (dd in 2:DD) {
      
      data.r[, dd] <- round(data[, dd] + rowSums(err.r[, 1:(dd - 1),drop=FALSE]), digits)
      
      err.r[, dd] <- data[, dd] - data.r[, dd]
      
    }
    
    if (is.data.frame(data)){
      
      data.r <- data.frame(data.r)
      
      colnames(data.r) <- colnames(data)
      
    }
    
    if ( ! preserve ){ data.r <- round( data, digits ) }
    
  } else {
    
    DD <- length(data)
    
    err.r <- data.r <- rep(0, DD)
    
    data.r[1] <- round(data[1], digits)
    
    err.r[1] <- data[1] - data.r[1]
    
    for (dd in 2:DD) {
      
      data.r[dd] <- round(data[dd] + sum(err.r[1:(dd - 1)]), digits)
      
      err.r[dd] <- data[dd] - data.r[dd]
      
    }
    
    if ( ! preserve ){ data.r <- round( data, digits ) }
    
  }
  
  return(data.r)
  
}

























