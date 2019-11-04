
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

run_model <- function(in.parms, in.ah, num.ens, n, N, tm.range, tmstep, tm.strt, tm.end, dt, pop.dat, s0.method = NULL) {
  
  # Store country-level S0:
  s0.by.count <- NULL
  
  # Get initial conditions:
  S0.temp = I0.temp = vector('list', num.ens)
  for (i in 1:num.ens) {
    S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
    
    if (s0.method == 'dist') {
      diag(S0.temp[[i]]) <- rnorm(n, mean = in.parms[1, i], sd = 0.05)
      diag(I0.temp[[i]]) <- in.parms[(1:n) + 1, i]
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
    S0.temp[[i]] <- t(S0.temp[[i]])
    S0.temp[[i]] <- S0.temp[[i]] * N
    
    I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
    I0.temp[[i]] <- I0.temp[[i]] * N
  }
  
  # Get parameters:
  in.parms <- in.parms[(dim(in.parms)[1] - 4):(dim(in.parms)[1]), ]
  
  print(dim(in.parms))
  
  beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
  AHpt <- in.ah[beta.range, ]
  AHpt <- as.matrix(AHpt, length(AHpt), n)
  b <- log(in.parms[3, ] - in.parms[4, ])
  a <- -180
  
  beta <- lapply(1:num.ens, function(ix) {
    (exp(a * AHpt + b[ix]) + in.parms[4, ix]) / in.parms[2, ix]
  })
  
  D.temp <- in.parms[2, ]; L.temp <- in.parms[1, ]; airScale.temp <- in.parms[5, ]
  
  # Run model:
  m <- sapply(1:num.ens, function(ix) {
    propagateToySIRS(tm_strt = tm.strt, tm_end = tm.end, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     airScale = airScale.temp[ix], realdata = TRUE,
                     prohibAir = FALSE)
  })
  
  # Re-format results:
  res.list <- format_model_results(m, num.ens, tmstep, tm.strt, tm.end, n, pop.dat)
  res.list <- list(res.list, s0.by.count)
  
  # Return results:
  return(res.list)
  
}

### Analysis ###

calc_metrics <- function(m) {
  
  df <- NULL
  
  for (run in 1:length(m)) {
    out.temp <- m[[run]]
    
    for (count.index in 1:20) {
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
      
      if (length(which(df$pt[df$run == run] %in% 52:64 & !is.na(df$pt[df$run == run]))) >= 17) {
        is.real <- c(is.real, T)
      } else {
        is.real <- c(is.real, F)
      }
      
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
  world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
                                 world.cities$capital == 1, ]
  world.cities$country.etc <- factor(world.cities$country.etc)
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




























