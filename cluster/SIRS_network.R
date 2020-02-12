
# Note: During day, we care about where people work (columns). During night, we care
# about where they live (rows).

allocate_travelers <- function(ntrav, St, It, Rt, Nt, randtrav, n) {
  ### Function to allocate a set integer number of travelers proportionally among S, I, and R compartments,
  ### while maintaining the correct total and avoiding rounding errors
  
  s.out.frac <- St / Nt; i.out.frac <- It / Nt; r.out.frac <- Rt / Nt
  r.out.frac <- r.out.frac + i.out.frac + s.out.frac
  i.out.frac <- i.out.frac + s.out.frac
  
  s.out.frac[is.na(s.out.frac)] <- 0
  i.out.frac[is.na(i.out.frac)] <- 0
  r.out.frac[is.na(r.out.frac)] <- 0
  
  s.in.frac <- matrix(((colSums(St) / colSums(Nt)) %*% randtrav) / colSums(randtrav), nrow = n, ncol = n, byrow = T)
  i.in.frac <- matrix(((colSums(It) / colSums(Nt)) %*% randtrav) / colSums(randtrav), nrow = n, ncol = n, byrow = T)
  r.in.frac <- matrix(((colSums(Rt) / colSums(Nt)) %*% randtrav) / colSums(randtrav), nrow = n, ncol = n, byrow = T)
  
  r.in.frac <- r.in.frac + i.in.frac + s.in.frac
  i.in.frac <- i.in.frac + s.in.frac
  
  s.out = i.out = r.out = s.in = i.in = r.in = matrix(NA, nrow = n, ncol = n)
  
  for (irow in 1:dim(ntrav)[1]) {
    for (jcol in 1:dim(ntrav)[2]) {
      n.trav.unif <- sort(runif(ntrav[irow, jcol], min = 0, max = 1))
      length(n.trav.unif)
      
      s.out[irow, jcol] <- length(n.trav.unif[n.trav.unif <= s.out.frac[irow, jcol]])
      i.out[irow, jcol] <- length(n.trav.unif[n.trav.unif <= i.out.frac[irow, jcol] & n.trav.unif > s.out.frac[irow, jcol]])
      r.out[irow, jcol] <- length(n.trav.unif[n.trav.unif <= r.out.frac[irow, jcol] & n.trav.unif > i.out.frac[irow, jcol]])
      # r.out[irow, jcol] <- length(n.trav.unif[n.trav.unif <= 1.0 & n.trav.unif > i.out.frac[irow, jcol]])
      
      # print(paste(irow, jcol, sep = '_'))
      # print(all.equal(ntrav[irow, jcol], s.out[irow, jcol] + i.out[irow, jcol] + r.out[irow, jcol]))
      if (!all.equal(ntrav[irow, jcol], s.out[irow, jcol] + i.out[irow, jcol] + r.out[irow, jcol])) {
        print('Error - Out')
      }
      
      s.in[irow, jcol] <- length(n.trav.unif[n.trav.unif <= s.in.frac[irow, jcol]])
      i.in[irow, jcol] <- length(n.trav.unif[n.trav.unif <= i.in.frac[irow, jcol] & n.trav.unif > s.in.frac[irow, jcol]])
      r.in[irow, jcol] <- length(n.trav.unif[n.trav.unif <= r.in.frac[irow, jcol] & n.trav.unif > i.in.frac[irow, jcol]])
      # r.in[irow, jcol] <- length(n.trav.unif[n.trav.unif <= 1.0 & n.trav.unif > i.in.frac[irow, jcol]])
      
      if (!all.equal(ntrav[irow, jcol], s.in[irow, jcol] + i.in[irow, jcol] + r.in[irow, jcol])) {
        print('Error - In')
      }
      
    }
  }
  
  return(list(s.out, i.out, r.out, s.in, i.in, r.in))
}

propagateToySIRS <- function(tm_strt, tm_end, tm_step, S0, I0, N, D, L, beta, airScale, realdata = FALSE, prohibAir = FALSE) {
  cnt <- 1
  
  tm_strt <- tm_strt - tm.range[1] + 1 # adjust the index to match beta
  tm_end <- tm_end - tm.range[1] + 1
  
  tm_vec <- seq(tm_strt, tm_end, by = tm_step)
  tm_sz <- length(tm_vec) + 1 # include initial conditions
  
  S.list = I.list = newI.list = R.list = N.list = vector('list', tm_sz)
  S.list[[1]] <- S0; I.list[[1]] <- I0; R.list[[1]] <- N - S0 - I0
  N.list[[1]] <- N
  newI.list[[1]] <- matrix(0, nrow = n, ncol = n)
  
  # Set up times by month:
  t.jan.orig <- c(1:31); t.feb.orig <- c(32:59); t.mar.orig <- c(60:90); t.apr.orig <- c(91:120); t.may.orig <- c(121:151); t.jun.orig <- c(152:181);
  t.jul.orig <- c(182:212); t.aug.orig <- c(213:243); t.sep.orig <- c(244:273); t.oct.orig <- c(274:304); t.nov.orig <- c(305:334); t.dec.orig <- c(335:365)
  
  t.jan <- c(1:31); t.feb <- c(32:59); t.mar <- c(60:90); t.apr <- c(91:120); t.may <- c(121:151); t.jun <- c(152:181);
  t.jul <- c(182:212); t.aug <- c(213:243); t.sep <- c(244:273); t.oct <- c(274:304); t.nov <- c(305:334); t.dec <- c(335:365)
  
  for (yr.later in seq(365, by = 365, length.out = 21)) {
    t.jan <- c(t.jan, t.jan.orig + yr.later)
    t.feb <- c(t.feb, t.feb.orig + yr.later)
    t.mar <- c(t.mar, t.mar.orig + yr.later)
    t.apr <- c(t.apr, t.apr.orig + yr.later)
    t.may <- c(t.may, t.may.orig + yr.later)
    t.jun <- c(t.jun, t.jun.orig + yr.later)
    t.jul <- c(t.jul, t.jul.orig + yr.later)
    t.aug <- c(t.aug, t.aug.orig + yr.later)
    t.sep <- c(t.sep, t.sep.orig + yr.later)
    t.oct <- c(t.oct, t.oct.orig + yr.later)
    t.nov <- c(t.nov, t.nov.orig + yr.later)
    t.dec <- c(t.dec, t.dec.orig + yr.later)
  }
  
  # run continuously
  for (t in tm_vec) {
    
    # First, choose t.rand by month:
    t.true <- t + tm.range[1] - 1
    if (t.true %in% t.jan) {
      # print('January')
      load('formatTravelData/formattedData/air_1_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.feb) {
      # print('February')
      load('formatTravelData/formattedData/air_2_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.mar) {
      # print('March')
      load('formatTravelData/formattedData/air_3_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.apr) {
      # print('April')
      load('formatTravelData/formattedData/air_4_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.may) {
      # print('May')
      load('formatTravelData/formattedData/air_5_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.jun) {
      # print('June')
      load('formatTravelData/formattedData/air_6_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.jul) {
      # print('July')
      load('formatTravelData/formattedData/air_7_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.aug) {
      # print('August')
      load('formatTravelData/formattedData/air_8_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.sep) {
      # print('September')
      load('formatTravelData/formattedData/air_9_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.oct) {
      # print('October')
      load('formatTravelData/formattedData/air_10_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.nov) {
      # print('November')
      load('formatTravelData/formattedData/air_11_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.dec) {
      # print('December')
      load('formatTravelData/formattedData/air_12_01-31.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else {
      print('ERROR: Out of seasonal range!')
    }
    
    # Remove air travel:
    if (prohibAir) {
      a.temp <- a.temp * 0
    }
    
    # Add "random" travel data:
    all.rand <- airScale * a.temp
    
    # Run model forward:
    cnt <- cnt + 1
    
    # set t-1 S, I, newI:
    S <- S.list[[cnt - 1]]; I <- I.list[[cnt - 1]]; newI <- newI.list[[cnt - 1]]
    
    if (!exists('discrete')) {
      discrete <- FALSE
    }
    
    if (discrete) { # run stochastically
      N <- round(N, 0) # N needs to be whole numbers, then!
      
      S <- round(S, 0); I <- round(I, 0)
      R <- N - S - I
      
      if (cnt == 2) {
        S.list[[cnt - 1]] <- S
        I.list[[cnt - 1]] <- I
        R.list[[cnt - 1]] <- R
      } # shouldn't be an issue after this
      
      ### Daytime ###
      # during the daytime, N.d describes how many people are in each compartment (sum over each COLUMN); during the nighttime, it's N.s (sum over each ROW)
      
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (R))
      Einf <- sweep(sweep(sweep(S, 2, colSums(I), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * I)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      ######################################################################################################################################################
      # Now incorporate travel:
      # In order to keep population sizes constant, need to first calculate the TOTAL travelers in and out, then allocate them by compartment
      n.out <- sweep(sweep(N, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      n.in <- matrix((colSums(N) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = TRUE))
      print(all.equal(n.out, n.in))
      # now we have how many people are traveling in and out of each compartment total, rpois on that; allocate by what percentage of people are S, I, R
      # and obviously n.out and n.in have to be the same; so it doesn't make sense just to allocate by proportion S/I/R...
      # allocating this way for out is correct; but not for in, because that relates to the proportion S/I/R in all the incoming compartments
      # maybe go ahead and incorporate the divide by 3 for daytime here, so it doesn't cause issues later?
      
      # Check:
      s.out <- (S / N) * n.out; s.out[is.na(s.out)] <- 0
      i.out <- (I / N) * n.out; i.out[is.na(i.out)] <- 0
      r.out <- (R / N) * n.out; r.out[is.na(r.out)] <- 0
      
      s.out.old <- sweep(sweep(S, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      i.out.old <- sweep(sweep(I, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      r.out.old <- sweep(sweep(R, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      
      print(all.equal(s.out, s.out.old))
      print(all.equal(i.out, i.out.old))
      print(all.equal(r.out, r.out.old))
      
      s.in <- matrix(((colSums(S) / colSums(N)) %*% all.rand) / colSums(all.rand), nrow = n, ncol = n, byrow = T) * n.in
      i.in <- matrix(((colSums(I) / colSums(N)) %*% all.rand) / colSums(all.rand), nrow = n, ncol = n, byrow = T) * n.in
      r.in <- matrix(((colSums(R) / colSums(N)) %*% all.rand) / colSums(all.rand), nrow = n, ncol = n, byrow = T) * n.in
      
      s.in.old <- matrix((colSums(S) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      i.in.old <- matrix((colSums(I) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      r.in.old <- matrix((colSums(R) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      
      print(all.equal(s.in, s.in.old))
      print(all.equal(i.in, i.in.old))
      print(all.equal(r.in, r.in.old))
      print('OLD CHECK OVER')
      
      ######################################################################################################################################################
      # # Now for real:
      # code R in explicitly instead of using N-S-I - then I think we could set r.in/r.out as those traveling but not in s.in/i.in/s.out/i.out
      # if we do this, the above could probably be re-simplified
      # or can we somehow draw from the integer of people in n.trav.rand randomly?
      # although I'm hesitant to add this much complexity to an already-slow model
      
      n.trav <- (tm_step * (1 / 3)) * sweep(sweep(N, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      n.trav.rand <- rpois(length(n.trav), n.trav); dim(n.trav.rand) <- dim(n.trav)
      
      curr.trav <- allocate_travelers(n.trav.rand, S, I, R, N, all.rand, n)
      s.out <- curr.trav[[1]]; i.out <- curr.trav[[2]]; r.out <- curr.trav[[3]]
      s.in <- curr.trav[[4]]; i.in <- curr.trav[[5]]; r.in <- curr.trav[[6]]
      
      print(all.equal(s.out + i.out + r.out, n.trav.rand))
      print(all.equal(s.in + i.in + r.in, n.trav.rand))
      
      Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
      Eitrav <- i.in - i.out
      Ertrav <- r.in - r.out
      
      print(all(Estrav + Eitrav + Ertrav == 0))
      
      sk1 <- smcl - smci + Estrav
      ik1 <- smci - smcr + Eitrav
      ik1a <- smci# + (tm_step * (1 / 3) * i.in) # another option would be to count incoming by HOME country, and not by individual compartment?
      rk1 <- smcr - smcl + Ertrav
      print(all(sk1+ik1+rk1==0)) # so it really is the divide by 2 that jacks it up
      
      # ONLY count those who are newly infected; i.in describes people who are already infected, and are traveling to a country; they are not NEWLY infected, so don't double-count
      Ts1 <- S.list[[cnt - 1]] + round(sk1 / 2, 0)
      Ti1 <- I.list[[cnt - 1]] + round(ik1 / 2, 0)
      # Tr1 <- R.list[[cnt - 1]] + round(rk1 / 2, 0)
      Tr1 <- N - Ts1 - Ti1
      # this rounding might temporarily mess up population stability...
      # in the end, we might need to incorporate R explicitly...
      print(all.equal(Ts1 + Ti1 + Tr1, N))
      
      # STEP 2
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (Tr1))
      Einf <- sweep(sweep(sweep(Ts1, 2, colSums(Ti1), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti1)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      n.trav <- (tm_step * (1 / 3)) * sweep(sweep(N, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      n.trav.rand <- rpois(length(n.trav), n.trav); dim(n.trav.rand) <- dim(n.trav)
      
      curr.trav <- allocate_travelers(n.trav.rand, Ts1, Ti1, Tr1, N, all.rand, n)
      s.out <- curr.trav[[1]]; i.out <- curr.trav[[2]]; r.out <- curr.trav[[3]]
      s.in <- curr.trav[[4]]; i.in <- curr.trav[[5]]; r.in <- curr.trav[[6]]
      
      print(all.equal(s.out + i.out + r.out, n.trav.rand))
      print(all.equal(s.in + i.in + r.in, n.trav.rand))
      
      Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
      Eitrav <- i.in - i.out
      Ertrav <- r.in - r.out
      
      print(all(Estrav + Eitrav + Ertrav == 0))
      
      sk2 <- smcl - smci + Estrav
      ik2 <- smci - smcr + Eitrav
      ik2a <- smci# + (tm_step * (1 / 3) * i.in)
      rk2 <- smcr - smcl + Ertrav
      
      Ts2 <- S.list[[cnt - 1]] + round(sk2 / 2, 0)
      Ti2 <- I.list[[cnt - 1]] + round(ik2 / 2, 0)
      # Tr2 <- R.list[[cnt - 1]] + round(rk2 / 2, 0)
      Tr2 <- N - Ts2 - Ti2
      print(all.equal(Ts2 + Ti2 + Tr2, N))
      
      # STEP 3
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (Tr2))
      Einf <- sweep(sweep(sweep(Ts2, 2, colSums(Ti2), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti2)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      n.trav <- (tm_step * (1 / 3)) * sweep(sweep(N, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      n.trav.rand <- rpois(length(n.trav), n.trav); dim(n.trav.rand) <- dim(n.trav)
      
      curr.trav <- allocate_travelers(n.trav.rand, Ts2, Ti2, Tr2, N, all.rand, n)
      s.out <- curr.trav[[1]]; i.out <- curr.trav[[2]]; r.out <- curr.trav[[3]]
      s.in <- curr.trav[[4]]; i.in <- curr.trav[[5]]; r.in <- curr.trav[[6]]
      
      print(all.equal(s.out + i.out + r.out, n.trav.rand))
      print(all.equal(s.in + i.in + r.in, n.trav.rand))
      
      Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
      Eitrav <- i.in - i.out
      Ertrav <- r.in - r.out
      
      print(all(Estrav + Eitrav + Ertrav == 0))
      
      sk3 <- smcl - smci + Estrav
      ik3 <- smci - smcr + Eitrav
      ik3a <- smci# + (tm_step * (1 / 3) * i.in)
      rk3 <- smcr - smcl + Ertrav
      
      Ts3 <- S.list[[cnt - 1]] + round(sk3, 0)# / 2
      Ti3 <- I.list[[cnt - 1]] + round(ik3, 0)# / 2
      # Tr3 <- R.list[[cnt - 1]] + round(rk3 / 2, 0)
      Tr3 <- N - Ts3 - Ti3
      
      # STEP 4
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (Tr3))
      Einf <- sweep(sweep(sweep(Ts3, 2, colSums(Ti3), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti3)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      n.trav <- (tm_step * (1 / 3)) * sweep(sweep(N, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      n.trav.rand <- rpois(length(n.trav), n.trav); dim(n.trav.rand) <- dim(n.trav)
      
      curr.trav <- allocate_travelers(n.trav.rand, Ts3, Ti3, Tr3, N, all.rand, n)
      s.out <- curr.trav[[1]]; i.out <- curr.trav[[2]]; r.out <- curr.trav[[3]]
      s.in <- curr.trav[[4]]; i.in <- curr.trav[[5]]; r.in <- curr.trav[[6]]
      
      print(all.equal(s.out + i.out + r.out, n.trav.rand))
      print(all.equal(s.in + i.in + r.in, n.trav.rand))
      
      Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
      Eitrav <- i.in - i.out
      Ertrav <- r.in - r.out
      
      print(all(Estrav + Eitrav + Ertrav == 0))
      
      sk4 <- smcl - smci + Estrav
      ik4 <- smci - smcr + Eitrav
      ik4a <- smci# + (tm_step * (1 / 3) * i.in)
      rk4 <- smcr - smcl + Ertrav
      
      # seed <- 0.1 # Do seeding at end, if any - not twice a day
      S <- S + round(sk1/6 + sk2/3 + sk3/3 + sk4/6, 0)# - seed
      I <- I + round(ik1/6 + ik2/3 + ik3/3 + ik4/6, 0)# + seed
      newI <- newI + round(ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6, 0)# + seed
      # R <- R + round(rk1/6 + rk2/3 + rk3/3 + rk4/6, 0)# + seed
      R <- N - S - I
      print(all.equal(N, S + I + R))
      
      # This might be about the most we can do - it at least let's us assume R is N-S-I, even though tracking R explicitly doesn't allow for things to remain
      # constant (it almost does, though, it's just the last rounding)
      # At this point I might also be able to return to a "simpler" version of allocating s.out/s.in and i.out/i.in - we can assume that r.out/r.in are just
      # whichever of the n.trav travelers who are left - I don't think this plays a role anywhere else
      # Or maybe it's good to impose a little more randomness in how many travelers are S/I/R?
      # This is still better than before, where Estrav and Eitrav were drawn separately from rpois, and wouldn't necessarily agree with an unchanging N
      
      # print(Eimmloss)
      # print(Einf)
      # print(Estrav)
      # print(S)
      # print(''); print(''); print('')
      
      ### Nighttime ###
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - S - I))
      Einf <- sweep(sweep(sweep(S, 1, rowSums(I), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * I)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      # incorporate travel:
      n.trav <- (tm_step * (2 / 3)) * sweep(sweep(N, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # n.trav <- sweep(sweep(N, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      n.trav.rand <- rpois(length(n.trav), n.trav); dim(n.trav.rand) <- dim(n.trav)
      
      # ### old check:
      # s.out.old <- sweep(sweep(S, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # i.out.old <- sweep(sweep(I, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # 
      # s.out <- (S / N) * n.trav; s.out[is.na(s.out)] <- 0
      # i.out <- (I / N) * n.trav; i.out[is.na(i.out)] <- 0
      # 
      # print(all.equal(s.out, s.out.old))
      # print(all.equal(i.out, i.out.old))
      # 
      # s.in.old <- matrix((rowSums(S) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
      #   (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      # i.in.old <- matrix((rowSums(I) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
      #   (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      # 
      # s.in <- matrix(((rowSums(S) / rowSums(N)) %*% all.rand) / rowSums(all.rand), nrow = n, ncol = n, byrow = F) * n.trav
      # i.in <- matrix(((rowSums(I) / rowSums(N)) %*% all.rand) / rowSums(all.rand), nrow = n, ncol = n, byrow = F) * n.trav
      # 
      # print(all.equal(s.in, s.in.old))
      # print(all.equal(i.in, i.in.old))
      
      s.out <- (S / N) * n.trav.rand; s.out[is.na(s.out)] <- 0
      i.out <- (I / N) * n.trav.rand; i.out[is.na(i.out)] <- 0
      r.out <- (R / N) * n.trav.rand; r.out[is.na(r.out)] <- 0
      
      s.in <- matrix(((rowSums(S) / rowSums(N)) %*% all.rand) / rowSums(all.rand), nrow = n, ncol = n, byrow = F) * n.trav.rand
      i.in <- matrix(((rowSums(I) / rowSums(N)) %*% all.rand) / rowSums(all.rand), nrow = n, ncol = n, byrow = F) * n.trav.rand
      r.in <- matrix(((rowSums(R) / rowSums(N)) %*% all.rand) / rowSums(all.rand), nrow = n, ncol = n, byrow = F) * n.trav.rand
      
      Estrav <- round(s.in - s.out, 0)
      Eitrav <- round(i.in - i.out, 0)
      # Ertrav.calc <- round(r.in - r.out, 0)
      # print(all(Estrav + Eitrav + Ertrav.calc == 0))
      # print(Estrav + Eitrav + Ertrav.calc)
      Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
      # print(all.equal(Ertrav, Ertrav.calc)) # but this holds up!
      print(all(Estrav + Eitrav + Ertrav == 0))
      # print(Estrav + Eitrav + Ertrav)
      # this seems just as valid - just take any extra and use R to deal with it
      
      # # HALT: Is all of this also allocated the same?
      # curr.trav <- allocate_travelers(n.trav.rand, S, I, R, N, all.rand, n)
      # s.out <- curr.trav[[1]]; i.out <- curr.trav[[2]]; r.out <- curr.trav[[3]]
      # s.in <- curr.trav[[4]]; i.in <- curr.trav[[5]]; r.in <- curr.trav[[6]]
      # 
      # print(all.equal(s.out + i.out + r.out, n.trav.rand))
      # print(all.equal(s.in + i.in + r.in, n.trav.rand))
      # 
      # Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
      # Eitrav <- i.in - i.out
      # Ertrav <- r.in - r.out
      # 
      # print(all(Estrav + Eitrav + Ertrav == 0))
      
      # # Just check, but I imagine the old version wouldn't have added up properly:
      # s.out <- sweep(sweep(S, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # i.out <- sweep(sweep(I, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # r.out <- sweep(sweep(R, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      # 
      # s.in <- matrix((rowSums(S) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
      #   (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      # i.in <- matrix((rowSums(I) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
      #   (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      # r.in <- matrix((rowSums(R) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
      #   (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      # 
      # Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
      # Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
      # Ertrav <- (tm_step * (2 / 3)) * (r.in - r.out)
      # 
      # print(Estrav + Eitrav + Ertrav) # almost zero at least
      # 
      # Estrav <- apply(Estrav, 1:2, function(ix) {
      #   rpois(1, abs(ix)) * sign(ix)
      # })
      # Eitrav <- apply(Eitrav, 1:2, function(ix) {
      #   rpois(1, abs(ix)) * sign(ix)
      # })
      # Ertrav <- apply(Ertrav, 1:2, function(ix) {
      #   rpois(1, abs(ix)) * sign(ix)
      # })
      # 
      # print(Estrav + Eitrav + Ertrav) # now there are 10s and even >100 people moving into certain compartments
      
      sk1 <- smcl - smci + Estrav
      ik1 <- smci - smcr + Eitrav
      ik1a <- smci# + (tm_step * (2 / 3) * i.in)
      rk1 <- smcr - smcl + Ertrav
      
      Ts1 <- S + round(sk1 / 2, 0)
      Ti1 <- I + round(ik1 / 2, 0)
      # Tr1 <- R + round(rk1 / 2, 0)
      Tr1 <- N - Ts1 - Ti1
      
      # STEP 2
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts1 - Ti1))
      Einf <- sweep(sweep(sweep(Ts1, 1, rowSums(Ti1), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti1)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      
      
      
      
      
      
      
      
      
      
      
      
      sk2=smcl-smci+Estrav
      ik2=smci-smcr+Eitrav
      ik2a=smci#+(tm_step * (2 / 3) * i.in)
      rk2 <- smcr - smcl + Ertrav
      
      Ts2 <- S + round(sk2 / 2, 0)
      Ti2 <- I + round(ik2 / 2, 0)
      # Tr2 <- R + round(rk2 / 2, 0)
      Tr2 <- N - Ts2 - Ti2
      
      # STEP 3
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts2 - Ti2))
      Einf <- sweep(sweep(sweep(Ts2, 1, rowSums(Ti2), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti2)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      
      
      
      
      
      
      
      
      
      sk3=smcl-smci+Estrav
      ik3=smci-smcr+Eitrav
      ik3a=smci#+(tm_step * (2 / 3) * i.in)
      rk3 <- smcr - smcl + Ertrav
      
      Ts3 <- S + round(sk3, 0)# / 2
      Ti3 <- I + round(ik3, 0)# / 2
      # Tr3 <- R + round(rk3, 0)# / 2
      Tr3 <- N - Ts3 - Ti3
      
      # STEP 4
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts3 - Ti3))
      Einf <- sweep(sweep(sweep(Ts3, 1, rowSums(Ti3), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti3)
      
      smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
      smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
      smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
      
      
      
      
      
      
      
      
      
      sk4=smcl-smci+Estrav
      ik4=smci-smcr+Eitrav
      ik4a=smci#+(tm_step * (2 / 3) * i.in)
      rk4 <- smcr - smcl + Ertrav
      
      # add continuous seeding?
      seed <- diag(rpois(n, 0.1), n, n) # here seeding only in home-home compartments; 10% chance of a single new infection
      
      S.list[[cnt]] <- S + round(sk1/6 + sk2/3 + sk3/3 + sk4/6, 0) - seed
      I.list[[cnt]] <- I + round(ik1/6 + ik2/3 + ik3/3 + ik4/6, 0) + seed
      newI.list[[cnt]] <- newI + round(ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6, 0) + seed
      # R.list[[cnt]] <- R + round(rk1/6 + rk2/3 + rk3/3 + rk4/6, 0)# + seed
      R.list[[cnt]] <- N - S.list[[cnt]] - I.list[[cnt]]
      
    } else { # run continuously/deterministically
      
      ### Daytime ###
      # during the daytime, N.d describes how many people are in each compartment (sum over each COLUMN); during the nighttime, it's N.s (sum over each ROW)
      
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - S - I))
      # daytime, so we want col1 multiplied by beta1, etc.
      Einf <- sweep(sweep(sweep(S, 2, colSums(I), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      # Einf=tm_step/2*(tnetcorr%*%(beta[t,]*(netcorr%*%I[cnt-1,])/N_hat)*S[cnt-1,]) # corrected
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * I)
      
      # Einf.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     Einf.check[i, j] <- (tm_step * (1 / 3)) * (beta[t, j] * S[i, j] * sum(I[, j])) / sum(N[, j])
      #   }
      # }
      # colnames(Einf.check) = rownames(Einf.check) = countries
      # print(all.equal(Einf, Einf.check))
      
      # Now incorporate travel:
      s.out <- sweep(sweep(S, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      i.out <- sweep(sweep(I, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      
      # s.out.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     s.out.check[i, j] <- (S[i, j] / sum(N[, j])) * sum(all.rand[j, ])
      #   }
      # }
      # colnames(s.out.check) = rownames(s.out.check) = countries
      # print(all.equal(s.out, s.out.check))
      
      s.in <- matrix((colSums(S) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      i.in <- matrix((colSums(I) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      # QUESTION: Is it a problem to do the infection and travel steps separately? Should they be sequential or something?
      # QUESTION: "all.equal" is true, but "==" is false?
      
      # s.in.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     s.in.temp = 0
      #     for (k in 1:3) { # country people are flying in FROM
      #       for (h in 1:3) { # home (day) / work (night) place of those in country being traveled from
      #         s.in.temp <- s.in.temp + (N[i, j] / sum(N[, j])) * all.rand[k, j] * S[h, k] / sum(N[, k])
      #         # s.in.temp <- s.in.temp + (N[i, j] / sum(N[i, ])) * all.rand[k, i] * S[k, h] / sum(N[k, ])
      #       }
      #     }
      #     s.in.check[i, j] <- s.in.temp
      #   }
      # }
      # colnames(s.in.check) = rownames(s.in.check) = countries
      # print(all.equal(s.in, s.in.check))
      
      Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
      
      # print(sum(Estrav))
      # print(sum(Eitrav))
      
      Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
      Einf[Einf < 0] <- 0
      Erecov[Erecov < 0] <- 0
      
      smcl <- Eimmloss
      smci <- Einf
      smcr <- Erecov
      
      sk1 <- smcl - smci + Estrav
      ik1 <- smci - smcr + Eitrav
      ik1a <- smci# + (tm_step * (1 / 3) * i.in) # another option would be to count incoming by HOME country, and not by individual compartment?
      # ONLY count those who are newly infected; i.in describes people who are already infected, and are traveling to a country; they are not NEWLY infected, so don't double-count
      Ts1 <- S.list[[cnt - 1]] + sk1 / 2
      Ti1 <- I.list[[cnt - 1]] + ik1 / 2
      
      # print(Eimmloss)
      # print(Einf)
      # print(Estrav)
      # print(Ts1)
      # print('')
      
      # STEP 2
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts1 - Ti1))
      Einf <- sweep(sweep(sweep(Ts1, 2, colSums(Ti1), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti1)
      
      s.out <- sweep(sweep(Ts1, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti1, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
      s.in <- matrix((colSums(Ts1) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      i.in <- matrix((colSums(Ti1) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      
      Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
      Einf[Einf < 0] <- 0
      Erecov[Erecov < 0] <- 0
      
      smcl <- Eimmloss
      smci <- Einf
      smcr <- Erecov
      
      sk2 <- smcl - smci + Estrav
      ik2 <- smci - smcr + Eitrav
      ik2a <- smci# + (tm_step * (1 / 3) * i.in)
      
      Ts2 <- S.list[[cnt - 1]] + sk2 / 2
      Ti2 <- I.list[[cnt - 1]] + ik2 / 2
      
      # STEP 3
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts2 - Ti2))
      Einf <- sweep(sweep(sweep(Ts2, 2, colSums(Ti2), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti2)
      
      s.out <- sweep(sweep(Ts2, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti2, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
      s.in <- matrix((colSums(Ts2) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      i.in <- matrix((colSums(Ti2) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      
      Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
      Einf[Einf < 0] <- 0
      Erecov[Erecov < 0] <- 0
      
      smcl <- Eimmloss
      smci <- Einf
      smcr <- Erecov
      
      sk3 <- smcl - smci + Estrav
      ik3 <- smci - smcr + Eitrav
      ik3a <- smci# + (tm_step * (1 / 3) * i.in)
      
      Ts3 <- S.list[[cnt - 1]] + sk3# / 2
      Ti3 <- I.list[[cnt - 1]] + ik3# / 2
      
      # STEP 4
      Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts3 - Ti3))
      Einf <- sweep(sweep(sweep(Ts3, 2, colSums(Ti3), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                    2, 1 / colSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti3)
      
      s.out <- sweep(sweep(Ts3, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti3, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
      s.in <- matrix((colSums(Ts3) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      i.in <- matrix((colSums(Ti3) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
        (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
      
      Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
      Einf[Einf < 0] <- 0
      Erecov[Erecov < 0] <- 0
      
      smcl <- Eimmloss
      smci <- Einf
      smcr <- Erecov
      
      sk4 <- smcl - smci + Estrav
      ik4 <- smci - smcr + Eitrav
      ik4a <- smci# + (tm_step * (1 / 3) * i.in)
      
      # seed <- 0.1 # Do seeding at end, if any - not twice a day
      S <- S + sk1/6 + sk2/3 + sk3/3 + sk4/6# - seed
      I <- I + ik1/6 + ik2/3 + ik3/3 + ik4/6# + seed
      newI <- newI + ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6# + seed
      
      # print(Eimmloss)
      # print(Einf)
      # print(Estrav)
      # print(S)
      # print(''); print(''); print('')
      
      ### Nighttime ###
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - S - I))
      Einf <- sweep(sweep(sweep(S, 1, rowSums(I), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * I)
      
      # Einf.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     Einf.check[i, j] <- (tm_step * (2 / 3)) * (beta[t, i] * S[i, j] * sum(I[i, ])) / sum(N[i, ])
      #   }
      # }
      # colnames(Einf.check) = rownames(Einf.check) = countries
      # print(all.equal(Einf, Einf.check))
      
      # Now incorporate travel:
      s.out <- sweep(sweep(S, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      i.out <- sweep(sweep(I, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      
      # s.out.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     s.out.check[i, j] <- (S[i, j] / sum(N[i, ])) * sum(all.rand[i, ])
      #   }
      # }
      # colnames(s.out.check) = rownames(s.out.check) = countries
      # print(all.equal(s.out, s.out.check))
      
      s.in <- matrix((rowSums(S) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      i.in <- matrix((rowSums(I) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      
      # s.in.check <- matrix(0, 3, 3)
      # for (i in 1:3) {
      #   for (j in 1:3) {
      #     # i is home, j is work
      #     s.in.temp = 0
      #     for (k in 1:3) { # country people are flying in FROM
      #       for (h in 1:3) { # home (day) / work (night) place of those in country being traveled from
      #         # s.in.temp <- s.in.temp + (N[i, j] / sum(N[, j])) * all.rand[k, j] * S[h, k] / sum(N[, k])
      #         s.in.temp <- s.in.temp + (N[i, j] / sum(N[i, ])) * all.rand[k, i] * S[k, h] / sum(N[k, ])
      #       }
      #     }
      #     s.in.check[i, j] <- s.in.temp
      #   }
      # }
      # colnames(s.in.check) = rownames(s.in.check) = countries
      # print(all.equal(s.in, s.in.check))
      
      # print(s.in); print(''); print(''); print('')
      
      Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
      Einf[Einf < 0] <- 0
      Erecov[Erecov < 0] <- 0
      
      smcl <- Eimmloss
      smci <- Einf
      smcr <- Erecov
      
      sk1 <- smcl - smci + Estrav
      ik1 <- smci - smcr + Eitrav
      ik1a <- smci# + (tm_step * (2 / 3) * i.in)
      
      Ts1 <- S + sk1 / 2
      Ti1 <- I + ik1 / 2
      
      # print(Eimmloss)
      # print(Einf)
      # print(Estrav)
      # print(Ts1)
      # print(''); print(''); print('')
      
      # STEP 2
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts1 - Ti1))
      Einf <- sweep(sweep(sweep(Ts1, 1, rowSums(Ti1), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti1)
      
      s.out <- sweep(sweep(Ts1, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti1, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      s.in <- matrix((rowSums(Ts1) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      i.in <- matrix((rowSums(Ti1) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      
      Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
      Einf[Einf<0]=0
      Erecov[Erecov<0]=0
      
      smcl=Eimmloss
      smci=Einf
      smcr=Erecov
      
      sk2=smcl-smci+Estrav
      ik2=smci-smcr+Eitrav
      ik2a=smci#+(tm_step * (2 / 3) * i.in)
      
      Ts2 <- S + sk2 / 2
      Ti2 <- I + ik2 / 2
      
      # STEP 3
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts2 - Ti2))
      Einf <- sweep(sweep(sweep(Ts2, 1, rowSums(Ti2), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti2)
      
      s.out <- sweep(sweep(Ts2, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti2, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      s.in <- matrix((rowSums(Ts2) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      i.in <- matrix((rowSums(Ti2) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      
      Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
      Einf[Einf<0]=0
      Erecov[Erecov<0]=0
      
      smcl=Eimmloss
      smci=Einf
      smcr=Erecov
      
      sk3=smcl-smci+Estrav
      ik3=smci-smcr+Eitrav
      ik3a=smci#+(tm_step * (2 / 3) * i.in)
      
      Ts3 <- S + sk3# / 2
      Ti3 <- I + ik3# / 2
      
      # STEP 4
      Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts3 - Ti3))
      Einf <- sweep(sweep(sweep(Ts3, 1, rowSums(Ti3), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                    1, 1 / rowSums(N), '*')
      Einf[is.na(Einf)] <- 0
      Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti3)
      
      s.out <- sweep(sweep(Ts3, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      i.out <- sweep(sweep(Ti3, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
      s.in <- matrix((rowSums(Ts3) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      i.in <- matrix((rowSums(Ti3) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
        (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
      
      Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
      Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
      
      Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
      Einf[Einf<0]=0
      Erecov[Erecov<0]=0
      
      smcl=Eimmloss
      smci=Einf
      smcr=Erecov
      
      sk4=smcl-smci+Estrav
      ik4=smci-smcr+Eitrav
      ik4a=smci#+(tm_step * (2 / 3) * i.in)
      
      S.list[[cnt]] <- S + sk1/6 + sk2/3 + sk3/3 + sk4/6# - seed
      I.list[[cnt]] <- I + ik1/6 + ik2/3 + ik3/3 + ik4/6# + seed
      newI.list[[cnt]] <- newI + ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6# + seed
      
    }
    
    
  }
  
  temp.S <- lapply(1:length(S.list), function(ix) {
    as.vector(t(S.list[[ix]]))
  })
  temp.S <- as.data.frame(matrix(unlist(temp.S), ncol = n ** 2, byrow = T))
  
  temp.I <- lapply(1:length(I.list), function(ix) {
    as.vector(t(I.list[[ix]]))
  })
  temp.I <- as.data.frame(matrix(unlist(temp.I), ncol = n ** 2, byrow = T))
  
  temp.newI <- lapply(1:length(newI.list), function(ix) {
    as.vector(t(newI.list[[ix]]))
  })
  temp.newI <- as.data.frame(matrix(unlist(temp.newI), ncol = n ** 2, byrow = T))
  
  # rows.by.country <- matrix(1:(n ** 2), ncol = n, byrow = TRUE)
  # temp.N.c <- NULL
  
  if (realdata == FALSE) {
    rec <- list(S = temp.S, I = temp.I)
  } else {
    rec <- list(S = temp.S, I = temp.I, newI = temp.newI)
  }
  
  rec
}
