
# # Generate Vtype matrix:
# Vtype <- matrix(0, nrow = 365 * 20, ncol = 2) # first column is strain 1/H1N1, second column is strain 2/H3N2
# # day 91 would be first of April, 121 would be first of May; Jeff's seem to change on 123 (3rd of May) - did they give February 30 days? Or did they go 122/7 weeks? (no, not whole)
# # let's use May - so change on day 121
# year.starts <- c(1, 365 * (1:19) + 1)
# seed.starts <- year.starts + 120
# 
# Vtype[c(121:485, 851:1580, 1946:2310, 3041:3405, 3771:4135, 4501:5230, 5596:5960, 6691:7055), 1] <- 1 # 1, 3:4, 6, 9, 11, 13:14, 16, 19
# Vtype[c(486:850, 1216:1945, 2311:2675, 3041:3405, 4136:4500, 4866:5595, 5961:6325, 6691:7056), 2] <- 1 # 2, 4:5, 7, 9, 12, 14:15, 17, 19
# 
# Vtype <- rbind(c(0, 0), Vtype) # add a row of zeros (at cnt value where initial conditions are)
# Vtype <- rbind(Vtype, c(0, 0)); Vtype <- rbind(Vtype, c(0, 0)) # and two to make sure the final week runs through
# 
# write.csv(Vtype, file = 'data/subtypes_seeding.csv', row.names = FALSE)

# Code to run stochastic, multistrain SIRS:
propagateToySIRS_multi <- function(tm_strt, tm_end, tm_step, S01, I01, S02, I02, N, D, L, beta, airScale, realdata = FALSE, prohibAir = FALSE) {
  
  if (!exists('prohibAir')) {
    prohibAir <- FALSE
  }
  
  cnt <- 1
  
  tm_strt <- tm_strt - tm.range[1] + 1 # adjust the index to match beta
  tm_end <- tm_end - tm.range[1] + 1
  
  tm_vec <- seq(tm_strt, tm_end, by = tm_step)
  tm_sz <- length(tm_vec) + 1 # include initial conditions
  
  S.list = R.list = N.list = array(0, c(n, n, tm_sz, 2))#vector('list', tm_sz)
  I.list = newI.list = array(0, c(n, n, tm_sz, 2)) # can change to track total too, but really no need
  
  S.list[,, 1, 1] <- S01
  I.list[,, 1, 1] <- I01
  R.list[,, 1, 1] <- N - S01 - I01
  
  S.list[,, 1, 2] <- S02
  I.list[,, 1, 2] <- I02
  R.list[,, 1, 2] <- N - S02 - I02
  
  # I.list[,, 1, 3] <- I.list[,, 1, 1] + I.list[,, 1, 2]
  # newI.list[,, 1, 1:3] <- matrix(0, nrow = n, ncol = n)
  
  # S.list[[1]] <- S0; I.list[[1]] <- I0; R.list[[1]] <- N - S0 - I0
  # N.list[[1]] <- N
  # newI.list[[1]] <- matrix(0, nrow = n, ncol = n)
  
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
    # print(cnt)
    
    # First, choose t.rand by month:
    t.true <- t + tm.range[1] - 1
    if (t.true %in% t.jan) {
      # print('January')
      load('formatTravelData/formattedData/air_1_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.feb) {
      # print('February')
      load('formatTravelData/formattedData/air_2_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.mar) {
      # print('March')
      load('formatTravelData/formattedData/air_3_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.apr) {
      # print('April')
      load('formatTravelData/formattedData/air_4_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.may) {
      # print('May')
      load('formatTravelData/formattedData/air_5_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.jun) {
      # print('June')
      load('formatTravelData/formattedData/air_6_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.jul) {
      # print('July')
      load('formatTravelData/formattedData/air_7_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.aug) {
      # print('August')
      load('formatTravelData/formattedData/air_8_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.sep) {
      # print('September')
      load('formatTravelData/formattedData/air_9_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.oct) {
      # print('October')
      load('formatTravelData/formattedData/air_10_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.nov) {
      # print('November')
      load('formatTravelData/formattedData/air_11_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% t.dec) {
      # print('December')
      load('formatTravelData/formattedData/air_12_05-07.RData')
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
    S <- S.list[,, cnt - 1, ]; I <- I.list[,, cnt - 1, ]; newI <- newI.list[,, cnt - 1, ]
    # S <- S.list[[cnt - 1]]; I <- I.list[[cnt - 1]]; newI <- newI.list[[cnt - 1]]
    
    if (!exists('discrete')) {
      discrete <- TRUE
    }
    
    if (discrete) { # run stochastically
      N <- round(N, 0) # N needs to be whole numbers, then!
      
      S <- round(S, 0); I <- round(I, 0)
      R <- array(0, c(n, n, 2))
      R[,, 1] <- N - S[,, 1] - I[,, 1]; R[,, 2] <- N - S[,, 2] - I[,, 2]
      
      if (cnt == 2) {
        S.list[,, cnt - 1, ] <- S
        I.list[,, cnt - 1, ] <- I
        R.list[,, cnt - 1, ] <- R
      } # shouldn't be an issue after this
      
      # Determine number of daytime and nighttime travelers:
      all.rand.day <- all.rand / 3; all.rand.night <- all.rand * 2 / 3
      all.rand.day.temp <- rpois(length(all.rand.day[upper.tri(all.rand.day)]), all.rand.day[upper.tri(all.rand.day)])
      all.rand.night.temp <- rpois(length(all.rand.night[upper.tri(all.rand.night)]), all.rand.night[upper.tri(all.rand.night)])
      trav.day = trav.night = matrix(0, nrow = n, ncol = n)
      trav.day[upper.tri(trav.day)] <- all.rand.day.temp; trav.night[upper.tri(trav.night)] <- all.rand.night.temp
      trav.day <- trav.day + t(trav.day); trav.night <- trav.night + t(trav.night)
      # print(isSymmetric(trav.day))
      # print(isSymmetric(trav.night))
      
      # Now we need to loop through the different "strains":
      for (i.strain in 1:2) {
        # Daytime:
        
        # Step 1:
        Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - S[,, i.strain] - I [,, i.strain]))
        Einf <- sweep(sweep(sweep(S[,, i.strain], 2, colSums(I[,, i.strain]), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                      2, 1 / colSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (1 / 3)) * (1 / D * I[,, i.strain])
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        # travel:
        s.out <- sweep(sweep(S[,, i.strain], 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        i.out <- sweep(sweep(I[,, i.strain], 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        r.out <- sweep(sweep(R[,, i.strain], 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        
        s.in <- matrix((colSums(S[,, i.strain]) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        i.in <- matrix((colSums(I[,, i.strain]) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        r.in <- matrix((colSums(R[,, i.strain]) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk1 <- smcl - smci + Estrav
        ik1 <- smci - smcr + Eitrav
        ik1a <- smci# + (tm_step * (1 / 3) * i.in) # another option would be to count incoming by HOME country, and not by individual compartment?
        rk1 <- smcr - smcl + Ertrav
        # print(all(sk1+ik1+rk1==0))
        
        Ts1 <- S[,, i.strain] + round(sk1 / 2, 0)
        Ti1 <- I[,, i.strain] + round(ik1 / 2, 0)
        # Tr1 <- R[,, i.strain] + round(rk1 / 2, 0)
        Ts1[Ts1 < 0] <- 0; Ti1[Ti1 < 0] <- 0
        Tr1 <- N - Ts1 - Ti1
        # print(all.equal(Ts1 + Ti1 + Tr1, N))
        
        # STEP 2
        Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts1 - Ti1))
        Einf <- sweep(sweep(sweep(Ts1, 2, colSums(Ti1), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                      2, 1 / colSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti1)
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)

        s.out <- sweep(sweep(Ts1, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        i.out <- sweep(sweep(Ti1, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        r.out <- sweep(sweep(Tr1, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        
        s.in <- matrix((colSums(Ts1) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        i.in <- matrix((colSums(Ti1) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        r.in <- matrix((colSums(Tr1) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk2 <- smcl - smci + Estrav
        ik2 <- smci - smcr + Eitrav
        ik2a <- smci# + (tm_step * (1 / 3) * i.in)
        rk2 <- smcr - smcl + Ertrav
        
        Ts2 <- S[,, i.strain] + round(sk2 / 2, 0)
        Ti2 <- I[,, i.strain] + round(ik2 / 2, 0)
        # Tr2 <- R.list[[cnt - 1]] + round(rk2 / 2, 0)
        Ts2[Ts2 < 0] <- 0; Ti2[Ti2 < 0] <- 0
        Tr2 <- N - Ts2 - Ti2
        # print(all.equal(Ts2 + Ti2 + Tr2, N))
        
        # STEP 3
        Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts2 - Ti2))
        Einf <- sweep(sweep(sweep(Ts2, 2, colSums(Ti2), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                      2, 1 / colSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti2)
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        s.out <- sweep(sweep(Ts2, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        i.out <- sweep(sweep(Ti2, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        r.out <- sweep(sweep(Tr2, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        
        s.in <- matrix((colSums(Ts2) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        i.in <- matrix((colSums(Ti2) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        r.in <- matrix((colSums(Tr2) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk3 <- smcl - smci + Estrav
        ik3 <- smci - smcr + Eitrav
        ik3a <- smci# + (tm_step * (1 / 3) * i.in)
        rk3 <- smcr - smcl + Ertrav
        
        Ts3 <- S[,, i.strain] + round(sk3, 0)# / 2
        Ti3 <- I[,, i.strain] + round(ik3, 0)# / 2
        # Tr3 <- R.list[[cnt - 1]] + round(rk3 / 2, 0)
        Ts3[Ts3 < 0] <- 0; Ti3[Ti3 < 0] <- 0
        Tr3 <- N - Ts3 - Ti3
        
        # STEP 4
        Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts3 - Ti3))
        Einf <- sweep(sweep(sweep(Ts3, 2, colSums(Ti3), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
                      2, 1 / colSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti3)
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        s.out <- sweep(sweep(Ts3, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        i.out <- sweep(sweep(Ti3, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        r.out <- sweep(sweep(Tr3, 2, 1 / colSums(N), '*'), 2, rowSums(trav.day), '*')
        
        s.in <- matrix((colSums(Ts3) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        i.in <- matrix((colSums(Ti3) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        r.in <- matrix((colSums(Tr3) / colSums(N)) %*% trav.day, nrow = n, ncol = n, byrow = T) *
          (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk4 <- smcl - smci + Estrav
        ik4 <- smci - smcr + Eitrav
        ik4a <- smci# + (tm_step * (1 / 3) * i.in)
        rk4 <- smcr - smcl + Ertrav
        
        S[,, i.strain] <- S[,, i.strain] + round(sk1/6 + sk2/3 + sk3/3 + sk4/6, 0)# - seed
        I[,, i.strain] <- I[,, i.strain] + round(ik1/6 + ik2/3 + ik3/3 + ik4/6, 0)# + seed
        newI[,, i.strain] <- newI[,, i.strain] + round(ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6, 0)# + seed
        # R <- R + round(rk1/6 + rk2/3 + rk3/3 + rk4/6, 0)# + seed
        S[,, i.strain][S[,, i.strain] < 0] <- 0; I[,, i.strain][I[,, i.strain] < 0] <- 0
        R[,, i.strain] <- N - S[,, i.strain] - I[,, i.strain]
        # print(all.equal(N, S[,, i.strain] + I[,, i.strain] + R[,, i.strain], check.attributes = FALSE))
        
        # Nighttime:
        # Step 1:
        Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - S[,, i.strain] - I [,, i.strain]))
        Einf <- sweep(sweep(sweep(S[,, i.strain], 1, rowSums(I[,, i.strain]), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                      1, 1 / rowSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (2 / 3)) * (1 / D * I[,, i.strain])
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        s.out <- sweep(sweep(S[,, i.strain], 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        i.out <- sweep(sweep(I[,, i.strain], 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        r.out <- sweep(sweep(R[,, i.strain], 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        
        s.in <- matrix((rowSums(S[,, i.strain]) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        i.in <- matrix((rowSums(I[,, i.strain]) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        r.in <- matrix((rowSums(R[,, i.strain]) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk1 <- smcl - smci + Estrav
        ik1 <- smci - smcr + Eitrav
        ik1a <- smci
        rk1 <- smcr - smcl + Ertrav
        
        Ts1 <- S[,, i.strain] + round(sk1 / 2, 0)
        Ti1 <- I[,, i.strain] + round(ik1 / 2, 0)
        Ts1[Ts1 < 0] <- 0; Ti1[Ti1 < 0] <- 0
        # Tr1 <- R.list[[cnt - 1]] + round(rk1 / 2, 0)
        Tr1 <- N - Ts1 - Ti1
        # print(all.equal(Ts1 + Ti1 + Tr1, N))
        
        # STEP 2
        Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts1 - Ti1))
        Einf <- sweep(sweep(sweep(Ts1, 1, rowSums(Ti1), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                      1, 1 / rowSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti1)
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        s.out <- sweep(sweep(Ts1, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        i.out <- sweep(sweep(Ti1, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        r.out <- sweep(sweep(Tr1, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        
        s.in <- matrix((rowSums(Ts1) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        i.in <- matrix((rowSums(Ti1) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        r.in <- matrix((rowSums(Tr1) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk2 <- smcl - smci + Estrav
        ik2 <- smci - smcr + Eitrav
        ik2a <- smci
        rk2 <- smcr - smcl + Ertrav
        
        Ts2 <- S[,, i.strain] + round(sk2 / 2, 0)
        Ti2 <- I[,, i.strain] + round(ik2 / 2, 0)
        # Tr2 <- R.list[[cnt - 1]] + round(rk2 / 2, 0)
        Ts2[Ts2 < 0] <- 0; Ti2[Ti2 < 0] <- 0
        Tr2 <- N - Ts2 - Ti2
        # print(all.equal(Ts2 + Ti2 + Tr2, N))
        
        # STEP 3
        Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts2 - Ti2))
        Einf <- sweep(sweep(sweep(Ts2, 1, rowSums(Ti2), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
                      1, 1 / rowSums(N), '*')
        Einf[is.na(Einf)] <- 0
        Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti2)
        
        smcl <- rpois(length(Eimmloss), Eimmloss); dim(smcl) <- dim(Eimmloss)
        smci <- rpois(length(Einf), Einf); dim(smci) <- dim(Einf)
        smcr <- rpois(length(Erecov), Erecov); dim(smcr) <- dim(Erecov)
        
        s.out <- sweep(sweep(Ts2, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        i.out <- sweep(sweep(Ti2, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        r.out <- sweep(sweep(Tr2, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        
        s.in <- matrix((rowSums(Ts2) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        i.in <- matrix((rowSums(Ti2) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        r.in <- matrix((rowSums(Tr2) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        
        Estrav <- s.in - s.out # tm_step * 2/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk3 <- smcl - smci + Estrav
        ik3 <- smci - smcr + Eitrav
        ik3a <- smci
        rk3 <- smcr - smcl + Ertrav
        
        Ts3 <- S[,, i.strain] + round(sk3, 0)# / 2
        Ti3 <- I[,, i.strain] + round(ik3, 0)# / 2
        # Tr3 <- R.list[[cnt - 1]] + round(rk3 / 2, 0)
        # first: set any Ts/Ti<0 to 0:
        Ts3[Ts3 < 0] <- 0; Ti3[Ti3 < 0] <- 0
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
        
        s.out <- sweep(sweep(Ts3, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        i.out <- sweep(sweep(Ti3, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        r.out <- sweep(sweep(Tr3, 1, 1 / rowSums(N), '*'), 1, rowSums(trav.night), '*')
        
        s.in <- matrix((rowSums(Ts3) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        i.in <- matrix((rowSums(Ti3) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        r.in <- matrix((rowSums(Tr3) / rowSums(N)) %*% trav.night, nrow = n, ncol = n, byrow = F) *
          (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
        
        Estrav <- s.in - s.out # tm_step * 1/3 incorporated earlier instead
        Eitrav <- i.in - i.out
        Ertrav <- r.in - r.out
        
        Estrav <- matrix(sumpreserving.rounding(as.vector(Estrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Eitrav <- matrix(sumpreserving.rounding(as.vector(Eitrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(sumpreserving.rounding(as.vector(Ertrav), 0, preserve = TRUE), nrow = n, ncol = n, byrow = FALSE)
        Ertrav <- matrix(0, nrow = n, ncol = n) - Estrav - Eitrav
        # print(all(Estrav + Eitrav + Ertrav == 0))
        
        sk4 <- smcl - smci + Estrav
        ik4 <- smci - smcr + Eitrav
        ik4a <- smci
        rk4 <- smcr - smcl + Ertrav
        
        # seed <- 0.1 # Do seeding at end, if any - not twice a day
        S.list[,, cnt, i.strain] <- S[,, i.strain] + round(sk1/6 + sk2/3 + sk3/3 + sk4/6, 0)# - seed
        I.list[,, cnt, i.strain] <- I[,, i.strain] + round(ik1/6 + ik2/3 + ik3/3 + ik4/6, 0)# + seed
        newI.list[,, cnt, i.strain] <- newI[,, i.strain] + round(ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6, 0)# + seed
        # R <- R + round(rk1/6 + rk2/3 + rk3/3 + rk4/6, 0)# + seed
        S.list[,, cnt, i.strain][S.list[,, cnt, i.strain] < 0] <- 0; I.list[,, cnt, i.strain][I.list[,, cnt, i.strain] < 0] <- 0
        R.list[,, cnt, i.strain] <- N - S.list[,, cnt, i.strain] - I.list[,, cnt, i.strain]
        # print(all.equal(N, S[,, i.strain] + I[,, i.strain] + R[,, i.strain], check.attributes = FALSE))
      }
      
      # Finally, seeding:
      # Base this on the dominant strain(s) of that year; start new seeding in May
      # seed <- diag(rpois(n, 0.1), n, n) # here seeding only in home-home compartments; 10% chance of a single new infection
      # seeding also needs to be 10% chance of 1 PER 100,000 - so seeding matrix * (N / 100,000), then round
      if (Vtype[cnt, 1] == 1) {
        # print(paste(cnt, 1, sep = '_'))
        # seed <- diag(rpois(n, 0.1), n, n) # here seeding only in home-home compartments; 10% chance of a single new infection - actually, higher RATE for bigger places
        
        # N / max(N) / 10 # this makes it 1 new case every 10 days for DE-DE compartment, and proportionally less elsewhere
        
        seed <- matrix(sapply(N, function(ix) {
          rpois(1, ix / max(N) / 10)
          # ix / max(N)
        }), nrow = n, ncol = n, byrow = FALSE)
        
        # rpois(1, sum(N) / 100000) # number of seeds to check
        # sum(rpois(rpois(1, sum(N) / 100000), 0.1)) # total # of cases to seed
        # # then could distribute these based on relative population sizes of compartments
        
        # every compartment has 10% chance of seeding
        S.list[,, cnt, 1] <- S.list[,, cnt, 1] - seed
        I.list[,, cnt, 1] <- I.list[,, cnt, 1] + seed
        newI.list[,, cnt, 1] <- newI.list[,, cnt, 1] + seed
      }
      if (Vtype[cnt, 2] == 1) {
        # print(paste(cnt, 2, sep = '_'))
        # seed <- diag(rpois(n, 0.1), n, n)
        seed <- matrix(sapply(N, function(ix) {
          rpois(1, ix / max(N) / 10)
          # ix / max(N)
        }), nrow = n, ncol = n, byrow = FALSE)
        
        S.list[,, cnt, 2] <- S.list[,, cnt, 2] - seed
        I.list[,, cnt, 2] <- I.list[,, cnt, 2] + seed
        newI.list[,, cnt, 2] <- newI.list[,, cnt, 2] + seed
      }
      
      # And get total I/newI:
      # I.list[,, cnt, 3] <- I.list[,, cnt, 1] + I.list[,, cnt, 2]
      # newI.list[,, cnt, 3] <- newI.list[,, cnt, 1] + newI.list[,, cnt, 2]
      
    } # else { # run continuously/deterministically
    #   
    #   ### Daytime ###
    #   # during the daytime, N.d describes how many people are in each compartment (sum over each COLUMN); during the nighttime, it's N.s (sum over each ROW)
    #   
    #   Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - S - I))
    #   # daytime, so we want col1 multiplied by beta1, etc.
    #   Einf <- sweep(sweep(sweep(S, 2, colSums(I), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
    #                 2, 1 / colSums(N), '*')
    #   # Einf=tm_step/2*(tnetcorr%*%(beta[t,]*(netcorr%*%I[cnt-1,])/N_hat)*S[cnt-1,]) # corrected
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (1 / 3)) * (1 / D * I)
    #   
    #   # Einf.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     Einf.check[i, j] <- (tm_step * (1 / 3)) * (beta[t, j] * S[i, j] * sum(I[, j])) / sum(N[, j])
    #   #   }
    #   # }
    #   # colnames(Einf.check) = rownames(Einf.check) = countries
    #   # print(all.equal(Einf, Einf.check))
    #   
    #   # Now incorporate travel:
    #   s.out <- sweep(sweep(S, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(I, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   
    #   # s.out.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     s.out.check[i, j] <- (S[i, j] / sum(N[, j])) * sum(all.rand[j, ])
    #   #   }
    #   # }
    #   # colnames(s.out.check) = rownames(s.out.check) = countries
    #   # print(all.equal(s.out, s.out.check))
    #   
    #   s.in <- matrix((colSums(S) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   i.in <- matrix((colSums(I) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   # QUESTION: Is it a problem to do the infection and travel steps separately? Should they be sequential or something?
    #   # QUESTION: "all.equal" is true, but "==" is false?
    #   
    #   # s.in.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     s.in.temp = 0
    #   #     for (k in 1:3) { # country people are flying in FROM
    #   #       for (h in 1:3) { # home (day) / work (night) place of those in country being traveled from
    #   #         s.in.temp <- s.in.temp + (N[i, j] / sum(N[, j])) * all.rand[k, j] * S[h, k] / sum(N[, k])
    #   #         # s.in.temp <- s.in.temp + (N[i, j] / sum(N[i, ])) * all.rand[k, i] * S[k, h] / sum(N[k, ])
    #   #       }
    #   #     }
    #   #     s.in.check[i, j] <- s.in.temp
    #   #   }
    #   # }
    #   # colnames(s.in.check) = rownames(s.in.check) = countries
    #   # print(all.equal(s.in, s.in.check))
    #   
    #   Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
    #   Einf[Einf < 0] <- 0
    #   Erecov[Erecov < 0] <- 0
    #   
    #   smcl <- Eimmloss
    #   smci <- Einf
    #   smcr <- Erecov
    #   
    #   sk1 <- smcl - smci + Estrav
    #   ik1 <- smci - smcr + Eitrav
    #   ik1a <- smci# + (tm_step * (1 / 3) * i.in) # another option would be to count incoming by HOME country, and not by individual compartment?
    #   # ONLY count those who are newly infected; i.in describes people who are already infected, and are traveling to a country; they are not NEWLY infected, so don't double-count
    #   Ts1 <- S.list[[cnt - 1]] + sk1 / 2
    #   Ti1 <- I.list[[cnt - 1]] + ik1 / 2
    #   
    #   # print(Eimmloss)
    #   # print(Einf)
    #   # print(Estrav)
    #   # print(Ts1)
    #   # print('')
    #   
    #   # STEP 2
    #   Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts1 - Ti1))
    #   Einf <- sweep(sweep(sweep(Ts1, 2, colSums(Ti1), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
    #                 2, 1 / colSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti1)
    #   
    #   s.out <- sweep(sweep(Ts1, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti1, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   s.in <- matrix((colSums(Ts1) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   i.in <- matrix((colSums(Ti1) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   
    #   Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
    #   Einf[Einf < 0] <- 0
    #   Erecov[Erecov < 0] <- 0
    #   
    #   smcl <- Eimmloss
    #   smci <- Einf
    #   smcr <- Erecov
    #   
    #   sk2 <- smcl - smci + Estrav
    #   ik2 <- smci - smcr + Eitrav
    #   ik2a <- smci# + (tm_step * (1 / 3) * i.in)
    #   
    #   Ts2 <- S.list[[cnt - 1]] + sk2 / 2
    #   Ti2 <- I.list[[cnt - 1]] + ik2 / 2
    #   
    #   # STEP 3
    #   Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts2 - Ti2))
    #   Einf <- sweep(sweep(sweep(Ts2, 2, colSums(Ti2), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
    #                 2, 1 / colSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti2)
    #   
    #   s.out <- sweep(sweep(Ts2, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti2, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   s.in <- matrix((colSums(Ts2) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   i.in <- matrix((colSums(Ti2) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   
    #   Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
    #   Einf[Einf < 0] <- 0
    #   Erecov[Erecov < 0] <- 0
    #   
    #   smcl <- Eimmloss
    #   smci <- Einf
    #   smcr <- Erecov
    #   
    #   sk3 <- smcl - smci + Estrav
    #   ik3 <- smci - smcr + Eitrav
    #   ik3a <- smci# + (tm_step * (1 / 3) * i.in)
    #   
    #   Ts3 <- S.list[[cnt - 1]] + sk3# / 2
    #   Ti3 <- I.list[[cnt - 1]] + ik3# / 2
    #   
    #   # STEP 4
    #   Eimmloss <- (tm_step * (1 / 3)) * (1 / L * (N - Ts3 - Ti3))
    #   Einf <- sweep(sweep(sweep(Ts3, 2, colSums(Ti3), '*') * (tm_step * (1 / 3)), 2, beta[t, ], '*'),
    #                 2, 1 / colSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (1 / 3)) * (1 / D * Ti3)
    #   
    #   s.out <- sweep(sweep(Ts3, 2, 1 / colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti3, 2, 1/colSums(N), '*'), 2, rowSums(all.rand), '*')
    #   s.in <- matrix((colSums(Ts3) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   i.in <- matrix((colSums(Ti3) / colSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = T) *
    #     (N / matrix(colSums(N), nrow = n, ncol = n, byrow = T))
    #   
    #   Estrav <- (tm_step * (1 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (1 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
    #   Einf[Einf < 0] <- 0
    #   Erecov[Erecov < 0] <- 0
    #   
    #   smcl <- Eimmloss
    #   smci <- Einf
    #   smcr <- Erecov
    #   
    #   sk4 <- smcl - smci + Estrav
    #   ik4 <- smci - smcr + Eitrav
    #   ik4a <- smci# + (tm_step * (1 / 3) * i.in)
    #   
    #   # seed <- 0.1 # Do seeding at end, if any - not twice a day
    #   S <- S + sk1/6 + sk2/3 + sk3/3 + sk4/6# - seed
    #   I <- I + ik1/6 + ik2/3 + ik3/3 + ik4/6# + seed
    #   newI <- newI + ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6# + seed
    #   
    #   # print(Eimmloss)
    #   # print(Einf)
    #   # print(Estrav)
    #   # print(S)
    #   # print(''); print(''); print('')
    #   
    #   ### Nighttime ###
    #   Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - S - I))
    #   Einf <- sweep(sweep(sweep(S, 1, rowSums(I), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
    #                 1, 1 / rowSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (2 / 3)) * (1 / D * I)
    #   
    #   # Einf.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     Einf.check[i, j] <- (tm_step * (2 / 3)) * (beta[t, i] * S[i, j] * sum(I[i, ])) / sum(N[i, ])
    #   #   }
    #   # }
    #   # colnames(Einf.check) = rownames(Einf.check) = countries
    #   # print(all.equal(Einf, Einf.check))
    #   
    #   # Now incorporate travel:
    #   s.out <- sweep(sweep(S, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(I, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   
    #   # s.out.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     s.out.check[i, j] <- (S[i, j] / sum(N[i, ])) * sum(all.rand[i, ])
    #   #   }
    #   # }
    #   # colnames(s.out.check) = rownames(s.out.check) = countries
    #   # print(all.equal(s.out, s.out.check))
    #   
    #   s.in <- matrix((rowSums(S) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   i.in <- matrix((rowSums(I) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   
    #   # s.in.check <- matrix(0, 3, 3)
    #   # for (i in 1:3) {
    #   #   for (j in 1:3) {
    #   #     # i is home, j is work
    #   #     s.in.temp = 0
    #   #     for (k in 1:3) { # country people are flying in FROM
    #   #       for (h in 1:3) { # home (day) / work (night) place of those in country being traveled from
    #   #         # s.in.temp <- s.in.temp + (N[i, j] / sum(N[, j])) * all.rand[k, j] * S[h, k] / sum(N[, k])
    #   #         s.in.temp <- s.in.temp + (N[i, j] / sum(N[i, ])) * all.rand[k, i] * S[k, h] / sum(N[k, ])
    #   #       }
    #   #     }
    #   #     s.in.check[i, j] <- s.in.temp
    #   #   }
    #   # }
    #   # colnames(s.in.check) = rownames(s.in.check) = countries
    #   # print(all.equal(s.in, s.in.check))
    #   
    #   # print(s.in); print(''); print(''); print('')
    #   
    #   Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss < 0] <- 0 # set any values below 0 to 0
    #   Einf[Einf < 0] <- 0
    #   Erecov[Erecov < 0] <- 0
    #   
    #   smcl <- Eimmloss
    #   smci <- Einf
    #   smcr <- Erecov
    #   
    #   sk1 <- smcl - smci + Estrav
    #   ik1 <- smci - smcr + Eitrav
    #   ik1a <- smci# + (tm_step * (2 / 3) * i.in)
    #   
    #   Ts1 <- S + sk1 / 2
    #   Ti1 <- I + ik1 / 2
    #   
    #   # print(Eimmloss)
    #   # print(Einf)
    #   # print(Estrav)
    #   # print(Ts1)
    #   # print(''); print(''); print('')
    #   
    #   # STEP 2
    #   Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts1 - Ti1))
    #   Einf <- sweep(sweep(sweep(Ts1, 1, rowSums(Ti1), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
    #                 1, 1 / rowSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti1)
    #   
    #   s.out <- sweep(sweep(Ts1, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti1, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   s.in <- matrix((rowSums(Ts1) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   i.in <- matrix((rowSums(Ti1) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   
    #   Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
    #   Einf[Einf<0]=0
    #   Erecov[Erecov<0]=0
    #   
    #   smcl=Eimmloss
    #   smci=Einf
    #   smcr=Erecov
    #   
    #   sk2=smcl-smci+Estrav
    #   ik2=smci-smcr+Eitrav
    #   ik2a=smci#+(tm_step * (2 / 3) * i.in)
    #   
    #   Ts2 <- S + sk2 / 2
    #   Ti2 <- I + ik2 / 2
    #   
    #   # STEP 3
    #   Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts2 - Ti2))
    #   Einf <- sweep(sweep(sweep(Ts2, 1, rowSums(Ti2), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
    #                 1, 1 / rowSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti2)
    #   
    #   s.out <- sweep(sweep(Ts2, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti2, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   s.in <- matrix((rowSums(Ts2) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   i.in <- matrix((rowSums(Ti2) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   
    #   Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
    #   Einf[Einf<0]=0
    #   Erecov[Erecov<0]=0
    #   
    #   smcl=Eimmloss
    #   smci=Einf
    #   smcr=Erecov
    #   
    #   sk3=smcl-smci+Estrav
    #   ik3=smci-smcr+Eitrav
    #   ik3a=smci#+(tm_step * (2 / 3) * i.in)
    #   
    #   Ts3 <- S + sk3# / 2
    #   Ti3 <- I + ik3# / 2
    #   
    #   # STEP 4
    #   Eimmloss <- (tm_step * (2 / 3)) * (1 / L * (N - Ts3 - Ti3))
    #   Einf <- sweep(sweep(sweep(Ts3, 1, rowSums(Ti3), '*') * (tm_step * (2 / 3)), 1, beta[t, ], '*'),
    #                 1, 1 / rowSums(N), '*')
    #   Einf[is.na(Einf)] <- 0
    #   Erecov <- (tm_step * (2 / 3)) * (1 / D * Ti3)
    #   
    #   s.out <- sweep(sweep(Ts3, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   i.out <- sweep(sweep(Ti3, 1, 1 / rowSums(N), '*'), 1, rowSums(all.rand), '*')
    #   s.in <- matrix((rowSums(Ts3) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   i.in <- matrix((rowSums(Ti3) / rowSums(N)) %*% all.rand, nrow = n, ncol = n, byrow = F) *
    #     (N / matrix(rowSums(N), nrow = n, ncol = n, byrow = F))
    #   
    #   Estrav <- (tm_step * (2 / 3)) * (s.in - s.out)
    #   Eitrav <- (tm_step * (2 / 3)) * (i.in - i.out)
    #   
    #   Eimmloss[Eimmloss<0]=0   # adjust, set <0 to 0
    #   Einf[Einf<0]=0
    #   Erecov[Erecov<0]=0
    #   
    #   smcl=Eimmloss
    #   smci=Einf
    #   smcr=Erecov
    #   
    #   sk4=smcl-smci+Estrav
    #   ik4=smci-smcr+Eitrav
    #   ik4a=smci#+(tm_step * (2 / 3) * i.in)
    #   
    #   S.list[[cnt]] <- S + sk1/6 + sk2/3 + sk3/3 + sk4/6# - seed
    #   I.list[[cnt]] <- I + ik1/6 + ik2/3 + ik3/3 + ik4/6# + seed
    #   newI.list[[cnt]] <- newI + ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6# + seed
    #   
    # }
    
  }
  
  # Format S:
  s.list1 <- lapply(1:dim(S.list)[3], function(ix) {
    S.list[,, ix, 1]
  })
  s.list2 <- lapply(1:dim(S.list)[3], function(ix) {
    S.list[,, ix, 2]
  })
  
  temp.S1 <- lapply(1:length(s.list1), function(ix) {
    as.vector(t(s.list1[[ix]]))
  })
  temp.S1 <- as.data.frame(matrix(unlist(temp.S1), ncol = n ** 2, byrow = T))
  
  temp.S2 <- lapply(1:length(s.list2), function(ix) {
    as.vector(t(s.list2[[ix]]))
  })
  temp.S2 <- as.data.frame(matrix(unlist(temp.S2), ncol = n ** 2, byrow = T))
  # these functions take the matrices row by row (row1, then row2, etc.), giving each compartment a column
  
  rm(s.list1, s.list2)
  
  # Format I:
  i.list1 <- lapply(1:dim(I.list)[3], function(ix) {
    I.list[,, ix, 1]
  })
  i.list2 <- lapply(1:dim(I.list)[3], function(ix) {
    I.list[,, ix, 2]
  })
  # i.list3 <- lapply(1:dim(I.list)[3], function(ix) {
  #   I.list[,, ix, 3]
  # })
  
  temp.I1 <- lapply(1:length(i.list1), function(ix) {
    as.vector(t(i.list1[[ix]]))
  })
  temp.I1 <- as.data.frame(matrix(unlist(temp.I1), ncol = n ** 2, byrow = T))
  
  temp.I2 <- lapply(1:length(i.list2), function(ix) {
    as.vector(t(i.list2[[ix]]))
  })
  temp.I2 <- as.data.frame(matrix(unlist(temp.I2), ncol = n ** 2, byrow = T))
  
  # temp.I3 <- lapply(1:length(i.list3), function(ix) {
  #   as.vector(t(i.list3[[ix]]))
  # })
  # temp.I3 <- as.data.frame(matrix(unlist(temp.I3), ncol = n ** 2, byrow = T))
  
  # temp.I3 <- temp.I1 + temp.I2 # only the first values of some columns are different due to rounding; but this way makes more sense
  
  # matplot(temp.I1, pch = 20, col = viridis(144), cex = 0.5)
  # matplot(temp.I2, pch = 20, col = viridis(144), cex = 0.5)
  # matplot(temp.I3, pch = 20, col = viridis(144), cex = 0.5)
  
  rm(i.list1, i.list2)
  
  # Format newI:
  newi.list1 <- lapply(1:dim(newI.list)[3], function(ix) {
    newI.list[,, ix, 1]
  })
  newi.list2 <- lapply(1:dim(newI.list)[3], function(ix) {
    newI.list[,, ix, 2]
  })
  
  temp.newI1 <- lapply(1:length(newi.list1), function(ix) {
    as.vector(t(newi.list1[[ix]]))
  })
  temp.newI1 <- as.data.frame(matrix(unlist(temp.newI1), ncol = n ** 2, byrow = T))
  
  temp.newI2 <- lapply(1:length(newi.list2), function(ix) {
    as.vector(t(newi.list2[[ix]]))
  })
  temp.newI2 <- as.data.frame(matrix(unlist(temp.newI2), ncol = n ** 2, byrow = T))
  
  # temp.newI3 <- temp.newI1 + temp.newI2
  
  # ##################################################################################################################################
  # ### Check whether die out occurs, and where ###
  # temp.I1 <- temp.I1[3651:7303, ]; temp.I2 <- temp.I2[3651:7303, ]
  # die.out1 = die.out2 = c()
  # for (i in 1:n**2) {
  #   die.out1 <- c(die.out1, any(temp.I1[, i] == 0))
  #   die.out2 <- c(die.out2, any(temp.I2[, i] == 0))
  # }
  # print(summary(die.out1)); print(summary(die.out2))
  # # die.out1 = die.out2 = c()
  # # for (i in 1:3653) {
  # #   die.out1 <- c(die.out1, all(temp.I1[i, ] == 0))
  # #   die.out2 <- c(die.out2, all(temp.I2[i, ] == 0))
  # # }
  # # print(summary(die.out1)); print(summary(die.out2))
  # print(which(!die.out1)); print(which(!die.out2))
  # plot(rowSums(temp.I1), pch = 20, cex = 0.5)
  # points(rowSums(temp.I2), pch = 20, cex = 0.5, col = 'blue')
  # abline(v = seq(0, 4000, by = 365))
  # # 299: only non-die-out was 118 (PL-PL) (strain1) / 53/118 (DE-DE, PL-PL) (strain 2)
  # # 33: all die out? but all at once? - yes, there are times when all die out, but all in first 10 years
  #     # limit to last 10 years: don't die out ever: 40 (FR-FR) (strain1) / 40/53 (FR-FR/DE-DE) (strain2)
  # # 431: none of the same home/work compartments die out (note: this is one of the ones w/ outbreaks every year - very short L)
  # # 1179: most of the home/work compartments; changed R0diff to 0.5: 8/12 don't die out, 0.8: FR, DE, IT, ES, NL; changed R0mx to 2.4: survived in FR-LU too?, 2.0: even more survive - turns into oscillation
  # ##################################################################################################################################
  
  # Format R (to check that nothing ridiculous is happening):
  r.list1 <- lapply(1:dim(R.list)[3], function(ix) {
    R.list[,, ix, 1]
  })
  r.list2 <- lapply(1:dim(R.list)[3], function(ix) {
    R.list[,, ix, 2]
  })
  
  temp.R1 <- lapply(1:length(r.list1), function(ix) {
    as.vector(t(r.list1[[ix]]))
  })
  temp.R1 <- as.data.frame(matrix(unlist(temp.R1), ncol = n ** 2, byrow = T))
  
  temp.R2 <- lapply(1:length(r.list2), function(ix) {
    as.vector(t(r.list2[[ix]]))
  })
  temp.R2 <- as.data.frame(matrix(unlist(temp.R2), ncol = n ** 2, byrow = T))
  
  rm(r.list1, r.list2)
  # matplot(temp.R2, pch = 20, col = viridis(144), cex = 0.5)
  
  # Put results into lists:
  temp.S <- list(temp.S1, temp.S2)
  temp.I <- list(temp.I1, temp.I2) # don't even need to return I3 - can just calculate it later
  temp.newI <- list(temp.newI1, temp.newI2)
  temp.R <- list(temp.R1, temp.R2)
  
  if (realdata == FALSE) {
    rec <- list(S = temp.S, I = temp.I)
  } else {
    rec <- list(S = temp.S, I = temp.I, newI = temp.newI, R = temp.R)
  }
  
  rec
}
