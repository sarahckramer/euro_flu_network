
# Note: During day, we care about where people work (columns). During night, we care
# about where they live (rows).

propagateToySIRS <- function(tm_strt, tm_end, tm_step, S0, I0, N, D, L, beta, airScale, nu, realdata = FALSE, prohibAir = FALSE) {
  cnt <- 1
  
  tm_strt <- tm_strt - tm.range[1] + 1 # adjust the index to match beta
  tm_end <- tm_end - tm.range[1] + 1
  
  tm_vec <- seq(tm_strt, tm_end, by = tm_step)
  tm_sz <- length(tm_vec) + 1 # include initial conditions
  
  S.list = I.list = newI.list = N.list = vector('list', tm_sz)
  S.list[[1]] <- S0; I.list[[1]] <- I0
  newI.list[[1]] <- matrix(0, nrow = n, ncol = n)
  
  # run continuously
  for (t in tm_vec) {
    
    # First, choose t.rand by month:
    t.true <- t + tm.range[1] - 1
    if (t.true %in% c(1:31, 1:31 + 365)) {
      # print('January')
      load('formatTravelData/formattedData/air_1_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(32:59, 32:59 + 365)) {
      # print('February')
      load('formatTravelData/formattedData/air_2_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(60:90, 60:90 + 365)) {
      # print('March')
      load('formatTravelData/formattedData/air_3_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(91:120, 91:120 + 365)) {
      # print('April')
      load('formatTravelData/formattedData/air_4_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(121:151, 121:151 + 365)) {
      # print('May')
      load('formatTravelData/formattedData/air_5_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(152:181, 152:181 + 365)) {
      # print('June')
      load('formatTravelData/formattedData/air_6_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(182:212, 182:212 + 365)) {
      # print('July')
      load('formatTravelData/formattedData/air_7_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(213:243, 213:243 + 365)) {
      # print('August')
      load('formatTravelData/formattedData/air_8_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(244:273, 244:273 + 365)) {
      # print('September')
      load('formatTravelData/formattedData/air_9_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(274:304, 274:304 + 365)) {
      # print('October')
      load('formatTravelData/formattedData/air_10_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(305:334, 305:334 + 365)) {
      # print('November')
      load('formatTravelData/formattedData/air_11_05-07.RData')
      a.temp <- a.temp.sym[countries, countries]; rm(a.temp.sym)
    } else if (t.true %in% c(335:365, 335:365 + 365)) {
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
    all.rand <- all.rand + nu * t.comm.sym
    
    # Run model forward:
    cnt <- cnt + 1
    
    # set t-1 S, I, newI:
    S <- S.list[[cnt - 1]]; I <- I.list[[cnt - 1]]; newI <- newI.list[[cnt - 1]]
    
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
