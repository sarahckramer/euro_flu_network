
import numpy as np

# SIRS (including air travel and commuting):
def propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0, I_0, popN, D_d, L_d, beta_d, airScale_d, Countries, n_count, airRand):

    cnt = 0

    #print(tmStrt)
    #print(tmRange)
    
    tmStrt = tmStrt - tmRange[0]# + 1 # adjust the index to match beta # QUESTION: Check that these match with tmstrt and tmend in R code
    tmEnd = tmEnd - tmRange[0]# + 1

    #print(tmStrt)
    #print(tmEnd)

    tm_vec = list(range(tmStrt, tmEnd + 1))
    tm_sz = len(tm_vec) + 1 # plus 1 allows us to include the initial conditions

    S_list = np.empty([n_count, n_count, tm_sz])
    S_list[:, :, 0] = S_0
    
    I_list = np.empty([n_count, n_count, tm_sz])
    I_list[:, :, 0] = I_0
    
    newI_list = np.empty([n_count, n_count, tm_sz])
    newI_list[:, :, 0] = np.zeros([n_count, n_count])
    
    #c1_index = [0, 1, 2]
    #c2_index = [3, 4, 5]
    #c3_index = [6, 7, 8]
    #popN = np.asarray(([popN[i] for i in c1_index], [popN[i] for i in c2_index], [popN[i] for i in c3_index]))
    #print(popN)

    for t in tm_vec:
        
        # First, choose correct month's air travel matrix
        t_true = t + tmRange[0]# - 1 # starts at 270 # Question: is subtracting 1 still correct? I think this puts it at the time of the init conditions, but want to check
        #print(t_true)

        if t_true in range(1, 32) or t_true in range((1 + 365), (32 + 365)):
            airRand_temp = airRand['Jan']
        elif t_true in range(32, 60) or t_true in range((32 + 365), (60 + 365)):
            airRand_temp = airRand['Feb']
        elif t_true in range(60, 91) or t_true in range((60 + 365), (91 + 365)):
            airRand_temp = airRand['Mar']
        elif t_true in range(91, 121) or t_true in range((91 + 365), (121 + 365)):
            airRand_temp = airRand['Apr']
        elif t_true in range(121, 152) or t_true in range((121 + 365), (152 + 365)):
            airRand_temp = airRand['May']
        elif t_true in range(152, 182) or t_true in range((152 + 365), (182 + 365)):
            airRand_temp = airRand['Jun']
        elif t_true in range(182, 213) or t_true in range((182 + 365), (213 + 365)):
            airRand_temp = airRand['Jul']
        elif t_true in range(213, 244) or t_true in range((213 + 365), (244 + 365)):
            airRand_temp = airRand['Aug']
        elif t_true in range(244, 274) or t_true in range((244 + 365), (274 + 365)):
            #print('September')
            airRand_temp = airRand['Sep']
        elif t_true in range(274, 305) or t_true in range((274 + 365), (305 + 365)):
            #print('October')
            airRand_temp = airRand['Oct']
        elif t_true in range(305, 335) or t_true in range((305 + 365), (335 + 365)):
            airRand_temp = airRand['Nov']
        elif t_true in range(335, 366) or t_true in range((335 + 365), (366 + 365)):
            airRand_temp = airRand['Dec']
        else:
            print('ERROR: Out of seasonal range!')

        #airRand_temp = airRand['Jan'] # for now, just use one
        # Multiply airRand by airScale:
        airRand_temp = airScale_d * airRand_temp
        #print(airRand_temp.shape)

        # Now move on to normal model code
        cnt = cnt + 1
        #print(cnt)

        # Set S, I, newI
        S = S_list[:, :, cnt - 1]
        I = I_list[:, :, cnt - 1]
        newI = newI_list[:, :, cnt - 1]
        
        ### Daytime ###

        # Step 1 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)
        
        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (1 / 3) * (1 / L_d) * (popN[i, j] - S[i, j] - I[i, j])
                Erecov[i, j] = tmStep * (1 / 3) * (1 / D_d) * I[i, j]
                Einf[i, j] = tmStep * (1 / 3) * ((float(beta_d[Countries[j]][t])) * S[i, j] * sum(I[:, j]) / sum(popN[:, j]))
        
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (S[i, j] / sum(popN[:, j])) * airRand_temp[j, k]
                     iOutTemp = iOutTemp + (I[i, j] / sum(popN[:, j])) * airRand_temp[j, k]

                     for h in range(len(Countries)): # h = current location of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (S[h, k] / sum(popN[:, k]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (I[h, k] / sum(popN[:, k]))

                Estrav[i, j] = (tmStep * (1 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (1 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci# + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        #print(Eimmloss)
        #print(Einf)
        #print(Estrav)
        #print(Ts1)
        #print()




        '''
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
'''

        # Step 2 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (1 / 3) * (1 / L_d) * (popN[i, j] - Ts1[i, j] - Ti1[i, j])
                Einf[i, j] = tmStep * (1 / 3) * ((float(beta_d[Countries[j]][t])) * Ts1[i, j] * sum(Ti1[:, j]) / sum(popN[:, j]))
                Erecov[i, j] = tmStep * (1 / 3) * (1 / D_d) * Ti1[i, j]
                
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts1[i, j] / sum(popN[:, j])) * airRand_temp[j, k]
                     iOutTemp = iOutTemp + (Ti1[i, j] / sum(popN[:, j])) * airRand_temp[j, k]

                     for h in range(len(Countries)): # h = current location of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ts1[h, k] / sum(popN[:, k]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ti1[h, k] / sum(popN[:, k]))

                Estrav[i, j] = (tmStep * (1 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (1 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2

        # Step 3 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (1 / 3) * (1 / L_d) * (popN[i, j] - Ts2[i, j] - Ti2[i, j])
                Einf[i, j] = tmStep * (1 / 3) * ((float(beta_d[Countries[j]][t])) * Ts2[i, j] * sum(Ti2[:, j]) / sum(popN[:, j]))
                Erecov[i, j] = tmStep * (1 / 3) * (1 / D_d) * Ti2[i, j]
                
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts2[i, j] / sum(popN[:, j])) * airRand_temp[j, k]
                     iOutTemp = iOutTemp + (Ti2[i, j] / sum(popN[:, j])) * airRand_temp[j, k]

                     for h in range(len(Countries)): # h = current location of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ts2[h, k] / sum(popN[:, k]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ti2[h, k] / sum(popN[:, k]))

                Estrav[i, j] = (tmStep * (1 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (1 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci

        Ts3 = S + sk3
        Ti3 = I + ik3

        # Step 4 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (1 / 3) * (1 / L_d) * (popN[i, j] - Ts3[i, j] - Ti3[i, j])
                Einf[i, j] = tmStep * (1 / 3) * ((float(beta_d[Countries[j]][t])) * Ts3[i, j] * sum(Ti3[:, j]) / sum(popN[:, j]))
                Erecov[i, j] = tmStep * (1 / 3) * (1 / D_d) * Ti3[i, j]
                
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts3[i, j] / sum(popN[:, j])) * airRand_temp[j, k]
                     iOutTemp = iOutTemp + (Ti3[i, j] / sum(popN[:, j])) * airRand_temp[j, k]

                     for h in range(len(Countries)): # h = current location of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ts3[h, k] / sum(popN[:, k]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ti3[h, k] / sum(popN[:, k]))

                Estrav[i, j] = (tmStep * (1 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (1 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci

        S = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6
        
        #print(Eimmloss)
        #print(Einf)
        #print(Estrav)
        #print(S)
        #print()
        
        ### Nighttime ###

        # Step 1 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (2 / 3) * (1 / L_d) * (popN[i, j] - S[i, j] - I[i, j])
                Erecov[i, j] = tmStep * (2 / 3) * (1 / D_d) * I[i, j]
                Einf[i, j] = tmStep * (2 / 3) * ((float(beta_d[Countries[i]][t])) * S[i, j] * sum(I[i, :]) / sum(popN[i, :]))
        
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (S[i, j] / sum(popN[i, :])) * airRand_temp[i, k]
                     iOutTemp = iOutTemp + (I[i, j] / sum(popN[i, :])) * airRand_temp[i, k]

                     for h in range(len(Countries)): # h = actual compartment of those living in k
                         # sInTemp = sInTemp + (popN[i, j] / sum(popN[:, j])) * airRand_temp[k, j] * (Ts3[h, k] / sum(popN[:, k])) # Daytime code
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (S[k, h] / sum(popN[k, ]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (I[k, h] / sum(popN[k, ]))

                Estrav[i, j] = (tmStep * (2 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (2 / 3)) * (iInTemp - iOutTemp)

                #print(sInTemp)
                #print(sOutTemp)
                
        #print('NEXT')

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci# + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        #print(Eimmloss)
        #print(Einf)
        #print(Estrav)
        #print(Ts1)
        #print()

        # Step 2 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (2 / 3) * (1 / L_d) * (popN[i, j] - Ts1[i, j] - Ti1[i, j])
                Erecov[i, j] = tmStep * (2 / 3) * (1 / D_d) * Ti1[i, j]
                Einf[i, j] = tmStep * (2 / 3) * ((float(beta_d[Countries[i]][t])) * Ts1[i, j] * sum(Ti1[i, :]) / sum(popN[i, :]))
        
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts1[i, j] / sum(popN[i, :])) * airRand_temp[i, k]
                     iOutTemp = iOutTemp + (Ti1[i, j] / sum(popN[i, :])) * airRand_temp[i, k]

                     for h in range(len(Countries)): # h = actual compartment of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ts1[k, h] / sum(popN[k, ]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ti1[k, h] / sum(popN[k, ]))

                Estrav[i, j] = (tmStep * (2 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (2 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci# + Eitrav

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2

        # Step 3 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (2 / 3) * (1 / L_d) * (popN[i, j] - Ts2[i, j] - Ti2[i, j])
                Erecov[i, j] = tmStep * (2 / 3) * (1 / D_d) * Ti2[i, j]
                Einf[i, j] = tmStep * (2 / 3) * ((float(beta_d[Countries[i]][t])) * Ts2[i, j] * sum(Ti2[i, :]) / sum(popN[i, :]))
        
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts2[i, j] / sum(popN[i, :])) * airRand_temp[i, k]
                     iOutTemp = iOutTemp + (Ti2[i, j] / sum(popN[i, :])) * airRand_temp[i, k]

                     for h in range(len(Countries)): # h = actual compartment of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ts2[k, h] / sum(popN[k, ]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ti2[k, h] / sum(popN[k, ]))

                Estrav[i, j] = (tmStep * (2 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (2 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci# + Eitrav

        Ts3 = S + sk3
        Ti3 = I + ik3

        # Step 4 #
        Eimmloss = np.zeros(S.shape)
        Einf = np.zeros(S.shape)
        Erecov = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = workplace
                Eimmloss[i, j] = tmStep * (2 / 3) * (1 / L_d) * (popN[i, j] - Ts3[i, j] - Ti3[i, j])
                Erecov[i, j] = tmStep * (2 / 3) * (1 / D_d) * Ti3[i, j]
                Einf[i, j] = tmStep * (2 / 3) * ((float(beta_d[Countries[i]][t])) * Ts3[i, j] * sum(Ti3[i, :]) / sum(popN[i, :]))
        
        # incorporate travel:
        Estrav = np.zeros(S.shape)
        Eitrav = np.zeros(S.shape)

        for i in range(len(Countries)): # i = home
            for j in range(len(Countries)): # j = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for k in range(len(Countries)): # k = travel to/from
                     sOutTemp = sOutTemp + (Ts3[i, j] / sum(popN[i, :])) * airRand_temp[i, k]
                     iOutTemp = iOutTemp + (Ti3[i, j] / sum(popN[i, :])) * airRand_temp[i, k]

                     for h in range(len(Countries)): # h = actual compartment of those living in k
                         sInTemp = sInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ts3[k, h] / sum(popN[k, ]))
                         iInTemp = iInTemp + (popN[i, j] / sum(popN[i, :])) * airRand_temp[k, i] * (Ti3[k, h] / sum(popN[k, ]))

                Estrav[i, j] = (tmStep * (2 / 3)) * (sInTemp - sOutTemp)
                Eitrav[i, j] = (tmStep * (2 / 3)) * (iInTemp - iOutTemp)

        for i in range(len(Countries)):
            for j in range(len(Countries)):
                if Eimmloss[i, j] < 0.0:
                    Eimmloss[i, j] = 0.0
                if Einf[i, j] < 0.0:
                    Einf[i, j] = 0.0
                if Erecov[i, j] < 0.0:
                    Erecov[i, j] = 0.0
                    
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci# + Eitrav

        S = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6

        S.resize((1, 9))
        I.resize((1, 9))
        newI.resize((1, 9))

        S_list[cnt, ] = S
        I_list[cnt, ] = I
        newI_list[cnt, ] = newI

    # Format for returning?
    newI_count = np.zeros((tm_sz, len(Countries)))
    newI_count[:, 0] = newI_list[:, 0] + newI_list[:, 1] + newI_list[:, 2]
    newI_count[:, 1] = newI_list[:, 3] + newI_list[:, 4] + newI_list[:, 5]
    newI_count[:, 2] = newI_list[:, 6] + newI_list[:, 7] + newI_list[:, 8]
    
    return(newI_count)
