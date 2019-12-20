
import numpy as np

# Run full ensemble:
def run_ensemble(tmStrt, tmEnd, tmStep, tmRange, S0, I0, popN, D, L, beta, airScale, Countries, n, airRand, num_ens):
    xprior_temp = np.empty([np.square(n) * 3, num_ens])

    for i in range(num_ens):
        Sr_tmp = propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0 = S0[:, :, i], I_0 = I0[:, :, i], popN = popN,
                                D_d = D[i], L_d = L[i], beta_d = beta[:, :, i], airScale_d = airScale[i],
                                Countries = Countries, n_count = n, airRand = airRand)

##        print(np.array((Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)]), Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)]),
##               Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)]))).reshape([1, np.square(n) * 3]))

        xprior_temp[:, i] = np.array((Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)]),
                                      Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)]),
                                      Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)]))).reshape([1, np.square(n) * 3])

        #xprior[S0_indices, i, 0] = Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)])
        #xprior[I0_indices, i, 0] = Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)])
        #xprior[newI_indices, i, 0] = Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)])

    return(xprior_temp)

# Or try running in parallel?:
def run_ensemble2(i, tmStrt, tmEnd, tmStep, tmRange, S0, I0, popN, D, L, beta, airScale, Countries, n, airRand):

    S_0 = S0[:, :, i]
    I_0 = I0[:, :, i]
    D_d = D[i]
    L_d = L[i]
    beta_d = beta[:, :, i]
    airScale_d = airScale[i]

    Sr_tmp = propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0, I_0, popN, D_d, L_d, beta_d, airScale_d,
                                Countries = Countries, n_count = n, airRand = airRand)

    return(np.array((Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)]),
                    Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)]),
                    Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)]))).reshape([1, np.square(n) * 3]))

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
    #print(tm_sz)

    S_list = np.empty([n_count, n_count, tm_sz])
    S_list[:, :, 0] = S_0
    #print(S_list.shape)

    I_list = np.empty([n_count, n_count, tm_sz])
    I_list[:, :, 0] = I_0

    newI_list = np.empty([n_count, n_count, tm_sz])
    newI_list[:, :, 0] = np.zeros([n_count, n_count])

    for t in tm_vec:

        # First, choose correct month's air travel matrix
        t_true = t + tmRange[0]# - 1 # starts at 270 # Question: is subtracting 1 still correct? I think this puts it at the time of the init conditions, but want to check
        #print(t_true) # pretty sure this is correct! first value of 2010-11 corresponds to third day of October

        if t_true in range(0, 31) or t_true in range((0 + 365), (31 + 365)):
            airRand_temp = airRand['Jan']
        elif t_true in range(31, 59) or t_true in range((31 + 365), (59 + 365)):
            airRand_temp = airRand['Feb']
        elif t_true in range(59, 90) or t_true in range((59 + 365), (90 + 365)):
            airRand_temp = airRand['Mar']
        elif t_true in range(90, 120) or t_true in range((90 + 365), (120 + 365)):
            airRand_temp = airRand['Apr']
        elif t_true in range(120, 151) or t_true in range((120 + 365), (151 + 365)):
            airRand_temp = airRand['May']
        elif t_true in range(151, 181) or t_true in range((151 + 365), (181 + 365)):
            airRand_temp = airRand['Jun']
        elif t_true in range(181, 212) or t_true in range((181 + 365), (212 + 365)):
            airRand_temp = airRand['Jul']
        elif t_true in range(212, 243) or t_true in range((212 + 365), (243 + 365)):
            airRand_temp = airRand['Aug']
        elif t_true in range(243, 273) or t_true in range((243 + 365), (273 + 365)):
            #print('September')
            airRand_temp = airRand['Sep']
        elif t_true in range(273, 304) or t_true in range((273 + 365), (304 + 365)):
            #print('October')
            airRand_temp = airRand['Oct']
        elif t_true in range(304, 334) or t_true in range((304 + 365), (334 + 365)):
            airRand_temp = airRand['Nov']
        elif t_true in range(334, 365) or t_true in range((334 + 365), (365 + 365)):
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
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - S - I)
        Einf = tmStep * (1 / 3) * np.transpose(np.transpose(S) * np.reshape(beta_d[t, :] * np.sum(I, axis = 0) / np.sum(popN, axis = 0), [12, 1]))
        Erecov = tmStep * (1 / 3) * (I / D_d)

        '''
        Einf_check = np.zeros(S.shape)
        for k in range(len(Countries)):
            for n in range(len(Countries)):
                Einf_check[k, n] = tmStep * (1 / 3) * beta_d[t, n] * S[k, n] * sum(I[:, n]) / sum(popN[:, n])
        del k
        del n
        '''

        #print(Einf)
        #print(Einf_check)
        #print(np.allclose(Einf, Einf_check))
        #print(np.array_equiv(Einf, Einf_check))
        #print(np.array_equal(Einf, Einf_check))

        '''
        for i in range(12):
            for j in range(12):
                print(Einf[i, j] == Einf_check[i, j])
                print(Einf[i, j])
                print(Einf_check[i, j])
                print()
        # differences are way past the decimal point - close enough
        '''

        #print(Eimmloss)
        #print(Einf)
        #print(Erecov)

        # incorporate travel:
        '''
        EstravCheck = np.zeros(S.shape)
        EitravCheck = np.zeros(S.shape)

        sOutCheck = np.zeros(S.shape)
        sInCheck = np.zeros(S.shape)
        iOutCheck = np.zeros(S.shape)
        iInCheck = np.zeros(S.shape)

        for k in range(len(Countries)): # k = home
            for n in range(len(Countries)): # n = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for m in range(len(Countries)): # m = travel to/from
                     sOutTemp = sOutTemp + (S[k, n] / sum(popN[:, n])) * airRand_temp[n, m]
                     iOutTemp = iOutTemp + (I[k, n] / sum(popN[:, n])) * airRand_temp[n, m]

                     for h in range(len(Countries)): # h = current location of those living in k
                         sInTemp = sInTemp + (popN[k, n] / sum(popN[:, n])) * airRand_temp[m, n] * (S[h, m] / sum(popN[:, m]))
                         iInTemp = iInTemp + (popN[k, n] / sum(popN[:, n])) * airRand_temp[m, n] * (I[h, m] / sum(popN[:, m]))

                sOutCheck[k, n] = sOutTemp
                sInCheck[k, n] = sInTemp
                iOutCheck[k, n] = iOutTemp
                iInCheck[k, n] = iInTemp
                
                EstravCheck[k, n] = (tmStep * (1 / 3)) * (sInTemp - sOutTemp)
                EitravCheck[k, n] = (tmStep * (1 / 3)) * (iInTemp - iOutTemp)
        del k
        del n
        del m
        del h
        '''

        sOut = (S / np.sum(popN, 0)) * np.sum(airRand_temp, 1) # works due to "broadcasting"
        iOut = (I / np.sum(popN, 0)) * np.sum(airRand_temp, 1)

        sIn = np.tile(np.matmul(np.sum(S, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))
        iIn = np.tile(np.matmul(np.sum(I, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)
        #print(np.allclose(Estrav, EstravCheck))
        #print(np.allclose(Eitrav, EitravCheck))

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci# + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        #print(Ts1)
        #print(Ti1)
        #print()

        #print(Eimmloss)
        #print(Einf)
        #print(Estrav)
        #print(Ts1)
        #print()

        # Step 2 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts1 - Ti1)
        Einf = tmStep * (1 / 3) * np.transpose(np.transpose(Ts1) * np.reshape(beta_d[t, :] * np.sum(Ti1, axis = 0) / np.sum(popN, axis = 0), [12, 1]))
        Erecov = tmStep * (1 / 3) * (Ti1 / D_d)

        sOut = (Ts1 / np.sum(popN, 0)) * np.sum(airRand_temp, 1) # works due to "broadcasting"
        iOut = (Ti1 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.tile(np.matmul(np.sum(Ts1, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))
        iIn = np.tile(np.matmul(np.sum(Ti1, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)
        #print(Estrav.shape)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci# + Eitrav

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2
        #print(Ts2)
        #print(Ti2)
        #print()

        # Step 3 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts2 - Ti2)
        Einf = tmStep * (1 / 3) * np.transpose(np.transpose(Ts2) * np.reshape(beta_d[t, :] * np.sum(Ti2, axis = 0) / np.sum(popN, axis = 0), [12, 1]))
        Erecov = tmStep * (1 / 3) * (Ti2 / D_d)

        sOut = (Ts2 / np.sum(popN, 0)) * np.sum(airRand_temp, 1) # works due to "broadcasting"
        iOut = (Ti2 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.tile(np.matmul(np.sum(Ts2, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))
        iIn = np.tile(np.matmul(np.sum(Ti2, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci# + Eitrav

        Ts3 = S + sk3
        Ti3 = I + ik3

        #print(Ts3)
        #print(Ti3)
        #print()

        # Step 4 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts3 - Ti3)
        Einf = tmStep * (1 / 3) * np.transpose(np.transpose(Ts3) * np.reshape(beta_d[t, :] * np.sum(Ti3, axis = 0) / np.sum(popN, axis = 0), [12, 1]))
        Erecov = tmStep * (1 / 3) * (Ti3 / D_d)

        sOut = (Ts3 / np.sum(popN, 0)) * np.sum(airRand_temp, 1) # works due to "broadcasting"
        iOut = (Ti3 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.tile(np.matmul(np.sum(Ts3, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))
        iIn = np.tile(np.matmul(np.sum(Ti3, 0) / np.sum(popN, 0), airRand_temp), (n_count, 1)) * popN / np.tile(np.sum(popN, 0), (n_count, 1))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci# + Eitrav

        S = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6

        #print(S)
        #print(I)
        #print(newI)
        #print()

        #print(Eimmloss)
        #print(Einf)
        #print(Estrav)
        #print(S)
        #print()

        ### Nighttime ###
        # Step 1 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - S - I)
        Einf = tmStep * (2 / 3) * (S * np.reshape(beta_d[t, :] * np.sum(I, axis = 1) / np.sum(popN, axis = 1), [12, 1]))
        Erecov = tmStep * (2 / 3) * (I / D_d)

        '''
        Einf_check = np.zeros(S.shape)
        for k in range(len(Countries)):
            for n in range(len(Countries)):
                Einf_check[k, n] = tmStep * (2 / 3) * beta_d[t, k] * S[k, n] * sum(I[k, :]) / sum(popN[k, :])
        del k
        del n

        #print(Einf)
        #print(Einf_check)
        #print(np.allclose(Einf, Einf_check))
        #print(np.array_equiv(Einf, Einf_check))
        #print(np.array_equal(Einf, Einf_check))
        '''

        #print(Eimmloss)
        #print(Einf)
        #print(Erecov)
        #print()

        # incorporate travel:
        '''
        EstravCheck = np.zeros(S.shape)
        EitravCheck = np.zeros(S.shape)

        sOutCheck = np.zeros(S.shape)
        sInCheck = np.zeros(S.shape)
        iOutCheck = np.zeros(S.shape)
        iInCheck = np.zeros(S.shape)

        for k in range(len(Countries)): # k = home
            for n in range(len(Countries)): # n = work
                sOutTemp = 0
                iOutTemp = 0
                sInTemp = 0
                iInTemp = 0

                for h in range(len(Countries)): # h = travel to/from
                     sOutTemp = sOutTemp + (S[k, n] / sum(popN[k, :])) * airRand_temp[k, h]
                     iOutTemp = iOutTemp + (I[k, n] / sum(popN[k, :])) * airRand_temp[k, h]

                     for m in range(len(Countries)): # m = working location of those living in h
                         sInTemp = sInTemp + (popN[k, n] / sum(popN[k, :])) * airRand_temp[h, k] * (S[h, m] / sum(popN[h, :]))
                         iInTemp = iInTemp + (popN[k, n] / sum(popN[k, :])) * airRand_temp[h, k] * (I[h, m] / sum(popN[h, :]))

                sOutCheck[k, n] = sOutTemp
                sInCheck[k, n] = sInTemp
                iOutCheck[k, n] = iOutTemp
                iInCheck[k, n] = iInTemp

                EstravCheck[k, n] = (tmStep * (2 / 3)) * (sInTemp - sOutTemp)
                EitravCheck[k, n] = (tmStep * (2 / 3)) * (iInTemp - iOutTemp)
        del k
        del n
        del m
        del h
        '''

        sOut = np.transpose(np.transpose(S) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(I) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.transpose(np.tile(np.matmul(np.sum(S, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))
        iIn = np.transpose(np.tile(np.matmul(np.sum(I, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        #print(Estrav)
        #print(EstravCheck)

        #print(np.allclose(Estrav, EstravCheck))
        #print(np.allclose(Eitrav, EitravCheck))

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci# + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        # Step 2 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts1 - Ti1)
        Einf = tmStep * (2 / 3) * (Ts1 * np.reshape(beta_d[t, :] * np.sum(Ti1, axis = 1) / np.sum(popN, axis = 1), [12, 1]))
        Erecov = tmStep * (2 / 3) * (Ti1 / D_d)

        sOut = np.transpose(np.transpose(Ts1) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti1) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.transpose(np.tile(np.matmul(np.sum(Ts1, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))
        iIn = np.transpose(np.tile(np.matmul(np.sum(Ti1, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci# + Eitrav

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2

        # Step 3 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts2 - Ti2)
        Einf = tmStep * (2 / 3) * (Ts2 * np.reshape(beta_d[t, :] * np.sum(Ti2, axis = 1) / np.sum(popN, axis = 1), [12, 1]))
        Erecov = tmStep * (2 / 3) * (Ti2 / D_d)

        sOut = np.transpose(np.transpose(Ts2) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti2) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.transpose(np.tile(np.matmul(np.sum(Ts2, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))
        iIn = np.transpose(np.tile(np.matmul(np.sum(Ti2, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci# + Eitrav

        Ts3 = S + sk3
        Ti3 = I + ik3

        # Step 4 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts3 - Ti3)
        Einf = tmStep * (2 / 3) * (Ts3 * np.reshape(beta_d[t, :] * np.sum(Ti3, axis = 1) / np.sum(popN, axis = 1), [12, 1]))
        Erecov = tmStep * (2 / 3) * (Ti3 / D_d)

        sOut = np.transpose(np.transpose(Ts3) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti3) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.transpose(np.tile(np.matmul(np.sum(Ts3, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))
        iIn = np.transpose(np.tile(np.matmul(np.sum(Ti3, 1) / np.sum(popN, 1), airRand_temp), (n_count, 1))) * popN / np.transpose(np.tile(np.sum(popN, 1), (n_count, 1)))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss[np.where(Eimmloss < 0)] = 0 # set any values below 0 to 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci# + Eitrav

        S_list[:, :, cnt] = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I_list[:, :, cnt] = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI_list[:, :, cnt] = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6

        #print(S_list[:, :, cnt])
        #print()

    #print(S_list.shape)
    #print(newI_list.shape)

    '''
    # Format for returning?
    newI_count = np.zeros((tm_sz, len(Countries)))
    newI_count[:, 0] = newI_list[:, 0] + newI_list[:, 1] + newI_list[:, 2]
    newI_count[:, 1] = newI_list[:, 3] + newI_list[:, 4] + newI_list[:, 5]
    newI_count[:, 2] = newI_list[:, 6] + newI_list[:, 7] + newI_list[:, 8]
    
    return(newI_count)
    '''
    return(S_list, I_list, newI_list)
