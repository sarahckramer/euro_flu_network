# Run networked SIRS model #

import sys
import numpy as np
from numba import jit, prange
from scipy import *


# Run full ensemble:
@jit(nopython=True, nogil=True, parallel=True, cache=True)
def run_ensemble(tmStrt, tmEnd, tmStep, tmRange, S0, I0, popN, D, L, beta, airScale, n, airRand, num_ens):
    xprior_temp = np.zeros((num_ens, np.square(n) * 3), dtype=np.float64)

    for i in prange(num_ens):
        Sr_tmp = propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0=S0[:, :, i], I_0=I0[:, :, i], popN=popN[i],
                                D_d=D[i], L_d=L[i], beta_d=beta[:, :, i], airScale_d=airScale[i],
                                n_count=n, airRand=airRand)

        xprior_temp_i = Sr_tmp[0][Sr_tmp[0].shape[0] - 1].reshape(np.square(n))
        xprior_temp_i = np.append(xprior_temp_i, Sr_tmp[1][Sr_tmp[1].shape[0] - 1].reshape(np.square(n)))
        xprior_temp_i = np.append(xprior_temp_i, Sr_tmp[2][Sr_tmp[2].shape[0] - 1].reshape(np.square(n)))
        xprior_temp[i] = xprior_temp_i

    return np.transpose(xprior_temp)


# noinspection PyShadowingNames
@jit(nopython=True, nogil=True, parallel=True, cache=True)
def run_forecast(tmStrt, tmEnd, tmStep, tmRange, S0, I0, popN, D, L, beta, airScale, n, airRand,
                 num_ens, nfc, tm_step):
    fcast_temp = np.zeros((np.square(n) * 3, num_ens, nfc), dtype=np.float64)

    for i in prange(num_ens):
        Sr_tmp = propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0=S0[:, :, i], I_0=I0[:, :, i], popN=popN[i],
                                D_d=D[i], L_d=L[i], beta_d=beta[:, :, i], airScale_d=airScale[i],
                                n_count=n, airRand=airRand)

        statesresS = np.zeros((nfc, np.square(n)), dtype=np.float64)
        statesresI = np.zeros((nfc, np.square(n)), dtype=np.float64)
        statesresnewI = np.zeros((nfc, np.square(n)), dtype=np.float64)
        prev_newI = Sr_tmp[2][0].reshape(np.square(n))

        for j in range(0, nfc):
            k = tm_step * (j + 1)
            statesresS[j] = Sr_tmp[0][k].reshape(np.square(n))
            statesresI[j] = Sr_tmp[1][k].reshape(np.square(n))

            curr_newI = Sr_tmp[2][k].reshape(np.square(n))
            statesresnewI[j] = curr_newI - prev_newI
            prev_newI = curr_newI

        fcast_temp_i = np.transpose(statesresS)
        fcast_temp_i = np.append(fcast_temp_i, np.transpose(statesresI), axis=0)
        fcast_temp_i = np.append(fcast_temp_i, np.transpose(statesresnewI), axis=0)

        fcast_temp[:, i] = fcast_temp_i

    return fcast_temp


# noinspection PyShadowingNames
@jit(nopython=True, nogil=True, cache=True)
def remove_negatives(mat):
    i, j = np.where(mat < 0.0)
    if len(i) > 0:
        for ix in i:
            for jx in j:
                mat[ix, jx] = 0.0
    return mat


# SIRS (including air travel and commuting):
@jit(nopython=True, nogil=True, cache=True)
# @njit
def propagate_SIRS(tmStrt, tmEnd, tmStep, tmRange, S_0, I_0, popN, D_d, L_d, beta_d, airScale_d, n_count, airRand):
    cnt = 0

    tmStrt = tmStrt - tmRange[0]  # + 1 # adjust the index to match beta
    tmEnd = tmEnd - tmRange[0]  # + 1

    tm_vec = list(range(tmStrt, tmEnd + 1))
    tm_sz = len(tm_vec) + 1  # plus 1 allows us to include the initial conditions

    # n_count = np.float64(n_count)
    # tm_sz = np.float64(tm_sz)

    S_list = np.zeros((tm_sz, n_count, n_count), dtype=np.float64)
    S_list[0] = S_0

    I_list = np.zeros((tm_sz, n_count, n_count), dtype=np.float64)
    I_list[0] = I_0

    newI_list = np.zeros((tm_sz, n_count, n_count), dtype=np.float64)
    newI_list[0] = np.zeros((n_count, n_count), dtype=np.float64)

    for t in tm_vec:

        # First, choose correct month's air travel matrix
        t_true = t + tmRange[0]  # - 1 # starts at 270
        # is subtracting 1 still correct? I think this puts it at the time of the init conditions,
        # but want to check
        # pretty sure this is correct! first value of 2010-11 corresponds to third day of October

        if t_true in range(0, 31) or t_true in range((0 + 365), (31 + 365)):
            airRand_temp = airRand[0]
        elif t_true in range(31, 59) or t_true in range((31 + 365), (59 + 365)):
            airRand_temp = airRand[1]
        elif t_true in range(59, 90) or t_true in range((59 + 365), (90 + 365)):
            airRand_temp = airRand[2]
        elif t_true in range(90, 120) or t_true in range((90 + 365), (120 + 365)):
            airRand_temp = airRand[3]
        elif t_true in range(120, 151) or t_true in range((120 + 365), (151 + 365)):
            airRand_temp = airRand[4]
        elif t_true in range(151, 181) or t_true in range((151 + 365), (181 + 365)):
            airRand_temp = airRand[5]
        elif t_true in range(181, 212) or t_true in range((181 + 365), (212 + 365)):
            airRand_temp = airRand[6]
        elif t_true in range(212, 243) or t_true in range((212 + 365), (243 + 365)):
            airRand_temp = airRand[7]
        elif t_true in range(243, 273) or t_true in range((243 + 365), (273 + 365)):
            airRand_temp = airRand[8]
        elif t_true in range(273, 304) or t_true in range((273 + 365), (304 + 365)):
            airRand_temp = airRand[9]
        elif t_true in range(304, 334) or t_true in range((304 + 365), (334 + 365)):
            airRand_temp = airRand[10]
        elif t_true in range(334, 365) or t_true in range((334 + 365), (365 + 365)):
            airRand_temp = airRand[11]
        else:
            # print('ERROR: Out of seasonal range!')
            raise Exception('t_true is out of seasonal range')

        # Multiply airRand by airScale:
        airRand_temp = airScale_d * airRand_temp  # * 0

        # Now move on to normal model mainCode
        cnt = cnt + 1

        # Set S, I, newI
        S = S_list[cnt - 1]
        I = I_list[cnt - 1]
        newI = newI_list[cnt - 1]

        # ### Daytime ###
        # Step 1 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - S - I)
        Einf = tmStep * (1 / 3) * np.transpose(
            np.transpose(S) * np.reshape(beta_d[t, :] * np.sum(I, axis=0) / np.sum(popN, axis=0), (12, 1)))
        Erecov = tmStep * (1 / 3) * (I / D_d)

        # incorporate travel:
        sOut = (S / np.sum(popN, 0)) * np.sum(airRand_temp, 1)  # works due to "broadcasting"
        iOut = (I / np.sum(popN, 0)) * np.sum(airRand_temp, 1)

        sIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(S, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))
        iIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(I, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)
        # Eimmloss[np.where(Eimmloss < 0.0)[0], :] = 0.0 # set any values below 0 to 0
        # Einf[np.where(Einf < 0)] = 0
        # Erecov[np.where(Erecov < 0)] = 0

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci  # + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        # Step 2 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts1 - Ti1)
        Einf = tmStep * (1 / 3) * np.transpose(
            np.transpose(Ts1) * np.reshape(beta_d[t, :] * np.sum(Ti1, axis=0) / np.sum(popN, axis=0), (12, 1)))
        Erecov = tmStep * (1 / 3) * (Ti1 / D_d)

        sOut = (Ts1 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)  # works due to "broadcasting"
        iOut = (Ti1 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ts1, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))
        iIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ti1, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)
        # print(Estrav.shape)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci  # + Eitrav

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2

        # Step 3 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts2 - Ti2)
        Einf = tmStep * (1 / 3) * np.transpose(
            np.transpose(Ts2) * np.reshape(beta_d[t, :] * np.sum(Ti2, axis=0) / np.sum(popN, axis=0), (12, 1)))
        Erecov = tmStep * (1 / 3) * (Ti2 / D_d)

        sOut = (Ts2 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)  # works due to "broadcasting"
        iOut = (Ti2 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ts2, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))
        iIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ti2, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci  # + Eitrav

        Ts3 = S + sk3
        Ti3 = I + ik3

        # Step 4 #
        Eimmloss = tmStep * (1 / 3) * (1 / L_d) * (popN - Ts3 - Ti3)
        Einf = tmStep * (1 / 3) * np.transpose(
            np.transpose(Ts3) * np.reshape(beta_d[t, :] * np.sum(Ti3, axis=0) / np.sum(popN, axis=0), (12, 1)))
        Erecov = tmStep * (1 / 3) * (Ti3 / D_d)

        sOut = (Ts3 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)  # works due to "broadcasting"
        iOut = (Ti3 / np.sum(popN, 0)) * np.sum(airRand_temp, 1)
        sIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ts3, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))
        iIn = np.transpose(np.reshape(np.repeat(np.dot(np.sum(Ti3, 0) / np.sum(popN, 0), airRand_temp), n_count),
                                      (n_count, n_count))) * popN / np.transpose(np.reshape(np.repeat(np.sum(popN, 0), n_count), (n_count, n_count)))

        Estrav = (tmStep * (1 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (1 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci  # + Eitrav

        S = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6

        # ### Nighttime ###
        # Step 1 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - S - I)
        Einf = tmStep * (2 / 3) * (S * np.reshape(beta_d[t, :] * np.sum(I, axis=1) / np.sum(popN, axis=1), (12, 1)))
        Erecov = tmStep * (2 / 3) * (I / D_d)

        # incorporate travel:
        sOut = np.transpose(np.transpose(S) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(I) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.reshape(np.repeat(np.dot(np.sum(S, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))
        iIn = np.reshape(np.repeat(np.dot(np.sum(I, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk1 = smcl - smci + Estrav
        ik1 = smci - smcr + Eitrav
        ik1a = smci  # + Eitrav

        Ts1 = S + sk1 / 2
        Ti1 = I + ik1 / 2

        # Step 2 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts1 - Ti1)
        Einf = tmStep * (2 / 3) * (Ts1 * np.reshape(beta_d[t, :] * np.sum(Ti1, axis=1) / np.sum(popN, axis=1), (12, 1)))
        Erecov = tmStep * (2 / 3) * (Ti1 / D_d)

        sOut = np.transpose(np.transpose(Ts1) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti1) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.reshape(np.repeat(np.dot(np.sum(Ts1, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))
        iIn = np.reshape(np.repeat(np.dot(np.sum(Ti1, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk2 = smcl - smci + Estrav
        ik2 = smci - smcr + Eitrav
        ik2a = smci  # + Eitrav

        Ts2 = S + sk2 / 2
        Ti2 = I + ik2 / 2

        # Step 3 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts2 - Ti2)
        Einf = tmStep * (2 / 3) * (Ts2 * np.reshape(beta_d[t, :] * np.sum(Ti2, axis=1) / np.sum(popN, axis=1), (12, 1)))
        Erecov = tmStep * (2 / 3) * (Ti2 / D_d)

        sOut = np.transpose(np.transpose(Ts2) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti2) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.reshape(np.repeat(np.dot(np.sum(Ts2, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))
        iIn = np.reshape(np.repeat(np.dot(np.sum(Ti2, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk3 = smcl - smci + Estrav
        ik3 = smci - smcr + Eitrav
        ik3a = smci  # + Eitrav

        Ts3 = S + sk3
        Ti3 = I + ik3

        # Step 4 #
        Eimmloss = tmStep * (2 / 3) * (1 / L_d) * (popN - Ts3 - Ti3)
        Einf = tmStep * (2 / 3) * (Ts3 * np.reshape(beta_d[t, :] * np.sum(Ti3, axis=1) / np.sum(popN, axis=1), (12, 1)))
        Erecov = tmStep * (2 / 3) * (Ti3 / D_d)

        sOut = np.transpose(np.transpose(Ts3) / np.sum(popN, 1) * np.sum(airRand_temp, 1))
        iOut = np.transpose(np.transpose(Ti3) / np.sum(popN, 1) * np.sum(airRand_temp, 1))

        sIn = np.reshape(np.repeat(np.dot(np.sum(Ts3, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))
        iIn = np.reshape(np.repeat(np.dot(np.sum(Ti3, 1) / np.sum(popN, 1), airRand_temp), n_count),
                         (n_count, n_count)) * popN / np.reshape(np.repeat(np.sum(popN, 1), n_count), (n_count, n_count))

        Estrav = (tmStep * (2 / 3)) * (sIn - sOut)
        Eitrav = (tmStep * (2 / 3)) * (iIn - iOut)

        Eimmloss = remove_negatives(Eimmloss)
        Einf = remove_negatives(Einf)
        Erecov = remove_negatives(Erecov)

        smcl = Eimmloss
        smci = Einf
        smcr = Erecov

        sk4 = smcl - smci + Estrav
        ik4 = smci - smcr + Eitrav
        ik4a = smci  # + Eitrav

        S_list[cnt] = S + sk1 / 6 + sk2 / 3 + sk3 / 3 + sk4 / 6
        I_list[cnt] = I + ik1 / 6 + ik2 / 3 + ik3 / 3 + ik4 / 6
        newI_list[cnt] = newI + ik1a / 6 + ik2a / 3 + ik3a / 3 + ik4a / 6

    return S_list, I_list, newI_list
