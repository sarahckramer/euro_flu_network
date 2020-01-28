import sys
import numpy as np
from numba import jit, prange
from scipy import *


def propagate_SIRS_ISOLATED(tmStrt, tmEnd, tmStep, tmRange, S_0, I_0, popN, D_d, L_d, beta_d):
    cnt = 0

    tmStrt = tmStrt - tmRange[0]  # + 1 # adjust the index to match beta
    tmEnd = tmEnd - tmRange[0]  # + 1

    tm_vec = list(range(tmStrt, tmEnd + 1))
    tm_sz = len(tm_vec) + 1  # plus 1 allows us to include the initial conditions

    Np = len(S_0)  # number of ensemble members / particles

    S_list = np.zeros([tm_sz, Np], dtype=np.float64)
    I_list = np.zeros([tm_sz, Np], dtype=np.float64)
    newI_list = np.zeros([tm_sz, Np], dtype=np.float64)

    S_list[0] = S_0
    I_list[0] = I_0
    # newI_list[0] = 0  # already 0

    for t in tm_vec:
        cnt = cnt + 1

        Eimmloss = tmStep * (1 / L_d) * (popN - S_list[cnt - 1] - I_list[cnt - 1])
        # Eimmloss_check = np.zeros([Np], dtype=np.float64)
        # for i in range(Np):
        #     Eimmloss_check[i] = tmStep * (1 / L_d[i]) * (popN - S_list[cnt-1, i] - I_list[cnt-1, i])
        # print(np.array_equal(Eimmloss, Eimmloss_check))

        Einf = tmStep * (beta_d[t] * I_list[cnt - 1] * S_list[cnt - 1] / popN)
        # Einf_check = np.zeros([Np], dtype=np.float64)
        # for i in range(Np):
        #     Einf_check[i] = tmStep * (beta_d[t, i] * I_list[cnt-1, i] * S_list[cnt-1, i] / popN)
        # print(np.array_equal(Einf, Einf_check))

        Erecov = tmStep * (I_list[cnt - 1] / D_d)
        # Erecov_check = np.zeros([Np], dtype=np.float64)
        # for i in range(Np):
        #     Erecov_check[i] = tmStep * (I_list[cnt-1, i] / D_d[i])
        # print(np.array_equal(Erecov, Erecov_check))

        Eimmloss[np.where(Eimmloss < 0)] = 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov
        sk1 = smcl - smci
        ik1 = smci - smcr
        ik1a = smci
        Ts1 = S_list[cnt - 1] + sk1 / 2
        Ti1 = I_list[cnt - 1] + ik1 / 2

        Eimmloss = tmStep * (1 / L_d) * (popN - Ts1 - Ti1)
        Einf = tmStep * (beta_d[t] * Ti1 * Ts1 / popN)
        Erecov = tmStep * (Ti1 / D_d)
        Eimmloss[np.where(Eimmloss < 0)] = 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov
        sk2 = smcl - smci
        ik2 = smci - smcr
        ik2a = smci
        Ts2 = S_list[cnt - 1] + sk2 / 2
        Ti2 = I_list[cnt - 1] + ik2 / 2

        Eimmloss = tmStep * (1 / L_d) * (popN - Ts2 - Ti2)
        Einf = tmStep * (beta_d[t] * Ti2 * Ts2 / popN)
        Erecov = tmStep * (Ti2 / D_d)
        Eimmloss[np.where(Eimmloss < 0)] = 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov
        sk3 = smcl - smci
        ik3 = smci - smcr
        ik3a = smci
        Ts3 = S_list[cnt - 1] + sk3
        Ti3 = I_list[cnt - 1] + ik3

        Eimmloss = tmStep * (1 / L_d) * (popN - Ts3 - Ti3)
        Einf = tmStep * (beta_d[t] * Ti3 * Ts3 / popN)
        Erecov = tmStep * (Ti3 / D_d)
        Eimmloss[np.where(Eimmloss < 0)] = 0
        Einf[np.where(Einf < 0)] = 0
        Erecov[np.where(Erecov < 0)] = 0
        smcl = Eimmloss
        smci = Einf
        smcr = Erecov
        sk4 = smcl - smci
        ik4 = smci - smcr
        ik4a = smci

        seed = np.float64(0.1)
        S_list[cnt] = S_list[cnt - 1] + sk1/6 + sk2/3 + sk3/3 + sk4/6 - seed
        I_list[cnt] = I_list[cnt-1] + ik1/6 + ik2/3 + ik3/3 + ik4/6 + seed
        newI_list[cnt] = newI_list[cnt-1] + ik1a/6 + ik2a/3 + ik3a/3 + ik4a/6 + seed

    return S_list, I_list, newI_list
