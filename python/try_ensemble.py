%% cython
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
