import numpy as np
from SIRS_python import *
from multiprocessing import *

# EAKF code for python runs
def EAKF_fn(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range, n, N, AH,
            dt, countries, airRand):

    num_times = np.floor(len(tm_range) / tm_step)
    nfc = nsn - ntrn # number of weeks for forecasting
    #tstep = range(tm_ini + tm_step, nsn * tm_step + tm_ini, tm_step) # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    
    # Initialize arrays:
    xprior = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn + 1])
    xpost = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn])
    fcast = np.zeros([np.square(n) * 3, num_ens, nfc])

    obsprior = np.empty([n, num_ens, ntrn + 1]) # in R initialized with NAs - is "empty" okay?
    obspost = np.empty([n, num_ens, ntrn])

    # Where each state/param stored:
    S0_indices = range(np.square(n))
    I0_indices = [i + np.square(n) for i in S0_indices]
    newI_indices = [i + np.square(n) * 2 for i in S0_indices]
    param_indices = range(3 * np.square(n), 3 * np.square(n) + 5)

    '''
    # Store results as forecasts are generated:
    fc_met = pd.DataFrame()
    fc_op = pd.DataFrame()
    fc_dist = pd.DataFrame()
    fc_ens = pd.DataFrame()
    '''
    
    # Set initial conditions based on input parameters:
    S0_temp = np.empty([n, n, num_ens]) # originally had these stored as dictionaries, indexed by compartment
    I0_temp = np.empty([n, n, num_ens])
    for i in range(num_ens):
        S0_temp[:, :, i] = np.multiply(np.reshape(init_parms[0:144, i], [n, n]), N)
        I0_temp[:, :, i] = np.multiply(np.reshape(init_parms[144:288, i], [n, n]), N)
    del i
    
    init_parms = init_parms[(init_parms.shape[0] - 5):init_parms.shape[0], :]
    #init_parms = init_parms[288:293, :] # L, D, R0mx, R0diff, airScale
    
    # Calculate the reproductive number at time t:
    beta_range = range(tm_range[0], max(tm_range) + 2 * tm_step + 1)
    AH = AH[beta_range, :]
    #print(AH.shape)
    
    a = -180
    b = np.log(init_parms[3, :])
    beta = np.empty([AH.shape[0], AH.shape[1], num_ens])
    #print(beta.shape)
    for i in range(num_ens):
        beta[:, :, i] = (np.exp(a * AH + b[i]) + (init_parms[2, i] - init_parms[3, i])) / init_parms[1, i]
    del i
    
    tcurrent = tm_ini
    #print(tcurrent) # Question: this is 1 less than tcurrent in R - I'm almost certain this is correct
    
    pass

    # Get lists of initial parameters:
    D_temp = init_parms[1, :]
    L_temp = init_parms[0, :]
    airScale_temp = init_parms[4, :]
    #print(D_temp)
    #print(L_temp)
    #print(airScale_temp)
    print()
    #print(beta_range)

    # Integrate 1 step forwards using SIRS code:
##    xprior[range(xprior.shape[0] - 5), :, 0] = run_ensemble(tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
##                                                            S0 = S0_temp, I0 = I0_temp, popN = N, D = D_temp, L = L_temp, beta = beta,
##                                                            airScale = airScale_temp, Countries = countries, n = n, airRand = airRand,
##                                                            num_ens = num_ens)
##    print(xprior[:, 1, 0])
    
    for i in range(num_ens):
        Sr_tmp = propagate_SIRS(tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
                                S_0 = S0_temp[:, :, i], I_0 = I0_temp[:, :, i], popN = N,
                                D_d = D_temp[i], L_d = L_temp[i], beta_d = beta[:, :, i], airScale_d = airScale_temp[i],
                                Countries = countries, n_count = n, airRand = airRand) # Question: tmRange is beta_range or tm_range?
        
        xprior[S0_indices, i, 0] = Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)])
        xprior[I0_indices, i, 0] = Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)])
        xprior[newI_indices, i, 0] = Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)])
        
    del i
    print(xprior[:, 1, 0])

    
##    proc = Process(target = run_ensemble, args = (tcurrent + dt, tcurrent + tm_step, dt, beta_range, S0_temp, I0_temp, N, D_temp, L_temp, beta, airScale_temp,
##                                                  countries, n, airRand, num_ens))
##    proc.start()
##    print(proc.join().shape)
    
    
##    check_output = [run_ensemble2(i, tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
##                   S0 = S0_temp, I0 = I0_temp, popN = N, D = D_temp, L = L_temp, beta = beta, airScale = airScale_temp,
##                   Countries = countries, n = n, airRand = airRand) for i in range(num_ens)]

##    init_conditions_list = []
##    for i in range(num_ens):
##        init_dict = {}
##        init_dict['S0'] = S0_temp[:, :, i]
##        init_dict['I0'] = I0_temp[:, :, i]
##        init_dict['N'] = N
##        init_dict['beta'] = beta[:, :, i]
##        init_dict['parms'] = [D_temp[i], L_temp[i], airScale_temp[i]]
##        init_conditions_list.append(init_dict)
##    print(init_conditions_list)

##    check_output = map(run_ensemble2(p, tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
##                                     S0 = S0_temp, I0 = I0_temp, popN = N, D = D_temp, L = L_temp, beta = beta,
##                                     airScale = airScale_temp, Countries = countries, n = n, airRand = airRand),
##                       range(num_ens))
##    
    #check_output = map(run_ensemble2, range(num_ens))
    #pool = multiprocessing.Pool(processes=3)
    #r = pool.map(run_ensemble2, i)
    #pool.close()
    #print('Output here!')
    #print(check_output[0])
    #print('-------------')
    #for item in check_output:
    #    print(item)
    


    
    xprior[param_indices, :, 0] = init_parms

    #return(xprior[:, :, 0])

    #print(xprior[:, 1, 0])
    

'''
  ### Also calculate total newI for each COUNTRY, and call these "obs_ens"
  obs_ens <- lapply(1:n, function(ix) {
    colSums(Sr_tmp_newI[1:n + n * (ix - 1), ]) # adding up all new infecteds LIVING in each country
  })
  obs_ens <- t(matrix(unlist(obs_ens), ncol = n, byrow = F)) # each row is a single country
  
  # Convert to rate per 100,000
  for (i in 1:n) {
    obs_ens[i, ] <- obs_ens[i, ] / pop.size$pop[i] * 100000
  }
  
  obsprior[,, 1] <- obs_ens # standardized per 100,000

'''
