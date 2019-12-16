import numpy as np
from SIRS_python import *

# EAKF code for python runs
def EAKF_fn(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range, n, N, AH,
            dt, countries, airRand):

    num_times = np.floor(len(tm_range) / tm_step)
    nfc = nsn - ntrn # number of weeks for forecasting
    tstep = range(tm_ini + tm_step, nsn * tm_step + tm_ini, tm_step) # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    
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

    tcurrent = tm_ini
    #print(tcurrent)
    
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
    for i in range(1):
        Sr_tmp = propagate_SIRS(tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
                                S_0 = S0_temp[:, :, i], I_0 = I0_temp[:, :, i], popN = N,
                                D_d = D_temp[i], L_d = L_temp[i], beta_d = beta[:, :, i], airScale_d = airScale_temp[i],
                                Countries = countries, n_count = n, airRand = airRand) # Question: tmRange is beta_range or tm_range?
        # and store results somewhere, or else format then store

    

'''
  # integrate 1 step forward
  Sr_tmp <- sapply(1:num_ens, function(ix) {
    propagateToySIRS(tm_strt = tcurrent + dt, tm_end = tcurrent + tmstep, dt,
                     S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                     D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                     airScale = airScale.temp[ix], realdata = TRUE,
                     prohibAir = FALSE)
  })


'''
