import numpy as np
import pandas as pd
from SIRS_python import *
from functions_all import *
#from multiprocessing import *

# EAKF code for python runs
def EAKF_fn(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range, n, N, AH,
            dt, countries, airRand, lambda_val, wk_start):

    num_times = np.floor(len(tm_range) / tm_step)
    #nfc = nsn - ntrn # number of weeks for forecasting
    #tstep = range(tm_ini + tm_step, nsn * tm_step + tm_ini, tm_step) # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    
    # Initialize arrays:
    
    xprior = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn + 1])
    xpost = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn])
    #fcast = np.zeros([np.square(n) * 3, num_ens, nfc])

    obsprior = np.empty([n, num_ens, ntrn + 1]) # in R initialized with NAs - is "empty" okay?
    obspost = np.empty([n, num_ens, ntrn])
    
    '''
    xprior = np.zeros([np.square(n) * 3 + 5, num_ens, nsn + 1])
    xpost = np.zeros([np.square(n) * 3 + 5, num_ens, nsn])
    #fcast = np.zeros([np.square(n) * 3, num_ens, nfc])

    obsprior = np.empty([n, num_ens, nsn + 1])
    obspost = np.empty([n, num_ens, nsn])
    # set these instead so that they can run for the entire outbreak period, and we can generate forecasts at each time step as we go?
    # but in reality we only train through week 30, since this is our final forecast - so just put 30 in for ntrn?
    '''
    
    # Where each state/param stored:
    S_indices = range(np.square(n))
    I_indices = [i + np.square(n) for i in S_indices]
    newI_indices = [i + np.square(n) * 2 for i in S_indices]
    param_indices = range(3 * np.square(n), 3 * np.square(n) + 5)
    
    # Store results as forecasts are generated:
    fc_met = pd.DataFrame()
    fc_op = pd.DataFrame()
    fc_dist = pd.DataFrame()
    fc_ens = pd.DataFrame()
    
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
    beta_range = range(tm_range[0], max(tm_range) + 2 * tm_step)
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
    #print()
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
                                Countries = countries, n_count = n, airRand = airRand) # Question: tmRange is beta_range or tm_range? - SIRS code only ever uses index 0, which is same for both, so it doesn't matter
        
        xprior[S_indices, i, 0] = Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)])
        xprior[I_indices, i, 0] = Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)])
        xprior[newI_indices, i, 0] = Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)])
    del i
    #print(xprior[:, 1, 0])

    '''
    print(xprior[0, 1, 0])
    print(xprior[4, 1, 0])
    print(xprior[54, 1, 0])
    print(xprior[55, 1, 0])
    print(xprior[157, 1, 0])
    print(xprior[416, 1, 0])
    print()
    '''
    
    xprior[param_indices, :, 0] = init_parms
    #print(init_parms[:, range(20)])

    #return(xprior[:, :, 0])

    # Also calculate total newI for each country:
    #print(xprior[newI_indices, :, 0].shape)
    for i in range(n):
        temp_range = [j + n * i for j in range(n)]
        obsprior[i, :, 0] = np.sum(xprior[newI_indices, :, 0][temp_range, :], 0) / np.sum(N, 1)[i] * 100000

    #print(obsprior[:, 0, 0])
    
    # Begin training:
    #obs_i = obs_i.to_numpy()
    #obs_i = obs_i[:, 1:(n + 2)]
    obs_i = obs_i.iloc[:, 1:(n + 1)].to_numpy()
    print(obs_i)

    for tt in range(5):
        print('Time: ' + str(tt))
        
        # Inflate all states and parameters:
        inflat = np.diag(np.repeat(lambda_val, np.square(n) * 3 + 5))
        inflat_obs = np.diag(np.repeat(lambda_val, n))
        xmn = np.mean(xprior[:, :, tt], 1)
        obs_ensmn = np.mean(obsprior[:, :, tt], 1)
        x = np.matmul(inflat, xprior[:, :, tt] - np.matmul(xmn.reshape([xprior.shape[0], 1]), np.ones([1, num_ens]))) + (np.matmul(xmn.reshape([xprior.shape[0], 1]), np.ones([1, num_ens])))
        obs_ens = np.matmul(inflat_obs, obsprior[:, :, tt] - np.matmul(obs_ensmn.reshape([n, 1]), np.ones([1, num_ens]))) + np.matmul(obs_ensmn.reshape([n, 1]), np.ones([1, num_ens]))
        
        # Quick fix: Don't allow xprior/obsprior to be <0:
        x[np.where(x < 0)] = 0
        obs_ens[np.where(obs_ens < 0)] = 0

        #print(obs_ens[0, range(5)])

        # Update priors with inflated values:
        xprior[:, :, tt] = x
        obsprior[:, :, tt] = obs_ens

        # Loop through observations:
        for loc in range(n):
            #print('Location: ' + countries[loc])

            # Check that data point is not NA, and obs_var is not 0/NA:
            obs_var = obs_vars[tt, loc]
            #print(obs_var)
            #print(obs_i[tt, loc])
            
            if not np.isnan(obs_i[tt, loc]) and not np.isnan(obs_var) and obs_var > 0:

                # Calcualte prior/post means and variances:
                prior_var = np.var(obs_ens[loc, :], ddof = 1)
                post_var = prior_var * (obs_var / (prior_var + obs_var))
                #print(prior_var)
                #print(post_var)

                if prior_var == 0:
                    post_var = 0
                    prior_var = 1e-3

                prior_mean = np.mean(obs_ens[loc, :])
                post_mean = post_var * (prior_mean / prior_var + obs_i[tt, loc] / obs_var)
                #print(prior_mean)
                #print(post_mean)

                # Compute alpha and adjust distribution to conform to posterior moments:
                alp = np.sqrt(obs_var / (obs_var + prior_var))
                dy = post_mean + alp * (obs_ens[loc, :] - prior_mean) - obs_ens[loc, :]
                #print(alp)
                #print(dy)
                
                # Get covariances of the prior state space and the observations, and loop over each state variable:
                rr = np.empty([x.shape[0]])
                for j in range(x.shape[0]):
                    C = (np.cov(x[j, :], obs_ens[loc, :]) / prior_var)[0, 1]
                    rr[j] = C
                del j
                dx = np.matmul(rr.reshape([len(rr), 1]), dy.reshape([1, len(dy)]))

                # Get adjusted ensemble and obs_ens:
                x = x + dx
                obs_ens[loc, :] = obs_ens[loc, :] + dy

                # Question: Do we want to check for S0 values being reduced below 0, like in R code?

                # Correct for values adjusted out of bounds:
                x[np.where(x < 0)] = 0
                x = fn_checkxnobounds(x, S_indices, I_indices, param_indices, N, n)
                obs_ens[loc, :][np.where(obs_ens[loc, :] < 0)] = 0
                
            else:
                #print(obs_i[tt, loc])
                #print(obs_var)
                #print()
                pass
            #print()
            
        del loc
        
        # Store posterior for time tt as xnew:
        xnew = x

        # And store posteriors in proper locations:
        xpost[:, :, tt] = x
        obspost[:, :, tt] = obs_ens
        #print(obs_ens[:, range(5)])

        # Integrate forward one time step to get priors for time tt+1:
        b = np.log(xpost[param_indices[3], :, tt])
        beta = np.empty([AH.shape[0], AH.shape[1], num_ens])
        for i in range(num_ens):
            beta[:, :, i] = (np.exp(a * AH + b[i]) + (xpost[param_indices[2], i, tt] - xpost[param_indices[3], i, tt])) / xpost[param_indices[1], i, tt]
        del i
    
        tcurrent = tm_ini + tm_step * (tt + 1)

        # Get S0/I0/params from xpost:
        '''
        S0_temp = np.empty([n, n, num_ens]) # originally had these stored as dictionaries, indexed by compartment
        I0_temp = np.empty([n, n, num_ens])
        for i in range(num_ens):
            S0_temp[:, :, i] = np.reshape(xpost[S_indices, i, tt], [n, n])
            I0_temp[:, :, i] = np.reshape(xpost[I_indices, i, tt], [n, n])
        del i
        '''

        S0_temp = xpost[S_indices, :, tt].reshape([n, n, num_ens])
        I0_temp = xpost[I_indices, :, tt].reshape([n, n, num_ens])

        #print(np.array_equal(S0_temp, S0_comp))
        #print(np.array_equal(I0_temp, I0_comp))

        D_temp = xpost[param_indices[1], :, tt]
        L_temp = xpost[param_indices[0], :, tt]
        airScale_temp = xpost[param_indices[4], :, tt]

        # Loop through all ensemble members:
        for i in range(num_ens):
            Sr_tmp = propagate_SIRS(tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step, tmStep = dt, tmRange = beta_range,
                                    S_0 = S0_temp[:, :, i], I_0 = I0_temp[:, :, i], popN = N,
                                    D_d = D_temp[i], L_d = L_temp[i], beta_d = beta[:, :, i], airScale_d = airScale_temp[i],
                                    Countries = countries, n_count = n, airRand = airRand)
            
            xprior[S_indices, i, tt + 1] = Sr_tmp[0][:, :, Sr_tmp[0].shape[2] - 1].reshape([1, np.square(n)])
            xprior[I_indices, i, tt + 1] = Sr_tmp[1][:, :, Sr_tmp[1].shape[2] - 1].reshape([1, np.square(n)])
            xprior[newI_indices, i, tt + 1] = Sr_tmp[2][:, :, Sr_tmp[2].shape[2] - 1].reshape([1, np.square(n)])
        del i

        xprior[param_indices, :, tt + 1] = xpost[param_indices, :, tt]

        # Also calculate total newI for each country, and store in obsprior:
        for i in range(n):
            temp_range = [j + n * i for j in range(n)]
            obsprior[i, :, tt + 1] = np.sum(xprior[newI_indices, :, tt + 1][temp_range, :], 0) / np.sum(N, 1)[i] * 100000
        
        # END OF TRAINING
        # Up to tt=3 at least, pretty close match between here and R - think there might be some rounding differences, but seems okay?

        # If tt >= 4 (at least 4 weeks of training data / at week 44), also perform a forecast:
        if tt >= 4:
            nfc = nsn - (tt + 1)
            fcast = np.zeros([np.square(n) * 3, num_ens, nfc])

            # Get states and parameters from most recent update:
            b = np.log(xpost[param_indices[3], :, tt]) # won't this be the same as above?
            beta = np.empty([AH.shape[0], AH.shape[1], num_ens])
            for i in range(num_ens):
                beta[:, :, i] = (np.exp(a * AH + b[i]) + (xpost[param_indices[2], i, tt] - xpost[param_indices[3], i, tt])) / xpost[param_indices[1], i, tt]
            del i
    
            tcurrent = tm_ini + tm_step * (tt + 1)
            #print(tcurrent)            

            # Get S0/I0/params from xpost:
            S0_temp = xpost[S_indices, :, tt].reshape([n, n, num_ens])
            I0_temp = xpost[I_indices, :, tt].reshape([n, n, num_ens])

            D_temp = xpost[param_indices[1], :, tt]
            L_temp = xpost[param_indices[0], :, tt]
            airScale_temp = xpost[param_indices[4], :, tt]

            # Loop through all ensemble members:
            print('Forecasting...')
            for i in range(num_ens):
                #print(tm_range)
                Sr_tmp = propagate_SIRS(tmStrt = tcurrent + dt, tmEnd = tcurrent + tm_step * (nsn - tt - 1), tmStep = dt, tmRange = tm_range,
                                        S_0 = S0_temp[:, :, i], I_0 = I0_temp[:, :, i], popN = N,
                                        D_d = D_temp[i], L_d = L_temp[i], beta_d = beta[:, :, i], airScale_d = airScale_temp[i],
                                        Countries = countries, n_count = n, airRand = airRand)

                fcast[S_indices, i, :] = Sr_tmp[0][:, :, [tm_step * j for j in range(1, nfc + 1)]].reshape([np.square(n), nsn - tt - 1])
                fcast[I_indices, i, :] = Sr_tmp[1][:, :, [tm_step * j for j in range(1, nfc + 1)]].reshape([np.square(n), nsn - tt - 1])
                fcast[newI_indices, i, :] = (Sr_tmp[2][:, :, [tm_step * j for j in range(1, nfc + 1)]] - Sr_tmp[2][:, :, [tm_step * j for j in range(0, nfc)]]).reshape([np.square(n), nsn - tt - 1])
            
            del i

            # Also calculate total newI at the country level, and call these obsfcast:
            obsfcast = np.empty([n, num_ens, nfc])
            #print(obsfcast.shape)
            for i in range(n):
                temp_range = [j + n * i for j in range(n)]
                obs_temp = np.sum(fcast[np.array(newI_indices)[temp_range], :, :], 0) / np.sum(N, 1)[i] * 100000
                #print(obs_temp.shape)
                obsfcast[i, :, :] = obs_temp
            del i

            # Begin processing forecast results
            # Calculate S and I by country:
            s_fcast = fcast[S_indices, :, :]
            i_fcast = fcast[I_indices, :, :]

            s_fcast_by_count = np.empty([n, num_ens, nfc])
            i_fcast_by_count = np.empty([n, num_ens, nfc])
            for i in range(n):
                country_vals = np.array([j + n * i for j in range(n)])
                #s_temp = np.sum(s_fcast[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
                #print(s_temp.shape)
                s_fcast_by_count[i, :, :] = np.sum(s_fcast[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
                i_fcast_by_count[i, :, :] = np.sum(i_fcast[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
            del i
            
            # Calculate ensemble means and sds:
            obsfcast_mean = np.mean(obsfcast, 1)
            obsfcast_sd = np.std(obsfcast, 1, ddof = 1)
            
            obspost_mean = np.mean(obspost[:, :, range(tt + 1)], 1) # temporary - used for assessing forecasts

            sfcast_mean = np.mean(s_fcast_by_count, 1)
            sfcast_sd = np.std(s_fcast_by_count, 1, ddof = 1)
            ifcast_mean = np.mean(i_fcast_by_count, 1)
            ifcast_sd = np.std(i_fcast_by_count, 1, ddof = 1)
            
            # Store results so far in relevant data frames:
            statesfcast = np.concatenate((sfcast_mean.reshape([n * nfc, 1]), sfcast_sd.reshape([n * nfc, 1]), ifcast_mean.reshape([n * nfc, 1]), ifcast_sd.reshape([n * nfc, 1]), obsfcast_mean.reshape([n * nfc, 1]), obsfcast_sd.reshape([n * nfc, 1])), 1)
            
            states_weeks = np.array([i % nfc for i in range(statesfcast.shape[0])]) + wk_start + tt # this will give the actual week value, not a python index
            states_countries = np.array([np.ceil((i + 1) / nfc) - 1 for i in range(statesfcast.shape[0])]).astype(int)
            statesfcast = np.c_[states_weeks, states_countries, statesfcast]
            
            statesfcast = pd.DataFrame(statesfcast)
            statesfcast.columns = ['week', 'country', 'S', 'S_sd', 'I', 'I_sd', 'newI', 'newI_sd']
            
            # QUESTION: Where does this go??



            

            # Calculate metrics to return:
            Y = np.r_[np.transpose(obspost_mean), np.transpose(obsfcast_mean)] # 52 x 12

            Y_ens = np.empty([n, num_ens, nsn]) # 12 x 300 x 52
            Y_ens[:, :, range(tt + 1)] = obspost[:, :, range(tt + 1)]
            Y_ens[:, :, range(tt + 1, nsn)] = obsfcast

            obs_pkwk = np.empty([1, 12])
            pkwk_mean = np.empty([1, 12])
            pkwk_var = np.empty([1, 12])
            
            obs_peak_int = np.empty([1, 12])
            peak_intensity = np.empty([1, 12])
            peak_intensity_var = np.empty([1, 12])

            totAttackObs = np.empty([1, 12])
            tot_attack = np.empty([1, 12])
            ar_var = np.empty([1, 12])

            onsetObs5 = np.empty([1, 12])
            onset5 = np.empty([1, 12])
            onset5_var = np.empty([1, 12])

            obs1wk = np.empty([1, 12])
            obs2wk = np.empty([1, 12])
            obs3wk = np.empty([1, 12])
            obs4wk = np.empty([1, 12])
            fcast1wk = np.empty([1, 12])
            fcast2wk = np.empty([1, 12])
            fcast3wk = np.empty([1, 12])
            fcast4wk = np.empty([1, 12])

            corr = np.empty([1, 12])
            rmse = np.empty([1, 12])
            corr_fcast = np.empty([1, 12])
            rmse_fcast = np.empty([1, 12])

            peakWeeks = np.empty([n, num_ens])
            peakIntensities = np.empty([n, num_ens])
            totalARs = np.empty([n, num_ens])
            onsets5 = np.empty([n, num_ens])

            nextILI = obsfcast[:, :, range(4)]

            ### If data are NA, Y and Y_ens should also be NA! ##################
            Y[np.where(np.isnan(obs_i))] = np.nan
            for i in range(num_ens):
                Y_ens[:, i, :][np.where(np.isnan(np.transpose(obs_i)))] = np.nan
            del i
            #####################################################################
            
            for i in range(n):
                #print(i)

                if not all(np.isnan(obs_i[:, i])):

                    # distributions:
                    for ensmem in range(num_ens):
                        yy = Y_ens[i, ensmem, :]

                        peakWeeks[i, ensmem] = np.nanargmax(yy)
                        peakIntensities[i, ensmem] = np.nanmax(yy)
                        totalARs[i, ensmem] = np.nansum(yy)
                        onsets5[i, ensmem] = findOnset(yy, 500.0)[0]
                    del ensmem

                    # point metrics:
                    obs_pkwk[0, i] = np.nanargmax(obs_i[:, i]) + 1
                    pkwk_mean[0, i] = np.nanargmax(Y[:, i]) + 1
                    pkwk_var[0, i] = np.var(peakWeeks[i, :], ddof = 1) # shouldn't be any NAs in peakWeeks

                    obs_peak_int[0, i] = np.nanmax(obs_i[:, i])
                    peak_intensity[0, i] = np.nanmax(Y[: ,i])
                    peak_intensity_var[0, i] = np.var(peakIntensities[i, :], ddof = 1)

                    totAttackObs[0, i] = np.nansum(obs_i[:, i])
                    tot_attack[0, i] = np.nansum(Y[:, i])
                    ar_var[0, i] = np.var(totalARs[i, :], ddof = 1)

                    onsetObs5[0, i] = findOnset(obs_i[:, i], 500.0)[0]
                    #onset5[0, i] = findOnset(Y[:, i], 500.0)[0] # this is done later!
                    onset5_var[0, i] = np.nanvar(onsets5[i, :], ddof = 1)

                    # continuous error metrics:
                    corr[0, i] = np.ma.corrcoef(Y[:, i][np.where(~np.isnan(obs_i[:, i]))], obs_i[:, i][np.where(~np.isnan(obs_i[:, i]))])[0, 1]
                    rmse[0, i] = np.sqrt(np.nanmean(np.square(Y[:, i] - obs_i[:, i])))
                    corr_fcast[0, i] = np.ma.corrcoef(obsfcast_mean[i, np.where(~np.isnan(obs_i[range(tt + 1, nsn), i]))], obs_i[range(tt + 1, nsn), i][np.where(~np.isnan(obs_i[range(tt + 1, nsn), i]))])[0, 1]
                    rmse_fcast[0, i] = np.sqrt(np.nanmean(np.square(obsfcast_mean[i, :] - obs_i[range(tt + 1, nsn), i])))

                    # 1-4 wk ahead:
                    if obsfcast_mean.shape[1] > 0 and ~np.isnan(obs_i[tt + 1, i]):
                        obs1wk[0, i] = obs_i[tt + 1, i]
                        fcast1wk[0, i] = obsfcast_mean[i, 0]
                    if obsfcast_mean.shape[1] > 1 and ~np.isnan(obs_i[tt + 2, i]):
                        obs2wk[0, i] = obs_i[tt + 2, i]
                        fcast2wk[0, i] = obsfcast_mean[i, 1]
                    if obsfcast_mean.shape[1] > 2 and ~np.isnan(obs_i[tt + 3, i]):
                        obs3wk[0, i] = obs_i[tt + 3, i]
                        fcast3wk[0, i] = obsfcast_mean[i, 2]
                    if obsfcast_mean.shape[1] > 3 and ~np.isnan(obs_i[tt + 4, i]):
                        obs4wk[0, i] = obs_i[tt + 4, i]
                        fcast4wk[0, i] = obsfcast_mean[i, 3]
                    
                else :
                    obs_pkwk[0, i] = np.nan
                    pkwk_mean[0, i] = np.nan
                    pkwk_var[0, i] = np.nan
                    obs_peak_int[0, i] = np.nan
                    peak_intensity[0, i] = np.nan
                    peak_intensity_var[0, i] = np.nan
                    totAttackObs[0, i] = np.nan
                    tot_attack[0, i] = np.nan
                    ar_var[0, i] = np.nan
                    onsetObs5[0, i] = np.nan
                    onset5[0, i] = np.nan
                    onset5_var[0, i] = np.nan
                    peakWeeks[i, :] = np.nan
                    peakIntensities[i, :] = np.nan
                    totalARs[i, :] = np.nan
                    onsets5[i, :] = np.nan
                    
            del i

            leadpkwk_mean = pkwk_mean - (tt + 1)
            delta_pkwk_mean = pkwk_mean - obs_pkwk
            intensity_err = peak_intensity - obs_peak_int
            ar_err = tot_attack - totAttackObs
            
            # Calculate distributions for onset:
            onsets5DistNA = np.empty([1, n + 1])
            onsets5DistNA[:, 0] = -1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    onsets5DistNA[:, j + 1] = np.around(len(onsets5[j, :][np.where(np.isnan(onsets5[j, :]))]) / num_ens, 4)
                else:
                    onsets5DistNA[:, j + 1] = np.nan
            onsets5Dist = np.empty([len(np.unique(onsets5.reshape([1, n * num_ens])[np.where(~np.isnan(onsets5.reshape([1, n * num_ens])))])), n + 1])
            row = 0
            for i in np.sort(np.unique(onsets5.reshape([1, n * num_ens])[np.where(~np.isnan(onsets5.reshape([1, n * num_ens])))])):
                onsets5Dist[row, 0] = i

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        onsets5Dist[row, j + 1] = np.around(len(onsets5[j, :][np.where(onsets5[j, :] == i)]) / num_ens, 4)
                    else:
                        onsets5Dist[row, j + 1] = np.nan

                row += 1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    #print(onsets5Dist[:, j + 1])
                    if onsets5DistNA[:, j + 1] <= np.max(onsets5Dist[:, j + 1]):
                        onset5[0, j] = findOnset(Y[:, j], 500.0)[0]
                    else:
                        onset5[0, j] = np.nan
                else:
                    onset5[0, j] = np.nan
            onsets5Dist = np.r_[onsets5Dist, onsets5DistNA]
            onsets5Dist = pd.DataFrame(onsets5Dist)
            del i
            del j

            # Calculate error in onsets:
            delta_onset5 = onset5 - onsetObs5

            # Calculate probability distributions for peak weeks:
            peakWeeksDistNA = np.empty([1, n + 1])
            peakWeeksDistNA[:, 0] = -1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    peakWeeksDistNA[:, j + 1] = np.around(len(peakWeeks[j, :][np.where(np.isnan(peakWeeks[j, :]))]) / num_ens, 4)
                else:
                    peakWeeksDistNA[:, j + 1] = np.nan
            peakWeeksDist = np.empty([len(np.unique(peakWeeks.reshape([1, n * num_ens])[np.where(~np.isnan(peakWeeks.reshape([1, n * num_ens])))])), n + 1])
            row = 0
            for i in np.sort(np.unique(peakWeeks.reshape([1, n * num_ens])[np.where(~np.isnan(peakWeeks.reshape([1, n * num_ens])))])):
                peakWeeksDist[row, 0] = i

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        peakWeeksDist[row, j + 1] = np.around(len(peakWeeks[j, :][np.where(peakWeeks[j, :] == i)]) / num_ens, 4)

                        if any(np.isnan(peakWeeks[j, :])):
                            print('NAs in peakWeeks!')
                        
                    else:
                        peakWeeksDist[row, j + 1] = np.nan

                row += 1
            peakWeeksDist = np.r_[peakWeeksDist, peakWeeksDistNA]
            peakWeeksDist = pd.DataFrame(peakWeeksDist)
            del i
            del j

            return(obs_i, peakIntensities)
            
            # Calculate probability distributions for peak intensities:
            reqLimits = np.arange(14001, step = 500)
            peakIntensitiesDist = np.empty([len(reqLimits), n + 1])
            row = 0

            for i in range(1, len(reqLimits)):
                peakIntensitiesDist[row, 0] = reqLimits[i]

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        peakIntensitiesDist[row, j + 1] = np.around(len(peakIntensities[j, :][np.where(peakIntensities[j, :] >= reqLimits[i - 1] & peakIntensities[j, :] < reqLimits[i])]) / num_ens, 4)
                    else:
                        peakIntensitiesDist[row, j + 1] = np.nan

                row += 1


'''
  
  peakIntensitiesDist500[row, 1] <- 1e5
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      peakIntensitiesDist500[row, j + 1] <- round(length(peakIntensities[j, ][peakIntensities[j, ] >= max(reqLimits500)]) / 300, 4)
    }
  }
  
  
  # Calculate prob. distribution for next 4 weeks:
  nextILIDist500 <- array(NA, c(length(reqLimits500), n + 1, dim(nextILI)[3]))
  
  row <- 1
  for (i in 2:length(reqLimits500)) {
    nextILIDist500[row, 1, ] <- reqLimits500[i]
    
    for (j in 1:n) {
      if (!all(is.na(obs_i[, j]))) {
        
        for (k in 1:dim(nextILI)[3]) { # depending on where we are in the season, could be fewer than 4 weeks left
          values <- nextILI[j,, k] # don't think these can possibly be NA, right?
          nextILIDist500[row, j + 1, k] <- round(length(values[values >= reqLimits500[i - 1] & values < reqLimits500[i]]) / 300, 4)
        }
        
      }
    }
    
    row <- row + 1
  }
  nextILIDist500[row, 1, ] <- 1e5
  for (j in 1:n) {
    if (!all(is.na(obs_i[, j]))) {
      
      for (k in 1:dim(nextILI)[3]) {
        values <- nextILI[j,, k]
        nextILIDist500[row, j + 1, k] <- round(length(values[values >= max(reqLimits500)]) / 300, 4)
      }
      
    }
  }
  
  
  ### Output data:
  tstep <- seq(tm.ini + tmstep, nsn * tmstep + tm.ini, by = tmstep)
  fc_start <- ntrn + wk_start - 1
  
  out1 <- cbind(fc_start, 'train', rep(tstep[1:ntrn], n), statespost)
  out1 <- out1[, c(1, 3:4, 2, 5:6, 8, 10, 7, 9, 11)]
  
  out2 <- cbind(fc_start, 'fcast', rep(tstep[(ntrn + 1):nsn], n), statesfcast)
  out2 <- out2[, c(1, 3:4, 2, 5:6, 8, 10, 7, 9, 11)]
  
  out3 <- cbind(fc_start, countries, obs_pkwk + wk_start - 1, pkwk_mode + wk_start - 1, delta_pkwk_mode, pkwk_mean + wk_start - 1, delta_pkwk_mean,
                leadpkwk_mode, leadpkwk_mean, sqrt(pkwk_var), obs_peak_int, peak_intensity, intensity_err, sqrt(peak_intensity_var),
                totAttackObs, tot_attack, ar_err, sqrt(ar_var), obs1wk, obs2wk, obs3wk, obs4wk, fcast1wk, fcast2wk, fcast3wk, fcast4wk,
                rdiff_next_newI, rdiff_next_newI2, rdiff_next_newI3, rdiff_next_newI4, onsetObs3 + wk_start - 1, onsetObs4 + wk_start - 1,
                onsetObs5 + wk_start - 1, onsetObs6 + wk_start - 1, onset3 + wk_start - 1, onset4 + wk_start - 1, onset5 + wk_start - 1,
                onset6 + wk_start - 1, delta_onset3, delta_onset4, delta_onset5, delta_onset6, sqrt(onset3_var), sqrt(onset4_var),
                sqrt(onset5_var), sqrt(onset6_var), endObs3 + wk_start - 1, endObs4 + wk_start - 1, endObs5 + wk_start - 1, endObs6 + wk_start - 1,
                end3 + wk_start - 1, end4 + wk_start - 1, end5 + wk_start - 1, end6 + wk_start - 1, delta_end3, delta_end4, delta_end5,
                delta_end6, sqrt(end3_var), sqrt(end4_var), sqrt(end5_var), sqrt(end6_var), durObs3, durObs4, durObs5, durObs6,
                duration3, duration4, duration5, duration6, delta_dur3, delta_dur4, delta_dur5, delta_dur6, sqrt(dur3_var), sqrt(dur4_var),
                sqrt(dur5_var), sqrt(dur6_var), corr, rmse, corr_fcast, rmse_fcast, mape, wape, smape)
  
  out4 <- cbind(fc_start, tstep[1:ntrn], 1:ntrn, params.post_df)
  
  out5 <- rbind(cbind('onset3', onsets3Dist), cbind('onset4', onsets4Dist), cbind('onset5', onsets5Dist), cbind('onset6', onsets6Dist),
                cbind('pw', peakWeeksDist), cbind('pi500', peakIntensitiesDist500), cbind('pi250', peakIntensitiesDist250))
  for (i in 1:4) {
    out5 <- rbind(out5, cbind(paste0('nextweek500_', i), nextILIDist500[,, i]), cbind(paste0('nextweek250_', i), nextILIDist250[,, i]))
  }
  out5 <- as.data.frame(cbind(fc_start, out5))
  names(out5) <- c('fc_start', 'metric', 'bin', countries)
  out5 <- melt(out5, id.vars = c('fc_start', 'metric', 'bin'))
  names(out5)[4] <- 'country'
  
  out6 <- rbind(cbind(fc_start, countries, 'pi', peakIntensities), cbind(fc_start, countries, 'ar', totalARs))
  for (i in 1:4) {
    out6 <- rbind(out6, cbind(fc_start, countries, paste0(i, 'week'), nextILI[,, i]))
  }
  out6 <- as.data.frame(out6)
  names(out6) <- c('fc_start', 'country', 'metric', 1:300)
  
  colnames(out1) = colnames(out2) = c('fc_start', 'time', 'week', 'result', 'country', 'S', 'I', 'Est', 'S_sd', 'I_sd', 'Est_sd')
  out1$result <- as.character(out1$result); out2$result <- as.character(out2$result)
  out1 <- rbind(out1, out2)
  out1$result <- factor(out1$result)
  
  colnames(out3) = c('fc_start', 'country', 'obs_pkwk', 'pkwk_mode', 'delta_pkwk_mode', 'pkwk_mean', 'delta_pkwk_mean',
                     'leadpkwk_mode', 'leadpkwk_mean', 'pkwk_sd', 'obs_peak_int', 'peak_intensity', 'intensity_err', 'peak_intensity_sd',
                     'totAttackObs', 'tot_attack', 'delta_AR', 'AR_sd', 'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week',
                     'fcast_1week', 'fcast_2week', 'fcast_3week', 'fcast_4week', 'delta_1w', 'delta_2w', 'delta_3w', 'delta_4w',
                     'onsetObs3', 'onsetObs4', 'onsetObs5', 'onsetObs6', 'onset3', 'onset4', 'onset5', 'onset6', 'delta_onset3',
                     'delta_onset4', 'delta_onset5', 'delta_onset6', 'onset3_sd','onset4_sd','onset5_sd','onset6_sd', 'endObs3',
                     'endObs4', 'endObs5', 'endObs6', 'end3', 'end4', 'end5', 'end6', 'delta_end3', 'delta_end4', 'delta_end5', 'delta_end6',
                     'end3_sd', 'end4_sd', 'end5_sd', 'end6_sd', 'durationObs3', 'durationObs4', 'durationObs5', 'durationObs6',
                     'duration3', 'duration4', 'duration5', 'duration6', 'delta_dur3', 'delta_dur4', 'delta_dur5', 'delta_dur6',
                     'duration3_sd', 'duration4_sd', 'duration5_sd', 'duration6_sd', 'corr', 'rmse', 'corr_fcast', 'rmse_fcast',
                     'mape', 'wape', 'smape')
  colnames(out4)[1:3] <- c('fc_start', 'time', 'week')
  
  var.df <- as.data.frame(var.df)
  
  out <- list(opStates = out1, metrics = out3, trainParams = out4, dist = out5, ensembles = out6, vars = var.df)
  # out <- list(train = out1, fcast = out2, metrics = out3, trainParams = out4, dist = out5, ensembles = out6)
  




  

# SAVE FOR AFTER FORECASTING COMPLETED:
### Calculate S and I by country (prior, post, and fcast):
  s.post <- xpost[S0.indices,, ]; s.prior <- xprior[S0.indices,, ]
  i.post <- xpost[I0.indices,, ]; i.prior <- xprior[I0.indices,, ]
  
s.post.by.count = i.post.by.count = array(0, c(n, num_ens, ntrn))
  s.prior.by.count = i.prior.by.count = array(0, c(n, num_ens, ntrn + 1))

  for (i in 1:n) {
    country.vals <- (1:n) + n * (i - 1)
    s.post.temp <- s.post[country.vals,, ]
    s.prior.temp <- s.prior[country.vals,, ]
    i.post.temp <- i.post[country.vals,, ]
    i.prior.temp <- i.prior[country.vals,, ]
    
    for (j in 1:num_ens) {
      s.post.by.count[i, j, ] <- colSums(s.post.temp[, j, ]) / pop.size$pop[i] * 100000
      s.prior.by.count[i, j, ] <- colSums(s.prior.temp[, j, ]) / pop.size$pop[i] * 100000
      i.post.by.count[i, j, ] <- colSums(i.post.temp[, j, ]) / pop.size$pop[i] * 100000
      i.prior.by.count[i, j, ] <- colSums(i.prior.temp[, j, ]) / pop.size$pop[i] * 100000
    }
    
  }

### Calculate ensemble means and sds (prior, post, and fcast; S, I, newI):
  obsprior_mean <- t(apply(obsprior[,, 1:(ntrn + 1)], c(1, 3), mean))
  obspost_mean <- t(apply(obspost[,, 1:ntrn], c(1, 3), mean))
  obspost_sd <- t(apply(obspost[,, 1:ntrn], c(1, 3), sd))
  
  sprior_mean <- t(apply(s.prior.by.count[,, 1:(ntrn + 1)], c(1, 3), mean))
  spost_mean <- t(apply(s.post.by.count[,, 1:ntrn], c(1, 3), mean))
  spost_sd <- t(apply(s.post.by.count[,, 1:ntrn], c(1, 3), sd))
  
  iprior_mean <- t(apply(i.prior.by.count[,, 1:(ntrn + 1)], c(1, 3), mean))
  ipost_mean <- t(apply(i.post.by.count[,, 1:ntrn], c(1, 3), mean))
  ipost_sd <- t(apply(i.post.by.count[,, 1:ntrn], c(1, 3), sd))
  
  statesprior <- cbind(melt(sprior_mean), melt(iprior_mean), melt(obsprior_mean)); statesprior <- statesprior[, c(1:3, 6, 9)];
  names(statesprior) <- c('week', 'country', 'S', 'I', 'newI');
  statesprior$week <- statesprior$week + wk_start - 1; statesprior$country <- countries[statesprior$country]
  
  statespost <- cbind(melt(spost_mean), melt(spost_sd), melt(ipost_mean), melt(ipost_sd), melt(obspost_mean), melt(obspost_sd));
  statespost <- statespost[, c(1:3, 6, 9, 12, 15, 18)];
  names(statespost) <- c('week', 'country', 'S', 'S_sd', 'I', 'I_sd', 'newI', 'newI_sd');
  statespost$week <- statespost$week + wk_start - 1; statespost$country <- countries[statespost$country]
  
  ### Get parameter means and sds over time (prior and post):
  params.prior_mean <- t(apply(xprior[param.indices,, 1:(ntrn + 1)], c(1, 3), mean))
  params.post_mean <- t(apply(xpost[param.indices,, 1:ntrn], c(1, 3), mean))
  params.post_sd <- t(apply(xpost[param.indices,, 1:ntrn], c(1, 3), sd))
  # QUESTION: SD continues to increase near end of outbreak - okay?
  params.post_df <- as.data.frame(cbind(params.post_mean, params.post_sd))
  names(params.post_df) <- c('L', 'D', 'R0mx', 'R0diff', 'airScale', 'L_sd', 'D_sd', 'R0mx_sd', 'R0diff_sd', 'airScale_sd')
  

        
'''
