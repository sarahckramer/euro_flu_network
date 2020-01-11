import pandas as pd
from SIRS_python import *
from functions_all import *


# EAKF code for python runs
# noinspection PyShadowingNames
def EAKF_fn(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range, n, N, AH,
            dt, countries, airRand, lambda_val, wk_start):
    # num_times = np.floor(len(tm_range) / tm_step)
    # nfc = nsn - ntrn # number of weeks for forecasting
    tstep = np.arange(tm_ini + tm_step + 1, nsn * tm_step + tm_ini + 2, step=tm_step)
    # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    fc_start = wk_start

    # Initialize arrays:
    xprior = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn + 1], dtype=np.float64)
    xpost = np.zeros([np.square(n) * 3 + 5, num_ens, ntrn], dtype=np.float64)

    obsprior = np.zeros([n, num_ens, ntrn + 1], dtype=np.float64)  # in R initialized with NAs - is "empty" okay?
    obspost = np.zeros([n, num_ens, ntrn], dtype=np.float64)

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
    S0_temp = np.zeros([n, n, num_ens], dtype=np.float64)
    #  originally had these stored as dictionaries, indexed by compartment
    I0_temp = np.zeros([n, n, num_ens], dtype=np.float64)
    for i in range(num_ens):
        S0_temp[:, :, i] = np.multiply(np.reshape(init_parms[0:144, i], [n, n]), N)
        I0_temp[:, :, i] = np.multiply(np.reshape(init_parms[144:288, i], [n, n]), N)
    del i

    init_parms = init_parms[(init_parms.shape[0] - 5):init_parms.shape[0], :]
    # init_parms = init_parms[288:293, :] # L, D, R0mx, R0diff, airScale

    # Calculate the reproductive number at time t:
    beta_range = range(tm_range[0], max(tm_range) + 2 * tm_step)
    AH = AH[beta_range, :]
    # print(AH.shape)

    a = -180
    b = np.log(init_parms[3, :])
    beta = np.zeros([AH.shape[0], AH.shape[1], num_ens], dtype=np.float64)
    # print(beta.shape)
    for i in range(num_ens):
        beta[:, :, i] = (np.exp(a * AH + b[i]) + (init_parms[2, i] - init_parms[3, i])) / init_parms[1, i]
    del i

    tcurrent = tm_ini
    # print(tcurrent) # Question: this is 1 less than tcurrent in R - I'm almost certain this is correct

    # Get lists of initial parameters:
    D_temp = init_parms[1, :]
    L_temp = init_parms[0, :]
    airScale_temp = init_parms[4, :]

    xprior[range(np.square(n) * 3), :, 0] = run_ensemble(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt,
                                                         tmRange=tm_range, S0=S0_temp, I0=I0_temp, popN=N,
                                                         D=D_temp, L=L_temp, beta=beta, airScale=airScale_temp,
                                                         n=n, airRand=airRand, num_ens=300)
    xprior[param_indices, :, 0] = init_parms

    # Also calculate total newI for each country:
    for i in range(n):
        temp_range = [j + n * i for j in range(n)]
        obsprior[i, :, 0] = np.sum(xprior[newI_indices, :, 0][temp_range, :], 0) / np.sum(N, 1)[i] * 100000

    # Begin training:
    obs_i = obs_i.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)
    # print(obs_i)

    for tt in range(ntrn):
        print('Time: ' + str(tt))

        # Inflate all states and parameters:
        inflat = np.diag(np.repeat(lambda_val, np.square(n) * 3 + 5))
        inflat_obs = np.diag(np.repeat(lambda_val, n))
        xmn = np.mean(xprior[:, :, tt], 1)
        obs_ensmn = np.mean(obsprior[:, :, tt], 1)
        x = np.matmul(inflat,
                      xprior[:, :, tt] - np.matmul(xmn.reshape([xprior.shape[0], 1]), np.ones([1, num_ens]))) + (
                np.matmul(xmn.reshape([xprior.shape[0], 1]), np.ones([1, num_ens])))
        obs_ens = np.matmul(inflat_obs, obsprior[:, :, tt] - np.matmul(obs_ensmn.reshape([n, 1]),
                                                                       np.ones([1, num_ens]))) + np.matmul(
            obs_ensmn.reshape([n, 1]), np.ones([1, num_ens]))
        # print(obs_ensmn)
        # Quick fix: Don't allow xprior/obsprior to be <0:
        x[np.where(x < 0)] = 0
        obs_ens[np.where(obs_ens < 0)] = 0

        # Update priors with inflated values:
        xprior[:, :, tt] = x
        obsprior[:, :, tt] = obs_ens

        # Loop through observations:
        for loc in range(n):
            # print('Location: ' + countries[loc])

            # Check that data point is not NA, and obs_var is not 0/NA:
            obs_var = obs_vars[tt, loc]
            # print(obs_var)

            if not np.isnan(obs_i[tt, loc]) and not np.isnan(obs_var) and obs_var > 0:

                # Calcualte prior/post means and variances:
                prior_var = np.var(obs_ens[loc, :], ddof=1)
                post_var = prior_var * (obs_var / (prior_var + obs_var))

                if prior_var == 0:
                    post_var = 0
                    prior_var = 1e-3

                prior_mean = np.mean(obs_ens[loc, :])
                post_mean = post_var * (prior_mean / prior_var + obs_i[tt, loc] / obs_var)

                # Compute alpha and adjust distribution to conform to posterior moments:
                alp = np.sqrt(obs_var / (obs_var + prior_var))
                dy = post_mean + alp * (obs_ens[loc, :] - prior_mean) - obs_ens[loc, :]

                # Get covariances of the prior state space and the observations, and loop over each state variable:
                rr = np.zeros([x.shape[0]], dtype=np.float64)
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

        del loc

        # And store posteriors in proper locations:
        xpost[:, :, tt] = x
        obspost[:, :, tt] = obs_ens
        # print(obs_ens[:, range(5)])

        # Integrate forward one time step to get priors for time tt+1:
        b = np.log(xpost[param_indices[3], :, tt])
        beta = np.zeros([AH.shape[0], AH.shape[1], num_ens], dtype=np.float64)
        for i in range(num_ens):
            beta[:, :, i] = (np.exp(a * AH + b[i]) + (
                    xpost[param_indices[2], i, tt] - xpost[param_indices[3], i, tt])) / xpost[
                                param_indices[1], i, tt]
        del i

        tcurrent = tm_ini + tm_step * (tt + 1)

        # Get S0/I0/params from xpost:
        S0_temp = xpost[S_indices, :, tt].reshape([n, n, num_ens])
        I0_temp = xpost[I_indices, :, tt].reshape([n, n, num_ens])

        D_temp = xpost[param_indices[1], :, tt]
        L_temp = xpost[param_indices[0], :, tt]
        airScale_temp = xpost[param_indices[4], :, tt]

        # Loop through all ensemble members:
        xprior[range(np.square(n) * 3), :, tt + 1] = run_ensemble(tmStrt=tcurrent+dt, tmEnd=tcurrent+tm_step, tmStep=dt,
                                                                  tmRange=tm_range, S0=S0_temp, I0=I0_temp, popN=N,
                                                                  D=D_temp, L=L_temp, beta=beta, airScale=airScale_temp,
                                                                  n=n, airRand=airRand, num_ens=300)
        xprior[param_indices, :, tt + 1] = xpost[param_indices, :, tt]

        # Also calculate total newI for each country, and store in obsprior:
        for i in range(n):
            temp_range = [j + n * i for j in range(n)]
            obsprior[i, :, tt+1] = np.sum(xprior[newI_indices, :, tt + 1][temp_range, :], 0) / np.sum(N, 1)[i] * 100000

        # END OF TRAINING

        # If tt >= 4 (at least 4 weeks of training data / at week 44), also perform a forecast:
        if tt >= 4:
            nfc = nsn - (tt + 1)

            # Get states and parameters from most recent update:
            b = np.log(xpost[param_indices[3], :, tt])  # won't this be the same as above?
            beta = np.zeros([AH.shape[0], AH.shape[1], num_ens], dtype=np.float64)
            for i in range(num_ens):
                beta[:, :, i] = (np.exp(a * AH + b[i]) + (
                        xpost[param_indices[2], i, tt] - xpost[param_indices[3], i, tt])) / xpost[
                                    param_indices[1], i, tt]
            del i

            tcurrent = tm_ini + tm_step * (tt + 1)

            # Get S0/I0/params from xpost:
            S0_temp = xpost[S_indices, :, tt].reshape([n, n, num_ens])
            I0_temp = xpost[I_indices, :, tt].reshape([n, n, num_ens])

            D_temp = xpost[param_indices[1], :, tt]
            L_temp = xpost[param_indices[0], :, tt]
            airScale_temp = xpost[param_indices[4], :, tt]

            # Loop through all ensemble members:
            print('Forecasting...')
            fcast = run_forecast(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step * (nsn - tt - 1), tmStep=dt,
                                 tmRange=tm_range, S0=S0_temp, I0=I0_temp, popN=N, D=D_temp, L=L_temp, beta=beta,
                                 airScale=airScale_temp, n=n, airRand=airRand, num_ens=300, nfc=nfc, tm_step=tm_step)

            # Also calculate total newI at the country level, and call these obsfcast:
            obsfcast = np.zeros([n, num_ens, nfc], dtype=np.float64)
            # print(obsfcast.shape)
            for i in range(n):
                temp_range = [j + n * i for j in range(n)]
                obs_temp = np.sum(fcast[np.array(newI_indices)[temp_range], :, :], 0) / np.sum(N, 1)[i] * 100000
                # print(obs_temp.shape)
                obsfcast[i, :, :] = obs_temp
            del i

            # Begin processing forecast results
            # Calculate S and I by country:
            s_fcast = fcast[S_indices, :, :]
            i_fcast = fcast[I_indices, :, :]

            s_fcast_by_count = np.zeros([n, num_ens, nfc], dtype=np.float64)
            i_fcast_by_count = np.zeros([n, num_ens, nfc], dtype=np.float64)
            for i in range(n):
                country_vals = np.array([j + n * i for j in range(n)])
                s_fcast_by_count[i, :, :] = np.sum(s_fcast[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
                i_fcast_by_count[i, :, :] = np.sum(i_fcast[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
            del i

            # Calculate ensemble means and sds:
            obsfcast_mean = np.mean(obsfcast, 1)
            obsfcast_sd = np.std(obsfcast, 1, ddof=1)

            obspost_mean = np.mean(obspost[:, :, range(tt + 1)], 1)  # temporary - used for assessing forecasts

            sfcast_mean = np.mean(s_fcast_by_count, 1)
            sfcast_sd = np.std(s_fcast_by_count, 1, ddof=1)
            ifcast_mean = np.mean(i_fcast_by_count, 1)
            ifcast_sd = np.std(i_fcast_by_count, 1, ddof=1)

            # Store results so far in relevant data frames:
            statesfcast = np.concatenate((sfcast_mean.reshape([n * nfc, 1]), sfcast_sd.reshape([n * nfc, 1]),
                                          ifcast_mean.reshape([n * nfc, 1]), ifcast_sd.reshape([n * nfc, 1]),
                                          obsfcast_mean.reshape([n * nfc, 1]), obsfcast_sd.reshape([n * nfc, 1])), 1)

            states_weeks = np.array([i % nfc for i in range(statesfcast.shape[0])]) + wk_start + tt + 1
            # this will give the actual week value, not a python index
            states_countries = np.array([np.ceil((i + 1) / nfc) - 1 for i in range(statesfcast.shape[0])]).astype(int)
            statesfcast = np.c_[states_weeks, states_countries, statesfcast]

            statesfcast = pd.DataFrame(statesfcast)
            statesfcast.columns = ['week', 'country', 'S', 'S_sd', 'I', 'I_sd', 'Est', 'Est_sd']

            # Calculate metrics to return:
            Y = np.r_[np.transpose(obspost_mean), np.transpose(obsfcast_mean)]  # 52 x 12

            Y_ens = np.zeros([n, num_ens, nsn], dtype=np.float64)  # 12 x 300 x 52
            Y_ens[:, :, range(tt + 1)] = obspost[:, :, range(tt + 1)]
            Y_ens[:, :, range(tt + 1, nsn)] = obsfcast

            obs_pkwk = np.empty([1, 12], dtype=np.float64)
            pkwk_mean = np.empty([1, 12], dtype=np.float64)
            pkwk_var = np.empty([1, 12], dtype=np.float64)

            obs_peak_int = np.empty([1, 12], dtype=np.float64)
            peak_intensity = np.empty([1, 12], dtype=np.float64)
            peak_intensity_var = np.empty([1, 12], dtype=np.float64)

            totAttackObs = np.empty([1, 12], dtype=np.float64)
            tot_attack = np.empty([1, 12], dtype=np.float64)
            ar_var = np.empty([1, 12], dtype=np.float64)

            onsetObs5 = np.empty([1, 12], dtype=np.float64)
            onset5 = np.empty([1, 12], dtype=np.float64)
            onset5_var = np.empty([1, 12], dtype=np.float64)

            obs1wk = np.empty([1, 12], dtype=np.float64)
            obs2wk = np.empty([1, 12], dtype=np.float64)
            obs3wk = np.empty([1, 12], dtype=np.float64)
            obs4wk = np.empty([1, 12], dtype=np.float64)
            fcast1wk = np.empty([1, 12], dtype=np.float64)
            fcast2wk = np.empty([1, 12], dtype=np.float64)
            fcast3wk = np.empty([1, 12], dtype=np.float64)
            fcast4wk = np.empty([1, 12], dtype=np.float64)

            corr = np.empty([1, 12], dtype=np.float64)
            rmse = np.empty([1, 12], dtype=np.float64)
            corr_fcast = np.empty([1, 12], dtype=np.float64)
            rmse_fcast = np.empty([1, 12], dtype=np.float64)

            peakWeeks = np.empty([n, num_ens], dtype=np.float64)
            peakIntensities = np.empty([n, num_ens], dtype=np.float64)
            totalARs = np.empty([n, num_ens], dtype=np.float64)
            onsets5 = np.empty([n, num_ens], dtype=np.float64)

            nextILI = obsfcast[:, :, range(4)]

            # ### If data are NA, Y and Y_ens should also be NA! #################
            Y[np.where(np.isnan(obs_i))] = np.nan
            for i in range(num_ens):
                Y_ens[:, i, :][np.where(np.isnan(np.transpose(obs_i)))] = np.nan
            del i
            #####################################################################

            for i in range(n):
                # print(i)

                if not all(np.isnan(obs_i[:, i])):

                    # distributions:
                    for ensmem in range(num_ens):
                        yy = Y_ens[i, ensmem, :]

                        peakWeeks[i, ensmem] = np.nanargmax(yy) + 1
                        peakIntensities[i, ensmem] = np.nanmax(yy)
                        totalARs[i, ensmem] = np.nansum(yy)
                        onsets5[i, ensmem] = findOnset(yy, 500.0)[0]
                    del ensmem

                    # point metrics:
                    obs_pkwk[0, i] = np.nanargmax(obs_i[:, i]) + 1
                    pkwk_mean[0, i] = np.nanargmax(Y[:, i]) + 1
                    pkwk_var[0, i] = np.var(peakWeeks[i, :], ddof=1)  # shouldn't be any NAs in peakWeeks

                    obs_peak_int[0, i] = np.nanmax(obs_i[:, i])
                    peak_intensity[0, i] = np.nanmax(Y[:, i])
                    peak_intensity_var[0, i] = np.var(peakIntensities[i, :], ddof=1)

                    totAttackObs[0, i] = np.nansum(obs_i[:, i])
                    tot_attack[0, i] = np.nansum(Y[:, i])
                    ar_var[0, i] = np.var(totalARs[i, :], ddof=1)

                    onsetObs5[0, i] = findOnset(obs_i[:, i], 500.0)[0]
                    # onset5[0, i] = findOnset(Y[:, i], 500.0)[0] # this is done later!
                    onset5_var[0, i] = np.nanvar(onsets5[i, :], ddof=1) if ~all(np.isnan(onsets5[i, :])) else np.nan

                    # continuous error metrics:
                    corr[0, i] = np.ma.corrcoef(Y[:, i][np.where(~np.isnan(obs_i[:, i]))],
                                                obs_i[:, i][np.where(~np.isnan(obs_i[:, i]))])[0, 1]
                    rmse[0, i] = np.sqrt(np.nanmean(np.square(Y[:, i] - obs_i[:, i])))
                    corr_fcast[0, i] = \
                        np.ma.corrcoef(obsfcast_mean[i, np.where(~np.isnan(obs_i[range(tt + 1, nsn), i]))],
                                       obs_i[range(tt + 1, nsn), i][
                                           np.where(~np.isnan(obs_i[range(tt + 1, nsn), i]))])[0, 1]
                    rmse_fcast[0, i] = np.sqrt(
                        np.nanmean(np.square(obsfcast_mean[i, :] - obs_i[range(tt + 1, nsn), i])))

                    # 1-4 wk ahead:
                    if obsfcast_mean.shape[1] > 0 and ~np.isnan(obs_i[tt + 1, i]):
                        obs1wk[0, i] = obs_i[tt + 1, i]
                        fcast1wk[0, i] = obsfcast_mean[i, 0]
                    else:
                        obs1wk[0, i] = np.nan
                        fcast1wk[0, i] = np.nan
                    if obsfcast_mean.shape[1] > 1 and ~np.isnan(obs_i[tt + 2, i]):
                        obs2wk[0, i] = obs_i[tt + 2, i]
                        fcast2wk[0, i] = obsfcast_mean[i, 1]
                    else:
                        obs2wk[0, i] = np.nan
                        fcast2wk[0, i] = np.nan
                    if obsfcast_mean.shape[1] > 2 and ~np.isnan(obs_i[tt + 3, i]):
                        obs3wk[0, i] = obs_i[tt + 3, i]
                        fcast3wk[0, i] = obsfcast_mean[i, 2]
                    else:
                        obs3wk[0, i] = np.nan
                        fcast3wk[0, i] = np.nan
                    if obsfcast_mean.shape[1] > 3 and ~np.isnan(obs_i[tt + 4, i]):
                        obs4wk[0, i] = obs_i[tt + 4, i]
                        fcast4wk[0, i] = obsfcast_mean[i, 3]
                    else:
                        obs4wk[0, i] = np.nan
                        fcast4wk[0, i] = np.nan

                else:
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
                    corr[0, i] = np.nan
                    rmse[0, i] = np.nan
                    corr_fcast[0, i] = np.nan
                    rmse_fcast[0, i] = np.nan
                    obs1wk[0, i] = np.nan
                    obs2wk[0, i] = np.nan
                    obs3wk[0, i] = np.nan
                    obs4wk[0, i] = np.nan
                    fcast1wk[0, i] = np.nan
                    fcast2wk[0, i] = np.nan
                    fcast3wk[0, i] = np.nan
                    fcast4wk[0, i] = np.nan

            del i

            leadpkwk_mean = pkwk_mean - (tt + 1)
            delta_pkwk_mean = pkwk_mean - obs_pkwk
            intensity_err = peak_intensity - obs_peak_int
            ar_err = tot_attack - totAttackObs

            # Calculate distributions for onset:
            onsets5DistNA = np.empty([1, n + 1], dtype=np.float64)
            onsets5DistNA[:, 0] = -1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    onsets5DistNA[:, j + 1] = np.around(len(onsets5[j, :][np.where(np.isnan(onsets5[j, :]))]) / num_ens,
                                                        4)
                else:
                    onsets5DistNA[:, j + 1] = np.nan
            onsets5Dist = np.empty([len(
                np.unique(onsets5.reshape([1, n * num_ens])[np.where(~np.isnan(onsets5.reshape([1, n * num_ens])))])),
                n + 1], dtype=np.float64)
            row = 0
            for i in np.sort(np.unique(
                    onsets5.reshape([1, n * num_ens])[np.where(~np.isnan(onsets5.reshape([1, n * num_ens])))])):
                onsets5Dist[row, 0] = i

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        onsets5Dist[row, j + 1] = np.around(len(onsets5[j, :][np.where(onsets5[j, :] == i)]) / num_ens,
                                                            4)
                    else:
                        onsets5Dist[row, j + 1] = np.nan

                row += 1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    # print(onsets5Dist[:, j + 1])
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
            peakWeeksDistNA = np.empty([1, n + 1], dtype=np.float64)
            peakWeeksDistNA[:, 0] = -1
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    peakWeeksDistNA[:, j + 1] = np.around(
                        len(peakWeeks[j, :][np.where(np.isnan(peakWeeks[j, :]))]) / num_ens, 4)
                else:
                    peakWeeksDistNA[:, j + 1] = np.nan
            peakWeeksDist = np.empty([len(np.unique(
                peakWeeks.reshape([1, n * num_ens])[np.where(~np.isnan(peakWeeks.reshape([1, n * num_ens])))])), n + 1],
                dtype=np.float64)
            row = 0
            for i in np.sort(np.unique(
                    peakWeeks.reshape([1, n * num_ens])[np.where(~np.isnan(peakWeeks.reshape([1, n * num_ens])))])):
                peakWeeksDist[row, 0] = i

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        peakWeeksDist[row, j + 1] = np.around(
                            len(peakWeeks[j, :][np.where(peakWeeks[j, :] == i)]) / num_ens, 4)

                        if any(np.isnan(peakWeeks[j, :])):
                            print('NAs in peakWeeks!')

                    else:
                        peakWeeksDist[row, j + 1] = np.nan

                row += 1
            peakWeeksDist = np.r_[peakWeeksDist, peakWeeksDistNA]
            peakWeeksDist = pd.DataFrame(peakWeeksDist)
            del i
            del j

            # Calculate probability distributions for peak intensities:
            reqLimits = np.arange(14001, step=500)
            peakIntensitiesDist = np.empty([len(reqLimits), n + 1], dtype=np.float64)
            row = 0

            for i in range(1, len(reqLimits)):
                peakIntensitiesDist[row, 0] = reqLimits[i]

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):
                        peakIntensitiesDist[row, j + 1] = np.around(len(peakIntensities[j, :][np.where(
                            np.greater_equal(peakIntensities[j, :], reqLimits[i - 1]) & np.less(peakIntensities[j, :],
                                                                                                reqLimits[
                                                                                                    i]))]) / num_ens, 4)
                    else:
                        peakIntensitiesDist[row, j + 1] = np.nan

                row += 1
            peakIntensitiesDist[row, 0] = 1e5
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):
                    peakIntensitiesDist[row, j + 1] = np.around(len(peakIntensities[j, :][np.where(
                        np.greater_equal(peakIntensities[j, :], np.max(reqLimits)))]) / num_ens, 4)
                else:
                    peakIntensitiesDist[row, j + 1] = np.nan
            peakIntensitiesDist = pd.DataFrame(peakIntensitiesDist)
            del i
            del j

            # Calculate probabilitiy distribution for next 4 weeks:
            nextILIDist = np.empty([len(reqLimits), n + 1, nextILI.shape[2]], dtype=np.float64)
            row = 0
            for i in range(1, len(reqLimits)):
                nextILIDist[row, 0, :] = reqLimits[i]

                for j in range(n):
                    if not all(np.isnan(obs_i[:, j])):

                        for k in range(nextILI.shape[2]):
                            values = nextILI[j, :, k]
                            nextILIDist[row, j + 1, k] = np.around(len(values[np.where(
                                np.greater_equal(values, reqLimits[i - 1]) & np.less(values, reqLimits[i]))]) / num_ens,
                                                                   4)

                    else:
                        nextILIDist[row, j + 1, :] = np.nan

                row += 1
            nextILIDist[row, 0, :] = 1e5
            for j in range(n):
                if not all(np.isnan(obs_i[:, j])):

                    for k in range(nextILI.shape[2]):
                        values = nextILI[j, :, k]
                        nextILIDist[row, j + 1, k] = np.around(
                            len(values[np.where(np.greater_equal(values, max(reqLimits)))]) / num_ens, 4)

                else:
                    nextILIDist[row, j + 1, :] = np.nan

            # nextILIDist = pd.DataFrame(nextILIDist)
            # print(nextILIDist)
            del i
            del j
            del k

            # Store results from this round of forecasting:
            tstep = np.arange(tm_ini + tm_step + 1, nsn * tm_step + tm_ini + 2, step=tm_step)
            fc_start = tt + wk_start

            statesfcast['fc_start'] = fc_start
            statesfcast['result'] = 'fcast'
            statesfcast['time'] = pd.Series(np.tile(tstep[range(tt + 1, nsn)], n))
            # noinspection PyTypeChecker
            statesfcast = statesfcast[statesfcast.columns[[8, 10, 0, 9, 1, 2, 4, 6, 3, 5, 7]]]
            fc_op = fc_op.append(statesfcast, ignore_index=True)

            dist_temp = pd.DataFrame(np.r_[np.c_[np.repeat('onset5', onsets5Dist.shape[0]), onsets5Dist],
                                           np.c_[np.repeat('pw', peakWeeksDist.shape[0]), peakWeeksDist],
                                           np.c_[np.repeat('pi', peakIntensitiesDist.shape[0]), peakIntensitiesDist]])
            for i in range(4):
                dist_temp = dist_temp.append(
                    pd.DataFrame(np.c_[np.repeat('nextweek' + str(i + 1), nextILIDist.shape[0]), nextILIDist[:, :, i]]),
                    ignore_index=True)
            del i
            dist_temp['fc_start'] = fc_start
            fc_dist = fc_dist.append(dist_temp, ignore_index=True)
            del dist_temp

            ens_temp = pd.DataFrame(np.c_[np.repeat(fc_start, n), countries, np.repeat('pi', n), peakIntensities])
            for i in range(4):
                ens_temp = ens_temp.append(pd.DataFrame(
                    np.c_[np.repeat(fc_start, n), countries, np.repeat(str(i + 1) + 'week', n), nextILI[:, :, i]]),
                    ignore_index=True)
            del i
            fc_ens = fc_ens.append(ens_temp)
            del ens_temp

            met_temp = np.r_[
                np.repeat(fc_start, obs_pkwk.shape[1]).reshape([1, 12]), np.array(countries).reshape([1, 12]),
                obs_pkwk + wk_start - 1, pkwk_mean + wk_start - 1, delta_pkwk_mean, leadpkwk_mean, np.sqrt(pkwk_var),
                obs_peak_int, peak_intensity, intensity_err, np.sqrt(peak_intensity_var),
                totAttackObs, tot_attack, ar_err, np.sqrt(ar_var),
                obs1wk, obs2wk, obs3wk, obs4wk, fcast1wk, fcast2wk, fcast3wk, fcast4wk,
                onsetObs5 + wk_start - 1, onset5 + wk_start - 1, delta_onset5, np.sqrt(onset5_var),
                corr, rmse, corr_fcast, rmse_fcast]
            met_temp = np.transpose(met_temp)
            # print(met_temp.shape)
            met_temp = pd.DataFrame(met_temp)
            fc_met = fc_met.append(met_temp, ignore_index=True)
            del met_temp

        # End of forecasting
    # End of fitting, too

    # ### Process priors and posteriors after fitting is finished for full period ###
    # Calculate S and I by country (prior and post):
    s_prior = xprior[S_indices, :, :]
    s_post = xpost[S_indices, :, :]
    i_prior = xprior[I_indices, :, :]
    i_post = xpost[I_indices, :, :]

    s_prior_by_count = np.zeros([n, num_ens, ntrn + 1], dtype=np.float64)
    i_prior_by_count = np.zeros([n, num_ens, ntrn + 1], dtype=np.float64)
    s_post_by_count = np.zeros([n, num_ens, ntrn], dtype=np.float64)
    i_post_by_count = np.zeros([n, num_ens, ntrn], dtype=np.float64)
    for i in range(n):
        country_vals = np.array([j + n * i for j in range(n)])
        s_prior_by_count[i, :, :] = np.sum(s_prior[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
        i_prior_by_count[i, :, :] = np.sum(i_prior[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
        s_post_by_count[i, :, :] = np.sum(s_post[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
        i_post_by_count[i, :, :] = np.sum(i_post[country_vals, :, :], 0) / np.sum(N, 1)[i] * 100000
    del i

    # Calculate ensemble means and sds (prior and post; S, I, newI):
    obsprior_mean = np.mean(obsprior, 1)
    obspost_mean = np.mean(obspost, 1)
    obspost_sd = np.std(obspost, 1, ddof=1)

    sprior_mean = np.mean(s_prior_by_count, 1)
    spost_mean = np.mean(s_post_by_count, 1)
    spost_sd = np.std(s_post_by_count, 1, ddof=1)

    iprior_mean = np.mean(i_prior_by_count, 1)
    ipost_mean = np.mean(i_post_by_count, 1)
    ipost_sd = np.std(i_post_by_count, 1, ddof=1)

    # And store these in relevant data frames:
    statesprior = np.concatenate((sprior_mean.reshape([n * (ntrn + 1), 1]),
                                  iprior_mean.reshape([n * (ntrn + 1), 1]),
                                  obsprior_mean.reshape([n * (ntrn + 1), 1])), 1)
    statespost = np.concatenate((spost_mean.reshape([n * ntrn, 1]),
                                 ipost_mean.reshape([n * ntrn, 1]),
                                 obspost_mean.reshape([n * ntrn, 1]),
                                 spost_sd.reshape([n * ntrn, 1]),
                                 ipost_sd.reshape([n * ntrn, 1]),
                                 obspost_sd.reshape([n * ntrn, 1])), 1)

    states_weeks = np.array([i % (ntrn + 1) for i in range(statesprior.shape[0])]) + wk_start
    states_countries = np.array([np.ceil((i + 1) / (ntrn + 1)) - 1 for i in range(statesprior.shape[0])]).astype(int)
    statesprior = np.c_[states_weeks, states_countries, statesprior]
    statesprior = pd.DataFrame(statesprior)
    statesprior.columns = ['week', 'country', 'S', 'I', 'Est']

    states_weeks = np.array([i % ntrn for i in range(statespost.shape[0])]) + wk_start
    states_countries = np.array([np.ceil((i + 1) / ntrn) - 1 for i in range(statespost.shape[0])]).astype(int)
    statespost = np.c_[states_weeks, states_countries, statespost]
    statespost = pd.DataFrame(statespost)
    statespost.columns = ['week', 'country', 'S', 'I', 'Est', 'S_sd', 'I_sd', 'Est_sd']

    # Get parameter means and sds over time:
    # params_prior_mean = np.mean(xprior[param_indices, :, :], 1)
    params_post_mean = np.mean(xpost[param_indices, :, :], 1)
    params_post_sd = np.std(xpost[param_indices, :, :], 1, ddof=1)
    params_post_df = pd.DataFrame(np.transpose(np.concatenate((params_post_mean, params_post_sd), 0)))
    params_post_df.columns = ['L', 'D', 'R0mx', 'R0diff', 'airScale',
                              'L_sd', 'D_sd', 'R0mx_sd', 'R0diff_sd', 'airScale_sd']

    # Format outputs:
    fc_met.columns = ['fc_start', 'country',
                      'obs_pkwk', 'pkwk_mean', 'delta_pkwk_mean', 'leadpkwk_mean', 'pkwk_sd',
                      'obs_peak_int', 'peak_intensity', 'intensity_err', 'peak_intensity_sd',
                      'totAttackObs', 'tot_attack', 'delta_AR', 'AR_sd',
                      'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week',
                      'fcast_1week', 'fcast_2week', 'fcast_3week', 'fcast_4week',
                      'onsetObs5', 'onset5', 'delta_onset5', 'onset5_sd',
                      'corr', 'rmse', 'corr_fcast', 'rmse_fcast']

    statespost['fc_start'] = fc_start
    statespost['result'] = 'train'
    statespost['time'] = pd.Series(np.tile(tstep[range(0, ntrn)], n))
    # noinspection PyTypeChecker
    statespost = statespost[statespost.columns[[8, 10, 0, 9] + list(range(1, 8))]]

    out_op = statespost.append(fc_op, ignore_index=True)

    params_post_df['time'] = pd.Series(tstep[range(0, ntrn)])
    params_post_df['week'] = pd.Series(range(ntrn)) + wk_start
    params_post_df = params_post_df[params_post_df.columns[[10, 11] + list(range(10))]]

    # noinspection PyTypeChecker
    fc_dist = fc_dist[fc_dist.columns[[14] + list(range(0, 14))]]
    fc_dist.columns = ['fc_start', 'metric', 'bin'] + list(countries)
    fc_dist = fc_dist.melt(id_vars=('fc_start', 'metric', 'bin'))
    fc_dist.columns = ['fc_start', 'metric', 'bin', 'country', 'value']

    fc_ens.columns = ['fc_start', 'country', 'metric'] + list(range(1, 301))

    return fc_met, out_op, params_post_df, fc_dist, fc_ens
