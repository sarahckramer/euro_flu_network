import pandas as pd
from SIRS_ISOLATED import *
from functions_all import *


# EAKF code for python runs
# noinspection PyShadowingNames,PyTypeChecker
def EAKF_fn_ISOLATED(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range,
                     N, AH, dt, lambda_val, wk_start):
    # num_times = np.floor(len(tm_range) / tm_step)
    # nfc = nsn - ntrn # number of weeks for forecasting
    tstep = np.arange(tm_ini + tm_step + 1, nsn * tm_step + tm_ini + 2, step=tm_step)
    # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    fc_start = wk_start

    # Initialize arrays:
    xprior = np.zeros([7, num_ens, ntrn + 1], dtype=np.float64)
    xpost = np.zeros([7, num_ens, ntrn], dtype=np.float64)

    # Store results as forecasts are generated:
    fc_met = pd.DataFrame()
    fc_op = pd.DataFrame()
    fc_dist = pd.DataFrame()
    fc_ens = pd.DataFrame()

    # Set initial conditions based on input parameters:
    So = init_parms
    So[0] = So[0] * N
    So[1] = So[1] * N

    # Calculate the reproductive number at time t:
    beta_range = range(tm_range[0], max(tm_range) + 2 * tm_step)
    AH = AH[beta_range]

    a = -180
    b = np.log(So[5])
    beta = np.zeros([len(AH), num_ens], dtype=np.float64)
    for i in range(num_ens):
        beta[:, i] = (np.exp(a * AH + b[i]) + (So[4, i] - So[5, i])) / So[3, i]
    del i

    tcurrent = tm_ini
    # print(tcurrent) # Question: this is 1 less than tcurrent in R - I'm almost certain this is correct

    # Integrate forward one time step:
    Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt, tmRange=tm_range,
                                     S_0=So[0], I_0=So[1], popN=N, D_d=So[3], L_d=So[2], beta_d=beta)
    xprior[0, :, 0] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
    xprior[1, :, 0] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
    xprior[2:6, :, 0] = So[2:6]
    xprior[6, :, 0] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

    # Define the mapping operator:
    H = 6  # 7 in R, but remember python indexes differently

    # Set onset baseline:
    baseline = np.float64(500.0)

    # Begin training:
    obs_i = obs_i.to_numpy(dtype=np.float64)
    for tt in range(ntrn):
        # print(tt)

        if not np.isnan(obs_i[tt]):  # if data aren't NA, obs_var shouldn't be either

            # Inflate states/parameters:
            inflat = np.diag(np.repeat(lambda_val, 7))
            xmn = np.mean(xprior[:, :, tt], 1).reshape([7, 1])  # 7x7 times 1x7?
            x = np.matmul(inflat, xprior[:, :, tt] - np.matmul(xmn, np.ones([1, num_ens]))) + (
                np.matmul(xmn, np.ones([1, num_ens])))
            x[np.where(x < 0)] = 0
            xprior[:, :, tt] = x
            del x

            # Get the variance of the ensemble:
            obs_var = obs_vars[tt]

            # CHECK - is obs_var ever NA?
            if np.isnan(obs_var):
                print('ERROR: OEV is NA')

            # Get prior and post var:
            prior_var = np.var(xprior[H, :, tt], ddof=1)
            post_var = prior_var * (obs_var / (prior_var + obs_var))

            if prior_var == 0 or np.isnan(prior_var):
                post_var = np.float64(0)
                prior_var = np.float64(1e-3)

            # Compute prior and post means:
            prior_mean = np.mean(xprior[H, :, tt])
            post_mean = post_var * (prior_mean / prior_var + obs_i[tt] / obs_var)

            # Compute alpha and adjust distribution to conform to posterior moments:
            alp = np.sqrt(obs_var / (obs_var + prior_var))
            dy = post_mean + alp * (xprior[H, :, tt] - prior_mean) - xprior[H, :, tt]

            # Get the covariance of the prior state space and observations
            rr = np.zeros([7], dtype=np.float64)
            for j in range(7):
                C = (np.cov(xprior[j, :, tt], xprior[H, :, tt]) / prior_var)[0, 1]
                rr[j] = C
            del j
            dx = np.matmul(rr.reshape([len(rr), 1]), dy.reshape([1, len(dy)]))

            # Get the new ensemble and save prior and posterior
            xnew = xprior[:, :, tt] + dx

            # Corrections to data aphysicalities:
            xnew[range(H)] = fn_checkxnobounds_ISOLATED(xnew[range(H)], N)
            xnew[H][np.where(xnew[H] < 0)] = 0

            # Store as posterior:
            xpost[:, :, tt] = xnew

            # Integrate forward one time step:
            a = -180
            b = np.log(xpost[5, :, tt])
            beta = np.zeros([len(AH), num_ens], dtype=np.float64)
            for i in range(num_ens):
                beta[:, i] = (np.exp(a * AH + b[i]) + (xpost[4, i, tt] - xpost[5, i, tt])) / xpost[3, i, tt]
            del i

            tcurrent = tm_ini + tm_step * (tt + 1)
            Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt,
                                             tmRange=tm_range, S_0=xpost[0, :, tt], I_0=xpost[1, :, tt], popN=N,
                                             D_d=xpost[3, :, tt], L_d=xpost[2, :, tt], beta_d=beta)
            xprior[0, :, tt + 1] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
            xprior[1, :, tt + 1] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
            xprior[2:6, :, tt + 1] = xpost[2:6, :, tt]
            xprior[6, :, tt + 1] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

            # END OF TRAINING

            # If we have at least 5 weeks of training data, continue to forecast:
            if tt >= 4:
                nfc = nsn - (tt + 1)

                # Get states/parameters from most recent update:
                b = np.log(xpost[5, :, tt])
                beta = np.zeros([len(AH), num_ens], dtype=np.float64)
                for i in range(num_ens):
                    beta[:, i] = (np.exp(a * AH + b[i]) + (xpost[4, i, tt] - xpost[5, i, tt])) / xpost[3, i, tt]
                del i

                tcurrent = tm_ini + tm_step * (tt + 1)
                # FORECAST:
                fcast = np.zeros([3, num_ens, nfc])
                Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step * (nsn - tt - 1),
                                                 tmStep=dt, tmRange=tm_range, S_0=xpost[0, :, tt], I_0=xpost[1, :, tt],
                                                 popN=N, D_d=xpost[3, :, tt], L_d=xpost[2, :, tt], beta_d=beta)
                prev_newI = Sr_tmp[2][0]
                for j in range(0, nfc):
                    k = tm_step * (j + 1)
                    fcast[0, :, j] = Sr_tmp[0][k]
                    fcast[1, :, j] = Sr_tmp[1][k]

                    curr_newI = Sr_tmp[2][k]
                    fcast[2, :, j] = curr_newI - prev_newI
                    prev_newI = curr_newI
                del j

                # Calculate ensemble means and sds:
                # xprior_mean = np.mean(xprior, 1)
                # xpost_mean = np.mean(xpost, 1)
                # xpost_sd = np.std(xpost, 1, ddof=1)
                fcast_mean = np.mean(fcast, 1)
                fcast_sd = np.std(fcast, 1, ddof=1)

                xpost_mean = np.mean(xpost[:, :, range(tt + 1)], 1)  # temporary - use to assess forecasts

                # Store forecasting results so far as dataframe:
                states_weeks = np.array([i % nfc for i in range(fcast_mean.shape[1])]) + wk_start + tt + 1
                # this will give the actual week value, not a python index
                # we can insert the country name outside the EAKF function
                statesfcast = np.c_[states_weeks, fcast_mean.transpose(), fcast_sd.transpose()]
                statesfcast = pd.DataFrame(statesfcast)
                statesfcast.columns = ['week', 'S', 'I', 'Est', 'S_sd', 'I_sd', 'Est_sd']

                # Metrics for comparison:
                # Y = np.r_[np.transpose(xpost_mean), np.transpose(fcast_mean)]  # 52 x 3
                Y = np.append(xpost_mean[H], fcast_mean[2])

                Y_ens = np.zeros([num_ens, nsn], dtype=np.float64)  # 300 x 52
                Y_ens[:, range(tt + 1)] = xpost[H, :, range(tt + 1)].transpose()
                Y_ens[:, range(tt + 1, nsn)] = fcast[2]

                # ### If data are NA, Y and Y_ens should also be NA! #################
                Y[np.where(np.isnan(obs_i))] = np.nan
                for i in range(num_ens):
                    Y_ens[i][np.where(np.isnan(obs_i))] = np.nan
                del i
                #####################################################################

                # Calculate metrics to return:
                # distributions:
                peakWeeks = np.zeros([num_ens])
                peakIntensities = np.zeros([num_ens])
                totalARs = np.zeros([num_ens])
                onsets5 = np.zeros([num_ens])
                nextILI = fcast[2, :, range(4)]

                for ensmem in range(num_ens):
                    yy = Y_ens[ensmem]
                    peakWeeks[ensmem] = np.nanargmax(yy) + 1
                    peakIntensities[ensmem] = np.nanmax(yy)
                    totalARs[ensmem] = np.nansum(yy)
                    onsets5[ensmem] = findOnset(yy, baseline)[0]
                del ensmem
                del yy

                # point metrics:
                obs_pkwk = np.nanargmax(obs_i) + 1
                pkwk_mean = np.nanargmax(Y) + 1
                pkwk_var = np.var(peakWeeks, ddof=1)

                obs_peak_int = np.nanmax(obs_i)
                peak_intensity = np.nanmax(Y)
                peak_intensity_var = np.var(peakIntensities, ddof=1)

                totAttackObs = np.nansum(obs_i)
                tot_attack = np.nansum(Y)
                ar_var = np.var(totalARs, ddof=1)

                onsetObs5 = findOnset(obs_i, baseline)[0]
                onset5_var = np.nanvar(onsets5, ddof=1) if ~all(np.isnan(onsets5)) else np.nan

                # continuous error metrics:
                corr = np.ma.corrcoef(Y[np.where(~np.isnan(obs_i))], obs_i[np.where(~np.isnan(obs_i))])[0, 1]
                rmse = np.sqrt(np.nanmean(np.square(Y - obs_i)))

                corr_fcast = np.ma.corrcoef(fcast_mean[2][np.where(~np.isnan(obs_i[range(tt + 1, nsn)]))],
                                            obs_i[range(tt + 1, nsn)][np.where(~np.isnan(obs_i[range(tt + 1, nsn)]))])[
                    0, 1]
                rmse_fcast = np.sqrt(np.nanmean(np.square(fcast_mean[2] - obs_i[range(tt + 1, nsn)])))

                # 1-4 wk ahead:
                if fcast_mean.shape[1] > 0 and ~np.isnan(obs_i[tt + 1]):
                    obs1wk = obs_i[tt + 1]
                    fcast1wk = fcast_mean[2, 0]
                else:
                    obs1wk = np.nan
                    fcast1wk = np.nan
                if fcast_mean.shape[1] > 1 and ~np.isnan(obs_i[tt + 2]):
                    obs2wk = obs_i[tt + 2]
                    fcast2wk = fcast_mean[2, 1]
                else:
                    obs2wk = np.nan
                    fcast2wk = np.nan
                if fcast_mean.shape[1] > 2 and ~np.isnan(obs_i[tt + 3]):
                    obs3wk = obs_i[tt + 3]
                    fcast3wk = fcast_mean[2, 2]
                else:
                    obs3wk = np.nan
                    fcast3wk = np.nan
                if fcast_mean.shape[1] > 3 and ~np.isnan(obs_i[tt + 4]):
                    obs4wk = obs_i[tt + 4]
                    fcast4wk = fcast_mean[2, 3]
                else:
                    obs4wk = np.nan
                    fcast4wk = np.nan

                # leads/errors:
                leadpkwk_mean = pkwk_mean - (tt + 1)
                delta_pkwk_mean = pkwk_mean - obs_pkwk
                intensity_err = peak_intensity - obs_peak_int
                ar_err = tot_attack - totAttackObs

                # Calculate distributions for onset:
                onsets5DistNA = np.array([-1, np.around(len(onsets5[np.where(np.isnan(onsets5))]) / num_ens, 4)],
                                         dtype=np.float64)
                # onsets5DistNA = np.empty([2], dtype=np.float64)
                # onsets5DistNA[0] = -1
                # onsets5DistNA[1] = np.around(len(onsets5[np.where(np.isnan(onsets5))]) / num_ens, 4)
                onsets5 = onsets5[np.where(~np.isnan(onsets5))]
                onsets5Dist = np.empty([len(np.unique(onsets5)), 2], dtype=np.float64)
                row = 0
                for i in np.sort(np.unique(onsets5)):
                    onsets5Dist[row, 0] = i
                    onsets5Dist[row, 1] = np.around(len(onsets5[np.where(onsets5 == i)]) / num_ens, 4)
                    row += 1
                del i
                if onsets5DistNA[1] <= np.max(onsets5Dist[:, 1]):
                    onset5 = findOnset(Y, baseline)[0]
                else:
                    onset5 = np.nan
                onsets5Dist = np.r_[onsets5Dist, onsets5DistNA.reshape([1, 2])]
                onsets5Dist = pd.DataFrame(onsets5Dist)
                del onsets5DistNA

                # Calculate error in onsets:
                delta_onset5 = onset5 - onsetObs5

                # Calculate probability distributions for peak weeks:
                peakWeeksDistNA = np.array([-1, np.around(len(peakWeeks[np.where(np.isnan(peakWeeks))]) / num_ens, 4)],
                                           dtype=np.float64)
                peakWeeks = peakWeeks[np.where(~np.isnan(peakWeeks))]
                peakWeeksDist = np.empty([len(np.unique(peakWeeks)), 2], dtype=np.float64)
                row = 0
                for i in np.sort(np.unique(peakWeeks)):
                    peakWeeksDist[row, 0] = i
                    peakWeeksDist[row, 1] = np.around(len(peakWeeks[np.where(peakWeeks == i)]) / num_ens, 4)
                    row += 1
                del i
                peakWeeksDist = np.r_[peakWeeksDist, peakWeeksDistNA.reshape([1, 2])]
                peakWeeksDist = pd.DataFrame(peakWeeksDist)
                del peakWeeksDistNA

                # Calculate probability distribution for peak intensities:
                reqLimits = np.arange(14001, step=500)
                peakIntensities = peakIntensities[np.where(~np.isnan(peakIntensities))]
                peakIntensitiesDist = np.empty([len(reqLimits), 2], dtype=np.float64)
                row = 0

                for i in range(1, len(reqLimits)):
                    peakIntensitiesDist[row, 0] = reqLimits[i]
                    peakIntensitiesDist[row, 1] = np.around(len(peakIntensities[np.where(np.greater_equal(
                        peakIntensities, reqLimits[i - 1]) & np.less(peakIntensities, reqLimits[i]))]) / num_ens, 4)
                    row += 1
                del i
                peakIntensitiesDist[row, 0] = 1e5
                peakIntensitiesDist[row, 1] = np.around(len(peakIntensities[np.where(np.greater_equal(
                    peakIntensities, np.max(reqLimits)))]) / num_ens, 4)
                peakIntensitiesDist = pd.DataFrame(peakIntensitiesDist)

                # Calculate probability distribution for next 4 weeks:
                nextILIDist = np.empty([len(reqLimits), 2, nextILI.shape[0]], dtype=np.float64)
                row = 0
                for i in range(1, len(reqLimits)):
                    nextILIDist[row, 0] = reqLimits[i]
                    for k in range(nextILI.shape[0]):
                        values = nextILI[k]
                        nextILIDist[row, 1, k] = np.around(len(values[np.where(np.greater_equal(
                            values, reqLimits[i - 1]) & np.less(values, reqLimits[i]))]) / num_ens, 4)
                    row += 1
                nextILIDist[row, 0] = 1e5
                for k in range(nextILI.shape[0]):
                    values = nextILI[k]
                    nextILIDist[row, 1, k] = np.around(
                        len(values[np.where(np.greater_equal(values, max(reqLimits)))]) / num_ens, 4)
                del i
                del k

                # Store results from this round of forecasting:
                fc_start = tt + wk_start
                statesfcast['fc_start'] = fc_start
                statesfcast['result'] = 'fcast'
                statesfcast['time'] = pd.Series(tstep[range(tt + 1, nsn)])
                statesfcast = statesfcast[statesfcast.columns[[7, 9, 0, 8, 1, 2, 3, 4, 5, 6]]]
                fc_op = fc_op.append(statesfcast, ignore_index=True)

                dist_temp = pd.DataFrame(np.r_[np.c_[np.repeat('onset5', onsets5Dist.shape[0]), onsets5Dist],
                                               np.c_[np.repeat('pw', peakWeeksDist.shape[0]), peakWeeksDist],
                                               np.c_[
                                                   np.repeat('pi', peakIntensitiesDist.shape[0]), peakIntensitiesDist]])
                for i in range(4):
                    dist_temp = dist_temp.append(pd.DataFrame(
                        np.c_[np.repeat('nextweek' + str(i + 1), nextILIDist.shape[0]), nextILIDist[:, :, i]]),
                                                 ignore_index=True)
                del i
                dist_temp['fc_start'] = fc_start
                fc_dist = fc_dist.append(dist_temp, ignore_index=True)
                del dist_temp

                ens_temp = pd.DataFrame(np.append([fc_start, 'pi'], peakIntensities))
                for i in range(4):
                    ens_temp = pd.concat([ens_temp, pd.DataFrame(np.append([fc_start, str(i + 1) + 'week'],
                                                                           nextILI[i]))], axis=1)
                del i
                ens_temp = ens_temp.transpose()
                fc_ens = fc_ens.append(ens_temp)
                del ens_temp

                met_temp = np.array([fc_start, obs_pkwk + wk_start - 1, pkwk_mean + wk_start - 1, delta_pkwk_mean,
                                     leadpkwk_mean, np.sqrt(pkwk_var), obs_peak_int, peak_intensity, intensity_err,
                                     np.sqrt(peak_intensity_var), totAttackObs, tot_attack, ar_err, np.sqrt(ar_var),
                                     obs1wk, obs2wk, obs3wk, obs4wk, fcast1wk, fcast2wk, fcast3wk, fcast4wk,
                                     onsetObs5 + wk_start - 1, onset5 + wk_start - 1, delta_onset5, np.sqrt(onset5_var),
                                     corr, rmse, corr_fcast, rmse_fcast])
                met_temp = pd.DataFrame(met_temp)
                met_temp = met_temp.transpose()
                fc_met = fc_met.append(met_temp, ignore_index=True)
                del met_temp

                # End of forecasting

        else:
            # Run forward without fitting:
            a = -180
            b = np.log(xprior[5, :, tt])
            beta = np.zeros([len(AH), num_ens], dtype=np.float64)
            for i in range(num_ens):
                beta[:, i] = (np.exp(a * AH + b[i]) + (xprior[4, i, tt] - xprior[5, i, tt])) / xprior[3, i, tt]
            del i

            tcurrent = tm_ini + tm_step * (tt + 1)
            Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt,
                                             tmRange=tm_range, S_0=xprior[0, :, tt], I_0=xprior[1, :, tt], popN=N,
                                             D_d=xprior[3, :, tt], L_d=xprior[2, :, tt], beta_d=beta)
            xprior[0, :, tt + 1] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
            xprior[1, :, tt + 1] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
            xprior[2:6, :, tt + 1] = xprior[2:6, :, tt]
            xprior[6, :, tt + 1] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

    # ### Process priors and posteriors after fitting is finished for full period ###
    # Calculate ensemble means and sds:
    xprior_mean = np.mean(xprior, 1)
    xpost_mean = np.mean(xpost, 1)
    xpost_sd = np.std(xpost, 1, ddof=1)

    states_weeks = np.array([i % (ntrn + 1) for i in range(xprior_mean.shape[1])]) + wk_start
    statesprior = np.c_[states_weeks, xprior_mean.transpose()]
    statesprior = pd.DataFrame(statesprior)
    statesprior.columns = ['week', 'S', 'I', 'L', 'D', 'R0mx', 'R0diff', 'Est']

    states_weeks = np.array([i % ntrn for i in range(xpost_mean.shape[1])]) + wk_start
    statespost = np.c_[states_weeks, xpost_mean.transpose(), xpost_sd.transpose()]
    statespost = pd.DataFrame(statespost)
    statespost.columns = ['week', 'S', 'I', 'L', 'D', 'R0mx', 'R0diff', 'Est',
                          'S_sd', 'I_sd', 'L_sd', 'D_sd', 'R0mx_sd', 'R0diff_sd', 'Est_sd']

    # Format outputs:
    fc_met.columns = ['fc_start',
                      'obs_pkwk', 'pkwk_mean', 'delta_pkwk_mean', 'leadpkwk_mean', 'pkwk_sd',
                      'obs_peak_int', 'peak_intensity', 'intensity_err', 'peak_intensity_sd',
                      'totAttackObs', 'tot_attack', 'delta_AR', 'AR_sd',
                      'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week',
                      'fcast_1week', 'fcast_2week', 'fcast_3week', 'fcast_4week',
                      'onsetObs5', 'onset5', 'delta_onset5', 'onset5_sd',
                      'corr', 'rmse', 'corr_fcast', 'rmse_fcast']

    statespost['fc_start'] = fc_start
    statespost['result'] = 'train'
    statespost['time'] = pd.Series(tstep[range(0, ntrn)])
    statespost = statespost[statespost.columns[[15, 17, 0, 16] + list(range(1, 15))]]
    out_op = statespost.append(fc_op, ignore_index=True, sort=False)

    fc_dist = fc_dist[fc_dist.columns[[3, 0, 1, 2]]]
    fc_dist.columns = ['fc_start', 'metric', 'bin', 'value']

    fc_ens.columns = ['fc_start', 'metric'] + list(range(1, 301))

    return fc_met, out_op, fc_dist, fc_ens


def EAKF_fn_fitOnly_ISOLATED(num_ens, tm_step, init_parms, obs_i, ntrn, nsn, obs_vars, tm_ini, tm_range,
                             N, AH, dt, lambda_val, wk_start):
    # num_times = np.floor(len(tm_range) / tm_step)
    # nfc = nsn - ntrn # number of weeks for forecasting
    tstep = np.arange(tm_ini + tm_step + 1, nsn * tm_step + tm_ini + 2, step=tm_step)
    # Question: don't need -1 b/c already adjusted tm_ini for off-by-one-ness?
    fc_start = wk_start

    # Initialize arrays:
    xprior = np.zeros([7, num_ens, ntrn + 1], dtype=np.float64)
    xpost = np.zeros([7, num_ens, ntrn], dtype=np.float64)

    # Set initial conditions based on input parameters:
    So = init_parms
    So[0] = So[0] * N
    So[1] = So[1] * N

    # Calculate the reproductive number at time t:
    beta_range = range(tm_range[0], max(tm_range) + 2 * tm_step)
    AH = AH[beta_range]

    a = -180
    b = np.log(So[5])
    beta = np.zeros([len(AH), num_ens], dtype=np.float64)
    for i in range(num_ens):
        beta[:, i] = (np.exp(a * AH + b[i]) + (So[4, i] - So[5, i])) / So[3, i]
    del i

    tcurrent = tm_ini
    # print(tcurrent) # Question: this is 1 less than tcurrent in R - I'm almost certain this is correct

    # Integrate forward one time step:
    Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt, tmRange=tm_range,
                                     S_0=So[0], I_0=So[1], popN=N, D_d=So[3], L_d=So[2], beta_d=beta)
    xprior[0, :, 0] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
    xprior[1, :, 0] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
    xprior[2:6, :, 0] = So[2:6]
    xprior[6, :, 0] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

    # Define the mapping operator:
    H = 6  # 7 in R, but remember python indexes differently

    # Set onset baseline:
    baseline = np.float64(500.0)

    # Begin training:
    obs_i = obs_i.to_numpy(dtype=np.float64)
    for tt in range(ntrn):
        # print(tt)

        if not np.isnan(obs_i[tt]):  # if data aren't NA, obs_var shouldn't be either

            # Inflate states/parameters:
            inflat = np.diag(np.repeat(lambda_val, 7))
            xmn = np.mean(xprior[:, :, tt], 1).reshape([7, 1])  # 7x7 times 1x7?
            x = np.matmul(inflat, xprior[:, :, tt] - np.matmul(xmn, np.ones([1, num_ens]))) + (
                np.matmul(xmn, np.ones([1, num_ens])))
            x[np.where(x < 0)] = 0
            xprior[:, :, tt] = x
            del x

            # Get the variance of the ensemble:
            obs_var = obs_vars[tt]

            # CHECK - is obs_var ever NA?
            if np.isnan(obs_var):
                print('ERROR: OEV is NA')

            # Get prior and post var:
            prior_var = np.var(xprior[H, :, tt], ddof=1)
            post_var = prior_var * (obs_var / (prior_var + obs_var))

            if prior_var == 0 or np.isnan(prior_var):
                post_var = np.float64(0)
                prior_var = np.float64(1e-3)

            # Compute prior and post means:
            prior_mean = np.mean(xprior[H, :, tt])
            post_mean = post_var * (prior_mean / prior_var + obs_i[tt] / obs_var)

            # Compute alpha and adjust distribution to conform to posterior moments:
            alp = np.sqrt(obs_var / (obs_var + prior_var))
            dy = post_mean + alp * (xprior[H, :, tt] - prior_mean) - xprior[H, :, tt]

            # Get the covariance of the prior state space and observations
            rr = np.zeros([7], dtype=np.float64)
            for j in range(7):
                C = (np.cov(xprior[j, :, tt], xprior[H, :, tt]) / prior_var)[0, 1]
                rr[j] = C
            del j
            dx = np.matmul(rr.reshape([len(rr), 1]), dy.reshape([1, len(dy)]))

            # Get the new ensemble and save prior and posterior
            xnew = xprior[:, :, tt] + dx

            # Corrections to data aphysicalities:
            xnew[range(H)] = fn_checkxnobounds_ISOLATED(xnew[range(H)], N)
            xnew[H][np.where(xnew[H] < 0)] = 0

            # Store as posterior:
            xpost[:, :, tt] = xnew

            # Integrate forward one time step:
            a = -180
            b = np.log(xpost[5, :, tt])
            beta = np.zeros([len(AH), num_ens], dtype=np.float64)
            for i in range(num_ens):
                beta[:, i] = (np.exp(a * AH + b[i]) + (xpost[4, i, tt] - xpost[5, i, tt])) / xpost[3, i, tt]
            del i

            tcurrent = tm_ini + tm_step * (tt + 1)
            Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt,
                                             tmRange=tm_range, S_0=xpost[0, :, tt], I_0=xpost[1, :, tt], popN=N,
                                             D_d=xpost[3, :, tt], L_d=xpost[2, :, tt], beta_d=beta)
            xprior[0, :, tt + 1] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
            xprior[1, :, tt + 1] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
            xprior[2:6, :, tt + 1] = xpost[2:6, :, tt]
            xprior[6, :, tt + 1] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

            # END OF TRAINING
        else:
            # Run forward without fitting:
            a = -180
            b = np.log(xprior[5, :, tt])
            beta = np.zeros([len(AH), num_ens], dtype=np.float64)
            for i in range(num_ens):
                beta[:, i] = (np.exp(a * AH + b[i]) + (xprior[4, i, tt] - xprior[5, i, tt])) / xprior[3, i, tt]
            del i

            tcurrent = tm_ini + tm_step * (tt + 1)
            Sr_tmp = propagate_SIRS_ISOLATED(tmStrt=tcurrent + dt, tmEnd=tcurrent + tm_step, tmStep=dt,
                                             tmRange=tm_range, S_0=xprior[0, :, tt], I_0=xprior[1, :, tt], popN=N,
                                             D_d=xprior[3, :, tt], L_d=xprior[2, :, tt], beta_d=beta)
            xprior[0, :, tt + 1] = Sr_tmp[0][Sr_tmp[0].shape[0] - 1]
            xprior[1, :, tt + 1] = Sr_tmp[1][Sr_tmp[1].shape[0] - 1]
            xprior[2:6, :, tt + 1] = xprior[2:6, :, tt]
            xprior[6, :, tt + 1] = Sr_tmp[2][Sr_tmp[2].shape[0] - 1]

    # ### Process priors and posteriors after fitting is finished for full period ###
    # Calculate ensemble means and sds:
    xprior_mean = np.mean(xprior, 1)
    xpost_mean = np.mean(xpost, 1)
    xpost_sd = np.std(xpost, 1, ddof=1)

    states_weeks = np.array([i % (ntrn + 1) for i in range(xprior_mean.shape[1])]) + wk_start
    statesprior = np.c_[states_weeks, xprior_mean.transpose()]
    statesprior = pd.DataFrame(statesprior)
    statesprior.columns = ['week', 'S', 'I', 'L', 'D', 'R0mx', 'R0diff', 'Est']

    states_weeks = np.array([i % ntrn for i in range(xpost_mean.shape[1])]) + wk_start
    statespost = np.c_[states_weeks, xpost_mean.transpose(), xpost_sd.transpose()]
    statespost = pd.DataFrame(statespost)
    statespost.columns = ['week', 'S', 'I', 'L', 'D', 'R0mx', 'R0diff', 'Est',
                          'S_sd', 'I_sd', 'L_sd', 'D_sd', 'R0mx_sd', 'R0diff_sd', 'Est_sd']

    # Format outputs:
    statespost['fc_start'] = fc_start
    statespost['result'] = 'train'
    statespost['time'] = pd.Series(tstep[range(0, ntrn)])
    statespost = statespost[statespost.columns[[15, 17, 0, 16] + list(range(1, 15))]]

    return statespost