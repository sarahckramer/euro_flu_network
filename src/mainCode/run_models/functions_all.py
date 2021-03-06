# Contains all functions needed to run forecasting/fitting mainCode #

import numpy as np
from numba import jit


# Function to replace any NAs in the observations that occur before first/after last non-zero data point
def replaceLeadLag(vals_temp):
    if any(np.isnan(vals_temp)) and not np.isnan(vals_temp).all():
        start_index = 0
        while vals_temp[start_index] == 0 or np.isnan(vals_temp[start_index]):
            start_index += 1
        start_index = start_index  # - 1

        end_index = len(vals_temp) - 1
        while vals_temp[end_index] == 0 or np.isnan(vals_temp[end_index]):
            end_index -= 1
        end_index = end_index + 1

        if start_index >= 0:
            vals_temp[:start_index] = 0

        if end_index < len(vals_temp):
            vals_temp[end_index:] = 0

    return vals_temp


# Function to calculate OEV
def calc_obsvars(obs, oev_base, oev_denom, n):
    obs = obs.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)

    tmp = np.zeros([obs.shape[0], obs.shape[1]], dtype=np.float64)
    for i in range(obs.shape[1]):
        for j in range(2, obs.shape[0]):
            tmp[j, i] = np.NaN if np.all(
                obs[range(j - 2, j + 1), i] != obs[range(j - 2, j + 1), i]) else np.nanmean(
                obs[range(j - 2, j + 1), i])
        tmp[1, i] = np.NaN if np.all(obs[0:2, i] != obs[0:2, i]) else np.nanmean(obs[0:2, i])
        if not np.isnan(obs[0, i]):
            tmp[0, i] = obs[0, i]
        else:
            tmp[0, i] = np.nan

    vars_temp = np.zeros([obs.shape[0], obs.shape[1]], dtype=np.float64)
    for i in range(obs.shape[1]):
        vars_temp[:, i] = oev_base + (np.square(tmp[:, i]) / oev_denom)

    return vars_temp


# Function to calculate OEV, taking into account the number of tests conducted in each country,
# the number of syndromic cases (scaled), and percent positive for influenza
# Tested but not ultimately used; this is a more complex way of assigning error by country,
# but did not perform well here
def calc_obsvars_nTest(obs, syndat, ntests, posprops, oev_base, oev_denom, n):
    # noinspection PyUnusedLocal
    obs = obs.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)
    syndat = syndat.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)
    ntests = ntests.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)
    posprops = posprops.iloc[:, 1:(n + 1)].to_numpy(dtype=np.float64)
    # print(posprops.shape)

    tmp = np.zeros([posprops.shape[0], posprops.shape[1]], dtype=np.float64)
    for i in range(posprops.shape[1]):
        for j in range(2, posprops.shape[0]):
            tmp[j, i] = np.NaN if np.all(
                posprops[range(j - 2, j + 1), i] != posprops[range(j - 2, j + 1), i]) else np.nanmean(
                posprops[range(j - 2, j + 1), i])
            # tmp[j, i] = np.nanmean(posprops[range(j - 2, j + 1), i])
        tmp[1, i] = np.NaN if np.all(posprops[0:2, i] != posprops[0:2, i]) else np.nanmean(posprops[0:2, i])
        # tmp[1, i] = np.nanmean(posprops[0:1, i])
        if not np.isnan(posprops[0, i]):
            tmp[0, i] = posprops[0, i]
        else:
            tmp[0, i] = np.nan

    tmp_syn = np.zeros([syndat.shape[0], syndat.shape[1]], dtype=np.float64)
    for i in range(syndat.shape[1]):
        for j in range(2, syndat.shape[0]):
            tmp_syn[j, i] = np.NaN if np.all(
                syndat[range(j - 2, j + 1), i] != syndat[range(j - 2, j + 1), i]) else np.nanmean(
                syndat[range(j - 2, j + 1), i])
        tmp_syn[1, i] = np.NaN if np.all(syndat[0:2, i] != syndat[0:2, i]) else np.nanmean(syndat[0:2, i])
        if not np.isnan(syndat[0, i]):
            tmp_syn[0, i] = syndat[0, i]
        else:
            tmp_syn[0, i] = np.nan

    tmp_test = np.zeros([ntests.shape[0], ntests.shape[1]], dtype=np.float64)
    for i in range(ntests.shape[1]):
        for j in range(2, ntests.shape[0]):
            tmp_test[j, i] = np.NaN if np.all(
                ntests[range(j - 2, j + 1), i] != ntests[range(j - 2, j + 1), i]) else np.nanmean(
                ntests[range(j - 2, j + 1), i])
        tmp_test[1, i] = np.NaN if np.all(ntests[0:2, i] != ntests[0:2, i]) else np.nanmean(ntests[0:2, i])
        if not np.isnan(ntests[0, i]):
            tmp_test[0, i] = ntests[0, i]
        else:
            tmp_test[0, i] = np.nan

    # NAs here are either NAs in data, or else leading/lagging 0s (which were originally NAs)
    # Originally, error in first 2 weeks was just set to oev_base

    # Incorporate syndromic counts:
    vars_temp = np.zeros([posprops.shape[0], posprops.shape[1]], dtype=np.float64)
    for i in range(posprops.shape[1]):
        vars_temp[:, i] = (np.square(tmp_syn[:, i]) / tmp_test[:, i]) * (oev_base + (np.square(tmp[:, i]) / oev_denom))

    return vars_temp


# Function to calculate OEV for an individual country
def calc_obsvars_ISOLATED(obs, oev_base, oev_denom):
    obs = obs.to_numpy(dtype=np.float64)

    tmp = np.zeros([obs.shape[0], 1], dtype=np.float64)
    for j in range(2, obs.shape[0]):
        tmp[j] = np.NaN if np.all(
            obs[range(j - 2, j + 1)] != obs[range(j - 2, j + 1)]) else np.nanmean(
            obs[range(j - 2, j + 1)])
    tmp[1] = np.NaN if np.all(obs[0:2] != obs[0:2]) else np.nanmean(obs[0:2])
    if not np.isnan(obs[0]):
        tmp[0] = obs[0]
    else:
        tmp[0] = np.nan

    vars_temp = oev_base + (np.square(tmp) / oev_denom)

    return vars_temp.transpose()[0]


# Function to ensure states and parameters remain within reasonable bounds (network)
# @jit(nopython=True, nogil=True, parallel=False, cache=True)
@jit(nopython=True)
def fn_checkxnobounds(xnew, popN, n_count):
    n_ens = xnew.shape[1]  # number of ensemble members
    n_var = xnew.shape[0]  # number of variables

    S_rows = [i for i in range(np.square(n_count))]
    I_rows = [i + np.square(n_count) for i in S_rows]
    newI_rows = [i + np.square(n_count) + np.square(n_count) for i in S_rows]
    param_rows = [i for i in range(np.square(n_count) + np.square(n_count) + np.square(n_count),
                                   np.square(n_count) + np.square(n_count) + np.square(n_count) + 5)]

    for ii in S_rows:
        ind0 = int(np.ceil((ii + 1) / n_count) - 1)
        ind1 = ii % n_count
        for jj in range(n_ens):
            if xnew[ii, jj] > popN[jj, ind0, ind1]:
                xnew[ii, jj] = popN[jj, ind0, ind1] - 1
        # This causes output to change very slightly
        # ug = np.max(xnew[ii])
        # if ug > popN[int(np.ceil((ii + 1) / n_count) - 1), ii % n_count]:
        #     ind0 = int(np.ceil((ii + 1) / n_count) - 1)
        #     ind1 = ii % n_count
        #     for jj in range(n_ens):
        #         if xnew[ii, jj] > popN[ind0, ind1]:
        #             # print('We did it! (S)')
        #             xnew[ii, jj] = popN[ind0, ind1] - 1
    # print('check')
    for ii in I_rows:
        ind0 = int(np.ceil((ii + 1) / n_count) - n_count - 1)
        ind1 = ii % n_count
        for jj in range(n_ens):
            if xnew[ii, jj] > popN[jj, ind0, ind1]:
                xnew[ii, jj] = np.median(xnew[ii])
        # This and setting newI to median below have no impact!! (At least with "original" seasonal matrices)
        # ug = np.max(xnew[ii])
        # if ug > popN[int(np.ceil((ii + 1) / n_count) - n_count - 1), ii % n_count]:
        #     ind0 = int(np.ceil((ii + 1) / n_count) - n_count - 1)
        #     ind1 = ii % n_count
        #     for jj in range(n_ens):
        #         if xnew[ii, jj] > popN[ind0, ind1]:
        #             # print('We did it! (I)')
        #             xnew[ii, jj] = np.median(xnew[ii])  # popN[ind0, ind1]
    # print('check')

    # Should probably add one of these for newI, too...
    for ii in newI_rows:
        ind0 = int(np.ceil((ii + 1) / n_count) - n_count - n_count - 1)
        ind1 = ii % n_count
        for jj in range(n_ens):
            if xnew[ii, jj] > popN[jj, ind0, ind1]:
                xnew[ii, jj] = np.median(xnew[ii])
        # ug = np.max(xnew[ii])
        # if ug > popN[int(np.ceil((ii + 1) / n_count) - n_count - n_count - 1), ii % n_count]:
        #     ind0 = int(np.ceil((ii + 1) / n_count) - n_count - n_count - 1)
        #     ind1 = ii % n_count
        #     for jj in range(n_ens):
        #         if xnew[ii, jj] > popN[ind0, ind1]:
        #             # print('We did it! (newI)')
        #             xnew[ii, jj] = np.median(xnew[ii])  # popN[ind0, ind1]

    # Don't really need this chunk, b/c above just set to 0?
    # ADD THIS IN
    # Where states/params are negative, set to 0 or the mean of all ensembles for that state/parameter
    # Potential issue: Ensure that no empty compartments are set to anything other than 0!
    ug = np.min(xnew)
    if ug <= 0:
        # print('New function part used.')
        for jj in range(n_ens):
            for ii in range(n_var):
                if xnew[ii, jj] <= 0:
                    xnew[ii, jj] = np.maximum(np.mean(xnew[ii]), 1)
                    # if np.maximum(np.mean(xnew[ii]), 0) != np.max(np.mean(xnew[ii, :]), 0):
                    #     print('!!!')
                    #     print(np.maximum(np.mean(xnew[ii]), 0))
                    #     print(np.max(np.mean(xnew[ii]), 0))
                    #     print(xnew[ii])
                    #     print()
                    # Sasi had it as max of mean and 1, but we don't want 1's in empty compartments!

    ug = np.min(xnew[param_rows[2]])  # Correct if R0mx < 0.5
    if ug < 0.5:
        for jj in range(n_ens):
            if xnew[param_rows[2], jj] < 0.5:
                # print('R0mx changed!')
                xnew[param_rows[2], jj] = np.maximum(np.median(xnew[param_rows[2]]), 0.5)

    ug = np.min(xnew[param_rows[3]])  # Correct if R0diff < 0.01
    if ug < 0.01:
        for jj in range(n_ens):
            if xnew[param_rows[3], jj] < 0.01:
                # print('We did it! (R0diff)')
                xnew[param_rows[3], jj] = np.maximum(np.median(xnew[param_rows[3]]), 0.01)

    ug = np.min(xnew[param_rows[0]])  # Correct if L < 200 days
    if ug < 200.0:
        for jj in range(n_ens):
            if xnew[param_rows[0], jj] < 200.0:
                # print('We did it! (L)')
                xnew[param_rows[0], jj] = np.maximum(np.median(xnew[param_rows[0]]), 200.0)

    ug = np.min(xnew[param_rows[1]])  # Correct if D < 0.5 days
    if ug <= 1.0:
        for jj in range(n_ens):
            if xnew[param_rows[1], jj] < 0.5:
                # print('We did it! (D)')
                xnew[param_rows[1], jj] = np.maximum(np.median(xnew[param_rows[1]]), 0.5)

    ug = np.min(xnew[param_rows[2]] - xnew[param_rows[3]])  # Correct if R0mx < R0diff
    if ug <= 0:
        for jj in range(n_ens):
            if xnew[param_rows[2], jj] < xnew[param_rows[3], jj]:
                # print('We did it! (R0mx/R0diff)')
                xnew[param_rows[2], jj] = xnew[param_rows[3], jj]

    return xnew


# Function to ensure observed states remain within reasonable bounds (network)
def fn_checkxnobounds_obsens(xnew, popN, n_count):
    n_ens = xnew.shape[1]  # number of ensemble members
    # n_var = xnew.shape[0]  # number of variables (should be n_count)
    # print(n_var == n_count)

    for ii in range(n_count):
        ug = np.max(xnew[ii, :])
        if ug > np.sum(popN[ii]):
            print('Obs adjusted above N')
            print(ii)
            for jj in range(n_ens):
                if xnew[ii, jj] > np.sum(popN[ii]):
                    xnew[ii, jj] = np.sum(popN[ii])

    ug = np.min(xnew)
    if ug < 0:
        # print('Obs adjusted below 0')
        for jj in range(n_ens):
            for ii in range(n_count):
                if xnew[ii, jj] <= 0:
                    xnew[ii, jj] = np.maximum(np.mean(xnew[ii, :]), 0)
                    # Sasi had it as max of mean and 1, but this keeps it consistent with function for x

    return xnew


# Function to ensure states and parameters remain within reasonable bounds (isolated)
def fn_checkxnobounds_ISOLATED(xnew, N):
    n_ens = xnew.shape[1]  # number of ensemble members
    n_var = xnew.shape[0]  # number of variables

    # Correct if S > N
    ug = np.max(xnew[0])
    if ug > N:
        for jj in range(n_ens):
            if xnew[0, jj] > N:
                xnew[0, jj] = N-1

    # Corrects if I > N:
    ug = np.max(xnew[1])
    if ug > N:
        for jj in range(n_ens):
            if xnew[1, jj] > N:
                xnew[1, jj] = np.median(xnew[1])

    # Corrects if any state or parameter goes negative:
    ug = np.min(xnew)
    if ug <= 0:
        for jj in range(n_ens):
            for ii in range(n_var):
                if xnew[ii, jj] <= 0:
                    xnew[ii, jj] = np.maximum(np.mean(xnew[ii]), 1)  # could be 1 instead of 0, if no empty compartments
    # for network, 0 even makes more sense, b/c allows more chance of die-out
    # but for individual model, we don't necessarily want cases to be 0, right?
    # although filter can pull them back up later

    # Corrects if R0mx < 0.5:
    ug = np.min(xnew[4])
    if ug < 0.5:
        for jj in range(n_ens):
            if xnew[4, jj] < 0.5:
                xnew[4, jj] = np.maximum(np.median(xnew[4]), 0.5)

    # Corrects if R0diff < 0.01:
    ug = np.min(xnew[5])
    if ug < 0.01:
        for jj in range(n_ens):
            if xnew[5, jj] < 0.01:
                xnew[5, jj] = np.maximum(np.median(xnew[5]), 0.01)

    # Corrects if L < 200 days:
    ug = np.min(xnew[2])
    if ug < 200.0:
        for jj in range(n_ens):
            if xnew[2, jj] < 200.0:
                xnew[2, jj] = np.maximum(np.median(xnew[2]), 200.0)

    # Corrects if D < 0.5:
    ug = np.min(xnew[3])
    if ug <= 1.0:
        for jj in range(n_ens):
            if xnew[3, jj] < 0.5:
                xnew[3, jj] = np.maximum(np.median(xnew[3]), 0.5)

    # Corrects if R0mx < R0diff:
    ug = np.min(xnew[4] - xnew[5])
    if ug <= 0:
        for jj in range(n_ens):
            if xnew[4, jj] < xnew[5, jj]:
                xnew[4, jj] = xnew[5, jj]

    return xnew


# Function to identify the observed onset week
def findOnset(vals, baseline):
    onset = np.nan
    end = np.nan
    duration = np.nan

    # above = np.where((vals > baseline) & (~np.isnan(vals)))[0]
    above = np.where(np.greater(vals, baseline, where=~np.isnan(vals)) & ~np.isnan(vals))[0]

    if len(above) > 2:
        for i in range(len(above)):
            if above[i] + 1 in above and above[i] + 2 in above:
                onset = above[i] + 1
                break

        for i in range(len(above) - 1, -1, -1):
            if above[i] - 1 in above and above[i] - 2 in above:
                end = above[i] + 1
                break

        if not np.isnan(onset) and not np.isnan(end):
            duration = end - onset + 1

    return onset, end, duration
