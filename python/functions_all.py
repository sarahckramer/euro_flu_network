import numpy as np

def replaceLeadLag(vals_temp):
        #print(vals_temp)
        #print(np.isnan(vals_temp).all())
        if any(np.isnan(vals_temp)) and not np.isnan(vals_temp).all():
            start_index = 0
            #print(vals_temp[start_index])
            while vals_temp[start_index] == 0 or np.isnan(vals_temp[start_index]):
                start_index += 1
            start_index <- start_index - 1

            #print(start_index)

            end_index = len(vals_temp) - 1
            while vals_temp[end_index] == 0 or np.isnan(vals_temp[end_index]):
                end_index -= 1
            end_index = end_index + 1

            #print(end_index)

            if start_index >= 0:
                vals_temp[:start_index] = 0
                
            if end_index < len(vals_temp):
                vals_temp[end_index:] = 0

            #print(vals_temp)
            return vals_temp

def calc_obsvars_nTest(obs, syndat, ntests, posprops, oev_base, oev_denom, tmp_exp, n):
        #print(posprops.iloc[:, 1:(n+1)])
        
        obs = obs.iloc[:, 1:(n + 1)].to_numpy()
        syndat = syndat.iloc[:, 1:(n + 1)].to_numpy()
        ntests = ntests.iloc[:, 1:(n + 1)].to_numpy()
        posprops = posprops.iloc[:, 1:(n + 1)].to_numpy()
        #print(posprops.shape)

        tmp = np.empty([posprops.shape[0], posprops.shape[1]])
        for i in range(posprops.shape[1]):
            for j in range(2, posprops.shape[0]):
                tmp[j, i] = np.NaN if np.all(posprops[range(j - 2, j + 1), i] != posprops[range(j - 2, j + 1), i]) else np.nanmean(posprops[range(j - 2, j + 1), i])
                #tmp[j, i] = np.nanmean(posprops[range(j - 2, j + 1), i])
            tmp[1, i] = np.NaN if np.all(posprops[0:2, i] != posprops[0:2, i]) else np.nanmean(posprops[0:2, i])
            #tmp[1, i] = np.nanmean(posprops[0:1, i])
            if not np.isnan(posprops[0, i]):
                tmp[0, i] = posprops[0, i]
            else:
                tmp[0, i] = 0
        #print(tmp)
        
        tmp_syn = np.empty([syndat.shape[0], syndat.shape[1]])
        for i in range(syndat.shape[1]):
            for j in range(2, syndat.shape[0]):
                tmp_syn[j, i] = np.NaN if np.all(syndat[range(j - 2, j + 1), i] != syndat[range(j - 2, j + 1), i]) else np.nanmean(syndat[range(j - 2, j + 1), i])
            tmp_syn[1, i] = np.NaN if np.all(syndat[0:2, i] != syndat[0:2, i]) else np.nanmean(syndat[0:2, i])
            if not np.isnan(syndat[0, i]):
                tmp_syn[0, i] = syndat[0, i]
            else:
                tmp_syn[0, i] = 0
        #print(tmp_syn)

        tmp_test = np.empty([ntests.shape[0], ntests.shape[1]])
        for i in range(ntests.shape[1]):
            for j in range(2, ntests.shape[0]):
                tmp_test[j, i] = np.NaN if np.all(ntests[range(j - 2, j + 1), i] != ntests[range(j - 2, j + 1), i]) else np.nanmean(ntests[range(j - 2, j + 1), i])
            tmp_test[1, i] = np.NaN if np.all(ntests[0:2, i] != ntests[0:2, i]) else np.nanmean(ntests[0:2, i])
            if not np.isnan(ntests[0, i]):
                tmp_test[0, i] = ntests[0, i]
            else:
                tmp_test[0, i] = np.NaN
        #print(tmp_test)

        # NAs here are either NAs in data, or else leading/lagging 0s (which were originally NAs)
        # Originally, error in first 2 weeks was just set to oev_base

        # Incorporate syndromic counts:
        vars_temp = np.empty([posprops.shape[0], posprops.shape[1]])
        for i in range(posprops.shape[1]):
            #print(i)
            #print(tmp_syn[:, i])
            #print(tmp_test[:, i])
            #print(tmp_syn[:, i] / tmp_test[:, i])
            vars_temp[:, i] = (np.square(tmp_syn[:, i]) / tmp_test[:, i]) * (oev_base + (np.square(tmp[:, i]) / oev_denom))
        #print(vars_temp.shape)

        return(vars_temp)

def fn_checkxnobounds(xnew, S_rows, I_rows, param_rows, popN, n_count):
    n_ens = xnew.shape[1] # number of ensemble members
    n_var = xnew.shape[0] # number of variables

    for ii in S_rows:
        ug = np.max(xnew[ii, :])
        if ug > popN[int(np.ceil((ii + 1) / n_count) - 1), ii % n_count]:
            ind0 = int(np.ceil((ii + 1) / n_count) - 1)
            ind1 = ii % n_count
            for jj in range(n_ens):
                if xnew[ii, jj] > popN[ind0, ind1]:
                    xnew[ii, jj] = popN[ind0, ind1]
    #print('check')               
    for ii in I_rows:
        ug = np.max(xnew[ii, :])
        if ug > popN[int(np.ceil((ii + 1) / n_count) - n_count - 1), ii % n_count]:
            ind0 = int(np.ceil((ii + 1) / n_count) - n_count - 1)
            ind1 = ii % n_count
            for jj in range(n_ens):
                if xnew[ii, jj] > popN[ind0, ind1]:
                    xnew[ii, jj] = popN[ind0, ind1]
    #print('check')
    
    ''' # Don't really need this chunk, b/c above just set to 0?
    ug = np.min(xnew)
    if ug < 0:
        for jj in range(n_ens):
            for ii in range(n_var):
                if xnew[ii, jj] <= 0:
                    xnew[ii, jj] = np.max(np.mean(xnew[ii, :]), 0) # Sasi had it as max of mean and 1, but we don't want 1's in empty compartments!
    '''

    ug = np.min(xnew[param_rows[0], :]) # Correct if L < 200 days
    #print(ug)
    if ug < 200.0:
        for jj in range(n_ens):
            if xnew[param_rows[0], jj] < 200.0:
                xnew[param_rows[0], jj] = np.maximum(np.median(xnew[param_rows[0], :]), 200.0)

    ug = np.min(xnew[param_rows[1], :]) # Correct if D < 0.5 days
    #print(ug)
    if ug <= 1.0:
        for jj in range(n_ens):
            if xnew[param_rows[1], jj] < 0.5:
                xnew[param_rows[1], jj] = np.maximum(np.median(xnew[param_rows[1], :]), 0.5)

    ug = np.min(xnew[param_rows[2], :] - xnew[param_rows[3], :]) # Correct if R0mx < R0diff
    #print(ug)
    if ug <= 0:
        for jj in range(n_ens):
            if xnew[param_rows[2], jj] < xnew[params_rows[3], jj]:
                xnew[param_rows[2], jj] = xnew[param_rows[3], jj]
    
    return(xnew)

def findOnset(vals, baseline):
        onset = np.nan
        end = np.nan
        duration = np.nan
        
        #above = np.where((vals > baseline) & (~np.isnan(vals)))[0]
        above = np.where(np.greater(vals, baseline, where = ~np.isnan(vals)) & ~np.isnan(vals))[0]

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

        return(onset, end, duration)

