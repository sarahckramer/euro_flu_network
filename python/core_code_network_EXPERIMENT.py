import os
import sys
from numba.typed import List
from EAKF_python import *
import datetime

# print(os.path.dirname(sys.executable))

# Turn off SettingWithCopyWarning:
pd.options.mode.chained_assignment = None

# Time benchmark start:
timestamp_start = datetime.datetime.now()

# Specify subtype:
strain = 'A(H1)'

# Specifiy experiment:
experiment = 'fnCheck_check'

# Specify global variables
dt = 1
tmstep = 7
wk_start = 40

# Parameters for filters
discrete = False
oev_base = np.float64(1e5)
oev_denom = np.float64(10.0)
lambda_val = np.float64(1.05)
num_ens = 300
num_runs = 3  # EVENTUALLY WANT 5

# Vector of countries used
countries = np.array(['AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES'])
count_indices = [0, 1, 3, 5, 6, 7, 10, 11, 12, 13, 16, 18]
n = len(countries)

# Read in additional French scalings:
scalings_fr = pd.read_csv('../data/by_subtype/scalings_frame_FR.csv')

# Read in data and set seasons:
if strain == 'A(H1)':
    seasons = ('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18')  # H1

    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H1)_SCALED.csv')
    # iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H1)_SCALED_full.csv')
    test_dat = pd.read_csv('../data/testRates_010820.csv')
    syn_dat = pd.read_csv('../data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
    pos_dat = pd.read_csv('../data/by_subtype/posprop_A(H1).csv')

    # test_dat = test_dat[iliiso.columns]
    pos_dat = pos_dat[iliiso.columns]

    scalings = pd.read_csv('../data/by_subtype/scalings_frame_A(H1).csv')
    scalings_early = pd.read_csv('../data/by_subtype/scalings_frame_A(H1).csv')
    scalings_early['gamma'][3] = scalings_fr['x'][0]

elif strain == 'A(H3)':
    seasons = ('2011-12', '2012-13', '2013-14', '2014-15', '2016-17')  # H3

    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H3)_SCALED.csv')
    # iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H3)_SCALED_full.csv')
    test_dat = pd.read_csv('../data/testRates_010820.csv')
    syn_dat = pd.read_csv('../data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
    pos_dat = pd.read_csv('../data/by_subtype/posprop_A(H3).csv')

    # test_dat = test_dat[iliiso.columns]
    pos_dat = pos_dat[iliiso.columns]

    scalings = pd.read_csv('../data/by_subtype/scalings_frame_A(H3).csv')
    scalings_early = pd.read_csv('../data/by_subtype/scalings_frame_A(H3).csv')
    scalings_early['gamma'][3] = scalings_fr['x'][1]

elif strain == 'B':
    seasons = ('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18')  # B

    iliiso = pd.read_csv('../data/by_subtype/WHO_data_B_SCALED.csv')
    test_dat = pd.read_csv('../data/testRates_010820.csv')
    syn_dat = pd.read_csv('../data/by_subtype/synDatCounts_B_SCALED.csv')
    pos_dat = pd.read_csv('../data/by_subtype/posprop_B.csv')

    # test_dat = test_dat[iliiso.columns]
    pos_dat = pos_dat[iliiso.columns]

    scalings = pd.read_csv('../data/by_subtype/scalings_frame_B.csv')
    scalings_early = pd.read_csv('../data/by_subtype/scalings_frame_B.csv')
    scalings_early['gamma'][3] = scalings_fr['x'][2]

elif strain == 'A(all)':
    seasons = ('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')  # all A

    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(all)_SCALED.csv')
    # iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(all)_SCALED_full.csv')
    test_dat = pd.read_csv('../data/testRates_010820.csv')
    syn_dat = pd.read_csv('../data/by_subtype/synDatCounts_A(all)_SCALED.csv')
    pos_dat = pd.read_csv('../data/by_subtype/posprop_A(all).csv')

    # test_dat = test_dat[iliiso.columns]
    pos_dat = pos_dat[iliiso.columns]

    scalings = pd.read_csv('../data/by_subtype/scalings_frame_A(all).csv')
    scalings_early = pd.read_csv('../data/by_subtype/scalings_frame_A(all).csv')
    scalings_early['gamma'][3] = np.float64(1.36084848)
    
else:
    print('Error: Subtype not recognized.')
    sys.exit()

# print(seasons)
# # Or, try using old ("all") scalings:
# scalings = pd.read_csv('../data/scalings_frame_ALL_red.csv')
# scalings_early = pd.read_csv('../data/scalings_frame_ALL_red.csv')
# scalings_early['gamma'][3] = np.float64(1.3)

# Read in humidity data:
ah = pd.read_csv('../data/ah_Europe_07142019.csv')
ah = ah[ah.columns[count_indices]]
ah = ah.append(ah)
ah = ah.to_numpy(dtype=np.float64)

# Read in air travel data:
a_rand = np.zeros([12, n, n], dtype=np.float64)
for i in range(n):
    a_rand[i] = np.loadtxt('air_travel/aRand' + str(i + 1) + '.txt', unpack=True, dtype=np.float64)

# Season-specific start and end dates:
clim_start_dict = {'2010-11': 276, '2011-12': 275, '2012-13': 274, '2013-14': 272, '2014-15': 271, '2015-16': 270,
                   '2016-17': 276, '2017-18': 274}
clim_end_dict = {'2010-11': 639, '2011-12': 638, '2012-13': 637, '2013-14': 635, '2014-15': 634, '2015-16': 640,
                 '2016-17': 639, '2017-18': 637}
wks_dict = {'2010-11': range(78, 130), '2011-12': range(130, 182), '2012-13': range(182, 234),
            '2013-14': range(234, 286), '2014-15': range(286, 338), '2015-16': range(338, 391),
            '2016-17': range(391, 443), '2017-18': range(443, 495)}
season_len_dict = {'2010-11': 52, '2011-12': 52, '2012-13': 52, '2013-14': 52, '2014-15': 52, '2015-16': 53,
                   '2016-17': 52, '2017-18': 52}

# ### Start main loop ###
# Initiate results frames:
outputMetrics = pd.DataFrame()
outputOP = pd.DataFrame()
outputOPParams = pd.DataFrame()
outputDist = pd.DataFrame()
outputEns = pd.DataFrame()

# GRID SEARCH:
# for oev_base in [5e4, 1e5]:  # 0.0, 0.1 or 5e4, 1e5
#     oev_base = np.float64(oev_base)
#     print(oev_base)
#
# for oev_denom in [1.0, 2.0, 10.0, 50.0]:  # 1.0, 2.0, 10.0 or 1.0, 10.0, 100.0
#     oev_denom = np.float64(oev_denom)
#     print(oev_denom)

# Loop through seasons and forecast:
for season_index in range(len(seasons)):
    season = seasons[season_index]
    print(season)

    # Get season-specific population matrix:
    N = pd.read_csv(os.path.join('compartment_sizes/', 'N' + season + '_NEW.txt'), header=None, sep='\t')
    N = N.to_numpy(dtype=np.float64)

    # Get observations for current season:
    obs_i = iliiso.iloc[wks_dict[season]]
    syn_i = syn_dat.iloc[wks_dict[season]]
    pos_i = pos_dat.iloc[wks_dict[season]]
    test_i = test_dat.iloc[wks_dict[season]]

    # Get season duration:
    nsn = season_len_dict[season]

    # Reindex data:
    obs_i = obs_i.reset_index(drop=True)
    syn_i = syn_i.reset_index(drop=True)
    pos_i = pos_i.reset_index(drop=True)
    test_i = test_i.reset_index(drop=True)

    # Replace leading/lagging zeros:
    for count_index in range(n):
        replaceLeadLag(obs_i.iloc[:, count_index + 1])
        replaceLeadLag(syn_i.iloc[:, count_index + 1])
        replaceLeadLag(pos_i.iloc[:, count_index + 1])

    # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
    for count_index in range(n):
        if any(test_i.iloc[:, count_index + 1] == 0):
            print('0s found in test data!')

    # Get OEV:
    # obs_vars = 1e5 + calc_obsvars_nTest(obs_i, syn_i, test_i, pos_i, oev_base, oev_denom, n)
    obs_vars = calc_obsvars(obs_i, oev_base, oev_denom, n)
    # DO WE NEED DATES?
    # np.savetxt('results/original_checks_R/obs_vars_old.txt', obs_vars, delimiter=',')

    # Get time start and time range:
    tm_ini = clim_start_dict[season] - 2  # b/c python indexes at 0, while R does it at 1
    tm_range1 = [i for i in range(clim_start_dict[season] - 1, clim_end_dict[season], 1)]
    tm_range = List()
    [tm_range.append(i) for i in tm_range1]

    # Run forecasts!
    for run in range(num_runs):
        print(run)

        # Get initial states/parameters for each ensemble member:
        param_init = pd.read_csv(os.path.join('initial_parms/', 'parms' + str(run) + '_NEW.txt'), header=None,
                                 sep='\t')
        param_init = param_init.to_numpy(dtype=np.float64)
        # print(param_init.shape)

        # Run EAKF:
        res = EAKF_fn(num_ens, tmstep, param_init, obs_i, 30, nsn, obs_vars, tm_ini, tm_range, n, N, ah,
                      dt, countries, a_rand, lambda_val, wk_start)  # and variables needed for SIRS

        outputMet_temp = res[0]
        outputOP_temp = res[1]
        outputOPParams_temp = res[2]
        outputDist_temp = res[3]
        outputEns_temp = res[4]

        outputMet_temp['season'] = season
        outputMet_temp['run'] = run
        outputMet_temp['oev_base'] = oev_base
        outputMet_temp['oev_denom'] = oev_denom
        outputMet_temp['lambda'] = lambda_val
        if season in ('2010-11', '2011-12', '2012-13', '2013-14'):
            outputMet_temp['scaling'] = np.tile(scalings_early['gamma'], int(outputMet_temp.shape[0] / n))
        else:
            outputMet_temp['scaling'] = np.tile(scalings['gamma'], int(outputMet_temp.shape[0] / n))

        if season in ('2010-11', '2011-12', '2012-13', '2013-14'):
            outputMet_temp = outputMet_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                      'oev_denom': oev_denom, 'lambda': lambda_val,
                                                      'scaling': np.tile(scalings_early['gamma'],
                                                                         int(outputMet_temp.shape[0] / n))})
        else:
            outputMet_temp = outputMet_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                      'oev_denom': oev_denom, 'lambda': lambda_val,
                                                      'scaling': np.tile(scalings['gamma'],
                                                                         int(outputMet_temp.shape[0] / n))})

        outputOP_temp = outputOP_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                'oev_denom': oev_denom, 'lambda': lambda_val})
        outputOPParams_temp = outputOPParams_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                            'oev_denom': oev_denom, 'lambda': lambda_val})
        outputDist_temp = outputDist_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                    'oev_denom': oev_denom, 'lambda': lambda_val})
        outputEns_temp = outputEns_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                  'oev_denom': oev_denom, 'lambda': lambda_val})

        outputMetrics = outputMetrics.append(outputMet_temp, ignore_index=True)
        outputOP = outputOP.append(outputOP_temp, ignore_index=True)
        outputOPParams = outputOPParams.append(outputOPParams_temp, ignore_index=True)
        outputDist = outputDist.append(outputDist_temp, ignore_index=True)
        outputEns = outputEns.append(outputEns_temp, ignore_index=True)

    print()

# Fix scaling value for early season FR?:
# Think I managed to do that up earlier

# Time benchmark end
print('Done.')
timestamp_end = datetime.datetime.now()
print('Time Elapsed: ' + str(timestamp_end - timestamp_start))

outputMetrics.to_csv('results/explore/outputMet_' + strain + '_' + experiment + '.csv', na_rep='NA', index=False)
outputOP.to_csv('results/explore/outputOP_' + strain + '_' + experiment + '.csv', na_rep='NA', index=False)
outputOPParams.to_csv('results/explore/outputOPParams_' + strain + '_' + experiment + '.csv', na_rep='NA', index=False)
outputDist.to_csv('results/explore/outputDist_' + strain + '_' + experiment + '.csv', na_rep='NA', index=False)
outputEns.to_csv('results/explore/outputEns_' + strain + '_' + experiment + '.csv', na_rep='NA', index=False)

print('Finished writing to file!')
