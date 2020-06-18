import os
import sys
from numba.typed import List
from EAKF_ISOLATED import *
import datetime

# print(os.path.dirname(sys.executable))

# Turn off SettingWithCopyWarning:
pd.options.mode.chained_assignment = None

# Time benchmark start:
timestamp_start = datetime.datetime.now()

# Specify subtype:
strain = 'B'

# Specify global variables
dt = 1
tmstep = 7
wk_start = 40

# Parameters for filters
discrete = False
oev_base = np.float64(1e5)  # or 5e4?
oev_denom = np.float64(10.0)
lambda_val = np.float64(1.05)  # or 1.02?
num_ens = 300
num_runs = 5  # EVENTUALLY WANT 5

# Vector of countries used
countries = np.array(['AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES'])
count_indices = [0, 1, 3, 5, 6, 7, 10, 11, 12, 13, 16, 18]
n = len(countries)

# SET POPULATION SIZE
N = np.float64(1e5)

# Read in data and set seasons:
if strain == 'A(H1)':
    seasons = ('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18')  # H1
    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H1)_SCALED.csv')

elif strain == 'A(H3)':
    seasons = ('2011-12', '2012-13', '2013-14', '2014-15', '2016-17')  # H3
    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H3)_SCALED.csv')

elif strain == 'B':
    seasons = ('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18')  # B
    iliiso = pd.read_csv('../data/by_subtype/WHO_data_B_SCALED.csv')

else:
    print('Error: Subtype not recognized.')
    sys.exit()

# Read in humidity data:
ah = pd.read_csv('../data/ah_Europe_07142019.csv')
ah = ah[ah.columns[count_indices]]
ah = ah.append(ah)
ah = ah.to_numpy(dtype=np.float64)

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
outputOP = pd.DataFrame()
outputCrossEnsVar = pd.DataFrame()
outputCorrCoefs = pd.DataFrame()
outputKalmanGain = pd.DataFrame()
outputVarRatio = pd.DataFrame()

# Loop through COUNTRIES and seasons:
for count_index in range(n):
    country = countries[count_index]
    print(country)

    # Get country-specific humidity:
    ah_count = ah[:, count_index]

    # Get country-specific DATA:
    # Can also do this while looping through seasons?

    for season_index in range(len(seasons)):
        season = seasons[season_index]
        print(season)

        # Get observations for current season AND COUNTRY:
        obs_i = iliiso.iloc[wks_dict[season], count_index + 1]

        # Get season duration:
        nsn = season_len_dict[season]

        # Reindex data:
        obs_i = obs_i.reset_index(drop=True)

        # Check that there are ANY data for this country/season:
        if any(obs_i > 0) and not obs_i.isnull().all():

            # Replace leading/lagging zeros:
            replaceLeadLag(obs_i)

            # Get OEV:
            obs_vars = calc_obsvars_ISOLATED(obs_i, oev_base, oev_denom)

            # Get time start and time range:
            tm_ini = clim_start_dict[season] - 2  # b/c python indexes at 0, while R does it at 1
            tm_range1 = [i for i in range(clim_start_dict[season] - 1, clim_end_dict[season], 1)]
            tm_range = List()
            [tm_range.append(i) for i in tm_range1]

            # Run forecasts!
            for run in range(num_runs):
                # print(run)

                # Get initial states/parameters for each ensemble member:
                param_init = pd.read_csv(os.path.join('initial_parms/', 'parms' + str(run) + '_INDIV.txt'), header=None,
                                         sep='\t')
                param_init = param_init.to_numpy(dtype=np.float64)
                # print(param_init.shape)  # (6, 300)

                # Run EAKF:
                res = EAKF_fn_fitOnly_ISOLATED(num_ens, tmstep, param_init, obs_i, nsn, nsn, obs_vars, tm_ini,
                                               tm_range, N, ah_count, dt, lambda_val, wk_start)

                # Append results to main results frames:
                outputOP_temp = res[0]
                outputCross_temp = res[1]
                outputCorr_temp = res[2]
                outputKG_temp = res[3]
                outputRat_temp = res[4]

                # continue appending:
                outputOP_temp = outputOP_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                        'oev_base': oev_base, 'oev_denom': oev_denom,
                                                        'lambda': lambda_val})
                outputCross_temp = outputCross_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                              'oev_base': oev_base, 'oev_denom': oev_denom,
                                                              'lambda': lambda_val})
                outputCorr_temp = outputCorr_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                            'oev_base': oev_base, 'oev_denom': oev_denom,
                                                            'lambda': lambda_val})
                outputKG_temp = outputKG_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                        'oev_base': oev_base, 'oev_denom': oev_denom,
                                                        'lambda': lambda_val})
                outputRat_temp = outputRat_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                          'oev_base': oev_base, 'oev_denom': oev_denom,
                                                          'lambda': lambda_val})

                outputOP = outputOP.append(outputOP_temp, ignore_index=True)
                outputCrossEnsVar = outputCrossEnsVar.append(outputCross_temp, ignore_index=True)
                outputCorrCoefs = outputCorrCoefs.append(outputCorr_temp, ignore_index=True)
                outputKalmanGain = outputKalmanGain.append(outputKG_temp, ignore_index=True)
                outputVarRatio = outputVarRatio.append(outputRat_temp, ignore_index=True)

    print()  # so we'll get a line break when a country is done, right?

# Time benchmark end
print('Done.')
timestamp_end = datetime.datetime.now()
print('Time Elapsed: ' + str(timestamp_end - timestamp_start))

outputOP.to_csv('results/outputOP_' + strain + '_fitsOnly_ISOLATED.csv', na_rep='NA', index=False)
outputCrossEnsVar.to_csv('results/outputCrossEnsVar_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputCorrCoefs.to_csv('results/outputCorrCoefs_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputKalmanGain.to_csv('results/outputKalmanGain_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputVarRatio.to_csv('results/outputVarRatio_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
print('Finished writing to file!')
