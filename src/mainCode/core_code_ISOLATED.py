# Code to run through all countries, seasons, and (sub)types and forecast using isolated models #

import os
import sys
import datetime
from numba.typed import List

from run_models.EAKF_ISOLATED import *

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
    iliiso = pd.read_csv('data/WHO_data_A(H1)_SCALED.csv')

elif strain == 'A(H3)':
    seasons = ('2011-12', '2012-13', '2013-14', '2014-15', '2016-17')  # H3
    iliiso = pd.read_csv('data/WHO_data_A(H3)_SCALED.csv')

elif strain == 'B':
    seasons = ('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18')  # B
    iliiso = pd.read_csv('data/WHO_data_B_SCALED.csv')

else:
    print('Error: Subtype not recognized.')
    sys.exit()

# Read in humidity data:
ah = pd.read_csv('data/ah_Europe_07142019.csv')
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
outputMetrics = pd.DataFrame()
outputOP = pd.DataFrame()
outputDist = pd.DataFrame()
outputEns = pd.DataFrame()

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

        # Get scaling:
        scalings = pd.read_csv('data/scaling_frames/scalings_frame_' + strain + '_' + season + '.csv')
        gamma = scalings['gamma'][count_index]

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
            tm_ini = clim_start_dict[season] - 2  # b/c python_init indexes at 0, while R does it at 1
            tm_range1 = [i for i in range(clim_start_dict[season] - 1, clim_end_dict[season], 1)]
            tm_range = List()
            [tm_range.append(i) for i in tm_range1]

            # Run forecasts!
            for run in range(num_runs):
                # print(run)

                # Get initial states/parameters for each ensemble member:
                param_init = pd.read_csv(os.path.join('data/python_init/initial_parms/', 'parms' + str(run) + '_INDIV.txt'),
                                         header=None, sep='\t')
                param_init = param_init.to_numpy(dtype=np.float64)
                # print(param_init.shape)  # (6, 300)

                # Run EAKF:
                res = EAKF_fn_ISOLATED(num_ens, tmstep, param_init, obs_i, 30, nsn, obs_vars, tm_ini, tm_range, N,
                                       ah_count, dt, lambda_val, wk_start)

                # Append results to main results frames:
                outputMet_temp = res[0]
                outputOP_temp = res[1]
                outputDist_temp = res[2]
                outputEns_temp = res[3]

                outputMet_temp['country'] = country
                outputMet_temp['season'] = season
                outputMet_temp['run'] = run
                outputMet_temp['oev_base'] = oev_base
                outputMet_temp['oev_denom'] = oev_denom
                outputMet_temp['lambda'] = lambda_val
                outputMet_temp['scaling'] = gamma

                # # deal with scalings:
                # outputMet_temp['scaling'] = scalings['gamma'][count_index]
                # if season in ('2010-11', '2011-12', '2012-13', '2013-14') and country == 'FR':
                #     print('Used')
                #     outputMet_temp['scaling'] = scalings_early['gamma'][count_index]

                # continue appending:
                outputOP_temp = outputOP_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                        'oev_base': oev_base, 'oev_denom': oev_denom,
                                                        'lambda': lambda_val})
                outputDist_temp = outputDist_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                            'oev_base': oev_base, 'oev_denom': oev_denom,
                                                            'lambda': lambda_val})
                outputEns_temp = outputEns_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                          'oev_base': oev_base, 'oev_denom': oev_denom,
                                                          'lambda': lambda_val})

                outputMetrics = outputMetrics.append(outputMet_temp, ignore_index=True)
                outputOP = outputOP.append(outputOP_temp, ignore_index=True)
                outputDist = outputDist.append(outputDist_temp, ignore_index=True)
                outputEns = outputEns.append(outputEns_temp, ignore_index=True)

    print()  # so we'll get a line break when a country is done, right?

# Time benchmark end
print('Done.')
timestamp_end = datetime.datetime.now()
print('Time Elapsed: ' + str(timestamp_end - timestamp_start))

outputMetrics.to_csv('results/temp/outputMet_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputOP.to_csv('results/temp/outputOP_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputDist.to_csv('results/temp/outputDist_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
outputEns.to_csv('results/temp/outputEns_' + strain + '_ISOLATED.csv', na_rep='NA', index=False)
print('Finished writing to file!')
