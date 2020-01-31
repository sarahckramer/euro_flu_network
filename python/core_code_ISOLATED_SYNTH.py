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
num_runs = 5  # EVENTUALLY WANT 5

# Vector of countries used
countries = np.array(['AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES'])
count_indices = [0, 1, 3, 5, 6, 7, 10, 11, 12, 13, 16, 18]
n = len(countries)

# Read in data and set outbreaks:
iliiso = pd.read_csv('../syntheticTests/syntheticData/for_python/synth_wError_ISOLATED.csv')
seasons = ('1', '2', '3')  # not yet sure how to handle this - have 3 "outbreaks"

# SET POPULATION SIZE
N = np.float64(1e5)

# Read in humidity data:
ah = pd.read_csv('../data/ah_Europe_07142019.csv')
ah = ah[ah.columns[count_indices]]
ah = ah.append(ah)
ah = ah.to_numpy(dtype=np.float64)

# ### Start main loop ###
# Initiate results frames:
outputOP = pd.DataFrame()

# Loop through COUNTRIES and seasons:
count_index = 3  # should be FR
country = countries[count_index]
print(country)

# Get country-specific humidity:
ah_count = ah[:, count_index]

# Loop through outbreaks:
for season_index in range(len(seasons)):
    season = seasons[season_index]
    print(season)

    # Get "observations":
    obs_i = iliiso.iloc[:, season_index]
    print(obs_i)

    # Get season duration:
    nsn = 52

    # Reindex data:
    obs_i = obs_i.reset_index(drop=True)

    # Get OEV:
    obs_vars = calc_obsvars_ISOLATED(obs_i, oev_base, oev_denom)

    # Get time start and time range:
    tm_ini = 273 - 2  # b/c python indexes at 0, while R does it at 1
    tm_range1 = [i for i in range(273 - 1, 636, 1)]
    tm_range = List()
    [tm_range.append(i) for i in tm_range1]

    # Run forecasts!
    for run in range(num_runs):
        # print(run)

        # Get initial states/parameters for each ensemble member:
        param_init = pd.read_csv(os.path.join('initial_parms/', 'parms' + str(run) + '_INDIV.txt'), header=None,
                                 sep='\t')
        param_init = param_init.to_numpy(dtype=np.float64)

        # Run EAKF:
        res = EAKF_fn_fitOnly_ISOLATED(num_ens, tmstep, param_init, obs_i, nsn, nsn, obs_vars, tm_ini, tm_range, N,
                                       ah_count, dt, lambda_val, wk_start)

        # Append results to main results frames:
        outputOP_temp = res
        outputOP_temp = outputOP_temp.assign(**{'country': country, 'season': season, 'run': run,
                                                'oev_base': oev_base, 'oev_denom': oev_denom,
                                                'lambda': lambda_val})
        outputOP = outputOP.append(outputOP_temp, ignore_index=True)

# Time benchmark end
print('Done.')
timestamp_end = datetime.datetime.now()
print('Time Elapsed: ' + str(timestamp_end - timestamp_start))

# outputOP.to_csv('results/outputOP_ISOLATED_SYNTH.csv', na_rep='NA', index=False)
print('Finished writing to file!')
