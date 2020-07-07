# Code to fit/forecast using synthetic "observations" for 12 countries #

from numba.typed import List
from run_models.EAKF_python import *
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
seasons = ('1', '2', '3', '4', '5')  # not yet sure how to handle this - have 5 "outbreaks"

# Read in humidity data:
ah = pd.read_csv('data/ah_Europe_07142019.csv')
ah = ah[ah.columns[count_indices]]
ah = ah.append(ah)
ah = ah.to_numpy(dtype=np.float64)

# Read in air travel data:
a_rand = np.zeros([12, n, n], dtype=np.float64)
for i in range(n):
    a_rand[i] = np.loadtxt('data/python_init/air_travel/aRand' + str(i + 1) + '.txt', unpack=True, dtype=np.float64)

# ### Start main loop ###
# Initiate results frames:
outputOP = pd.DataFrame()
outputOPParams = pd.DataFrame()
outputMetrics = pd.DataFrame()
outputDist = pd.DataFrame()
outputEns = pd.DataFrame()

# Loop through outbreaks and fit:
for season_index in range(len(seasons)):
    season = seasons[season_index]
    print(season)  # again, this is the synthetic outbreak, not a season

    # Get observations for current season:
    obs_i = pd.read_csv(os.path.join('src/syntheticTests/syntheticData/for_python/', 'synth_wError_' + season + '.csv'))

    # Get season duration:
    nsn = 52  # also can just be set at 52, or 43 or whatever

    # Reindex data:
    obs_i = obs_i.reset_index(drop=True)

    # Get OEV:
    obs_vars = calc_obsvars(obs_i, oev_base, oev_denom, n)

    # Get time start and time range:
    tm_ini = 273 - 2  # b/c python_init indexes at 0, while R does it at 1
    tm_range1 = [i for i in range(273 - 1, 636, 1)]
    tm_range = List()
    [tm_range.append(i) for i in tm_range1]

    # Run forecasts!
    for run in range(num_runs):
        print(run)

        # Get initial states/parameters for each ensemble member:
        param_init = pd.read_csv(os.path.join('data/python_init/initial_parms/', 'parms' + str(run) + '.txt'), header=None,
                                 sep='\t')
        param_init = param_init.to_numpy(dtype=np.float64)
        # Here we use the same as used in forecasting and fitting of observed data

        # Get season-specific population matrix:
        N = np.zeros([num_ens, n, n], dtype=np.float64)
        for ensmem in range(num_ens):
            N_temp = pd.read_csv(os.path.join('data/python_init/compartment_sizes_SYNTH/',
                                              'N_' + str(run + 1) + '_' + str(ensmem + 1) + '.txt'),
                                 header=None, sep='\t')
            N[ensmem] = N_temp.to_numpy(dtype=np.float64)
        # print(N_temp)
        del N_temp

        # Run EAKF:
        res = EAKF_fn_fitOnly(num_ens, tmstep, param_init, obs_i, nsn, nsn, obs_vars, tm_ini, tm_range, n, N, ah,
                              dt, a_rand, lambda_val, wk_start)  # and variables needed for SIRS

        outputOP_temp = res[0]
        outputOPParams_temp = res[1]

        # # OR: Forecast:
        # res = EAKF_fn(num_ens, tmstep, param_init, obs_i, 30, nsn, obs_vars, tm_ini, tm_range, n, N, ah,
        #               dt, countries, a_rand, lambda_val, wk_start)  # and variables needed for SIRS
        #
        # outputMet_temp = res[0]
        # outputOP_temp = res[1]
        # outputOPParams_temp = res[2]
        # outputDist_temp = res[3]
        # outputEns_temp = res[4]
        #
        # outputMet_temp = outputMet_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
        #                                           'oev_denom': oev_denom, 'lambda': lambda_val})
        # outputDist_temp = outputDist_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
        #                                             'oev_denom': oev_denom, 'lambda': lambda_val})
        # outputEns_temp = outputEns_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
        #                                           'oev_denom': oev_denom, 'lambda': lambda_val})
        # outputMetrics = outputMetrics.append(outputMet_temp, ignore_index=True)
        # outputDist = outputDist.append(outputDist_temp, ignore_index=True)
        # outputEns = outputEns.append(outputEns_temp, ignore_index=True)

        outputOP_temp = outputOP_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                'oev_denom': oev_denom, 'lambda': lambda_val})
        outputOPParams_temp = outputOPParams_temp.assign(**{'season': season, 'run': run, 'oev_base': oev_base,
                                                            'oev_denom': oev_denom, 'lambda': lambda_val})

        outputOP = outputOP.append(outputOP_temp, ignore_index=True)
        outputOPParams = outputOPParams.append(outputOPParams_temp, ignore_index=True)

    print()

# Time benchmark end
print('Done.')
timestamp_end = datetime.datetime.now()
print('Time Elapsed: ' + str(timestamp_end - timestamp_start))

outputOP.to_csv('src/syntheticTests/outputOP_synth_070220.csv', na_rep='NA', index=False)
outputOPParams.to_csv('src/syntheticTests/outputOPParams_synth_070220.csv', na_rep='NA', index=False)

# outputMetrics.to_csv('src/syntheticTests/outputMet_synth_fcast.csv', na_rep='NA', index=False)
# outputDist.to_csv('src/syntheticTests/outputDist_synth_fcast.csv', na_rep='NA', index=False)
# outputEns.to_csv('src/syntheticTests/outputEns_synth_fcast.csv', na_rep='NA', index=False)

print('Finished writing to file!')
