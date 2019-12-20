import os
import sys

# Read in all model functions:
from EAKF_python import *

# print(os.path.dirname(sys.executable))

# Read in all necessary data/functions and run forecasts using network model
# It seems this is done automatically?

# Turn off SettingWithCopyWarning:
pd.options.mode.chained_assignment = None

# Specify subtype:
strain = 'A(H1)'

# Specify global variables
dt = 1
tmstep = 7
wk_start = 40

'''
# Parameter bounds
theta_low = (365.0, 2.0, 2.0, 0.2, 0.75)
theta_up = (8 * 365.0, 7.0, 2.8, 1.0, 1.25)
'''

S0_low = 0.3
S0_up = 0.9
# sd_low = 0.05
# sd_up = 0.18
I0_low = 0
I0_up = 0.00005

# Parameters for filters
discrete = False
oev_base = 0.3
oev_denom = 1.0
lambda_val = 1.02
num_ens = 300
num_runs = 1  # EVENTUALLY WANT 5

# Vector of countries used
countries = ('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# count_indices = (1, 2, 4, 6, 7, 8, 11, 12, 13, 14, 17, 19)
count_indices = [0, 1, 3, 5, 6, 7, 10, 11, 12, 13, 16, 18]
n = len(countries)

# Read in data and set seasons:
if strain == 'all':
    seasons = ('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
    # DATA #
elif strain == 'A(all)':
    seasons = ('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
    # DATA #
elif strain == 'A(H1)':
    seasons = ('2010-11', '2012-13')  # , '2013-14', '2014-15', '2015-16', '2017-18') # H1

    iliiso = pd.read_csv('../data/by_subtype/WHO_data_A(H1)_SCALED.csv')
    test_dat = pd.read_csv('../data/testCounts_052719.csv', na_values=-1)
    syn_dat = pd.read_csv('../data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
    pos_dat = pd.read_csv('../data/by_subtype/posprop_A(H1).csv', na_values=-1)

    test_dat = test_dat[iliiso.columns]
    pos_dat = pos_dat[iliiso.columns]

    scalings = pd.read_csv('../data/by_subtype/scalings_frame_A(H1).csv')

    print(iliiso.columns)
    print(test_dat.columns)

elif strain == 'A(H3)':
    seasons = ('2011-12', '2012-13', '2013-14', '2014-15', '2016-17')  # H3
    # DATA #
elif strain == 'B':
    seasons = ('2010-11', '2011-12', '2012-13', '2014-15', '2015-16', '2017-18')  # B
    # DATA #
else:
    print('Error: Subtype not recognized.')
    sys.exit()

# print(seasons)

# Read in humidity data:
ah = pd.read_csv('../../GLDAS_data/ah_Europe_07142019.csv')
ah = ah[ah.columns[count_indices]]
ah = ah.append(ah)
ah = ah.to_numpy()

# Read in air travel data:
a_rand = np.empty([n, n, 12])
for i in range(n):
    a_rand[:, :, i] = np.loadtxt('air_travel/aRand' + str(i + 1) + '.txt', unpack=True)

a_rand_comp = {'Jan': np.loadtxt('air_travel/aRand1.txt', unpack=True),
               'Feb': np.loadtxt('air_travel/aRand2.txt', unpack=True),
               'Mar': np.loadtxt('air_travel/aRand3.txt', unpack=True),
               'Apr': np.loadtxt('air_travel/aRand4.txt', unpack=True),
               'May': np.loadtxt('air_travel/aRand5.txt', unpack=True),
               'Jun': np.loadtxt('air_travel/aRand6.txt', unpack=True),
               'Jul': np.loadtxt('air_travel/aRand7.txt', unpack=True),
               'Aug': np.loadtxt('air_travel/aRand8.txt', unpack=True),
               'Sep': np.loadtxt('air_travel/aRand9.txt', unpack=True),
               'Oct': np.loadtxt('air_travel/aRand10.txt', unpack=True),
               'Nov': np.loadtxt('air_travel/aRand11.txt', unpack=True),
               'Dec': np.loadtxt('air_travel/aRand12.txt', unpack=True)}

# print(a_rand['Aug'])

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

# Loop through seasons and forecast:
for season_index in range(len(seasons)):
    season = seasons[season_index]
    print(season)

    # Get season-specific population matrix:
    N = pd.read_csv(os.path.join('compartment_sizes/', 'N' + season + '.txt'), header=None, sep='\t')
    N = N.to_numpy()

    # Get observations for current season:
    obs_i = iliiso.iloc[wks_dict[season]]
    syn_i = syn_dat.iloc[wks_dict[season]]
    pos_i = pos_dat.iloc[wks_dict[season]]
    test_i = test_dat.iloc[wks_dict[season]]

    # Get season duration:
    nsn = season_len_dict[season]

    # Reindex data:
    obs_i = obs_i.reset_index(drop=True)

    # Replace leading/lagging zeros:
    for count_index in range(n):
        # print(count_index)
        # obs_i.iloc[:, count_index + 1]
        replaceLeadLag(obs_i.iloc[:, count_index + 1])

    # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
    # print(test_i)
    for count_index in range(n):
        if any(test_i.iloc[:, count_index + 1] == 0):
            print('0s found in test data!')

    # Get OEV:
    obs_vars = calc_obsvars_nTest(obs_i, syn_i, test_i, pos_i, oev_base, oev_denom, 2.0, n)

    # DO WE NEED DATES?

    # Get time start and time range:
    tm_ini = clim_start_dict[season] - 2  # b/c python indexes at 0, while R does it at 1
    # tm_range = range(clim_start_dict[season], clim_end_dict[season] + 1, 1)
    tm_range = range(clim_start_dict[season] - 1, clim_end_dict[season], 1)  # see previous note
    tm_range = list(tm_range)
    # print(tm_range)

    # Run forecasts!
    for run in range(num_runs):
        print(run)

        # Get initial states/parameters for each ensemble member:
        param_init = pd.read_csv(os.path.join('initial_parms/', 'parms' + str(run) + '.txt'), header=None, sep='\t')
        param_init = param_init.to_numpy()
        # print(param_init.shape)

        # Run EAKF:
        res = EAKF_fn(num_ens, tmstep, param_init, obs_i, 6, nsn, obs_vars, tm_ini, tm_range, n, N, ah,
                      dt, countries, a_rand, lambda_val, wk_start)  # and variables needed for SIRS
        # loop through ntrns or run all as we go?
        # for now, set ntrn to 30, which is our final forecast week

        # res = pd.Panel(res)
        # print(res.shape)
        # np.savetxt(os.path.join('results/', 'xprior_ens_' + str(run) + '_' + season + '_NEW.txt'),
        #            res, delimiter = ',')

    print()

'''
# Specify variables for running model
tm_strt = 273 # early September
tm_end = 573
tm_step = 1
t = 1
tm_range = []
for i in range(tm_strt, tm_end + 1):
    tm_range.append(i)
'''

# Save results
# np.savetxt('results/res.txt', res, delimiter = ',')
# np.savetxt('results/resAirOnly.txt', resA, delimiter = ',')
# np.savetxt('results/resAll_1.txt', resAll, delimiter = ',')
