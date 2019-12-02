
# Read in metrics:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')

hist(m$oev_scaled)
quantile(m$oev_scaled) # 75: 649379.6; 100: 1.4e7 (14 million)
quantile(m$oev_scaled, probs = c(0.80, 0.85, 0.90, 0.95))
# 85 is still 950,000 - under 1e6; 90 is 1,181800; 95 is 2,477,609

# check what they look like for relevant lead weeks

m.obs <- m[m$FWeek_pkwk < 5 & m$FWeek_pkwk > 0, ]
m <- m[m$leadpkwk_mean < 5 & m$leadpkwk_mean > 0, ]

hist(m$oev_scaled)
quantile(m$oev_scaled, probs = c(0.8, 0.85, 0.9, 0.95)) # 95th around 1e6 for both, 90th around 858,000
quantile(m.obs$oev_scaled, probs = c(0.8, 0.85, 0.9, 0.95))

m[m$oev_scaled > 2e6, ] # 16; SK only
m[m$oev_scaled > 1.5e6, ] # 18; only 37; SK, LU
m[m$oev_scaled > 1e6, ]$country # 77; CZ, SK, LU, BE
# weirdly DE doesn't fall into these - but DE's trouble might be more the ens_var:obs_var ratio
# these high values are pretty restricted to certain countries
# but still, just restricting these values doesn't seem like it would actually improve forecasts for countries like DE where there is a second uptick in activity

m.obs[m.obs$oev > 1e6, 'country'] # similar, but >100; CZ, SK, DE - so maybe could help

m[m$oev_scaled > 8e5, 'country']
# 1e6 seems like it might be a good cutoff to try

m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
hist(m$oev_scaled)
abline(v = 1e6)

m.lim <- m[m$oev_scaled > 1e6, ] # AT, BE, CZ, DE, HU, LU, PL, SK (all countries except ES, FR, IT, NL affected)
table(m.lim$country[m.lim$oev_scaled > 1.5e6]) # AT, CZ, DE, HU, LU, PL, SK (removes BE)
table(m.lim$country[m.lim$oev_scaled > 2e6]) # AT, DE, HU, LU, PL, SK (removes CZ) - but this seems too extreme

m.lim <- m.lim[m.lim$leadpkwk_mean >= -8 & m.lim$leadpkwk_mean < 5, ]
table(m.lim$country) # still all the same countries
table(m.lim$country, m.lim$season) # all seasons: LU and SK only
table(m.lim$leadpkwk_mean) # most of these actually occur before peak

m.lim <- m.lim[m.lim$leadpkwk_mean > 0, ]
table(m.lim$country) # BE, CZ, HU, LU, SK

# 1e6 has some potential, but seems like it might be particularly low for early outbreak; higher (1.5e6) might not necessarily catch later outbreak though

########################################################################################################################################################################
# Get scaled OEV values:

# # Read in metrics:
# m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
# 
# # List countries:
# countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
# n <- length(countries)
# 
# # Get observations:
# iliiso <- read.csv('data/WHO_data_05-09-19.csv')
# iliiso <- iliiso[, c(1, count.indices + 1)]
# 
# test.dat <- read.csv('data/testCounts_052719.csv')
# syn.dat <- read.csv('data/synDatCounts_060519.csv')
# pos.dat <- read.csv('data/posProp_060519.csv')
# 
# test.dat <- test.dat[, c(1, count.indices + 1)]
# syn.dat <- syn.dat[, c(1, count.indices + 1)]
# pos.dat <- pos.dat[, c(1, count.indices + 1)]
# 
# # Scale; replace -1 with NA:
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[count.indices, ]
# for (i in 2:13) {
#   if (names(iliiso)[i] == 'France') {
#     iliiso[1:286, i] <- iliiso[1:286, i] * 1.3
#     iliiso[287:495, i] <- iliiso[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#     syn.dat[1:286, i] <- syn.dat[1:286, i] * 1.3
#     syn.dat[287:495, i] <- syn.dat[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   } else {
#     iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#     syn.dat[, i] <- syn.dat[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   }
# 
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
#   syn.dat[, i][syn.dat[, i] < 0] <- NA
#   pos.dat[, i][pos.dat[, i] < 0] <- NA
#   test.dat[, i][test.dat[, i] < 0] <- NA
# }
# 
# # # And rename the columns:
# # names(iliiso) = names(syn.dat) = names(pos.dat) = names(test.dat)
# 
# # Load required functions:
# source('cluster/functions/Fn_initializations.R')
# source('cluster/functions/replaceLeadingLaggingNAs.R')
# source('cluster/functions/calc_obsvars.R')
# 
# # Get mini-dataset that can be matched with larger ones later:
# m.mini <- unique(m[, c(1, 7:8)])
# m.mini$oev = m.mini$obs <- NA
# 
# # Loop through seasons to get matching observations:
# seasons <- levels(m$season)
# 
# for (season in seasons) {
#   tmp <- Fn_dates(season)
#   weeks <- tmp$weeks + 3
#   nsn <- tmp$nsn
# 
#   obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
#   syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
#   test_i <- test.dat[weeks, (1:length(countries) + 1)]
#   pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
# 
#   # replace leading/lagging NAs:
#   for (count.index in 1:n) {
#     obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
#     syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
#     pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
#   }
# 
#   # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
#   test_i[test_i == 0 & !is.na(test_i)] <- NA
# 
#   # Variance of syndromic+ data:
#   oev_base <- 1e4; oev_denom <- 10
#   obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
#                                  oev_base, oev_denom, tmp_exp = 2.0)
# 
#   # Match obs_i/obs_vars to appropriate season/country/fc_start:
#   for (count.index in 1:n) {
#     for (fc in unique(m.mini$fc_start)) {
#       m.mini$obs[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_i[fc - 39, count.index]
#       m.mini$oev[m.mini$season == season & m.mini$country == countries[count.index] & m.mini$fc_start == fc] <- obs_vars[fc - 39, count.index]
# 
#     }
#   }
# 
# }
# 
# # Remove from m.mini where obs is NA, or oev NA or 0:
# m.mini <- m.mini[!is.na(m.mini$obs) & !is.na(m.mini$oev) & m.mini$oev != 0, ]
# 
# # Merge m.mini with all important data frames:
# m <- merge(m, m.mini, by = c('season', 'country', 'fc_start')) # 9304
# names(m)[78:81] <- c('obs', 'oev', 'obs_scaled', 'oev_scaled')
# 
# # Save new results files:
# write.csv(m, file = 'results/original/fairComp/outputMet_110819_pro_PROC.csv', row.names = FALSE)
