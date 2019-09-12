
# Read in files from all models:
m1 <- read.csv('results/original/outputMet_090119_pro.csv')
m2 <- read.csv('results/newScalings/outputMet_090919_pro.csv')
m3 <- read.csv('results/propRandTravel/outputMet_090919_pro.csv')
m4 <- read.csv('results/indivCountries/outputMet_082819_pro.csv')

# Limit to needed columns:
m1 <- m1[, c(1:3, 6:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 92:95, 97, 99:101)]
m2 <- m2[, c(1:3, 6:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 92:95, 97, 99:101)]
m3 <- m3[, c(1:3, 6:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 92:95, 97, 99:101)]
m4 <- m4[, c(2:3, 5, 4, 8, 1, 9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]

# Label by model type:
m1$model <- 'Original'; m2$model <- 'New Scale'; m3$model <- 'Random Travel'; m4$model <- 'Individual'

# # Check that observed values are the same for ALL files:
# m.check <- merge(m1, m2, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
# all.equal(m.check$obs_pkwk.x, m.check$obs_pkwk.y)
# all.equal(m.check$obs_peak_int.x, m.check$obs_peak_int.y)
# all.equal(m.check$onsetObs5.x, m.check$onsetObs5.y)
# all.equal(m.check$FWeek_pkwk.x, m.check$FWeek_pkwk.y)
# all.equal(m.check$FWeek_onwk.x, m.check$FWeek_onwk.y)
# 
# m.check <- merge(m1, m3, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
# all.equal(m.check$obs_pkwk.x, m.check$obs_pkwk.y)
# all.equal(m.check$obs_peak_int.x, m.check$obs_peak_int.y)
# all.equal(m.check$onsetObs5.x, m.check$onsetObs5.y)
# all.equal(m.check$FWeek_pkwk.x, m.check$FWeek_pkwk.y)
# all.equal(m.check$FWeek_onwk.x, m.check$FWeek_onwk.y)
# 
# m.check <- merge(m1, m4, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
# all.equal(m.check$obs_pkwk.x, m.check$obs_pkwk.y)
# all.equal(m.check$obs_peak_int.x, m.check$obs_peak_int.y)
# all.equal(m.check$onsetObs5.x, m.check$onsetObs5.y)
# all.equal(m.check$FWeek_pkwk.x, m.check$FWeek_pkwk.y)
# all.equal(m.check$FWeek_onwk.x, m.check$FWeek_onwk.y)
# # only difference is between network obs_peak_int and individual values, but these are small and seem to be due to rounding
# # and that there are different countries with no observed onset, depending on the scaling values

# Remove the "extra" rows from network models, for a fair comparison:
m.check <- merge(m1, m4, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
m.new <- m.check[, 1:33]
names(m.new)[7:33] <- names(m1)[7:33]
names(m.new)[6] <- 'scaling'
m1 <- m.new; rm(m.new, m.check)

m.check <- merge(m2, m4, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
m.new <- m.check[, 1:33]
names(m.new)[7:33] <- names(m2)[7:33]
names(m.new)[6] <- 'scaling'
m2 <- m.new; rm(m.new, m.check)

m.check <- merge(m3, m4, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))
m.new <- m.check[, 1:33]
names(m.new)[7:33] <- names(m3)[7:33]
names(m.new)[6] <- 'scaling'
m3 <- m.new; rm(m.new, m.check)

# Remove where no observed onset:
m1 <- m1[!is.na(m1$onsetObs5), ]
m2 <- m2[!is.na(m2$onsetObs5), ]
m3 <- m3[!is.na(m3$onsetObs5), ]
m4 <- m4[!is.na(m4$onsetObs5), ]
# note: now there are still outbreaks included in m2 that aren't in the others, and maybe also vice versa, because scalings are different
    # let's keep for now, but can remove later to check

# Compile all data sets:
m <- rbind(m1, m2, m3, m4)
rm(m1, m2, m3, m4)

# Make relevant columns factors:
m$oev_base <- factor(m$oev_base)
m$model <- factor(m$model)
m$run <- factor(m$run)

# And relevel models:
m$model <- factor(m$model, levels = levels(m$model)[c(3, 2, 4, 1)])

# Calculate lead onsets (predicted):
m$leadonset5 <- m$fc_start - m$onset5

# Calculate MAEs:
m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

# OR: Remove where observed are 0:
m$obs_1week[m$obs_1week == 0] <- NA
m$obs_2week[m$obs_2week == 0] <- NA
m$obs_3week[m$obs_3week == 0] <- NA
m$obs_4week[m$obs_4week == 0] <- NA

m$abs_err_1wk_perc <- (abs(m$fcast_1week - m$obs_1week) / m$obs_1week) * 100
m$abs_err_2wk_perc <- (abs(m$fcast_2week - m$obs_2week) / m$obs_2week) * 100
m$abs_err_3wk_perc <- (abs(m$fcast_3week - m$obs_3week) / m$obs_3week) * 100
m$abs_err_4wk_perc <- (abs(m$fcast_4week - m$obs_4week) / m$obs_4week) * 100

