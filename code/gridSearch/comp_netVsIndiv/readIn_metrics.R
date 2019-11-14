
# Read in metrics from both network and individual models:
m <- read.csv(file = paste0(model1, list.files(path = model1, pattern = '_pro')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = '_pro')))

# Limit to needed columns:
m <- m[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m$leadonset5 <- m$fc_start - m$onset5

m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m2$leadonset5 <- m2$fc_start - m2$onset5

# Label by model type:
m$model <- 'Network'; m2$model <- 'Individual'

# # Check that observed values are the same for both files:
m.check <- merge(m, m2, by = c('season', 'run', 'oev_base', 'country', 'fc_start'))#, all = T)
# # note that there are about 20,000 more entries in network model results than in individual model results; likely where there are no data, so individual forecasts can't be run
# 
# all.equal(m.check$obs_pkwk.x, m.check$obs_pkwk.y)
# all.equal(m.check$obs_peak_int.x, m.check$obs_peak_int.y)
# all.equal(m.check$onsetObs5.x, m.check$onsetObs5.y)
# all.equal(m.check$FWeek_pkwk.x, m.check$FWeek_pkwk.y)
# all.equal(m.check$FWeek_onwk.x, m.check$FWeek_onwk.y)
#
# # Why aren't PIs equal?
# m.check1 <- m.check[round(m.check$obs_peak_int.x, 1) != round(m.check$obs_peak_int.y, 1), c(1:7, 13, 34)]
# # PIs have slight differences, but never past decimal point; original complications caused by wrong scaling values in df for FR

# Remove the "extra" rows from network model, for a fair comparison:
m.new <- m.check[, 1:36]
names(m.new)[9:36] <- names(m)[9:36]
names(m.new)[6:7] <- names(m)[4:5]
names(m.new)[8] <- 'scaling'
m <- m.new; rm(m.new, m.check)

# Remove if no onset:
m <- m[!is.na(m$onsetObs5), ]; m2 <- m2[!is.na(m2$onsetObs5), ]

# Compile two data sets:
m <- rbind(m, m2)
rm(m2)

# Make relevant columns factors:
m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)
m$model <- factor(m$model)
m$run <- factor(m$run)

# Calculate MAEs:
m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

# m$obs_1week[m$obs_1week == 0] <- 1.0
# m$obs_2week[m$obs_2week == 0] <- 1.0
# m$obs_3week[m$obs_3week == 0] <- 1.0
# m$obs_4week[m$obs_4week == 0] <- 1.0

# OR: Remove where observed are 0:
# actually don't think we want to do that, at least not yet...
# m$obs_1week[m$obs_1week == 0] <- NA
# m$obs_2week[m$obs_2week == 0] <- NA
# m$obs_3week[m$obs_3week == 0] <- NA
# m$obs_4week[m$obs_4week == 0] <- NA

m$abs_err_1wk_perc <- (abs(m$fcast_1week - m$obs_1week) / m$obs_1week) * 100
m$abs_err_2wk_perc <- (abs(m$fcast_2week - m$obs_2week) / m$obs_2week) * 100
m$abs_err_3wk_perc <- (abs(m$fcast_3week - m$obs_3week) / m$obs_3week) * 100
m$abs_err_4wk_perc <- (abs(m$fcast_4week - m$obs_4week) / m$obs_4week) * 100

m$abs_err_1wk_perc[m$abs_err_1wk_perc == Inf & !is.na(m$abs_err_1wk_perc)] <- NA
m$abs_err_2wk_perc[m$abs_err_2wk_perc == Inf & !is.na(m$abs_err_2wk_perc)] <- NA
m$abs_err_3wk_perc[m$abs_err_3wk_perc == Inf & !is.na(m$abs_err_3wk_perc)] <- NA
m$abs_err_4wk_perc[m$abs_err_4wk_perc == Inf & !is.na(m$abs_err_4wk_perc)] <- NA



