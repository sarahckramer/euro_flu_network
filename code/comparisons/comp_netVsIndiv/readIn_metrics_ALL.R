
# Read in metrics from both network and individual models:
m <- read.csv(file = paste0(model1, list.files(path = model1, pattern = '_pro')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = '_pro')))
m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = '_pro')))
m4 <- read.csv(file = paste0(model4, list.files(path = model4, pattern = '_pro')))
m5 <- read.csv(file = paste0(model5, list.files(path = model5, pattern = '_pro')))
# first thing to notice: seem to be way more results for when OEVnew vs. OEVold is used - looks like I removed oev_base 1e5 already from these?
    # just stick with this for now - oev_base 1e4 is better for all models anyway
# why so many fewer results from network model? - 2 runs rather than 3

# Limit to needed columns:
m <- m[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m$leadonset5 <- m$fc_start - m$onset5

m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m2$leadonset5 <- m2$fc_start - m2$onset5

m3 <- m3[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m3$leadonset5 <- m3$fc_start - m3$onset5

m4 <- m4[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m4$leadonset5 <- m4$fc_start - m4$onset5

m5 <- m5[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69)]
m5$leadonset5 <- m5$fc_start - m5$onset5

# Label by model type:
m$model <- 'Network'; m2$model <- 'Indiv. (R0diff/newOEV)'; m3$model <- 'Indiv. (R0diff/oldOEV)'; m4$model <- 'Indiv. (R0min/newOEV)'; m5$model <- 'Indiv. (R0min/oldOEV)'

# Remove if no onset:
m <- m[!is.na(m$onsetObs5), ]; m2 <- m2[!is.na(m2$onsetObs5), ]; m3 <- m3[!is.na(m3$onsetObs5), ]; m4 <- m4[!is.na(m4$onsetObs5), ]; m5 <- m5[!is.na(m5$onsetObs5), ]

# Compile two data sets:
m <- rbind(m, m2, m3, m4, m5)
rm(m2, m3, m4, m5)

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

# # Relevel models:
# m$model <- factor(m$model, levels = levels(m$model)[c(5, 1:4)])

# Remove all w/ oev_base 1e5:
m <- m[m$oev_base == 1e4, ]; m$oev_base <- factor(m$oev_base)
