
# List of patterns to read in:
patterns <- c('pi_alt1_1000', 'pi_alt2_1000', 'pi_alt2_500', 'pi_alt2_250',
              '1-4wk_alt1_1000', '1-4wk_alt2_1000', '1-4wk_alt2_500', '1-4wk_alt2_250')

# for PI bins: season, country, run, oev_base, fc_start, onsetObs5, obs_peak_int_bin, score...

# Read in log scores from all models:
d <- read.csv(paste0(model1, list.files(path = model1, pattern = '_pt_ot')))
d2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_pt_ot')))
d3 <- read.csv(paste0(model3, list.files(path = model3, pattern = '_pt_ot')))
d4 <- read.csv(paste0(model4, list.files(path = model4, pattern = '_pt_ot')))
d5 <- read.csv(paste0(model5, list.files(path = model5, pattern = '_pt_ot')))

# Add factor describing model:
d$model <- 'Network'; d2$model <- 'Indiv. (R0diff/newOEV)'; d3$model <- 'Indiv. (R0diff/oldOEV)'; d4$model <- 'Indiv. (R0min/newOEV)'; d5$model <- 'Indiv. (R0min/oldOEV)'

e.list = e.list2 = e.list3 = e.list4 = e.list5 = vector('list', length(patterns))
for (i in 1:length(patterns)) {
  e.temp <- read.csv(paste0(model1, list.files(path = model1, pattern = patterns[i])))
  e.temp$model <- 'Network'
  e.list[[i]] <- e.temp
  
  e.temp <- read.csv(paste0(model2, list.files(path = model2, pattern = patterns[i])))
  e.temp$model <- 'Indiv. (R0diff/newOEV)'
  e.list2[[i]] <- e.temp
  
  e.temp <- read.csv(paste0(model3, list.files(path = model3, pattern = patterns[i])))
  e.temp$model <- 'Indiv. (R0diff/oldOEV)'
  e.list3[[i]] <- e.temp
  
  e.temp <- read.csv(paste0(model4, list.files(path = model4, pattern = patterns[i])))
  e.temp$model <- 'Indiv. (R0min/newOEV)'
  e.list4[[i]] <- e.temp
  
  e.temp <- read.csv(paste0(model5, list.files(path = model5, pattern = patterns[i])))
  e.temp$model <- 'Indiv. (R0min/oldOEV)'
  e.list5[[i]] <- e.temp
}

# Remove if no onset:
# Already done, except for 1-4 weeks, where might not be necessary?

# Reorder columns as needed:
d2 <- d2[, c(1:5, 8:16)]
d3 <- d3[, c(1:5, 8:16)]
d4 <- d4[, c(1:5, 8:16)]
d5 <- d5[, c(1:5, 8:16)]

e.list[[1]] <- e.list[[1]][, c(1:5, 7, 9, 12:17)]
e.list2[[1]] <- e.list2[[1]][, c(1:5, 8, 10:16)]
e.list3[[1]] <- e.list3[[1]][, c(1:5, 8, 10:16)]
e.list4[[1]] <- e.list4[[1]][, c(1:5, 8, 10:16)]
e.list5[[1]] <- e.list5[[1]][, c(1:5, 8, 10:16)]

for (i in 2:4) {
  e.list[[i]] <- e.list[[i]][, c(1:8, 11:16)]
  e.list2[[i]] <- e.list2[[i]][, c(1:5, 9:11, 14:19)]
  e.list3[[i]] <- e.list3[[i]][, c(1:5, 9:11, 14:19)]
  e.list4[[i]] <- e.list4[[i]][, c(1:5, 9:11, 14:19)]
  e.list5[[i]] <- e.list5[[i]][, c(1:5, 9:11, 14:19)]
}

e.list[[5]] <- e.list[[5]][, c(1:9, 12:17)]
e.list2[[5]] <- e.list2[[5]][, c(1:5, 9:18)]
e.list3[[5]] <- e.list3[[5]][, c(1:5, 9:18)]
e.list4[[5]] <- e.list4[[5]][, c(1:5, 9:18)]
e.list5[[5]] <- e.list5[[5]][, c(1:5, 9:18)]

for (i in 6:8) {
  e.list[[i]] <- e.list[[i]][, c(1:7, 10:15)]
  names(e.list[[i]])[6] <- 'metrics'
  e.list2[[i]] <- e.list2[[i]][, c(1:6, 9, 12:17)]
  e.list3[[i]] <- e.list3[[i]][, c(1:6, 9, 12:17)]
  e.list4[[i]] <- e.list4[[i]][, c(1:6, 9, 12:17)]
  e.list5[[i]] <- e.list5[[i]][, c(1:6, 9, 12:17)]
}

# Compile:
d <- rbind(d, d2, d3, d4, d5)
rm(d2, d3, d4, d5)

e.pi.list = e.wks.list = vector('list', length(patterns) / 2)
for (i in 1:length(e.pi.list)) {
  e.pi.list[[i]] <- rbind(e.list[[i]], e.list2[[i]], e.list3[[i]], e.list4[[i]], e.list5[[i]])
  e.wks.list[[i]] <- rbind(e.list[[i + length(patterns) / 2]], e.list2[[i + length(patterns) / 2]], e.list3[[i + length(patterns) / 2]],
                           e.list4[[i + length(patterns) / 2]], e.list5[[i + length(patterns) / 2]])
}
rm(e.list, e.list2, e.list3, e.list4, e.list5, e.temp)

# For now, remove oev_denom 1e5:
d <- d[d$oev_base == 1e4, ]
for (i in 1:length(e.pi.list)) {
  e.pi.list[[i]] <- e.pi.list[[i]][e.pi.list[[i]]$oev_base == 1e4, ]
  e.wks.list[[i]] <- e.wks.list[[i]][e.wks.list[[i]]$oev_base == 1e4, ]
}


