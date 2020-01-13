
# setwd('results/PROCESS/') # already done in process metrics file

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
### BINS ###

### Peak/Onset Timing ###
# Read in dist files:
d.pt <- read.csv(file = list.files(pattern = 'Dist_pt'))
d.ot <- read.csv(file = list.files(pattern = 'Dist_ot'))

# Join observed values:
m <- unique(m.store[, c(1, 8:9, 39)])

d.pt <- merge(d.pt, m, by = c('season', 'country'))
d.ot <- merge(d.ot, m, by = c('season', 'country'))

# Remove where no onset observed:
d.pt <- d.pt[!is.na(d.pt$onsetObs5), ]
d.ot <- d.ot[!is.na(d.ot$onsetObs5), ]

# Correct bins:
d.pt$bin <- d.pt$bin + 40 - 1
d.ot$bin <- d.ot$bin + 40 - 1

# Keep only results for bins THE SAME week as the observed values:
d.pt <- d.pt[d.pt$bin == d.pt$obs_pkwk, ]; d.ot <- d.ot[d.ot$bin == d.ot$onsetObs5, ]

# Calculate log scores:
d.pt$score <- log(d.pt$value); d.pt$score[d.pt$score == -Inf] <- -10
d.ot$score <- log(d.ot$value); d.ot$score[d.ot$score == -Inf] <- -10

# Determine predicted and observed lead weeks:
m.store$leadonset5 <- m.store$fc_start - m.store$onset5
m <- unique(m.store[, c(1:5, 7:8, 15, 92, 99, 110)])

d.pt <- merge(d.pt, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
d.ot <- merge(d.ot, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Combine:
d <- rbind(d.pt, d.ot)

# Reduce appropriately:
d <- d[, c(1:8, 11:12, 14:17, 13)]

# Save:
write.csv(d, file = 'logScores_pt_ot.csv', row.names = FALSE)

# Clean up:
rm(d, d.pt, d.ot, m)

### Peak Intensity ###
# Read in dist files:
d500 <- read.csv(file = list.files(pattern = 'Dist_pi500'))
d250 <- read.csv(file = list.files(pattern = 'Dist_pi250'))

# Get observed peak intensity:
m <- unique(m.store[, c(1, 6, 8, 17, 39)])
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d500 <- merge(d500, m, by = c('season', 'country'))
d250 <- merge(d250, m, by = c('season', 'country'))
d500 <- d500[!is.na(d500$onsetObs5), ]; d250 <- d250[!is.na(d250$onsetObs5), ]

# Categorize obs_peak_int by bin:
d500$obs_peak_int_bin <- cut(d500$obs_peak_int, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_peak_int_bin) <- c(seq(500, 14000, by = 500), 100000)

d250$obs_peak_int_bin <- cut(d250$obs_peak_int, c(seq(0, 14000, by = 250), 100000))
levels(d250$obs_peak_int_bin) <- c(seq(250, 14000, by = 250), 100000)

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$bin == d500$obs_peak_int_bin, ]
d250 <- d250[d250$bin == d250$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$value); d500$score[d500$score == -Inf] <- -10
d250$score <- log(d250$value); d250$score[d250$score == -Inf] <- -10

# Combine:
d <- rbind(d500, d250)
rm(d500, d250)

# Reduce appropriately:
d <- d[, c(1:8, 12:13, 15)]

# Get lead weeks:
# m.store$leadonset5 <- m.store$fc_start - m.store$onset5
m <- unique(m.store[, c(1:5, 7:9, 15, 92, 99, 110)])

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:8, 12, 9:10, 13:16, 11)]

# Save:
write.csv(d, file = 'logScores_pi_bin.csv', row.names = FALSE)
rm(d, m)

### 1-4 Weeks ###
d500 <- read.csv(file = list.files(pattern = 'Dist_1-4wk500'))
d250 <- read.csv(file = list.files(pattern = 'Dist_1-4wk250'))

m1 <- unique(m.store[, c(1:8, 25, 39)])
m2 <- unique(m.store[, c(1:8, 26, 39)])
m3 <- unique(m.store[, c(1:8, 27, 39)])
m4 <- unique(m.store[, c(1:8, 28, 39)])

# Rescale values:
m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

# Add/edit metrics markers:
levels(d500$metric) <- c('1week', '2week', '3week', '4week')
levels(d250$metric) <- c('1week', '2week', '3week', '4week')

m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

# Compile m's:
names(m1)[9] = names(m2)[9] = names(m3)[9] = names(m4)[9] = 'obs_xweek'
m <- rbind(m1, m2, m3, m4)

# Merge:
d500 <- merge(d500, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'metric'))
d250 <- merge(d250, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'metric'))

# Remove where no onset:
d500 <- d500[!is.na(d500$onsetObs5), ]
d250 <- d250[!is.na(d250$onsetObs5), ]

# Categorize obs_xweek by bin:
d500$obs_xweek_bin <- cut(d500$obs_xweek, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_xweek_bin) <- c(seq(500, 14000, by = 500), 100000)
d500$obs_xweek_bin[d500$obs_xweek == 0 & !is.na(d500$obs_xweek)] <- 500

d250$obs_xweek_bin <- cut(d250$obs_xweek, c(seq(0, 14000, by = 250), 100000))
levels(d250$obs_xweek_bin) <- c(seq(250, 14000, by = 250), 100000)
d250$obs_xweek_bin[d250$obs_xweek == 0 & !is.na(d250$obs_xweek)] <- 250
# Note that next week observations are very 0-heavy

# Remove where obs is NA:
d500 <- d500[!is.na(d500$obs_xweek), ]
d250 <- d250[!is.na(d250$obs_xweek), ]

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$bin == d500$obs_xweek_bin, ]
d250 <- d250[d250$bin == d250$obs_xweek_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$value); d500$score[d500$score == -Inf] <- -10
d250$score <- log(d250$value); d250$score[d250$score == -Inf] <- -10

# Combine:
d500$metric2 <- 'bin500'; d250$metric2 <- 'bin250'
d <- rbind(d500, d250)
rm(d500, d250)

# Reduce appropriately:
d <- d[, c(1:8, 12:13, 15:16)]

# Get lead weeks:
m <- unique(m.store[, c(1:5, 7:9, 15, 92, 99, 110)])

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:8, 12, 9:10, 13:17, 11)]

# Save:
write.csv(d, file = 'logScores_1-4wks_bin.csv', row.names = FALSE)
rm(d, m, m1, m2, m3, m4)


