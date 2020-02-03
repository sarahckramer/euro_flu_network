
# setwd('results/PROCESS/') # already done in process metrics file

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
### BINS ###

### Peak/Onset Timing ###
# Read in dist files:
d.full <- read.csv(file = list.files(pattern = 'Dist'))

d.pt <- d.full[d.full$metric == 'pw', ]
d.ot <- d.full[d.full$metric == 'onset5', ]

# Join observed values:
if (model.type == 'Network') {
  m <- unique(m.store[, c(2:3, 24, 32)]) # want: country, season, obs_pkwk, onsetObs5
} else if (model.type == 'Individual') {
  m <- unique(m.store[, c(2, 23, 31:32)])
}

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
if (model.type == 'Network') {
  m <- unique(m.store[, c(1:2, 6, 32:36, 38, 45, 56)]) # want: season, country, run, oev_base, oev_denom, lambda, fc_start, all 4 leads
} else if (model.type == 'Individual') {
  m <- unique(m.store[, c(1, 5, 31:36, 38, 45, 56)])
}

d.pt <- merge(d.pt, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
d.ot <- merge(d.ot, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Combine:
d <- rbind(d.pt, d.ot)

# Reduce appropriately:
d <- d[, c(1:2, 7, 3:6, 8, 11:12, 14:17, 13)]

# Save:
write.csv(d, file = 'logScores_pt_ot.csv', row.names = FALSE)

# Clean up:
rm(d, d.pt, d.ot, m)

### Peak Intensity ###
# Get PI:
d500 <- d.full[d.full$metric == 'pi', ]

# Get observed peak intensity:
if (model.type == 'Network') {
  m <- unique(m.store[, c(2, 8, 24, 32, 37)])
} else if (model.type == 'Individual') {
  m <- unique(m.store[, c(7, 23, 31:32, 37)])
}
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d500 <- merge(d500, m, by = c('season', 'country'))
d500 <- d500[!is.na(d500$onsetObs5), ]

# Categorize obs_peak_int by bin:
d500$obs_peak_int_bin <- cut(d500$obs_peak_int, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_peak_int_bin) <- c(seq(500, 14000, by = 500), 100000)

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$bin == d500$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$value); d500$score[d500$score == -Inf] <- -10
d <- d500
rm(d500)

# Get lead weeks:
if (model.type == 'Network') {
  m <- unique(m.store[, c(1:2, 6, 32:36, 38, 45, 56)]) # want: season, country, run, oev_base, oev_denom, lambda, fc_start, all 4 leads
} else if (model.type == 'Individual') {
  m <- unique(m.store[, c(1, 5, 31:36, 38, 45, 56)])
}

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:2, 4, 3, 5:8, 11:12, 16:19, 15)]

# Save:
write.csv(d, file = 'logScores_pi_bin.csv', row.names = FALSE)
rm(d, m)

### 1-4 Weeks ###
d500 <- d.full[d.full$metric %in% c('nextweek1', 'nextweek2', 'nextweek3', 'nextweek4'), ]
d500$metric <- factor(d500$metric)

if (model.type == 'Network') {
  m1 <- unique(m.store[, c(1:2, 16, 24, 32:37)]) # 7 metrics, scaling, onsetObs, observed 1-4 weeks
  m2 <- unique(m.store[, c(1:2, 17, 24, 32:37)])
  m3 <- unique(m.store[, c(1:2, 18, 24, 32:37)])
  m4 <- unique(m.store[, c(1:2, 19, 24, 32:37)])
} else if (model.type == 'Individual') {
  m1 <- unique(m.store[, c(1, 15, 23, 31:37)])
  m2 <- unique(m.store[, c(1, 16, 23, 31:37)])
  m3 <- unique(m.store[, c(1, 17, 23, 31:37)])
  m4 <- unique(m.store[, c(1, 18, 23, 31:37)])
}

# Rescale values:
m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

# Add/edit metrics markers:
levels(d500$metric) <- c('1week', '2week', '3week', '4week')

m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

# Compile m's:
if (model.type == 'Network') {
  names(m1)[3] = names(m2)[3] = names(m3)[3] = names(m4)[3] = 'obs_xweek'
} else if (model.type == 'Individual') {
  names(m1)[2] = names(m2)[2] = names(m3)[2] = names(m4)[2] = 'obs_xweek'
}
m <- rbind(m1, m2, m3, m4)

# Merge:
d500 <- merge(d500, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'metric'))

# Remove where no onset:
d500 <- d500[!is.na(d500$onsetObs5), ]

# Categorize obs_xweek by bin:
d500$obs_xweek_bin <- cut(d500$obs_xweek, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_xweek_bin) <- c(seq(500, 14000, by = 500), 100000)
d500$obs_xweek_bin[d500$obs_xweek == 0 & !is.na(d500$obs_xweek)] <- 500
# Note that next week observations are very 0-heavy

# Remove where obs is NA:
d500 <- d500[!is.na(d500$obs_xweek), ]

# Check that no probs NA?:
print(d500$value[is.na(d500$value)])

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$bin == d500$obs_xweek_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$value); d500$score[d500$score == -Inf] <- -10

# Combine:
d <- d500
rm(d500)

# Get lead weeks:
if (model.type == 'Network') {
  m <- unique(m.store[, c(1:2, 6, 32:36, 38, 45, 56)]) # want: season, country, run, oev_base, oev_denom, lambda, fc_start, all 4 leads
} else if (model.type == 'Individual') {
  m <- unique(m.store[, c(1, 5, 31:36, 38, 45, 56)])
}

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:2, 4, 3, 5:8, 11:12, 16:19, 15)]

# Save:
write.csv(d, file = 'logScores_1-4wks_bin.csv', row.names = FALSE)
rm(d.full, d, m, m1, m2, m3, m4)


