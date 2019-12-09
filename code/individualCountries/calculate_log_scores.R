
### BINS ###
### Peak/Onset Timing ###
# Read in dist files:
d.full <- read.csv(file = list.files(pattern = 'Dist'))

d.pt <- d.full[d.full$metric == 'pw', ]
d.ot <- d.full[d.full$metric == 'onset5', ]

# Join observed values:
m <- unique(m.store[, c(1, 8:9, 39)])

d.pt <- merge(d.pt, m, by = c('season', 'country'))
d.ot <- merge(d.ot, m, by = c('season', 'country'))

# Remove where no onset observed:
d.pt <- d.pt[!is.na(d.pt$onsetObs5), ]
d.ot <- d.ot[!is.na(d.ot$onsetObs5), ]

# Correct bins:
# d.pt$bin <- d.pt$bin + 40 - 1
d.ot$week <- d.ot$week + 40 - 1

# Keep only results for bins THE SAME week as the observed values:
d.pt <- d.pt[d.pt$week == d.pt$obs_pkwk, ]; d.ot <- d.ot[d.ot$week == d.ot$onsetObs5, ]

# Calculate log scores:
d.pt$score <- log(d.pt$prob); d.pt$score[d.pt$score == -Inf] <- -10
d.ot$score <- log(d.ot$prob); d.ot$score[d.ot$score == -Inf] <- -10

# Determine predicted and observed lead weeks:
m.store$leadonset5 <- m.store$fc_start - m.store$onset5
m <- unique(m.store[, c(1:5, 7:8, 15, 60, 67, 78)])

d.pt <- merge(d.pt, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
d.ot <- merge(d.ot, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Combine:
d <- rbind(d.pt, d.ot)

# Reduce appropriately:
d <- d[, c(1:7, 9, 12:13, 15:18, 14)]

# Save:
write.csv(d, file = 'logScores_pt_ot.csv', row.names = FALSE)

# Clean up:
rm(d, d.pt, d.ot, m)

### Peak Intensity ###
# Get dist info:
d500 <- d.full[d.full$metric == 'pi', ]

# Get observed peak intensity:
m <- unique(m.store[, c(1, 6, 8, 17, 39)])
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d500 <- merge(d500, m, by = c('season', 'country'))
d500 <- d500[!is.na(d500$onsetObs5), ]

# Categorize obs_peak_int by bin:
d500$obs_peak_int_bin <- cut(d500$obs_peak_int, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_peak_int_bin) <- c(seq(500, 14000, by = 500), 100000)

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$week == d500$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$prob); d500$score[d500$score == -Inf] <- -10
d <- d500

# Reduce appropriately:
d <- d[, c(1:3, 8, 5:7, 9, 13:14, 16)]

# Get lead weeks:
m <- unique(m.store[, c(1:5, 7:9, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:8, 12, 9:10, 13:16, 11)]

# Save:
write.csv(d, file = 'logScores_pi_bin.csv', row.names = FALSE)
rm(d, m)

### 1-4 Weeks ###
d500 <- d.full[d.full$metric %in% c('next_week_1', 'next_week_2', 'next_week_3', 'next_week_4'), ]
d500$metric <- factor(d500$metric)

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
m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

# Compile m's:
names(m1)[9] = names(m2)[9] = names(m3)[9] = names(m4)[9] = 'obs_xweek'
m <- rbind(m1, m2, m3, m4)

# Merge:
d500 <- merge(d500, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'metric'))

# Remove where no onset:
d500 <- d500[!is.na(d500$onsetObs5), ]

# Categorize obs_xweek by bin:
d500$obs_xweek_bin <- cut(d500$obs_xweek, c(seq(0, 14000, by = 500), 100000))
levels(d500$obs_xweek_bin) <- c(seq(500, 14000, by = 500), 100000)
d500$obs_xweek_bin[d500$obs_xweek == 0 & !is.na(d500$obs_xweek)] <- 500

# Remove where obs is NA:
d500 <- d500[!is.na(d500$obs_xweek), ]

# Any where prob is NA?
# print(d500$prob[is.na(d500$prob)]) # I think this can happen w/ new OEV where other data sources need to be non-0/NA
d500 <- d500[!is.na(d500$prob), ]
print(d500$prob[is.na(d500$prob)])

# Remove where bin not equal to obs_peak_int_bin:
d500 <- d500[d500$week == d500$obs_xweek_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d500$score <- log(d500$prob); d500$score[d500$score == -Inf] <- -10

# Combine:
d500$metric2 <- 'bin500'
d <- d500
rm(d500)

# Reduce appropriately:
d <- d[, c(1:8, 13:14, 16:17)]

# Get lead weeks:
m <- unique(m.store[, c(1:5, 7:9, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
d <- d[, c(1:2, 4, 3, 5:8, 12, 9:10, 13:17, 11)]

# Save:
write.csv(d, file = 'logScores_1-4wks_bin.csv', row.names = FALSE)
rm(d, m, m1, m2, m3, m4, d.full)

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

### Kernel Density ###

### Peak intensity:
# Read in ens file:
e.full <- read.csv(file = list.files(pattern = 'Ens'))
e <- e.full[e.full$metrics == 'pi', ]

# Determine mean and sd for each row:
means <- sapply(1:dim(e)[1], function(ix) {
  mean(as.numeric(e[ix, 10:309]))
})
sds <- sapply(1:dim(e)[1], function(ix) {
  sd(as.numeric(e[ix, 10:309]))
})
e$mean <- means; e$sd <- sds

# Reduce appropriately:
e <- e[, c(1:9, 310:311)]

# Remove where mean/sd NA:
e <- e[!is.na(e$mean), ]

# Get observed peak intensity and bin:
m <- unique(m.store[, c(1, 6, 8, 17, 39)])
m$obs_peak_int <- m$obs_peak_int * m$scaling

m$obs_peak_int_bin <- cut(m$obs_peak_int, c(seq(0, 14000, by = 500), 100000))
levels(m$obs_peak_int_bin) <- c(seq(500, 14000, by = 500), 100000)
m$obs_peak_int_bin <- as.numeric(as.character(m$obs_peak_int_bin))

# Merge:
e <- merge(e, m, by = c('season', 'country'))

# Find % of normal distribution within observed bin:
e$lower <- e$obs_peak_int_bin - 500; e$upper <- e$obs_peak_int_bin
probs <- sapply(1:dim(e)[1], function(ix) {
  pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
})
e$prob <- probs

# Calculate scores:
e$score <- log(e$prob)
e$score[e$score == -Inf] <- min(e$score[e$score != -Inf]) - 1
print(e$score[e$score == -Inf])

# Reduce appropriately:
e <- e[, c(1:9, 13:14, 19)]

# Get lead weeks:
m <- unique(m.store[, c(1:5, 7:9, 15, 60, 67, 78)])

e <- merge(e, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
e <- e[, c(1:2, 4, 3, 5:7, 13, 10:11, 14:17, 12, 9)]
names(e)[16] <- 'metric'

# Save:
write.csv(e, file = 'logScores_pi_kd.csv', row.names = FALSE)
rm(e, m, probs, means, sds)

# Now repeat for 1-4 weeks:
e1 <- e.full[e.full$metrics == '1week', ]
e2 <- e.full[e.full$metrics == '2week', ]
e3 <- e.full[e.full$metrics == '3week', ]
e4 <- e.full[e.full$metrics == '4week', ]
e <- rbind(e1, e2, e3, e4)
rm(e1, e2, e3, e4)

# Determine mean and sd for each row:
means <- sapply(1:dim(e)[1], function(ix) {
  mean(as.numeric(e[ix, 10:309]))
})
sds <- sapply(1:dim(e)[1], function(ix) {
  sd(as.numeric(e[ix, 10:309]))
})
e$mean <- means; e$sd <- sds

# Reduce appropriately:
e <- e[, c(1:9, 310:311)]

# Remove where mean/sd NA:
e <- e[!is.na(e$mean), ]

# Get observed values and bin:
m1 <- unique(m.store[, c(1:8, 25, 39)])
m2 <- unique(m.store[, c(1:8, 26, 39)])
m3 <- unique(m.store[, c(1:8, 27, 39)])
m4 <- unique(m.store[, c(1:8, 28, 39)])

m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

names(m1)[9] = names(m2)[9] = names(m3)[9] = names(m4)[9] = 'obs_xweek'
m <- rbind(m1, m2, m3, m4)
rm(m1, m2, m3, m4)

m$obs_xweek_bin <- cut(m$obs_xweek, c(seq(0, 14000, by = 500), 100000))
levels(m$obs_xweek_bin) <- c(seq(500, 14000, by = 500), 100000)
m$obs_xweek_bin[m$obs_xweek == 0 & !is.na(m$obs_xweek)] <- 500

m <- m[!is.na(m$obs_xweek), ]

# Merge:
m <- unique(m[, c(1, 7:12)])
names(e)[8] <- 'metric'
e <- merge(e, m, by = c('country', 'season', 'fc_start', 'metric'))

# Find % of normal distribution within observed bin:
e$obs_xweek_bin <- as.numeric(as.character(e$obs_xweek_bin))
e$lower <- e$obs_xweek_bin - 500; e$upper <- e$obs_xweek_bin
probs <- sapply(1:dim(e)[1], function(ix) {
  pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
})
e$prob <- probs

# Calculate scores:
e$score <- log(e$prob)
e$score[e$score == -Inf] <- min(e$score[e$score != -Inf]) - 1
print(e$score[e$score == -Inf])
# hist(e$score)

# Reduce appropriately:
e <- e[, c(1:9, 12:13, 18)]

# Get lead weeks:
m <- unique(m.store[, c(1:5, 7:8, 15, 60, 67, 78)])

e <- merge(e, m, by = c('season', 'country', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
e$metric2 <- 500
e <- e[, c(1:2, 4, 3, 5:8, 17, 10:11, 13:16, 12)]

# Save:
write.csv(e, file = 'logScores_1-4wk_alt.csv', row.names = FALSE)
rm(e, e.full, m, means, sds, probs)






