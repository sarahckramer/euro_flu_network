
### By Bins ###
# Read in dist file:
d <- read.csv('results/original/outputDist_110819.csv')
d.wks <- d[d$metric %in% levels(d$metric)[1:4], ]; d.wks$metric <- factor(d.wks$metric)
d <- d[d$metric == 'pi', ]

# at this point, results are already binned by 1000s; can just use these, no need to recode

# Get observed peak intensity:
m <- read.csv('results/original/outputMet_110819_pro.csv')
m <- unique(m[, c(1, 6, 8, 17, 39)])

# Scale peak intensity values:
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d <- merge(d, m, by = c('season', 'country'))
d <- d[!is.na(d$onsetObs5), ]

# Categorize obs_peak_int by bin:
d$obs_peak_int_bin <- cut(d$obs_peak_int, c(seq(0, 10000, by = 1000), 100000))
levels(d$obs_peak_int_bin) <- c(seq(1000, 10000, by = 1000), 100000)
# QUESTION: Does it matter that bins aren't equally sized?

# Remove where bin not equal to obs_peak_int:
d <- d[d$bin == d$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d$score <- log(d$value)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:4, 7, 11:15)]

# Get lead weeks:
m <- read.csv('results/original/outputMet_110819_pro.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:3, 7:8, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'results/original/logScores_pi_alt1.csv', row.names = FALSE)
rm(d)

# Now 1-4 week-ahead forecasts:
d1 <- d.wks[d.wks$metric == 'nextweek1', ]
d2 <- d.wks[d.wks$metric == 'nextweek2', ]
d3 <- d.wks[d.wks$metric == 'nextweek3', ]
d4 <- d.wks[d.wks$metric == 'nextweek4', ]
rm(d.wks)

m <- read.csv('results/original/outputMet_110819_pro.csv')
m1 <- unique(m[, c(1:3, 6:8, 25, 39)])
m2 <- unique(m[, c(1:3, 6:8, 26, 39)])
m3 <- unique(m[, c(1:3, 6:8, 27, 39)])
m4 <- unique(m[, c(1:3, 6:8, 28, 39)])

# Rescale values:
m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

# Merge:
d1 <- merge(d1, m1, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d2 <- merge(d2, m2, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d3 <- merge(d3, m3, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d4 <- merge(d4, m4, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

names(d1)[12] = names(d2)[12] = names(d3)[12] = names(d4)[12] = 'obs_xweek'

# Combine into one data frame:
d <- rbind(d1, d2, d3, d4)

# Remove where no onset:
d <- d[!is.na(d$onsetObs5), ]

# Categorize obs_peak_int by bin:
d$obs_xweek_bin <- cut(d$obs_xweek, c(seq(0, 10000, by = 1000), 100000))
levels(d$obs_xweek_bin) <- c(seq(1000, 10000, by = 1000), 100000)
d$obs_xweek_bin[d$obs_xweek == 0 & !is.na(d$obs_xweek)] <- '1000'

# Remove where obs is NA:
d <- d[!is.na(d$obs_xweek), ]

# Remove where bin not equal to obs_xweek_bin:
d <- d[d$bin == d$obs_xweek_bin, ]

# Calculate log scores:
d$score <- log(d$value)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:5, 8, 11:13, 15)]

# Get lead weeks:
m <- read.csv('results/original/outputMet_110819_pro.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:3, 7:8, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'results/original/logScores_1-4wk_alt1.csv', row.names = FALSE)
# QUESTION: Do we leave 0s in when analyzing these, or not?

### Kernel Density ###
# Read in ens file:
e <- read.csv('results/original/outputEns_110819_PI.csv')
# these should still be scaled, but check

# Determine mean and sd for each row:
means <- sapply(1:dim(e)[1], function(ix) {
  mean(e[ix, 9:308])
})
sds <- sapply(1:dim(e)[1], function(ix) {
  sd(e[ix, 9:308])
})
sds.check <- sapply(1:dim(e)[1], function(ix) {
  sqrt(var(e[ix, 9:308]))
})

e$mean <- means; e$sd <- sds

# Reduce appropriately:



# Draw 300 from each and bin:




# Get observed peak intensity:
m <- read.csv('results/original/outputMet_110819_pro.csv')
m <- unique(m[, c(1, 6, 8, 17, 39)])

# Scale peak intensity values:
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d <- merge(d, m, by = c('season', 'country'))
d <- d[!is.na(d$onsetObs5), ]

# Assign each observed peak intensity to a bin:



# Get proportion of 300 draws that are in the "correct" bin:



# Calculate log scores:




# Reduce appropriately:
d <- d[, c(1:4, 7, 11:15)]

# Get lead weeks:
m <- read.csv('results/original/outputMet_110819_pro.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:3, 7:8, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'results/original/logScores_pi_alt2.csv', row.names = FALSE)
rm(d)









# For each row, determine how many of the 300 ensemble members are within 25% of the observed peak intensity:
scores <- sapply(1:dim(e)[1], function(ix) {
  log(length(which(e[ix, 9:308] < 1.25 * e$obs_peak_int[ix])) / 300)
}) # timed: ~30-40 minutes
scores[scores == -Inf] <- -10
e$score <- scores






















