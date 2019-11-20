
### By Bins ###
# # Read in dist file:
# d <- read.csv('results/original/outputDist_111819.csv')
# d.wks <- d[d$metric %in% levels(d$metric)[1:4], ]; d.wks$metric <- factor(d.wks$metric)
# d <- d[d$metric == 'pi', ]
# 
# # at this point, results are already binned by 1000s; try binning by 500/250 instead:
# # Read in ens files:
# e.pi <- read.csv('results/original/outputEns_111819_PI.csv') # max observed scaled PI is ~14000
# e1 <- read.csv('results/original/outputEns_111819_1wk.csv')
# e2 <- read.csv('results/original/outputEns_111819_2wk.csv')
# e3 <- read.csv('results/original/outputEns_111819_3wk.csv')
# e4 <- read.csv('results/original/outputEns_111819_4wk.csv')
# 
# # Remove where there are NAs in the ens. predictions (these are where no data for that season/country)
# print(all.equal(e.pi[, 1:7], e1[, 1:7])) # check that data frames in same order
# 
# e1 <- e1[!is.na(e.pi$X1), ]
# e2 <- e2[!is.na(e.pi$X1), ]
# e3 <- e3[!is.na(e.pi$X1), ]
# e4 <- e4[!is.na(e.pi$X1), ]
# 
# e.pi <- e.pi[!is.na(e.pi$X1), ]
# 
# # Function to get new bins:
# resortDist <- function(e, lims) {
#   d.mat <- matrix(NA, nrow = length(lims), ncol = 2)
#   d.mat[, 1] <- lims
#   for (i in 2:length(lims)) {
#     d.mat[i - 1, 2] <- round(length(e[, 9:308][e[, 9:308] >= lims[i - 1] & e[, 9:308] < lims[i]]) / 300, 4)
#   }
#   d.mat[i, 2] <- round(length(e[, 9:308][e[, 9:308] >= max(lims)]) / 300, 4)
#   return(d.mat)
# }
# 
# # Loop through seasons, countries, oev_base, run, and fc_start to calculate the proportion of ens. members in each bin:
# reqLimits500 <- seq(0, 1.4e4, by = 500)
# reqLimits250 <- seq(0, 1.4e4, by = 250)
# 
# e.pi.list <- split(e.pi, 1:nrow(e.pi))
# # e.temp <- e.pi.list[1:1000]
# 
# timestamp()
# d500.list <- lapply(e.pi.list, function(ix) {
#   cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits500), row.names = NULL)
# })
# timestamp() # 100: 17 seconds; with cbind: 15 seconds; 1000: 2 min, 37 seconds
# 
# d250.list <- lapply(e.pi.list, function(ix) {
#   cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits250), row.names = NULL)
# })
# timestamp()
# 
# # Convert lists into new "dist" data frames:
# d500 <- do.call(rbind.data.frame, d500.list)
# d250 <- do.call(rbind.data.frame, d250.list)
# names(d500) = names(d250) = c('season', 'run', 'oev_base', 'fc_start', 'country', 'bin', 'value')
# 
# # Save these!
# write.csv(d500, file = 'results/original/outputDist_111819_PI500.csv', row.names = FALSE)
# write.csv(d250, file = 'results/original/outputDist_111819_PI250.csv', row.names = FALSE)

# And for 1-4 weeks:
e1.list <- split(e1, 1:nrow(e1))
e2.list <- split(e2, 1:nrow(e2))
e3.list <- split(e3, 1:nrow(e3))
e4.list <- split(e4, 1:nrow(e4))

d500.list1 <- lapply(e1.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits500), row.names = NULL)
})
d250.list1 <- lapply(e1.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits250), row.names = NULL)
})

d500.list2 <- lapply(e2.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits500), row.names = NULL)
})
d250.list2 <- lapply(e2.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits250), row.names = NULL)
})

d500.list3 <- lapply(e3.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits500), row.names = NULL)
})
d250.list3 <- lapply(e3.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits250), row.names = NULL)
})

d500.list4 <- lapply(e4.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits500), row.names = NULL)
})
d250.list4 <- lapply(e4.list, function(ix) {
  cbind(ix[, c(1:3, 6:7)], resortDist(ix, reqLimits250), row.names = NULL)
})

d1.500 <- do.call(rbind.data.frame, d500.list1)
# d1.250 <- do.call(rbind.data.frame, d250.list1)

d2.500 <- do.call(rbind.data.frame, d500.list2)
# d2.250 <- do.call(rbind.data.frame, d250.list2)

d3.500 <- do.call(rbind.data.frame, d500.list3)
# d3.250 <- do.call(rbind.data.frame, d250.list3)

d4.500 <- do.call(rbind.data.frame, d500.list4)
# d4.250 <- do.call(rbind.data.frame, d250.list4)

names(d1.500) = names(d2.500) = names(d3.500) = names(d4.500) = c('season', 'run', 'oev_base', 'fc_start', 'country', 'bin', 'value')
# note: haven't done 250 for these yet!

# Save these!
write.csv(d1.500, file = 'results/original/outputDist_111819_1wk500.csv', row.names = FALSE)
write.csv(d2.500, file = 'results/original/outputDist_111819_2wk500.csv', row.names = FALSE)
write.csv(d3.500, file = 'results/original/outputDist_111819_3wk500.csv', row.names = FALSE)
write.csv(d4.500, file = 'results/original/outputDist_111819_4wk500.csv', row.names = FALSE)
# REMEMBER THAT PROPORTION IS THOSE ABOVE THE BIN VALUE, and below the next bin value!!!

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

# Read in dist files:
d <- read.csv('results/original/outputDist_111819_PI250.csv')

# Get observed peak intensity:
m <- read.csv('results/original/outputMet_111819_pro.csv')
m <- unique(m[, c(1, 6, 8, 17, 39)])

# Scale peak intensity values:
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d <- merge(d, m, by = c('season', 'country'))
d <- d[!is.na(d$onsetObs5), ]

# Categorize obs_peak_int by bin:
d$obs_peak_int_bin <- cut(d$obs_peak_int, c(seq(0, 14000, by = 250), 100000))
levels(d$obs_peak_int_bin) <- seq(0, 14000, by = 250)
# QUESTION: Does it matter that bins aren't equally sized?

# Remove where bin not equal to obs_peak_int:
d <- d[d$bin == d$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d$score <- log(d$value)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:5, 9:12)]

# Get lead weeks:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 8:9, 15, 60, 67, 80)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'results/original/logScores_pi_alt1_250.csv', row.names = FALSE)
rm(d)

# Now 1-4 week-ahead forecasts:
d1 <- read.csv('results/original/outputDist_111819_1wk500.csv')
d2 <- read.csv('results/original/outputDist_111819_2wk500.csv')
d3 <- read.csv('results/original/outputDist_111819_3wk500.csv')
d4 <- read.csv('results/original/outputDist_111819_4wk500.csv')

m <- read.csv('results/original/outputMet_111819_pro.csv')
m1 <- unique(m[, c(1:3, 6:8, 25, 39)])
m2 <- unique(m[, c(1:3, 6:8, 26, 39)])
m3 <- unique(m[, c(1:3, 6:8, 27, 39)])
m4 <- unique(m[, c(1:3, 6:8, 28, 39)])

# Rescale values:
m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

# Add metrics marker:
m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

# Merge:
d1 <- merge(d1, m1, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d2 <- merge(d2, m2, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d3 <- merge(d3, m3, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d4 <- merge(d4, m4, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

names(d1)[9] = names(d2)[9] = names(d3)[9] = names(d4)[9] = 'obs_xweek'

# Combine into one data frame:
d <- rbind(d1, d2, d3, d4)
d$metric <- factor(d$metric)

# Remove where no onset:
d <- d[!is.na(d$onsetObs5), ]

# Categorize obs_peak_int by bin:
d$obs_xweek_bin <- cut(d$obs_xweek, c(seq(0, 14000, by = 500), 100000))
levels(d$obs_xweek_bin) <- seq(0, 14000, by = 500)
d$obs_xweek_bin[d$obs_xweek == 0 & !is.na(d$obs_xweek)] <- '0'

# Remove where obs is NA:
d <- d[!is.na(d$obs_xweek), ]

# Remove where bin not equal to obs_xweek_bin:
d <- d[d$bin == d$obs_xweek_bin, ]

# Calculate log scores:
d$score <- log(d$value)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:5, 9:11, 13)]

# Get lead weeks:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 8:9, 15, 60, 67, 80)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'results/original/logScores_1-4wk_alt1_500.csv', row.names = FALSE)
# QUESTION: Do we leave 0s in when analyzing these, or not?

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

### Kernel Density ###
# Read in ens file:
e <- read.csv('results/original/outputEns_111819_PI.csv')
# these should still be scaled, but check

# Determine mean and sd for each row:
means <- sapply(1:dim(e)[1], function(ix) {
  mean(as.numeric(e[ix, 9:308]))
})
sds <- sapply(1:dim(e)[1], function(ix) {
  sd(as.numeric(e[ix, 9:308]))
})
# sds.check <- sapply(1:dim(e)[1], function(ix) {
#   sqrt(var(as.numeric(e[ix, 9:308])))
# })
# print(all.equal(sds, sds.check))
# rm(sds.check)

e$mean <- means; e$sd <- sds

# Reduce appropriately:
e <- e[, c(1:3, 6:7, 309:310)]

# Remove where mean/sd NA:
e <- e[!is.na(e$mean), ]

# Get observed peak intensity and bin:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m <- unique(m[, c(1:2, 8, 17, 39)])
m$obs_peak_int <- m$obs_peak_int * m$scaling

m$obs_peak_int_bin <- cut(m$obs_peak_int, c(seq(0, 14000, by = 250), 100000))
levels(m$obs_peak_int_bin) <- seq(0, 14000, by = 250)
m$obs_peak_int_bin <- as.numeric(as.character(m$obs_peak_int_bin))

# Merge:
e <- merge(e, m, by = c('season', 'country'))

# Find % of normal distribution within observed bin:
e$lower <- e$obs_peak_int_bin; e$upper <- e$obs_peak_int_bin + 250
probs <- sapply(1:dim(e)[1], function(ix) {
  pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
})
e$prob <- probs

# Calculate scores:
e$score <- log(e$prob)
print(e$score[e$score == -Inf])
hist(e$score)

# Reduce appropriately:
e <- e[, c(1:5, 9:10, 15)]

# Get lead weeks:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 8:9, 15, 60, 67, 80)])

e <- merge(e, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(e, file = 'results/original/logScores_pi_alt2_250.csv', row.names = FALSE)
rm(e)

# Now repeat for 1-4 weeks:
e1 <- read.csv('results/original/outputEns_111819_1wk.csv')
e2 <- read.csv('results/original/outputEns_111819_2wk.csv')
e3 <- read.csv('results/original/outputEns_111819_3wk.csv')
e4 <- read.csv('results/original/outputEns_111819_4wk.csv')

e <- rbind(e1, e2, e3, e4)
rm(e1, e2, e3, e4)

# Determine mean and sd for each row:
means <- sapply(1:dim(e)[1], function(ix) {
  mean(as.numeric(e[ix, 9:308]))
})
sds <- sapply(1:dim(e)[1], function(ix) {
  sd(as.numeric(e[ix, 9:308]))
})
e$mean <- means; e$sd <- sds

# Reduce appropriately:
e <- e[, c(1:3, 6:8, 309:310)]

# Remove where mean/sd NA:
e <- e[!is.na(e$mean), ]

# # Re-divide by metric:
# e1 <- e[e$metric == '1week', ]
# e2 <- e[e$metric == '2week', ]
# e3 <- e[e$metric == '3week', ]
# e4 <- e[e$metric == '4week', ]

# Get observed values and bin:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m1 <- unique(m[, c(1:5, 8:9, 25, 39)])
m2 <- unique(m[, c(1:5, 8:9, 26, 39)])
m3 <- unique(m[, c(1:5, 8:9, 27, 39)])
m4 <- unique(m[, c(1:5, 8:9, 28, 39)])

m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

names(m1)[8] = names(m2)[8] = names(m3)[8] = names(m4)[8] = 'obs_xweek'
m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'
m <- rbind(m1, m2, m3, m4)
rm(m1, m2, m3, m4)

m$obs_xweek_bin <- cut(m$obs_xweek, c(seq(0, 14000, by = 250), 100000))
levels(m$obs_xweek_bin) <- seq(0, 14000, by = 250)
m$obs_xweek_bin <- as.numeric(as.character(m$obs_xweek_bin))

m <- m[!is.na(m$obs_xweek), ]
m$obs_xweek_bin[m$obs_xweek == 0] <- 0

# Merge:
m <- unique(m[, c(1:3, 10:11)])
e <- merge(e, m, by = c('season', 'country', 'fc_start', 'metric'))

# Find % of normal distribution within observed bin:
e$lower <- e$obs_xweek_bin; e$upper <- e$obs_xweek_bin + 250
probs <- sapply(1:dim(e)[1], function(ix) {
  pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
})
e$prob <- probs

# Calculate scores:
e$score <- log(e$prob)
print(e$score[e$score == -Inf])
hist(e$score)

# Reduce appropriately:
e <- e[, c(1:6, 13)]

# Get lead weeks:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 8:9, 15, 60, 67, 80)])

e <- merge(e, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(e, file = 'results/original/fairComp/logScores_1-4wk_alt2_250.csv.csv', row.names = FALSE)
rm(e)













