
### Deal with PT and OT first - can use Dist:
d.pt <- read.csv('code/gridSearch/outputs/outputDist_081219_PT.csv')
d.ot <- read.csv('code/gridSearch/outputs/outputDist_081219_OT.csv')

# Join observed values:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
m <- unique(m[, c(1, 8:9, 39)])

d.pt <- merge(d.pt, m, by = c('season', 'country'))
d.ot <- merge(d.ot, m, by = c('season', 'country'))

# Remove where no onset observed:
d.pt <- d.pt[!is.na(d.pt$onsetObs5), ]
d.ot <- d.ot[!is.na(d.ot$onsetObs5), ]

# Create factors:
d.pt$oev_base <- factor(d.pt$oev_base); d.pt$oev_denom <- factor(d.pt$oev_denom); d.pt$lambda <- factor(d.pt$lambda); d.pt$run <- factor(d.pt$run)
d.ot$oev_base <- factor(d.ot$oev_base); d.ot$oev_denom <- factor(d.ot$oev_denom); d.ot$lambda <- factor(d.ot$lambda); d.ot$run <- factor(d.ot$run)

# Correct bins:
d.pt$bin <- d.pt$bin + 40 - 1
d.ot$bin <- d.ot$bin + 40 - 1

# Keep only results for bins within 1 week of the observed values:
d.pt <- d.pt[d.pt$bin == d.pt$obs_pkwk - 1 | d.pt$bin == d.pt$obs_pkwk | d.pt$bin == d.pt$obs_pkwk + 1, ]
d.ot <- d.ot[d.ot$bin == d.ot$onsetObs5 - 1 | d.ot$bin == d.ot$onsetObs5 | d.ot$bin == d.ot$onsetObs5 + 1, ]

# Search through combinations of country, season, run, oev_base, oev_denom, lambda, fc_start to calculate log score for PT and store:
countries <- levels(d.pt$country) # note: DIFFERENT ORDER than used in forecasting!
log.pt = log.ot = NULL

for (i in 1:length(countries)) {
  print(countries[i])
  
  for (o1 in levels(d.pt$oev_base)) {
    for (o2 in levels(d.pt$oev_denom)) {
      for (l in levels(d.pt$lambda)) {
        
        for (season in levels(d.pt$season)) {
          for (run in levels(d.pt$run)) {
          
          d.temp <- d.pt[d.pt$country == countries[i] & d.pt$oev_base == o1 & d.pt$oev_denom == o2 & d.pt$lambda == l & d.pt$season == season & d.pt$run == run, ]
          
          if (length(d.temp$season) > 0) {
            scores <- unlist(lapply(sort(unique(d.temp$fc_start)), function(ix) {
              log(sum(d.temp[d.temp$fc_start == ix, 'value']))
            }))
            log.pt <- rbind(log.pt, cbind(season, countries[i], run, o1, o2, l, sort(unique(d.temp$fc_start)), scores))
          }
          
          d.temp <- d.ot[d.ot$country == countries[i] & d.ot$oev_base == o1 & d.ot$oev_denom == o2 & d.ot$lambda == l & d.ot$season == season & d.ot$run == run, ]
          if (length(d.temp$season) > 0) {
            scores <- unlist(lapply(sort(unique(d.temp$fc_start)), function(ix) {
              log(sum(d.temp[d.temp$fc_start == ix, 'value']))
            }))
            log.ot <- rbind(log.ot, cbind(season, countries[i], run, o1, o2, l, sort(unique(d.temp$fc_start)), scores))
          }
          
          }
        }
        
      }
    }
  }

}

log.pt <- as.data.frame(log.pt)
log.ot <- as.data.frame(log.ot)

# Set colnames:
names(log.pt) <- c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'score')
names(log.ot) <- c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'score')

# Make scores numeric:
log.pt$score <- as.numeric(as.character(log.pt$score))
log.ot$score <- as.numeric(as.character(log.ot$score))

# Replace -Infs with -10:
log.pt$score[log.pt$score == -Inf] <- -10
log.ot$score[log.ot$score == -Inf] <- -10

# Determine predicted and observed lead weeks:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- unique(m[, c(1:5, 7:9, 15, 39, 92, 107, 118)])
m <- m[!is.na(m$onsetObs5), ]

log.pt <- merge(log.pt, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
log.ot <- merge(log.ot, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Label with relevant metric:
log.pt$metric <- 'pt'
log.ot$metric <- 'ot'

# Save as "temporary" files:
write.csv(log.pt, file = 'code/gridSearch/outputs/logScores_pt.csv', row.names = FALSE)
write.csv(log.ot, file = 'code/gridSearch/outputs/logScores_ot.csv', row.names = FALSE)

rm(list = ls())

### For PI (within 25%) and 1-4 weeks (within 1%), use Ens:
e <- read.csv('code/gridSearch/outputs/outputEns_081219_PI.csv')

# Get observed peak intensity:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
# m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1, 6, 8, 17, 39)])
# and remove repeats:
m <- m[c(1:20, 30:69), ]

e <- merge(e, m, by = c('season', 'country'))
e <- e[!is.na(e$onsetObs5), ]

# Ensure that everything is properly un-scaled:
for (i in 9:308) {
  e[, i] <- e[, i] / e$scaling
}

# Calculate absolute error + obs:
for (i in 9:308) {
  e[, i] <- abs(e[, i] - e$obs_peak_int) + e$obs_peak_int
}

# For each row, determine how many of the 300 ensemble members are within 25% of the observed peak intensity:
scores <- sapply(1:dim(e)[1], function(ix) {
  log(length(which(e[ix, 9:308] < 1.25 * e$obs_peak_int[ix])) / 300)
  # log(length(which(e[ix, 9:308] > 0.75 * e$obs_peak_int[ix] & e[ix, 9:308] < 1.25 * e$obs_peak_int[ix])) / 300)
}) # timed: ~30-40 minutes

# e.temp <- e[100, ]
# log(length(which(e.temp[, 9:308] > 0.875 * e.temp$obs_peak_int & e.temp[, 9:308] < 1.125 * e.temp$obs_peak_int)) / 300)

scores[scores == -Inf] <- -10
e$score <- scores

# Reduce appropriately:
e <- e[, c(1:8, 310, 312)]

# Get lead weeks:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 7:8, 15, 92, 107, 118)])

e <- merge(e, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Save as temporary file:
write.csv(e, file = 'code/gridSearch/outputs/logScores_pi.csv', row.names = FALSE)
rm(list = ls())

# Now 1-4 weeks:
e1 <- read.csv('code/gridSearch/outputs/outputEns_081219_1wk.csv')
e2 <- read.csv('code/gridSearch/outputs/outputEns_081219_2wk.csv')
e3 <- read.csv('code/gridSearch/outputs/outputEns_081219_3wk.csv')
e4 <- read.csv('code/gridSearch/outputs/outputEns_081219_4wk.csv')

# Get appropriate values for that week:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
# m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m1 <- unique(m[, c(1:8, 25, 39)])
m2 <- unique(m[, c(1:8, 26, 39)])
m3 <- unique(m[, c(1:8, 27, 39)])
m4 <- unique(m[, c(1:8, 28, 39)])

e1 <- merge(e1, m1, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
e1 <- e1[!is.na(e1$onsetObs5), ]

e2 <- merge(e2, m2, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
e2 <- e2[!is.na(e2$onsetObs5), ]

e3 <- merge(e3, m3, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
e3 <- e3[!is.na(e3$onsetObs5), ]

e4 <- merge(e4, m4, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
e4 <- e4[!is.na(e4$onsetObs5), ]

# Un-scale ens:
for (i in 9:308) {
  e1[, i] <- e1[, i] / e1$scaling
  e2[, i] <- e2[, i] / e2$scaling
  e3[, i] <- e3[, i] / e3$scaling
  e4[, i] <- e4[, i] / e4$scaling
}

# Remove NAs:
e1 <- e1[!is.na(e1$obs_1week), ]
e2 <- e2[!is.na(e2$obs_2week), ]
e3 <- e3[!is.na(e3$obs_3week), ]
e4 <- e4[!is.na(e4$obs_4week), ]

# Also remove where obs are 0:
e1 <- e1[e1$obs_1week > 0, ]
e2 <- e2[e2$obs_2week > 0, ]
e3 <- e3[e3$obs_3week > 0, ]
e4 <- e4[e4$obs_4week > 0, ]

# Calculate absolute error + obs:
for (i in 9:308) {
  e1[, i] <- abs(e1[, i] - e1$obs_1week) + e1$obs_1week
  e2[, i] <- abs(e2[, i] - e2$obs_2week) + e2$obs_2week
  e3[, i] <- abs(e3[, i] - e3$obs_3week) + e3$obs_3week
  e4[, i] <- abs(e4[, i] - e4$obs_4week) + e4$obs_4week
}

# For each row, determine how many of the 300 ensemble members are within 5% of the observed value:
scores1 <- sapply(1:dim(e1)[1], function(ix) {
  log(length(which(e1[ix, 9:308] < 1.05 * e1$obs_1week[ix])) / 300)
  # log(length(which(e1[ix, 9:308] > 0.95 * e1$obs_1week[ix] & e1[ix, 9:308] < 1.05 * e1$obs_1week[ix])) / 300)
})
scores1[scores1 == -Inf] <- -10
e1$scores <- scores1

# # or else just remove where obs is 0 entirely?? or change to 1?
# e1$obs_1week_1 <- e1$obs_1week
# scores1.comp2 <- sapply(1:174720, function(ix) {
#   log(length(which(e1[ix, 9:308] > 0.975 * e1$obs_1week_1[ix] & e1[ix, 9:308] < 1.025 * e1$obs_1week_1[ix])) / 300)
# })
# scores1.comp2[scores1.comp2 == -Inf] <- -10
# 
# for (i in 9:308) {
#   e1[, i][e1[, i] == 0] <- 1
# }
# scores1.comp3 <- sapply(1:174720, function(ix) {
#   log(length(which(e1[ix, 9:308] > 0.975 * e1$obs_1week_1[ix] & e1[ix, 9:308] < 1.025 * e1$obs_1week_1[ix])) / 300)
# })
# scores1.comp3[scores1.comp3 == -Inf] <- -10

scores2 <- sapply(1:dim(e2)[1], function(ix) {
  log(length(which(e2[ix, 9:308] < 1.05 * e2$obs_2week[ix])) / 300)
})
scores2[scores2 == -Inf] <- -10
e2$scores <- scores2

scores3 <- sapply(1:dim(e3)[1], function(ix) {
  log(length(which(e3[ix, 9:308] < 1.05 * e3$obs_3week[ix])) / 300)
})
scores3[scores3 == -Inf] <- -10
e3$scores <- scores3

scores4 <- sapply(1:dim(e4)[1], function(ix) {
  log(length(which(e4[ix, 9:308] < 1.05 * e4$obs_4week[ix])) / 300)
})
scores4[scores4 == -Inf] <- -10
e4$scores <- scores4

# Reduce appropriately:
e1 <- e1[, c(1:8, 310, 312)]
e2 <- e2[, c(1:8, 310, 312)]
e3 <- e3[, c(1:8, 310, 312)]
e4 <- e4[, c(1:8, 310, 312)]

# Get lead weeks (just to plot all things by consistent x-axis):
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:5, 7:8, 15, 92)])

e1 <- merge(e1, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
e2 <- merge(e2, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
e3 <- merge(e3, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
e4 <- merge(e4, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

# Save as temporary file:
write.csv(e1, file = 'code/gridSearch/outputs/logScores_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'code/gridSearch/outputs/logScores_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'code/gridSearch/outputs/logScores_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'code/gridSearch/outputs/logScores_4wk.csv', row.names = FALSE)
rm(list = ls())

### Compile files:
d.pt <- read.csv('code/gridSearch/outputs/logScores_pt.csv')
d.ot <- read.csv('code/gridSearch/outputs/logScores_ot.csv')
e.pi <- read.csv('code/gridSearch/outputs/logScores_pi.csv')
e1 <- read.csv('code/gridSearch/outputs/logScores_1wk.csv')
e2 <- read.csv('code/gridSearch/outputs/logScores_2wk.csv')
e3 <- read.csv('code/gridSearch/outputs/logScores_3wk.csv')
e4 <- read.csv('code/gridSearch/outputs/logScores_4wk.csv')

d <- rbind(d.pt, d.ot)

names(e1)[9] = names(e2)[9] = names(e3)[9] = names(e4)[9] <- 'obs_val'
e <- rbind(e1, e2, e3, e4)

write.csv(d, file = 'code/gridSearch/outputs/logScores_pt_ot.csv', row.names = FALSE)
write.csv(e, file = 'code/gridSearch/outputs/logScores_1-4wk.csv', row.names = FALSE)


