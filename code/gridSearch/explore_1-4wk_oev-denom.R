
# # Note: only ran all oev_denoms for the first 3 seasons; 1/10/50 for all
# e1 <- read.csv('code/gridSearch/outputs/outputEns_081219_1wk.csv')
# e2 <- read.csv('code/gridSearch/outputs/outputEns_081219_2wk.csv')
# e3 <- read.csv('code/gridSearch/outputs/outputEns_081219_3wk.csv')
# e4 <- read.csv('code/gridSearch/outputs/outputEns_081219_4wk.csv')
# 
# # can still stick to a single lambda at least:
# e1 <- e1[e1$lambda == 1.02, ]
# e2 <- e2[e2$lambda == 1.02, ]
# e3 <- e3[e3$lambda == 1.02, ]
# e4 <- e4[e4$lambda == 1.02, ]
# 
# # then need to calculate the relevant log scores:
# m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
# m <- m[m$lambda == 1.02, ]
# 
# m1 <- unique(m[, c(1:8, 25, 39)])
# m2 <- unique(m[, c(1:8, 26, 39)])
# m3 <- unique(m[, c(1:8, 27, 39)])
# m4 <- unique(m[, c(1:8, 28, 39)])
# 
# e1 <- merge(e1, m1, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
# e1 <- e1[!is.na(e1$onsetObs5), ]
# 
# e2 <- merge(e2, m2, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
# e2 <- e2[!is.na(e2$onsetObs5), ]
# 
# e3 <- merge(e3, m3, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
# e3 <- e3[!is.na(e3$onsetObs5), ]
# 
# e4 <- merge(e4, m4, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country'))
# e4 <- e4[!is.na(e4$onsetObs5), ]
# 
# # Un-scale ens:
# for (i in 9:308) {
#   e1[, i] <- e1[, i] / e1$scaling
#   e2[, i] <- e2[, i] / e2$scaling
#   e3[, i] <- e3[, i] / e3$scaling
#   e4[, i] <- e4[, i] / e4$scaling
# }
# 
# # Remove NAs:
# e1 <- e1[!is.na(e1$obs_1week), ]
# e2 <- e2[!is.na(e2$obs_2week), ]
# e3 <- e3[!is.na(e3$obs_3week), ]
# e4 <- e4[!is.na(e4$obs_4week), ]
# 
# # Also remove where obs are 0:
# e1 <- e1[e1$obs_1week > 0, ]
# e2 <- e2[e2$obs_2week > 0, ]
# e3 <- e3[e3$obs_3week > 0, ]
# e4 <- e4[e4$obs_4week > 0, ]
# 
# # Calculate absolute error + obs:
# for (i in 9:308) {
#   e1[, i] <- abs(e1[, i] - e1$obs_1week) + e1$obs_1week
#   e2[, i] <- abs(e2[, i] - e2$obs_2week) + e2$obs_2week
#   e3[, i] <- abs(e3[, i] - e3$obs_3week) + e3$obs_3week
#   e4[, i] <- abs(e4[, i] - e4$obs_4week) + e4$obs_4week
# }
# 
# # For each row, determine how many of the 300 ensemble members are within 5% of the observed value:
# scores1 <- sapply(1:dim(e1)[1], function(ix) {
#   log(length(which(e1[ix, 9:308] < 1.10 * e1$obs_1week[ix])) / 300)
# })
# scores1[scores1 == -Inf] <- -10
# e1$scores <- scores1
# hist(scores1)
# 
# scores2 <- sapply(1:dim(e2)[1], function(ix) {
#   log(length(which(e2[ix, 9:308] < 1.10 * e2$obs_2week[ix])) / 300)
# })
# scores2[scores2 == -Inf] <- -10
# e2$scores <- scores2
# hist(scores2)
# 
# scores3 <- sapply(1:dim(e3)[1], function(ix) {
#   log(length(which(e3[ix, 9:308] < 1.10 * e3$obs_3week[ix])) / 300)
# })
# scores3[scores3 == -Inf] <- -10
# e3$scores <- scores3
# hist(scores3)
# 
# scores4 <- sapply(1:dim(e4)[1], function(ix) {
#   log(length(which(e4[ix, 9:308] < 1.10 * e4$obs_4week[ix])) / 300)
# })
# scores4[scores4 == -Inf] <- -10
# e4$scores <- scores4
# hist(scores4)
# 
# # Reduce appropriately:
# e1 <- e1[, c(1:4, 6:8, 310, 312)]
# e2 <- e2[, c(1:4, 6:8, 310, 312)]
# e3 <- e3[, c(1:4, 6:8, 310, 312)]
# e4 <- e4[, c(1:4, 6:8, 310, 312)]
# 
# # Get lead weeks (just to plot all things by consistent x-axis):
# m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
# m <- m[m$lambda == 1.02, ]
# m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
# m <- unique(m[, c(1:4, 7:8, 15, 92)])
# 
# e1 <- merge(e1, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'fc_start'))
# e2 <- merge(e2, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'fc_start'))
# e3 <- merge(e3, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'fc_start'))
# e4 <- merge(e4, m, by = c('season', 'country', 'run', 'oev_base', 'oev_denom', 'fc_start'))
# 
# # Joing all files:
# names(e1)[8] = names(e2)[8] = names(e3)[8] = names(e4)[8] = 'obs_val'
# e <- rbind(e1, e2, e3, e4)
# 
# # Save as temporary file:
# write.csv(e, file = 'code/gridSearch/outputs/logScores_1-4wk_081219.csv', row.names = FALSE)
# rm(list = ls())

### 1/2/5/10/20/50 for 2010-13
library(ggplot2); library(gridExtra)

# Read in and calculate MAEs for Group 1:
m1 <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')

m1 <- m1[m1$lambda == 1.02, ]
m1 <- m1[, c(1:4, 6:8, 15, 25:32, 39, 43, 92)]
m1 <- m1[!is.na(m1$onsetObs5), ]
m1$oev_base <- factor(m1$oev_base); m1$oev_denom <- factor(m1$oev_denom); m1$run <- factor(m1$run)

m1$obs_1week[m1$obs_1week == 0] <- NA
m1$obs_2week[m1$obs_2week == 0] <- NA
m1$obs_3week[m1$obs_3week == 0] <- NA
m1$obs_4week[m1$obs_4week == 0] <- NA

m1$abs_err_1wk_perc <- (abs(m1$fcast_1week - m1$obs_1week) / m1$obs_1week) * 100
m1$abs_err_2wk_perc <- (abs(m1$fcast_2week - m1$obs_2week) / m1$obs_2week) * 100
m1$abs_err_3wk_perc <- (abs(m1$fcast_3week - m1$obs_3week) / m1$obs_3week) * 100
m1$abs_err_4wk_perc <- (abs(m1$fcast_4week - m1$obs_4week) / m1$obs_4week) * 100

# Read in log scores for Group 1:
e1 <- read.csv('code/gridSearch/outputs/logScores_1-4wk_081219.csv')

# Plot MAEs for Group 1:
m <- m1
m.temp1 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp2 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp3 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp4 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp1, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp2, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp3, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp4, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p1 <- ggplot(data = m.agg, aes(x = leadpkwk_mean, y = log(mae), col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Spectral') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Denom.')

m.temp.1wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp.2wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp.3wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp.4wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.2wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.3wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.4wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p2 <- ggplot(data = m.agg, aes(x = FWeek_pkwk, y = log(mae), col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Spectral') +
  labs(x = 'Observed Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Denom.')

# Plot log scores for Group 1:
names(e1)[9] <- 'score'
e.temp <- e1[e1$leadpkwk_mean >= -6 & e1$leadpkwk_mean < 5, ]
e.agg <- aggregate(score ~ leadpkwk_mean + metric + oev_base + oev_denom, data = e.temp, FUN = median)
e.agg$oev_denom <- factor(e.agg$oev_denom)

p3 <- ggplot(data = e.agg, aes(x = leadpkwk_mean, y = score, col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_color_brewer(palette = 'Spectral') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score', col = 'OEV Denom.') + scale_x_continuous(breaks = -8:4)

e.temp <- e1[e1$FWeek_pkwk >= -6 & e1$FWeek_pkwk < 5, ]
e.agg <- aggregate(score ~ FWeek_pkwk + metric + oev_base + oev_denom, data = e.temp, FUN = median)
e.agg$oev_denom <- factor(e.agg$oev_denom)

p4 <- ggplot(data = e.agg, aes(x = FWeek_pkwk, y = score, col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_color_brewer(palette = 'Spectral') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = 'OEV Denom.') + scale_x_continuous(breaks = -8:4)

# Save all:
pdf('results/plots/alt_OEVDenom1.pdf', width = 14, height = 9)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()
rm(list = ls())

### 1/10/50 for all seasons
# Read in and calculate MAEs for Group 2:
m2 <- read.csv('code/gridSearch/outputs/outputMet_090119_pro.csv')

m2 <- m2[m2$lambda == 1.02, ]
m2 <- m2[, c(1:4, 6:8, 15, 25:32, 39, 43, 92)]
m2 <- m2[!is.na(m2$onsetObs5), ]
m2$oev_base <- factor(m2$oev_base); m2$oev_denom <- factor(m2$oev_denom); m2$run <- factor(m2$run)

m2$obs_1week[m2$obs_1week == 0] <- NA
m2$obs_2week[m2$obs_2week == 0] <- NA
m2$obs_3week[m2$obs_3week == 0] <- NA
m2$obs_4week[m2$obs_4week == 0] <- NA

m2$abs_err_1wk_perc <- (abs(m2$fcast_1week - m2$obs_1week) / m2$obs_1week) * 100
m2$abs_err_2wk_perc <- (abs(m2$fcast_2week - m2$obs_2week) / m2$obs_2week) * 100
m2$abs_err_3wk_perc <- (abs(m2$fcast_3week - m2$obs_3week) / m2$obs_3week) * 100
m2$abs_err_4wk_perc <- (abs(m2$fcast_4week - m2$obs_4week) / m2$obs_4week) * 100

# Read in log scores for Group 2:
e2 <- read.csv('code/gridSearch/outputs/logScores_1-4wk.csv')
e2 <- e2[e2$lambda == 1.02, ]

# Plot MAEs for Group 2:
m <- m2
m.temp1 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp2 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp3 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp4 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp1, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp2, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp3, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ leadpkwk_mean + oev_base + oev_denom, data = m.temp4, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p1 <- ggplot(data = m.agg, aes(x = leadpkwk_mean, y = log(mae), col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Denom.')

m.temp.1wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp.2wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp.3wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp.4wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.2wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.3wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ FWeek_pkwk + oev_denom + oev_base, data = m.temp.4wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p2 <- ggplot(data = m.agg, aes(x = FWeek_pkwk, y = log(mae), col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Denom.')

# Plot log scores for Group 2:
names(e2)[10] <- 'score'
e.temp <- e2[e2$leadpkwk_mean >= -6 & e2$leadpkwk_mean < 5, ]
e.agg <- aggregate(score ~ leadpkwk_mean + metric + oev_base + oev_denom, data = e.temp, FUN = median)
e.agg$oev_denom <- factor(e.agg$oev_denom)

p3 <- ggplot(data = e.agg, aes(x = leadpkwk_mean, y = score, col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score', col = 'OEV Denom.') + scale_x_continuous(breaks = -8:4)

e.temp <- e2[e2$FWeek_pkwk >= -6 & e2$FWeek_pkwk < 5, ]
e.agg <- aggregate(score ~ FWeek_pkwk + metric + oev_base + oev_denom, data = e.temp, FUN = median)
e.agg$oev_denom <- factor(e.agg$oev_denom)

p4 <- ggplot(data = e.agg, aes(x = FWeek_pkwk, y = score, col = oev_denom)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = 'OEV Denom.') + scale_x_continuous(breaks = -8:4)

# Save all:
pdf('results/plots/alt_OEVDenom2.pdf', width = 14, height = 9)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()
rm(list = ls())




