
# Read in metrics and calculate MAEs:
m <- read.csv('results/highOEVBase/outputMet_091619_pro.csv')

m <- m[, c(1:3, 6:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 92:95, 97, 99:101)]
m <- m[!is.na(m$onsetObs5), ]
m$oev_base <- factor(m$oev_base); m$run <- factor(m$run)
m$leadonset5 <- m$fc_start - m$onset5

m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

m$obs_1week[m$obs_1week == 0] <- NA
m$obs_2week[m$obs_2week == 0] <- NA
m$obs_3week[m$obs_3week == 0] <- NA
m$obs_4week[m$obs_4week == 0] <- NA

m$abs_err_1wk_perc <- (abs(m$fcast_1week - m$obs_1week) / m$obs_1week) * 100
m$abs_err_2wk_perc <- (abs(m$fcast_2week - m$obs_2week) / m$obs_2week) * 100
m$abs_err_3wk_perc <- (abs(m$fcast_3week - m$obs_3week) / m$obs_3week) * 100
m$abs_err_4wk_perc <- (abs(m$fcast_4week - m$obs_4week) / m$obs_4week) * 100

# Read in log scores:
e <- read.csv('results/highOEVBase/logScores_1-4wk.csv')

# Combine with the lower options so we can compare all:
m.high <- m; e.high <- e

source('code/compareModels/readIn_metrics.R')
source('code/compareModels/readIn_logScores.R')
rm(d, e.pi)

m <- m[m$model == 'Original', ]; e <- e[e$model == 'Original', ]
m$model <- NULL; e$model <- NULL

m <- rbind(m, m.high)
e <- rbind(e, e.high)
rm(m.high, e.high)

# Plot MAE results:
m.temp1 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp2 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp3 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp4 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ leadpkwk_mean + oev_base, data = m.temp1, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ leadpkwk_mean + oev_base, data = m.temp2, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ leadpkwk_mean + oev_base, data = m.temp3, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ leadpkwk_mean + oev_base, data = m.temp4, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[3] = names(m.2wk.agg)[3] = names(m.3wk.agg)[3] = names(m.4wk.agg)[3] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p1 <- ggplot(data = m.agg, aes(x = leadpkwk_mean, y = log(mae), col = oev_base)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ metric, scales = 'free_y', ncol = 1) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Base:') + theme(legend.position = 'bottom')

m.temp.1wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp.2wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp.3wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp.4wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_4wk_perc), ]

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ FWeek_pkwk + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ FWeek_pkwk + oev_base, data = m.temp.2wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ FWeek_pkwk + oev_base, data = m.temp.3wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ FWeek_pkwk + oev_base, data = m.temp.4wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[3] = names(m.2wk.agg)[3] = names(m.3wk.agg)[3] = names(m.4wk.agg)[3] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

p2 <- ggplot(data = m.agg, aes(x = FWeek_pkwk, y = log(mae), col = oev_base)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ metric, scales = 'free_y', ncol = 1) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'log(Mean Absolute Percentage Error)', col = 'OEV Base:') + theme(legend.position = 'bottom')

# Plot log scores:
names(e)[8] <- 'score'
e.temp <- e[e$leadpkwk_mean >= -6 & e$leadpkwk_mean < 5, ]
e.agg <- aggregate(score ~ leadpkwk_mean + metric + oev_base, data = e.temp, FUN = median)
e.agg$oev_base <- factor(e.agg$oev_base)

p3 <- ggplot(data = e.agg, aes(x = leadpkwk_mean, y = score, col = oev_base)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ metric, scales = 'free_y', ncol = 1) + scale_color_brewer(palette = 'Set1') + theme(legend.position = 'bottom') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score', col = 'OEV Base:') + scale_x_continuous(breaks = -8:4)

e.temp <- e[e$FWeek_pkwk >= -6 & e$FWeek_pkwk < 5, ]
e.agg <- aggregate(score ~ FWeek_pkwk + metric + oev_base, data = e.temp, FUN = median)
e.agg$oev_base <- factor(e.agg$oev_base)

p4 <- ggplot(data = e.agg, aes(x = FWeek_pkwk, y = score, col = oev_base)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ metric, scales = 'free_y', ncol = 1) + scale_color_brewer(palette = 'Set1') + theme(legend.position = 'bottom') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = 'OEV Base:') + scale_x_continuous(breaks = -8:4)

# Save plots:
pdf('results/plots/alt_OEVBase.pdf', width = 14, height = 9)
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
dev.off()

rm(list = ls())
