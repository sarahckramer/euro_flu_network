
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Read in and compile metrics files:
source('code/compareModels/readIn_metrics.R')

# Reduce data frames to lead weeks of interest:
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$leadonset5), ]

m.temp.1wk <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp.2wk <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp.3wk <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp.4wk <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_4wk_perc), ]

# Get aggregated data frames for each relevant variable:
m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + model + oev_base, data = m.temp2, FUN = mean)

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

# Plot:
p1 <- ggplot(data = m.pt.agg, aes(x = leadpkwk_mean, y = abs_err_pkwk, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = leadpkwk_mean, y = abs_err_int_perc, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = leadonset5, y = abs_err_onset, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
p4 <- ggplot(data = m.agg, aes(x = leadpkwk_mean, y = log(mae), col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = '')

pdf('results/plots/comp_MAE.pdf', width = 14, height = 9)
grid.arrange(p1, p2, p3, ncol = 1)
print(p4)
dev.off()

# Clean up:
rm(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg, m.agg, m.pt.agg, m.pi.agg, m.ot.agg, m.temp.1wk, m.temp.2wk,
   m.temp.3wk, m.temp.4wk, m.temp, m.temp2, p1, p2, p3, p4)

# And for observed leads:
m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$FWeek_onwk), ]
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7 & !is.na(m$FWeek_onwk), ]

m.temp.1wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_1wk_perc), ]
m.temp.2wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_2wk_perc), ]
m.temp.3wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_3wk_perc), ]
m.temp.4wk <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$abs_err_4wk_perc), ]

# Get aggregated data frames for each relevant variable:
m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + oev_base, data = m.temp2, FUN = mean)

m.1wk.agg <- aggregate(abs_err_1wk_perc ~ FWeek_pkwk + model + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ FWeek_pkwk + model + oev_base, data = m.temp.1wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ FWeek_pkwk + model + oev_base, data = m.temp.1wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ FWeek_pkwk + model + oev_base, data = m.temp.1wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'
m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)

# Plot:
p1 <- ggplot(data = m.pt.agg, aes(x = FWeek_pkwk, y = abs_err_pkwk, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = FWeek_pkwk, y = abs_err_int_perc, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = FWeek_onwk, y = abs_err_onset, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
p4 <- ggplot(data = m.agg, aes(x = FWeek_pkwk, y = log(mae), col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = '')

pdf('results/plots/comp_MAE_byObs.pdf', width = 14, height = 9)
grid.arrange(p1, p2, p3, ncol = 1)
print(p4)
dev.off()

# Clean up:
rm(list = ls())


