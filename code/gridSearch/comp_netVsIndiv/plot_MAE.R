
# Get aggregated data frames for each relevant variable:
m.temp <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$leadonset5), ]

m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = mean)
# note: lambda does seem to have some impact here, but collapse for now
m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + model + oev_base, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + oev_base, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ leadonset5 + model + oev_base, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('leadpkwk_mean', 'model', 'oev_base'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('leadpkwk_mean', 'model', 'oev_base'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('leadonset5', 'model', 'oev_base'))

# p1 <- ggplot(data = m.pt.agg, aes(x = leadpkwk_mean, y = abs_err_pkwk.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_pkwk.y)) +
#   theme_bw() + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
# p2 <- ggplot(data = m.pi.agg, aes(x = leadpkwk_mean, y = abs_err_int_perc.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_int_perc.y)) +
#   theme_bw() + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
# p3 <- ggplot(data = m.ot.agg, aes(x = leadonset5, y = abs_err_onset.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_onset.y)) +
#   theme_bw() + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
# 
# pdf('code/gridSearch/plots/pres_comp_MAE_byPred.pdf', width = 8, height = 9)
# grid.arrange(p1, p2, p3, ncol = 1)
# dev.off()

p1 <- ggplot(data = m.pt.agg, aes(x = leadpkwk_mean, y = abs_err_pkwk.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_pkwk.y)) +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = leadpkwk_mean, y = abs_err_int_perc.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_int_perc.y)) +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = leadonset5, y = abs_err_onset.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_onset.y)) +
  theme_bw() + facet_wrap(~ oev_base) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
grid.arrange(p1, p2, p3, ncol = 1)

# # By obs:
# m.temp <- m[m$FWeek_pkwk >= -8 & m$FWeek_pkwk < 5 & !is.na(m$leadonset5), ]
# m.temp2 <- m[m$FWeek_onwk >= -4 & m$FWeek_onwk < 7 & !is.na(m$leadonset5), ]
# 
# m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = mean)
# m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = mean)
# m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + oev_base, data = m.temp2, FUN = mean)
# 
# m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = length)
# m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + oev_base, data = m.temp, FUN = length)
# m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + model + oev_base, data = m.temp2, FUN = length)
# 
# m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'model', 'oev_base'))
# m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'model', 'oev_base'))
# m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'model', 'oev_base'))
# 
# p1 <- ggplot(data = m.pt.agg, aes(x = FWeek_pkwk, y = abs_err_pkwk.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_pkwk.y)) +
#   theme_bw() + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
# p2 <- ggplot(data = m.pi.agg, aes(x = FWeek_pkwk, y = abs_err_int_perc.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_int_perc.y)) +
#   theme_bw() + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Observed Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
# p3 <- ggplot(data = m.ot.agg, aes(x = FWeek_onwk, y = abs_err_onset.x, col = model)) + geom_line() + geom_point(aes(size = abs_err_onset.y)) +
#   theme_bw() + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
#   labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
# 
# pdf('code/gridSearch/plots/pres_comp_MAE_byObs.pdf', width = 8, height = 9)
# grid.arrange(p1, p2, p3, ncol = 1)
# dev.off()

# Back to original code:
m.temp.1wk <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_1wk_perc), ]
m.1wk.agg <- aggregate(abs_err_1wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.1wk.agg$metric <- '1 Week'
m.temp.2wk <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_2wk_perc), ]
m.2wk.agg <- aggregate(abs_err_2wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.2wk.agg$metric <- '2 Week'
m.temp.3wk <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_3wk_perc), ]
m.3wk.agg <- aggregate(abs_err_3wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.3wk.agg$metric <- '3 Week'
m.temp.4wk <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$abs_err_4wk_perc), ]
m.4wk.agg <- aggregate(abs_err_4wk_perc ~ leadpkwk_mean + model + oev_base, data = m.temp.1wk, FUN = mean)
m.4wk.agg$metric <- '4 Week'
names(m.1wk.agg)[4] = names(m.2wk.agg)[4] = names(m.3wk.agg)[4] = names(m.4wk.agg)[4] = 'mae'

m.agg <- rbind(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg)
m.agg$metric <- factor(m.agg$metric)
p4 <- ggplot(data = m.agg, aes(x = leadpkwk_mean, y = log(mae), col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_grid(metric ~ oev_base, scales = 'free_y') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'log(Mean Absolute Percentage Error)', col = '')
print(p4)

rm(m.1wk.agg, m.2wk.agg, m.3wk.agg, m.4wk.agg, m.agg, m.pt.agg, m.pi.agg, m.ot.agg, m.temp.1wk, m.temp.2wk,
   m.temp.3wk, m.temp.4wk, m.temp, m.temp2, p1, p2, p3, p4)

