
# Are there countries that are improving and others that get worse? Which countries?
library(ggplot2)

# Read in metrics and log scores:
source('code/compareModels/readIn_metrics.R')
source('code/compareModels/readIn_logScores.R')
rm(e)

# Limit to desired countries, models, and parameters:
m <- m[m$oev_base == 1e4 & m$country %in% c('ES', 'PT', 'LU', 'HU') & m$model %in% c('Original', 'Individual'), ]
d <- d[d$oev_base == 1e4 & d$country %in% c('ES', 'PT', 'LU', 'HU') & d$model %in% c('Original', 'Individual'), ]
e.pi <- e.pi[e.pi$oev_base == 1e4 & e.pi$country %in% c('ES', 'PT', 'LU', 'HU') & e.pi$model %in% c('Original', 'Individual'), ]

m$model <- factor(m$model); m$country <- factor(m$country)
d$model <- factor(d$model); d$country <- factor(d$country)
e.pi$model <- factor(e.pi$model); e.pi$country <- factor(e.pi$country)

levels(m$model) <- c('Network', 'Individual')
levels(d$model) <- c('Network', 'Individual')
levels(e.pi$model) <- c('Network', 'Individual')

### Compare MAE ###
m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7, ]

m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + oev_base + country, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + oev_base + country, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + oev_base + country, data = m.temp2, FUN = mean)

p1 <- ggplot(data = m.pt.agg, aes(x = FWeek_pkwk, y = abs_err_pkwk, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = FWeek_pkwk, y = abs_err_int_perc, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute PErcentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = FWeek_onwk, y = abs_err_onset, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')

# # pdf('results/plots/comp_MAE_byCountry_PRES.pdf', width = 14, height = 9)
# grid.arrange(p1, p2, p3, ncol = 1)
# # dev.off()

### Compare log scores ###
d$model <- factor(d$model, levels = levels(d$model)[2:1])
d$country <- factor(d$country, levels = levels(d$country)[c(4, 1, 3, 2)])
levels(d$country) <- c('Portugal', 'Spain', 'Luxembourg', 'Hungary')

e.pi$model <- factor(e.pi$model, levels = levels(e.pi$model)[2:1])
e.pi$country <- factor(e.pi$country, levels = levels(e.pi$country)[c(4, 1, 3, 2)])
levels(e.pi$country) <- c('Portugal', 'Spain', 'Luxembourg', 'Hungary')

d.temp <- d[d$FWeek_pkwk >= -6 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = d.temp, FUN = median)

p1 <- ggplot(data = d.agg, aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Timing')

e.temp <- e.pi[e.pi$FWeek_pkwk >= -6 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$leadonset5), ]
e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = e.temp, FUN = median)

p2 <- ggplot(data = e.agg, aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Intensity')

d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot', ]# & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_onwk + oev_base + model + country, data = d.temp, FUN = median)

p3 <- ggplot(data = d.agg, aes(x = FWeek_onwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y', nrow = 1) + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:6) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Onset Timing')

pdf('results/plots/comp_logScores_byObs_byCountry_PRES.pdf', width = 12, height = 7)
grid.arrange(p1, p2, p3, ncol = 1)
# improves: AT, CZ, DK, HR, HU, IE, LU; worse: DE, ES, NL, UK
# improves: AT, HU, LU ; worse: BE, ES, NL, PL, PT, RO, UK
# improves: AT, SK, HU, RO, SE, SK; worse: DE, ES, FR, HR, IE, IT, NL, PT, SI, UK
dev.off()


