
# Are there countries that are improving and others that get worse? Which countries?
library(ggplot2)

# Read in metrics and log scores:
source('code/compareModels/readIn_metrics.R')
source('code/compareModels/readIn_logScores.R')

### Compare MAE ###
m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7, ]

m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + oev_base + country, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + oev_base + country, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + oev_base + country, data = m.temp2, FUN = mean)

p1 <- ggplot(data = m.pt.agg[m.pt.agg$oev_base == 1e4, ], aes(x = FWeek_pkwk, y = abs_err_pkwk, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing (OEV Base = 1e4)', col = '')
p2 <- ggplot(data = m.pt.agg[m.pt.agg$oev_base == 1e5, ], aes(x = FWeek_pkwk, y = abs_err_pkwk, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing (OEV Base = 1e5)', col = '')

p3 <- ggplot(data = m.pi.agg[m.pi.agg$oev_base == 1e4, ], aes(x = FWeek_pkwk, y = abs_err_int_perc, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute PErcentage Error', title = 'Peak Intensity (OEV Base = 1e4)', col = '')
p4 <- ggplot(data = m.pi.agg[m.pi.agg$oev_base == 1e5, ], aes(x = FWeek_pkwk, y = abs_err_int_perc, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity (OEV Base = 1e5)', col = '')

p5 <- ggplot(data = m.ot.agg[m.ot.agg$oev_base == 1e4, ], aes(x = FWeek_onwk, y = abs_err_onset, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing (OEV Base = 1e4)', col = '')
p6 <- ggplot(data = m.ot.agg[m.ot.agg$oev_base == 1e5, ], aes(x = FWeek_onwk, y = abs_err_onset, col = model)) + geom_line() + geom_point() +
  theme_bw() + facet_wrap(~ country, scales = 'free_y') + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing (OEV Base = 1e5)', col = '')

# Save:
pdf('results/plots/comp_MAE_byCountry.pdf', width = 14, height = 9)
print(p1) # improve: LU; obviously worse: DE (late), IT, RO
print(p2) # improve: AT, CZ, DE, LU, PL, SK; no obvious decreases; note that new scalings also look worse
print(p3) # improve: DE (early; worse later), DK, LU, SE; obviously worse: ES, FR, PL, PT, RO, UK (although most of these do improve with "new" scalings)
print(p4) # improve: AT, BE, CZ, DE, HU, LU, RO, SI, SK; worse: DK (although new scalings help), ES, NL (but not a lot that are super dramatic)
print(p5) # improve: LU, SK?; obviously worse: ES, FR, IT, PT
print(p6) # improve: AT, BE, CZ, DE, HU, LU, NL, PL, RO, SI, SK; obviously worse: ES, FR, IE, PT (again, new scalings help a bit)
# note that p5 and p6 actually do exclude those forecasts that don't predict an onset, since these don't have an error value
dev.off()

### Compare log scores ###
d.temp <- d[d$FWeek_pkwk >= -6 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = d.temp, FUN = median)

p1 <- ggplot(data = d.temp[d.temp$oev_base == 1e4, ]) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Timing (OEV Base = 1e4)', fill = '') +
  scale_x_continuous(breaks = -8:4)
p2 <- ggplot(data = d.temp[d.temp$oev_base == 1e5, ]) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Timing (OEV Base = 1e5)', fill = '') +
  scale_x_continuous(breaks = -8:4)

print(p1) # overall the ranges of scores are just much wider for all the network models than for the individual models;
# normally I would say this means there are fewer forecasts predicting any onset for individual model; but I'm not sure this is true?
# either way, these don't look super informative - averages would be clearer
print(p2) # 
# print(p3) # 
# print(p4) # 
# print(p5) # 
# print(p6) # 

d.check <- d[d$FWeek_pkwk >= -6 & d$FWeek_pkwk < 5 & d$metric == 'pt', ]
d.check$onsetNA <- ifelse(is.na(d.check$leadonset5), 'isNA', 'notNA')
for (country in unique(d.check$country)) {
  d.check.temp <- d.check[d.check$country == country, ]
  print(country)
  print(table(d.check.temp$onsetNA, d.check.temp$FWeek_pkwk, d.check.temp$model))
  print(''); print('')
}
# where does individual have way more NAs?: AT, BE, CZ, DE (but most have onsets in all models), DK, HR, HU, IE (early), LU, PL, RO, SE, SI, SK
    # not: ES, FR, IT, NL, PT, UK
# so almost 75% of countries have substantially more NAs in predicted onsets in individual than in network model

d.temp <- d[d$FWeek_pkwk >= -6 & d$FWeek_pkwk < 5 & d$metric == 'pt', ]
d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = d.temp, FUN = median)

p1 <- ggplot(data = d.temp[d.temp$oev_base == 1e4, ]) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Timing (OEV Base = 1e4)', fill = '') +
  scale_x_continuous(breaks = -8:4)
p2 <- ggplot(data = d.temp[d.temp$oev_base == 1e5, ]) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Timing (OEV Base = 1e5)', fill = '') +
  scale_x_continuous(breaks = -8:4)
print(p1); print(p2)

# Averages likely more informative; first, leaving out where no onsets predicted (except for OT):
d.temp <- d[d$FWeek_pkwk >= -6 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = d.temp, FUN = median)

p1 <- ggplot(data = d.agg[d.agg$oev_base == 1e4, ], aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Timing (OEV base = 1e4)')
p2 <- ggplot(data = d.agg[d.agg$oev_base == 1e5, ], aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Timing (OEV base = 1e5)')

e.temp <- e.pi[e.pi$FWeek_pkwk >= -6 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$leadonset5), ]
e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model + country, data = e.temp, FUN = median)

p3 <- ggplot(data = e.agg[e.agg$oev_base == 1e4, ], aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Intensity (OEV base = 1e4)')
p4 <- ggplot(data = e.agg[e.agg$oev_base == 1e5, ], aes(x = FWeek_pkwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Peak Intensity (OEV base = 1e5)')

d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot', ]# & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_onwk + oev_base + model + country, data = d.temp, FUN = median)

p5 <- ggplot(data = d.agg[d.agg$oev_base == 1e4, ], aes(x = FWeek_onwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Onset Timing (OEV base = 1e4)')
p6 <- ggplot(data = d.agg[d.agg$oev_base == 1e5, ], aes(x = FWeek_onwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Onset Timing (OEV base = 1e5)')

pdf('results/plots/comp_logScores_byObs_byCountry.pdf', width = 14, height = 9)
print(p1) # improves: AT, CZ, DK, HR, HU, IE, LU; worse: DE, ES, NL, UK
print(p2) # improves: AT, CZ, DK, HR, HU, IT, LU, PL, PT, RO, SE, SI, SK; worse: DE, UK
print(p3) # improves: AT, HU, LU ; worse: BE, ES, NL, PL, PT, RO, UK
print(p4) # improves: AT, BE; CZ, HU, LU, RO, SK; worse: NL, PT, UK 
print(p5) # improves: AT, SK, HU, RO, SE, SK; worse: DE, ES, FR, HR, IE, IT, NL, PT, SI, UK
print(p6) # improves: AT, cZ, DK, HU, PL, RO, SE, SI, SK; worse: ES, FR, IT, NL, PT
dev.off()

# What do ranges of OT log scores look like?:
d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot', ]# & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)

p1 <- ggplot(data = d.temp[d.temp$oev_base == 1e4, ]) + geom_boxplot(aes(x = FWeek_onwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Onset Timing (OEV Base = 1e4)', fill = '') +
  scale_x_continuous(breaks = -8:4)
p2 <- ggplot(data = d.temp[d.temp$oev_base == 1e5, ]) + geom_boxplot(aes(x = FWeek_onwk, y = score, group = group, fill = model)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Onset Timing (OEV Base = 1e5)', fill = '') +
  scale_x_continuous(breaks = -8:4)
print(p1); print(p2)
# in most countries, ranges are still wider for the network models

# What about only where onset predicted?:
d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot' & !is.na(d$leadonset5), ]
d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
d.agg <- aggregate(score ~ FWeek_onwk + oev_base + model + country, data = d.temp, FUN = median)

p5 <- ggplot(data = d.agg[d.agg$oev_base == 1e4, ], aes(x = FWeek_onwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Onset Timing (OEV base = 1e4)')
p6 <- ggplot(data = d.agg[d.agg$oev_base == 1e5, ], aes(x = FWeek_onwk, y = score, col = model)) + geom_line() + geom_point() + theme_bw() +
  facet_wrap(~ country, scales = 'free_y') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:4) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = '', title = 'Onset Timing (OEV base = 1e5)')
print(p5); print(p6)
# network does tend to have earlier predictions, but not a giant shift in accuracy post-onset






