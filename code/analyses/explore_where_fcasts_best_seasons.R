
# Read in log scores:
d1 <- read.csv('results/network/PROC_logScores_pt_ot.csv')
d2 <- read.csv('results/network/PROC_logScores_pi_bin.csv')

d1.isol <- read.csv('results/isolated/logScores_pt_ot.csv')
d2.isol <- read.csv('results/isolated/logScores_pi_bin.csv')

# Combine files:
d1$model <- 'Network'; d2$model <- 'Network'; d1.isol$model <- 'Isolated'; d2.isol$model <- 'Isolated'
d1 <- rbind(d1, d1.isol); d2 <- rbind(d2, d2.isol); rm(d1.isol, d2.isol)

d1 <- d1[, c(1:4, 8, 10:17)]
d2 <- d2[, c(1:4, 8, 10:17)]

d <- rbind(d1, d2); rm(d1, d2)

# Rename leads:
d.ot <- d[d$metric == 'onset5', ]
d <- d[d$metric != 'onset5', ]

d <- d[, c(1:8, 10:13)]
d.ot <- d.ot[, c(1:7, 9:13)]
d.ot$leadpkwk_mean <- d.ot$leadonset5

names(d)[7:8] = names(d.ot)[7:8] = c('lead_mean', 'FWeek')
d <- rbind(d, d.ot); rm(d.ot)

d$metric <- factor(d$metric); d$model <- factor(d$model)
levels(d$metric) <- c('Onset Timing', 'Peak Timing', 'Peak Intensity')
d$metric <- factor(d$metric, levels = levels(d$metric)[c(2:3, 1)])
d$model <- factor(d$model, levels = levels(d$model)[2:1])

# Plot log scores by season:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'season')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

d.agg <- d.agg[d.agg$metric != 'Onset Timing', ]

p1 <- ggplot() + geom_line(data = d.agg, aes(x = lead_mean, y = score, col = model)) +
  geom_point(data = d.agg, aes(x = lead_mean, y = score, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(metric ~ season, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

# Plot log scores by season/subtype:
d.agg <- aggregate(score ~ lead_mean + model + metric + season + subtype, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + season + subtype, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'season', 'subtype')); rm(d.agg.count)
names(d.agg)[6:7] <- c('score', 'count')

d.agg <- d.agg[d.agg$metric != 'Onset Timing', ]

p2 <- ggplot() + geom_line(data = d.agg[d.agg$metric == 'Peak Timing', ], aes(x = lead_mean, y = score, col = model)) +
  geom_point(data = d.agg[d.agg$metric == 'Peak Timing', ], aes(x = lead_mean, y = score, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(subtype ~ season, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p2)

p3 <- ggplot() + geom_line(data = d.agg[d.agg$metric == 'Peak Intensity', ], aes(x = lead_mean, y = score, col = model)) +
  geom_point(data = d.agg[d.agg$metric == 'Peak Intensity', ], aes(x = lead_mean, y = score, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(subtype ~ season, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p3)

# Compare network log scores by season/subtype:
p4 <- ggplot() + geom_line(data = d.agg[d.agg$model == 'Network', ], aes(x = lead_mean, y = score, col = subtype)) +
  geom_point(data = d.agg[d.agg$model == 'Network', ], aes(x = lead_mean, y = score, col = subtype, size = count)) +
  geom_hline(yintercept = -4) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(season ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p4)

p5 <- ggplot() + geom_line(data = d.agg[d.agg$model == 'Network', ], aes(x = lead_mean, y = score, col = subtype, group = paste(season, subtype))) +
  geom_point(data = d.agg[d.agg$model == 'Network', ], aes(x = lead_mean, y = score, col = subtype, group = paste(season, subtype), size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p5)
# some evidence for H3 coming out as the worst, esp for PI post-peak; H1 looks pretty good for PI but wouldn't say it's definitively the best
# 

# Does it seems to matter where NAs are occurring? Or how many NAs occur? Is there a pattern there?

# Look at range of PIs for the different seasons/subtypes - does this impact anything?
# I guess this would be PIs just where onsets occur?
# Or could just eyeball it using fit plots...

# For country-level thoughts:
# no obvious impact of longitude, latitude, net incoming commuters, total commuter flow
# check: total commuter flow / pop size? pop size? pop density? smoothness? missingness? how early onset/peak? size of scaled PI? country area?
