
# Read in log scores:
# d1 <- read.csv('results/network/RED_PROC_logScores_pt_ot.csv')
# d2 <- read.csv('results/network/RED_PROC_logScores_pi_bin.csv')
# 
# d1.isol <- read.csv('results/isolated/RED_logScores_pt_ot.csv')
# d2.isol <- read.csv('results/isolated/RED_logScores_pi_bin.csv')

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

# pdf('results/plots/logScores_noRemoval.pdf', width = 16, height = 9)

# First plot COMBINED by PREDICTED LEAD WEEK:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = median)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric')); rm(d.agg.count)
# names(d.agg)[4:5] <- c('score', 'count')
# 
# p2 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Median Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# # print(p2)
# 
# grid.arrange(p1, p2)

# COMBINED by OBSERVED (include ALL forecasts):
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)

d.agg <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = median)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Median Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)

grid.arrange(p1, p2)

# Breakdown by SUBTYPE:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'subtype')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(subtype~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'subtype')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(subtype ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5.5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p2)

# dev.off()

# # Breakdown by DATA TYPE:
# # Need info on this merged first
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'subtype')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(subtype~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 400), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# d.agg <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'subtype')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(subtype ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5.5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 400), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)

# Breakdown by SEASON:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'season')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(season~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg <- aggregate(score ~ FWeek + model + metric + season, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric + season, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'season')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(season ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-7, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p2)

# Do country plots in another file, but quick check:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'country')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(country~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg <- aggregate(score ~ FWeek + model + metric + country, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric + country, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'country')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(country ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p2)

ggplot(data = d.agg[d.agg$metric == 'Onset Timing', ], aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() + facet_wrap(~ country)
ggplot(data = d.agg[d.agg$metric == 'Onset Timing', ], aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() + facet_wrap(~ country)





# Others?
    # 1-4 weeks, by predicted and observed_all?
    # 




