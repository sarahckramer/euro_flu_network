
# Read in metrics files:
# m1 <- read.csv('results/network/RED_outputMet_pro_PROC.csv')
# m2 <- read.csv('results/isolated/RED_outputMet_pro_PROC.csv')
# same length - good!

m1 <- read.csv('results/network/outputMet_pro_PROC.csv')
m2 <- read.csv('results/isolated/outputMet_pro_PROC.csv')

# Combine files:
m1$model <- 'Network'; m2$model <- 'Isolated'
m <- rbind(m1, m2); rm(m1, m2)

# Remove where no observed onset:
m <- m[!is.na(m$onsetObs5), ]

# Calculate absolute errors:
m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

# Reduce data frame:
m <- m[, c(1:2, 7:8, 12, 38, 45, 50, 56:61)]

pdf('results/plots/MAE_022420_removeNoOnsets.pdf', width = 12, height = 8)

# Plot COMBINED by PREDICTED lead weeks:
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 5 & !is.na(m$leadonset5), ]

m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + model, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ leadonset5 + model, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('leadpkwk_mean', 'model'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('leadpkwk_mean', 'model'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('leadonset5', 'model'))

names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'abs_err', 'count')
m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'

m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)

m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])

# p1 <- ggplot(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_wrap(~ metric, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(breaks = c(10, 100, 300, 900), labels = c(10, 100, 300, 900),
#                         limits = c(0, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
m.agg1 <- m.agg

# # COMBINED by OBSERVED (remove where no onset):
# m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$leadonset5), ]
# m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5 & !is.na(m$leadonset5), ]
# 
# m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model, data = m.temp, FUN = mean)
# m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model, data = m.temp, FUN = mean)
# m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model, data = m.temp2, FUN = mean)
# 
# m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model, data = m.temp, FUN = length)
# m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model, data = m.temp, FUN = length)
# m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + model, data = m.temp2, FUN = length)
# 
# m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'model'))
# m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'model'))
# m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'model'))
# 
# names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'abs_err', 'count')
# m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'
# 
# m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
# rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)
# 
# m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
# m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])
# 
# # Plot by predicted and observed:
# m.agg$method <- 'obs'; m.agg1$method <- 'pred'
# m.agg <- rbind(m.agg1, m.agg); rm(m.agg1)
# m.agg$method <- factor(m.agg$method)
# m.agg$method <- factor(m.agg$method, levels = levels(m.agg$method)[2:1])
# 
# p1 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
#   geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
#   theme_classic() + theme(aspect.ratio = 1,
#                           legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_blank(),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12),
#                           strip.background = element_blank()) +
#   facet_wrap(method ~ metric, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(0, 900), range = c(1,6)) +
#   labs(x = 'Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F'),
#                        metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing'),
#                        method = c(rep('pred', 3), rep('obs', 3)),
#                        y = c(3.5, 56, 5.4, 2.6, 49, 3.89))
# print(p1 + geom_text(data = dat.text, mapping = aes(x = 3.5, y = y, label = label), size = 8))
# m.agg.soFar <- m.agg

# COMBINED by OBSERVED (include ALL forecasts):
m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5, ]
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ] # no mean prediction for those with no prediction - still have to remove NAs

m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group) # 5435 levels = 54350/5/2
levels.to.remove <- c()
for (ix in levels(m.temp$group)) {
  d.check <- m.temp[m.temp$group == ix, ]
  if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
    levels.to.remove <- c(levels.to.remove, ix)
  }
}; rm(d.check)
m.temp <- m.temp[!(m.temp$group %in% levels.to.remove), ]
m.temp <- m.temp[!is.na(m.temp$leadonset5), ] # also remove where no onset predicted

m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group) # 5435 levels = 54350/5/2
levels.to.remove <- c()
for (ix in levels(m.temp2$group)) {
  d.check <- m.temp2[m.temp2$group == ix, ]
  if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
    levels.to.remove <- c(levels.to.remove, ix)
  }
}; rm(d.check)
m.temp2 <- m.temp2[!(m.temp2$group %in% levels.to.remove), ]
m.temp2 <- m.temp2[!is.na(m.temp2$leadonset5), ] # also remove where no onset predicted

m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + model, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'model'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'model'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'model'))

# names(m.pt.agg) = names(m.pi.agg) = c('leadweek', 'model', 'abs_err', 'count')
names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'abs_err', 'count')
m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'

m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)

m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])

# m.agg$method <- 'obs_all'
# m.agg <- rbind(m.agg.soFar, m.agg)
m.agg$method <- 'obs'; m.agg1$method <- 'pred'
m.agg <- rbind(m.agg1, m.agg); rm(m.agg1)
m.agg$method <- factor(m.agg$method)
m.agg$method <- factor(m.agg$method, levels = levels(m.agg$method)[2:1])

# p2 <- ggplot(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_classic() + theme(aspect.ratio = 1,
#                           legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_blank(),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12),
#                           strip.background = element_blank()) +
#   facet_wrap(~ metric, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(breaks = c(10, 100, 300, 900), labels = c(10, 100, 300, 900),
#                         limits = c(0, 900), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)

p1 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
  geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
  theme_classic() + theme(aspect.ratio = 1,
                          legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12),
                          strip.background = element_blank()) +
  facet_wrap(method ~ metric, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(0, 900), range = c(1,6)) +
  labs(x = 'Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F'),
                       metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing'),
                       method = c(rep('pred', 3), rep('obs', 3)),
                       y = c(3.5, 56, 5.4, 2.6, 49, 5.8))
print(p1 + geom_text(data = dat.text, mapping = aes(x = 3.5, y = y, label = label), size = 8))

# # Friedman test:
# m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5, ]
# m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ] # no mean prediction for those with no prediction - still have to remove NAs
# 
# m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp$group)) {
#   d.check <- m.temp[m.temp$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp <- m.temp[!(m.temp$group %in% levels.to.remove), ]
# m.temp <- m.temp[!is.na(m.temp$leadonset5), ] # also remove where no onset predicted
# 
# m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp2$group)) {
#   d.check <- m.temp2[m.temp2$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp2 <- m.temp2[!(m.temp2$group %in% levels.to.remove), ]
# m.temp2 <- m.temp2[!is.na(m.temp2$leadonset5), ] # also remove where no onset predicted
# 
# p1 <- ggplot(data = m.temp, aes(x = FWeek_pkwk, y = abs_err_pkwk, fill = model, group = paste(FWeek_pkwk, model, '_'))) + geom_boxplot() + theme_classic() +
#   scale_x_continuous(breaks = -6:4) + scale_fill_brewer(palette = 'Set2')
# p2 <- ggplot(data = m.temp, aes(x = FWeek_pkwk, y = abs_err_int_perc, fill = model, group = paste(FWeek_pkwk, model, '_'))) + geom_boxplot() + theme_classic() +
#   scale_x_continuous(breaks = -6:4) + scale_fill_brewer(palette = 'Set2')
# p3 <- ggplot(data = m.temp2, aes(x = FWeek_onwk, y = abs_err_onset, fill = model, group = paste(FWeek_onwk, model, '_'))) + geom_boxplot() + theme_classic() +
#   scale_x_continuous(breaks = -6:4) + scale_fill_brewer(palette = 'Set2')
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group)
# m.temp$group2 <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, m.temp$model, sep = '_'); m.temp$group2 <- factor(m.temp$group2)
# 
# m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group)
# m.temp2$group2 <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, m.temp2$model, sep = '_'); m.temp2$group2 <- factor(m.temp2$group2)
# 
# # Function to select only ONE representative from each level of group2:
# permute.by.run <- function(dat) {
#   dat.a <- split(dat, dat$group2)
#   dat.b <- lapply(dat.a, function(ix) {
#     ix[sample(dim(ix)[1], 1), ]
#   })
#   dat.c <- do.call(rbind, dat.b)
#   rownames(dat.c) <- NULL
#   return(dat.c)
# }
# 
# set.seed(1089437584)
# p.vals <- c()
# net.to.isol.rat <- c()
# for (i in 1:100) {
#   m.red <- permute.by.run(m.temp)
#   m.red$group <- factor(m.red$group)
#   p.vals <- c(p.vals, friedmanTest(m.red$abs_err_pkwk, m.red$model, m.red$group, dist = 'FDist')$p.value)
#   
#   network.better = isol.better = 0
#   for (block in levels(m.red$group)) {
#     if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_pkwk'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_pkwk']) {
#       network.better <- network.better + 1
#     } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_pkwk'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_pkwk']) {
#       isol.better <- isol.better + 1
#     }
#   }
#   net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#   
# }
# print(median(p.vals)) # 1.12718e-08
# print(summary(net.to.isol.rat)) # network always WORSE
# 
# set.seed(1089437584)
# p.vals <- c()
# net.to.isol.rat <- c()
# for (i in 1:100) {
#   m.red <- permute.by.run(m.temp)
#   m.red$group <- factor(m.red$group)
#   p.vals <- c(p.vals, friedmanTest(m.red$abs_err_int_perc, m.red$model, m.red$group, dist = 'FDist')$p.value)
#   
#   network.better = isol.better = 0
#   for (block in levels(m.red$group)) {
#     if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_int_perc'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_int_perc']) {
#       network.better <- network.better + 1
#     } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_int_perc'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_int_perc']) {
#       isol.better <- isol.better + 1
#     }
#   }
#   net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#   
# }
# print(median(p.vals)) # 0.683
# print(summary(net.to.isol.rat)) # median is 1 - network better about half the time
# 
# set.seed(1089437584)
# p.vals <- c()
# net.to.isol.rat <- c()
# for (i in 1:100) {
#   m.red <- permute.by.run(m.temp2)
#   m.red$group <- factor(m.red$group)
#   p.vals <- c(p.vals, friedmanTest(m.red$abs_err_onset, m.red$model, m.red$group, dist = 'FDist')$p.value)
#   
#   network.better = isol.better = 0
#   for (block in levels(m.red$group)) {
#     if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_onset'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_onset']) {
#       network.better <- network.better + 1
#     } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_onset'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_onset']) {
#       isol.better <- isol.better + 1
#     }
#   }
#   net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#   
# }
# print(median(p.vals)) # 0.002695287
# print(summary(net.to.isol.rat)) # isolated always better?
# # there are 726 comparisons, but many (>500) of them are equal, which means neither better
# 
# # b/c discrete, there are a lot of ties - not sure how trustworthy these are; for onset almost entirely due to post-peak stuff

# By Subtype (by PREDICTED):
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 5 & !is.na(m$leadonset5), ]

m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + subtype, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + subtype, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + model + subtype, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + subtype, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + subtype, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ leadonset5 + model + subtype, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('leadpkwk_mean', 'model', 'subtype'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('leadpkwk_mean', 'model', 'subtype'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('leadonset5', 'model', 'subtype'))

names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'subtype', 'abs_err', 'count')
m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'

m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)

m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])

p3 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
  geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(metric ~ subtype, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(0, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                       subtype = c('A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B'),
                       metric = c(rep('Peak Timing', 3), rep('Peak Intensity', 3), rep('Onset Timing', 3)),
                       y = c(rep(5.5, 3), rep(63, 3), rep(8.3, 3)))
print(p3 + geom_text(data = dat.text, mapping = aes(x = 3.5, y = y, label = label), size = 8))

# by observed (include all):
m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5, ]
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ] # no mean prediction for those with no prediction - still have to remove NAs

m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group) # 5435 levels = 54350/5/2
levels.to.remove <- c()
for (ix in levels(m.temp$group)) {
  d.check <- m.temp[m.temp$group == ix, ]
  if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
    levels.to.remove <- c(levels.to.remove, ix)
  }
}; rm(d.check)
m.temp <- m.temp[!(m.temp$group %in% levels.to.remove), ]
m.temp <- m.temp[!is.na(m.temp$leadonset5), ] # also remove where no onset predicted

m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group) # 5435 levels = 54350/5/2
levels.to.remove <- c()
for (ix in levels(m.temp2$group)) {
  d.check <- m.temp2[m.temp2$group == ix, ]
  if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
    levels.to.remove <- c(levels.to.remove, ix)
  }
}; rm(d.check)
m.temp2 <- m.temp2[!(m.temp2$group %in% levels.to.remove), ]
m.temp2 <- m.temp2[!is.na(m.temp2$leadonset5), ] # also remove where no onset predicted

m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + subtype, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + subtype, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + subtype, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + subtype, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + subtype, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + model + subtype, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'model', 'subtype'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'model', 'subtype'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'model', 'subtype'))

names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'subtype', 'abs_err', 'count')
m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'

m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)

m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])

p4 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
  geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(metric ~ subtype, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(0, 400), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p4)
# dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
#                        subtype = c('A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B'),
#                        metric = c(rep('Peak Timing', 3), rep('Peak Intensity', 3), rep('Onset Timing', 3)),
#                        y = c(rep(5.5, 3), rep(63, 3), rep(8.3, 3)))
# print(p3 + geom_text(data = dat.text, mapping = aes(x = 3.5, y = y, label = label), size = 8))

dev.off()

# # Friedman test:
# m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5, ]
# m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ] # no mean prediction for those with no prediction - still have to remove NAs
# 
# m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp$group)) {
#   d.check <- m.temp[m.temp$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp <- m.temp[!(m.temp$group %in% levels.to.remove), ]
# m.temp <- m.temp[!is.na(m.temp$leadonset5), ] # also remove where no onset predicted
# 
# m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp2$group)) {
#   d.check <- m.temp2[m.temp2$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp2 <- m.temp2[!(m.temp2$group %in% levels.to.remove), ]
# m.temp2 <- m.temp2[!is.na(m.temp2$leadonset5), ] # also remove where no onset predicted
# 
# m.temp$group2 <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, m.temp$model, sep = '_'); m.temp$group2 <- factor(m.temp$group2)
# m.temp2$group2 <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, m.temp2$model, sep = '_'); m.temp2$group2 <- factor(m.temp2$group2)
# 
# m.temp.orig <- m.temp
# m.temp.orig2 <- m.temp2
# 
# par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (subtype in levels(m.temp$subtype)) {
#   print(subtype)
#   
#   m.temp <- m.temp.orig[m.temp.orig$subtype == subtype, ]
#   m.temp2 <- m.temp.orig2[m.temp.orig2$subtype == subtype, ]
#   
#   m.temp$group <- factor(m.temp$group); m.temp$group2 <- factor(m.temp$group2)
#   m.temp2$group <- factor(m.temp2$group); m.temp2$group2 <- factor(m.temp2$group2)
#   
#   set.seed(1089437584)
#   p.vals <- c()
#   net.to.isol.rat <- c()
#   for (i in 1:100) {
#     m.red <- permute.by.run(m.temp)
#     m.red$group <- factor(m.red$group)
#     p.vals <- c(p.vals, friedmanTest(m.red$abs_err_pkwk, m.red$model, m.red$group, dist = 'FDist')$p.value)
#     
#     network.better = isol.better = 0
#     for (block in levels(m.red$group)) {
#       if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_pkwk'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_pkwk']) {
#         network.better <- network.better + 1
#       } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_pkwk'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_pkwk']) {
#         isol.better <- isol.better + 1
#       }
#     }
#     net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#     
#   }
#   print(median(p.vals))
#   print(summary(net.to.isol.rat))
#   hist(p.vals); hist(net.to.isol.rat)
#   
#   set.seed(1089437584)
#   p.vals <- c()
#   net.to.isol.rat <- c()
#   for (i in 1:100) {
#     m.red <- permute.by.run(m.temp)
#     m.red$group <- factor(m.red$group)
#     p.vals <- c(p.vals, friedmanTest(m.red$abs_err_int_perc, m.red$model, m.red$group, dist = 'FDist')$p.value)
#     
#     network.better = isol.better = 0
#     for (block in levels(m.red$group)) {
#       if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_int_perc'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_int_perc']) {
#         network.better <- network.better + 1
#       } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_int_perc'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_int_perc']) {
#         isol.better <- isol.better + 1
#       }
#     }
#     net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#     
#   }
#   print(median(p.vals))
#   print(summary(net.to.isol.rat))
#   hist(p.vals); hist(net.to.isol.rat)
#   
#   set.seed(1089437584)
#   p.vals <- c()
#   net.to.isol.rat <- c()
#   for (i in 1:100) {
#     m.red <- permute.by.run(m.temp2)
#     m.red$group <- factor(m.red$group)
#     p.vals <- c(p.vals, friedmanTest(m.red$abs_err_onset, m.red$model, m.red$group, dist = 'FDist')$p.value)
#     
#     network.better = isol.better = 0
#     for (block in levels(m.red$group)) {
#       if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_onset'] < m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_onset']) {
#         network.better <- network.better + 1
#       } else if (m.red[m.red$group == block & m.red$model == 'Network', 'abs_err_onset'] > m.red[m.red$group == block & m.red$model == 'Isolated', 'abs_err_onset']) {
#         isol.better <- isol.better + 1
#       }
#     }
#     net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
#     
#   }
#   print(median(p.vals))
#   print(summary(net.to.isol.rat))
#   hist(p.vals); hist(net.to.isol.rat)
# }
# # A(H1): 0.01272059, isolated better; 0.007750326, isolated better; 0.03465086, isolated better
# # A(H3): 0.0001090133, isolated better; 0.06726061, network better; 0.003081804, isolated better
# # B: 0.00029691, isolated better; 0.3946375, network better; 0.6697305, depends

# ### Plot by country:
# pdf('results/plots/MAE_022420_byCountry.pdf', width = 22, height = 8)
# 
# m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
# m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 5 & !is.na(m$leadonset5), ]
# 
# m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + country, data = m.temp, FUN = mean)
# m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + country, data = m.temp, FUN = mean)
# m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + model + country, data = m.temp2, FUN = mean)
# 
# m.pt.agg.c <- aggregate(abs_err_pkwk ~ leadpkwk_mean + model + country, data = m.temp, FUN = length)
# m.pi.agg.c <- aggregate(abs_err_int_perc ~ leadpkwk_mean + model + country, data = m.temp, FUN = length)
# m.ot.agg.c <- aggregate(abs_err_onset ~ leadonset5 + model + country, data = m.temp2, FUN = length)
# 
# m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('leadpkwk_mean', 'model', 'country'))
# m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('leadpkwk_mean', 'model', 'country'))
# m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('leadonset5', 'model', 'country'))
# 
# names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'country', 'abs_err', 'count')
# m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'
# 
# m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
# rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)
# 
# m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
# m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])
# 
# p5 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
#   geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
#   theme_classic() + theme(legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_text(size = 12),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12)) +
#   facet_grid(metric ~ country, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(0, 400), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p5)
# 
# # by observed (include all):
# m.temp <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5, ]
# m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ] # no mean prediction for those with no prediction - still have to remove NAs
# 
# m.temp$group <- paste(m.temp$season, m.temp$country, m.temp$FWeek_pkwk, m.temp$subtype, sep = '_'); m.temp$group <- factor(m.temp$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp$group)) {
#   d.check <- m.temp[m.temp$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp <- m.temp[!(m.temp$group %in% levels.to.remove), ]
# m.temp <- m.temp[!is.na(m.temp$leadonset5), ] # also remove where no onset predicted
# 
# m.temp2$group <- paste(m.temp2$season, m.temp2$country, m.temp2$FWeek_onwk, m.temp2$subtype, sep = '_'); m.temp2$group <- factor(m.temp2$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(m.temp2$group)) {
#   d.check <- m.temp2[m.temp2$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }; rm(d.check)
# m.temp2 <- m.temp2[!(m.temp2$group %in% levels.to.remove), ]
# m.temp2 <- m.temp2[!is.na(m.temp2$leadonset5), ] # also remove where no onset predicted
# 
# m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + country, data = m.temp, FUN = mean)
# m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + country, data = m.temp, FUN = mean)
# m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + model + country, data = m.temp2, FUN = mean)
# 
# m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + model + country, data = m.temp, FUN = length)
# m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + model + country, data = m.temp, FUN = length)
# m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + model + country, data = m.temp2, FUN = length)
# 
# m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'model', 'country'))
# m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'model', 'country'))
# m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'model', 'country'))
# 
# names(m.pt.agg) = names(m.pi.agg) = names(m.ot.agg) = c('leadweek', 'model', 'country', 'abs_err', 'count')
# m.pt.agg$metric <- 'Peak Timing'; m.pi.agg$metric <- 'Peak Intensity'; m.ot.agg$metric <- 'Onset Timing'
# 
# m.agg <- rbind(m.pt.agg, m.pi.agg, m.ot.agg)
# rm(m.pt.agg, m.pt.agg.c, m.pi.agg, m.pi.agg.c, m.ot.agg, m.ot.agg.c, m.temp, m.temp2)
# 
# m.agg$metric <- factor(m.agg$metric); m.agg$metric <- factor(m.agg$metric, levels = levels(m.agg$metric)[3:1])
# m.agg$model <- factor(m.agg$model); m.agg$model <- factor(m.agg$model, levels = levels(m.agg$model)[2:1])
# 
# p6 <- ggplot() + geom_line(data = m.agg, aes(x = leadweek, y = abs_err, col = model)) +
#   geom_point(data = m.agg, aes(x = leadweek, y = abs_err, col = model, size = count)) +
#   theme_classic() + theme(legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_text(size = 12),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12)) +
#   facet_grid(metric ~ country, scales = 'free') + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(0, 400), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'MAE/MAPE', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p6)
# 
# dev.off()

rm(list = ls())




