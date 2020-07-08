
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

# First plot COMBINED by PREDICTED LEAD WEEK:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model), size = 3) +
  theme_classic() + theme(aspect.ratio = 1,
                          legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12),
                          strip.background = element_blank()) +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
  # scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
  #                       limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:')
  # guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
dat.text <- data.frame(label = c('A', 'B', 'C'), metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing'))
p1 <- p1 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8)
# print(p1)

ggsave('results/plots/Fig3.svg', plot = p1, width = 11, height = 7.5)

# # Info for Table 1:
# d.agg[d.agg$model == 'Network' & d.agg$metric == 'Peak Timing', ]
# d.agg[d.agg$model == 'Network' & d.agg$metric == 'Onset Timing', ]
# d.agg[d.agg$model == 'Isolated' & d.agg$metric == 'Peak Timing', 'count']
# d.agg[d.agg$model == 'Isolated' & d.agg$metric == 'Onset Timing', 'count']

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# # Get number of forecasts by observed lead week, and how many "removed:"
# # forecasts with no OBSERVED onset have already been removed by this point
# m <- read.csv('results/network/outputMet_pro_PROC.csv')
# # m <- unique(m[, c(1, 8, 30, 38, 45, 57)])
# m <- unique(m[, c(1, 8, 30, 57)])
# 
# # So we can't count those left out for not having an observed onset by lead week, but we can look at season-country-subtype combos, and say how many total removed?
# # out of 198 season-country-subtype combinations in our data (some countries missing data entirely for a season), 29 combos had no observed onset (14.65%)
# rm(m)

# COMBINED by OBSERVED (still removing no onsets):
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

# COMBINED by OBSERVED (include ALL forecasts):
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg3 <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg3 <- merge(d.agg3, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg3)[4:5] <- c('score', 'count')

# For Friedman, remove where no runs of a group predict an onset for EITHER network or isolated:
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# d.temp <- d.temp[d.temp$score > -10, ] # FOR COMPARISON
d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, d.temp$metric, sep = '_'); d.temp$group <- factor(d.temp$group) # 5434 levels = 54340/5/2

######################################################
# # For S2 Table:
# d.unique <- unique(d.temp[, c(1:2, 5, 8, 11:13)])
# d.unique$group <- factor(d.unique$group) # still 5434
# 
# table(d.unique$FWeek, d.unique$metric, d.unique$model)
# 
# # see below: 2021 levels to remove; not sure which are which metrics, though
# 
# d.unique1 <- d.unique[!(d.unique$group %in% levels.to.remove), ]
# d.unique1$group <- factor(d.unique1$group)
# 
# d.unique2 <- unique(d.temp[, c(1:2, 5, 8, 11:13)])
# d.unique2$group <- factor(d.unique2$group)
# 
# summary(levels(d.unique1$group) == levels(d.unique2$group)) # all true
# rm(d.unique2)
# 
# table(d.unique1$FWeek, d.unique1$metric, d.unique1$model)
# 
# pk.start <- c(164, 164, 166, 167, 167, 163, 169, 167, 164, 167, 167)
# pk.end <- c(39, 58, 83, 106, 124, 138, 162, 160, 157, 158, 158)
# 
# on.start <- c(148, 156, 162, 159, 165, 152, 169, 169, 169, 166, 169)
# on.end <- c(1, 5, 2, 14, 15, 4, 70, 144, 158, 156, 158)
# 
# print(pk.start - pk.end)
# print(pk.end / pk.start * 100)
# 
# print(on.start - on.end)
# print(on.end / on.start * 100)
######################################################

levels.to.remove <- c()
for (ix in levels(d.temp$group)) {
  d.check <- d.temp[d.temp$group == ix, ]
  if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
    levels.to.remove <- c(levels.to.remove, ix)
  }
}
d.temp <- d.temp[!(d.temp$group %in% levels.to.remove), ]
d.temp <- d.temp[!is.na(d.temp$leadonset5), ] # also remove where no onset predicted
d.agg2 <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg2 <- merge(d.agg2, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg2)[4:5] <- c('score', 'count')

# Plot (combined):
d.agg$method <- 'standard'
d.agg2$method <- 'exclude'
d.agg3$method <- 'include'
d.agg <- rbind(d.agg, d.agg2, d.agg3); rm(d.agg2); rm(d.agg3)
d.agg$method <- factor(d.agg$method)
d.agg$method <- factor(d.agg$method, levels = levels(d.agg$method)[c(3, 1, 2)])

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
  theme_classic() + theme(aspect.ratio = 1,
                          legend.text = element_text(size = 14),
                          axis.text = element_text(size = 10),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 14),
                          legend.title = element_text(size = 14),
                          strip.background = element_blank()) +
  facet_grid(method ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                       metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing'),
                       method = c(rep('standard', 3), rep('exclude', 3), rep('include', 3)))
p2 <- p2 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8)
# print(p2)
ggsave(file = 'results/plots/FigS5.svg', plot = p2, width = 12, height = 10)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# ### FRIEDMAN TESTS ###
# # d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# 
# # ggplot(data = d.temp, aes(x = FWeek, y = score, fill = model, group = paste(FWeek, model, '_'))) + geom_boxplot() + facet_wrap(~ metric) +
# #   theme_classic() + scale_x_continuous(breaks = -6:4) + scale_y_continuous(breaks = -10:0) + scale_fill_brewer(palette = 'Set2')
# # # obviously a lot of overlap - I think the question is, how often is one better than the other?
# # # this seems to require pairing to some extent, though - so pair by runs?
# 
# d.temp <- d.temp[, c(1:2, 4:5, 8:12)]
# d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, sep = '_'); d.temp$group <- factor(d.temp$group)
# d.temp$group2 <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, d.temp$model, sep = '_'); d.temp$group2 <- factor(d.temp$group2)
# 
# d.pt <- d.temp[d.temp$metric == 'Peak Timing', ]
# d.pt$metric <- NULL
# 
# d.pi <- d.temp[d.temp$metric == 'Peak Intensity', ]
# d.pi$metric <- NULL
# 
# d.ot <- d.temp[d.temp$metric == 'Onset Timing', ]
# d.ot$metric <- NULL
# 
# # Find a way to draw a random run for each:
# d.pt <- d.pt[order(d.pt$group, d.pt$model), ]
# rownames(d.pt) <- 1:dim(d.pt)[1]
# 
# d.pi <- d.pi[order(d.pi$group, d.pi$model), ]
# rownames(d.pi) <- 1:dim(d.pi)[1]
# 
# d.ot <- d.ot[order(d.ot$group, d.ot$model), ]
# rownames(d.ot) <- 1:dim(d.ot)[1]
# 
# # Keep only levels relevant for each metric:
# d.pt$group <- factor(d.pt$group); d.pt$group2 <- factor(d.pt$group2)
# d.pi$group <- factor(d.pi$group); d.pi$group2 <- factor(d.pi$group2)
# d.ot$group <- factor(d.ot$group); d.ot$group2 <- factor(d.ot$group2)
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
# # Now perform Friedman tests:
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   if (i%%10 == 0) {
#     print(i)
#   }
#   d.pt.red <- permute.by.run(d.pt)
#   d.pt.red$group <- factor(d.pt.red$group)
#   p.vals <- c(p.vals, friedmanTest(d.pt.red$score, d.pt.red$model, d.pt.red$group, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.0482865; 0.247285 if no -10s; 0.007969452 if all included
# # Old: 0.04070164; 0.2610895 if no -10s; 0.003365633 if all included
# 
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   if (i%%10 == 0) {
#     print(i)
#   }
#   d.pi.red <- permute.by.run(d.pi)
#   d.pi.red$group <- factor(d.pi.red$group)
#   p.vals <- c(p.vals, friedmanTest(d.pi.red$score, d.pi.red$model, d.pi.red$group, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.2954887; 0.2779738 if no -10s; 0.00337134 if all included
# # Old: 0.02154314; 0.07148775 if no -10s; 5.053524e-05 if all included
# 
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   if (i%%10 == 0) {
#     print(i)
#   }
#   d.ot.red <- permute.by.run(d.ot)
#   d.ot.red$group <- factor(d.ot.red$group)
#   p.vals <- c(p.vals, friedmanTest(d.ot.red$score, d.ot.red$model, d.ot.red$group, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.1608506; 0.2674348 if no -10s; 0.1631202 if all included
# # Old: 0.1247537; 0.1636237 if no -10s; 0.1012045 if all included
# 
# 
# # Check direction of relationship (which is better?):
# network.better = isol.better = 0
# for (block in levels(d.pt$group)) {
#   if (mean(d.pt[d.pt$group == block & d.pt$model == 'Network', 'score']) > mean(d.pt[d.pt$group == block & d.pt$model == 'Isolated', 'score'])) {
#     network.better <- network.better + 1
#   } else if (mean(d.pt[d.pt$group == block & d.pt$model == 'Network', 'score']) < mean(d.pt[d.pt$group == block & d.pt$model == 'Isolated', 'score'])) {
#     isol.better <- isol.better + 1
#   }
# }
# # network 609, isolated 721
# 
# network.better = isol.better = 0
# for (block in levels(d.pi$group)) {
#   if (mean(d.pi[d.pi$group == block & d.pi$model == 'Network', 'score']) > mean(d.pi[d.pi$group == block & d.pi$model == 'Isolated', 'score'])) {
#     network.better <- network.better + 1
#   } else if (mean(d.pi[d.pi$group == block & d.pi$model == 'Network', 'score']) < mean(d.pi[d.pi$group == block & d.pi$model == 'Isolated', 'score'])) {
#     isol.better <- isol.better + 1
#   }
# }
# # network 632, isolated 670
# # so isolated better for both

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# Breakdown by SUBTYPE:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + subtype, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'subtype')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p1 <- ggplot() + geom_line(data = d.agg, aes(x = lead_mean, y = score, col = model)) +
  geom_point(data = d.agg, aes(x = lead_mean, y = score, col = model, size = count)) +
  theme_classic() + theme(legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12)) +
  facet_grid(metric ~ subtype, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                       subtype = c('A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B'),
                       metric = c(rep('Peak Timing', 3), rep('Peak Intensity', 3), rep('Onset Timing', 3)))
p1 <- p1 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8)
print(p1)

ggsave(file = 'results/plots/FigS6.svg', plot = p1, width = 12, height = 8)

# # If we remove no forecasts:
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# d.agg <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'subtype')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_classic() +
#   facet_grid(subtype ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5.5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 400), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# ### Friedman tests (by subtype) ###
# # p = 0.01 / 9 = just use 0.001 to be extra safe
# # wait - for initial tests, just use 0.05 - I don't think it matters a ton
# 
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# # d.temp <- d.temp[d.temp$score > -10, ] # FOR COMPARISON
# d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, d.temp$metric, sep = '_'); d.temp$group <- factor(d.temp$group) # 5435 levels = 54350/5/2
# levels.to.remove <- c()
# for (ix in levels(d.temp$group)) {
#   d.check <- d.temp[d.temp$group == ix, ]
#   if (all(is.na(d.check$leadonset5[d.check$model == 'Network'])) | all(is.na(d.check$leadonset5[d.check$model == 'Isolated']))) {
#     levels.to.remove <- c(levels.to.remove, ix)
#   }
# }
# d.temp <- d.temp[!(d.temp$group %in% levels.to.remove), ]
# d.temp$group <- factor(d.temp$group)
# 
# d.temp <- d.temp[, c(1:2, 4:5, 8:12)]
# d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, sep = '_'); d.temp$group <- factor(d.temp$group)
# d.temp$group2 <- paste(d.temp$season, d.temp$country, d.temp$FWeek, d.temp$subtype, d.temp$model, sep = '_'); d.temp$group2 <- factor(d.temp$group2)
# 
# d.temp <- d.temp[order(d.temp$metric, d.temp$subtype, d.temp$group, d.temp$model), ]
# d.temp <- d.temp[!is.na(d.temp$leadonset5), ] # remove where no onset predicted
# 
# d.agg <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ FWeek + model + metric + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'subtype')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p2 <- ggplot() + geom_line(data = d.agg, aes(x = FWeek, y = score, col = model)) +
#   geom_point(data = d.agg, aes(x = FWeek, y = score, col = model, size = count)) +
#   theme_classic() + theme(legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_text(size = 12),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12)) +
#   facet_grid(metric ~ subtype, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 400), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)
# 
# # dev.off()
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
# # calculate scores:
# par(mfrow = c(3, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (metric in levels(d.temp$metric)) {
#   for (subtype in levels(d.temp$subtype)) {
#     print(paste(metric, subtype, sep = '_'))
# 
#     d.red <- d.temp[d.temp$metric == metric & d.temp$subtype == subtype, ]
#     rownames(d.red) <- 1:dim(d.red)[1]
# 
#     d.red$group <- factor(d.red$group)
#     d.red$group2 <- factor(d.red$group2)
# 
#     set.seed(1089437584)
#     p.vals <- c()
#     for (i in 1:100) {
#       d.red2 <- permute.by.run(d.red)
#       d.red2$group <- factor(d.red2$group)
#       p.vals <- c(p.vals, friedmanTest(d.red2$score, d.red2$model, d.red2$group, dist = 'FDist')$p.value)
#     }
#     hist(p.vals)
#     print(median(p.vals))
#   }
# }
# # PT/H1: 0.523
# # PT/H3: 0.443
# # PT/B: 0.036
# # PI/H1: 0.074
# # PI/H3: 0.612
# # PI/B: 0.625
# # OT/H1: 0.551
# # OT/H3: 0.109
# # OT/B: 0.733

# sig: PT/B
# if we remove where scores==-10, not longer sig (p>0.1)
# 
# d.red <- d.temp[d.temp$metric == 'Peak Timing' & d.temp$subtype == 'B', ]
# rownames(d.red) <- 1:dim(d.red)[1]
# 
# d.red$group <- factor(d.red$group)
# d.red$group2 <- factor(d.red$group2)
# 
# set.seed(1089437584)
# net.to.isol.rat <- c()
# for (i in 1:100) {
#   d.red2 <- permute.by.run(d.red)
#   d.red2$group <- factor(d.red2$group)
#   # p.vals <- c(p.vals, friedmanTest(d.red2$score, d.red2$model, d.red2$group, dist = 'FDist')$p.value)
# 
#   network.better = isol.better = 0
#   for (block in levels(d.red2$group)) {
#     if (d.red2[d.red2$group == block & d.red2$model == 'Network', 'score'] > d.red2[d.red2$group == block & d.red2$model == 'Isolated', 'score']) {
#       network.better <- network.better + 1
#     } else if (d.red2[d.red2$group == block & d.red2$model == 'Network', 'score'] < d.red2[d.red2$group == block & d.red2$model == 'Isolated', 'score']) {
#       isol.better <- isol.better + 1
#     }
#   }
#   net.to.isol.rat <- c(net.to.isol.rat, network.better / isol.better)
# }
# print(summary(net.to.isol.rat))
# # network WORSE - always lower

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# By country:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5) & d$score > -10, ]
d.agg <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'country')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

levels(d.agg$country) <- c('Austria', 'Belgium', 'Czechia', 'Germany', 'Spain', 'France', 'Hungary', 'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia')

p1 <- ggplot(data = d.agg[d.agg$metric != 'Onset Timing', ], aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() + theme(axis.title = element_text(size = 12), strip.text = element_text(size = 12)) +
  facet_grid(metric ~ country, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-8.5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

ggsave(file = 'results/plots/FigS7.svg', plot = p1, width = 22, height = 4.5)

# ggplot(data = d.agg[d.agg$metric == 'Onset Timing', ], aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() + facet_wrap(~ country, nrow = 2)

dev.off()
rm(list = ls())





