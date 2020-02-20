
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

# pdf('results/plots/logScores_021820.pdf', width = 12, height = 8)

# First plot COMBINED by PREDICTED LEAD WEEK:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
  theme_classic() + theme(aspect.ratio = 1,
                          legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12),
                          strip.background = element_blank()) +
  facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
dat.text <- data.frame(label = c('A', 'B', 'C'), metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing'))
print(p1 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8))

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# # Explore why PT worse for network:
# boxplot(score ~ model + lead_mean, data = d.temp[d.temp$metric == 'Peak Timing', ])
# # note that only the mean, not the median, is lower - suggests that there are an abundance of "failures"
# # and gets worse with increasing lead week
# 
# d.temp <- d.temp[d.temp$metric == 'Peak Timing', ]
# 
# d.temp[d.temp$score == -10 & d.temp$season == '2012-13', ] # actually pretty few, but mostly network
# # so is it not necessarily that there are a lot of -10s, just that there are a lot of really low values in general for the network?
# # for network, mostly HU and LU - could be missing observed peaks b/c of noisiness?
# 
# # remove LU and look again:
# d.noLU <- d.temp[d.temp$country != 'LU', ]
# 
# d.agg <- aggregate(score ~ lead_mean + model, data = d.noLU, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model, data = d.noLU, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model')); rm(d.agg.count)
# names(d.agg)[3:4] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # difference is still there; pretty similar to before
# 
# d.noLU <- d.temp[d.temp$country != 'HU' & d.temp$country != 'IT' & d.temp$country != 'ES' & d.temp$country != 'PL', ]
# 
# d.agg <- aggregate(score ~ lead_mean + model, data = d.noLU, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model, data = d.noLU, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model')); rm(d.agg.count)
# names(d.agg)[3:4] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # again, still there - so it's not consistently enough just these countries
# rm(d.noLU)
# # see above - now very little difference before predicted peak
# 
# # check by season:
# d.agg <- aggregate(score ~ lead_mean + model + season, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + season, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'season')); rm(d.agg.count)
# names(d.agg)[4:5] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   facet_wrap(~ season, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# # the difference after the predicted peak is pretty consistent - suggests an issue with fits almost?
# # biggest consistent difference for 2013-14, 2016-17, 2017-18
# 
# # check by subtype:
# d.agg <- aggregate(score ~ lead_mean + model + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'subtype')); rm(d.agg.count)
# names(d.agg)[4:5] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   facet_wrap(~ subtype, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # different after peak for all, but only before peak for H3
# 
# # where -10?:
# d.low <- d.temp[d.temp$score == -10, ] # 832 / 14,202
# 
# table(d.low$model) # 597 network, 235 isolated
# table(d.low$model, d.low$season)
# table(d.low$model, d.low$lead_mean) # always more in network, but amount more doesn't always increase over time
# table(d.low$subtype, d.low$lead_mean, d.low$model) # H3 worst, but all but early B have more -10 in network
# table(d.low$country, d.low$model) # not consistently one country - generally more issues with network than isolated
# # but LU has a lot, as do DE, BE, and HU
# table(d.low$country, d.low$lead_mean, d.low$model) # there's just more everywhere, I don't think we can attribute this to a single country
# # so we can attribute the differences to this small amount of failures, but not a consistent spot where they happen?
# table(d.low$model, d.low$fc_start)
# 
# # check by country:
# d.agg <- aggregate(score ~ lead_mean + model + country, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + country, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'country')); rm(d.agg.count)
# names(d.agg)[4:5] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   facet_wrap(~ country, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # really bad over all times for ES, HU, IT, (LU from -2 on), PL (SK?)
# # actually better or no real difference for AT, CZ, NL
# 
# # try after removing -10:
# d.notlow <- d.temp[d.temp$score > -10, ]
# 
# d.agg <- aggregate(score ~ lead_mean + model, data = d.notlow, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model, data = d.notlow, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model')); rm(d.agg.count)
# names(d.agg)[3:4] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # almost no difference
# # so it seems to be these -10s - note that without them, the median score is -1.6261 - so -10 will really bring the mean down
# 
# par(mfrow = c(2, 1))
# hist(d.notlow$score[d.notlow$model == 'Network'])
# hist(d.notlow$score[d.notlow$model == 'Isolated'])
# 
# summary(d.notlow$score[d.notlow$model == 'Network'])
# summary(d.notlow$score[d.notlow$model == 'Isolated'])
# # so distributions are pretty similar (isolated has less variance, but same mean/median) when -10s are removed
# 
# ### So we can attribute the difference in means to a tendency to more "failure" with the network model than the isolated ###
# 
# # Do the forecasts with failure have equivalent isolated forecasts, or is the difference b/c of tendency to predict earlier onset forecasts?
# d.low$group <- paste(d.low$season, d.low$country, d.low$fc_start, d.low$subtype, d.low$lead_mean, sep = '_')
# d.low$group <- factor(d.low$group)
# 
# d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$fc_start, d.temp$subtype, d.temp$lead_mean, sep = '_')
# d.temp$group <- factor(d.temp$group)
# 
# for (i in levels(d.low2$group)) {
#   print(i)
#   print(as.vector(unique(d.temp$model[d.temp$group == i])))
# }
# # mostly these ARE forecasts that exist for both the network and isolated models, so it's not just a tendency to produce earlier forecasts
# # wait, but not at the same lead! -- many actually do have both, though
# 
# # can find a single/or a couple spots where this failure is happening, and investigate those to get some idea of what is happening overall
# d.low2 <- d.low[d.low$subtype == 'A(H3)' & d.low$lead_mean %in% c(-3, 3), ]
# d.low2$group <- factor(d.low2$group)
# levels(d.low2$group) # a couple have both - DE/55, HU/55, HU/60, HU/64
# # choose one before and one after maybe
# # HU/55/16-17 and HU/64/2012-13 (both have isolated forecasts at same predicted lead and everything)
# # first: predicted lead -3, observed lead -1; second: predicted lead 3, observed lead 4
# d.full1 <- read.csv('results/by_subtype/network_A(H3)/original/outputDist_A(H3)_testSet1.csv')
# d.full2 <- read.csv('results/by_subtype/isolated_A(H3)/original/outputDist_A(H3)_ISOLATED_update.csv')
# 
# d.full1 <- d.full1[d.full1$country == 'HU' & d.full1$season == '2016-17' & d.full1$fc_start == 55 & d.full1$metric == 'pw', ]
# d.full1 <- d.full1[d.full1$value > 0, ]
# d.full2 <- d.full2[d.full2$country == 'HU' & d.full2$season == '2016-17' & d.full2$fc_start == 55 & d.full2$metric == 'pw', ]
# d.full2 <- d.full2[d.full2$value > 0, ]
# # obs pkwk 56
# d.full1$bin <- d.full1$bin + 40 - 1
# d.full2$bin <- d.full2$bin + 40 - 1
# # by this point 6/12 countries have already peaked - would expect the model to bring things down, but seems to want to forecast peak 2 weeks later
# # HU has the highest scaled peak this season
# 
# # main point might just be that - yes, this would be less accurate even if we widened the window, b/c isolated tend to be 1 rather than 2 weeks late; but it's assigning
# # the value of -10 to forecasts where 0 vs 1 (score = -5.7) ensemble members are in the right 1-week window
#     # isolated tend to assign ~14 to correct bin
# # but these forecasts have the same mean prediction
# 
# # By season and subtype - is the effect only seen for some combinations?:
# # check by season:
# d.agg <- aggregate(score ~ lead_mean + model + season + subtype, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + season + subtype, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'season', 'subtype')); rm(d.agg.count)
# names(d.agg)[4:5 + 1] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() +
#   facet_grid(subtype ~ season, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# ### Look at where onset accuracy improved ###
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 0 & !is.na(d$leadonset5) & d$metric == 'Onset Timing', ] # 444 observations
# 
# table(d.temp$country[d.temp$lead_mean == -6], d.temp$model[d.temp$lead_mean == -6]) # 2 networks are LU and PL, single run - not robust
# table(d.temp$country[d.temp$lead_mean == -5], d.temp$model[d.temp$lead_mean == -5]) # 1 AT and 2 HU not predicted in isolated; 8 vs. 5 LU; 6 vs. 25 CZ
# # LU: one is early prediction of onset for 2016-17/H3; others are same forecasts for 2011-12/fcstart54/H3(all 5 runs): network scores are better!
# # CZ: all network are 12-13, but for different strains/runs (all start 44); same spot has forecasts for isolated for all runs, and network score only better for H1
# table(d.temp$country[d.temp$lead_mean == -4], d.temp$model[d.temp$lead_mean == -4]) # AT similar for network/isolated; LU only network has anything (6, 5 for same spot as above)
# table(d.temp$country[d.temp$lead_mean == -3], d.temp$model[d.temp$lead_mean == -3]) # LU: different season, similar performance of both models
# # AT: similar; DE: 2010-11, fc50 - network better for H1 but worse for B?; HU: similar; NL: same season, network appears a bit better (2012-13/H1)
# table(d.temp$country[d.temp$lead_mean == -2], d.temp$model[d.temp$lead_mean == -2]) # DE: same season, similar results;
# # HU 2013-14/H1 (above) or 2016-17/H3? or 2012-13/B (above) - little difference though - little improvement for H1, more forecasts of H3?; PL 2016-17(H3) seems better?
# # NL 2012-13 - H3 seems improved, no isolated forecasts of H1; IT: 2010-11/B/fc55 for network but not isolated;
# # LU: same as above, but add 2016-17(H3), and 2012-13(H3)(network only)
# table(d.temp$country[d.temp$lead_mean == -1], d.temp$model[d.temp$lead_mean == -1]) # way more network than isolated for AT, DE, ES, SK; LU high for both; more isolated for CZ
# # PL 17-18/B and 15-16/B - network only, but different season/strain than above; NL different seasons/strains than above;
# # LU also looking different than above - 17-18/H1, H3 all different by network/isolated; B not much interesting
# 
# # 2011-12 season (H3) for LU - seems that network performed better very early?; 2016-17/H3 and 2012-13/H3 too?; 2017-18/H1?
# # DE 2010-11 for H1, but not B; more forecasts at -1, and better than isolated, but still not good
# # NL 2012-13/H1 and H3?
# # AT has way more (and better) predictions at lead -1
# # also looks like lead one has a lot more predictions especially for later seasons (2015-16, 2016-17, 2017-18)s
# 
# # What would this plot look like if we limited to where forecasts at same season/country/subtype/fc_start available?:
# d.temp$group <- paste(d.temp$season, d.temp$country, d.temp$fc_start, d.temp$subtype, sep = '_'); d.temp$group <- factor(d.temp$group)
# to.keep <- c()
# for (i in levels(d.temp$group)) {
#   d.temp2 <- d.temp[d.temp$group == i, ]
#   if (length(unique(d.temp2$model)) == 2) {
#     to.keep <- c(to.keep, i)
#   }
# }
# # 26 levels to keep
# rm(d.temp2)
# d.temp2 <- d.temp[d.temp$group %in% to.keep, ]
# 
# d.agg <- aggregate(score ~ lead_mean + model, data = d.temp2, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model, data = d.temp2, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model')); rm(d.agg.count)
# names(d.agg)[3:4] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 20, 30, 40), labels = c(10, 20, 30, 40),
#                         limits = c(1, 50), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# # slight advantage at -6, -5, and -1; all others similar; and isolated produces more forecasts at -6 and -5

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# COMBINED by OBSERVED (still removing no onsets):
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5 & !is.na(d$leadonset5), ]
d.agg <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg)[4:5] <- c('score', 'count')

# COMBINED by OBSERVED (include ALL forecasts):
d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg2 <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric, data = d.temp, FUN = length)
d.agg2 <- merge(d.agg2, d.agg.count, by = c('FWeek', 'model', 'metric')); rm(d.agg.count)
names(d.agg2)[4:5] <- c('score', 'count')

# Plot (combined):
d.agg$method <- 'remove'
d.agg2$method <- 'include'
d.agg <- rbind(d.agg, d.agg2); rm(d.agg2)
d.agg$method <- factor(d.agg$method)
d.agg$method <- factor(d.agg$method, levels = levels(d.agg$method)[2:1])

p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
  theme_classic() + theme(aspect.ratio = 1,
                          legend.text = element_text(size = 12),
                          axis.text = element_text(size = 10),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 12),
                          legend.title = element_text(size = 12),
                          strip.background = element_blank()) +
  facet_grid(method ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-6, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
                        limits = c(1, 900), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model:', size = '# of Fcasts:') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F'),
                       metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing'),
                       method = c(rep('remove', 3), rep('include', 3)))
print(p2 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8))

# p3 <- ggplot(data = d.agg, aes(x = FWeek, y = score)) + geom_line(aes(col = model)) + geom_point(aes(col = model, size = count)) +
#   theme_classic() + theme(aspect.ratio = 1,
#                           legend.text = element_text(size = 12),
#                           axis.text = element_text(size = 10),
#                           strip.text = element_blank(),
#                           axis.title = element_text(size = 12),
#                           legend.title = element_text(size = 12),
#                           strip.background = element_blank()) +
#   facet_wrap(~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 800), labels = c(10, 100, 300, 800),
#                         limits = c(1, 900), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# 
# print(p2 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8))
# print(p3 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8))

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# ### FRIEDMAN TEST ###
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# 
# ggplot(data = d.temp, aes(x = FWeek, y = score, fill = model, group = paste(FWeek, model, '_'))) + geom_boxplot() + facet_wrap(~ metric) +
#   theme_classic() + scale_x_continuous(breaks = -6:4) + scale_y_continuous(breaks = -10:0) + scale_fill_brewer(palette = 'Set2')
# # obviously a lot of overlap - I think the question is, how often is one better than the other?
# # this seems to require pairing to some extent, though - so pair by runs?
# 
# d.temp <- d.temp[, c(1:2, 4:5, 8, 10:12)]
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
# # friedman.test(score ~ model | FWeek, data = d.pt)
# # would probably have to do it separately for each lead? or really just look at ranks overall?
# # either way, could just use a Wilcoxon, since only two groups (models)
# 
# d.pt$group1 <- paste(d.pt$season, d.pt$country, d.pt$run, d.pt$subtype, sep = '_'); d.pt$group1 <- factor(d.pt$group1)
# d.pt$group2 <- paste(d.pt$FWeek, d.pt$season, d.pt$country, d.pt$run, d.pt$subtype, sep = '_'); d.pt$group2 <- factor(d.pt$group2)
# d.pt$group3 <- paste(d.pt$FWeek, d.pt$season, d.pt$country, d.pt$subtype, sep = '_'); d.pt$group3 <- factor(d.pt$group3)
# 
# d.pi$group1 <- paste(d.pi$season, d.pi$country, d.pi$run, d.pi$subtype, sep = '_'); d.pi$group1 <- factor(d.pi$group1)
# d.pi$group2 <- paste(d.pi$FWeek, d.pi$season, d.pi$country, d.pi$run, d.pi$subtype, sep = '_'); d.pi$group2 <- factor(d.pi$group2)
# d.pi$group3 <- paste(d.pi$FWeek, d.pi$season, d.pi$country, d.pi$subtype, sep = '_'); d.pi$group3 <- factor(d.pi$group3)
# 
# d.ot$group1 <- paste(d.ot$season, d.ot$country, d.ot$run, d.ot$subtype, sep = '_'); d.ot$group1 <- factor(d.ot$group1)
# d.ot$group2 <- paste(d.ot$FWeek, d.ot$season, d.ot$country, d.ot$run, d.ot$subtype, sep = '_'); d.ot$group2 <- factor(d.ot$group2)
# d.ot$group3 <- paste(d.ot$FWeek, d.ot$season, d.ot$country, d.ot$subtype, sep = '_'); d.ot$group3 <- factor(d.ot$group3)
# 
# network.better = isol.better = 0
# for (block in levels(d.pt$group2)) {
#   if (d.pt[d.pt$group2 == block & d.pt$model == 'Network', 'score'] > d.pt[d.pt$group2 == block & d.pt$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.pt[d.pt$group2 == block & d.pt$model == 'Network', 'score'] < d.pt[d.pt$group2 == block & d.pt$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   }
# }
# # 5042 (4676) isol to 4083 network (PT)
# 
# network.better = isol.better = 0
# for (block in levels(d.pi$group2)) {
#   if (d.pi[d.pi$group2 == block & d.pi$model == 'Network', 'score'] > d.pi[d.pi$group2 == block & d.pi$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.pi[d.pi$group2 == block & d.pi$model == 'Network', 'score'] < d.pi[d.pi$group2 == block & d.pi$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   }
# }
# # 5257 (4668) isol to 3868 network (PI)
# 
# network.better = isol.better = 0
# for (block in levels(d.ot$group2)) {
#   if (d.ot[d.ot$group2 == block & d.ot$model == 'Network', 'score'] > d.ot[d.ot$group2 == block & d.ot$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.ot[d.ot$group2 == block & d.ot$model == 'Network', 'score'] < d.ot[d.ot$group2 == block & d.ot$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   } # else {
#   #   print(d.ot[d.ot$group2 == block, ])
#   # }
# }
# # 4818 (4490 - rest are ties) isol to 4107 network (OT)
# 
# friedman.test(score ~ model | group2, data = d.pt)
# # wilcox.test(score ~ model + group2, data = d.pt, alternative = 'two.sided', paired = TRUE)
# binom.test(4083, 8759, 0.5, alternative = 'two.sided')
# 
# friedman.test(score ~ model | group2, data = d.pi)
# binom.test(3868, 8536, 0.5, alternative = 'two.sided')
# 
# friedman.test(score ~ model | group2, data = d.ot)
# binom.test(4107, 8597, 0.5, alternative = 'two.sided')
# 
# # now they're much more similar
# 
# # Find a way to draw a random run for each:
# d.pt <- d.pt[order(d.pt$group3, d.pt$model), ]
# rownames(d.pt) <- 1:dim(d.pt)[1]
# 
# d.pi <- d.pi[order(d.pi$group3, d.pi$model), ]
# rownames(d.pi) <- 1:dim(d.pi)[1]
# 
# d.ot <- d.ot[order(d.ot$group3, d.ot$model), ]
# rownames(d.ot) <- 1:dim(d.ot)[1]
# 
# permute.by.run <- function(dat, choices) {
#   runs.to.use <- round(runif(choices, 1, 5)) + 5 * (seq(1:choices) - 1)
#   rows.to.use <- as.numeric(rownames(dat))[runs.to.use]
#   return(dat[rows.to.use, ])
# }
# 
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   d.pt.red <- permute.by.run(d.pt, dim(d.pt)[1] / 5)
#   d.pt.red$group3 <- factor(d.pt.red$group3)
#   p.vals <- c(p.vals, friedmanTest(d.pt.red$score, d.pt.red$model, d.pt.red$group3, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.0015
# 
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   d.pi.red <- permute.by.run(d.pi, dim(d.pi)[1] / 5)
#   d.pi.red$group3 <- factor(d.pi.red$group3)
#   p.vals <- c(p.vals, friedmanTest(d.pi.red$score, d.pi.red$model, d.pi.red$group3, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.0000305
# 
# set.seed(1089437584)
# p.vals <- c()
# for (i in 1:100) {
#   d.ot.red <- permute.by.run(d.ot, dim(d.ot)[1] / 5)
#   d.ot.red$group3 <- factor(d.ot.red$group3)
#   p.vals <- c(p.vals, friedmanTest(d.ot.red$score, d.ot.red$model, d.ot.red$group3, dist = 'FDist')$p.value)
# }
# print(median(p.vals)) # 0.0388
# 
# # d.pt.red <- permute.by.run(d.pt, dim(d.pt)[1] / 5)
# # d.pi.red <- permute.by.run(d.pi, dim(d.pi)[1] / 5)
# # d.ot.red <- permute.by.run(d.ot, dim(d.ot)[1] / 5)
# # 
# # d.pt.red$group3 <- factor(d.pt.red$group3)
# # d.pi.red$group3 <- factor(d.pi.red$group3)
# # d.ot.red$group3 <- factor(d.ot.red$group3)
# # 
# # friedman.test(score ~ model | group3, data = d.pt.red) # sig
# # friedman.test(score ~ model | group3, data = d.pi.red) # sig
# # friedman.test(score ~ model | group3, data = d.ot.red) # nonsig
# # p = 0.01 / 3 = 0.0034
# 
# network.better = isol.better = 0
# for (block in levels(d.pt.red$group3)) {
#   if (d.pt.red[d.pt.red$group3 == block & d.pt.red$model == 'Network', 'score'] > d.pt.red[d.pt.red$group3 == block & d.pt.red$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.pt.red[d.pt.red$group3 == block & d.pt.red$model == 'Network', 'score'] < d.pt.red[d.pt.red$group3 == block & d.pt.red$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   }
# }
# # 951 isolated to 807 network
# 
# network.better = isol.better = 0
# for (block in levels(d.pi.red$group3)) {
#   if (d.pi.red[d.pi.red$group3 == block & d.pi.red$model == 'Network', 'score'] > d.pi.red[d.pi.red$group3 == block & d.pi.red$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.pi.red[d.pi.red$group3 == block & d.pi.red$model == 'Network', 'score'] < d.pi.red[d.pi.red$group3 == block & d.pi.red$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   }
# }
# # 948 isolated to 765 network
# 
# network.better = isol.better = 0
# for (block in levels(d.ot.red$group3)) {
#   if (d.ot.red[d.ot.red$group3 == block & d.ot.red$model == 'Network', 'score'] > d.ot.red[d.ot.red$group3 == block & d.ot.red$model == 'Isolated', 'score']) {
#     network.better <- network.better + 1
#   } else if (d.ot.red[d.ot.red$group3 == block & d.ot.red$model == 'Network', 'score'] < d.ot.red[d.ot.red$group3 == block & d.ot.red$model == 'Isolated', 'score']) {
#     isol.better <- isol.better + 1
#   } # else {
#   #   print(d.ot.red[d.ot.red$group3 == block, ])
#   # }
# }
# # 883 isolated to 838 network

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
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
#                        metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing', 'Peak Timing', 'Peak Intensity', 'Onset Timing',
#                                   'Peak Timing', 'Peak Intensity', 'Onset Timing'),
#                        subtype = c(rep('A(H1)', 3), rep('A(H3)', 3), rep('B', 3)))
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
                       subtype = c('A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B', 'A(H1)', 'A(H3)', 'B'),
                       metric = c(rep('Peak Timing', 3), rep('Peak Intensity', 3), rep('Onset Timing', 3)))
print(p1 + geom_text(data = dat.text, mapping = aes(x = -5.6, y = -0.25, label = label), size = 8))

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

# dev.off()

rm(list = ls())

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# ### Friedman tests ###
# # p = 0.01 / 9 = just use 0.001 to be extra safe
# 
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# ggplot(data = d.temp, aes(x = FWeek, y = score, fill = model, group = paste(FWeek, model, '_'))) + geom_boxplot() + facet_grid(subtype ~ metric) +
#   theme_classic() + scale_x_continuous(breaks = -6:4) + scale_y_continuous(breaks = -10:0) + scale_fill_brewer(palette = 'Set2')
# 
# d.temp <- d.temp[, c(1:2, 4:5, 8, 10:12)]
# d.temp$group1 <- paste(d.temp$FWeek, d.temp$season, d.temp$country, d.temp$subtype, sep = '_'); d.temp$group1 <- factor(d.temp$group1)
# 
# d.temp <- d.temp[order(d.temp$metric, d.temp$subtype, d.temp$group1, d.temp$model), ]
# 
# for (metric in levels(d.temp$metric)) {
#   for (subtype in levels(d.temp$subtype)) {
#     print(paste(metric, subtype, sep = '_'))
#     
#     d.red <- d.temp[d.temp$metric == metric & d.temp$subtype == subtype, ]
#     # print(length(unique(d.red$group1)))
#     # print(dim(d.red))
#     rownames(d.red) <- 1:dim(d.red)[1]
#     
#     set.seed(1089437584)
#     p.vals = c()
#     for (i in 1:100) {
#       d.red2 <- permute.by.run(d.red, dim(d.red)[1] / 5)
#       d.red2$group1 <- factor(d.red2$group1)
#       # print(friedman.test(score ~ model | group1, data = d.red))
#       a <- friedmanTest(d.red2$score, d.red2$model, d.red2$group1, dist = 'FDist')
#       # f.vals <- c(f.vals, a$statistic) # only need to know if the difference is significant; don't need test statistic
#       p.vals <- c(p.vals, a$p.value)
#     }
#     # d.red <- permute.by.run(d.red, dim(d.red)[1] / 5)
#     # d.red$group1 <- factor(d.red$group1)
#     # print(friedman.test(score ~ model | group1, data = d.red))
#     print(median(p.vals))
#     
#   }
# }
# # PT not sig; PI sig for H3 (p<0.001); OT not sig

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

# # Breakdown by DATA TYPE:
# # Need info on this merged first
# d$data.type <- 'ILI+'
# d$data.type[d$country %in% c('DE', 'LU')] <- 'ARI+'
# d$data.type[d$country == 'FR' & d$season %in% c('2012-13', '2013-14')] <- 'ARI+'
# d$data.type <- factor(d$data.type)
# d$data.type <- factor(d$data.type, levels = levels(d$data.type)[2:1])
# 
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric + data.type, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric + data.type, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'data.type')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(data.type~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5.5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 700), labels = c(10, 100, 300, 700),
#                         limits = c(1, 700), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# d.agg <- aggregate(score ~ FWeek + model + metric + data.type, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ FWeek + model + metric + data.type, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'data.type')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(data.type ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-5.5, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300, 700), labels = c(10, 100, 300, 700),
#                         limits = c(1, 700), range = c(1,6)) +
#   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p2)
# # no dramatic differences by obs.; some stuff there by predicted lead, but probably country-specific - only 2.3 countries have ARI, and one is LU

# # Breakdown by SEASON:
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric + season, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'season')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(season~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 400), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# # d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# # d.agg <- aggregate(score ~ FWeek + model + metric + season, data = d.temp, FUN = mean)
# # d.agg.count <- aggregate(score ~ FWeek + model + metric + season, data = d.temp, FUN = length)
# # d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'season')); rm(d.agg.count)
# # names(d.agg)[5:6] <- c('score', 'count')
# # 
# # p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
# #   theme_bw() +
# #   facet_grid(season ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
# #   scale_y_continuous(limits = c(-7, 0), breaks = -10:0) +
# #   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
# #                         limits = c(1, 400), range = c(1,6)) +
# #   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
# #   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# # print(p2)

# # By region?
# m <- read.csv('results/network/outputMet_pro_PROC.csv')
# m <- unique(m[, c(1, 8, 48:49, 57)])
# d <- merge(d, m, by = c('season', 'country', 'subtype'))
# 
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric + region, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric + region, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'region')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(region~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-6, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 650), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# # d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# # d.agg <- aggregate(score ~ FWeek + model + metric + region, data = d.temp, FUN = mean)
# # d.agg.count <- aggregate(score ~ FWeek + model + metric + region, data = d.temp, FUN = length)
# # d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'region')); rm(d.agg.count)
# # names(d.agg)[5:6] <- c('score', 'count')
# # 
# # p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
# #   theme_bw() +
# #   facet_grid(region ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
# #   scale_y_continuous(limits = c(-6, 0), breaks = -10:0) +
# #   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
# #                         limits = c(1, 650), range = c(1,6)) +
# #   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
# #   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# # print(p2)
# # # southwest a bit better than eastern, but not notable really

# # And by scaling range?:
# d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
# d.agg <- aggregate(score ~ lead_mean + model + metric + scaling.range, data = d.temp, FUN = mean)
# d.agg.count <- aggregate(score ~ lead_mean + model + metric + scaling.range, data = d.temp, FUN = length)
# d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'scaling.range')); rm(d.agg.count)
# names(d.agg)[5:6] <- c('score', 'count')
# 
# p1 <- ggplot(data = d.agg, aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
#   theme_bw() +
#   facet_grid(scaling.range~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
#   scale_y_continuous(limits = c(-7, 0), breaks = -10:0) +
#   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
#                         limits = c(1, 275), range = c(1,6)) +
#   labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
#   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# print(p1)
# 
# # d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
# # d.agg <- aggregate(score ~ FWeek + model + metric + scaling.range, data = d.temp, FUN = mean)
# # d.agg.count <- aggregate(score ~ FWeek + model + metric + scaling.range, data = d.temp, FUN = length)
# # d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'scaling.range')); rm(d.agg.count)
# # names(d.agg)[5:6] <- c('score', 'count')
# # 
# # p2 <- ggplot(data = d.agg, aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
# #   theme_bw() +
# #   facet_grid(scaling.range ~ metric, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
# #   scale_y_continuous(limits = c(-7, 0), breaks = -10:0) +
# #   scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
# #                         limits = c(1, 275), range = c(1,6)) +
# #   labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
# #   guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# # print(p2)
# # highest scalings worst for PT, but no consistent pattern over all groups; no patterns for OT, except that maybe more forecasts for higher scalings?
# # but highest group is literally just LU
# # try regrouping:
# d$scaling.range <- as.character(d$scaling.range)
# d$scaling.range[d$scaling.range %in% c('(0,0.5]', '(0.5,1]')] <- '(0,1]'
# d$scaling.range[d$scaling.range %in% c('(2,10]', '(10,20]')] <- '(2,20]'
# d$scaling.range[d$scaling.range %in% c('(20,50]', '(50,100]')] <- '(20,100]'
# d$scaling.range <- factor(d$scaling.range)
# # no clear patterns; fewer fcasts of OT for smallest scalings (DE, FR, SK)

# Do country plots in another file, but quick check:
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5) & d$score > -10, ]
d.agg <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ lead_mean + model + metric + country, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('lead_mean', 'model', 'metric', 'country')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p1 <- ggplot(data = d.agg[d.agg$metric != 'Onset Timing', ], aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(metric ~ country, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Predicted Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p1)

ggplot(data = d.agg[d.agg$metric == 'Onset Timing', ], aes(x = lead_mean, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() + facet_wrap(~ country, nrow = 2)

d.temp <- d[d$FWeek >= -6 & d$FWeek < 5, ]
d.agg <- aggregate(score ~ FWeek + model + metric + country, data = d.temp, FUN = mean)
d.agg.count <- aggregate(score ~ FWeek + model + metric + country, data = d.temp, FUN = length)
d.agg <- merge(d.agg, d.agg.count, by = c('FWeek', 'model', 'metric', 'country')); rm(d.agg.count)
names(d.agg)[5:6] <- c('score', 'count')

p2 <- ggplot(data = d.agg[d.agg$metric != 'Onset Timing', ], aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() +
  facet_grid(metric ~ country, scales = 'free') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -8:4) +
  scale_y_continuous(limits = c(-10, 0), breaks = -10:0) +
  scale_size_continuous(breaks = c(10, 100, 300), labels = c(10, 100, 300),
                        limits = c(1, 400), range = c(1,6)) +
  labs(x = 'Observed Lead Week', y = 'Mean Log Score', col = 'Model', size = '# of Fcasts') +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
print(p2)

ggplot(data = d.agg[d.agg$metric == 'Onset Timing', ], aes(x = FWeek, y = score, col = model)) + geom_line() + geom_point(aes(size = count)) +
  theme_bw() + facet_wrap(~ country, nrow = 2)







