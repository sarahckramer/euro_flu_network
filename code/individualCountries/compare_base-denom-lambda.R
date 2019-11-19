
# Read in metrics file:
m <- read.csv('results/R0diff_OEVold/outputMet_111819_INDIV_R0diff-OEVold_FULL_pro.csv')

# Calculate predicted onset lead:
m$leadonset5 <- m$fc_start - m$onset5

# Remove if no onset:
m <- m[!is.na(m$onsetObs5), ]

# Make relevant columns factors:
m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)
m$run <- factor(m$run)

# Calculate MAEs:
m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

# Plot PT/PI/OT by oev_base/oev_denom/lambda by predicted lead:
# Note: These aren't super great measures, but will let us make a quick decision on which to use!
source('code/gridSearch/comp_netVsIndiv/plotting_functions.R')

m.temp1 <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$onset5), ]
m.temp1$leadpkwk_mean <- factor(m.temp1$leadpkwk_mean)

m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$onset5), ]
m.temp2$leadonset5 <- factor(m.temp2$leadonset5)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      m.temp.ot <- m.temp2[m.temp2$oev_base == o1 & m.temp2$oev_denom == o2 & m.temp2$lambda == l,]
      
      res.pt <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_pkwk_mean')
      res.pt$metric <- 'pt'
      
      res.pi <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_peak_int_bin')
      res.pi$metric <- 'pi'
      
      res.ot <- format_for_plot(m.temp.ot, 'leadonset5', 'abs_delta_onset')
      res.ot$metric <- 'ot'
      
      res <- rbind(res.pt, res.pi, res.ot)
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
    }
  }
}

res.all$group <- paste(res.all$metric, res.all$oev_base, res.all$oev_denom, res.all$lambda, sep = '_'); res.all$group <- factor(res.all$group)

p1 <- ggplot(data = res.all[res.all$metric == 'pt', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p2 <- ggplot(data = res.all[res.all$metric == 'pi', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p3 <- ggplot(data = res.all[res.all$metric == 'ot', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
print(p1)
print(p2)
print(p3)

# PT: 1e4/50/1.05 (but lower lamdba better early on - use 1.02 as compromise?)
# PI: x/50/x (50 and 10 are definitely better than 1 for oev_denom, but less clear patterns for oev_base and lambda; lower and mid lambda seem to have more fcasts early on, though)
# OT: 1e4/50/1.00 (lambda esp. important for early values)
# overall: 1e4/50/1.02 - use middle lambda as compromise?

# And by observed:
m.temp1 <- m[m$FWeek_pkwk >= -8 & m$FWeek_pkwk < 5 & !is.na(m$onset5), ]
m.temp1$FWeek_pkwk <- factor(m.temp1$FWeek_pkwk)

m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7 & !is.na(m$onset5), ]
m.temp2$FWeek_onwk <- factor(m.temp2$FWeek_onwk)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      m.temp.ot <- m.temp2[m.temp2$oev_base == o1 & m.temp2$oev_denom == o2 & m.temp2$lambda == l,]
      
      res.pt <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_pkwk_mean')
      res.pt$metric <- 'pt'
      
      res.pi <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_peak_int_bin')
      res.pi$metric <- 'pi'
      
      res.ot <- format_for_plot(m.temp.ot, 'FWeek_onwk', 'abs_delta_onset')
      res.ot$metric <- 'ot'
      
      res <- rbind(res.pt, res.pi, res.ot)
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
    }
  }
}

res.all$group <- paste(res.all$metric, res.all$oev_base, res.all$oev_denom, res.all$lambda, sep = '_'); res.all$group <- factor(res.all$group)

p1 <- ggplot(data = res.all[res.all$metric == 'pt', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p2 <- ggplot(data = res.all[res.all$metric == 'pi', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p3 <- ggplot(data = res.all[res.all$metric == 'ot', ], aes(x = lead, y = value, group = group, col = oev_denom)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
print(p1)
print(p2)
print(p3)

# PT: x/x/x (all are pretty unclear; even w/ oev_denom, sometimes better early on when 1)
# PI: x/50/1.02 (lower lambda better earlier, larger later; oev_base unclear)
# OT: 1e4/10or50/1.00
# overall: 1e4/50/1.00or1.02

# so far: 1e4/50/1.02

# Also plot by MAE:
m.temp <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$leadonset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$leadonset5), ]

m.pt.agg <- aggregate(abs_err_pkwk ~ leadpkwk_mean + oev_base + oev_denom + lambda, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ leadpkwk_mean + oev_base + oev_denom + lambda, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ leadonset5 + oev_base + oev_denom + lambda, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ leadpkwk_mean + oev_base + oev_denom + lambda, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ leadpkwk_mean + oev_base + oev_denom + lambda, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ leadonset5 + oev_base + oev_denom + lambda, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('leadpkwk_mean', 'oev_base', 'oev_denom', 'lambda'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('leadpkwk_mean', 'oev_base', 'oev_denom', 'lambda'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('leadonset5', 'oev_base', 'oev_denom', 'lambda'))

p1 <- ggplot(data = m.pt.agg, aes(x = leadpkwk_mean, y = abs_err_pkwk.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_pkwk.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = leadpkwk_mean, y = abs_err_int_perc.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_int_perc.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = leadonset5, y = abs_err_onset.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_onset.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
grid.arrange(p1, p2, p3, ncol = 1)
# no substantial differences by lambda; for OT, 1e4 better for oev_base; for PI and 1e4, oev_denom 1 is best around peak

m.temp <- m[m$FWeek_pkwk >= -8 & m$FWeek_pkwk < 5 & !is.na(m$FWeek_onwk), ] # continue to remove where no onset predicted
m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7 & !is.na(m$FWeek_onwk), ]

m.pt.agg <- aggregate(abs_err_pkwk ~ FWeek_pkwk + oev_base + oev_denom + lambda, data = m.temp, FUN = mean)
m.pi.agg <- aggregate(abs_err_int_perc ~ FWeek_pkwk + oev_base + oev_denom + lambda, data = m.temp, FUN = mean)
m.ot.agg <- aggregate(abs_err_onset ~ FWeek_onwk + oev_base + oev_denom + lambda, data = m.temp2, FUN = mean)

m.pt.agg.c <- aggregate(abs_err_pkwk ~ FWeek_pkwk + oev_base + oev_denom + lambda, data = m.temp, FUN = length)
m.pi.agg.c <- aggregate(abs_err_int_perc ~ FWeek_pkwk + oev_base + oev_denom + lambda, data = m.temp, FUN = length)
m.ot.agg.c <- aggregate(abs_err_onset ~ FWeek_onwk + oev_base + oev_denom + lambda, data = m.temp2, FUN = length)

m.pt.agg <- merge(m.pt.agg, m.pt.agg.c, by = c('FWeek_pkwk', 'oev_base', 'oev_denom', 'lambda'))
m.pi.agg <- merge(m.pi.agg, m.pi.agg.c, by = c('FWeek_pkwk', 'oev_base', 'oev_denom', 'lambda'))
m.ot.agg <- merge(m.ot.agg, m.ot.agg.c, by = c('FWeek_onwk', 'oev_base', 'oev_denom', 'lambda'))

p1 <- ggplot(data = m.pt.agg, aes(x = FWeek_pkwk, y = abs_err_pkwk.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_pkwk.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Peak Timing', col = '')
p2 <- ggplot(data = m.pi.agg, aes(x = FWeek_pkwk, y = abs_err_int_perc.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_int_perc.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -8:4) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Percentage Error', title = 'Peak Intensity', col = '')
p3 <- ggplot(data = m.ot.agg, aes(x = FWeek_onwk, y = abs_err_onset.x, col = oev_denom)) + geom_line() + geom_point(aes(size = abs_err_onset.y)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + scale_x_continuous(breaks = -6:6) + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Mean Absolute Error', title = 'Onset Timing', col = '')
grid.arrange(p1, p2, p3, ncol = 1)
# similar results as by predicted; PT: lower oev_denom better earlier and worse towards end

# Overall best: 1e4/10/1.02 (differs by metric, but this seems to best capture everything)

# Now reduce accordingly and save results:
m <- read.csv('results/R0diff_OEVold/outputMet_111819_INDIV_R0diff-OEVold_FULL_pro.csv')
o <- read.csv('results/R0diff_OEVold/outputOP_111819_INDIV_R0diff-OEVold_FULL.csv')
d <- read.csv('results/R0diff_OEVold/outputDist_111819_INDIV_R0diff-OEVold_FULL.csv')
d.pt
d.ot
e <- read.csv('results/R0diff_OEVold/outputEns_111119_PI.csv')
e1 <- read.csv('results/R0diff_OEVold/outputEns_111119_1wk.csv')
e2 <- read.csv('results/R0diff_OEVold/outputEns_111119_2wk.csv')
e3 <- read.csv('results/R0diff_OEVold/outputEns_111119_3wk.csv')
e4 <- read.csv('results/R0diff_OEVold/outputEns_111119_4wk.csv')


















