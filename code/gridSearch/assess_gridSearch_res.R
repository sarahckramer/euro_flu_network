
# Notes:
    # Definitely use oev_base 1e4!
    # Countries that perform poorly typically also did so in Aim 1, and have relatively few tests performed
    # But: oev_base 1e4 leads to wider parameter ranges, and often unrealistic estimates of airScale

# Read in all metrics files and merge:
file.list <- list.files('code/gridSearch/outputs/obs/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/obs/', file.list[[i]]))
}

# Merge all files:
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(file.list, met.list, i)

# Remove where obs are NA:
m <- m[!is.na(m$onsetObs), ]

# Calculate onset acc:
m$ot_acc <- 'n'
m$ot_acc[m$delta_onset %in% c(-1, 0, 1)] <- 'y'
m$ot_acc <- factor(m$ot_acc)

# Look at PT, PI, OT, corr, and rmse by oev_base, oev_denom, and lambda
chisq.test(table(m$oev_base, m$pt_acc))
chisq.test(table(m$oev_base, m$pi_acc))
chisq.test(table(m$oev_base, m$ot_acc))
chisq.test(table(m$oev_denom, m$pt_acc))
chisq.test(table(m$oev_denom, m$pi_acc))
chisq.test(table(m$oev_denom, m$ot_acc))
chisq.test(table(m$lambda, m$pt_acc))
chisq.test(table(m$lambda, m$pi_acc))
chisq.test(table(m$lambda, m$ot_acc))
# sig differences by oev_base and lambda, but not oev_denom

m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)

a1 <- glm(pt_acc ~ oev_base + oev_denom + lambda, data = m, family = 'binomial')
a2 <- glm(pi_acc ~ oev_base + oev_denom + lambda, data = m, family = 'binomial')
a3 <- glm(ot_acc ~ oev_base + oev_denom + lambda, data = m, family = 'binomial')
# preferences for oev_base = 1e4 and lambda = 1.03 or 1.05 (particularly for PI/OT 1.05 is best)

par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(corr ~ oev_base, data = m)
boxplot(corr ~ oev_denom, data = m)
boxplot(corr ~ lambda, data = m)
# for all of them the vast majority are near 1, so it's hard to really see what's happening

boxplot(log(rmse) ~ oev_base, data = m)
boxplot(log(rmse) ~ oev_denom, data = m)
boxplot(log(rmse) ~ lambda, data = m)
# looks a little better for 1e4, but otherwise not seeing much

a4 <- lm(corr ~ oev_base + oev_denom + lambda, data = m)
a5 <- lm(log(rmse) ~ oev_base + oev_denom + lambda, data = m)
# same directions as seen for PT, PI, and OT acc

m$group <- paste(m$oev_base, m$oev_denom, m$lambda, sep = '_'); m$group <- factor(m$group)
m$pt_acc <- m$pt_acc == 'y'
m$pi_acc <- m$pi_acc == 'y'
m$ot_acc <- m$ot_acc == 'y'

pt.acc.grouped <- aggregate(pt_acc ~ group, data = m, FUN = mean)
pt.acc.grouped <- pt.acc.grouped[order(-pt.acc.grouped$pt_acc), ]
# highest use oev_base 1e4, but even worse > 70%

pi.acc.grouped <- aggregate(pi_acc ~ group, data = m, FUN = mean)
pi.acc.grouped <- pi.acc.grouped[order(-pi.acc.grouped$pi_acc), ]
# best is 1e4/10/1.05, but oev_denom in full range can be in top; worst get only ~35% of peaks

ot.acc.grouped <- aggregate(ot_acc ~ group, data = m, FUN = mean)
ot.acc.grouped <- ot.acc.grouped[order(-ot.acc.grouped$ot_acc), ]
# best is 1e4/20/1.05, but oev_denom doesn't seem to matter; worst is ~ 47%

# Also see if patterns by country or season:
aggregate(pt_acc ~ season, data = m, FUN = mean) # 2012-13 best (>90%), 2011-12 worst (~65%)
aggregate(pt_acc ~ season, data = m, FUN = mean) # similar
aggregate(ot_acc ~ season, data = m, FUN = mean) # 2012-13 better than other two

aggregate(pt_acc ~ country, data = m, FUN = mean)[order(-aggregate(pt_acc ~ country, data = m, FUN = mean)[, 2]) ,] # DE worst at 23%; SI, BE, SK, RO, UK also bad
aggregate(pi_acc ~ country, data = m, FUN = mean)[order(-aggregate(pi_acc ~ country, data = m, FUN = mean)[, 2]) ,] # PT, SK, HR worst; alos DK, RO, CZ, LU
aggregate(ot_acc ~ country, data = m, FUN = mean)[order(-aggregate(ot_acc ~ country, data = m, FUN = mean)[, 2]) ,] # SK and LU worst; also DE and PT

# # Look at the data for these countries, as well as their Aim 1 results:
# # BE, CZ, DE, DK, HR, LU, PT, RO, SI, SK, UK
# aggregate(pt_acc ~ season, data = m[m$country == 'BE', ], FUN = mean) # only bad for first two seasons; 2010-11 has double peak, and it gets the second (2 wks late);
# # overall, data can be jumpy around peak, but not a systematic issue; bad for PT in Aim1, too
# 
# aggregate(pi_acc ~ season, data = m[m$country == 'CZ', ], FUN = mean) # common for all seasons; had trouble in Aim 1, too (not sure why)
# 
# aggregate(pt_acc ~ season, data = m[m$country == 'DE', ], FUN = mean) # fine for 10-11, but almost always misses other two; 
# aggregate(ot_acc ~ season, data = m[m$country == 'DE', ], FUN = mean) # fine for 12-13, but bad for others
# # data are very noisy; DE did poorly in Aim 1, too; some other metric (like total AR) better?
# 
# aggregate(pi_acc ~ season, data = m[m$country == 'DK', ], FUN = mean) # common across seasons; steep increase in 11-12, double peak in 12-13; subpar in Aim1, too
# 
# aggregate(pi_acc ~ season, data = m[m$country == 'HR', ], FUN = mean) # 0% in 11-12, worse in 12-13 than 10-11; also bad in Aim1; first 2 have double peak or misleading downturn;
# # !!! not sure why bad in 12-13 - lots of error or something?
# 
# aggregate(pi_acc ~ season, data = m[m$country == 'LU', ], FUN = mean) # 11-12 0%; others not great either; LU was already pretty bad in Aim1; data very noisy, lots of ups and downs
# aggregate(ot_acc ~ season, data = m[m$country == 'LU', ], FUN = mean) # 0% for 12-13, others around 25%; 
# 
# aggregate(pi_acc ~ season, data = m[m$country == 'PT', ], FUN = mean) # all bad; also bad in Aim1 - but these are the 3 worst seasons there; a bit noisy - I wonder if humidity plays a role?
# aggregate(ot_acc ~ season, data = m[m$country == 'PT', ], FUN = mean) # 10-11 has 0%, others aren't bad; likely just a bump in the data (2wk) - not normally that bad
# 
# aggregate(pt_acc ~ season, data = m[m$country == 'RO', ], FUN = mean) # mostly due to 11-12; also not great in Aim1
# aggregate(pi_acc ~ season, data = m[m$country == 'RO', ], FUN = mean) # exactly the opposite pattern; subpar but not terrible in Aim1; noisy data and double peaks
# 
# aggregate(pt_acc ~ season, data = m[m$country == 'SI', ], FUN = mean) # not bad in Aim1; mostly 11-12 - double peak
# 
# aggregate(pt_acc ~ season, data = m[m$country == 'SK', ], FUN = mean) # mostly 11-12; also bad in Aim1; late peak in 11-12; noisy data overall
# aggregate(pi_acc ~ season, data = m[m$country == 'SK', ], FUN = mean) # 11-12 worst, but none are good; also bad in Aim1; late peak in 11-12; noisy overall
# aggregate(ot_acc ~ season, data = m[m$country == 'SK', ], FUN = mean) # 12-13 a little better, but all bad
# # !!! doesn't necessarily seem worse than in Aim1, but still see how high error is
# 
# aggregate(pt_acc ~ season, data = m[m$country == 'UK', ], FUN = mean) # almost entirely due to 11-12 (0% vs. >95% for other 2 seasons) - double peak
# 
# # look at test counts, etc.: CZ, DE, DK, HR, LU, PT, RO, SK [all of these performed meh in Aim1, too] [ensure that they tend to have higher error - that would be good, right?]
# ntests <- read.csv('data/testCounts_052719.csv')
# # ntests <- ntests[79:234, ] # limit to seasons of interest
# matplot(ntests[, 2:21], pch = 20, type = 'b', lty = 1, col = viridis(20))
# 
# ntests$time <- 1:(dim(ntests)[1])
# ntests <- melt(ntests, id.vars = 'time')
# p1 <- ggplot(data = ntests) + geom_line(aes(x = time, y = value), col = 'mediumorchid4', lwd = 1.0) +
#   # geom_point(aes(x = time, y = value), col = 'mediumorchid3', pch = 20, size = 2.0) +
#   theme_classic() + facet_wrap(~ variable, scales = 'free_y')
# print(p1)
# # peaks around or below 500 tests: BE, CZ, DE, HU, HR, LU, NL, PT, RO, SK
# # here but not particularly bad in fitting: BE, HU
# # not great at fitting but not here: DK - actually < 500 for first 2 seasons, just greater later
# # so for the most part, countries performing more poorly also have low test counts (and therefore higher error)

# What if we limit to oev_base = 1e4?:
m.red <- m[m$oev_base == 1e4, ]

chisq.test(table(m.red$oev_denom, m.red$pt_acc))
chisq.test(table(m.red$oev_denom, m.red$pi_acc))
chisq.test(table(m.red$oev_denom, m.red$ot_acc))
chisq.test(table(m.red$lambda, m.red$pt_acc))
chisq.test(table(m.red$lambda, m.red$pi_acc))
chisq.test(table(m.red$lambda, m.red$ot_acc))
# same - lambda is sig, oev_denom is not

aggregate(pt_acc ~ season, data = m.red, FUN = mean)
aggregate(pt_acc ~ season, data = m.red, FUN = mean)
aggregate(ot_acc ~ season, data = m.red, FUN = mean)
# results largely the same

aggregate(pt_acc ~ country, data = m.red, FUN = mean)[order(-aggregate(pt_acc ~ country, data = m.red, FUN = mean)[, 2]) ,] # mostly same, but RO a little better now
aggregate(pi_acc ~ country, data = m.red, FUN = mean)[order(-aggregate(pi_acc ~ country, data = m.red, FUN = mean)[, 2]) ,] # CZ a little better, UK bad now
aggregate(ot_acc ~ country, data = m.red, FUN = mean)[order(-aggregate(ot_acc ~ country, data = m.red, FUN = mean)[, 2]) ,] # same; LU almost never correct now

# Also check how timing/size of the peak/timing of the onset impacts accuracy:
aggregate(pt_acc ~ obs_pkwk, data = m.red, FUN = mean) # peaks of 25-28 definitely worse, but how many have this? 4 countries (HR, DE, SK, UK) in 11-12
aggregate(pi_acc ~ obs_pkwk, data = m.red, FUN = mean) # starting to drop by 23, which is a little more common; and mostly countries that have trouble anyway
aggregate(ot_acc ~ onsetObs, data = m.red, FUN = mean) # has trouble at 47-49 (makes sense - these are the earliest), and 64 (just once, for SK)

a1 <- glm(pt_acc ~ oev_base + oev_denom + lambda + season + country + obs_pkwk + obs_peak_int, data = m, family = 'binomial') # later pkwk/larger peak = lower accuracy
a2 <- glm(pi_acc ~ oev_base + oev_denom + lambda + season + country + obs_pkwk + obs_peak_int, data = m, family = 'binomial') # later pkwk/SMALLER peak = lower accuracy
a3 <- glm(ot_acc ~ oev_base + oev_denom + lambda + season + country + onsetObs, data = m, family = 'binomial') # later onset = higher accuracy (aOR = 1.105)

# Read in all output files and merge:
file.list <- list.files('code/gridSearch/outputs/obs/', pattern = 'OP_')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/obs/', file.list[[i]]))
}

# Merge all files:
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(file.list, op.list, i)

# Change variables to factors:
for (i in 2:5) {
  o[, i] <- factor(o[, i])
}

# Look at inferred params by all 5 variables:
# L:
p1 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = L,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = L,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = L,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, ncol = 1)
# oev_base 1e5 a lot more consistent between runs; 1e4 lower L in 10-11
# no real pattern by oev_denom or lambda

# D:
p1 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = D,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = D,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = D,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, ncol = 1)
# no real patterns - D seems more consistent than other params?

# R0mx:
p1 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0max,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0max,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0max,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, ncol = 1)
# no consistent pattern, but oev_base 1e4 and 1e5 differ; no real impact of others

# R0mn:
p1 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0min,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0min,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = R0min,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, ncol = 1)
# again, more spread out for oev_base 1e4

# airScale:
p1 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = airScale,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = airScale,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week,
                                       y = airScale,
                                       group = paste(run, oev_denom, oev_base, lambda),
                                       col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season) +
  scale_color_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, ncol = 1)
# 1e4/higher lambdas tend to fit airScale outside given ranges - up to 3x what the data say, which isn't realistic; no pattern in oev_denom;
# issue is less common in 11-12 season
# Need more travel? Or lower lambda with oev_base 1e4

# Also look at the impact of settings on param values:
a1 <- lm(L ~ season + oev_base + oev_denom + lambda, data = o) # higher for oev_base 1e5, and for increasingly higher lambdas
a2 <- lm(D ~ season + oev_base + oev_denom + lambda, data = o) # 2/3 day shorter for oev_base 1e5, no other strong differences
a3 <- lm(R0max ~ season + oev_base + oev_denom + lambda, data = o) # sig lower for oev_base 1e5 and higher lambdas, but strongest is 0.134 less for lambda 1.05
a4 <- lm(R0min ~ season + oev_base + oev_denom + lambda, data = o) # all pretty weak
a5 <- lm(airScale ~ season + oev_base + oev_denom + lambda, data = o) # a little lower for oev_base 1e5, a little higher for lambda 1.05








