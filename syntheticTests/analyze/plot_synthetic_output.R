
library(ggplot2); library(gridExtra)

# pdf('syntheticTests/outputs/cluster/model_fit_070519_Truth.pdf',
#     width = 14, height = 10)

# Read in results:
m <- read.csv('syntheticTests/outputs/cluster/outputMet.csv')
o <- read.csv('syntheticTests/outputs/cluster/outputOP.csv')
oStates <- read.csv('syntheticTests/outputs/cluster/outputOPStates.csv')
alps <- read.csv('syntheticTests/outputs/cluster/outputAlps.csv')

# Check what combos ran:
table(m$oev_base, m$oev_denom)

# ################################################################################################################################################################################################
# ################################################################################################################################################################################################
# 
# # Get % of PT/PI accurately fit:
# (aggregate(pt_acc ~ outbreak, data = m, FUN = table)[, 2][, 2] / (aggregate(pt_acc ~ outbreak, data = m, FUN = table)[, 2][, 1] + aggregate(pt_acc ~ outbreak, data = m, FUN = table)[, 2][, 2])) * 100
# (aggregate(pi_acc ~ outbreak, data = m, FUN = table)[, 2][, 2] / (aggregate(pi_acc ~ outbreak, data = m, FUN = table)[, 2][, 1] + aggregate(pi_acc ~ outbreak, data = m, FUN = table)[, 2][, 2])) * 100
# # peak timing captured better than peak intensity
# 
# aggregate(pt_acc ~ outbreak + oev_base + oev_denom + lambda, data = m, FUN = table)
# 
# # don't consider those with no observed outbreak:
# unique(m[is.na(m$onsetObs), c('outbreak', 'country', 'onsetObs')])
# #           outbreak country onsetObs
# # 18         1      SI       NA
# # 57         2      PT       NA
# # 171        5      HR       NA
# # 174        5      FR       NA
# # 265        2      NL       NA
# # 679        2      DE       NA
# m <- m[!is.na(m$onsetObs), ]
# 
# # test pt_acc by various factors:
# chisq.test(m$pt_acc, m$outbreak) # sig
# chisq.test(m$pt_acc, m$run)
# chisq.test(m$pt_acc, m$oev_base) # sig
# chisq.test(m$pt_acc, m$oev_denom) # sig
# chisq.test(m$pt_acc, m$lambda)
# chisq.test(m$pt_acc, m$country) # sig
# chisq.test(m$pt_acc, m$obs_pkwk) # sig
# chisq.test(m$pt_acc, m$obs_peak_int) # sig
# chisq.test(m$pt_acc, m$onsetObs) # sig
# 
# for (i in 1:5) {
#   m[, i] <- factor(m[, i])
# }
# 
# m1 <- glm(pt_acc ~ oev_denom, data = m, family = 'binomial') # higher sig better
# m2 <- glm(pt_acc ~ oev_base, data = m, family = 'binomial') # higher sig better
# 
# m1 <- glm(pt_acc ~ oev_denom, data = m[m$oev_base == 1e5, ], family = 'binomial') # higher better even when only comparing those with base = 1e5
# m2 <- glm(pt_acc ~ oev_base, data = m[m$oev_denom == 5, ], family = 'binomial') # oev_base itself not sig different for oev_denom = 5
# 
# # test pi_acc by various factors:
# chisq.test(m$pi_acc, m$outbreak) # sig
# chisq.test(m$pi_acc, m$run)
# chisq.test(m$pi_acc, m$oev_base) # sig
# chisq.test(m$pi_acc, m$oev_denom) # sig
# chisq.test(m$pi_acc, m$lambda) # sig
# chisq.test(m$pi_acc, m$country) # sig
# chisq.test(m$pi_acc, m$obs_pkwk) # sig
# chisq.test(m$pi_acc, m$obs_peak_int) # sig
# chisq.test(m$pi_acc, m$onsetObs) # sig
# 
# m1 <- glm(pi_acc ~ oev_denom, data = m, family = 'binomial') # higher sig better
# m2 <- glm(pi_acc ~ oev_base, data = m, family = 'binomial') # higher sig better
# 
# m1 <- glm(pi_acc ~ oev_denom, data = m[m$oev_base == 1e5, ], family = 'binomial') # higher better even when only comparing those with base = 1e5
# m2 <- glm(pi_acc ~ oev_base, data = m[m$oev_denom == 5, ], family = 'binomial') # oev_base 1e5 actually worse here, when looking only at oev_denom 5
# 
# # account for lambda?
# m1 <- glm(pi_acc ~ oev_denom + lambda, data = m[m$oev_base == 1e5, ], family = 'binomial')
# m2 <- glm(pi_acc ~ oev_base + lambda, data = m[m$oev_denom == 5, ], family = 'binomial')
# # significance holds for both; only 1.05 better than baseline; BUT: no lambda 1.05 when oev_base = 1e4
# 
# m1 <- glm(pi_acc ~ lambda, data = m[m$oev_base == 1e5, ], family = 'binomial') # lambda only sig for 1.05
# m2 <- glm(pi_acc ~ lambda, data = m[m$oev_base == 1e4, ], family = 'binomial') # lambda not sig
# 
# # assess by "group"?
# aggregate(pi_acc ~ oev_base + oev_denom + lambda, data = m, FUN = table)
# m$group <- paste(m$oev_base, m$oev_denom, m$lambda, sep = '_'); m$group <- factor(m$group)
# m1 <- glm(pi_acc ~ group, data = m, family = 'binomial')
# # best seem to be 1e5/20/any?
# 
# # narrow down to best combos?:
# m.best <- m[m$oev_denom %in% c(10, 20), ]
# m.best$oev_base <- factor(m.best$oev_base); m.best$oev_denom <- factor(m.best$oev_denom)
# m1 <- glm(pt_acc ~ oev_denom + lambda, data = m.best, family = 'binomial') # no sig difference between oev_denom 10 and 20
# m2 <- glm(pi_acc ~ oev_denom + lambda, data = m.best, family = 'binomial') # oev_denom 20 and lambda 1.05 still best
# # so looks like, for these two metrics at least, 1e5/20/1.05 have performed best - but this is also the combination that puts the least error on observations during the outbreaks relative to during dormant periods
# 
# # differences by outbreak/run?:
# m1 <- glm(pt_acc ~ outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial')
# m2 <- glm(pi_acc ~ outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial')
# # nope!
# 
# # differences by country:
# m1 <- glm(pt_acc ~ country + outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial') # HU, IT, PL, PT, SK tend to be bad
# m2 <- glm(pi_acc ~ country + outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial') # IS worse; SK, SE, RO, PL, LU tend to be better
# 
# # pt_acc and pi_acc positively assoc.?:
# chisq.test(table(m$pt_acc, m$pi_acc))
# chisq.test(table(m$pt_acc, m$pi_acc))$residuals
# # yes!
# 
# # Worse for later peaks?
# m1 <- glm(pt_acc ~ obs_pkwk + obs_peak_int + outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial') # later peak means less accuracy (aOR = 0.691)
# m2 <- glm(pi_acc ~ obs_pkwk + obs_peak_int + outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial') # worse for later and larger peaks (aORs are 0.727 and 0.99992 (per 1 decrease))
# 
# ################################################################################################################################################################################################
# ################################################################################################################################################################################################
# 
# # Get average correlation/RMSE:
# aggregate(corr ~ group, data = m, FUN = mean)
# summary(aov(corr ~ oev_base + oev_denom + lambda, data = m)) # all sig, but mostly base/denom
# summary(aov(rmse ~ oev_base + oev_denom + lambda, data = m)) # base/denom sig
# 
# m1 <- lm(corr ~ oev_denom + lambda, data = m[m$oev_base == 1e5, ]) # higher oev_denom associated with slight increase
# m2 <- lm(corr ~ oev_base, data = m[m$oev_denom == 5 & m$lambda != 1.05, ]) # higher oev_base associated with slight increase
# 
# m1 <- lm(corr ~ oev_denom + lambda, data = m.best) # oev_denom 20 still even better (but all of these increases are very slight?)
# # most of the correlation coefficients are very high, so these differences don't mean much
# # but still 1e5/20/any that is best
# 
# m1 <- lm(rmse ~ oev_denom + lambda, data = m[m$oev_base == 1e5, ]) # higher oev_denom associated with slight decrease; so is lambda 1.03 but not 1.05
# m2 <- lm(rmse ~ oev_base, data = m[m$oev_denom == 5 & m$lambda != 1.05, ]) # higher oev_base associated with slight decrease
# 
# m1 <- lm(rmse ~ oev_denom + lambda, data = m.best) # now oev_denom 20 and lambda 1.03/1.05 best
# 
# # differences by outbreak/run?:
# m1 <- lm(corr ~ outbreak + run + oev_denom + lambda, data = m.best) # outbreaks 3-5 slightly better
# m2 <- lm(rmse ~ outbreak + run + oev_denom + lambda, data = m.best) # outbreak 4 worse?
# # nope!
# 
# # differences by country:
# m1 <- lm(corr ~ country + outbreak + run + oev_denom + lambda, data = m.best) # a few sig worse, but only by a little; only some are the same as the ones that do poorly for PT/PT
# m2 <- lm(rmse ~ country + outbreak + run + oev_denom + lambda, data = m.best) # BE, CZ, DE, ES, FR, IS, RO, SE, SI, UK all worse than AT; NL better
# 
# # are these also associated with correct PT/PI predictions?
# m1 <- lm(corr ~ rmse + pt_acc + pi_acc, data = m)
# m2 <- lm(corr ~ rmse + pt_acc + pi_acc, data = m.best)
# # all strongly associated
# 
# # Worse for later peaks?
# m1 <- lm(corr ~ obs_pkwk + obs_peak_int + outbreak + run + oev_denom + lambda, data = m.best) # lower for later peaks and higher for larger peaks
# m2 <- lm(rmse ~ obs_pkwk + obs_peak_int + outbreak + run + oev_denom + lambda, data = m.best) # worse for larger peak intensity
# 
# ################################################################################################################################################################################################
# ################################################################################################################################################################################################

# Best so far: 1e5 // (10)/20 // (any/1.03)/1.05

# Go ahead and remove oev_base 1e4:
oStates <- oStates[oStates$oev_base == 1e5, ]; oStates$oev_base <- factor(oStates$oev_base)
oStates$oev_denom <- factor(oStates$oev_denom)

# Plot observed data vs. fit obs:
# oStates$group <- paste(oStates$oev_base, oStates$oev_denom, sep = '_'); oStates$group <- factor(oStates$group)
# oStates$group <- factor(oStates$group, levels = levels(oStates$group)[c(1, 4, 2:3)])
# oStates$group <- factor(oStates$group, levels = levels(oStates$group)[c(3, 1:2, 6, 4:5)])
oStates$group.plot <- paste(oStates$run, oStates$oev_denom, oStates$lambda, sep = '_'); oStates$group.plot <- factor(oStates$group.plot)
oStates$lambda <- factor(oStates$lambda)

countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

load('syntheticTests/syntheticData/synth_06-26_RATES.RData')
to.keep <- c(3, 5, 13, 19, 26)
synth.runs.RATES <- synth.runs.RATES[to.keep]

for (i in 1:length(synth.runs.RATES)) {
  synth.runs.RATES[[i]] <- t(synth.runs.RATES[[i]])
}

for (outbreak in 1:length(to.keep)) {
  obs_i <- synth.runs.RATES[[outbreak]]
  obs_i <- melt(obs_i)
  names(obs_i) <- c('week', 'country', 'newI')
  obs_i$country <- countries[obs_i$country]
  
  oStates.temp <- oStates[oStates$outbreak == outbreak, ]
  
  p1 <- ggplot() + geom_line(data = obs_i, aes(x = week, y = newI), lwd = 0.6) +
    geom_point(data = obs_i, aes(x = week, y = newI), pch = 4, cex = 2) +
    geom_line(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
    facet_wrap(~ country, scales = 'free_y') +
    theme_classic() + labs(x = 'Week', y = 'New Cases') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p1)
  
  # # or limit to a few countries of interest:
  # oStates.temp <- oStates.temp[oStates.temp$country %in% c('IS', 'PT', 'SE', 'PL', 'CZ', 'ES'), ]; oStates.temp$country <- factor(oStates.temp$country)
  # obs_i <- obs_i[obs_i$country %in% c('IS', 'PT', 'SE', 'PL', 'CZ', 'ES'), ]; obs_i$country <- factor(obs_i$country)
  # p2 <- ggplot() + geom_line(data = obs_i, aes(x = week, y = newI), lwd = 0.6) +
  #   geom_point(data = obs_i, aes(x = week, y = newI), pch = 4, cex = 2) +
  #   geom_line(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
  #   geom_point(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
  #   facet_wrap(~ country, scales = 'free_y') +
  #   theme_classic() + labs(x = 'Week', y = 'New Cases') +
  #   scale_color_viridis(discrete = T, option = 'D')
  # print(p2)
  
}

# higher lambdas don't do well with 1e4/5 or 1e5/5; difficult to get later peaks (see
# IS and SE in outbreak 1); in general, 1e4/5 seems to either collapse part way through
# or else have trouble picking up early cases, especially w/ higher lambdas; it really
# doesn't look like any one "group" is the best in all cases; denom of 10 might be most
# balanced?

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot fit S over time, vs. initial S:
load('syntheticTests/syntheticData/initStates_06-26.RData')
init.states.SEL <- init.states.SEL[1:21, to.keep]

rownames(init.states.SEL) <- countries
init.states.SEL <- melt(init.states.SEL)
names(init.states.SEL) <- c('country', 'outbreak', 'S0')

load('syntheticTests/syntheticData/synth_06-26_S.RData')
to.keep <- c(3, 5, 13, 19, 26)
synth.runs.S <- synth.runs.S[to.keep]

for (outbreak in 1:length(to.keep)) {
  susc_i <- synth.runs.S[[outbreak]]
  susc_i <- melt(susc_i)
  names(susc_i) <- c('week', 'country', 'S')
  susc_i$country <- countries[susc_i$country]
  
  oStates.temp <- oStates[oStates$outbreak == outbreak, ]
  oStates.temp$S <- oStates.temp$S * 100000

  p1 <- ggplot() + geom_line(data = susc_i, aes(x = week, y = S), lwd = 1.0) +
    geom_line(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
    geom_hline(data = init.states.SEL[init.states.SEL$outbreak == outbreak, ], aes(yintercept = S0 * 100000), lwd = 1.0, lty = 2) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = '% Susceptible') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p1)

  # # or limit to a few countries of interest:
  # oStates.temp <- oStates.temp[oStates.temp$country %in% c('IS', 'PT', 'SE', 'PL', 'CZ', 'ES'), ]; oStates.temp$country <- factor(oStates.temp$country)
  # init.states.SEL <- init.states.SEL[init.states.SEL$country %in% c('IS', 'PT', 'SE', 'PL', 'CZ', 'ES'), ]; init.states.SEL$country <- factor(init.states.SEL$country)
  # susc_i <- susc_i[susc_i$country %in% c('IS', 'PT', 'SE', 'PL', 'CZ', 'ES'), ]; susc_i$country <- factor(susc_i$country)
  # p2 <- ggplot() + geom_line(data = susc_i, aes(x = week, y = S), lwd = 1.0) +
  #   geom_line(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
  #   geom_point(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
  #   geom_hline(data = init.states.SEL[init.states.SEL$outbreak == outbreak, ], aes(yintercept = S0 * 100000), lwd = 1.0, lty = 2) +
  #   facet_wrap(~ country, scales = 'free_y') +
  #   theme_classic() + labs(x = 'Week', y = '% Susceptible') +
  #   scale_color_viridis(discrete = T, option = 'D')
  # print(p2)
  
}

# 1e4/5 seems to lead to lowest end S, while 1e5/5 and 1e5/20 are the highest; again, 1e4/5
# sometimes fails entirely, or else overestimates
# But overall, the model doesn't seem to be having trouble capturing S0; haven't checked
# actual S over time, though

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot parameter fit over time vs. true params:
load('syntheticTests/syntheticData/params_06-26.RData')
select.parms <- select.parms[to.keep, ]
select.parms$L <- select.parms$L * 365
select.parms <- as.data.frame(cbind(rep(1:5, 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

o <- o[o$oev_base == 1e5, ]; o$oev_base <- factor(o$oev_base); o$oev_denom <- factor(o$oev_denom)
# o$group <- paste(o$oev_base, o$oev_denom, sep = '_'); o$group <- factor(o$group)
# o$group <- factor(o$group, levels = levels(o$group)[c(1, 4, 2:3)])
# o$group <- factor(o$group, levels = levels(o$group)[c(3, 1:2, 6, 4:5)])
o$lambda <- factor(o$lambda)
o$group.plot <- paste(o$outbreak, o$run, o$oev_denom, o$lambda, sep = '_'); o$group.plot <- factor(o$group.plot)

o.plot <- o[o$week <= 30, ]
# o.plot <- o.plot[o.plot$group != '10000_5', ]; o.plot$group <- factor(o.plot$group)
# o.plot <- o.plot[o.plot$oev_base == 1e5, ]; o.plot$group <- factor(o.plot$group)

p1 <- ggplot(data = o.plot) +
  geom_hline(data = select.parms[select.parms$parameter == 'L', ], aes(yintercept = value), lwd = 1.0) +
  geom_point(aes(x = week, y = L, col = oev_denom, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = L, col = oev_denom, group = group.plot), lwd = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'L (days)') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE)
# print(p1)
p2 <- ggplot(data = o.plot) +
  geom_hline(data = select.parms[select.parms$parameter == 'D', ], aes(yintercept = value), lwd = 1.0) +
  geom_point(aes(x = week, y = D, col = oev_denom, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = D, col = oev_denom, group = group.plot), lwd = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'D (days)') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1') +
  scale_shape_discrete(guide = FALSE)
p3 <- ggplot(data = o.plot) +
  geom_hline(data = select.parms[select.parms$parameter == 'R0mx', ], aes(yintercept = value), lwd = 1.0) +
  geom_point(aes(x = week, y = R0max, col = oev_denom, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = R0max, col = oev_denom, group = group.plot), lwd = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0max') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE)
p4 <- ggplot(data = o.plot) +
  geom_hline(data = select.parms[select.parms$parameter == 'R0mn', ], aes(yintercept = value), lwd = 1.0) +
  geom_point(aes(x = week, y = R0min, col = oev_denom, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = R0min, col = oev_denom, group = group.plot), lwd = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0min') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE)
p5 <- ggplot(data = o.plot) +
  geom_hline(data = select.parms[select.parms$parameter == 'airScale', ], aes(yintercept = value), lwd = 1.0) +
  geom_point(aes(x = week, y = airScale, col = oev_denom, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = airScale, col = oev_denom, group = group.plot), lwd = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'airScale') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# actually, these don't look super bad? but maybe y-axes are large enough to obscure errors

# # Plot parameter sd over time:
# p1 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = L_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = L_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'L (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# # print(p1)
# p2 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = D_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = D_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'D (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p3 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = R0max_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = R0max_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0max (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p4 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = R0min_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = R0min_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0min (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p5 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = airScale_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = airScale_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'airScale (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# SD starts increasing near end for higher lambdas, where ens. var. is being inflated, even
# though it's still too sure of itself to change much, right?

# Calculate TRUE values of beta, R0, Re at each time point:
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, 1:21], ah[, 1:21])

tm_strt <- 273; tm_end <- 573; tm_step <- 1 #; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end
tmstep <- 7
beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + tmstep), by = tmstep)

AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)

true.betas = true.R0 = true.Re = vector('list', length(synth.runs.RATES))
for (i in 1:length(true.betas)) {
  parms.temp <- select.parms[select.parms$outbreak == i, ]
  s.temp <- synth.runs.S[[i]]
  
  betas.temp = r0.temp = re.temp = matrix(NA, nrow = dim(s.temp)[1], ncol = dim(s.temp)[2])
  
  b <- log(parms.temp$value[parms.temp$parameter == 'R0mx'] - parms.temp$value[parms.temp$parameter == 'R0mn'])
  a <- -180
  
  r0.temp <- exp(a * AHpt + b) + parms.temp$value[parms.temp$parameter == 'R0mn']
  beta.temp <- r0.temp / parms.temp$value[parms.temp$parameter == 'D']
  
  for (j in 1:n) {
    re.temp[, j] <- r0.temp[, j] * (s.temp[, j] / 100000)
  }
  
  true.betas[[i]] <- beta.temp
  true.R0[[i]] <- r0.temp
  true.Re[[i]] <- re.temp
}

# Calculate SIMULATED values of beta, R0, Re at each time point:
oStates$beta = oStates$R0 = oStates$Re = NA

for (i in 1:length(synth.runs.RATES)) {
  for (j in 1:length(unique(o$run))) {
    for (oev in levels(o$oev_denom)) {
      for (lambda in levels(o$lambda)) {
        
        o.temp <- o[o$outbreak == i & o$run == j & o$oev_denom == oev & o$lambda == lambda, ]
        oStates.temp <- oStates[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda, ]
        
        b <- log(o.temp$R0max - o.temp$R0min); a <- -180
        
        for (country in 1:n) {
          oStates$R0[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <- exp(a * AHpt[, country] + b) + o.temp$R0min
          oStates$beta[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <-
            oStates$R0[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] / o.temp$D
          oStates$Re[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <-
            oStates$R0[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] *
            oStates$S[oStates$outbreak == i & oStates$run == j & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]]
        }
        
      }
    }
  }
}

# Plot fit accuracy for beta, R0, Re:
for (outbreak in 1:length(to.keep)) {
  beta.temp <- true.betas[[outbreak]]; R0.temp <- true.R0[[outbreak]]; Re.temp <- true.Re[[outbreak]]
  rownames(beta.temp) = rownames(R0.temp) = rownames(Re.temp) = 1:(dim(beta.temp)[1])
  colnames(beta.temp) = colnames(R0.temp) = colnames(Re.temp) = 1:(dim(beta.temp)[2])
  beta.temp <- melt(beta.temp); R0.temp <- melt(R0.temp); Re.temp <- melt(Re.temp)
  names(beta.temp) = names(R0.temp) = names(Re.temp) = c('week', 'country', 'value')
  beta.temp$country <- countries[beta.temp$country]
  R0.temp$country <- countries[R0.temp$country]
  Re.temp$country <- countries[Re.temp$country]
  
  oStates.temp <- oStates[oStates$outbreak == outbreak, ]
  
  p1 <- ggplot() + geom_line(data = beta.temp, aes(x = week, y = value), lwd = 1.0) +
    geom_line(data = oStates.temp, aes(x = week, y = beta, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = beta, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
    facet_wrap(~ country) + #scale_y_continuous(limits = c(0.2, 0.8)) +
    theme_classic() + labs(x = 'Week', y = 'Beta', title = paste0('Outbreak ', outbreak)) +
    scale_color_viridis(discrete = T, option = 'D')
  print(p1)
  
  p2 <- ggplot() + geom_line(data = R0.temp, aes(x = week, y = value), lwd = 1.0) +
    geom_line(data = oStates.temp, aes(x = week, y = R0, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = R0, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = 'R0') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p2)
  
  p3 <- ggplot() + geom_line(data = Re.temp, aes(x = week, y = value), lwd = 1.0) +
    geom_line(data = oStates.temp, aes(x = week, y = Re, group = group.plot, col = oev_denom), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = Re, group = group.plot, col = oev_denom, pch = lambda), alpha = 0.5) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = 'Re') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p3)
  
}

# Plot distribution of relative param error at t=15 and t=20:
# o.err <- o[o$week %in% c(15, 20), c(1:7, 9, 11, 13, 15)]
o.err <- o[o$week == 20, c(1:7, 9, 11, 13, 15)]
o.err <- o.err[o.err$oev_base == 1e5 & o.err$lambda != 1.05, ]
o.err$L.err = o.err$D.err = o.err$R0mx.err = o.err$R0mn.err = o.err$aS.err = NA

# First, subtract actual - fit, and divide by observed
for (outbreak in 1:5) {
  o.err$L.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'L'] -
       o.err$L[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'L']
  
  o.err$D.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'D'] -
       o.err$D[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'D']
  
  o.err$R0mx.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'R0mx'] -
       o.err$R0max[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'R0mx']
  
  o.err$R0mn.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'R0mn'] -
       o.err$R0min[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'R0mn']
  
  o.err$aS.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'airScale'] -
       o.err$airScale[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == 1 & select.parms$parameter == 'airScale']
}

o.err$oev_denom <- factor(o.err$oev_denom)
# p1 <- ggplot(data = o.err) +
#   geom_histogram(aes(x = L.err, fill = oev_denom), binwidth = 0.05, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) +
#   theme_classic() + labs(x = 'Relative Error (L)', y = '', fill = 'OEV Denom.') +
#   scale_fill_brewer(palette = 'Set1') +
#   facet_grid(week ~ oev_denom)

p1 <- ggplot(data = o.err) +
  geom_histogram(aes(x = L.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (L)', y = '', main = 'Error at t=20') +
  scale_fill_brewer(palette = 'Set1')
p2 <- ggplot(data = o.err) +
  geom_histogram(aes(x = D.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (D)', y = '') +
  scale_fill_brewer(palette = 'Set1')
p3 <- ggplot(data = o.err) +
  geom_histogram(aes(x = R0mx.err), binwidth = 0.015, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0max)', y = '') +
  scale_fill_brewer(palette = 'Set1')# + facet_grid(outbreak ~ oev_denom)
p4 <- ggplot(data = o.err) +
  geom_histogram(aes(x = R0mn.err), binwidth = 0.015, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0min)', y = '') +
  scale_fill_brewer(palette = 'Set1')
p5 <- ggplot(data = o.err) +
  geom_histogram(aes(x = aS.err), binwidth = 0.01, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (airScale)', y = '') +
  scale_fill_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, p4, p5)
# not great...

# Plot distribution of relative error for S, beta, R0, Re for each country at t = 10, 15, 20:
oStates.err <- oStates[oStates$week %in% c(10, 15, 20), c(1:2, 4:8, 11:13)]
oStates.err$Re.err = oStates.err$R0.err = oStates.err$beta.err = oStates.err$S.err = NA
oStates.err$S <- oStates.err$S * 100000

for (outbreak in 1:length(synth.runs.RATES)) {
  susc_i <- synth.runs.S[[outbreak]]
  beta.temp <- true.betas[[outbreak]]
  R0.temp <- true.R0[[outbreak]]
  Re.temp <- true.Re[[outbreak]]
  
  for (run in 1:length(unique(oStates.err$run))) {
    for (oev in levels(oStates.err$oev)) {
      for (lambda in levels(oStates.err$lambda)) {
        
        for (count.index in 1:n) {
          oStates.err.temp <- oStates.err[oStates.err$outbreak == outbreak & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index], ]
          
          oStates.err$S.err[oStates.err$outbreak == outbreak & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
            (-1 * (susc_i[c(10, 15, 20), count.index] - oStates.err.temp$S)) / susc_i[c(10, 15, 20), count.index]
          oStates.err$beta.err[oStates.err$outbreak == outbreak & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
            (-1 * (beta.temp[c(10, 15, 20), count.index] - oStates.err.temp$beta)) / beta.temp[c(10, 15, 20), count.index]
          oStates.err$R0.err[oStates.err$outbreak == outbreak & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
            (-1 * (R0.temp[c(10, 15, 20), count.index] - oStates.err.temp$R0)) / R0.temp[c(10, 15, 20), count.index]
          oStates.err$Re.err[oStates.err$outbreak == outbreak & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
            (-1 * (Re.temp[c(10, 15, 20), count.index] - oStates.err.temp$Re)) / Re.temp[c(10, 15, 20), count.index]
        }
        
        
      }
    }
  }
}

oStates.err.10 <- oStates.err[oStates.err$week == 10, ]
oStates.err.15 <- oStates.err[oStates.err$week == 15, ]
oStates.err.20 <- oStates.err[oStates.err$week == 20, ]

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = S.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = S.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = S.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = S.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = S.err), binwidth = 0.03, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = S.err), binwidth = 0.05, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = beta.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = beta.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = beta.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = beta.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = beta.err), binwidth = 0.03, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = beta.err), binwidth = 0.05, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = R0.err), binwidth = 0.01, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = R0.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = R0.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = R0.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = R0.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = R0.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = Re.err), binwidth = 0.01, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = Re.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = Re.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = Re.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = Re.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = Re.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, ncol = 3)
# estimating Re to be way higher than it actually is in Iceland, and still missing later outbreaks?

# dev.off()









