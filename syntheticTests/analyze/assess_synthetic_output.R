
# Read in results:
m <- read.csv('syntheticTests/outputs/cluster/071519/outputMet.csv')
o <- read.csv('syntheticTests/outputs/cluster/071519/outputOP.csv')
oStates <- read.csv('syntheticTests/outputs/cluster/071519/outputOPStates_beta-R0-Re.csv')

# Check what combos ran:
table(m$oev_base, m$oev_denom)

# Remove combos that lead to collapse:
m <- m[m$oev_base == 1e5 | (m$oev_base == 1e4 & m$oev_denom %in% c(5, 10) & m$lambda %in% c(1.0, 1.01)), ]
o <- o[o$oev_base == 1e5 | (o$oev_base == 1e4 & o$oev_denom %in% c(5, 10) & o$lambda %in% c(1.0, 1.01)), ]
# oStates <- oStates[oStates$oev_base == 1e5 | (oStates$oev_base == 1e4 & oStates$oev_denom %in% c(5, 10) & oStates$lambda %in% c(1.0, 1.01)), ]

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
# #      outbreak country onsetObs
# # 2714        9      PT       NA
# # 2715        9      RO       NA
# # 4094       13      PT       NA
# m <- m[!is.na(m$onsetObs), ]
# # oStates <- oStates[!(oStates$outbreak == 9 & oStates$country %in% c('PT', 'RO')) & !(oStates$outbreak == 13 & oStates$country == 'PT'), ]
# 
# # test pt_acc by various factors:
# chisq.test(m$pt_acc, m$outbreak) # sig
# chisq.test(m$pt_acc, m$run)
# chisq.test(m$pt_acc, m$oev_base)
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
# m1 <- glm(pt_acc ~ oev_denom, data = m, family = 'binomial') # 10 and 20 better
# m2 <- glm(pt_acc ~ oev_base, data = m, family = 'binomial') # not sig
# 
# m1 <- glm(pt_acc ~ oev_denom, data = m[m$oev_base == 1e5, ], family = 'binomial') # 10 and 20 still better; 10 better for 1e4, too
# # m2 <- glm(pt_acc ~ oev_base, data = m[m$oev_denom == 5, ], family = 'binomial') # oev_base itself not sig different for oev_denom = 5
# 
# # test pi_acc by various factors:
# chisq.test(m$pi_acc, m$outbreak)
# chisq.test(m$pi_acc, m$run)
# chisq.test(m$pi_acc, m$oev_base)
# chisq.test(m$pi_acc, m$oev_denom) # sig
# chisq.test(m$pi_acc, m$lambda) # sig
# chisq.test(m$pi_acc, m$country) # sig
# chisq.test(m$pi_acc, m$obs_pkwk) # sig
# chisq.test(m$pi_acc, m$obs_peak_int) # sig
# chisq.test(m$pi_acc, m$onsetObs) # sig
# 
# m1 <- glm(pi_acc ~ oev_denom, data = m, family = 'binomial') # 10 and 20 both better
# m2 <- glm(pi_acc ~ oev_base, data = m, family = 'binomial') # not sig
# 
# m1 <- glm(pi_acc ~ oev_denom, data = m[m$oev_base == 1e4, ], family = 'binomial') # same; 10 better for 1e4, too
# # m2 <- glm(pi_acc ~ oev_base, data = m[m$oev_denom == 5, ], family = 'binomial') # oev_base 1e5 actually worse here, when looking only at oev_denom 5
# 
# # account for lambda?
# # m1 <- glm(pt_acc ~ oev_base + oev_denom + lambda, data = m, family = 'binomial') # lambda not sig
# m1 <- glm(pt_acc ~ oev_denom + lambda, data = m[m$oev_base == 1e4, ], family = 'binomial') # lambda not sig
# m2 <- glm(pt_acc ~ oev_denom + lambda, data = m[m$oev_base == 1e5, ], family = 'binomial') # lambda not sig
# m1 <- glm(pi_acc ~ oev_denom + lambda, data = m[m$oev_base == 1e4, ], family = 'binomial') # lambda not sig
# m2 <- glm(pi_acc ~ oev_denom + lambda, data = m[m$oev_base == 1e5, ], family = 'binomial') # 1.03 better, 1.05 borderline
# # so higher oev_denom better, and lambda should be higher when oev_base is higher
# 
# # assess by "group"?
# aggregate(pi_acc ~ oev_base + oev_denom + lambda, data = m, FUN = table)
# m$group <- paste(m$oev_base, m$oev_denom, m$lambda, sep = '_'); m$group <- factor(m$group)
# m1 <- glm(pi_acc ~ group, data = m, family = 'binomial')
# # best seem to be 1e5/20/any?
# 
# # # differences by outbreak/run?:
# # m1 <- glm(pt_acc ~ outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial')
# # m2 <- glm(pi_acc ~ outbreak + run + oev_denom + lambda, data = m.best, family = 'binomial')
# # # nope!
# 
# # differences by country:
# m1 <- glm(pt_acc ~ country + outbreak + run + oev_base + oev_denom + lambda, data = m, family = 'binomial') # BE, FR, HU, IE, PL, PT, SI way worse; UK better
# m2 <- glm(pi_acc ~ country + outbreak + run + oev_base + oev_denom + lambda, data = m, family = 'binomial') # BE, FR, HU, IE, PL, PT, SI way worse; DK and UK best
# # So be sure to look at the synthetic data for these countries! - tend to have later and smaller peaks, but so do a lot of countries that do fine
# 
# # pt_acc and pi_acc positively assoc.?:
# chisq.test(table(m$pt_acc, m$pi_acc))
# chisq.test(table(m$pt_acc, m$pi_acc))$residuals
# # yes!
# 
# # Worse for later peaks?
# m1 <- glm(pt_acc ~ obs_pkwk + obs_peak_int + outbreak + run + oev_base + oev_denom + lambda, data = m, family = 'binomial') # later/larger peak = MORE accuracy?
# m2 <- glm(pi_acc ~ obs_pkwk + obs_peak_int + outbreak + run + oev_base + oev_denom + lambda, data = m, family = 'binomial') # later/larger peak = LESS accuracy (although barely sig for intensity)
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
