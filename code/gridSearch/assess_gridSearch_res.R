
# Read in all metrics files and merge:
file.list <- list.files('code/gridSearch/outputs/metrics/')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/metrics/', file.list[[i]]))
}

# Merge all files:
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(file.list, met.list, i)

# Remove where obs are NA:
m <- m[!is.na(m$pt.obs), ]

# So far, I have the sense that higher oev_base and lower oev_denom will perform better, as will those with lambda not equal to 1.05

# # Look at: PT acc, PI acc, corr, rmse by all 5 variables and by season:
# chisq.test(table(m$oev_base, m$pt.acc))
# chisq.test(table(m$oev_base, m$pi.acc))
# chisq.test(table(m$oev_fact, m$pt.acc))
# chisq.test(table(m$oev_fact, m$pi.acc))
# chisq.test(table(m$oev_denom, m$pt.acc))
# chisq.test(table(m$oev_denom, m$pi.acc))
# chisq.test(table(m$tmp_exp, m$pt.acc))
# chisq.test(table(m$tmp_exp, m$pi.acc))
# chisq.test(table(m$lambda, m$pt.acc))
# chisq.test(table(m$lambda, m$pi.acc))
# # all are sig
# 
# a <- glm(pt.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m, family = 'binomial')
# b <- glm(pi.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m, family = 'binomial')
# print(summary(a)) # higher oev_base, lower oev_denom/tmp_exp (relationship w/ tmp_exp much less strong)
# print(summary(b)) # lower oev_base, higher oev_denom, higher lambda
# 
# par(mfrow = c(5, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# boxplot(corrs ~ oev_base, data = m)
# boxplot(corrs ~ oev_fact, data = m)
# boxplot(corrs ~ oev_denom, data = m)
# boxplot(corrs ~ tmp_exp, data = m)
# boxplot(corrs ~ lambda, data = m)
# 
# boxplot(log(rmses) ~ oev_base, data = m)
# boxplot(log(rmses) ~ oev_fact, data = m)
# boxplot(log(rmses) ~ oev_denom, data = m)
# boxplot(log(rmses) ~ tmp_exp, data = m)
# boxplot(log(rmses) ~ lambda, data = m)
# # not really observing differences here
# 
# c <- glm(corrs ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m)
# d <- glm(rmses ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m)
# print(summary(c)) # higher oev_base, lower oev_denom/tmp_exp/lambda (associated with HIGHER corr)
# print(summary(d)) # higher oev_base, lower oev_fact/oev_denom/tmp_exp (associated with LOWER rmse)

m$group <- paste(m$oev_base, m$oev_fact, m$oev_denom, m$tmp_exp, m$lambda, sep = '_'); m$group <- factor(m$group)
m$pt.acc <- m$pt.acc == 'y'
m$pi.acc <- m$pi.acc == 'y'

pt.acc.grouped <- aggregate(pt.acc ~ group, data = m, FUN = mean)
pt.acc.grouped <- pt.acc.grouped[order(-pt.acc.grouped$pt.acc), ]
# most pretty high - if <50%, oev_denom > 10 and oev_base = 1e4
# top 3 (>80%): 1e5 or 1e4, oev_denom 1, others vary
pi.acc.grouped <- aggregate(pi.acc ~ group, data = m, FUN = mean)
pi.acc.grouped <- pi.acc.grouped[order(-pi.acc.grouped$pi.acc), ]

# aggregate(pt.acc ~ oev_base, data = m, FUN = mean) # 1e5 and 1e6 seem better
# aggregate(pt.acc ~ oev_fact, data = m, FUN = mean)
# aggregate(pt.acc ~ oev_denom, data = m, FUN = mean) # 1, 5, 10
# aggregate(pt.acc ~ tmp_exp, data = m, FUN = mean)
# aggregate(pt.acc ~ lambda, data = m, FUN = mean)
# 
# aggregate(pi.acc ~ oev_base, data = m, FUN = mean) # 1e4 and 1e5 seem better
# aggregate(pi.acc ~ oev_base, data = m, FUN = mean)
# aggregate(pi.acc ~ oev_base, data = m, FUN = mean)
# aggregate(pi.acc ~ oev_base, data = m, FUN = mean)
# aggregate(pi.acc ~ oev_base, data = m, FUN = mean)
# 
# # little difference by season
# 
# # Are parameters correlated with other parameters?
# chisq.test(table(m$oev_base, m$oev_fact))
# chisq.test(table(m$oev_base, m$oev_denom))
# chisq.test(table(m$oev_base, m$tmp_exp))
# chisq.test(table(m$oev_base, m$lambda))
# chisq.test(table(m$oev_fact, m$oev_denom))
# chisq.test(table(m$oev_fact, m$tmp_exp))
# chisq.test(table(m$oev_fact, m$lambda))
# chisq.test(table(m$oev_denom, m$tmp_exp))
# chisq.test(table(m$oev_denom, m$lambda))
# chisq.test(table(m$tmp_exp, m$lambda))
# # seem to be (all highly sig), but regression should control for these, right?
# 
# # And also assess this stuff by specific country:
# aggregate(pt.acc ~ countries, data = m, FUN = mean)[order(-aggregate(pt.acc ~ countries, data = m, FUN = mean)[, 2]), ] # worst: SK, DE, RO, SI, BE (<50%) [BE, DE, SK not great in individual runs either]
# aggregate(pi.acc ~ countries, data = m, FUN = mean)[order(-aggregate(pi.acc ~ countries, data = m, FUN = mean)[, 2]), ] # worst: SK, IS, HR, CZ, PT, RO, LU, BE (<40%) [PT and LU bad in individual, too]
# # this doesn't mean all bad - might be some combos that are much better
#  
# # So far, it looks like oev_base and oev_denom are the main influencers here
#       # to balance PT and PI, might want mid-range oev_base? (1e5); also lower oev_denom (1-10); maybe also lower tmp.exp?
#       # oev_fact and lambda have less influence - can be tuned later
# m$group <- paste(m$oev_base, m$oev_denom, m$tmp_exp, sep = '_'); m$group <- factor(m$group)
# aggregate(pt.acc ~ group, data = m[m$oev_denom %in% 1:10, ], FUN = mean) # no clear patterns
# aggregate(pi.acc ~ group, data = m[m$oev_denom %in% 1:10, ], FUN = mean) # maybe needs >1 oev_denom?
# aggregate(corrs ~ group, data = m[m$oev_denom %in% 1:10, ], FUN = mean) 
# aggregate(rmses ~ group, data = m[m$oev_denom %in% 1:10, ], FUN = mean)
# # basically don't mix low oev_base and high_oev_denom

# Read in all output files and merge:
file.list <- list.files('code/gridSearch/outputs/param_fits/')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/param_fits/', file.list[[i]]))
}

# Remove country incidence for now:
for (i in 1:length(file.list)) {
  op.list[[i]] <- op.list[[i]][, c(1:7, 29:33)]
}

# Add "time" variable:
for (i in 1:length(file.list)) {
  op.list[[i]]$time <- rep(1:40, 6)
}

# Merge all files:
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(file.list, op.list, i)

# Change variables to factors:
for (i in 2:7) {
  o[, i] <- factor(o[, i])
}

# # Look at inferred params by all 5 variables:
# # Plot by values?:
# # First by oev_denom:
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = L,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = L,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(365, 3650)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mx,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mn,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p3 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = airScale,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = L,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = L,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_denom)) +
#   theme_classic() + scale_y_continuous(limits = c(365, 3650)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_base)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_base)) +
#   theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mx,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_base)) +
#   theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mn,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_base)) +
#   theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p3 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = airScale,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_base)) +
#   theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_fact)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_fact)) +
#   theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mx,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_fact)) +
#   theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mn,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_fact)) +
#   theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p3 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = airScale,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = oev_fact)) +
#   theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = tmp_exp)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = tmp_exp)) +
#   theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mx,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = tmp_exp)) +
#   theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mn,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = tmp_exp)) +
#   theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p3 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = airScale,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = tmp_exp)) +
#   theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = lambda)) +
#   theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = D,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = lambda)) +
#   theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, ncol = 1)
# p1 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mx,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = lambda)) +
#   theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p2 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = R0mn,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = lambda)) +
#   theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# p3 <- ggplot(data = o) + geom_line(aes(x = time,
#                                        y = airScale,
#                                        group = paste(run, oev_denom, oev_fact,
#                                                      oev_base, tmp_exp, lambda),
#                                        col = lambda)) +
#   theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
#   facet_wrap(~ season)
# grid.arrange(p1, p2, p3, ncol = 1)

# Larger oev_denom values seem more likely to fall outside "realistic" ranges
# Oev_base 1e6 seems least likely to fail to get "realistic" params
# not really seeing patterns for oev_fact
# Important that most seem to fit perfectly realistic values

### Check pdfs:
# See notes

### Which countries also had low performance with individual country-level forecasts?:
# Some actually were quite bad (AT, DE, LU, PT, RO, SK, DK, SE), so this might still improve on some countries, but some are still bad (and don't always match those that were bad in Aim1 fits)

### Limit m/o to combinations found by visual inspection!
to.remove.m = to.remove.o = c()

for (i in 1:length(m$season)) {
  m.temp.parms <- m[i, 3:7]
  if (!file.exists(paste0('code/gridSearch/outputs/plots/any_success_both_seasons/plots_',
                          m.temp.parms[1], '_', m.temp.parms[2], '_', m.temp.parms[3], '_', m.temp.parms[4], '_', m.temp.parms[5],
                          '_2010-11_060519.pdf'))) {
    to.remove.m <- c(to.remove.m, i)
  }
}

for (i in 3:7) {
  o[, i] <- as.character(o[, i])
}
for (i in 1:length(o$season)) {
  o.temp.parms <- as.vector(o[i, 3:7])
  if (!file.exists(paste0('code/gridSearch/outputs/plots/any_success_both_seasons/plots_',
                          o.temp.parms[1], '_', o.temp.parms[2], '_', o.temp.parms[3], '_', o.temp.parms[4], '_', o.temp.parms[5],
                          '_2010-11_060519.pdf'))) {
    to.remove.o <- c(to.remove.o, i)
  }
}

rm(m.temp.parms, o.temp.parms, i)
m <- m[-to.remove.m, ]; o <- o[-to.remove.o, ]

# no 1e4 oev_base or 100 oev_denom

aggregate(pt.acc ~ oev_base, m, FUN = mean)
aggregate(pt.acc ~ oev_fact, m, FUN = mean)
aggregate(pt.acc ~ oev_denom, m, FUN = mean)
aggregate(pt.acc ~ tmp_exp, m, FUN = mean)
aggregate(pt.acc ~ lambda, m, FUN = mean)

aggregate(pi.acc ~ oev_base, m, FUN = mean)
aggregate(pi.acc ~ oev_fact, m, FUN = mean)
aggregate(pi.acc ~ oev_denom, m, FUN = mean)
aggregate(pi.acc ~ tmp_exp, m, FUN = mean)
aggregate(pi.acc ~ lambda, m, FUN = mean)

aggregate(corrs ~ oev_base, m, FUN = mean)
aggregate(corrs ~ oev_fact, m, FUN = mean)
aggregate(corrs ~ oev_denom, m, FUN = mean)
aggregate(corrs ~ tmp_exp, m, FUN = mean)
aggregate(corrs ~ lambda, m, FUN = mean)

aggregate(rmses ~ oev_base, m, FUN = mean)
aggregate(rmses ~ oev_fact, m, FUN = mean)
aggregate(rmses ~ oev_denom, m, FUN = mean)
aggregate(rmses ~ tmp_exp, m, FUN = mean)
aggregate(rmses ~ lambda, m, FUN = mean)

# PT acc looks highest for lambda 1.05 and lowest for lambda 1.00; PI acc lowest for eov_denom 1 and tmp_exp 1.5, also highest for lambda 1.05;
# no clear patterns for corr; RMSE lower as oev_denom increases and for lambda 1.05

a <- glm(pt.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m, family = 'binomial') # acc increases w/ lower oev_base and higher oev_denom/lambda
b <- glm(pi.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m, family = 'binomial') # same
c <- glm(corrs ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m) # same
d <- glm(rmses ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = m) # RMSE decreases with same params and in same direction

# So it looks like oev_fact and tmp_exp aren't important for accuracy anymore; focus on oev_base (lower), oev_denom (higher), lambda (higher)
# Do these trend together?
plot(table(m$oev_base, m$oev_denom)) # oev_denom 10/20 only used for oev_base 1e6; 1 almost never for 1e6
plot(table(m$oev_base, m$lambda)) # lower lambdas more common for 1e5; more even spread for 1e6
plot(table(m$oev_denom, m$lambda)) # for oev_denom 20, only lambdas 1 and 1.01; for 1 and 10, mostly 1 and 1.01; only for 5 more evenly distributed
plot(table(m$oev_base, m$oev_denom, m$lambda)) # 1e6/5/1-1.01 seem most flexible values

pt.acc.grouped <- aggregate(pt.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, m, FUN = mean)
pt.acc.grouped <- pt.acc.grouped[order(-pt.acc.grouped$pt.acc), ]
pi.acc.grouped <- aggregate(pi.acc ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, m, FUN = mean)
pi.acc.grouped <- pi.acc.grouped[order(-pi.acc.grouped$pi.acc), ]

par(mfrow = c(5, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(pt.acc.grouped$oev_base, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_base')
plot(pt.acc.grouped$oev_fact, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_fact')
plot(pt.acc.grouped$oev_denom, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_denom')
plot(pt.acc.grouped$tmp_exp, pch = 20, xlab = '', xaxt = 'n', ylab = 'Tmp_exp')
plot(pt.acc.grouped$lambda, pch = 20, xlab = '', xaxt = 'n', ylab = 'Lambda')
# best seem to be 1e5/1-10

plot(pi.acc.grouped$oev_base, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_base')
plot(pi.acc.grouped$oev_fact, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_fact')
plot(pi.acc.grouped$oev_denom, pch = 20, xlab = '', xaxt = 'n', ylab = 'OEV_denom')
plot(pi.acc.grouped$tmp_exp, pch = 20, xlab = '', xaxt = 'n', ylab = 'Tmp_exp')
plot(pi.acc.grouped$lambda, pch = 20, xlab = '', xaxt = 'n', ylab = 'Lambda')
# best seem to be 1e5/0.1-0.2/5or20/any/1-1.01

# Any combos in top 10 for each?
print(cbind(head(pt.acc.grouped, 10), head(pi.acc.grouped, 10)))
# 1e5/0.5/1/1/1.05; 1e6/1/10/1.5/1.05; 1e5/0.2/5/2/1; 1e6/1/5/1/1.05; 1e6/1/10/1/1.05; 1e6/1/20/1/1.01
# any of these identified by visual inspection as among the "best"? - 1e5/0.2/5/2/1; 1e6/1/10/1.5/1.05; 1e6/1/20/1/1.01

print(unique(m[m$oev_base == 1e5 & m$oev_denom == 5 & m$lambda == 1.00, 3:7])) # also in top: 0.2//1, 1.0//1
print(unique(m[m$oev_base == 1e5 & m$oev_denom == 1 & m$lambda == 1.05, 3:7])) # also in top: NA
print(unique(m[m$oev_base == 1e6 & m$oev_denom == 5 & m$lambda == 1.05, 3:7])) # also in top: 0.1//1.5, 0.2//2.0
print(unique(m[m$oev_base == 1e6 & m$oev_denom == 10 & m$lambda == 1.05, 3:7])) # also in top: see below
print(unique(m[m$oev_base == 1e6 & m$oev_denom == 10 & m$lambda == 1.05, 3:7])) # also in top: see above
print(unique(m[m$oev_base == 1e6 & m$oev_denom == 20 & m$lambda == 1.01, 3:7])) # also in top: 0.2//1.5, 0.5//1.0

# Be sure to run more seasons of combos that worked! See which (if any) work for ALL seasons
parms.to.test <- unique(m[, 3:7]) # 42 combos (vs. 384 possible permutations)
# write.csv(parms.to.test, file = 'code/gridSearch/outputs/successful_combos.csv', row.names = FALSE)

# Check parameter fit as well:
o$group <- paste(o$run, o$oev_base, o$oev_fact, o$oev_denom, o$tmp_exp, o$lambda, sep = '_'); o$group <- factor(o$group)

# Do they seem to differ based on any params?
for (i in 3:7) {
  o[, i] <- factor(o[, i])
}
levels(o$oev_denom)
o$oev_denom <- factor(o$oev_denom, levels = levels(o$oev_denom)[c(1, 4, 2:3)])

m1 <- glm(L ~ season + oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = o) # all sig
m2 <- glm(D ~ season + oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = o) # all sig except tmp_exp
m3 <- glm(R0mx ~ season + oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = o) # all sig (but less strong for tmp_exp, and only for 1.5)
m4 <- glm(R0mn ~ season + oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = o) # all sig except tmp_exp 1.5, lambda 1.01
m5 <- glm(airScale ~ season + oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = o) # all sig except tmp_exp, lambda 1.01 vs 1, oev_fact 0.2/0.5 vs. 0.1

# L: only changes >100 are season and lambda 1.03/1.05; estimated L is higher (and looks less settled? at higher values of lambda); higher and better agreement for 2011-12
# D: one outlier in 2010-11, but overall good agreement (again, better in 2011-12); above 0.3: lower for 2011-12, higher for 1e6, lower for oev_denom 10/20 (but these are tiny changes)
# R0mx: about 0.168 lower for 2011-12 vs 2010-11; also ~0.2 lower for lambdas 1.03 and 1.05; better agreement in 2011-12 but good overall
# R0mn: sig differences all on scale of 0.05 or less; similar for both seasons, but more agreement in 2011-12; lower for oev_base 1e6 and higher with increasing oev_denom (10/20 same)
# airScale: largest differences are higher for higher oev_denom and lower for oev_base 1e6; significantly but only 0.015 lower for 2011-12; higher agreement in 2011-12
# Note with differences based on oev_base and oev_denom that positivity fails

aggregate(L ~ season + lambda, data = o, FUN = mean) # 2010-11: 1766, 1796, 2140, 2443; 2011-12: 1945, 1952, 2321, 2534 (so spanning 4.8-7.0 years about)
aggregate(D ~ season, data = o, FUN = mean) # 2010-11: 3.97; 2011-12: 3.63
aggregate(R0mx ~ season, data = o, FUN = mean) # 10-11: 2.42; 11-12: 2.25
aggregate(R0mn ~ season, data = o, FUN = mean) # 10-11: 1.023; 11;12: 0.992
aggregate(airScale ~ season, data = o, FUN = mean) # 10-11: 1.024; 11;12: 1.008
# note that these are averages over entire time stream - look instead at o[o$time > 15, ]? Or just search through more thoroughly later

p1 <- ggplot(data = o) + geom_line(aes(x = time, y = L, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time, y = L, group = group, col = oev_fact)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time, y = L, group = group, col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p4 <- ggplot(data = o) + geom_line(aes(x = time, y = L, group = group, col = tmp_exp)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p5 <- ggplot(data = o) + geom_line(aes(x = time, y = L, group = group, col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time, y = D, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time, y = D, group = group, col = oev_fact)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time, y = D, group = group, col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p4 <- ggplot(data = o) + geom_line(aes(x = time, y = D, group = group, col = tmp_exp)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p5 <- ggplot(data = o) + geom_line(aes(x = time, y = D, group = group, col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mx, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mx, group = group, col = oev_fact)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mx, group = group, col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p4 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mx, group = group, col = tmp_exp)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p5 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mx, group = group, col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mn, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mn, group = group, col = oev_fact)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mn, group = group, col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p4 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mn, group = group, col = tmp_exp)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p5 <- ggplot(data = o) + geom_line(aes(x = time, y = R0mn, group = group, col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time, y = airScale, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time, y = airScale, group = group, col = oev_fact)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time, y = airScale, group = group, col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p4 <- ggplot(data = o) + geom_line(aes(x = time, y = airScale, group = group, col = tmp_exp)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p5 <- ggplot(data = o) + geom_line(aes(x = time, y = airScale, group = group, col = lambda)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

# Also just for "top" combos:
o.red <- o[(o$oev_base == 1e5 & o$oev_fact == 0.2 & o$oev_denom == 5 & o$tmp_exp == 2 & o$lambda == 1) |
             (o$oev_base == 1e5 & o$oev_fact == 0.5 & o$oev_denom == 1 & o$tmp_exp == 1 & o$lambda == 1.05) |
             (o$oev_base == 1e6 & o$oev_fact == 1 & o$oev_denom == 5 & o$tmp_exp == 1 & o$lambda == 1.05) |
             (o$oev_base == 1e6 & o$oev_fact == 1 & o$oev_denom == 10 & o$tmp_exp == 1 & o$lambda == 1.05) |
             (o$oev_base == 1e6 & o$oev_fact == 1 & o$oev_denom == 10 & o$tmp_exp == 1.5 & o$lambda == 1.05) |
             (o$oev_base == 1e6 & o$oev_fact == 1 & o$oev_denom == 20 & o$tmp_exp == 1 & o$lambda == 1.01), ]

p1 <- ggplot(data = o.red) + geom_line(aes(x = time, y = L, group = group, col = paste(oev_base, oev_denom, tmp_exp, sep = '_'))) +
  theme_classic() + labs(x = 'Time', col = 'Group') + facet_wrap(~ season)
p2 <- ggplot(data = o.red) + geom_line(aes(x = time, y = D, group = group, col = paste(oev_base, oev_denom, tmp_exp, sep = '_'))) +
  theme_classic() + labs(x = 'Time', col = 'Group') + facet_wrap(~ season)
p3 <- ggplot(data = o.red) + geom_line(aes(x = time, y = R0mx, group = group, col = paste(oev_base, oev_denom, tmp_exp, sep = '_'))) +
  theme_classic() + labs(x = 'Time', col = 'Group') + facet_wrap(~ season)
p4 <- ggplot(data = o.red) + geom_line(aes(x = time, y = R0mn, group = group, col = paste(oev_base, oev_denom, tmp_exp, sep = '_'))) +
  theme_classic() + labs(x = 'Time', col = 'Group') + facet_wrap(~ season)
p5 <- ggplot(data = o.red) + geom_line(aes(x = time, y = airScale, group = group, col = paste(oev_base, oev_denom, tmp_exp, sep = '_'))) +
  theme_classic() + labs(x = 'Time', col = 'Group') + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

# higher lambda makes L higher and less likely to "settle"; L and airScale seem most variable here
# pink and yellow lines represent more flexible combos, but can be outliers

# At this point, we have several (42) potential parameter combinations for OEV; will need to eventually narrow down, partially by expanding to other seasons









