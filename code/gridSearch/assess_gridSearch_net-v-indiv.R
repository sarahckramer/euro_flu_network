
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Save plots?:
outputPlots <- FALSE

# Read in all plotting code:
source('code/gridSearch/comp_netVsIndiv/plotting_functions.R')

# Set locations of 2 model results to be compared:
model1 <- 'results/original/'
model2 <- 'results/indiv_orig/'

# Read in and format metrics files:
# Question: Include only points where forecasts are made in both "models" (so, remove "extra" 20,000), in order to do fair comparison? (Probably.)
source('code/gridSearch/comp_netVsIndiv/readIn_metrics.R')

### Can swap out original network results with any other network results; for individual, also try running: 1. with old OEV (old results), 2. with new param ranges ###

# Question: Not seeing much impact at all of oev_denom, and relatively little of lambda - can we choose a single value?
# Plot overall PT, PI, and OT by PREDICTED lead week:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_byPred.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
}
rm(plots.by.pred)

# Plot overall PT, PI, and OT by OBSERVED lead week:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_byObs.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
}
rm(plots.by.obs)

# Plot MAEs:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_MAE.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/plot_MAE.R')
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/plot_MAE.R')
}

# Read in all log scores files:
source('code/gridSearch/comp_netVsIndiv/readIn_logScores.R')

# Plot log scores for PT, PI, OT, 1-4 weeks, by PREDICTED lead week:
# Question: Remove where obs are 0 for 1-4 weeks? Or where obs below some value?
# Question: Remove where no onset predicted before calculating these?
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_logScores_byPred.pdf', width = 14, height = 9)
  byWeek <- 'Predicted'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  dev.off()
  pdf('code/gridSearch/plots/comp_logScores_byObs.pdf', width = 14, height = 9)
  byWeek <- 'Observed'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  dev.off()
} else {
  byWeek <- 'Predicted'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  byWeek <- 'Observed'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
}
rm(d, e.pi, e, byWeek)

# # Plot calibration for PT, PI, OT, and 1-4 weeks:
# if (outputPlots) {
#   pdf('code/gridSearch/plots/comp_calib.pdf', width = 14, height = 9)
#   source('code/gridSearch/comp_netVsIndiv/plot_calibrationMethod2.R')
#   dev.off()
# } else {
#   source('code/gridSearch/comp_netVsIndiv/plot_calibrationMethod2.R')
# }

# Plot inferred parameter values at each time step (network only - individual allows parameter values to differ by country):
o <- read.csv('results/original/outputOPParams_110819.csv')
o$group <- paste(o$oev_base, o$oev_denom, o$lambda, o$season, o$run, o$fc_start, sep = '_')
o$group <- factor(o$group)
o$oev_base <- factor(o$oev_base)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'L', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'D', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0max', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p4 <- ggplot(data = o) + geom_line(aes(x = week, y = R0diff, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0diff', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p5 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'airScale', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')

if (outputPlots) {
  pdf('code/gridSearch/plots/param_ests.pdf', width = 14, height = 14)
  grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
  dev.off()
} else {
  grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
}
























