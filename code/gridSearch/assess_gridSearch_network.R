
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Save plots?:
outputPlots <- FALSE
pdf('results/gridSearch/indiv_grid_OEVold_B.pdf', width = 14, height = 9)

# Restrict the forecast start weeks for which results are shown?
restrict.fc <- FALSE

# Set locations of model results:
model.loc <- 'results/gridSearch/indiv_grid_OEVold_B/'

# Stipulate that grid search:
gridSearch <- TRUE

#########################################################################################################################################################
#########################################################################################################################################################

# Read in all plotting code:
source('code/comparisons/comp_netVsIndiv/plotting_functions.R')

# Read in and format metrics file:
m <- read.csv(file = paste0(model.loc, list.files(path = model.loc, pattern = 'Met_pro')))
m <- m[!is.na(m$onsetObs5), ]

# Trick pre-existing code into giving me results?:
m$model <- m$oev_base

# Continue formatting:
m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)
m$model <- factor(m$model)
m$run <- factor(m$run)

# Calculate MAEs:
m$abs_err_pkwk <- abs(m$delta_pkwk_mean)
m$abs_err_int_perc <- (abs(m$intensity_err) / m$obs_peak_int) * 100 # b/c of differences in surveillance, needs to be a percentage
m$abs_err_onset <- abs(m$delta_onset5)

m$abs_err_1wk_perc <- (abs(m$fcast_1week - m$obs_1week) / m$obs_1week) * 100
m$abs_err_2wk_perc <- (abs(m$fcast_2week - m$obs_2week) / m$obs_2week) * 100
m$abs_err_3wk_perc <- (abs(m$fcast_3week - m$obs_3week) / m$obs_3week) * 100
m$abs_err_4wk_perc <- (abs(m$fcast_4week - m$obs_4week) / m$obs_4week) * 100

m$abs_err_1wk_perc[m$abs_err_1wk_perc == Inf & !is.na(m$abs_err_1wk_perc)] <- NA
m$abs_err_2wk_perc[m$abs_err_2wk_perc == Inf & !is.na(m$abs_err_2wk_perc)] <- NA
m$abs_err_3wk_perc[m$abs_err_3wk_perc == Inf & !is.na(m$abs_err_3wk_perc)] <- NA
m$abs_err_4wk_perc[m$abs_err_4wk_perc == Inf & !is.na(m$abs_err_4wk_perc)] <- NA

# Plot overall PT, PI, and OT by PREDICTED lead week:
source('code/comparisons/comp_netVsIndiv/by_pred.R')
print(plots.by.pred)
rm(plots.by.pred)

# Plot overall PT, PI, and OT by OBSERVED lead week:
source('code/comparisons/comp_netVsIndiv/by_obs.R')
print(plots.by.obs)
rm(plots.by.obs)

# Plot MAEs:
source('code/comparisons/comp_netVsIndiv/plot_MAE.R')

# Read in all log scores files:
d <- read.csv(paste0(model.loc, list.files(path = model.loc, pattern = '_pt_ot')))
e.pi <- read.csv(paste0(model.loc, list.files(path = model.loc, pattern = '_pi_bin')))
e <- read.csv(paste0(model.loc, list.files(path = model.loc, pattern = '_1-4wks_bin')))

d$model <- d$oev_base
e.pi$model <- e.pi$oev_base
e$model <- e$oev_base

levels(d$metric) <- c('ot', 'pt')

d$model <- factor(d$model)
e.pi$model <- factor(e.pi$model)
e$model <- factor(e$model)

# Plot log scores for PT, PI, OT, 1-4 weeks, by PREDICTED lead week:
# Question: Remove where obs are 0 for 1-4 weeks? Or where obs below some value?
# Question: Remove where no onset predicted before calculating these?
byWeek <- 'Predicted'
source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
byWeek <- 'Observed'
source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
rm(d, e.pi, e, byWeek)

# # Plot calibration for PT, PI, OT, and 1-4 weeks:
# if (outputPlots) {
#   pdf('code/comparisons/plots/comp_calib.pdf', width = 14, height = 9)
#   source('code/comparisons/comp_netVsIndiv/plot_calibrationMethod2.R')
#   dev.off()
# } else {
#   source('code/comparisons/comp_netVsIndiv/plot_calibrationMethod2.R')
# }

# # Plot inferred parameter values at each time step (network only - individual allows parameter values to differ by country):
# o <- read.csv(paste0(model1, list.files(path = model1, pattern = 'OPParams')))
# o$group <- paste(o$oev_base, o$oev_denom, o$lambda, o$season, o$run, o$fc_start, sep = '_')
# o$group <- factor(o$group)
# o$oev_base <- factor(o$oev_base)
# 
# p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base), alpha = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'L', col = 'OEV Base') + facet_wrap(~ lambda) +
#   scale_color_brewer(palette = 'Set1')
# p2 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base), alpha = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'D', col = 'OEV Base') + facet_wrap(~ lambda) +
#   scale_color_brewer(palette = 'Set1')
# p3 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base), alpha = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0max', col = 'OEV Base') + facet_wrap(~ lambda) +
#   scale_color_brewer(palette = 'Set1')
# p4 <- ggplot(data = o) + geom_line(aes(x = week, y = R0diff, group = group, col = oev_base), alpha = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0diff', col = 'OEV Base') + facet_wrap(~ lambda) +
#   scale_color_brewer(palette = 'Set1')
# p5 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base), alpha = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'airScale', col = 'OEV Base') + facet_wrap(~ lambda) +
#   scale_color_brewer(palette = 'Set1')
# 
# if (outputPlots) {
#   pdf('code/comparisons/plots/param_ests.pdf', width = 14, height = 14)
#   grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
#   dev.off()
# } else {
#   grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# }

dev.off()

rm(list = ls())





















