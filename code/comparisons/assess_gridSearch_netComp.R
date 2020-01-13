
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Save plots?:
outputPlots <- FALSE
fileSuffix <- '_A(H1)'

# Restrict the forecast start weeks for which results are shown?
restrict.fc <- FALSE

# Set model labels:
m1.lab <- 'Network (0/2)'
m2.lab <- 'Network (0/2)(H3)'
m3.lab <- 'Indiv. (Old OEV)'

# Set locations of model results to be compared:
model1 <- 'results/A(H1)/network_NEW/'
model2 <- 'results/A(H3)/network_NEW/'
model3 <- 'results/A(H1)/indiv_mid/'

#########################################################################################################################################################
#########################################################################################################################################################

# Read in all plotting code:
source('code/comparisons/comp_netVsIndiv/plotting_functions.R')

# Read in and format metrics files:
m1 <- read.csv(file = paste0(model1, list.files(path = model1, pattern = 'Met_pro')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = 'Met_pro')))
m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = 'Met_pro')))

# m1 <- m1[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
# m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
# m3 <- m3[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]

m1$model <- m1.lab; m2$model <- m2.lab; m3$model <- m3.lab

m <- rbind(m1, m2, m3)
m$model <- factor(m$model, levels = c(m1.lab, m2.lab, m3.lab))
rm(m1, m2, m3)

m <- m[!is.na(m$onsetObs5), ]

### Equalize oev_base/oev_denom, just to avoid plotting errors ###
m$oev_base <- 0
m$oev_denom <- 2.0
##################################################################

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
if (outputPlots) {
  pdf(paste0('code/comparisons/plots/comp_byPred_', fileSuffix, '.pdf'), width = 14, height = 9)
  source('code/comparisons/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
  dev.off()
} else {
  source('code/comparisons/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
}
rm(plots.by.pred)

# Plot overall PT, PI, and OT by OBSERVED lead week:
if (outputPlots) {
  pdf(paste0('code/comparisons/plots/comp_byObs_', fileSuffix, '.pdf'), width = 14, height = 9)
  source('code/comparisons/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
  dev.off()
} else {
  source('code/comparisons/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
}
rm(plots.by.obs)

# Plot MAEs:
if (outputPlots) {
  pdf(paste0('code/comparisons/plots/comp_MAE_', fileSuffix,'.pdf'), width = 14, height = 9)
  source('code/comparisons/comp_netVsIndiv/plot_MAE.R')
  dev.off()
} else {
  source('code/comparisons/comp_netVsIndiv/plot_MAE.R')
}

# Read in all log scores files:
d1 <- read.csv(paste0(model1, list.files(path = model1, pattern = '_pt_ot')))
e.pi1 <- read.csv(paste0(model1, list.files(path = model1, pattern = '_pi_bin')))
e1 <- read.csv(paste0(model1, list.files(path = model1, pattern = '_1-4wks_bin')))

d2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_pt_ot')))
e.pi2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_pi_bin')))
e2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_1-4wks_bin')))

d3 <- read.csv(paste0(model3, list.files(path = model3, pattern = '_pt_ot')))
e.pi3 <- read.csv(paste0(model3, list.files(path = model3, pattern = '_pi_bin')))
e3 <- read.csv(paste0(model3, list.files(path = model3, pattern = '_1-4wks_bin')))

logs1 <- list(d1, e.pi1, e1)#, e.pi.alt1, e.alt1)
logs2 <- list(d2, e.pi2, e2)#, e.pi.alt2, e.alt2)
logs3 <- list(d3, e.pi3, e3)#, e.pi.alt3, e.alt3)

for (i in 1:3) {# 5) {
  logs1[[i]]$model <- m1.lab
  logs2[[i]]$model <- m2.lab
  logs3[[i]]$model <- m3.lab
}

d <- rbind(logs1[[1]], logs2[[1]], logs3[[1]])
e.pi <- rbind(logs1[[2]], logs2[[2]], logs3[[2]])
e <- rbind(logs1[[3]], logs2[[3]], logs3[[3]])
# e.pi.alt <- rbind(logs1[[4]], logs2[[4]], logs3[[4]])
# e.alt <- rbind(logs1[[5]], logs2[[5]], logs3[[5]])

##########################################
d$oev_base = 1.0; d$oev_denom = 1.0
e.pi$oev_base = 1.0; e.pi$oev_denom = 1.0
e$oev_base = 1.0; e$oev_denom = 1.0
##########################################

levels(d$metric) <- c('ot', 'pt')
rm(d1, d2, d3, e.pi1, e.pi2, e.pi3, e1, e2, e3, logs1, logs2, logs3)

# Plot log scores for PT, PI, OT, 1-4 weeks, by PREDICTED lead week:
# Question: Remove where obs are 0 for 1-4 weeks? Or where obs below some value?
# Question: Remove where no onset predicted before calculating these?
if (outputPlots) {
  pdf(paste0('code/comparisons/plots/comp_logScores_byPred_', fileSuffix, '.pdf'), width = 14, height = 9)
  byWeek <- 'Predicted'
  source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
  dev.off()
  pdf(paste0('code/comparisons/plots/comp_logScores_byObs_', fileSuffix, '.pdf'), width = 14, height = 9)
  byWeek <- 'Observed'
  source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
  dev.off()
} else {
  byWeek <- 'Predicted'
  source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
  byWeek <- 'Observed'
  source('code/comparisons/comp_netVsIndiv/plot_logScores.R')
}
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

rm(list = ls())





















