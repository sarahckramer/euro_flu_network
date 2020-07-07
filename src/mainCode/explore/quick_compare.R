### Allows quick, plotted comparisons of up to three model runs (network, isolated, different oev_denom/lambda, etc.) ###

# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Save plots?:
# outputPlots <- FALSE
fileSuffix <- 'B'

# Restrict the forecast start weeks for which results are shown to weeks AFTER conditions for season inclusion met?
restrict.fc <- FALSE

# Set model labels:
m1.lab <- 'Network'
m2.lab <- 'Isolated'
m3.lab <- 'Isolated (Repeat)'

# Set locations of model results to be compared:
model1 <- 'results/by_subtype/network_B/'
model2 <- 'results/by_subtype/isolated_B/'
model3 <- 'results/by_subtype/isolated_B/'

pdf(paste0('results/plots/comp_', fileSuffix, '.pdf'), width = 14, height = 9)

#########################################################################################################################################################
#########################################################################################################################################################

# Read in all plotting code:
source('src/mainCode/explore/quick_compare/plotting_functions.R')

# Read in and format metrics files:
m1 <- read.csv(file = paste0(model1, list.files(path = model1, pattern = 'Met_pro_P')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = 'Met_pro_P')))
m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = 'Met_pro_P')))

m1$model <- m1.lab; m2$model <- m2.lab; m3$model <- m3.lab

m <- rbind(m1, m2, m3)
m$model <- factor(m$model, levels = c(m1.lab, m2.lab, m3.lab))
rm(m1, m2, m3)

m <- m[!is.na(m$onsetObs5), ]

# ### Equalize oev_base/oev_denom, just to avoid plotting errors ###
# m$oev_base <- 0
# m$oev_denom <- 2.0
# ##################################################################
# The way the code is set up, plots are faceted by oev_denom; to
# compare model runs that differ by oev_denom, may want to set "model"
# equal to the oev_denom values, and equalize oev_denom variable
# in the dataframes, as a rough workaround

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
source('src/mainCode/explore/quick_compare/by_pred.R')
print(plots.by.pred)
rm(plots.by.pred)

# Plot overall PT, PI, and OT by OBSERVED lead week:
source('src/mainCode/explore/quick_compare/by_obs.R')
print(plots.by.obs)
rm(plots.by.obs)

# Plot MAEs:
source('src/mainCode/explore/quick_compare/plot_MAE.R')

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
byWeek <- 'Predicted'
source('src/mainCode/explore/quick_compare/plot_logScores.R')
byWeek <- 'Observed'
source('src/mainCode/explore/quick_compare/plot_logScores.R')
# byWeek <- 'Observed_All'
# source('mainCode/comparisons/plot_logScores.R')
rm(d, e.pi, e, byWeek)

dev.off()

rm(list = ls())





















