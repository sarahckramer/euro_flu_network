### Generate all plots in manuscript ###

if (!dir.exists('results/plots/')) {
  dir.create('results/plots/')
}

##########################################################################################################################################
##########################################################################################################################################

### Main text:
# Figure 1 is a map of countries used in the model + a model schematic; not generated in R

# Figure 2:
strain <- 'A(H1)'
plot_title <- ''
source('src/mainCode/analyses/plot_fits.R')
ggsave(file = 'results/plots/Fig2.svg', plot = p1, width = 13, height = 8)
rm(list = ls())

# Figure 3:
source('src/mainCode/analyses/plot_logScores.R')
# also covers S5-S7

# Figure 4:
source('src/mainCode/analyses/plot_calibration.R')

##########################################################################################################################################
##########################################################################################################################################

### Supplement:
# S1 Fig:
source('src/mainCode/format_obs/plot_syn+_by_subtype.R')

# S2 Fig:
source('src/mainCode/analyses/assess_covariance_and_divergence.R')
# also covers S3
# S3 Fig: See above

# S4 Fig:
strain <- 'A(H3)'
plot_title <- 'A'
source('src/mainCode/analyses/plot_fits.R')
ggsave(file = 'results/plots/FigS4_A.svg', plot = p1, width = 13, height = 8)
rm(list = ls())

strain <- 'B'
plot_title <- 'B'
source('src/mainCode/analyses/plot_fits.R')
ggsave(file = 'results/plots/FigS4_B.svg', plot = p1, width = 13, height = 8)
rm(list = ls())

# S5 Fig: See above
# S6 Fig: See above
# S7 Fig: See above

# S8 Fig:
source('src/mainCode/analyses/plot_MAE.R')
# also covers S9
# S9 Fig: See above

# S10 Fig:
source('src/mainCode/analyses/calibration_histograms.R')

# S11 Fig:
source('src/mainCode/analyses/synthetic_error.R')
# also covers S12
# S12 Fig: See above

# S13 Fig:
source('src/mainCode/analyses/check_onset_binary.R')

# S14 Fig:
source('src/mainCode/analyses/analyze_parameters-and-states.R')

print('DONE')
