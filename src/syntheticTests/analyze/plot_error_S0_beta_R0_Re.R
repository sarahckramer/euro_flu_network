### Plot error in S/beta/R0/Re over ALL timepoints ###

# Read in results with most everything calculated:
o <- read.csv('src/syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('src/syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv')

# Get countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

# Read in TRUE values of beta, R0, Re at each time point:
load('src/syntheticTests/syntheticData/for_synthetic_testing/true_betaR0Re_070220.RData')
true.betas <- true.list[[1]]
true.R0 <- true.list[[2]]
true.Re <- true.list[[3]]
rm(true.list)

# Read in true S:
load('src/syntheticTests/syntheticData/for_synthetic_testing/synth_S_toKeep_070220.RData')
for (i in 1:5) {
  synth.s[[i]] <- t(synth.s[[i]])
}

# Calculate error at ALL time points:
oStates$run <- factor(oStates$run)
oStates.err <- oStates[, c(1:3, 5:8, 12:13, 17:19)]
oStates.err$Re.err = oStates.err$R0.err = oStates.err$beta.err = oStates.err$S.err = NA

for (outbreak in 1:5) {
  print(outbreak)
  susc_i <- synth.s[[outbreak]]
  beta.temp <- true.betas[[outbreak]]
  R0.temp <- true.R0[[outbreak]]
  Re.temp <- true.Re[[outbreak]]
  
  for (run in levels(oStates.err$run)) {
    for (country in countries) {
      oStates.err.temp <- oStates.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country, ]
      
      oStates.err$S.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (susc_i[, which(countries == country)] - oStates.err.temp$S)) / susc_i[, which(countries == country)]
      oStates.err$beta.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (beta.temp[, which(countries == country)] - oStates.err.temp$beta)) / beta.temp[, which(countries == country)]
      oStates.err$R0.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (R0.temp[, which(countries == country)] - oStates.err.temp$R0)) / R0.temp[, which(countries == country)]
      oStates.err$Re.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (Re.temp[, which(countries == country)] - oStates.err.temp$Re)) / Re.temp[, which(countries == country)]
    }
  }
  
}
rm(oStates.err.temp)

# Plot!
oStates.err$season <- factor(oStates.err$season)
levels(oStates.err$season) <- c('Outbreak 1', 'Outbreak 2', 'Outbreak 3', 'Outbreak 4', 'Outbreak 5')
oStates.err$group <- paste(oStates.err$country, oStates.err$run, sep = '_'); oStates.err$group <- factor(oStates.err$group)

p1 <- ggplot(data = oStates.err, aes(x = week, y = S.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ season, nrow = 1) + labs(x = '', y = 'Relative Error (S)')
p2 <- ggplot(data = oStates.err, aes(x = week, y = beta.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ season, nrow = 1) + labs(x = '', y = 'Relative Error (Beta)')
p3 <- ggplot(data = oStates.err, aes(x = week, y = R0.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ season, nrow = 1) + labs(x = '', y = 'Relative Error (R0)')
p4 <- ggplot(data = oStates.err, aes(x = week, y = Re.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ season, nrow = 1) + labs(x = 'Weeks Since Outbreak Start', y = 'Relative Error (Re)')

# pdf('src/syntheticTests/outputs/errors_S_beta_R0_Re_070220.pdf', width = 14, height = 11)
grid.arrange(p1, p2, p3, p4, ncol = 1)
# dev.off()

# Clean up environment:
rm(list=ls())
