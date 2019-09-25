
# Read in results with most everything calculated:
load('syntheticTests/outputs/cluster/071519/res_loop_S0I0range.RData')

m <- res[[1]]
o <- res[[2]]
o.err <- res[[3]]
oStates <- res[[4]]
oStates.err <- res[[5]]
rm(res)

# Get countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

# Get outbreaks:
to.keep <- c(1, 6, 9, 13)

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/outputs/cluster/071519/true_betaR0Re.RData')
true.betas <- true.epi.params[[1]]
true.R0 <- true.epi.params[[2]]
true.Re <- true.epi.params[[3]]
rm(true.epi.params)

# Read in true S:
load('syntheticTests/syntheticData/synth_07-14_S.RData')
synth.runs.S <- synth.S.RATES[to.keep]

# Limit df to desired oev_base/oev_denom/lambda:
oStates <- oStates[oStates$oev_base == 1e4 & oStates$oev_denom == 10 & oStates$lambda == 1.03, ]

# Calculate error at ALL time points:
oStates.err <- oStates[, c(1:2, 6:8, 12:14)]
oStates.err$Re.err = oStates.err$R0.err = oStates.err$beta.err = oStates.err$S.err = NA
oStates.err$S <- oStates.err$S * 100000

for (outbreak in 1:length(to.keep)) {
  print(outbreak)
  susc_i <- synth.runs.S[[outbreak]]
  beta.temp <- true.betas[[outbreak]]
  R0.temp <- true.R0[[outbreak]]
  Re.temp <- true.Re[[outbreak]]
  
  for (run in 1:length(unique(oStates.err$run))) {
    for (count.index in 1:n) {
      oStates.err.temp <- oStates.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$country == countries[count.index], ]
      
      if (length(oStates.err.temp$country) > 0) {
        
        oStates.err$S.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$country == countries[count.index]] <-
          (-1 * (susc_i[, count.index] - oStates.err.temp$S)) / susc_i[, count.index]
        oStates.err$beta.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$country == countries[count.index]] <-
          (-1 * (beta.temp[, count.index] - oStates.err.temp$beta)) / beta.temp[, count.index]
        oStates.err$R0.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$country == countries[count.index]] <-
          (-1 * (R0.temp[, count.index] - oStates.err.temp$R0)) / R0.temp[, count.index]
        oStates.err$Re.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$country == countries[count.index]] <-
          (-1 * (Re.temp[, count.index] - oStates.err.temp$Re)) / Re.temp[, count.index]
        
      }
      
    }
  }
  
}
rm(oStates.err.temp)

# Plot!
oStates.err$group <- paste(oStates.err$country, oStates.err$run, sep = '_'); oStates.err$group <- factor(oStates.err$group)
oStates.err$outbreak <- factor(oStates.err$outbreak)
levels(oStates.err$outbreak) <- c('Outbreak #1', 'Outbreak #2', 'Outbreak #3', 'Outbreak #4')



p1 <- ggplot(data = oStates.err, aes(x = week, y = S.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ outbreak, nrow = 1) + labs(x = '', y = 'Relative Error (S)')
p2 <- ggplot(data = oStates.err, aes(x = week, y = beta.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ outbreak, nrow = 1) + labs(x = '', y = 'Relative Error (Beta)')
p3 <- ggplot(data = oStates.err, aes(x = week, y = R0.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ outbreak, nrow = 1) + labs(x = '', y = 'Relative Error (R0)')
p4 <- ggplot(data = oStates.err, aes(x = week, y = Re.err, group = group)) + geom_abline(slope = 0, intercept = 0, lty = 2, lwd = 1.0) + geom_line(col = 'gray70', lwd = 0.2) + theme_classic() +
  facet_wrap(~ outbreak, nrow = 1) + labs(x = 'Weeks Since Outbreak Start', y = 'Relative Error (Re)')

pdf('syntheticTests/presentations/errors_S_beta_R0_Re.pdf', width = 14, height = 11)
grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()

# Clean up environment:
rm(list=ls())








