### Calculate errors in state/parameter fits for synthetic "observations" ###

library(reshape2)

# Read in results:
o <- read.csv('src/syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('src/syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv')

# Get countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

### Calculate parameter error ###

# Get TRUE parameter values:
load('src/syntheticTests/syntheticData/for_synthetic_testing/parms_toKeep_070220.RData')
select.parms <- as.data.frame(t(parms.outbreaks[25:29, ]))
names(select.parms) <- c('L', 'D', 'R0max', 'R0diff', 'airScale')
select.parms <- as.data.frame(cbind(rep(1:5, 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

# Read in the TRUE values of beta, R0, Re at each point:
load('src/syntheticTests/syntheticData/for_synthetic_testing/true_betaR0Re_070220.RData')
true.betas <- true.list[[1]]
true.R0 <- true.list[[2]]
true.Re <- true.list[[3]]
rm(true.list)

# Calculate PARAMETER error at three time points:
o.err <- o[o$week %in% c(49, 54, 59), c(1:7, 13:14)] # time points 10, 15, 20
o.err$L.err = o.err$D.err = o.err$R0mx.err = o.err$R0diff.err = o.err$aS.err = NA

# First, subtract actual - fit, and divide by actual:
for (outbreak in 1:5) {
  o.err$L.err[o.err$season == outbreak] <- (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'L'] - o.err$L[o.err$season == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'L']
  o.err$D.err[o.err$season == outbreak] <- (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'D'] - o.err$D[o.err$season == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'D']
  o.err$R0mx.err[o.err$season == outbreak] <- (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0max'] - o.err$R0mx[o.err$season == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0max']
  o.err$R0diff.err[o.err$season == outbreak] <- (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0diff'] - o.err$R0diff[o.err$season == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0diff']
  o.err$aS.err[o.err$season == outbreak] <- (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'airScale'] - o.err$airScale[o.err$season == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'airScale']
}
# positive values indicate that fit value is HIGHER than truth

# Calculate relative error for S, beta, R0, Re for each country at t = 10, 15, 20:
load('src/syntheticTests/syntheticData/for_synthetic_testing/synth_S_toKeep_070220.RData')
for (i in 1:5) {
  synth.s[[i]] <- t(synth.s[[i]])
}

oStates$run <- factor(oStates$run)
oStates.err <- oStates[oStates$week %in% c(49, 54, 59), c(1:3, 5:8, 12:13, 17:19)]
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
        (-1 * (susc_i[c(10, 15, 20), which(countries == country)] - oStates.err.temp$S)) / susc_i[c(10, 15, 20), which(countries == country)]
      oStates.err$beta.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (beta.temp[c(10, 15, 20), which(countries == country)] - oStates.err.temp$beta)) / beta.temp[c(10, 15, 20), which(countries == country)]
      oStates.err$R0.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (R0.temp[c(10, 15, 20), which(countries == country)] - oStates.err.temp$R0)) / R0.temp[c(10, 15, 20), which(countries == country)]
      oStates.err$Re.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (Re.temp[c(10, 15, 20), which(countries == country)] - oStates.err.temp$Re)) / Re.temp[c(10, 15, 20), which(countries == country)]
    }
  }
  
}

# Write results to file:
write.csv(o.err, file = 'src/syntheticTests/outputOPParams_SYNTH_errors_070220.csv', row.names = FALSE)
write.csv(oStates.err, file = 'src/syntheticTests/outputOP_SYNTH_errors_070220.csv', row.names = FALSE)

rm(list=ls())

