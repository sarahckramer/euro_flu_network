
### Add normally-distributed noise using old OEV form as variance
set.seed(108946052)

# Read in "realistic" synthetic runs:
load('syntheticTests/syntheticData/synth_rates_toKeep_021020.RData')
# use rates since these are what we'll input into the model anyway

# Country names:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# "Flip" data to be in correct format
for (i in 1:length(synth.outbreaks)) {
  synth.outbreaks[[i]] <- t(synth.outbreaks[[i]])
}

# Prescribe OEV parameters:
oev_base <- 1e5 # 1e4, 1e5, 1e6
oev_denom <- 10.0 # 5, 10, 20

# Calculate "old" OEV format:
source('cluster/functions/calc_obsvars.R')

obs_vars <- vector('list', length(synth.outbreaks))
for (i in 1:length(synth.outbreaks)) {
  obs_vars[[i]] <- calc_obsvars(synth.outbreaks[[i]], oev_base, oev_denom)
}

# for (j in 1:5) {
#   par(mfrow = c(4, 3), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
#   for (i in 1:12) {
#     plot(obs_vars[[j]][, i], type = 'b', pch = 20, col = 'coral', cex = 0.7, main = countries[i], xaxt = 'n', xlab = '', ylab = 'Syn+')
#   }
# }

# Check visually:
par(mfrow = c(2, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(synth.outbreaks[[1]], pch = 20, col = viridis(12), cex = 0.8, type = 'b', lty = 1)
# matplot(obs_vars[[1]], pch = 20, col = viridis(12), cex = 0.8, type = 'b', lty = 1)
synth.outbreaks.orig <- synth.outbreaks

# Add noise to "data":
for (i in 1:length(synth.outbreaks)) {
  
  for (ix in 1:ncol(synth.outbreaks[[i]])) {
    for (jx in 1:nrow(synth.outbreaks[[i]])) {
      synth.outbreaks[[i]][jx, ix] <- rnorm(1, mean = synth.outbreaks[[i]][jx, ix], sd = sqrt(obs_vars[[i]][jx, ix]))
    }
  }
  
  synth.outbreaks[[i]][synth.outbreaks[[i]] < 0] <- 0
}

# Check visually:
matplot(synth.outbreaks[[1]], pch = 20, col = viridis(12), cex = 0.8, type = 'b', lty = 1)
for (i in 2:5) {
  matplot(synth.outbreaks.orig[[i]], pch = 20, col = viridis(12), cex = 0.8, type = 'b', lty = 1)
  matplot(synth.outbreaks[[i]], pch = 20, col = viridis(12), cex = 0.8, type = 'b', lty = 1)
}

for (j in 1:5) {
  par(mfrow = c(4, 3), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  for (i in 1:12) {
    plot(synth.outbreaks[[j]][, i], type = 'b', pch = 20, col = 'coral', cex = 0.7, main = countries[i], xaxt = 'n', xlab = '', ylab = 'Syn+')
  }
}

# Save results:
# save(synth.outbreaks, file = 'syntheticTests/syntheticData/synth_rates_toKeep_021020_wError_5e4_20.RData')










