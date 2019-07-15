
### Add normally-distributed noise using old OEV form as variance
set.seed(108946052)

# Read in "realistic" synthetic runs:
load('syntheticTests/syntheticData/synth_07-14_RATES.RData')
# use rates since these are what we'll input into the model anyway

# "Flip" data to be in correct format
for (i in 1:length(synth.runs.RATES)) {
  synth.runs.RATES[[i]] <- t(synth.runs.RATES[[i]])
}

# Prescribe OEV parameters:
oev_base <- 1e6 # 1e4, 1e5, 1e6
oev_denom <- 5.0 # 5, 10, 20

# Calculate "old" OEV format:
source('code/functions/calc_obsvars.R')

obs_vars <- vector('list', length(synth.runs.RATES))
for (i in 1:length(synth.runs.RATES)) {
  obs_vars[[i]] <- calc_obsvars(synth.runs.RATES[[i]], oev_base, oev_denom)
}

# for (j in 1:27) {
#   par(mfrow = c(5, 5), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
#   for (i in 1:n) {
#     plot(obs_vars[[j]][, i], type = 'b', pch = 20, col = 'coral', cex = 0.7, main = countries[i], xaxt = 'n', xlab = '', ylab = 'Syn+')
#   }
# }

# Check visually:
par(mfrow = c(2, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(synth.runs.RATES[[1]], pch = 20, col = viridis(21), cex = 0.8, type = 'b', lty = 1)
# matplot(obs_vars[[1]], pch = 20, col = viridis(21), cex = 0.8, type = 'b', lty = 1)

# Add noise to "data":
for (i in 1:length(synth.runs.RATES)) {
  
  for (ix in 1:ncol(synth.runs.RATES[[i]])) {
    for (jx in 1:nrow(synth.runs.RATES[[i]])) {
      synth.runs.RATES[[i]][jx, ix] <- 
        rnorm(1, mean = synth.runs.RATES[[i]][jx, ix], sd = sqrt(obs_vars[[i]][jx, ix]))
    }
  }
  
  synth.runs.RATES[[i]][synth.runs.RATES[[i]] < 0] <- 0
}

# Check visually:
matplot(synth.runs.RATES[[1]], pch = 20, col = viridis(21), cex = 0.8, type = 'b', lty = 1)

# Save results:
save(synth.runs.RATES, file = 'syntheticTests/syntheticData/synth_07-14_RATES_wError_1e6_5.RData')










