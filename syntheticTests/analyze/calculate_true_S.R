
# Load TRUE observations:
load('syntheticTests/syntheticData/synth_06-26_RATES.RData')

# Reformat "data":
for (i in 1:length(synth.runs.RATES)) {
  synth.runs.RATES[[i]] <- t(synth.runs.RATES[[i]])
}

# Load S0:
load('syntheticTests/syntheticData/initStates_06-26.RData')

# Keep only S0 (I0 is only used to run model and calculate cases in first week, right?):
    # or rather, S is already S0 even when I0 are already infected - decreases from there
init.states.SEL <- init.states.SEL[1:21, ]

# Calculate S0 as rate per 100,000:
init.states.SEL <- init.states.SEL * 100000

# Subtract all prior cases from S0 for each time step:
synth.runs.S <- vector('list', length(synth.runs.RATES))
for (i in 1:length(synth.runs.RATES)) {
  obs_i <- synth.runs.RATES[[i]]
  s0_i <- init.states.SEL[, i]
  
  susc_i <- matrix(NA, nrow = dim(obs_i)[1], ncol = dim(obs_i)[2])
  
  susc_i[1, ] <- s0_i - obs_i[1, ]
  for (j in 2:(dim(obs_i)[1])) {
    susc_i[j, ] <- s0_i - (colSums(obs_i[1:j, ]))
  }
  
  synth.runs.S[[i]] <- susc_i
}

# Quick plot to check:
par(mfrow = c(2, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(synth.runs.RATES)) {
  matplot(synth.runs.RATES[[i]], type = 'b', pch = 20, lty = 1, col = viridis(21))
  matplot(synth.runs.S[[i]], type = 'b', pch = 20, lty = 1, col = viridis(21))
}

# Save:
save(synth.runs.S, file = 'syntheticTests/syntheticData/synth_06-26_S.RData')






