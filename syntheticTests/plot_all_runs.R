
# Plot out ALL generated outbreaks from model inputs (rate):
load('syntheticTests/syntheticData/allRunsRATES_1000_0714.RData')
synth.run.RATES <- vector('list', length = dim(newI.c[[1]])[1])

for (i in 1:length(synth.run.RATES)) {
  synth.run.RATES[[i]] <- matrix(NA, nrow = dim(newI.c[[1]])[2], ncol = length(newI.c))
  
  for (j in 1:ncol(synth.run.RATES[[i]])) {
    synth.run.RATES[[i]][, j] <- newI.c[[j]][i, ]
  }
}

pdf('syntheticTests/syntheticData/07-14/allRuns_byOutbreak.pdf', width = 15, height = 15)
par(mfrow = c(10, 10), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(synth.run.RATES)) {
  matplot(synth.run.RATES[[i]], pch = 20, type = 'b', lty = 1, col = viridis(20), cex = 0.25)
}
dev.off()

# # Plot variation in outbreak pattern by country (counts):
# countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
#                'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
# for (i in 1:length(newI.c)) {
#   matplot(t(newI.c[[i]]), type = 'l', lty = 1, col = viridis(1000), main = countries[i])
# }


