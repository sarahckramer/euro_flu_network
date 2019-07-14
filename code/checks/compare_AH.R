
# Old AH data (by grid size):
ah <- read.csv('data/ah_05-07_formatted.csv')

# New AH data (by pop size):
ah.new <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')

# Plot:
pdf('code/checks/old_v_new_AH.pdf', width = 15, height = 13)
par(mfrow = c(7, 3), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:21) {
  plot(ah[, i], main = names(ah)[i], pch = 20, cex = 0.2)
  lines(ah.new[, i], col = 'blue')
}
dev.off()
