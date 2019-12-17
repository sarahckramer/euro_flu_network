
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

# Also plot AH by country, along with average:
ah <- ah.new; rm(ah.new)
ah <- ah[, c(1:2, 4, 6:8, 11:14, 17, 19)]
ah.mean <- rowMeans(ah); ah.mean <- as.data.frame(ah.mean); ah.mean$day <- 1:365

ah$day <- 1:365
ah <- melt(ah, id.vars = 'day')
colnames(ah) <- c('Day', 'Country', 'AH')

p1 <- ggplot(ah, aes(x = Day, y = AH, col = Country)) + geom_line(lwd = 0.8) + scale_color_brewer(palette = 'Paired')
p2 <- ggplot() + geom_line(data = ah, aes(x = Day, y = AH, col = Country), lwd = 0.8) + scale_color_brewer(palette = 'Paired') +
  geom_line(data = ah.mean, aes(x = day, y = ah.mean), lwd = 1.5)

pdf('code/checks/ah_by_country_w_mean.pdf', width = 15, height = 12)
grid.arrange(p1, p2, ncol = 1)
dev.off()
