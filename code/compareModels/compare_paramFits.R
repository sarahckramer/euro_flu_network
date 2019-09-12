













# Plot inferred parameter values at each time step (network only - individual allows parameter values to differ by country):
o <- read.csv('code/gridSearch/outputs/outputOPParams_090119.csv')
o$group <- paste(o$oev_base, o$oev_denom, o$lambda, o$season, o$run, o$fc_start, sep = '_')
o$group <- factor(o$group)
o$oev_base <- factor(o$oev_base)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'L', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'D', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0max', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p4 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mn, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'R0min', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')
p5 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base), alpha = 0.2) +
  theme_classic() + labs(x = 'Week', y = 'airScale', col = 'OEV Base') + facet_wrap(~ lambda) +
  scale_color_brewer(palette = 'Set1')

if (outputPlots) {
  pdf('code/gridSearch/plots/param_ests.pdf', width = 14, height = 14)
  grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
  dev.off()
} else {
  grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
}



