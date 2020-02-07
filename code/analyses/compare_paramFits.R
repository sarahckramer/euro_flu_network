
### Plot inferred parameter values at each time step (network only - individual allows parameter values to differ by country) ###

library(ggplot2); library(gridExtra)

o1 <- read.csv('results/original/outputOPParams_090119.csv')
o2 <- read.csv('results/newScalings/outputOPParams_090919.csv')
o3 <- read.csv('results/propRandTravel/outputOPParams_090919.csv')

o1$nu = o1$nu_sd = NA
o2$nu = o2$nu_sd = NA

o1$model <- 'Original'; o2$model <- 'New Scale'; o3$model <- 'Random Travel'

o <- rbind(o1, o2, o3)
o$group <- paste(o$oev_base, o$season, o$run, o$fc_start, o$model, sep = '_')
o$group <- factor(o$group)
o$oev_base <- factor(o$oev_base)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'L', col = 'OEV Base') + facet_wrap(~ model, ncol = 3) +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'D', col = 'OEV Base') + facet_wrap(~ model) +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'R0max', col = 'OEV Base') + facet_wrap(~ model) +
  scale_color_brewer(palette = 'Set1')
p4 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mn, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'R0min', col = 'OEV Base') + facet_wrap(~ model) +
  scale_color_brewer(palette = 'Set1')
p5 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'airScale', col = 'OEV Base') + facet_wrap(~ model) +
  scale_color_brewer(palette = 'Set1')
p6 <- ggplot(data = o) + geom_line(aes(x = week, y = nu, group = group, col = oev_base), alpha = 0.3) +
  theme_classic() + labs(x = 'Week', y = 'nu', col = 'OEV Base') + facet_wrap(~ model) +
  scale_color_brewer(palette = 'Set1')

pdf('results/plots/param_ests.pdf', width = 15, height = 16)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 1)
dev.off()

# Fits are very similar between models; OEV base 1e5 is much more stable and gets more realistic values for airScale and nu

