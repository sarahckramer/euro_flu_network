
library(ggplot2); library(gridExtra); library(viridis)

# Read in results:
o <- read.csv('syntheticTests/outputOP_SYNTH_beta-R0-Re_ISOLATED.csv')

# Plot observed data vs. fit obs:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

load('syntheticTests/syntheticData/synth_rates_ISOL.RData')
newI <- isol.keep[[1]]
s.true <- isol.keep[[2]]

# pdf('syntheticTests/outputs/synthFit_021720_ISOLATED.pdf', width = 16, height = 12)
################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot fit of S over time, vs. true S (and S0):
load('syntheticTests/syntheticData/params_ISOL.RData')
init.S <- params.keep[, 1] * 100000

for (i in 1:3) {
  newI_i <- newI[, i]
  susc_i <- s.true[, i]
  o.temp <- o[o$season == i, ]
  
  p1 <- ggplot() + geom_line(data = o.temp, aes(x = week, y = Est, group = run), lwd = 0.5, col = 'steelblue2') +
    geom_point(data = o.temp, aes(x = week, y = Est, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(aes(x = 40:91, y = newI_i), lwd = 0.6) + geom_point(aes(x = 40:91, y = newI_i), pch = 4, cex = 2) +
    theme_classic() + labs(x = 'Week', y = 'Incidence')
  p2 <- ggplot() + geom_line(data = o.temp, aes(x = week, y = S, group = run), lwd = 0.5, col = 'steelblue2') +
    geom_point(data = o.temp, aes(x = week, y = S, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(aes(x = 40:91, y = susc_i), lwd = 0.6) + geom_point(aes(x = 40:91, y = susc_i), pch = 4, cex = 2) +
    geom_hline(yintercept = init.S[i], lwd = 1.0, lty = 2) +
    theme_classic() + labs(x = 'Week', y = '# Susceptible')
  grid.arrange(p1, p2)
}

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot parameter fit over time vs. true params:
select.parms <- as.data.frame(params.keep); rm(params.keep)
names(select.parms) <- c('S0', 'I0', 'L', 'D', 'R0max', 'R0diff')
select.parms <- as.data.frame(cbind(rep(1:3, 6), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

names(o)[20] <- 'outbreak'
o.plot <- o[o$week <= 69, ]

p1 <- ggplot(data = o.plot, aes(x = week, y = L, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  geom_hline(data = select.parms[select.parms$parameter == 'L', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'L (days)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_y_continuous(limits = c(365, 3650))
p2 <- ggplot(data = o.plot, aes(x = week, y = D, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  geom_hline(data = select.parms[select.parms$parameter == 'D', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'D (days)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_y_continuous(limits = c(2, 7))
p3 <- ggplot(data = o.plot, aes(x = week, y = R0mx, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  geom_hline(data = select.parms[select.parms$parameter == 'R0max', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'R0max') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_y_continuous(limits = c(1.9, 3.0))
p4 <- ggplot(data = o.plot, aes(x = week, y = R0diff, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  geom_hline(data = select.parms[select.parms$parameter == 'R0diff', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'R0diff') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_y_continuous(limits = c(0.4, 1.6))
grid.arrange(p1, p2, p3, p4, ncol = 1)

# Plot parameter sd over time:
p1 <- ggplot(data = o.plot, aes(x = week, y = L_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'L (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p2 <- ggplot(data = o.plot, aes(x = week, y = D_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'D (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p3 <- ggplot(data = o.plot, aes(x = week, y = R0mx_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'R0max (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p4 <- ggplot(data = o.plot, aes(x = week, y = R0diff_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'R0diff (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
# grid.arrange(p1, p2, p3, p4, ncol = 1)

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/syntheticData/true_betaR0Re_ISOLATED.RData')
true.betas <- true.list[[1]]
true.R0 <- true.list[[2]]
true.Re <- true.list[[3]]
rm(true.list)

# Plot fit accuracy for beta, R0, Re:
for (i in 1:3) {
  beta.temp <- true.betas[, i]; R0.temp <- true.R0[, i]; Re.temp <- true.Re[, i]
  o.temp <- o[o$outbreak == i, ]
  
  p1 <- ggplot() +
    geom_line(data = o.temp, aes(x = week, y = beta, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = o.temp, aes(x = week, y = beta, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(aes(x = 40:91, y = beta.temp), lwd = 1.0) + theme_classic() +
    labs(x = 'Week', y = 'Beta', title = paste0('Outbreak ', i)) +
    scale_y_continuous(limits = c(0.1, 0.6))
  # print(p1)
  
  p2 <- ggplot() +
    geom_line(data = o.temp, aes(x = week, y = R0, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = o.temp, aes(x = week, y = R0, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(aes(x = 40:91, y = R0.temp), lwd = 1.0) + theme_classic() +
    labs(x = 'Week', y = 'R0', title = paste0('Outbreak ', i)) +
    scale_y_continuous(limits = c(1.4, 2.3))
  # print(p2)
  
  p3 <- ggplot() +
    geom_line(data = o.temp, aes(x = week, y = Re, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = o.temp, aes(x = week, y = Re, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(aes(x = 40:91, y = Re.temp), lwd = 1.0) + theme_classic() +
    labs(x = 'Week', y = 'Re', title = paste0('Outbreak ', i)) +
    scale_y_continuous(limits = c(0.5, 1.6))
  # print(p3)
  
  grid.arrange(p1, p2, p3)
}

# dev.off()
# rm(list=ls())




