### Plot several assessments of synthetic fit ###

library(ggplot2); library(gridExtra); library(viridis)

# Read in results:
o <- read.csv('syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv')

# Plot observed data vs. fit obs:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

load('syntheticTests/syntheticData/for_synthetic_testing/synth_rates_toKeep_070220.RData')
for (i in 1:length(synth.outbreaks)) {
  synth.outbreaks[[i]] <- t(synth.outbreaks[[i]])
}

# pdf('syntheticTests/outputs/synthFit_incidence_070220.pdf', width = 16, height = 12)
for (outbreak in 1:5) {
  obs_i <- synth.outbreaks[[outbreak]]
  obs_i <- melt(obs_i)
  names(obs_i) <- c('week', 'country', 'newI')
  obs_i$country <- countries[obs_i$country]
  obs_i$week <- obs_i$week + 40 - 1
  
  oStates.temp <- oStates[oStates$season == outbreak, ]
  
  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = Est, group = run), lwd = 0.5, col = 'steelblue2') +
    geom_point(data = oStates.temp, aes(x = week, y = Est, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(data = obs_i, aes(x = week, y = newI), lwd = 0.6) +
    geom_point(data = obs_i, aes(x = week, y = newI), pch = 4, cex = 2) +
    facet_wrap(~ country, scales = 'free_y') +
    theme_classic() + labs(x = 'Week', y = 'New Cases')
  print(p1)
}
# dev.off()

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot fit of S over time, vs. true S (and S0):
load('syntheticTests/syntheticData/for_synthetic_testing/synth_S_toKeep_070220.RData')
load('syntheticTests/syntheticData/for_synthetic_testing/parms_toKeep_070220.RData')

init.S <- parms.outbreaks[1:12, ] * 100000
rownames(init.S) <- countries
init.S <- melt(init.S)
names(init.S) <- c('country', 'outbreak', 'S0')

for (i in 1:5) {
  synth.s[[i]] <- t(synth.s[[i]])
}

# pdf('syntheticTests/outputs/synthFit_S_070220.pdf', width = 16, height = 12)
for (outbreak in 1:5) {
  susc_i <- synth.s[[outbreak]]
  susc_i <- melt(susc_i)
  names(susc_i) <- c('week', 'country', 'S')
  susc_i$country <- countries[susc_i$country]
  susc_i$week <- susc_i$week + 40 - 1
  
  oStates.temp <- oStates[oStates$season == outbreak, ]
  
  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = S, group = run), lwd = 0.5, col = 'steelblue2') +
    geom_point(data = oStates.temp, aes(x = week, y = S, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(data = susc_i, aes(x = week, y = S), lwd = 0.6) +
    geom_point(data = susc_i, aes(x = week, y = S), pch = 4, cex = 2) +
    geom_hline(data = init.S[init.S$outbreak == outbreak, ], aes(yintercept = S0), lwd = 1.0, lty = 2) +
    facet_wrap(~ country, scales = 'free_y') +
    theme_classic() + labs(x = 'Week', y = '# Susceptible')
  print(p1)
}
# dev.off()

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot parameter fit over time vs. true params:
select.parms <- as.data.frame(t(parms.outbreaks[25:29, ]))
names(select.parms) <- c('L', 'D', 'R0max', 'R0diff', 'airScale')
select.parms <- as.data.frame(cbind(rep(1:5, 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

names(o)[13] <- 'outbreak'
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
p5 <- ggplot(data = o.plot, aes(x = week, y = airScale, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  geom_hline(data = select.parms[select.parms$parameter == 'airScale', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'airScale') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_y_continuous(limits = c(0.75, 1.25))

# pdf('syntheticTests/outputs/synthFit_params_070220.pdf', width = 16, height = 12)
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# dev.off()

# Plot parameter sd over time:
p1 <- ggplot(data = o.plot, aes(x = week, y = L_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'L (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p2 <- ggplot(data = o.plot, aes(x = week, y = D_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'D (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p3 <- ggplot(data = o.plot, aes(x = week, y = R0mx_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'R0max (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p4 <- ggplot(data = o.plot, aes(x = week, y = R0diff_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'R0diff (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
p5 <- ggplot(data = o.plot, aes(x = week, y = airScale_sd, group = run)) + geom_point(size = 0.9, col = 'steelblue2') + geom_line(lwd = 0.5, col = 'steelblue2') +
  theme_classic() + labs(x = 'Week', y = 'airScale (st. dev.)') + facet_wrap(~ outbreak, ncol = 5, scales = 'free_y')
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# for all but D, just seem to increase over time - is lambda too high? Might just be another indicator that parameters other than D aren't fit strongly

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/syntheticData/for_synthetic_testing/true_betaR0Re_070220.RData')
true.betas <- true.list[[1]]
true.R0 <- true.list[[2]]
true.Re <- true.list[[3]]
rm(true.list)

# Plot fit accuracy for beta, R0, Re:
# pdf('syntheticTests/outputs/synthFit_beta-R0-Re_070220.pdf', width = 16, height = 12)
for (outbreak in 1:5) {
  beta.temp <- true.betas[[outbreak]]; R0.temp <- true.R0[[outbreak]]; Re.temp <- true.Re[[outbreak]]
  rownames(beta.temp) = rownames(R0.temp) = rownames(Re.temp) = 1:(dim(beta.temp)[1])
  colnames(beta.temp) = colnames(R0.temp) = colnames(Re.temp) = 1:(dim(beta.temp)[2])
  beta.temp <- melt(beta.temp); R0.temp <- melt(R0.temp); Re.temp <- melt(Re.temp)
  names(beta.temp) = names(R0.temp) = names(Re.temp) = c('week', 'country', 'value')
  beta.temp$country <- countries[beta.temp$country]
  R0.temp$country <- countries[R0.temp$country]
  Re.temp$country <- countries[Re.temp$country]
  beta.temp$week <- beta.temp$week + 40 - 1
  R0.temp$week <- R0.temp$week + 40 - 1
  Re.temp$week <- Re.temp$week + 40 - 1
  
  oStates.temp <- oStates[oStates$season == outbreak, ]
  
  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = beta, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = oStates.temp, aes(x = week, y = beta, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(data = beta.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) + theme_classic() +
    labs(x = 'Week', y = 'Beta', title = paste0('Outbreak ', outbreak)) +
    scale_y_continuous(limits = c(0.2, 0.7))
  print(p1)
  
  p2 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = R0, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = oStates.temp, aes(x = week, y = R0, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(data = R0.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) + theme_classic() +
    labs(x = 'Week', y = 'R0', title = paste0('Outbreak ', outbreak)) +
    scale_y_continuous(limits = c(1.2, 2.2))
  print(p2)
  
  p3 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = Re, group = run), lwd = 0.5, alpha = 0.5, col = 'steelblue2') +
    geom_point(data = oStates.temp, aes(x = week, y = Re, group = run), alpha = 0.5, col = 'steelblue2') +
    geom_line(data = Re.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) + theme_classic() +
    labs(x = 'Week', y = 'Re', title = paste0('Outbreak ', outbreak)) +
    scale_y_continuous(limits = c(0.5, 1.7))
  print(p3)
}
# dev.off()

# Plot distribution of relative param error at t=15 and t=20:
# pdf('syntheticTests/outputs/synthFit_errorHist_070220.pdf', width = 16, height = 12)

o.err <- read.csv('syntheticTests/outputOPParams_SYNTH_errors_070220.csv')
oStates.err <- read.csv('syntheticTests/outputOP_SYNTH_errors_070220.csv')

o.err15 <- o.err[o.err$week == 54, ]
o.err20 <- o.err[o.err$week == 59, ]

p1 <- ggplot(data = o.err15) + geom_histogram(aes(x = L.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (L)', y = '', title = 'Error at t=15')
p2 <- ggplot(data = o.err15) + geom_histogram(aes(x = D.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (D)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = o.err15) + geom_histogram(aes(x = R0mx.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0max)', y = '', title = 'Error at t=15')
p4 <- ggplot(data = o.err15) + geom_histogram(aes(x = R0diff.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0diff)', y = '', title = 'Error at t=15')
p5 <- ggplot(data = o.err15) + geom_histogram(aes(x = aS.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (airScale)', y = '', title = 'Error at t=15')
grid.arrange(p1, p2, p3, p4, p5)

p1 <- ggplot(data = o.err20) + geom_histogram(aes(x = L.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (L)', y = '', title = 'Error at t=20')
p2 <- ggplot(data = o.err20) + geom_histogram(aes(x = D.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (D)', y = '', title = 'Error at t=20')
p3 <- ggplot(data = o.err20) + geom_histogram(aes(x = R0mx.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0max)', y = '', title = 'Error at t=20')
p4 <- ggplot(data = o.err20) + geom_histogram(aes(x = R0diff.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0diff)', y = '', title = 'Error at t=20')
p5 <- ggplot(data = o.err20) + geom_histogram(aes(x = aS.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (airScale)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, p4, p5)

# Plot distribution of relative error for S, beta, R0, Re for each country at t = 10, 15, 20:
oStates.err.10 <- oStates.err[oStates.err$week == 49, ]
oStates.err.15 <- oStates.err[oStates.err$week == 54, ]
oStates.err.20 <- oStates.err[oStates.err$week == 59, ]

p1 <- ggplot(data = oStates.err.10) + geom_histogram(aes(x = S.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.10) + geom_histogram(aes(x = beta.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
p3 <- ggplot(data = oStates.err.10) + geom_histogram(aes(x = R0.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
p4 <- ggplot(data = oStates.err.10) + geom_histogram(aes(x = Re.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
grid.arrange(p1, p2, p3, p4)

p1 <- ggplot(data = oStates.err.15) + geom_histogram(aes(x = S.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')# + facet_wrap(~ season)
p2 <- ggplot(data = oStates.err.15) + geom_histogram(aes(x = beta.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15')# + facet_wrap(~ season)
p3 <- ggplot(data = oStates.err.15) + geom_histogram(aes(x = R0.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')# + facet_wrap(~ season)
p4 <- ggplot(data = oStates.err.15) + geom_histogram(aes(x = Re.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')# + facet_wrap(~ season)
grid.arrange(p1, p2, p3, p4)

p1 <- ggplot(data = oStates.err.20) + geom_histogram(aes(x = S.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')
p2 <- ggplot(data = oStates.err.20) + geom_histogram(aes(x = beta.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')
p3 <- ggplot(data = oStates.err.20) + geom_histogram(aes(x = R0.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')
p4 <- ggplot(data = oStates.err.20) + geom_histogram(aes(x = Re.err), binwidth = 0.05, col = 'white', fill = 'steelblue2') + geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')
grid.arrange(p1, p2, p3, p4)

# dev.off()

rm(list=ls())





