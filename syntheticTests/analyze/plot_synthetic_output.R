
library(ggplot2); library(gridExtra)

pdf('syntheticTests/outputs/cluster/model_fit_072219_loop_S0I0_Truth.pdf',
    width = 16, height = 12)

# Read in results:
load('syntheticTests/outputs/cluster/071519/res_loop_S0I0range.RData')

m <- res[[1]]
o <- res[[2]]
o.err <- res[[3]]
oStates <- res[[4]]
oStates.err <- res[[5]]
rm(res)

# Plot observed data vs. fit obs:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

load('syntheticTests/syntheticData/synth_07-14_RATES.RData')
to.keep <- c(1, 6, 9, 13)
synth.runs.RATES <- synth.runs.RATES[to.keep]

for (i in 1:length(synth.runs.RATES)) {
  synth.runs.RATES[[i]] <- t(synth.runs.RATES[[i]])
  # matplot(synth.runs.RATES[[i]], pch = 20, type = 'b', lty = 1, col = viridis(n), cex = 0.5)
}

for (outbreak in 1:length(to.keep)) {
  obs_i <- synth.runs.RATES[[outbreak]]
  obs_i <- melt(obs_i)
  names(obs_i) <- c('week', 'country', 'newI')
  obs_i$country <- countries[obs_i$country]
  
  oStates.temp <- oStates[oStates$outbreak == to.keep[outbreak], ]
  
  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = group), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = newI, group = group.plot, col = group, pch = lambda), alpha = 0.5) +
    geom_line(data = obs_i, aes(x = week, y = newI), lwd = 0.6) +
    geom_point(data = obs_i, aes(x = week, y = newI), pch = 4, cex = 2) +
    facet_wrap(~ country, scales = 'free_y') +
    theme_classic() + labs(x = 'Week', y = 'New Cases') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p1)
  
}

# Outbreak 1: Generally looks the best of all of them; some collapse w/ 1e4/10; undershoots IE, PL, SI, SK; see CZ - tries to increase, pulled back down, trouble increasing later
    # most of these (except SI) are fairly small and late (also has issues with very small late peaks, but I figure this might be inevitable?); SI early but very high
# Outbreak 6: Undershooting peaks in AT, BE, ES, NL, PL; 1e5/20 gets closest to hitting them, but still misses; seeing some collapse w/ 1e4/10
    # BE very late and low - one of (the?) last to peak; same with NL; both ES and PL are fairly late and small; AT is mid-way and medium-sized, so not sure what's happening here - SK peaks same time but a little lower?
# Outbreak 9: Trouble w/ UK, FR, PL; seems like there's too much error in the obs. early on, even with higher lambda
    # all 3 relatively late, and PL is still pretty high; FR is smallest of the 3 (and gets overshot); UK and FR actually pretty close, though; overshoots HU: also very late and small
# Outbreak 13: Difficulty w/ FR, BE, DE, IE (difficulty reaching peak intensity); seems to do better with countries w/ larger outbreaks; consistently overshoots IT and undershoots many others; no obvious best combo
    # interesting that it overshoot IT - this is the second, later very large peak (DK similar time but slightly smaller); other troublesome ones are late and are lower in peak intensity

# Here I took out some of the combos that lead to collapse beforehand, but it might be worth exploring 1e4/20 and 1e4/higher lambdas?
# Restructure OEV form so that lower relative error on lower obs. (so, something greater than squaring?)

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot fit S over time, vs. initial S:
load('syntheticTests/syntheticData/initStates_07-14.RData')
init.states.SEL <- init.states.SEL[1:20, to.keep]

rownames(init.states.SEL) <- countries
init.states.SEL <- melt(init.states.SEL)
names(init.states.SEL) <- c('country', 'outbreak', 'S0')

load('syntheticTests/syntheticData/synth_07-14_S.RData')
synth.runs.S <- synth.S.RATES[to.keep]

for (outbreak in 1:length(to.keep)) {
  susc_i <- synth.runs.S[[outbreak]]
  susc_i <- melt(susc_i)
  names(susc_i) <- c('week', 'country', 'S')
  susc_i$country <- countries[susc_i$country]
  
  oStates.temp <- oStates[oStates$outbreak == to.keep[outbreak], ]
  oStates.temp$S <- oStates.temp$S * 100000

  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = group), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = S, group = group.plot, col = group, pch = lambda), alpha = 0.5) +
    geom_line(data = susc_i, aes(x = week, y = S), lwd = 1.0) +
    geom_hline(data = init.states.SEL[init.states.SEL$outbreak == outbreak, ], aes(yintercept = S0 * 100000), lwd = 1.0, lty = 2) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = '% Susceptible') +
    scale_color_viridis(discrete = T, option = 'D')
  print(p1)
  
}

# 1e4 clearly best for outbreaks 1/13, but more even for outbreak 6 and 1e5 better for outbreak 9

################################################################################################################################################################################################
################################################################################################################################################################################################

# Plot parameter fit over time vs. true params:
load('syntheticTests/syntheticData/params_07-14.RData')
select.parms <- select.parms[to.keep, ]
select.parms$L <- select.parms$L * 365
select.parms <- as.data.frame(cbind(rep(c(1, 6, 9 , 13), 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

o.plot <- o[o$week <= 30, ]

p1 <- ggplot(data = o.plot) +
  geom_point(aes(x = week, y = L, col = group, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = L, col = group, group = group.plot), lwd = 0.2) +
  geom_hline(data = select.parms[select.parms$parameter == 'L', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'L (days)') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(365, 3650))
# print(p1)
p2 <- ggplot(data = o.plot) +
  geom_point(aes(x = week, y = D, col = group, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = D, col = group, group = group.plot), lwd = 0.2) +
  geom_hline(data = select.parms[select.parms$parameter == 'D', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'D (days)') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1') +
  scale_shape_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(2, 7))
p3 <- ggplot(data = o.plot) +
  geom_point(aes(x = week, y = R0max, col = group, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = R0max, col = group, group = group.plot), lwd = 0.2) +
  geom_hline(data = select.parms[select.parms$parameter == 'R0mx', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'R0max') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(2.0, 4.0))
p4 <- ggplot(data = o.plot) +
  geom_point(aes(x = week, y = R0min, col = group, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = R0min, col = group, group = group.plot), lwd = 0.2) +
  geom_hline(data = select.parms[select.parms$parameter == 'R0mn', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'R0min') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_y_continuous(limits = c(0.8, 1.2))
p5 <- ggplot(data = o.plot) +
  geom_point(aes(x = week, y = airScale, col = group, shape = lambda, group = group.plot), size = 0.9) +
  geom_line(aes(x = week, y = airScale, col = group, group = group.plot), lwd = 0.2) +
  geom_hline(data = select.parms[select.parms$parameter == 'airScale', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + labs(x = 'Week', y = 'airScale') +
  facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  scale_shape_discrete(guide = FALSE) +
  scale_y_continuous(limits = c(0.75, 1.25))
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

# With loop, earlys params don't seem to have as much trouble anymore

# # Plot parameter sd over time:
# p1 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = L_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = L_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'L (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# # print(p1)
# p2 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = D_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = D_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'D (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p3 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = R0max_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = R0max_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0max (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p4 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = R0min_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = R0min_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'R0min (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# p5 <- ggplot(data = o.plot) +
#   geom_point(aes(x = week, y = airScale_sd, col = group, shape = lambda, group = group.plot), size = 0.9) +
#   geom_line(aes(x = week, y = airScale_sd, col = group, group = group.plot), lwd = 0.2) +
#   theme_classic() + labs(x = 'Week', y = 'airScale (st. dev.)') +
#   facet_wrap(~ outbreak, ncol = 5, scales = 'free_y') +
#   scale_color_brewer(palette = 'Set1')
# grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
# SD starts increasing near end for higher lambdas, where ens. var. is being inflated, even
# Most look pretty settled by t = 10-20

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/outputs/cluster/071519/true_betaR0Re.RData')
true.betas <- true.epi.params[[1]]
true.R0 <- true.epi.params[[2]]
true.Re <- true.epi.params[[3]]
rm(true.epi.params)

# Plot fit accuracy for beta, R0, Re:
for (outbreak in 1:length(to.keep)) {
  beta.temp <- true.betas[[outbreak]]; R0.temp <- true.R0[[outbreak]]; Re.temp <- true.Re[[outbreak]]
  rownames(beta.temp) = rownames(R0.temp) = rownames(Re.temp) = 1:(dim(beta.temp)[1])
  colnames(beta.temp) = colnames(R0.temp) = colnames(Re.temp) = 1:(dim(beta.temp)[2])
  beta.temp <- melt(beta.temp); R0.temp <- melt(R0.temp); Re.temp <- melt(Re.temp)
  names(beta.temp) = names(R0.temp) = names(Re.temp) = c('week', 'country', 'value')
  beta.temp$country <- countries[beta.temp$country]
  R0.temp$country <- countries[R0.temp$country]
  Re.temp$country <- countries[Re.temp$country]
  
  oStates.temp <- oStates[oStates$outbreak == to.keep[outbreak], ]
  
  p1 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = beta, group = group.plot, col = group), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = beta, group = group.plot, col = group, pch = lambda), alpha = 0.5) +
    geom_line(data = beta.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) + #scale_y_continuous(limits = c(0.2, 0.8)) +
    theme_classic() + labs(x = 'Week', y = 'Beta', title = paste0('Outbreak ', to.keep[outbreak])) +
    scale_color_viridis(discrete = T, option = 'D') + scale_y_continuous(limits = c(0, 0.7))
  print(p1)
  
  p2 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = R0, group = group.plot, col = group), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = R0, group = group.plot, col = group, pch = lambda), alpha = 0.5) +
    geom_line(data = R0.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = 'R0') +
    scale_color_viridis(discrete = T, option = 'D') + scale_y_continuous(limits = c(1.0, 3.0))
  print(p2)
  
  p3 <- ggplot() +
    geom_line(data = oStates.temp, aes(x = week, y = Re, group = group.plot, col = group), lwd = 0.5, alpha = 0.5) +
    geom_point(data = oStates.temp, aes(x = week, y = Re, group = group.plot, col = group, pch = lambda), alpha = 0.5) +
    geom_line(data = Re.temp, aes(x = week, y = value), lwd = 1.0) +
    facet_wrap(~ country) +
    theme_classic() + labs(x = 'Week', y = 'Re') +
    scale_color_viridis(discrete = T, option = 'D') + scale_y_continuous(limits = c(0, 2.0))
  print(p3)
  
}

# Outbreak 1: similar for beta, but takes 1e4 a while to get there; 1e4 clearly better for R0; 1e4 seems better for Re
# Outbreak 6: similar for beta, but takes 1e4 a while to get there; similar, but 1e5 better for R0; 1e4 seems a bit better for Re, though
# Outbreak 9: beta does alright, but takes 1e4 a while to get there; 1e5 obviously better for R0 (b/c 1e4 has trouble in the beginning?); 1e4 best for Re
# Outbreak 13: beta tends to be overestimated; 1e4 clearly better for R0; 1e4 a little better I think

# Plot distribution of relative param error at t=15 and t=20:
o.err <- o.err[o.err$week == 20, ]

# o.err$L.err[o.err$L.err > 2.0] <- NA
# o.err$D.err[o.err$D.err > 2.0] <- NA
# o.err$R0mx.err[o.err$R0mx.err > 2.0] <- NA
# o.err$R0mn.err[o.err$R0mn.err > 2.0] <- NA
# o.err$aS.err[o.err$aS.err > 2.0] <- NA

p1 <- ggplot(data = o.err) +
  geom_histogram(aes(x = L.err), binwidth = 0.2, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (L)', y = '', main = 'Error at t=20') +
  scale_fill_brewer(palette = 'Set1')
p2 <- ggplot(data = o.err) +
  geom_histogram(aes(x = D.err), binwidth = 0.02, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (D)', y = '') +
  scale_fill_brewer(palette = 'Set1')
p3 <- ggplot(data = o.err) +
  geom_histogram(aes(x = R0mx.err), binwidth = 0.015, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0max)', y = '') +
  scale_fill_brewer(palette = 'Set1')# + facet_grid(outbreak ~ oev_denom)
p4 <- ggplot(data = o.err) +
  geom_histogram(aes(x = R0mn.err), binwidth = 0.015, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0min)', y = '') +
  scale_fill_brewer(palette = 'Set1')
p5 <- ggplot(data = o.err) +
  geom_histogram(aes(x = aS.err), binwidth = 0.01, col = 'white', fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (airScale)', y = '') +
  scale_fill_brewer(palette = 'Set1')
grid.arrange(p1, p2, p3, p4, p5)

# Okay, but we need to check this by oev_base/oev_denom:
for (o1 in levels(o.err$oev_base)) {
  for (o2 in levels(o.err$oev_denom)) {
    o.err.temp <- o.err[o.err$oev_base == o1 & o.err$oev_denom == o2, ]
    o.err.temp$outbreak <- factor(o.err.temp$outbreak)
    
    p1 <- ggplot(data = o.err.temp) +
      geom_histogram(aes(x = L.err, fill = lambda, col = outbreak), binwidth = 0.2, col = 'white') +#, fill = 'steelblue') +
      geom_vline(xintercept = 0, lty = 2) +
      theme_classic() + labs(x = 'Relative Error (L)', y = '', title = paste(o1, o2, sep = '_')) +
      scale_fill_brewer(palette = 'Set1')
    p2 <- ggplot(data = o.err.temp) +
      geom_histogram(aes(x = D.err, fill = lambda), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
      geom_vline(xintercept = 0, lty = 2) +
      theme_classic() + labs(x = 'Relative Error (D)', y = '') +
      scale_fill_brewer(palette = 'Set1')
    p3 <- ggplot(data = o.err.temp) +
      geom_histogram(aes(x = R0mx.err, fill = lambda), binwidth = 0.015, col = 'white') +#, fill = 'steelblue') +
      geom_vline(xintercept = 0, lty = 2) +
      theme_classic() + labs(x = 'Relative Error (R0max)', y = '') +
      scale_fill_brewer(palette = 'Set1')# + facet_grid(outbreak ~ oev_denom)
    p4 <- ggplot(data = o.err.temp) +
      geom_histogram(aes(x = R0mn.err, fill = lambda), binwidth = 0.015, col = 'white') +#, fill = 'steelblue') +
      geom_vline(xintercept = 0, lty = 2) +
      theme_classic() + labs(x = 'Relative Error (R0min)', y = '') +
      scale_fill_brewer(palette = 'Set1')
    p5 <- ggplot(data = o.err.temp) +
      geom_histogram(aes(x = aS.err, fill = lambda), binwidth = 0.01, col = 'white') +#, fill = 'steelblue') +
      geom_vline(xintercept = 0, lty = 2) +
      theme_classic() + labs(x = 'Relative Error (airScale)', y = '') +
      scale_fill_brewer(palette = 'Set1')
    grid.arrange(p1, p2, p3, p4, p5)
    
  }
}
# Good: 1e5/10 (but tends to underestimate D more than over), 1e5/20 (same w/ D); 1e4/5 and 1e4/10 aren't bad, but don't look quite as good?

# Plot distribution of relative error for S, beta, R0, Re for each country at t = 10, 15, 20:
oStates.err.10 <- oStates.err[oStates.err$week == 10, ]
oStates.err.15 <- oStates.err[oStates.err$week == 15, ]
oStates.err.20 <- oStates.err[oStates.err$week == 20, ]

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')# +
  # scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = S.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')# +
# scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

# p1 <- ggplot(data = oStates.err.10) +
#   geom_histogram(aes(x = S.err, fill = outbreak), binwidth = 0.02, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=10')
# p2 <- ggplot(data = oStates.err.15) +
#   geom_histogram(aes(x = S.err, fill = outbreak), binwidth = 0.03, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=15')
# p3 <- ggplot(data = oStates.err.20) +
#   geom_histogram(aes(x = S.err, fill = outbreak), binwidth = 0.05, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (S)', y = '', title = 'Error at t=20')# +
#   # scale_x_continuous(limits = c(-1, 1))
# grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')# +
  # scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = beta.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')# +
# scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

# p1 <- ggplot(data = oStates.err.10) +
#   geom_histogram(aes(x = beta.err, fill = outbreak), binwidth = 0.02, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=10')
# p2 <- ggplot(data = oStates.err.15) +
#   geom_histogram(aes(x = beta.err, fill = outbreak), binwidth = 0.03, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=15') +
#   scale_x_continuous(limits = c(-1, 1))
# p3 <- ggplot(data = oStates.err.20) +
#   geom_histogram(aes(x = beta.err, fill = outbreak), binwidth = 0.05, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (Beta)', y = '', title = 'Error at t=20')# +
#   # scale_x_continuous(limits = c(-1, 1))
# grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.01, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')# +
  # scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.01, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = R0.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')# +
# scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

# p1 <- ggplot(data = oStates.err.10) +
#   geom_histogram(aes(x = R0.err, fill = outbreak), binwidth = 0.02, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=10')
# p2 <- ggplot(data = oStates.err.15) +
#   geom_histogram(aes(x = R0.err, fill = outbreak), binwidth = 0.02, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=15')
# p3 <- ggplot(data = oStates.err.20) +
#   geom_histogram(aes(x = R0.err, fill = outbreak), binwidth = 0.02, col = 'white') +
#   geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
#   theme_classic() + labs(x = 'Relative Error (R0)', y = '', title = 'Error at t=20')# +
#   # scale_x_continuous(limits = c(-1, 1))
# grid.arrange(p1, p2, p3, ncol = 3)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.01, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')# +
  # scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.01, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = Re.err, fill = group), binwidth = 0.02, col = 'white') +#, fill = 'steelblue') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ outbreak) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')# +
# scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(data = oStates.err.10) +
  geom_histogram(aes(x = Re.err, fill = outbreak), binwidth = 0.02, col = 'white') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=10')
p2 <- ggplot(data = oStates.err.15) +
  geom_histogram(aes(x = Re.err, fill = outbreak), binwidth = 0.02, col = 'white') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=15')
p3 <- ggplot(data = oStates.err.20) +
  geom_histogram(aes(x = Re.err, fill = outbreak), binwidth = 0.02, col = 'white') +
  geom_vline(xintercept = 0, lty = 2) + facet_wrap(~ country, ncol = 1) +
  theme_classic() + labs(x = 'Relative Error (Re)', y = '', title = 'Error at t=20')# +
  # scale_x_continuous(limits = c(-1, 1))
grid.arrange(p1, p2, p3, ncol = 3)
# for things like R0, oev_base 1e4 seems best

dev.off()
rm(list=ls())








