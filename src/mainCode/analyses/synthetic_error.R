### Plot results of synthetic testing ###

library(ggplot2); library(gridExtra); library(viridis)

### Parameter error:

# Read in results:
o <- read.csv('syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv')

# Plot observed data vs. fit obs:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

# Plot parameter fit over time vs. true params:
load('syntheticTests/syntheticData/for_synthetic_testing/parms_toKeep_070220.RData')
select.parms <- as.data.frame(t(parms.outbreaks[25:29, ]))
names(select.parms) <- c('L', 'D', 'R0max', 'R0diff', 'airScale')
select.parms <- as.data.frame(cbind(rep(1:5, 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

names(o)[13] <- 'outbreak'
o.plot <- o[o$week <= 69, ]
o.plot <- o.plot[, c(2, 4:6, 13:14)]

p1 <- ggplot(data = o.plot, aes(x = week, y = D, group = run)) + geom_line(col = 'gray70') + geom_point(size = 0.75, col = 'gray70') +
  geom_hline(data = select.parms[select.parms$parameter == 'D', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + theme(aspect.ratio = 1,
                          axis.text = element_text(size = 11),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 13),
                          strip.background = element_blank()) +
  labs(x = '', y = 'D (Days)') + facet_wrap(~ outbreak, nrow = 1) + scale_y_continuous(breaks = 2:7) +
  geom_text(data = data.frame(label = 'A', outbreak = 1, run = 0), aes(label = label, x = 42, y = 6.9), size = 8)
p2 <- ggplot(data = o.plot, aes(x = week, y = R0mx, group = run)) + geom_line(col = 'gray70') + geom_point(size = 0.75, col = 'gray70') +
  geom_hline(data = select.parms[select.parms$parameter == 'R0max', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + theme(aspect.ratio = 1,
                          axis.text = element_text(size = 11),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 13),
                          strip.background = element_blank()) +
  labs(x = '', y = 'R0max') + facet_wrap(~ outbreak, nrow = 1) + #scale_y_continuous(breaks = 2:7) +
  geom_text(data = data.frame(label = 'B', outbreak = 1, run = 0), aes(label = label, x = 42, y = 2.75), size = 8)
p3 <- ggplot(data = o.plot, aes(x = week, y = R0diff, group = run)) + geom_line(col = 'gray70') + geom_point(size = 0.75, col = 'gray70') +
  geom_hline(data = select.parms[select.parms$parameter == 'R0diff', ], aes(yintercept = value), lwd = 1.0) +
  theme_classic() + theme(aspect.ratio = 1,
                          axis.text = element_text(size = 11),
                          strip.text = element_blank(),
                          axis.title = element_text(size = 13),
                          strip.background = element_blank()) +
  labs(x = 'Week', y = 'R0diff') + facet_wrap(~ outbreak, nrow = 1) + #scale_y_continuous(breaks = 2:7) +
  geom_text(data = data.frame(label = 'C', outbreak = 1, run = 0), aes(label = label, x = 42, y = 1.429), size = 8)

# grid.arrange(p1, p2, p3, ncol = 1)

g1 <- arrangeGrob(p1, p2, p3, ncol = 1)
ggsave(filename = 'results/plots/FigS11.svg', g1, width = 10, height = 7)

rm(list = ls())

### Hists of beta/R0/Re error:

# Read in results:
o <- read.csv('syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv')

# Plot observed data vs. fit obs:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/syntheticData/for_synthetic_testing/true_betaR0Re_070220.RData')
true.betas <- true.list[[1]]
true.R0 <- true.list[[2]]
true.Re <- true.list[[3]]
rm(true.list)

# Read in true S:
load('syntheticTests/syntheticData/for_synthetic_testing/synth_S_toKeep_070220.RData')
for (i in 1:5) {
  synth.s[[i]] <- t(synth.s[[i]])
}

# Calculate error at ALL time points:
oStates$run <- factor(oStates$run)
oStates.err <- oStates[, c(1:3, 5:8, 12:13, 17:19)]
oStates.err$Re.err = oStates.err$R0.err = oStates.err$beta.err = oStates.err$S.err = NA

for (outbreak in 1:5) {
  print(outbreak)
  susc_i <- synth.s[[outbreak]]
  beta.temp <- true.betas[[outbreak]]
  R0.temp <- true.R0[[outbreak]]
  Re.temp <- true.Re[[outbreak]]
  
  for (run in levels(oStates.err$run)) {
    for (country in countries) {
      oStates.err.temp <- oStates.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country, ]
      
      oStates.err$S.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (susc_i[, which(countries == country)] - oStates.err.temp$S)) / susc_i[, which(countries == country)]
      oStates.err$beta.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (beta.temp[, which(countries == country)] - oStates.err.temp$beta)) / beta.temp[, which(countries == country)]
      oStates.err$R0.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (R0.temp[, which(countries == country)] - oStates.err.temp$R0)) / R0.temp[, which(countries == country)]
      oStates.err$Re.err[oStates.err$season == outbreak & oStates.err$run == run & oStates.err$country == country] <-
        (-1 * (Re.temp[, which(countries == country)] - oStates.err.temp$Re)) / Re.temp[, which(countries == country)]
    }
  }
  
}
rm(oStates.err.temp, beta.temp, R0.temp, Re.temp, susc_i, synth.s, true.betas, true.R0, true.Re, country, outbreak, run, i)

# Plot distribution of relative error for S, beta, R0, Re for each country at t = 5, 10, 15, 20:
oStates.err <- oStates.err[, c(3:4, 8:9, 14:16)]
oStates.err <- oStates.err[oStates.err$week %in% c(44, 49, 54, 59), ]
oStates.err$week <- factor(oStates.err$week)
levels(oStates.err$week) <- c('5', '10', '15', '20')

oStates.err <- melt(oStates.err, id.vars = c('week', 'country', 'season', 'run'))
names(oStates.err)[5:6] <- c('parameter', 'error')

levels(oStates.err$week) <- c('t = 5 Weeks', 't = 10 Weeks', 't = 15 Weeks', 't = 20 Weeks')
levels(oStates.err$parameter) <- c('', ' ', '  ')

p1 <- ggplot(data = oStates.err) + geom_histogram(aes(x = error, y = 0.05 * ..density..), binwidth = 0.05, fill = 'gray60') +
  geom_vline(xintercept = 0, lty = 2) +
  theme_classic() + theme(aspect.ratio = 0.9,
                          axis.text = element_text(size = 11),
                          strip.text = element_text(size = 14),
                          axis.title = element_text(size = 14),
                          strip.background = element_blank(),
                          axis.text.x = element_text(angle = 35, vjust = 0.75)) +#,
  # axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  facet_grid(parameter ~ week) + labs(x = 'Relative Error', y = 'Proportion of Fits') +
  geom_text(data = data.frame(label = c('A', 'B', 'C'), week = rep('t = 5 Weeks', 3), parameter = c('', ' ', '  ')),
            aes(label = label, x = -0.42, y = 0.4), size = 8) +
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.7))
# print(p1)

ggsave('results/plots/FigS12.svg', plot = p1, width = 10, height = 7)

rm(list = ls())








