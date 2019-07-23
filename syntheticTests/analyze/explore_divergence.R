
# Read in newI by country and week:
oStates <- read.csv('syntheticTests/outputs/cluster/071519/outputOPStates_loop_reduceS0I0.csv')

# List countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

# Read in and format "observations":
load('syntheticTests/syntheticData/synth_07-14_RATES.RData')
to.keep <- c(1, 6, 9, 13)
synth.runs.RATES <- synth.runs.RATES[to.keep]

for (i in 1:length(synth.runs.RATES)) {
  synth.runs.RATES[[i]] <- t(synth.runs.RATES[[i]])
  # matplot(synth.runs.RATES[[i]], pch = 20, type = 'b', lty = 1, col = viridis(n), cex = 0.5)
}

# Plot observed data vs. fit obs:
oStates$group <- paste(oStates$oev_base, oStates$oev_denom, sep = '_'); oStates$group <- factor(oStates$group)
oStates$group <- factor(oStates$group, levels = levels(oStates$group)[c(3, 1:2, 6, 4:5)])
oStates$group.plot <- paste(oStates$run, oStates$oev_base, oStates$oev_denom, oStates$lambda, sep = '_'); oStates$group.plot <- factor(oStates$group.plot)
oStates$lambda <- factor(oStates$lambda)

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

# So let's stick with outbreaks 1 and 13 for now, and look at countries where divergence seems clear (1: BE, HR, IE, PL, SK; 13: BE, DE, FR, IE, IT, SI):
load('syntheticTests/syntheticData/synth_07-14_RATES_wError_1e4_20.RData')
obs_i <- synth.runs.RATES[[13]]
obs_i <- melt(obs_i)
names(obs_i) <- c('week', 'country', 'newI')
obs_i$country <- countries[obs_i$country]

oStates.temp <- oStates[oStates$outbreak == 13, ]

par(mfrow = c(5, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (country in 1:n) {
  plot(obs_i$newI[obs_i$country == countries[country]], pch = 4, main = countries[country], ylim = c(0, 1.25 * max(obs_i$newI[obs_i$country == countries[country]])))
  polygon(x = c(1:43, 43:1), y = c(0.5 * obs_i$newI[obs_i$country == countries[country]], rev(1.5 * obs_i$newI[obs_i$country == countries[country]])), col = 'gray95')
  points(obs_i$newI[obs_i$country == countries[country]], pch = 4)
  # lines(0.8 * obs_i$newI[obs_i$country == countries[country]], col = 'gray80')
  # lines(1.2 * obs_i$newI[obs_i$country == countries[country]], col = 'gray80')
  obs.synth <- oStates.temp$newI[oStates.temp$country == countries[country] & oStates.temp$run == 1 & oStates.temp$oev_base == 1e4 & oStates.temp$oev_denom == 20 & oStates.temp$lambda == 1.03]
  lines(obs.synth, type = 'b', pch = 20, col = 'coral')
  # lines(oStates.temp$newI[oStates.temp$country == countries[country] & oStates.temp$run == 2 & oStates.temp$oev_base == 1e4 & oStates.temp$oev_denom == 20 & oStates.temp$lambda == 1.03],
  #       type = 'b', pch = 20, col = 'coral')
  # lines(oStates.temp$newI[oStates.temp$country == countries[country] & oStates.temp$run == 3 & oStates.temp$oev_base == 1e4 & oStates.temp$oev_denom == 20 & oStates.temp$lambda == 1.03],
  #       type = 'b', pch = 20, col = 'coral')
  
  print(countries[country])
  print(which((obs.synth > 1.5 * obs_i$newI[obs_i$country == countries[country]] | obs.synth < 0.5 * obs_i$newI[obs_i$country == countries[country]]) &
                obs_i$newI[obs_i$country == countries[country]] > 1000))
  
}
# At 20%, most have divergence at several time points, regardless of whether they fit well or not - this really doesn't seem like substantial divergence, but making range
# larger is likely going to leave out even those that perform poorly
# The countries that tend to perform most poorly are also the ones that have sharp spikes down and back up at peak
# And I'd be doing it at pretty much every time point with a 20% rule; same for 25%; for 50%: times 16, 20-22, 25; if also >500 instead of 1000: time 16-17, 20-22, 24-25
# Any way to do for just one country? I guess not
# What about with outbreak 13?: times 18-23, 26, 30, 32-33; if 1000: 18-22
# Often they diverge from truth, but not from error-laden observations


























