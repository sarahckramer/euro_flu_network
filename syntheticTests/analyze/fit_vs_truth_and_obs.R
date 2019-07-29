
# Read in results:
load('syntheticTests/outputs/cluster/072319/res_loop_S0range_reprobe95.RData')
oStates <- res[[4]]
rm(res)

# Get countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

# Read in true "observations":
load('syntheticTests/syntheticData/synth_07-14_RATES.RData')
to.keep <- c(1, 6, 9, 13)
synth.runs.TRUE <- synth.runs.RATES[to.keep]

for (i in 1:length(synth.runs.TRUE)) {
  synth.runs.TRUE[[i]] <- t(synth.runs.TRUE[[i]])
  # matplot(synth.runs.TRUE[[i]], pch = 20, type = 'b', lty = 1, col = viridis(n), cex = 0.5)
}

# Plot "observed" data vs. fit obs:
oStates$oev_base <- factor(oStates$oev_base); oStates$oev_denom <- factor(oStates$oev_denom)
for (o1 in levels(oStates$oev_base)) {
  for (o2 in levels(oStates$oev_denom)) {
    o1.form <- formatC(o1, format = 'e', digits = 0)
    o1.form <- paste0(substr(o1.form, 1, 2), substr(o1.form, 5, 5))
    
    load(paste0('syntheticTests/syntheticData/synth_07-14_RATES_wError_1e5_', o2, '.RData'))
    synth.runs.RATES <- synth.runs.RATES[to.keep]
    
    for (outbreak in 1:length(to.keep)) {
      # matplot(synth.runs.RATES[[outbreak]], pch = 20, type = 'b', lty = 1, col = viridis(n), cex = 0.5)
      
      true_i <- synth.runs.TRUE[[outbreak]]
      true_i <- melt(true_i)
      names(true_i) <- c('week', 'country', 'newI')
      true_i$country <- countries[true_i$country]
      
      obs_i <- synth.runs.RATES[[outbreak]]
      obs_i <- melt(obs_i)
      names(obs_i) <- c('week', 'country', 'newI')
      obs_i$country <- countries[obs_i$country]
      
      oStates.temp <- oStates[oStates$outbreak == to.keep[outbreak] &
                                oStates$oev_base == o1 &
                                oStates$oev_denom == o2, ]
      
      p1 <- ggplot() +
        geom_line(data = oStates.temp, aes(x = week, y = newI, group = lambda, col = lambda), lwd = 0.5, alpha = 0.5) +
        geom_point(data = oStates.temp, aes(x = week, y = newI, group = lambda, col = lambda), alpha = 0.5) +
        geom_line(data = obs_i, aes(x = week, y = newI), lwd = 0.8) +
        geom_point(data = obs_i, aes(x = week, y = newI), pch = 4, cex = 2.5) +
        geom_line(data = true_i, aes(x = week, y = newI), lwd = 0.8, col = 'steelblue2') +
        geom_point(data = true_i, aes(x = week, y = newI), pch = 4, cex = 2.5, col = 'blue2') +
        facet_wrap(~ country, scales = 'free_y') +
        theme_classic() + labs(x = 'Week', y = 'New Cases', title = paste(outbreak, o1.form, o2, sep = ' ')) +
        scale_color_viridis(discrete = T, option = 'D')
      print(p1)
      
    }
    
    
  }
}

# Yeah, sometimes runs miss the truth but match the observations closely (although sometimes they both miss - PL in outbreak 4, LU has some issues)
# So some of the missing peaks are well explained by the error-laden observations


