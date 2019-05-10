
### Assess patterns in observed data

### Save all plots:
pdf('code/checks/analyzeDataRetro/outputs/data_patterns.pdf', width = 11, height = 10)

### Plot data for each season ###
# Read in data:
iliiso <- read.csv('data/WHO_data_05-09-19.csv')

# Scale data:
scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
for (i in 2:22) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:283, i] <- iliiso[1:283, i] * 1.3
    iliiso[284:495, i] <- iliiso[284:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  } else {
    iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}

# Plot data by season:
season.names <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
season.breaks <- list(79:110, 131:162, 183:214, 235:266, 287:318, 339:371, 392:423, 444:475)

par(mfrow = c(4, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(season.breaks)) {
  matplot(iliiso[season.breaks[[i]], 2:22], type = 'b', pch = 20, lty = 1, col = viridis(21),
          xlab = 'Weeks from Season Start', ylab = 'Syn+ Cases (Scaled)', main = season.names[i])
}
# already looks like these are later and maybe even more synchronous that in the simulated data

### Look at range of AR, PT, OT by country ###
# Read in and format metrics files:
m <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_new_FIN.csv')
m.1718 <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/outputMetrics_RT1718_onset.csv')

m <- unique(m[, c('country', 'season', 'scaling', 'obs_pkwk', 'obs_peak_int', 'onsetObs5', 'totAttackObs')])
m$obs_peak_int <- m$obs_peak_int * m$scaling; m$totAttackObs <- m$totAttackObs * m$scaling
m <- m[, -3]

m.1718 <- unique(m.1718[, c('country', 'scaling', 'obs_pkwk', 'obs_peak_int', 'onsetObs')])
m.1718$obs_peak_int <- m.1718$obs_peak_int * m.1718$scaling; m.1718$season <- '2017-18'
m.1718 <- m.1718[, c(1, 6, 3:5)]

# Reduce all to countries being used in network model:
countries <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
               'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
               'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
               'United Kingdom')
m <- m[m$country %in% countries, ]; m$country <- factor(m$country)
m.1718 <- m.1718[m.1718$country %in% countries, ]; m.1718$country <- factor(m.1718$country)

# Remove if PT = OT:
m <- m[!(m$onsetObs5 == m$obs_pkwk) | is.na(m$onsetObs5), ]
m.1718 <- m.1718[!(m.1718$onsetObs == m.1718$obs_pkwk) | is.na(m.1718$onsetObs), ]

# Calculate AR for 17-18 season:
m.1718$totAttackObs <- NA
for (country in levels(m.1718$country)) {
  m.1718$totAttackObs[m.1718$country == country] <- sum(iliiso[season.breaks[[8]], names(iliiso) == country], na.rm = TRUE)
}

# Combine the two data sets:
m$season <- as.character(m$season)
names(m)[5] <- 'onsetObs'
m <- rbind(m, m.1718)
m$season <- factor(m$season)
rm(m.1718)

# Remove where no onset:
m <- m[!is.na(m$onsetObs), ]

# Rename countries:
levels(m$country) <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT', 'LU',
                       'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

# Plot range of AR, PT, OT by country:
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = totAttackObs), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)')
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = onsetObs), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week') + scale_y_continuous(breaks = seq(46, 68, by = 2))
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = obs_pkwk), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week') + scale_y_continuous(breaks = seq(46, 68, by = 2))
p4 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = obs_peak_int), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Intensity (per 100,000)')
grid.arrange(p1, p2, p3, ncol = 1)
# grid.arrange(p1, p2, p3, p4, ncol = 1)

# Larger: DE, IS, AT, BE, HR, CZ; smaller: DK, FR, IE, PT, RO, SI, ES
# Later onsets: RO, SK, also HR and IS (?)
# Earlier: IE, IT, FR(?); later: DE, HU, IS, RO

# Plot organized by median (with color by capital longitude):
library(maps)
data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(countries, 'Czech Republic', 'UK')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- levels(m$country)
world.cities <- world.cities[, c('country.etc', 'long')]
m <- merge(m, world.cities, by.x = 'country', by.y = 'country.etc')
rm(world.cities)

ar.med <- aggregate(totAttackObs ~ country, data = m, FUN = median)
ar.med <- ar.med[order(ar.med$totAttackObs, decreasing = TRUE), ]

ot.med <- aggregate(onsetObs ~ country, data = m, FUN = median)
ot.med <- ot.med[order(ot.med$onsetObs), ]

pt.med <- aggregate(obs_pkwk ~ country, data = m, FUN = median)
pt.med <- pt.med[order(pt.med$obs_pkwk), ]

m$country <- factor(m$country, levels = ar.med$country)
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = totAttackObs, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)', fill = 'Long.') +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = ot.med$country)
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = onsetObs, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = pt.med$country)
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = obs_pkwk, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, p3, ncol = 1)
# might be a bit of a tendency for OT/PT to happen sooner in west than east, but not super strong

# Test differences statistically?
library(PMCMR); library(PMCMRplus)
kruskal.test(totAttackObs ~ country, data = m)
kruskal.test(onsetObs ~ country, data = m)
kruskal.test(obs_pkwk ~ country, data = m)
kruskal.test(obs_peak_int ~ country, data = m)
# only PI is significant
posthoc.kruskal.nemenyi.test(m$obs_peak_int, m$country, 'Tukey') # none sig when broken down to pairwise

### Assess synchrony ###
# For each season, what is the range of peak timings:
for (season in levels(m$season)) {
  m.temp <- m[m$season == season, ]
  # print(quantile(m.temp$obs_pkwk, probs = c(0, 0.1, 0.9, 1.0))) # don't seem highly "concentrated"
  hist(m.temp$obs_pkwk, breaks = 21)
  print(max(m.temp$obs_pkwk) - min(m.temp$obs_pkwk))
  print(as.vector(quantile(m.temp$obs_pkwk, prob = 0.975) - quantile(m.temp$obs_pkwk, prob = 0.025)))
  # print(quantile(m.temp$obs_pkwk, prob = 0.95) - quantile(m.temp$obs_pkwk, prob = 0.05))
  print('')
}
# c(7, 10, 10, 8, 7, 7, 9, 10) # difference between max and min peak weeks

# Calculate correlation coefficients between all pairs of countries, for each season:
    # Then average the values for all seasons and plot on a grid
cor.synch <- vector('list', length(season.breaks))
for (season in 1:length(cor.synch)) {
  cor.mat <- matrix(NA, nrow = length(countries), ncol = length(countries))
  diag(cor.mat) <- 1.0
  
  for (i in 1:(length(countries) - 1)) {
    for (j in (i + 1):length(countries)) {
      # plot(iliiso[season.breaks[[season]], i + 1])
      # points(iliiso[season.breaks[[season]], j + 1], col = 'blue')
      cor.mat[i, j] <- cor(iliiso[season.breaks[[season]], i + 1],
                           iliiso[season.breaks[[season]], j + 1],
                           use = 'pairwise.complete.obs', method = 'spearman')
      cor.mat[j, i] <- cor(iliiso[season.breaks[[season]], i + 1],
                           iliiso[season.breaks[[season]], j + 1],
                           use = 'pairwise.complete.obs', method = 'spearman')
      # matrices should be symmetric
    }
  }
  
  print(isSymmetric(cor.mat))
  cor.synch[[season]] <- cor.mat
}

# Average cor.synch across all seasons:
cor.synch.AVG <- apply(simplify2array(cor.synch), 1:2, mean, na.rm = TRUE)
print(isSymmetric(cor.synch.AVG))

# Test average synchrony against distance using Mantel test:
library(ecodist); library(geosphere)

data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(countries, 'Czech Republic', 'UK')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
                                      'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

dist.mat <- matrix(0, nrow = length(countries), ncol = length(countries))
for (i in 1:(length(countries) - 1)) {
  for (j in (i + 1):length(countries)) {
    long1 <- world.cities$long[world.cities$country.etc == levels(world.cities$country.etc)[i]]
    long2 <- world.cities$long[world.cities$country.etc == levels(world.cities$country.etc)[j]]
    lat1 <- world.cities$lat[world.cities$country.etc == levels(world.cities$country.etc)[i]]
    lat2 <- world.cities$lat[world.cities$country.etc == levels(world.cities$country.etc)[j]]
    
    dist.mat[i, j] = dist.mat[j, i] = distGeo(c(long1, long2), c(lat1, lat2)) / 1000 / 100 # in 100s of km
  }
}
rownames(dist.mat) = colnames(dist.mat) = levels(world.cities$country.etc)
print(isSymmetric(dist.mat))

synch.dist <- 1 - cor.synch.AVG # make into distance matrix
mantel(as.dist(synch.dist) ~ as.dist(dist.mat), nperm = 10000, mrank = TRUE)
# synchrony and distance between capitals are not significantly associated

# Is synchrony related to commuting flows?:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

t.comm.sym <- t.comm # have to be symmetric for Mantel test
t.comm.sym.lower <- which(lower.tri(t.comm.sym), arr.ind = TRUE)
avg.vals <- c()
for (i in 1:dim(t.comm.sym.lower)[1]) {
  index <- t.comm.sym.lower[i, ]
  avg.vals <- c(avg.vals, (t.comm.sym[index[1], index[2]] + t.comm.sym[index[2], index[1]]) / 2)
}
t.comm.sym[t.comm.sym.lower] <- avg.vals
t.comm.sym.upper <- cbind(t.comm.sym.lower[, 2], t.comm.sym.lower[, 1])
t.comm.sym[t.comm.sym.upper] <- avg.vals
print(isSymmetric(t.comm.sym))

t.comm.sym[t.comm.sym == 0] <- 1.0
t.comm.sym <- 1 / t.comm.sym # convert to distance

mantel(as.dist(synch.dist) ~ as.dist(t.comm.sym), nperm = 10000, mrank = TRUE)
# not sig

# Is synchrony related to air travel?:
air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  air.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym)
a.mean <- apply(simplify2array(air.by.month), 1:2, mean); rm(air.by.month)
a.mean <- a.mean[countries, countries]

a.mean[a.mean == 0] <- 1.0
a.mean <- 1 / a.mean # convert to distance

mantel(as.dist(synch.dist) ~ as.dist(a.mean), nperm = 10000, mrank = TRUE)
# not sig

# Plot mean correlations as matrix:
rownames(cor.synch.AVG) = colnames(cor.synch.AVG) = c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
                                                      'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
cor.synch.AVG[upper.tri(cor.synch.AVG)] <- NA
cor.synch.plot <- melt(cor.synch.AVG)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')
cor.synch.plot <- cor.synch.plot[!is.na(cor.synch.plot$corr), ]
cor.synch.plot$corr[cor.synch.plot$corr == 1] <- NA

# ranges from 0.4216 to 0.9487
p1 <- ggplot(cor.synch.plot, aes(x = c1, y = c2)) + geom_tile(aes(fill = corr), colour = 'white') +
  scale_fill_gradientn(colours = cividis(100), na.value = 'gray80') + theme_classic() +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Average Synchrony', x = '', y = '', fill = 'Corr.')
p1

# Plot relationships between synchrony and c(distance, air, commuting):
cor.synch <- 1 - synch.dist
rownames(cor.synch) = colnames(cor.synch) = countries
cor.synch.plot <- melt(cor.synch)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')

air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  air.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym)
a.mean <- apply(simplify2array(air.by.month), 1:2, mean); rm(air.by.month)
a.mean <- a.mean[countries, countries]
a.mean <- melt(a.mean)
names(a.mean) <- c('c1', 'c2', 'air')

t.comm <- melt(t.comm)
names(t.comm) <- c('c1', 'c2', 'comm')

dist.mat <- melt(dist.mat)
names(dist.mat) <- c('c1', 'c2', 'dist')

cor.synch.plot <- merge(cor.synch.plot, a.mean, by = c('c1', 'c2'))
cor.synch.plot <- merge(cor.synch.plot, t.comm, by = c('c1', 'c2'))
cor.synch.plot <- merge(cor.synch.plot, dist.mat, by = c('c1', 'c2'))

cor.synch.plot <- cor.synch.plot[cor.synch.plot$c1 != cor.synch.plot$c2, ]

cor.synch.plot$air <- log(cor.synch.plot$air)
cor.synch.plot$comm <- log(cor.synch.plot$comm)

par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(cor.synch.plot$air, cor.synch.plot$corr, pch = 20, xlab = 'log(Air Passengers)', ylab = 'Correlation')
plot(cor.synch.plot$comm, cor.synch.plot$corr, pch = 20, xlab = 'log(Commuters)', ylab = 'Correlation')
plot(cor.synch.plot$dist, cor.synch.plot$corr, pch = 20, xlab = 'Distance (100 km)', ylab = 'Correlation')

plot(cor.synch.plot$air, cor.synch.plot$comm, pch = 20, xlab = 'log(Air Passengers)', ylab = 'log(Commuters)')
plot(cor.synch.plot$dist, cor.synch.plot$air, pch = 20, xlab = 'Distance (100 km)', ylab = 'log(Air Passengers)')
plot(cor.synch.plot$dist, cor.synch.plot$comm, pch = 20, xlab = 'Distance (100 km)', ylab = 'log(Commuters)')

### Map out spatial patterns ###
# List of countries:
countries.europe <- c('Austria', 'Belarus', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark',
                      'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland',
                      'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Norway', 'Poland',
                      'Portugal', 'Moldova', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                      'Switzerland', 'Ukraine', 'UK')

# Create map:
world <- map_data('world')
eur <- world[world$region %in% countries.europe,]
rm(world)

# Code to remove axes:
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Change country levels in m:
m$country <- factor(as.character(m$country))
levels(m$country) <- c('Austria', 'Belgium', 'Czech Republic', 'Germany', 'Denmark', 'Spain',
                       'France', 'Croatia', 'Hungary', 'Ireland', 'Iceland', 'Italy',
                       'Luxembourg', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Sweden',
                       'Slovenia', 'Slovakia', 'UK')

# Store onset week for each season:
library(dplyr)
for (season in levels(m$season)) {
  m.temp <- m[m$season == season, c('country', 'onsetObs')]
  names(m.temp)[1] <- 'region'
  m.temp$region <- as.character(m.temp$region)
  eur <- full_join(eur, m.temp, by = 'region')
  names(eur)[length(names(eur))] <- season
}

# Loop through weeks and plot when country has onset:
for (season in levels(m$season)) {
  on.min <- min(eur[, season], na.rm = TRUE)
  on.max <- max(eur[, season], na.rm = TRUE)
  
  p = vector('list', length(on.min:on.max))
  for (wk in on.min:on.max) {
    eur.curr <- eur[eur[, season] == wk & !is.na(eur[, season]), ]
    eur.past <- eur[eur[, season] < wk & !is.na(eur[, season]), ]
    
    p[[wk - on.min + 1]] <- ggplot() + geom_polygon(data = eur, aes(x = long, y = lat, group = group),
                                            fill = 'gray95', colour = 'black', size = 1.0) +
      geom_polygon(data = eur.past, aes(x = long, y = lat, group = group),
                   fill = '#ffeda0', colour = 'black', size = 1.0) +
      geom_polygon(data = eur.curr, aes(x = long, y = lat, group = group),
                   fill = '#f03b20', colour = 'black', size = 1.0) +
      labs(title = paste0('Week ', wk)) + theme_classic() + ditch_the_axes
  }
  do.call('grid.arrange', c(p, nrow = 3))
}

dev.off()




