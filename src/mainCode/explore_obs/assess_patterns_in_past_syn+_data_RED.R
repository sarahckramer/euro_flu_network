### Assess patterns in observed data ###
library(ggplot2); library(gridExtra); library(viridis); library(reshape2)

strain <- 'A(H3)' # A(H1), A(H3), or B

### Save all plots:
pdf(paste0('results/plots/data_patterns_RED_', strain, '_070720.pdf'), width = 11, height = 10)

### Plot data for each season ###
# Read in data:
iliiso <- read.csv(paste0('data//WHO_data_', strain, '_SCALED.csv'))

# Plot data by season:
season.names <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
season.breaks <- list(79:110, 131:162, 183:214, 235:266, 287:318, 339:371, 392:423, 444:475)

par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(season.breaks)) {
  if (!all(is.na(iliiso[season.breaks[[i]], 2:13]))) {
    matplot(iliiso[season.breaks[[i]], 2:13], type = 'b', pch = 20, lty = 1, col = viridis(12),
            xlab = 'Weeks from Season Start', ylab = 'Syn+ Cases (Scaled)', main = season.names[i])
  }
}

# Calculate peak and onset timing by season/country:
source('src/mainCode/functions/Util.R')
met.df <- NULL
for (country in 2:13) {
  for (season in 1:8) {
    dat.temp <- iliiso[season.breaks[[season]], country]
    
    if (!all(is.na(dat.temp) | dat.temp == 0)) {
      pt.temp <- which(dat.temp == max(dat.temp, na.rm = TRUE))
      ot.temp <- findOnset(dat.temp, 500)$onset
      met.df <- rbind(met.df, c(names(iliiso)[country], season.names[season], pt.temp, ot.temp))
    }
    
  }
}
met.df <- as.data.frame(met.df)
names(met.df) <- c('country', 'season', 'pt', 'ot')
met.df$pt <- as.numeric(as.character(met.df$pt))
met.df$ot <- as.numeric(as.character(met.df$ot))
met.df$pt[is.na(met.df$ot)] <- NA
# met.df[met.df$pt == met.df$ot, ] # could remove these, but let's just use the peaks for these

table(met.df[is.na(met.df$ot), 'season'])
met.df <- met.df[!is.na(met.df$ot), ]
table(met.df$season)

if (strain == 'A(H1)') {
  met.df <- met.df[!(met.df$season %in% c('2011-12', '2016-17')), ]
} else if (strain == 'A(H3)') {
  met.df <- met.df[!(met.df$season %in% c('2010-11', '2015-16', '2017-18')), ]
} else if (strain == 'B') {
  met.df <- met.df[!(met.df$season %in% c('2011-12', '2013-14', '2016-17')), ]
  # also removes season w/ only 2 onsets
}

# Rename countries:
countries <- names(iliiso)[2:13]
levels(met.df$country) <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# Plot organized by median (with color by capital longitude):
library(maps)
data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(countries, 'Czech Republic')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- levels(met.df$country)
world.cities <- world.cities[, c('country.etc', 'long')]
m <- merge(met.df, world.cities, by.x = 'country', by.y = 'country.etc')
rm(world.cities)

names(m)[3:4] <- c('obs_pkwk', 'onsetObs')
m$onsetObs <- m$onsetObs + 40 - 1
m$obs_pkwk <- m$obs_pkwk + 40 - 1

ot.med <- aggregate(onsetObs ~ country, data = m, FUN = median)
ot.med <- ot.med[order(ot.med$onsetObs), ]

pt.med <- aggregate(obs_pkwk ~ country, data = m, FUN = median)
pt.med <- pt.med[order(pt.med$obs_pkwk), ]

m$country <- factor(m$country, levels = ot.med$country)
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = onsetObs, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = pt.med$country)
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = obs_pkwk, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, ncol = 1)

# Also look at ORDER of OT/PT:
m.new <- NULL
m$season <- factor(m$season)
for (ix in 1:length(levels(m$season))) {
  season <- levels(m$season)[ix]
  m.temp <- m[m$season == season, ]
  
  min.ot <- min(m.temp$onsetObs)
  min.pt <- min(m.temp$obs_pkwk)
  
  m.temp$ot_order <- m.temp$onsetObs - min.ot
  m.temp$pt_order <- m.temp$obs_pkwk - min.pt
  
  m.new <- rbind(m.new, m.temp)
}
m.new <- as.data.frame(m.new)

ot.med <- aggregate(ot_order ~ country, data = m.new, FUN = median)
ot.med <- ot.med[order(ot.med$ot_order), ]

pt.med <- aggregate(pt_order ~ country, data = m.new, FUN = median)
pt.med <- pt.med[order(pt.med$pt_order), ]

m.new$country <- factor(m.new$country, levels = ot.med$country)
p1 <- ggplot(data = m.new) + geom_boxplot(aes(x = country, y = ot_order, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset (Weeks from First Onset)', fill = 'Long.') +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m.new$country <- factor(m.new$country, levels = pt.med$country)
p2 <- ggplot(data = m.new) + geom_boxplot(aes(x = country, y = pt_order, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak (Weeks from First Peak)', fill = 'Long.') +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, ncol = 1)

# note that OT seems a bit less reliable than PT, b/c missing data can throw off the "3 consecutive weeks" metric

# Longitudinal pattern by season?:
par(mfrow = c(4, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (season in unique(m$season)) {
  print(season)
  print(cor.test(m$long[m$season == season], m$onsetObs[m$season == season], method = 'kendall'))
  print(cor.test(m$long[m$season == season], m$obs_pkwk[m$season == season], method = 'kendall'))
  plot(m$long[m$season == season], m$onsetObs[m$season == season], xlab = 'Longitude', ylab = 'Onset Timing', main = season, pch = 20)
  plot(m$long[m$season == season], m$obs_pkwk[m$season == season], xlab = 'Longitude', ylab = 'Peak Timing', main = season, pch = 20)
  print(''); print('')
}
# usually, but not always, w-e pattern stronger for OT, not PT

# Test differences statistically?
library(PMCMR); library(PMCMRplus)
kruskal.test(onsetObs ~ country, data = m)
kruskal.test(obs_pkwk ~ country, data = m)

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


if (strain == 'A(H1)') {
  cor.synch <- cor.synch[c(1, 3:6, 8)]
} else if (strain == 'A(H3)') {
  cor.synch <- cor.synch[c(2:5, 7)]
} else if (strain == 'B') {
  cor.synch <- cor.synch[c(1, 3, 5:6, 8)]
}

# Average cor.synch across all seasons:
cor.synch.AVG <- apply(simplify2array(cor.synch), 1:2, mean, na.rm = TRUE)
print(isSymmetric(cor.synch.AVG))

# Plot synchrony:
rownames(cor.synch.AVG) = colnames(cor.synch.AVG) = c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
cor.synch.AVG[upper.tri(cor.synch.AVG)] <- NA
cor.synch.plot <- melt(cor.synch.AVG)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')
cor.synch.plot <- cor.synch.plot[!is.na(cor.synch.plot$corr), ]
cor.synch.plot$corr[cor.synch.plot$corr == 1] <- NA

p3 <- ggplot(cor.synch.plot, aes(x = c1, y = c2)) + geom_tile(aes(fill = corr), colour = 'white') +
  scale_fill_gradientn(colours = cividis(100), na.value = 'gray80', limits = c(0.4, 0.95)) + theme_classic() +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Average Synchrony', x = '', y = '', fill = 'Corr.')
print(p3)

# Test average synchrony against distance using Mantel test:
library(ecodist); library(geosphere)

data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(countries, 'Czech Republic')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

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

# Is synchrony related to commuting flows?:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# load('src/formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
load('src/formatTravelData/formattedData/comm_mat_by_season_01-27.RData')
t.comm <- apply(simplify2array(comm.by.seas), 1:2, mean); rm(comm.by.seas)
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

# Is synchrony related to air travel?:
air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('src/formatTravelData/formattedData/air_', i, '_01-31.RData'))
  air.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym)
a.mean <- apply(simplify2array(air.by.month), 1:2, mean); rm(air.by.month)
a.mean <- a.mean[countries, countries]

a.mean[a.mean == 0] <- 1.0
a.mean <- 1 / a.mean # convert to distance

mantel(as.dist(synch.dist) ~ as.dist(a.mean), nperm = 10000, mrank = TRUE)

# Plot relationships between synchrony and c(distance, air, commuting):
cor.synch <- 1 - synch.dist
rownames(cor.synch) = colnames(cor.synch) = countries
cor.synch.plot <- melt(cor.synch)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')

air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('src/formatTravelData/formattedData/air_', i, '_01-31.RData'))
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

### Look at distribution of onsets/peaks by season ###
par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(onsetObs ~ season, data = m, col = 'lightblue2', xlab = 'Season', ylab = 'Onset Week')
boxplot(obs_pkwk ~ season, data = m, col = 'lightblue2', xlab = 'Season', ylab = 'Peak Week')

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
levels(m$country) <- c('Austria', 'Belgium', 'Czech Republic', 'Germany', 'Spain', 'France', 'Hungary',
                       'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia')

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

rm(list = ls())
