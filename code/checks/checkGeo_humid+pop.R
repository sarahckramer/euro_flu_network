
# Look at whether there are longitudinal patterns in humidity and population size that
# could be driving spatial patterns in synthetic runs
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- 1:21

# Read in population sizes:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in humidity data:
ah <- read.csv('data/ah_05-07_formatted.csv')

# First, a few countries w/ outlying AH patterns:
# matplot(ah, pch = 20, col = viridis(21), cex = 0.5)
matplot(ah, pch = 20, col = 'gray', cex = 0.5)
lines(ah$Portugal, col = 'coral', lwd = 1.5)
lines(ah$Sweden, col = 'green4', lwd = 1.5)
lines(ah$Iceland, col = 'steelblue2', lwd = 1.5)

# Get latitude and longitude of country centroids (since humidity for whole country):
l <- read.csv('data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% c(countries, 'GB'), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)

# Set time variable for AH and melt:
ah$day <- 1:365
ah <- melt(ah, id.vars = 'day')
names(ah) <- c('day', 'country', 'sh')

# Merge all data frames:
m <- merge(pop.size, l, by.x = 'country', by.y = 'iso_a2')
names(m)[3:4] <- c('long', 'lat')

# # Plot lat/long vs. popsize:
# plot(m$lat, log(m$pop), pch = 20, xlab = 'Latitude', ylab = 'log(Population Size)')
# plot(m$long, log(m$pop), pch = 20, xlab = 'Longitude', ylab = 'log(Population Size)')
# plot(m$long, m$lat, pch = 20, xlab = 'Longitude', ylab = 'Latitude')
# cor.test(m$lat, m$pop, method = 'spearman')
# cor.test(m$long, m$pop, method = 'spearman')
# cor.test(m$long, m$lat, method = 'spearman')

# Plot range of AH by country, colored by lat/long/popsize:
levels(ah$country) <- countries
m <- merge(m, ah, by = 'country')
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = sh, fill = long)) +
  theme_classic() + labs(x = '', y = 'AH', fill = '') +
  scale_fill_gradientn(colours = viridis(100))
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = sh, fill = lat)) +
  theme_classic() + labs(x = '', y = 'AH', fill = '') +
  scale_fill_gradientn(colours = viridis(100))
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = sh, fill = log(pop))) +
  theme_classic() + labs(x = '', y = 'AH', fill = '') +
  scale_fill_gradientn(colours = viridis(100))
grid.arrange(p1, p2, p3, ncol = 1)

# Plot median/min AH vs. lat/long/popsize:
ah.med <- aggregate(sh ~ country + lat + long + pop, data = m, FUN = median)
ah.min <- aggregate(sh ~ country + lat + long + pop, data = m, FUN = min)

par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(ah.med$lat, ah.med$sh, pch = 20, cex = 1.2, xlab = 'Latitude', ylab = 'Median AH')
plot(ah.min$lat, ah.min$sh, pch = 20, cex = 1.2, xlab = 'Latitude', ylab = 'Minimum AH')

plot(ah.med$long, ah.med$sh, pch = 20, cex = 1.2, xlab = 'Longitude', ylab = 'Median AH')
plot(ah.min$long, ah.min$sh, pch = 20, cex = 1.2, xlab = 'Longitude', ylab = 'Minimum AH')

# plot(log(ah.med$pop), ah.med$sh, pch = 20, cex = 1.2)
# plot(log(ah.min$pop), ah.min$sh, pch = 20, cex = 1.2)

# not really unexpected relationships, except min AH being typically lower in east (except IS)





