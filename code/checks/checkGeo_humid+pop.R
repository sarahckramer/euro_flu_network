
# Look at whether there are longitudinal patterns in humidity and population size that
# could be driving spatial patterns in synthetic runs
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Read in population sizes:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in humidity data:
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
ah <- ah[, count.indices]

# First, a few countries w/ outlying AH patterns:
# matplot(ah, pch = 20, col = viridis(21), cex = 0.5)
matplot(ah, pch = 20, col = 'gray', cex = 0.5)
# lines(ah$Portugal, col = 'coral', lwd = 1.5)
# lines(ah$Sweden, col = 'green4', lwd = 1.5)
# lines(ah$Iceland, col = 'steelblue2', lwd = 1.5)

# Get latitude and longitude of country centroids (since humidity for whole country):
l <- read.csv('data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% countries, ]
# l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)

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
  theme_classic() + labs(x = '', y = 'AH', fill = 'Long.') +
  scale_fill_gradientn(colours = viridis(100))
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = sh, fill = lat)) +
  theme_classic() + labs(x = '', y = 'AH', fill = 'Lat.') +
  scale_fill_gradientn(colours = viridis(100))
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = sh, fill = log(pop))) +
  theme_classic() + labs(x = '', y = 'AH', fill = 'log(Pop.)') +
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

plot(log(ah.med$pop), ah.med$sh, pch = 20, cex = 1.2)
plot(log(ah.min$pop), ah.min$sh, pch = 20, cex = 1.2)

cor.test(ah.min$lat, ah.min$sh, method = 'kendall') # higher SH with lower lat (as expected), but barely sig; w/ kendall not sig
cor.test(ah.min$long, ah.min$sh, method = 'kendall') # higher SH with lower long (west higher SH than east) - much stronger than lat
cor.test(ah.min$pop, ah.min$sh, method = 'kendall') # barely sig pos

# lat seems to really just be 3 countries with lower latitude that also have higher sh (ES, IT, FR); otherwise AH values are clustered or even trend up with increasing lat. (NL almost as high as IT/FR)
# but west have very clearly higher values than east
# higher pop countries tend to have higher SH, but barely sig

a <- lm(sh ~ lat + long + pop, data = ah.min)
summary(a) # only long is sig

# Also look at range between min and max AH:
ah.max <- aggregate(sh ~ country + lat + long + pop, data = m, FUN = max)

ah.df <- merge(ah.min, ah.med, by = c('country', 'lat', 'long', 'pop'))
ah.df <- merge(ah.df, ah.max, by = c('country', 'lat', 'long', 'pop'))
names(ah.df)[5:7] <- c('sh.min', 'sh.med', 'sh.max')

cor.test(ah.df$sh.max, ah.df$sh.min, method = 'kendall')
cor.test(ah.df$sh.max, ah.df$sh.med, method = 'kendall')
cor.test(ah.df$sh.med, ah.df$sh.min, method = 'kendall')
# median and minimum are sig correlated, but neither with max?

ah.df$range <- ah.df$sh.max - ah.df$sh.min
ah.df$range.med <- ah.df$sh.med - ah.df$sh.min

plot(ah.df$lat, ah.df$range, pch = 20, cex = 1.2, xlab = 'Latitude', ylab = 'AH Range')
plot(ah.df$long, ah.df$range, pch = 20, cex = 1.2, xlab = 'Longitude', ylab = 'AH Range')

plot(ah.df$lat, ah.df$range.med, pch = 20, cex = 1.2, xlab = 'Latitude', ylab = 'AH Range')
plot(ah.df$long, ah.df$range.med, pch = 20, cex = 1.2, xlab = 'Longitude', ylab = 'AH Range')
# ES has very small range, but others are all pretty similar; no clear pattern by geography
    # with ES as exception, range between median and minimum tend to get smaller as we move north, but not east

# Look only over season:
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
ah <- ah[, count.indices]

# ah <- rbind(ah[273:365, ], ah[1:151, ]) # leaving out Jun-Sept
# matplot(ah, pch = 20, col = 'gray', cex = 0.5)

# When does AH "peak" (minimum) for each country?:
mins <- c()
for (i in 1:length(countries)) {
  mins <- c(mins, which.min(ah[, i]))
}
min.df <- as.data.frame(cbind(countries, mins))
# earliest are PL, SK, HU, CZ, AT
# next are BE, FR, LU, NL, ES, ~4-5 weeks later; DE and IT last, about a week later

# How does this compare to ILI+ peak timings?:
# Obviously the peak timings show the opposite pattern; but the minimum AHs in general occur within the 52:12 weeks of peak (on the early side)


























