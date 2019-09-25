
pdf('formatTravelData/outputs/by_dist_9-24.pdf', width = 10, height = 5)

library(maps)
library(reshape2)
library(geosphere)
library(ggplot2)
library(gridExtra)
library(PMCMR)
library(PMCMRplus)

### Load travel data
a.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  a.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym, i)
a.rand <- apply(simplify2array(a.by.month), 1:2, mean); rm(a.by.month)

### List countries used
countries <- rownames(a.rand)

### Load centroid data
l <- read.csv('../travel_data_info/flight_data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[(l$iso_a2 %in% countries | l$iso_a2 == 'GB') & !is.na(l$iso_a2), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)
l <- l[c(1:7, 9:18, 21:19, 8), ]
l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)
l <- cbind(countries, as.data.frame(l))

### Load capital data
countries.europe <- c('Austria', 'Belgium', 'Croatia', 'Czech Republic', 'Denmark', 'France',
                      'Germany', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg',
                      'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                      'Spain', 'Sweden', 'UK')
data(world.cities)
world.cities <- world.cities[world.cities$country.etc %in% countries.europe, ]
world.cities$country.etc <- factor(world.cities$country.etc)
world.cities <- world.cities[world.cities$capital == 1, ]

world.cities <- world.cities[, c(2, 4:5)]
colnames(world.cities)[1] <- 'countries'
levels(world.cities$countries) <- countries[c(1:2, 8, 3, 5, 7, 4, 9, 11:10, 12:17, 20:19, 6, 18, 21)]
world.cities <- world.cities[, c(1, 3, 2)]

### Melt transportation matrices
df.rand <- melt(a.rand); names(df.rand) <- c('source', 'dest', 'w')
df.rand <- df.rand[df.rand$source != df.rand$dest, ]

### Add centroid data to data frame
df.rand <- merge(df.rand, l, by.x = 'source', by.y = 'countries')
df.rand <- merge(df.rand, l, by.x = 'dest', by.y = 'countries')
names(df.rand)[4:7] <- c('x2', 'y2', 'x1', 'y1')
df.rand <- df.rand[, c(1:2, 6:7, 4:5, 3)]

### Add capital data to data frame
df.rand <- merge(df.rand, world.cities, by.x = 'source', by.y = 'countries')
df.rand <- merge(df.rand, world.cities, by.x = 'dest', by.y = 'countries')
df.rand <- df.rand[, c(1:6, 8:11, 7)]

### Calculate distances between centroids
df.rand$dist <- distGeo(df.rand[, 3:4], df.rand[, 5:6])
df.rand$dist <- df.rand$dist / 10000

### Calculate distances bewteen capitals
df.rand$dist.cap <- distGeo(df.rand[, 7:8], df.rand[, 9:10])
df.rand$dist.cap <- df.rand$dist.cap / 10000

### Reduce data frames, take log of weights
df.rand <- df.rand[, c(1:2, 11:13)]
df.rand$w <- log(df.rand$w)

### Remove IS!
df.rand <- df.rand[df.rand$source != 'IS' & df.rand$dest != 'IS', ]
df.rand$source <- factor(df.rand$source); df.rand$dest <- factor(df.rand$dest)

### How is air travel weight related to travel distance?

# Continuous distance
leg.labs <- c(0.1, 1.0, 10, 100, 1000, 10000, 50000)
p1 <- ggplot(data = df.rand) + geom_point(aes(x = dist, y = w), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(-1.5, 10.82))
p2 <- ggplot(data = df.rand) + geom_point(aes(x = dist.cap, y = w), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(-1.5, 10.82))
grid.arrange(p1, p2, ncol = 2)

cor.test(df.rand$dist, df.rand$w, method = 'pearson')
cor.test(df.rand$dist, df.rand$w, method = 'kendall')
# post sig pos, but weak

cor.test(df.rand$dist.cap, df.rand$w, method = 'pearson')
cor.test(df.rand$dist.cap, df.rand$w, method = 'kendall')
# same

# Discrete distance
df.rand$dist.disc = df.rand$dist.cap.disc = NA
df.rand$dist.disc[df.rand$dist < 50] <- '<50'
df.rand$dist.disc[df.rand$dist < 100 & df.rand$dist >= 50] <- '50-100'
df.rand$dist.disc[df.rand$dist < 150 & df.rand$dist >= 100] <- '100-150'
df.rand$dist.disc[df.rand$dist >= 150] <- '150+'
df.rand$dist.disc <- factor(df.rand$dist.disc)
df.rand$dist.disc <- factor(df.rand$dist.disc,
                             levels = levels(df.rand$dist.disc)[c(1, 4, 2:3)])
df.rand$dist.cap.disc[df.rand$dist.cap < 50] <- '<50'
df.rand$dist.cap.disc[df.rand$dist.cap < 100 & df.rand$dist.cap >= 50] <- '50-100'
df.rand$dist.cap.disc[df.rand$dist.cap < 150 & df.rand$dist.cap >= 100] <- '100-150'
df.rand$dist.cap.disc[df.rand$dist.cap >= 150] <- '150+'
df.rand$dist.cap.disc <- factor(df.rand$dist.cap.disc)
df.rand$dist.cap.disc <- factor(df.rand$dist.cap.disc,
                                 levels = levels(df.rand$dist.cap.disc)[c(1, 4, 2:3)])

p3 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.disc, y = w), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p4 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.cap.disc, y = w), fill = 'lavender') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
grid.arrange(p3, p4, ncol = 2)

kruskal.test(w ~ dist.disc, data = df.rand) # sig
kruskal.test(w ~ dist.cap.disc, data = df.rand) # sig

posthoc.kruskal.nemenyi.test(w ~ dist.disc, data = df.rand) # 100-150 higher than all other categories
posthoc.kruskal.nemenyi.test(w ~ dist.cap.disc, data = df.rand) # same

dev.off()






