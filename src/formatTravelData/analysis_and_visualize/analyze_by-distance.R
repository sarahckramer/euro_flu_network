### Assess how travel magnitude trends with distance ###

pdf('src/formatTravelData/outputs/by_dist_5-28.pdf', width = 10, height = 5)

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
  load(paste0('src/formatTravelData/formattedData/air_', i, '_01-31.RData'))
  a.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym, i)
a.rand <- apply(simplify2array(a.by.month), 1:2, mean); rm(a.by.month)

### List countries used
countries <- rownames(a.rand)

### Load commuting data?
load('src/formatTravelData/formattedData/comm_mat_by_season_01-27.RData')
c.avg <- apply(simplify2array(comm.by.seas), 1:2, mean, na.rm = T); rm(comm.by.seas)

### Load centroid data
l <- read.csv('data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% countries, ]
l$iso_a2 <- factor(l$iso_a2)

# move: France, Croatia, UK
l$Longitude[6] <- 2
l$Latitude[6] <- 46.5

l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)
l <- cbind(countries, as.data.frame(l))

### Load capital data
countries.europe <- c('Austria', 'Belgium', 'Czech Republic', 'France', 'Germany', 'Hungary',
                      'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia', 'Spain')
data(world.cities)
world.cities <- world.cities[world.cities$country.etc %in% countries.europe, ]
world.cities$country.etc <- factor(world.cities$country.etc)
world.cities <- world.cities[world.cities$capital == 1, ]

world.cities <- world.cities[, c(2, 4:5)]
colnames(world.cities)[1] <- 'countries'
levels(world.cities$countries) <- countries[c(1:3, 6, 4, 7:12, 5)]
world.cities <- world.cities[, c(1, 3, 2)]

### Melt transportation matrices
df.rand <- melt(a.rand); names(df.rand) <- c('source', 'dest', 'w')
df.rand <- df.rand[df.rand$source != df.rand$dest, ]

df.comm <- melt(c.avg); names(df.comm) <- c('source', 'dest', 'w2')
df.comm <- df.comm[df.comm$source != df.comm$dest, ]

### Merge transportation matrices:
df.rand <- merge(df.rand, df.comm, by = c('source', 'dest'))

### Add centroid data to data frame
df.rand <- merge(df.rand, l, by.x = 'source', by.y = 'countries')
df.rand <- merge(df.rand, l, by.x = 'dest', by.y = 'countries')
names(df.rand)[5:8] <- c('x2', 'y2', 'x1', 'y1')
df.rand <- df.rand[, c(1:2, 7:8, 5:6, 3:4)]

### Add capital data to data frame
df.rand <- merge(df.rand, world.cities, by.x = 'source', by.y = 'countries')
df.rand <- merge(df.rand, world.cities, by.x = 'dest', by.y = 'countries')
df.rand <- df.rand[, c(1:6, 9:12, 7:8)]

### Calculate distances between centroids
df.rand$dist <- distGeo(df.rand[, 3:4], df.rand[, 5:6])
df.rand$dist <- df.rand$dist / 10000

### Calculate distances bewteen capitals
df.rand$dist.cap <- distGeo(df.rand[, 7:8], df.rand[, 9:10])
df.rand$dist.cap <- df.rand$dist.cap / 10000

### Reduce data frames, take log of weights
df.rand <- df.rand[, c(1:2, 11:14)]
df.rand$w <- log(df.rand$w)
df.rand$w2 <- log(df.rand$w2)

### How is air travel weight related to travel distance?

# Continuous distance
leg.labs <- c(0.1, 1.0, 10, 100, 1000, 10000, 50000)
p1 <- ggplot(data = df.rand) + geom_point(aes(x = dist, y = w), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(-1.5, 10.82))
p2 <- ggplot(data = df.rand) + geom_point(aes(x = dist.cap, y = w), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Passengers') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(-1.5, 10.82))
grid.arrange(p1, p2, ncol = 2)

cor.test(df.rand$dist, df.rand$w, method = 'pearson')
cor.test(df.rand$dist, df.rand$w, method = 'kendall')
# both sig pos

cor.test(df.rand$dist.cap, df.rand$w, method = 'pearson')
cor.test(df.rand$dist.cap, df.rand$w, method = 'kendall')
# same

leg.labs <- c(100, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
p1 <- ggplot(data = df.rand) + geom_point(aes(x = dist, y = w2), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Commuters') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(6.4, 11.4))
p2 <- ggplot(data = df.rand) + geom_point(aes(x = dist.cap, y = w2), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Commuters') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs, limits = c(6.4, 11.4))
grid.arrange(p1, p2, ncol = 2)

cor.test(df.rand$dist, df.rand$w2, method = 'pearson')
cor.test(df.rand$dist, df.rand$w2, method = 'kendall')
cor.test(df.rand$dist.cap, df.rand$w2, method = 'pearson')
cor.test(df.rand$dist.cap, df.rand$w2, method = 'kendall')
# all sig neg

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

leg.labs <- c(0.1, 1.0, 10, 100, 1000, 10000, 50000)
p3 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.disc, y = w), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p4 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.cap.disc, y = w), fill = 'lavender') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Passengers') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
grid.arrange(p3, p4, ncol = 2)

kruskal.test(w ~ dist.disc, data = df.rand) # sig
kruskal.test(w ~ dist.cap.disc, data = df.rand) # sig

posthoc.kruskal.nemenyi.test(w ~ dist.disc, data = df.rand) # none sig
posthoc.kruskal.nemenyi.test(w ~ dist.cap.disc, data = df.rand) # <50 lower than 100-150, 150+; 50-100 lower than 100-150

leg.labs <- c(100, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
p3 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.disc, y = w2), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Commuters') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p4 <- ggplot(data = df.rand) + geom_boxplot(aes(x = dist.cap.disc, y = w2), fill = 'lavender') +
  labs(x = 'Distance Between Capitals (in 10s of km)',
       y = '# of Commuters') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
grid.arrange(p3, p4, ncol = 2)

kruskal.test(w2 ~ dist.disc, data = df.rand) # sig
kruskal.test(w2 ~ dist.cap.disc, data = df.rand) # not sig; probably makes sense - people are commuting to border regions mostly, probably, not necessarily capitals

posthoc.kruskal.nemenyi.test(w2 ~ dist.disc, data = df.rand) # <50 higher than 100-150
# posthoc.kruskal.nemenyi.test(w2 ~ dist.cap.disc, data = df.rand) # don't do - kruskal not sig

# overall, a trend toward more people with greater distance for air travel, and the opposite for commuting (as expected)

dev.off()

rm(list = ls())
