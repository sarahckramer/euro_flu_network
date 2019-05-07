
# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/prelim_plots/by_dist.pdf',
#     width = 10, height = 5)

library(maps)
library(reshape2)
library(geosphere)
library(ggplot2)
library(gridExtra)

### Load travel data
a.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/air_', i, '_02-07.RData'))
  a.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym, i)
a.rand <- apply(simplify2array(a.by.month), 1:2, mean); rm(a.by.month)
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/train_02-07.RData')
t.rand <- t.rand.sym; rm(t.rand.sym)

### List countries used
countries <- rownames(a.rand)

### Load centroid data
l <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% countries | l$iso_a2 %in% c('GB', 'GR'), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[11:12] <- c('UK', 'EL'); l$iso_a2 <- factor(l$iso_a2)
l <- l[c(1:7, 12, 8:10, 13:28, 11), ]
l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)
l <- cbind(countries, as.data.frame(l))

### Load capital data
countries.europe <- c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark', 'Estonia',
                      'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy',
                      'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Norway', 'Poland', 'Portugal',
                      'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'UK')

data(world.cities)
world.cities <- world.cities[world.cities$country.etc %in% countries.europe, ]
world.cities$country.etc <- factor(world.cities$country.etc)
world.cities <- world.cities[world.cities$capital == 1, ]

world.cities <- world.cities[, c(2, 4:5)]
colnames(world.cities)[1] <- 'countries'
levels(world.cities$countries) <- countries[c(1:3, 12, 4, 6:7, 10:11, 5, 8, 13, 15, 14,
                                              16, 19, 17, 18, 20:24, 27, 26, 9, 25, 28)]
world.cities <- world.cities[, c(1, 3, 2)]

### Melt transportation matrices
df.t.rand <- melt(t.rand); names(df.t.rand) <- c('source', 'dest', 'w.train')
df.a.rand <- melt(a.rand); names(df.a.rand) <- c('source', 'dest', 'w.air')

### Create data frame of all train and air data
df.rand <- merge(df.t.rand, df.a.rand, by = c('source', 'dest'), all = TRUE)
df.rand <- df.rand[df.rand$source != df.rand$dest, ]
df.rand$w.train[df.rand$w.train == 0 & !is.na(df.rand$w.train)] <- NA

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

### Separate routes with air but no train, both air and train
df.rand.no.t <- df.rand[is.na(df.rand$w.train), ] # air but no train
df.rand <- df.rand[!is.na(df.rand$w.train), ] # both air and train

### Check whether air or train dominate
df.rand$ratio <- df.rand$w.air / df.rand$w.train
df.rand$dom <- ifelse(df.rand$ratio > 1, 'Air', 'Train')
df.rand$ratio[df.rand$dom == 'Train'] <- 1 / df.rand$ratio[df.rand$dom == 'Train']
df.rand$dom <- factor(df.rand$dom)

### Reduce data frames, take log of weights
df.rand.no.t <- df.rand.no.t[, c(1:2, 13:14, 12)]
df.rand.no.t$w.air <- log(df.rand.no.t$w.air)

df.rand.comp <- df.rand[, c(1:2, 13:14, 11:12, 15:16)]
df.rand.comp$w.train <- log(df.rand.comp$w.train)
df.rand.comp$w.air <- log(df.rand.comp$w.air)
rm(df.rand)

### TRAIN ONLY ###
# How is travel weight related to travel distance?
df.train <- df.rand.comp

# Continuous distance
leg.labs <- c(0.1, 1.0, 10, 100, 1000, 10000, 100000)
p1 <- ggplot(data = df.train) + geom_point(aes(x = dist, y = w.train), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p1

cor.test(df.train$dist, df.train$w.train, method = 'pearson')
cor.test(df.train$dist, df.train$w.train, method = 'kendall')
# both yield highly significant negative correlations

cor.test(df.train$dist.cap, df.train$w.train, method = 'pearson')
cor.test(df.train$dist.cap, df.train$w.train, method = 'kendall')
# same

# Discrete distance
df.train$dist.disc <- NA; df.train$dist.cap.disc <- NA
df.train$dist.disc[df.train$dist < 50] <- '<50'
df.train$dist.disc[df.train$dist < 100 & df.train$dist >= 50] <- '50-100'
df.train$dist.disc[df.train$dist < 150 & df.train$dist >= 100] <- '100-150'
df.train$dist.disc[df.train$dist >= 150] <- '150+'
df.train$dist.disc <- factor(df.train$dist.disc)
df.train$dist.disc <- factor(df.train$dist.disc,
                             levels = levels(df.train$dist.disc)[c(1, 4, 2:3)])
df.train$dist.cap.disc[df.train$dist.cap < 50] <- '<50'
df.train$dist.cap.disc[df.train$dist.cap < 100 & df.train$dist.cap >= 50] <- '50-100'
df.train$dist.cap.disc[df.train$dist.cap < 150 & df.train$dist.cap >= 100] <- '100-150'
df.train$dist.cap.disc[df.train$dist.cap >= 150] <- '150+'
df.train$dist.cap.disc <- factor(df.train$dist.cap.disc)
df.train$dist.cap.disc <- factor(df.train$dist.cap.disc,
                                 levels = levels(df.train$dist.cap.disc)[c(1, 4, 2:3)])

p2 <- ggplot(data = df.train) + geom_boxplot(aes(x = dist.disc, y = w.train), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p2

a <- aov(w.train ~ dist.disc, data = df.train) # sig
b <- aov(w.train ~ dist.cap.disc, data = df.train) # sig

p.sig <- 0.05 / 6
TukeyHSD(a) # sig more travel at <50 than all others; 50-100 sig more than 150+
TukeyHSD(b) # sig more travel at <50 than all others
# applying Bonferroni to both analyses

### AIR ONLY ###
# How is travel weight related to travel distance?
df.air <- df.rand.comp[, c(1:4, 6, 8)]
df.rand.no.t$dom <- 'No Train'
df.air <- rbind(df.air, df.rand.no.t)
df.air$dom <- factor(df.air$dom)
df.air$dom <- factor(df.air$dom, levels = levels(df.air$dom)[c(3, 1, 2)])

# Continuous distance
leg.labs <- c(0.01, 0.1, 1.0, 10, 100, 1000, 10000)
p3 <- ggplot(data = df.air) + geom_point(aes(x = dist, y = w.air), size = 1.5, col = 'steelblue2') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p3
# note differences in y-axis extent

cor.test(df.air$dist, df.air$w.air, method = 'pearson')
cor.test(df.air$dist, df.air$w.air, method = 'kendall')
# sig negative, but weaker than train

cor.test(df.air$dist.cap, df.air$w.air, method = 'pearson')
cor.test(df.air$dist.cap, df.air$w.air, method = 'kendall')
# same

# p4 <- ggplot(data = df.air) + geom_point(aes(x = dist, y = w.air, col = dom), size = 1.5) +
#   labs(title = 'Air Travel', x = 'Distance Between Centroids (in 10s of km)',
#        y = '# of Passengers (per 100,000 population)', col = 'Dominant Travel Mode') +
#   theme_bw() + theme(text = element_text(size = 14)) +
#   scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
# p4
# 
# cor.test(df.air$dist[df.air$dom == 'Train'], df.air$w.air[df.air$dom == 'Train'])
# # not sig
# cor.test(df.air$dist[df.air$dom == 'Air'], df.air$w.air[df.air$dom == 'Air'])
# # not sig

# Discrete distance
df.air$dist.disc <- NA; df.air$dist.cap.disc <- NA
df.air$dist.disc[df.air$dist < 50] <- '<50'
df.air$dist.disc[df.air$dist < 100 & df.air$dist >= 50] <- '50-100'
df.air$dist.disc[df.air$dist < 150 & df.air$dist >= 100] <- '100-150'
df.air$dist.disc[df.air$dist >= 150] <- '150+'
df.air$dist.disc <- factor(df.air$dist.disc)
df.air$dist.disc <- factor(df.air$dist.disc,
                             levels = levels(df.air$dist.disc)[c(1, 4, 2:3)])
df.air$dist.cap.disc[df.air$dist.cap < 50] <- '<50'
df.air$dist.cap.disc[df.air$dist.cap < 100 & df.air$dist.cap >= 50] <- '50-100'
df.air$dist.cap.disc[df.air$dist.cap < 150 & df.air$dist.cap >= 100] <- '100-150'
df.air$dist.cap.disc[df.air$dist.cap >= 150] <- '150+'
df.air$dist.cap.disc <- factor(df.air$dist.cap.disc)
df.air$dist.cap.disc <- factor(df.air$dist.cap.disc,
                                 levels = levels(df.air$dist.cap.disc)[c(1, 4, 2:3)])

p4 <- ggplot(data = df.air) + geom_boxplot(aes(x = dist.disc, y = w.air), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)',
       y = '# of Passengers (per 100,000 population)') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
p4

a <- aov(w.air ~ dist.disc, data = df.air) # borderline sig (p = 0.09)
b <- aov(w.air ~ dist.cap.disc, data = df.air) # borderline sig (p = 0.08)

grid.arrange(p3, p4, ncol = 2)

rm(df.train); rm(df.air)

### COMPARE DISTANCE AND DOMINANCE ###
df.rand.comp$dom <- factor(df.rand.comp$dom)
levels(df.rand.comp$dom) <- c('Air Dominates', 'Train Dominates')

### Strength of dominance vs. distance
leg.labs <- c(1.0, 10, 100, 1000, 10000)
p5 <- ggplot(df.rand.comp) + geom_point(aes(x = dist, y = log(ratio))) + facet_wrap(~dom) +
  theme_bw() + theme(text = element_text(size = 14), strip.text = element_text(size = 14)) +
  labs(x = 'Distance Between Centroids (in 10s of km)', y = 'Dominance Factor') + 
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs)
print(p5)

cor.test(df.rand.comp$dist[df.rand.comp$dom == 'Train Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Train Dominates']))
cor.test(df.rand.comp$dist[df.rand.comp$dom == 'Air Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Air Dominates']))
# not sig

cor.test(df.rand.comp$dist[df.rand.comp$dom == 'Train Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Train Dominates']), method = 'kendall', exact = FALSE)
cor.test(df.rand.comp$dist[df.rand.comp$dom == 'Air Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Air Dominates']), method = 'kendall')
# only second significant; positive, but low (0.123); higher ratio with increased distance, but only for air

cor.test(df.rand.comp$dist.cap[df.rand.comp$dom == 'Train Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Train Dominates']))
cor.test(df.rand.comp$dist.cap[df.rand.comp$dom == 'Air Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Air Dominates']))
cor.test(df.rand.comp$dist.cap[df.rand.comp$dom == 'Train Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Train Dominates']), method = 'kendall')
cor.test(df.rand.comp$dist.cap[df.rand.comp$dom == 'Air Dominates'], log(df.rand.comp$ratio[df.rand.comp$dom == 'Air Dominates']), method = 'kendall')
# all sig - when train dominates, ratios decrease with increasing distance; when air dominates, they increase

# try discrete as well:
df.rand.comp$dist.disc <- NA; df.rand.comp$dist.cap.disc <- NA
df.rand.comp$dist.disc[df.rand.comp$dist < 50] <- '<50'
df.rand.comp$dist.disc[df.rand.comp$dist < 100 & df.rand.comp$dist >= 50] <- '50-100'
df.rand.comp$dist.disc[df.rand.comp$dist < 150 & df.rand.comp$dist >= 100] <- '100-150'
df.rand.comp$dist.disc[df.rand.comp$dist >= 150] <- '150+'
df.rand.comp$dist.disc <- factor(df.rand.comp$dist.disc)
df.rand.comp$dist.disc <- factor(df.rand.comp$dist.disc,
                           levels = levels(df.rand.comp$dist.disc)[c(1, 4, 2:3)])
df.rand.comp$dist.cap.disc[df.rand.comp$dist.cap < 50] <- '<50'
df.rand.comp$dist.cap.disc[df.rand.comp$dist.cap < 100 & df.rand.comp$dist.cap >= 50] <- '50-100'
df.rand.comp$dist.cap.disc[df.rand.comp$dist.cap < 150 & df.rand.comp$dist.cap >= 100] <- '100-150'
df.rand.comp$dist.cap.disc[df.rand.comp$dist.cap >= 150] <- '150+'
df.rand.comp$dist.cap.disc <- factor(df.rand.comp$dist.cap.disc)
df.rand.comp$dist.cap.disc <- factor(df.rand.comp$dist.cap.disc,
                               levels = levels(df.rand.comp$dist.cap.disc)[c(1, 4, 2:3)])

p6 <- ggplot(data = df.rand.comp) +
  geom_boxplot(aes(x = dist.disc, y = log(ratio)), fill = 'lavender') +
  labs(x = 'Distance Between Centroids (in 10s of km)', y = 'Dominance Factor') +
  theme_bw() + theme(text = element_text(size = 14), strip.text = element_text(size = 14)) +
  scale_y_continuous(breaks = log(leg.labs), labels = leg.labs) +
  facet_wrap(~dom, ncol = 2)
print(p6)

a <- aov(log(ratio) ~ dist.disc, data = df.rand.comp[df.rand.comp$dom == 'Air Dominates', ])
b <- aov(log(ratio) ~ dist.cap.disc, data = df.rand.comp[df.rand.comp$dom == 'Air Dominates', ])
# both highly sig

c <- aov(log(ratio) ~ dist.disc, data = df.rand.comp[df.rand.comp$dom == 'Train Dominates', ])
d <- aov(log(ratio) ~ dist.cap.disc, data = df.rand.comp[df.rand.comp$dom == 'Train Dominates', ])
# neither sig

# Tukey w/ Bonferroni correction - <0.00833
TukeyHSD(a) # highest two groups sig higher than lowest
TukeyHSD(b) # same
# little data less than 50; seems that cutoff at about 1000kms where higher ratios
TukeyHSD(c) # none sig
TukeyHSD(d) # none sig

### Distance vs. Likelihood of air/train dominating
p7 <- ggplot(data = df.rand.comp) + geom_boxplot(aes(x = dom, y = dist), fill = 'lavender') +
  labs(x = 'Dominant Mode of Travel', y = 'Distance Between Centroids (in 10s of km)') +
  theme_bw() + theme(text = element_text(size = 14), strip.text = element_text(size = 14))
p7

a <- aov(dist ~ dom, data = df.rand.comp) # sig
b <- aov(dist.cap ~ dom, data = df.rand.comp) # highly sig
# So train travel is more likely to dominate at short distances

### Distance vs. Train present/absent
df.rand.comp <- df.rand.comp[, c(1:4, 6, 8)]
df.rand.comp$train <- 'Yes'; df.rand.no.t$train <- 'No'
df.rand.comp <- rbind(df.rand.comp, df.rand.no.t)

p8 <- ggplot(data = df.rand.comp) + geom_boxplot(aes(x = train, y = dist), fill = 'lavender') +
  labs(x = 'Train Data Available?', y = 'Distance Between Centroids (in 10s of km)') +
  theme_bw() + theme(text = element_text(size = 14), strip.text = element_text(size = 14))
p8

a <- aov(dist ~ train, data = df.rand.comp) # highly sig
b <- aov(dist.cap ~ train, data = df.rand.comp) # highly sig

grid.arrange(p7, p8, ncol = 2)

# dev.off()










