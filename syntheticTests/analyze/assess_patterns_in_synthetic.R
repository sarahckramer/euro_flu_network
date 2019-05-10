
### Assess patterns in synthetic "data" and compare to observed data

### Read in synthetic "data" ###
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
load('syntheticTests/syntheticData/synth_05-09_RATES1.RData')

### Plot synthetic runs ###
par(mfrow = c(3, 7), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (ix in 1:length(synth.runs.RATES)) {
  matplot(t(synth.runs.RATES[[ix]]), type = 'b', pch = 20, lty = 1, col = viridis(length(countries)),
          xlab = 'Weeks from Outbreak Start', ylab = 'Cases / 100,000')
}

### Create df of AR/PT/OT by country and run ###
source('code/functions/Util.R')
wk_start <- 1
ar.dists = pt.dists = ot.dists = vector('list', length(synth.runs.RATES))
for (ix in 1:length(synth.runs.RATES)) {
  
  for (count.index in 1:length(countries)) {
    ili.temp <- synth.runs.RATES[[ix]][count.index, ]
    
    ar.dists[[ix]] <- c(ar.dists[[ix]], sum(ili.temp))
    pt.dists[[ix]] <- c(pt.dists[[ix]], which.max(ili.temp))
    ot.dists[[ix]] <- c(ot.dists[[ix]], findOnset(ili.temp, 500)$onset)
  }
  
}

run.nums <- c()
for (i in 1:length(synth.runs.RATES)) {
  run.nums <- c(run.nums, rep(i, length(countries)))
}

m <- as.data.frame(cbind(run.nums, rep(countries, length(synth.runs.RATES)),
                         unlist(ar.dists), unlist(pt.dists), unlist(ot.dists)))
names(m) <- c('run', 'country', 'ar', 'pt', 'ot')
for (i in 3:5) {
  m[, i] <- as.numeric(as.character(m[, i]))
}
m$pt <- m$pt + 39; m$ot <- m$ot + 39

# Which countries/runs have no onset?
m.noOnset <- m[is.na(m$ot), ]; m.noOnset$country <- factor(m.noOnset$country)
print(length(unique(m.noOnset$run))) # 11 / 21 runs have at least 1 country with no onset - this seems like a lot
rev(sort(table(m.noOnset$country))) # 6/12 are PT; 2 SI, 2 FR, 1 RO, 1 IT
# obs: NL and SK in 13-14

# Save which runs have at least one country with no onset - can compare later
noOn <- unique(m.noOnset$run)
allOn <- (1:21)[which(!(1:21 %in% unique(m.noOnset$run)))]

# Now remove those for further analyses:
m <- m[!is.na(m$ot), ]

### Look at range of AR, PT, OT by COUNTRY ###
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ar), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)')
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ot), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week') + scale_y_continuous(breaks = seq(46, 68, by = 2))
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt), fill = 'lightblue2') +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week') + scale_y_continuous(breaks = seq(46, 68, by = 2))
grid.arrange(p1, p2, p3, ncol = 1)
# looks like IS very early in the model, despite it usually being very late

# And plot with geographic information:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
                   'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
                   'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                   'United Kingdom')
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'long')]
m <- merge(m, world.cities, by.x = 'country', by.y = 'country.etc')
rm(world.cities)

ar.med <- aggregate(ar ~ country, data = m, FUN = median)
ar.med <- ar.med[order(ar.med$ar, decreasing = TRUE), ]

ot.med <- aggregate(ot ~ country, data = m, FUN = median)
ot.med <- ot.med[order(ot.med$ot), ]

pt.med <- aggregate(pt ~ country, data = m, FUN = median)
pt.med <- pt.med[order(pt.med$pt), ]

# Largest: R0, IS, HU, AT, SK; smallest: PT, IT, BE, IE
    # Somewhat consistent? But RO should be smaller and BE larger, for example
# IS earliest onset by far; IT and PT tend to be last; same pattern for PT
    # IS and SK are early here, but should be much later

m$country <- factor(m$country, levels = ar.med$country)
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ar, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)', fill = 'Long.') +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = ot.med$country)
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ot, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = pt.med$country)
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, p3, ncol = 1)
# somewhat larger in east? but biggest deal is RO and IS are very large - strange travel connections?
# vague east-to-west pattern

# Test correlation between median values in observed and simulated data:
load('code/checks/analyzeDataRetro/outputs/median_metrics.RData')
med.m <- merge(ar.med, pt.med, by = 'country')
med.m <- merge(med.m, ot.med, by = 'country')

names(obs.dist[[1]])[2] <- 'ar_obs'
names(obs.dist[[2]])[2] <- 'ot_obs'
names(obs.dist[[3]])[2] <- 'pt_obs'

med.m <- merge(med.m, obs.dist[[1]], by = 'country')
med.m <- merge(med.m, obs.dist[[2]], by = 'country')
med.m <- merge(med.m, obs.dist[[3]], by = 'country')

cor.test(med.m$ar, med.m$ar_obs, method = 'spearman') # not sig
cor.test(med.m$ot, med.m$ot_obs, method = 'spearman') # sig negative
cor.test(med.m$pt, med.m$pt_obs, method = 'spearman') # sig negative

# Plot and correlate again using only runs in allOn:
m.allOn <- m[m$run %in% allOn, ]

ar.med <- aggregate(ar ~ country, data = m.allOn, FUN = median)
ar.med <- ar.med[order(ar.med$ar, decreasing = TRUE), ]

ot.med <- aggregate(ot ~ country, data = m.allOn, FUN = median)
ot.med <- ot.med[order(ot.med$ot), ]

pt.med <- aggregate(pt ~ country, data = m.allOn, FUN = median)
pt.med <- pt.med[order(pt.med$pt), ]

m.allOn$country <- factor(m.allOn$country, levels = ar.med$country)
p1 <- ggplot(data = m.allOn) + geom_boxplot(aes(x = country, y = ar, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)', fill = 'Long.') +
  scale_fill_gradientn(colors = viridis(100))
m.allOn$country <- factor(m.allOn$country, levels = ot.med$country)
p2 <- ggplot(data = m.allOn) + geom_boxplot(aes(x = country, y = ot, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m.allOn$country <- factor(m.allOn$country, levels = pt.med$country)
p3 <- ggplot(data = m.allOn) + geom_boxplot(aes(x = country, y = pt, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week', fill = 'Long.') +
  scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, p3, ncol = 1)
# still east to west pattern; IS still too early but a little less extreme

### Look at range of AR, PT, OT by RUN ###
# Overall, simulated outbreaks seem a bit larger and earlier to start than observed

# Check whether this is different for runs with and w/o any no-onset countries:
m$allOn <- NA
m$allOn[m$run %in% allOn] <- 'All Onset'
m$allOn[m$run %in% noOn] <- 'Any No Onset'
m$allOn <- factor(m$allOn)

par(mfrow = c(1, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(ar ~ allOn, data = m, ylab = 'Attack Rate (per 100,000)', col = 'lightblue2')
boxplot(ot ~ allOn, data = m, ylab = 'Onset Timing', col = 'lightblue2')
boxplot(pt ~ allOn, data = m, ylab = 'Peak Timing', col = 'lightblue2')

summary(aov(ar ~ allOn, data = m)) # sig higher when all onset; but looks to be a lot of overlap
summary(aov(ot ~ allOn, data = m)) # not
summary(aov(pt ~ allOn, data = m)) # not sig

# Test differences statistically?
library(PMCMR); library(PMCMRplus)
kruskal.test(ar ~ country, data = m)
kruskal.test(ot ~ country, data = m)
kruskal.test(pt ~ country, data = m)
# all sig

rev(sort(table(cbind(levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$ar, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 1] + 1],
                     levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$ar, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 2]]))))
rev(sort(table(cbind(levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$ot, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 1] + 1],
                     levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$ot, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 2]]))))
rev(sort(table(cbind(levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$pt, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 1] + 1],
                     levels(m$country)[which(posthoc.kruskal.nemenyi.test(m$pt, m$country, 'Tukey')$p.value < 0.05, arr.ind = TRUE)[, 2]]))))
# most involve IS; for AR: PT also involved a lot; for PT and OT: IT and PT also involved a lot
# basically, there are some significant differences, but mostly against the extremes

### Assess synchrony ###
# For each run, what is the range of peak timings?
m$country <- factor(m$country, levels = countries)
par(mfrow = c(3, 7), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:21) {
  m.temp <- m[m$run == i, ]
  hist(m.temp$pt, breaks = 21, xlab = 'PT', main = i)
  print(max(m.temp$pt) - min(m.temp$pt))
  print(as.vector(quantile(m.temp$pt, prob = 0.975) - quantile(m.temp$pt, prob = 0.025)))
  print('')
}
# seem to be less synchronous - wider range of peak timings; also seem like there are more "extremes"

# Calculate correlation coefficients between all pairs of countries for each run:
cor.synch <- vector('list', length(levels(m$run)))
for (ix in 1:length(cor.synch)) {
  cor.mat <- matrix(NA, nrow = length(countries), ncol = length(countries))
  diag(cor.mat) <- 1.0
  
  for (i in 1:(length(countries) - 1)) {
    for (j in (i + 1):length(countries)) {
      # plot(synth.runs.RATES[[ix]][i, ], type = 'b', pch = 20)
      # points(synth.runs.RATES[[ix]][j, ], col = 'blue', type = 'b', pch = 20)
      
      cor.mat[i, j] = cor.mat[j, i] = cor(synth.runs.RATES[[ix]][i, ],
                                          synth.runs.RATES[[ix]][j, ],
                                          method = 'spearman')
    }
  }
  
  print(isSymmetric(cor.mat))
  cor.synch[[ix]] <- cor.mat
}

# Average cor.synch across all seasons:
cor.synch.AVG <- apply(simplify2array(cor.synch), 1:2, mean)
print(isSymmetric(cor.synch.AVG))

# Test average synchrony against distance using Mantel test:
library(ecodist); library(geosphere)

data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
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
# highly sig, mantel r = 0.3552427

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
# sig one-sided, but not two-sided

# Plot mean correlations as matrix:
rownames(cor.synch.AVG) = colnames(cor.synch.AVG) = c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
                                                      'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
cor.synch.AVG[upper.tri(cor.synch.AVG)] <- NA
cor.synch.plot <- melt(cor.synch.AVG)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')
cor.synch.plot <- cor.synch.plot[!is.na(cor.synch.plot$corr), ]
cor.synch.plot$corr[cor.synch.plot$corr == 1] <- NA

# ranges from 0.2999 to 0.9566 (vs. 0.4216 to 0.9487)
# removing IS bumps min to 0.635 - so actually seems more synchronous than obs
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
cor.synch.plot$comm[cor.synch.plot$comm == -Inf] <- NA

par(mfrow = c(1, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(cor.synch.plot$air, cor.synch.plot$corr, pch = 20, xlab = 'log(Air Passengers)', ylab = 'Correlation')
plot(cor.synch.plot[!is.na(cor.synch.plot$comm), ]$comm, cor.synch.plot[!is.na(cor.synch.plot$comm), ]$corr, pch = 20, xlab = 'log(Commuters)', ylab = 'Correlation')
plot(cor.synch.plot$dist, cor.synch.plot$corr, pch = 20, xlab = 'Distance (100 km)', ylab = 'Correlation')

# Check synchrony for allOn vs. anyNoOn:
cor.synch.allOn <- vector('list', length(allOn))
cor.synch.anyNoOn <- vector('list', length(noOn))

for (ix in 1:length(cor.synch.allOn)) {
  run <- allOn[ix]
  
  cor.mat <- matrix(NA, nrow = length(countries), ncol = length(countries))
  diag(cor.mat) <- 1.0
  
  for (i in 1:(length(countries) - 1)) {
    for (j in (i + 1):length(countries)) {
      cor.mat[i, j] = cor.mat[j, i] = cor(synth.runs.RATES[[run]][i, ],
                                          synth.runs.RATES[[run]][j, ],
                                          method = 'spearman')
    }
  }
  
  print(isSymmetric(cor.mat))
  cor.synch.allOn[[ix]] <- cor.mat
}
for (ix in 1:length(cor.synch.anyNoOn)) {
  run <- noOn[ix]
  
  cor.mat <- matrix(NA, nrow = length(countries), ncol = length(countries))
  diag(cor.mat) <- 1.0
  
  for (i in 1:(length(countries) - 1)) {
    for (j in (i + 1):length(countries)) {
      cor.mat[i, j] = cor.mat[j, i] = cor(synth.runs.RATES[[run]][i, ],
                                          synth.runs.RATES[[run]][j, ],
                                          method = 'spearman')
    }
  }
  
  print(isSymmetric(cor.mat))
  cor.synch.anyNoOn[[ix]] <- cor.mat
}

cor.synch.AVG.allOn <- apply(simplify2array(cor.synch.allOn), 1:2, mean)
cor.synch.AVG.anyNoOn <- apply(simplify2array(cor.synch.anyNoOn), 1:2, mean)
print(isSymmetric(cor.synch.AVG.allOn))
print(isSymmetric(cor.synch.AVG.anyNoOn))

rownames(cor.synch.AVG.allOn) = colnames(cor.synch.AVG.allOn) = 
  rownames(cor.synch.AVG.anyNoOn) = colnames(cor.synch.AVG.anyNoOn) =
  c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
    'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

cor.synch.AVG.allOn[upper.tri(cor.synch.AVG.allOn)] <- NA
cor.synch.plot.allOn <- melt(cor.synch.AVG.allOn)
names(cor.synch.plot.allOn) <- c('c1', 'c2', 'corr')
cor.synch.plot.allOn <- cor.synch.plot.allOn[!is.na(cor.synch.plot.allOn$corr), ]
cor.synch.plot.allOn$corr[cor.synch.plot.allOn$corr == 1] <- NA

cor.synch.AVG.anyNoOn[upper.tri(cor.synch.AVG.anyNoOn)] <- NA
cor.synch.plot.anyNoOn <- melt(cor.synch.AVG.anyNoOn)
names(cor.synch.plot.anyNoOn) <- c('c1', 'c2', 'corr')
cor.synch.plot.anyNoOn <- cor.synch.plot.anyNoOn[!is.na(cor.synch.plot.anyNoOn$corr), ]
cor.synch.plot.anyNoOn$corr[cor.synch.plot.anyNoOn$corr == 1] <- NA

# correlation ranges are similar
p1 <- ggplot(cor.synch.plot.allOn, aes(x = c1, y = c2)) + geom_tile(aes(fill = corr), colour = 'white') +
  scale_fill_gradientn(colours = cividis(100), na.value = 'gray80', limits = c(0.34, 0.97)) + theme_classic() +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Average Synchrony (All Onset)', x = '', y = '', fill = 'Corr.')
p2 <- ggplot(cor.synch.plot.anyNoOn, aes(x = c1, y = c2)) + geom_tile(aes(fill = corr), colour = 'white') +
  scale_fill_gradientn(colours = cividis(100), na.value = 'gray80', limits = c(0.34, 0.97)) + theme_classic() +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Average Synchrony (Any No Onset)', x = '', y = '', fill = 'Corr.')
grid.arrange(p1, p2, ncol = 2)

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















### Look at parameters and initial states for allOn vs. anyNoOn ###






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




