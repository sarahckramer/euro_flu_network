
### Assess patterns in synthetic "data" and compare to observed data ###
library(reshape2); library(ggplot2); library(gridExtra); library(viridis)

### Save all plots:
pdf('src/syntheticTests/outputs/explore/patterns_synth_realisticOnly.pdf', width = 16, height = 10)
# note: plot "realistic" only

### Read in synthetic "data" ###
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
load('src/syntheticTests/syntheticData/synth_rates_REALISTIC_1000_070220.RData')
synth.runs.RATES <- synth.runs.RATES.realistic; rm(synth.runs.RATES.realistic)

# ### Plot synthetic runs ###
# par(mfrow = c(3, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# # par(mfrow = c(2, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (ix in 1:length(synth.runs.RATES)) {
#   matplot(t(synth.runs.RATES[[ix]]), type = 'b', pch = 20, lty = 1, col = viridis(length(countries)),
#           xlab = 'Weeks from Outbreak Start', ylab = 'Cases / 100,000')
# }

### Create df of AR/PT/OT by country and run ###
source('src/mainCode/functions/Util.R')
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

# If onset NA, so is pt:
m$pt[is.na(m$ot)] <- NA

# And calculate time since first onset/peak:
m.new <- NULL
for (ix in levels(m$run)) {
  m.temp <- m[m$run == ix, ]
  
  min.ot <- min(m.temp$ot, na.rm = TRUE)
  min.pt <- min(m.temp$pt, na.rm = TRUE)
  
  m.temp$ot_order <- m.temp$ot - min.ot
  m.temp$pt_order <- m.temp$pt - min.pt
  
  m.new <- rbind(m.new, m.temp)
}
m.new <- as.data.frame(m.new)
m <- m.new
rm(m.new, m.temp)

# Which countries/runs have no onset?
m.noOnset <- m[is.na(m$ot), ]; m.noOnset$country <- factor(m.noOnset$country)
print(length(unique(m.noOnset$run)))
rev(sort(table(m.noOnset$country))) # HU and FR most likely, but even then rarely; similar for wider, but more SK
# obs: NL and SK in 13-14
# IT PL ES SK HU NL FR CZ DE BE AT 
# 30 28 26 25 25 24 24 24 21 21 16
# never LU

# Now remove those w/o onset for further analyses:
m <- m[!is.na(m$ot), ]

### Look at range of AR, PT, OT by COUNTRY ###
# Plot with geographic information:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy',
                   'Luxembourg', 'Netherlands', 'Poland', 'Slovakia', 'Spain')
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic')) &
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

m$country <- factor(m$country, levels = ar.med$country)
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ar, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Attack Rate (per 100,000)', fill = 'Long.') +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = ot.med$country)
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ot, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset Week', fill = 'Long.') +
  # scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = pt.med$country)
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak Week', fill = 'Long.') +
  # scale_y_continuous(breaks = seq(46, 68, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, p3, ncol = 1)
# pattern doesn't seem super strong, but still looks pretty e-w, at least for PT; OT looks a little more neutral

# Plot by time since first onset/peak, too:
ot.med <- aggregate(ot_order ~ country, data = m, FUN = median)
ot.med <- ot.med[order(ot.med$ot_order), ]

pt.med <- aggregate(pt_order ~ country, data = m, FUN = median)
pt.med <- pt.med[order(pt.med$pt_order), ]

m$country <- factor(m$country, levels = ot.med$country)
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ot_order, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Onset (Weeks from First Onset)', fill = 'Long.') +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
m$country <- factor(m$country, levels = pt.med$country)
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt_order, fill = long)) +
  theme_classic() + theme(axis.text = element_text(size = 10)) +
  labs(x = '', y = 'Peak (Weeks from First Peak)', fill = 'Long.') +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  scale_fill_gradientn(colors = viridis(100))
grid.arrange(p1, p2, ncol = 1)
# don't look super clear either way

# Longitudinal pattern by run:
par(mfrow = c(4, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
onset.corrs = peak.corrs = c()
for (run in unique(m$run)) {
  # print(run)
  onset.corrs <- c(onset.corrs, cor.test(m$long[m$run == run], m$ot[m$run == run], method = 'kendall')$estimate)
  peak.corrs <- c(peak.corrs, cor.test(m$long[m$run == run], m$pt[m$run == run], method = 'kendall')$estimate)
  # plot(m$long[m$run == run], m$ot[m$run == run], xlab = 'Longitude', ylab = 'Onset Timing', main = run, pch = 20)
  # plot(m$long[m$run == run], m$pt[m$run == run], xlab = 'Longitude', ylab = 'Peak Timing', main = run, pch = 20)
  # print(''); print('')
}
# don't look too extreme - and many are w-e
par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
hist(onset.corrs, breaks = 20)
hist(peak.corrs, breaks = 20)
# look pretty evenly distributed around zero; a few more positive for onset; 50% > 0 for OT, 47.7% for PT

### Assess synchrony ###
# # For each run, what is the range of peak timings?
# m$country <- factor(m$country, levels = countries)
# par(mfrow = c(3, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in 1:length(synth.runs.RATES)) {
#   m.temp <- m[m$run == i, ]
#   hist(m.temp$pt, breaks = 21, xlab = 'PT', main = i)
#   # print(max(m.temp$pt) - min(m.temp$pt))
#   # print(as.vector(quantile(m.temp$pt, prob = 0.975) - quantile(m.temp$pt, prob = 0.025)))
#   # print('')
# }

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
  
  if (!isSymmetric(cor.mat)) {
    print(isSymmetric(cor.mat))
  }
  # print(isSymmetric(cor.mat))
  cor.synch[[ix]] <- cor.mat
}

# Average cor.synch across all seasons:
cor.synch.AVG <- apply(simplify2array(cor.synch), 1:2, mean)
print(isSymmetric(cor.synch.AVG))

# Test average synchrony against distance using Mantel test:
library(ecodist); library(geosphere)

data("world.cities")
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic')) &
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
# trends negative, but not sig; countries further away actually trend towards being more in synch

# Is synchrony related to commuting flows?:
load('src/formatTravelData/formattedData/comm_mat_by_season_01-27.RData')
t.comm <- apply(simplify2array(comm.by.seas), 1:2, mean, na.rm = TRUE); rm(comm.by.seas)
t.comm <- t.comm[countries, countries]

t.comm.sym <- t.comm # have to be symmetric for Mantel test
t.comm.sym.lower <- which(lower.tri(t.comm.sym), arr.ind = TRUE)
avg.vals <- c()
for (i in 1:dim(t.comm.sym.lower)[1]) {
  index <- t.comm.sym.lower[i, ]
  val1 <- t.comm.sym[index[1], index[2]]
  val2 <- t.comm.sym[index[2], index[1]]
  if (is.na(val1) & !is.na(val2)) {
    avg.vals <- c(avg.vals, val2)
  } else if (!is.na(val1) & is.na(val2)) {
    avg.vals <- c(avg.vals, val1)
  } else if (!is.na(val1) & !is.na(val2)) {
    avg.vals <- c(avg.vals, mean(avg.vals))
  } else {
    avg.vals <- c(avg.vals, NA)
  }
  # avg.vals <- c(avg.vals, (t.comm.sym[index[1], index[2]] + t.comm.sym[index[2], index[1]]) / 2)
}
t.comm.sym[t.comm.sym.lower] <- avg.vals
t.comm.sym.upper <- cbind(t.comm.sym.lower[, 2], t.comm.sym.lower[, 1])
t.comm.sym[t.comm.sym.upper] <- avg.vals
print(isSymmetric(t.comm.sym))

t.comm.sym[t.comm.sym == 0 & !is.na(t.comm.sym)] <- 1.0 # doesn't really matter - these are removed for analysis
t.comm.sym <- 1 / t.comm.sym # convert to distance
t.comm.sym[is.na(t.comm.sym)] <- 1.0 # give no commuting large distance?

mantel(as.dist(synch.dist) ~ as.dist(t.comm.sym), nperm = 10000, mrank = TRUE)
# not sig, but trends slightly negative - in other words, more synchrony between places with less commuting
# instead of using available value, assume NAs are 0 and cut in half?: same results

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
# one-sided sig negative; so greater air travel connection associated with less synchrony
# perhaps because air travel becomes greater with greater geo distance, which is associated with more difference in AH profile?

# Plot mean correlations as matrix:
rownames(cor.synch.AVG) = colnames(cor.synch.AVG) = c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
cor.synch.AVG[upper.tri(cor.synch.AVG)] <- NA
cor.synch.plot <- melt(cor.synch.AVG)
names(cor.synch.plot) <- c('c1', 'c2', 'corr')
cor.synch.plot <- cor.synch.plot[!is.na(cor.synch.plot$corr), ]
cor.synch.plot$corr[cor.synch.plot$corr == 1] <- NA

# onset: ranges from 0.6411 to 0.8637 (mean 0.7458) - so too synchronous at lower end, but not synchronous enough for higher
# lowest are 0.6411 (HU/IT)
p1 <- ggplot(cor.synch.plot, aes(x = c1, y = c2)) + geom_tile(aes(fill = corr), colour = 'white') +
  scale_fill_gradientn(colours = cividis(100), na.value = 'gray80', limits = c(0.4, 0.95)) + theme_classic() +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Average Synchrony', x = '', y = '', fill = 'Corr.')
print(p1)

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
cor.synch.plot$comm[cor.synch.plot$comm == -Inf] <- NA

par(mfrow = c(3, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(cor.synch.plot$air, cor.synch.plot$corr, pch = 20, xlab = 'log(Air Passengers)', ylab = 'Correlation')
plot(cor.synch.plot[!is.na(cor.synch.plot$comm), ]$comm, cor.synch.plot[!is.na(cor.synch.plot$comm), ]$corr, pch = 20, xlab = 'log(Commuters)', ylab = 'Correlation')
plot(cor.synch.plot$dist, cor.synch.plot$corr, pch = 20, xlab = 'Distance (100 km)', ylab = 'Correlation')

cor.test(cor.synch.plot$air, cor.synch.plot$corr, method = 'kendall') # sig neg - lower synchrony when air travel higher
cor.test(cor.synch.plot$comm, cor.synch.plot$corr, method = 'kendall') # not sig, but trend towards more synchrony when higher commuting
cor.test(cor.synch.plot$dist, cor.synch.plot$corr, method = 'kendall') # sig pos - higher synchrony when geo distance lower

m1 <- lm(corr ~ air + dist, data = cor.synch.plot)
m2 <- lm(corr ~ comm + dist, data = cor.synch.plot)

####################################################################################################
####################################################################################################
####################################################################################################

### Plot parameter ranges:
load('src/syntheticTests/syntheticData/params_1000_070220.RData')
load('src/syntheticTests/syntheticData/S0_1000_070220.Rdata')
load('src/syntheticTests/syntheticData/I0_1000_070220.Rdata')

parms.list <- parms.list[[3]] # "realistic" only
s0.list <- s0.list[[3]]
i0.list <- i0.list[[3]]

select.parms <- as.data.frame(t(parms.list))
names(select.parms) <- c('L', 'D', 'R0mx', 'R0diff', 'airScale')
select.parms$R0mn <- select.parms$R0mx - select.parms$R0diff

par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
hist(select.parms$D, xlab = 'D', main = '', breaks = 15)
hist(select.parms$L, xlab = 'L', main = '', breaks = 15)
hist(select.parms$R0mx, xlab = 'R0mx', main = '', breaks = 15)
hist(select.parms$R0mn, xlab = 'R0mn', main = '', breaks = 15)
hist(select.parms$R0mx - select.parms$R0mn, xlab = 'R0diff', main = '', breaks = 15)
hist(select.parms$airScale, xlab = 'airScale', main = '', breaks = 15)

init.S <- as.data.frame(t(s0.list))
init.I <- as.data.frame(t(i0.list))
names(init.S) = names(init.I) = countries

init.S.df <- melt(init.S); init.I.df <- melt(init.I)
kruskal.test(value ~ variable, data = init.S.df) # not sig
kruskal.test(value ~ variable, data = init.I.df) # not sig

par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(init.S.df$value ~ init.S.df$variable, col = 'lightblue2')
boxplot(init.I.df$value ~ init.I.df$variable, col = 'lightblue2')

####################################################################################################
####################################################################################################
####################################################################################################

### Look at distribution of onsets/peaks by run ###
par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(ot ~ run, data = m, col = 'lightblue2', xlab = 'Run', ylab = 'Onset Week')
boxplot(pt ~ run, data = m, col = 'lightblue2', xlab = 'Run', ylab = 'Peak Week')

# ### Map out spatial patterns ###
# # List of countries:
# countries.europe <- c('Austria', 'Belarus', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark',
#                       'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland',
#                       'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Norway', 'Poland',
#                       'Portugal', 'Moldova', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
#                       'Switzerland', 'Ukraine', 'UK')
# 
# # Create map:
# world <- map_data('world')
# eur <- world[world$region %in% countries.europe,]
# rm(world)
# 
# # Code to remove axes:
# ditch_the_axes <- theme(
#   axis.text = element_blank(),
#   axis.line = element_blank(),
#   axis.ticks = element_blank(),
#   panel.border = element_blank(),
#   panel.grid = element_blank(),
#   axis.title = element_blank()
# )
# 
# # Change country levels in m:
# m$country <- factor(as.character(m$country))
# levels(m$country) <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy',
#                        'Luxembourg', 'Netherlands', 'Poland', 'Slovakia', 'Spain')
# 
# # Store onset week for each run:
# library(dplyr)
# for (run in levels(m$run)) {
#   m.temp <- m[m$run == run, c('country', 'ot')]
#   names(m.temp)[1] <- 'region'
#   m.temp$region <- as.character(m.temp$region)
#   eur <- full_join(eur, m.temp, by = 'region')
#   names(eur)[length(names(eur))] <- run
# }
# 
# # Loop through weeks and plot when country has onset:
# for (run in levels(m$run)) {
#   on.min <- min(eur[, names(eur) == run], na.rm = TRUE)
#   on.max <- max(eur[, names(eur) == run], na.rm = TRUE)
# 
#   p = vector('list', length(on.min:on.max))
#   for (wk in on.min:on.max) {
#     eur.curr <- eur[eur[, names(eur) == run] == wk & !is.na(eur[, names(eur) == run]), ]
#     eur.past <- eur[eur[, names(eur) == run] < wk & !is.na(eur[, names(eur) == run]), ]
# 
#     p[[wk - on.min + 1]] <- ggplot() + geom_polygon(data = eur, aes(x = long, y = lat, group = group),
#                                                     fill = 'gray95', colour = 'black', size = 1.0) +
#       geom_polygon(data = eur.past, aes(x = long, y = lat, group = group),
#                    fill = '#ffeda0', colour = 'black', size = 1.0) +
#       geom_polygon(data = eur.curr, aes(x = long, y = lat, group = group),
#                    fill = '#f03b20', colour = 'black', size = 1.0) +
#       labs(title = paste0('Week ', wk)) + theme_classic() + ditch_the_axes
#   }
#   do.call('grid.arrange', c(p, nrow = 3))
# }

dev.off()
rm(list = ls())

