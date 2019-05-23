
library(ggplot2)

### OBSERVED ###
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

# Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
iliiso.raw <- iliiso

# "Original" scalings:
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
iliiso.scale <- iliiso

# Convert data to "counts" (based on population):
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]
for (i in 2:22) {
  iliiso[, i] <- (iliiso[, i] * (pop.size$pop[i - 1] / 100000))
}
iliiso.count <- iliiso
rm(iliiso, i)

### Calculate observed attack rates by season:
seasons <- list(79:121, 131:173, 183:225, 235:277, 287:329, 339:382, 392:434, 444:486)

ars.raw = ars.scale = ars.count = vector('list', 21)
for (i in 1:21) {
  ars.temp.raw = ars.temp.scale = ars.temp.count = c()
  for (j in 1:length(seasons)) {
    ars.temp.raw <- c(ars.temp.raw, sum(iliiso.raw[seasons[[j]], i + 1], na.rm = TRUE))
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
    ars.temp.count <- c(ars.temp.count, sum(iliiso.count[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.raw[ars.temp.raw == 0] <- NA
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.temp.count[ars.temp.count == 0] <- NA
  ars.raw[[i]] <- ars.temp.raw
  ars.scale[[i]] <- ars.temp.scale
  ars.count[[i]] <- ars.temp.count
}; rm(ars.temp.raw, ars.temp.scale, ars.temp.count)

# Convert list to data frame:
countries.new <- c()
for (i in 1:21) {
  countries.new <- c(countries.new, rep(countries[i], length(seasons)))
}
df.obs <- as.data.frame(cbind(countries, rep(c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18'), 21),
                              unlist(ars.raw), unlist(ars.scale), unlist(ars.count)))
names(df.obs) <- c('country', 'season', 'ar.raw', 'ar.scale', 'ar.count')
df.obs <- df.obs[!is.na(df.obs$ar.raw) & !is.na(df.obs$ar.scale) & !is.na(df.obs$ar.count), ]
for (i in 3:5) {
  df.obs[, i] <- as.numeric(as.character(df.obs[, i]))
}

### SYNTHETIC ###
# Read in synthetic "data" by country:
load('syntheticTests/syntheticData/synth_05-16_COUNTS2.RData')
load('syntheticTests/syntheticData/synth_05-16_RATES2.RData')
# note that these are only the "realistic" runs, not all synthetic runs

# Get range of attack rates:
ars.synth.count <- lapply(1:length(synth.runs.COUNTS), function(ix) {
  rowSums(synth.runs.COUNTS[[ix]])
})
ars.synth.rates <- lapply(1:length(synth.runs.COUNTS), function(ix) {
  rowSums(synth.runs.RATES[[ix]])
})

# Convert list to data frame:
countries.synth <- rep(countries, length(ars.synth.count))
df.synth <- as.data.frame(cbind(countries.synth, unlist(ars.synth.count), unlist(ars.synth.rates)))
names(df.synth) <- c('country', 'ar.count', 'ar.rate')
df.synth$ar.count <- as.numeric(as.character(df.synth$ar.count))
df.synth$ar.rate <- as.numeric(as.character(df.synth$ar.rate))

### PLOT ###
p1 <- ggplot(data = df.obs, aes(x = country, y = ar.scale)) + geom_boxplot(fill = 'gray90', outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate') + scale_y_continuous(limits = c(0, 100000)) +
  geom_point(data = df.synth, aes(x = country, y = ar.rate), col = 'coral', size = 0.5)
print(p1)

p2 <- ggplot(data = df.obs, aes(x = country, y = ar.count)) + geom_boxplot(fill = 'gray90', outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate') + #scale_y_continuous(limits = c(0, 100000)) +
  geom_point(data = df.synth, aes(x = country, y = ar.count), col = 'coral', size = 0.5)
print(p2)

p3 <- ggplot(data = df.obs, aes(x = country, y = ar.raw)) + geom_boxplot(fill = 'gray90', outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate') + #scale_y_continuous(limits = c(0, 100000)) +
  geom_point(data = df.synth, aes(x = country, y = ar.count), col = 'coral', size = 0.5)
print(p3)



