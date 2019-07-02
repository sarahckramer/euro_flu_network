
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
df.obs <- as.data.frame(cbind(countries.new, rep(c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18'), 21),
                              unlist(ars.raw), unlist(ars.scale), unlist(ars.count)))
names(df.obs) <- c('country', 'season', 'ar.raw', 'ar.scale', 'ar.count')
df.obs <- df.obs[!is.na(df.obs$ar.raw) & !is.na(df.obs$ar.scale) & !is.na(df.obs$ar.count), ]
for (i in 3:5) {
  df.obs[, i] <- as.numeric(as.character(df.obs[, i]))
}
rm(ars.count, ars.raw, ars.scale, iliiso.count, iliiso.raw, iliiso.scale, i, j, countries.new)

### SYNTHETIC ###
# Read in synthetic "data" by country:
load('syntheticTests/syntheticData/synth_06-26_COUNTS.RData')
load('syntheticTests/syntheticData/synth_06-26_RATES.RData')
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
rm(ars.synth.count, ars.synth.rates, synth.runs.COUNTS, synth.runs.RATES, countries.synth)

# ### PLOT ###
# p1 <- ggplot(data = df.obs, aes(x = country, y = ar.scale)) + geom_boxplot(fill = 'gray90', outlier.shape = 4) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'Attack Rate') + scale_y_continuous(limits = c(0, 100000)) +
#   geom_point(data = df.synth, aes(x = country, y = ar.rate), col = 'coral', size = 0.5)
# print(p1)
# 
# p2 <- ggplot(data = df.obs, aes(x = country, y = ar.count)) + geom_boxplot(fill = 'gray90', outlier.shape = 4) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'Attack Rate') + #scale_y_continuous(limits = c(0, 100000)) +
#   geom_point(data = df.synth, aes(x = country, y = ar.count), col = 'coral', size = 0.5)
# print(p2)
# 
# p3 <- ggplot(data = df.obs, aes(x = country, y = log(ar.raw))) + #geom_boxplot(fill = 'gray90', outlier.shape = 4) +
#   geom_point(col = 'black', size = 1.0) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'log(Attack Rate)') + #scale_y_continuous(limits = c(0, 100000)) +
#   geom_point(data = df.synth, aes(x = country, y = log(ar.rate)), col = 'coral', size = 1.0, alpha = 0.5)
# print(p3)

# Want a scaling that we can multiply raw observed data by to match up with the rates from the synthetic data
    # So compare RAW OBS with SYNTH RATES (p3), and get better scalings from these

df.obs.red <- df.obs[, c(1, 3)]
df.synth.red <- df.synth[, c(1, 3)]

df.obs.red$obs <- 'obs'; df.synth.red$obs <- 'synth'
names(df.obs.red)[2] <- 'ar'; names(df.synth.red)[2] <- 'ar'

df.new <- rbind(df.obs.red, df.synth.red)
df.new$obs <- factor(df.new$obs)

p4 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = log(ar), fill = obs), outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'log(Attack Rate)', fill = '')
print(p4)
# p5 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = obs), outlier.shape = 4) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'Attack Rate', fill = '')
# print(p5)
# p6 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = obs), outlier.shape = 4) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'Attack Rate', fill = '') + scale_y_continuous(limits = c(0, 100000)) # synth have to be in this range
# print(p6)

# Calculate approximate new scalings:
new.scaling.vals <- vector('list', length(countries))
for (country in levels(df.new$country)) {
  # print(country)
  # print(median(df.new$ar[df.new$country == country & df.new$obs == 'obs']))
  # print(median(df.new$ar[df.new$country == country & df.new$obs == 'synth']))
  # print('')
  
  med.obs <- median(df.new$ar[df.new$country == country & df.new$obs == 'obs'])
  med.synth <- median(df.new$ar[df.new$country == country & df.new$obs == 'synth'])
  
  # want a multiplier that makes med.obs * x = med.synth
  scale.temp <- med.synth / med.obs
  new.scaling.vals[[which(countries == country)]] <- scale.temp
}

# Do France separately:
med.obs1 <- median(df.obs$ar.raw[df.obs$country == 'FR' & df.obs$season %in% c('2012-13', '2013-14')])
med.obs2 <- median(df.obs$ar.raw[df.obs$country == 'FR' & !(df.obs$season %in% c('2012-13', '2013-14'))])

med.synth <- median(df.new$ar[df.new$country == 'FR' & df.new$obs == 'synth'])

scale.FR1 <- med.synth / med.obs1; scale.FR2 <- med.synth / med.obs2
new.scaling.vals[[6]] <- c(scale.FR1, scale.FR2)

# Compare to old scalings:
for (i in 1:length(countries)) {
  print(countries[i])
  print(scalings$gamma[i])
  print(new.scaling.vals[[i]])
  print(''); print('')
}
# many are fairly similar; way up: DK, PL, RO, SI, SE

# Rescale data and plot:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
for (i in 2:22) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:283, i] <- iliiso[1:283, i] * new.scaling.vals[[i - 1]][1]
    iliiso[284:495, i] <- iliiso[284:495, i] * new.scaling.vals[[i - 1]][2]
  } else {
    iliiso[, i] <- iliiso[, i] * new.scaling.vals[[i - 1]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}
iliiso.scale <- iliiso

# matplot(iliiso.scale[, 2:22], pch = 20, col = viridis(21), lty = 1, type = 'b', cex = 0.2)

ars.scale.new = vector('list', 21)
for (i in 1:21) {
  ars.temp.scale <- c()
  for (j in 1:length(seasons)) {
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.scale.new[[i]] <- ars.temp.scale
}; rm(ars.temp.scale)
df.obs$ar.scale.new <- unlist(ars.scale.new)[!is.na(unlist(ars.scale.new))]

# plot(df.obs$ar.scale, df.obs$ar.scale.new, pch = 20, xlab = 'AR (Original Scalings)', ylab = 'AR (New Scalings)')#, col = df.obs$country)

# What about when scaled data have AR > 100,000?:
print(df.obs[which(df.obs$ar.scale.new > 100000), ]) # DK and RO, both 2010-11
df.obs[df.obs$country == 'DK', ]
df.obs[df.obs$country == 'RO', ]
# consider these to be outliers and calculate scalings without them? -- wait, this will just yield even higher scalings...

med.obs <- median(df.obs$ar.raw[df.obs$country == 'DK' & df.obs$season == '2010-11'])
med.synth <- median(df.new$ar[df.new$country == 'DK' & df.new$obs == 'synth'])
scale.DK <- med.synth / med.obs

# scale them down by half and just say they're outliers? remove the seasons completely? scale so that these seasons are ~90% infected and let the others fall however?
      # number of tests hasn't really changed; denominator was higher for first few years in Romania, but this is the only abnormally large season

scale.DK <- 90000 / df.obs$ar.raw[df.obs$country == 'DK' & df.obs$season == '2010-11']
scale.RO <- 90000 / df.obs$ar.raw[df.obs$country == 'RO' & df.obs$season == '2010-11']

new.scaling.vals[[5]] <- scale.DK; new.scaling.vals[[16]] <- scale.RO

# And recalculate:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
for (i in 2:22) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:283, i] <- iliiso[1:283, i] * new.scaling.vals[[i - 1]][1]
    iliiso[284:495, i] <- iliiso[284:495, i] * new.scaling.vals[[i - 1]][2]
  } else {
    iliiso[, i] <- iliiso[, i] * new.scaling.vals[[i - 1]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}
iliiso.scale <- iliiso

# matplot(iliiso.scale[, 2:22], pch = 20, col = viridis(21), lty = 1, type = 'b', cex = 0.2)

ars.scale.new = vector('list', 21)
for (i in 1:21) {
  ars.temp.scale <- c()
  for (j in 1:length(seasons)) {
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.scale.new[[i]] <- ars.temp.scale
}; rm(ars.temp.scale)
df.obs$ar.scale.new <- unlist(ars.scale.new)[!is.na(unlist(ars.scale.new))]

plot(df.obs$ar.scale, df.obs$ar.scale.new, pch = 20, xlab = 'AR (Original Scalings)', ylab = 'AR (New Scalings)')#, col = df.obs$country)
# even now, these seem a bit high - original scalings rarely had a season go past 50%; here several over 60% - apply a multiplier? look for smaller synthetic runs?

# Redo boxplots for new scalings:
df.obs.red <- df.obs[, c(1, 6)]
df.obs.red$obs <- 'scaled'
names(df.obs.red)[2] <- 'ar'
df.new$obs <- as.character(df.new$obs); df.new <- rbind(df.new, df.obs.red); df.new$obs <- factor(df.new$obs)

# p7 <- ggplot(data = df.new[df.new$obs != 'obs', ]) + geom_boxplot(aes(x = country, y = log(ar), fill = obs), outlier.shape = 4) +
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
#   labs(x = '', y = 'log(Attack Rate)', fill = '')
# print(p7)
p8 <- ggplot(data = df.new[df.new$obs != 'obs', ]) + geom_boxplot(aes(x = country, y = ar, fill = obs), outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '') + scale_y_continuous(limits = c(0, 100000)) + # synth have to be in this range
  geom_hline(yintercept = 15000, col = 'black', lty = 2, lwd = 1.25) +
  geom_hline(yintercept = 50000, col = 'black', lty = 2, lwd = 1.25)
print(p8)

# Comparative plot of original scalings:
df.new$ar[df.new$obs == 'scaled'] <- df.obs$ar.scale
p9 <- ggplot(data = df.new[df.new$obs != 'obs', ]) + geom_boxplot(aes(x = country, y = ar, fill = obs), outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '') + scale_y_continuous(limits = c(0, 100000)) + # synth have to be in this range
  geom_hline(yintercept = 15000, col = 'black', lty = 2, lwd = 1.25) +
  geom_hline(yintercept = 50000, col = 'black', lty = 2, lwd = 1.25)
print(p9)

# Save new scalings:
names(new.scaling.vals) <- countries
save(new.scaling.vals, file = 'data/scalings_temp_06-27-19.RData')



