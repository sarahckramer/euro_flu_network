
library(ggplot2)

pdf('syntheticTests/outputs/new_scalings.pdf', width = 15, height = 8)

### OBSERVED ###
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

# Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
iliiso <- iliiso[, c(1:9, 11:22)]
iliiso.raw <- iliiso

# "Original" scalings:
scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
for (i in 2:21) {
  # print(names(iliiso)[i])
  # print(scalings[scalings$country == names(iliiso)[i], ])
  if (names(iliiso)[i] == 'France') {
    iliiso[1:286, i] <- iliiso[1:286, i] * 1.3
    iliiso[287:495, i] <- iliiso[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  } else {
    iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}
iliiso.scale <- iliiso
rm(iliiso)

### Calculate observed attack rates by season:
source('code/functions/Util.R'); wk_start <- 40
seasons <- list(79:121, 131:173, 183:225, 235:277, 287:329, 339:382, 392:434, 444:486) # weeks 40 through 30 (same length as synthetic outbreaks)

ars.raw = ars.scale = vector('list', 20)
for (i in 1:20) {
  ars.temp.raw = ars.temp.scale = c()
  for (j in 1:length(seasons)) {
    # first check for onset:
    # if (!is.na(findOnset(iliiso.scale[seasons[[j]], i + 1], baseline = 500)$onset)) {
    #   ars.temp.raw <- c(ars.temp.raw, sum(iliiso.raw[seasons[[j]], i + 1], na.rm = TRUE))
    #   ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
    # } else {
    #   ars.temp.raw <- c(ars.temp.raw, NA)
    #   ars.temp.scale <- c(ars.temp.scale, NA)
    # }
    # only outbreaks w/o onset are NL 11-12/13-14 and SK 13-14 - just include these for calculating scalings
    ars.temp.raw <- c(ars.temp.raw, sum(iliiso.raw[seasons[[j]], i + 1], na.rm = TRUE))
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.raw[ars.temp.raw == 0] <- NA
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.raw[[i]] <- ars.temp.raw
  ars.scale[[i]] <- ars.temp.scale
}; rm(ars.temp.raw, ars.temp.scale)

# Convert list to data frame:
countries.new <- c()
for (i in 1:20) {
  countries.new <- c(countries.new, rep(countries[i], length(seasons)))
}
df.obs <- as.data.frame(cbind(countries.new, rep(c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18'), 20),
                              unlist(ars.raw), unlist(ars.scale)))
names(df.obs) <- c('country', 'season', 'ar.raw', 'ar.scale')
df.obs <- df.obs[!is.na(df.obs$ar.raw) & !is.na(df.obs$ar.scale), ]
for (i in 3:4) {
  df.obs[, i] <- as.numeric(as.character(df.obs[, i]))
}
rm(ars.raw, ars.scale, iliiso.raw, iliiso.scale, i, j, countries.new)

### SYNTHETIC ###
# Read in synthetic "data" by country:

# Want to check: only realistic; all meeting onset criteria; all where individual country has onset; all

load('syntheticTests/syntheticData/synth_07-14_RATES.RData'); r1 <- synth.runs.RATES
# "realistic" only

load('syntheticTests/syntheticData/synth_07-14_RATES_onsetOnly.RData'); r2 <- synth.runs.RATES
# all with at least 18 countries w/ onsets

load('syntheticTests/syntheticData/synth_07-14_RATES_all.RData'); r3 <- synth.runs.RATES
# all

rm(synth.runs.RATES)

# Get range of attack rates:
ars1 <- lapply(1:length(r1), function(ix) {
  rowSums(r1[[ix]])
})

ars2 <- lapply(1:length(r2), function(ix) {
  rowSums(r2[[ix]])
})

ars3 <- lapply(1:length(r3), function(ix) {
  rowSums(r3[[ix]])
})

# Also find onsets:
ot.synth1 <- c()
for (i in 1:length(r1)) {
  ot.temp <- lapply(1:length(countries), function(ix) {
    findOnset(r1[[i]][ix, ], baseline = 500)$onset
  })
  ot.synth1 <- c(ot.synth1, unlist(ot.temp))
}

ot.synth2 <- c()
for (i in 1:length(r2)) {
  ot.temp <- lapply(1:length(countries), function(ix) {
    findOnset(r2[[i]][ix, ], baseline = 500)$onset
  })
  ot.synth2 <- c(ot.synth2, unlist(ot.temp))
}

ot.synth3 <- c()
for (i in 1:length(r3)) {
  ot.temp <- lapply(1:length(countries), function(ix) {
    findOnset(r3[[i]][ix, ], baseline = 500)$onset
  })
  ot.synth3 <- c(ot.synth3, unlist(ot.temp))
}

# Convert lists to data frames:
countries.synth <- rep(countries, length(r1))
df.synth1 <- as.data.frame(cbind(countries.synth, unlist(ars1), ot.synth1))
df.synth1 <- df.synth1[!is.na(df.synth1$ot.synth1), ]
names(df.synth1) <- c('country', 'ar.rate', 'ot')
df.synth1$ar.rate <- as.numeric(as.character(df.synth1$ar.rate))
rm(ars1, r1, ot.synth1, countries.synth)

countries.synth <- rep(countries, length(r2))
df.synth2 <- as.data.frame(cbind(countries.synth, unlist(ars2), ot.synth2))
df.synth2 <- df.synth2[!is.na(df.synth2$ot.synth2), ]
names(df.synth2) <- c('country', 'ar.rate', 'ot')
df.synth2$ar.rate <- as.numeric(as.character(df.synth2$ar.rate))
rm(ars2, r2, ot.synth2, countries.synth)

countries.synth <- rep(countries, length(r3))
df.synth3 <- as.data.frame(cbind(countries.synth, unlist(ars3), ot.synth3))
df.synth4 <- df.synth3
df.synth3 <- df.synth3[!is.na(df.synth3$ot.synth3), ]
names(df.synth3) = names(df.synth4) = c('country', 'ar.rate', 'ot')
df.synth3$ar.rate <- as.numeric(as.character(df.synth3$ar.rate))
df.synth4$ar.rate <- as.numeric(as.character(df.synth4$ar.rate))
rm(ars3, r3, ot.synth3, countries.synth)

df.synth1$type <- 'synth1'
df.synth2$type <- 'synth2'
df.synth3$type <- 'synth3'
df.synth4$type <- 'synth4'
df.synth <- rbind(df.synth1, df.synth2, df.synth3, df.synth4)

### Plots:
# Want to know: How do our SCALED values compare to the RATES coming from the model runs? What scalings would transform our RAW data into appropriate RATES?

# SCALED data vs. synthetic RATES:
df.obs$type <- 'obs'
df.obs.red <- df.obs[, c(1, 4:5)]
df.synth.red <- df.synth[, c(1:2, 4)]
names(df.obs.red)[2] <- 'ar'; names(df.synth.red)[2] <- 'ar'
df.new <- rbind(df.obs.red, df.synth.red)
df.new$type <- factor(df.new$type)
levels(df.new$type) <- c('Obs. (Scaled)', 'Synth. (Realistic)', 'Synth. (18 Onsets)', 'Synth. (Only w/ Onset)', 'Synth. (All)')

p1 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = type), outlier.shape = 4) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '')
print(p1)
p2 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = log(ar), fill = type), outlier.shape = 4) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'log(Attack Rate)', fill = '')
# print(p2)
# all 1000 w/ onsets looks pretty similar to "realistic" - think it's okay (and makes more sense) to use that

df.new <- df.new[df.new$type %in% c('Obs. (Scaled)', 'Synth. (Only w/ Onset)'), ]
df.new$type <- factor(df.new$type)
levels(df.new$type)[2] <- 'Synth. (Rate)'
p3 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = type), outlier.shape = 4) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '')
print(p3)
# NOTE: In the observed data, we've already removed the countries/seasons with no "onsets" according to the old scalings - is this okay, b/c they're "outliers,"
#       or should these be included in calculating the new scalings?
#       Included now!
# Some countries seem to already have decent scalings for this model, some are quite a bit off from what they might need to perform optimally (DK, RO, several others)
    # Any evidence that these are the countries that end up performing more poorly in fitting/forecasts?

# Now plot RAW data vs. synthetic rates, to help determine new scalings:
df.obs.red <- df.obs[, c(1, 3, 5)]
df.synth.red <- df.synth3[, c(1:2, 4)]
names(df.obs.red)[2] <- 'ar'; names(df.synth.red)[2] <- 'ar'
df.new <- rbind(df.obs.red, df.synth.red)
df.new$type <- factor(df.new$type)
levels(df.new$type) <- c('Obs. (Raw)', 'Synth. (Rate)')

p1 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = type), outlier.shape = 4) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '')
# print(p1)
p2 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = log(ar), fill = type), outlier.shape = 4) + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'log(Attack Rate)', fill = '')
print(p2)
# Most need to be multiplied up, some are pretty close already (CZ, HR, HU, SK); later seasons in France as well as Germany need to come down

### Calculate new scalings:
new.scalings <- vector('list', length(countries))
for (country in countries) {
  print(country); print(which(countries == country))
  
  med.obs <- median(df.obs$ar.raw[df.obs$country == country])
  med.synth <- median(df.synth3$ar.rate[df.synth3$country == country])
  
  # want: med.obs * x = med.synth
  # therefore: x = med.synth / med.obs
  new.scalings[[which(countries == country)]] <- med.synth / med.obs
}
# Have to do FR separately:
med.obs1 <- median(df.obs$ar.raw[df.obs$country == 'FR' & df.obs$season %in% c('2012-13', '2013-14')])
med.obs2 <- median(df.obs$ar.raw[df.obs$country == 'FR' & !(df.obs$season %in% c('2012-13', '2013-14'))])
med.synth <- median(df.synth3$ar.rate[df.synth3$country == country])
new.scalings[[6]] <- c(med.synth / med.obs1, med.synth / med.obs2)

# Compare to old scalings:
for (i in 1:length(countries)) {
  print(countries[i])
  print(scalings$gamma[scalings$country != 'Iceland'][i])
  print(new.scalings[[i]])
  print(''); print('')
}
# most are fairly similar; way up: DK, RO

### Rescale raw data and plot:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
iliiso <- iliiso[, c(1:9, 11:22)]

for (i in 2:21) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:286, i] <- iliiso[1:286, i] * new.scalings[[6]][1]
    iliiso[287:495, i] <- iliiso[287:495, i] * new.scalings[[6]][2]
  } else {
    iliiso[, i] <- iliiso[, i] * new.scalings[[i - 1]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}
iliiso.scale <- iliiso
rm(iliiso)
# matplot(iliiso.scale[, 2:21], pch = 20, col = viridis(20), lty = 1, type = 'b', cex = 0.2)

ars.scale.new <- vector('list', 20)
for (i in 1:20) {
  ars.temp.scale <- c()
  for (j in 1:length(seasons)) {
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.scale.new[[i]] <- ars.temp.scale
}; rm(ars.temp.scale)
df.obs$ar.scale.new <- unlist(ars.scale.new)[!is.na(unlist(ars.scale.new))]

# What about when scaled data have AR > 100,000?:
print(df.obs[which(df.obs$ar.scale.new > 100000), ]) # DK 10-11, HU 14-15/16-17/17-18, RO 10-11

# for now, we can rescale so that the largest outbreak hits 90%, and see what that does to the other outbreaks
    # option to also simply reduce these outbreaks alone to 90% AR - but then we don't have a consistent scaling parameter from year to year
    # see what Sen thinks, too
# for these 3 countries, these are unusually large outbreaks
max.obs.dk <- max(df.obs$ar.raw[df.obs$country == 'DK'])
max.obs.hu <- max(df.obs$ar.raw[df.obs$country == 'HU'])
max.obs.ro <- max(df.obs$ar.raw[df.obs$country == 'RO'])

scale.dk <- 90000 / max.obs.dk
scale.hu <- 90000 / max.obs.hu
scale.ro <- 90000 / max.obs.ro

new.scalings[[which(countries == 'DK')]] <- scale.dk
new.scalings[[which(countries == 'HU')]] <- scale.hu
new.scalings[[which(countries == 'RO')]] <- scale.ro

# Now recalculate:
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso[, 2:22][iliiso[, 2:22] < 1] <- NA
iliiso <- iliiso[, c(1:9, 11:22)]

for (i in 2:21) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:286, i] <- iliiso[1:286, i] * new.scalings[[6]][1]
    iliiso[287:495, i] <- iliiso[287:495, i] * new.scalings[[6]][2]
  } else {
    iliiso[, i] <- iliiso[, i] * new.scalings[[i - 1]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
}
iliiso.scale <- iliiso
rm(iliiso)
# matplot(iliiso.scale[, 2:21], pch = 20, col = viridis(20), lty = 1, type = 'b', cex = 0.2)

ars.scale.new <- vector('list', 20)
for (i in 1:20) {
  ars.temp.scale <- c()
  for (j in 1:length(seasons)) {
    ars.temp.scale <- c(ars.temp.scale, sum(iliiso.scale[seasons[[j]], i + 1], na.rm = TRUE))
  }
  ars.temp.scale[ars.temp.scale == 0] <- NA
  ars.scale.new[[i]] <- ars.temp.scale
}; rm(ars.temp.scale)
df.obs$ar.scale.new2 <- unlist(ars.scale.new)[!is.na(unlist(ars.scale.new))]
# even now, these seem a bit high - original scalings rarely had a season go past 50%; here several over 60% - apply a multiplier? look for smaller synthetic runs?

### Redo boxplots for new scalings:
df.obs.red <- df.obs[, c(1, 7)]
df.obs.red$type <- 'scaled'
df.synth.red <- df.synth3[, c(1:2, 4)]
names(df.obs.red)[2] <- 'ar'; names(df.synth.red)[2] <- 'ar'
df.new <- rbind(df.obs.red, df.synth.red)
df.new$type <- factor(df.new$type)
levels(df.new$type) <- c('Obs. (Scaled - NEW)', 'Synth. (Rate)')

p8 <- ggplot(data = df.new) + geom_boxplot(aes(x = country, y = ar, fill = type), outlier.shape = 4) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = '', y = 'Attack Rate', fill = '') + scale_y_continuous(limits = c(0, 100000)) + # synth have to be in this range
  geom_hline(yintercept = 15000, col = 'black', lty = 2, lwd = 1.25) +
  geom_hline(yintercept = 50000, col = 'black', lty = 2, lwd = 1.25)
print(p8)
# DK and HU look acceptable, but RO is pretty low

# Save new scalings:
names(new.scalings) <- countries
print(unlist(new.scalings))
# save(new.scalings, file = 'data/scalings_temp_08-19-19.RData')

dev.off()


