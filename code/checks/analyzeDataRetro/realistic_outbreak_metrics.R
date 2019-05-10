
### Determine what elements constitute a "realistic" free simulation, based on past data and past inferred parameters/initial conditions

# Read in and format metrics files (should have calculated PT, AR, OT)
m <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_new_FIN.csv')
m.1718 <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/outputMetrics_RT1718_onset.csv')
m.1819 <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/outputMet_RT1819_onsets.csv')

m <- unique(m[, c('country', 'season', 'scaling', 'obs_pkwk', 'obs_peak_int', 'onsetObs5', 'totAttackObs')])
m$obs_peak_int <- m$obs_peak_int * m$scaling; m$totAttackObs <- m$totAttackObs * m$scaling
m <- m[, -3]

m.1718 <- unique(m.1718[, c('country', 'scaling', 'obs_pkwk', 'obs_peak_int', 'onsetObs')])
m.1819 <- unique(m.1819[, c('country', 'scaling', 'obs_pkwk', 'obs_peak_int', 'onsetObs')])
m.1718$obs_peak_int <- m.1718$obs_peak_int * m.1718$scaling; m.1819$obs_peak_int <- m.1819$obs_peak_int * m.1819$scaling
m.1718$season <- '2017-18'; m.1819$season <- '2018-19'
m.1718 <- m.1718[, c(1, 6, 3:5)]; m.1819 <- m.1819[, c(1, 6, 3:5)]
m2 <- rbind(m.1718, m.1819); rm(m.1718, m.1819)

# Reduce all to countries being used in network model:
countries <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
               'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
               'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
               'United Kingdom')
m <- m[m$country %in% countries, ]; m$country <- factor(m$country)
m2 <- m2[m2$country %in% countries, ]; m2$country <- factor(m2$country)

# Remove if PT = OT:
m <- m[!(m$onsetObs5 == m$obs_pkwk) | is.na(m$onsetObs5), ]
m2 <- m2[!(m2$onsetObs == m2$obs_pkwk) | is.na(m2$onsetObs), ]

# Only have AR calculated for older retro forecasts, so look at those first:
ar.dist <- vector('list', length(levels(m$season)))
for (i in 1:length(levels(m$season))) {
  m.temp <- m[m$season == levels(m$season)[i], ]
  ar.dist[[i]] <- m.temp$totAttackObs
}

par(mfrow = c(4, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(ar.dist)) {
  hist(ar.dist[[i]], breaks = 5, xlab = 'AR (per 100,000)', main = levels(m$season)[i])
  # print(quantile(ar.dist[[i]], probs = c(0, 0.025, 0.05, 0.95, 0.975, 1.0)))
  print(length(ar.dist[[i]][ar.dist[[i]] > 15000 & ar.dist[[i]] < 50000]) / length(ar.dist[[i]]))
} # usually ~75-85% in this range, but for 2013-14 only 30%

p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = totAttackObs), fill = 'lightblue2') +
  theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  labs(x = '', y = 'Attack Rate (per 100,000)')
print(p1)

# Now combine m and m2 to look at PT, OT, PI(?):
m <- m[, -6]
m$season <- as.character(m$season)
names(m)[5] <- 'onsetObs'
m <- rbind(m, m2)
m$season <- factor(m$season)
rm(m2)

# First calculate dists. of PT, OT, PI:
pt.dists = ot.dists = pi.dists = vector('list', length(levels(m$season)))
for (i in 1:length(pt.dists)) {
  m.temp <- m[m$season == levels(m$season)[i], ]
  pt.dists[[i]] <- m.temp$obs_pkwk
  ot.dists[[i]] <- m.temp$onsetObs
  pi.dists[[i]] <- m.temp$obs_peak_int
}

# Check to see if PIs in 2 recent seasons very different:
par(mfrow = c(3, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(pi.dists)) {
  hist(pi.dists[[i]], breaks = 5, xlab = 'PI (per 100,000)', main = levels(m$season)[i])
}
# pretty consistent - don't think I really need to look at AR for these recent 2 seasons for now

# Look at when PT occurs:
for (i in 1:length(pt.dists)) {
  hist(pt.dists[[i]], breaks = 5, xlab = 'Peak Week', main = levels(m$season)[i])
  print(quantile(pt.dists[[i]], probs = c(0.025, 0.975)))
}
# really quite a wide range, and varies by season
quantile(m$obs_pkwk, probs = c(0, 0.025, 0.05, 0.95, 0.975, 1.0))
# 95% over all countries and seasons are between 52 and 64, inclusive
# starting our synthetic model with week 40 - so want peaks to be in (52:64) - 39 = 13:25
# by season, how many are in this range?
for (i in 1:length(pt.dists)) {
  print(length(pt.dists[[i]][pt.dists[[i]] > 51 & pt.dists[[i]] < 65]) / length(pt.dists[[i]]))
} # consistently at least 88%

# Missing OT?:
m[is.na(m$onsetObs), ] # 1 in 18-19, 2 in 13-14, otherwise 0
# for simplicity, allow only where ALL countries have outbreaks
# or: allow up to 2 countries not to have onsets

# Look at when OT occurs:
quantile(m$onsetObs, probs = c(0, 0.025, 0.05, 0.95, 0.975, 1.0), na.rm = TRUE)
# 95% in 49:60, 90% in 49:60

# How late after onset is peak?:
quantile(m$obs_pkwk - m$onsetObs, na.rm = TRUE, probs = c(0, 0.025, 0.975, 1.0))
# 95% in 1-10 weeks; up to 12 weeks possible

# Which countries have earliest OT each season? Which occur within a certain time frame?:
early.onsets <- c()
for (season in levels(m$season)) {
  m.temp <- m[m$season == season, ]
  m.temp <- m.temp[!is.na(m.temp$onsetObs), ]
  m.temp <- m.temp[order(m.temp$onsetObs), ]
  early.cut <- quantile(m.temp$onsetObs, probs = 0.25)
  print(as.vector(m.temp[m.temp$onsetObs <= early.cut, 1]))
  early.onsets <- c(early.onsets, as.vector(m.temp[m.temp$onsetObs <= early.cut, 1]))
}
rev(sort(table(early.onsets)))
# PT, IT, IE, LU tend to be early in >50% of seasons
length(unique(early.onsets)) # 18/21 countries can be in the earliest 25% to onset, though, so makes sense to explore seeding in each country
# maybe also random draws of 2-3 countries?
# even if we reduce it to first 5%, still a wide range of countries
# some seasons seem to begin in one country, some begin in multiple

# Find initial countries for each outbreak:
init.arrival <- vector('list', length(levels(m$season)))
for (i in 1:length(init.arrival)) {
  m.temp <- m[m$season == levels(m$season)[i], ]
  init.arrival[[i]] <- as.vector(m.temp[m.temp$onsetObs == min(m.temp$onsetObs, na.rm = TRUE) & !is.na(m.temp$onsetObs), 'country'])
}
# usually 1 country; can be 3 (once) or 6 (once)
# 2/9: Portugal, 2/9: Luxembourg, 1/9: UK, 1/9: Poland, 1/9: Italy
# 1/9: Germany, Italy, Spain
# 1/9: Austria, Ireland, Luxembourg, Netherlands, Portugal, Slovenia

# 3/9: Portugal, 3/9: Luxembourg, 2/9: Italy





