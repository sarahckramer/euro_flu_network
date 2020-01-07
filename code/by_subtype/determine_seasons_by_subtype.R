
# Which seasons to be forecasted for each of the three subtypes?

# Read in virological data by subtype:
pos.dat.h1 <- read.csv('data/by_subtype/posprop_A(H1).csv')
pos.dat.h3 <- read.csv('data/by_subtype/posprop_A(H3).csv')
pos.dat.b <- read.csv('data/by_subtype/posprop_B.csv')

pos.dat <- list(pos.dat.h1, pos.dat.h3, pos.dat.b)
rm(pos.dat.h1, pos.dat.h3, pos.dat.b)

# Limit to countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

for (i in 1:length(pos.dat)) {
  pos.dat[[i]] <- pos.dat[[i]][, c(1, count.indices + 1)]
}; rm(i)

# Change negatives to NAs:
for (i in 1:length(pos.dat)) {
  for (j in 2:13) {
    pos.dat[[i]][, j][pos.dat[[i]][, j] < 0] <- NA
  }
}; rm(i, j)

### Calculated proportion of total positive tests belonging to each subtype over each season for each country ###

# Season delineations:
pull.2009 <- 1:75
pull.1011 <- 76:127
pull.1112 <- 128:179
pull.1213 <- 180:231
pull.1314 <- 232:283
pull.1415 <- 284:335
pull.1516 <- 336:388
pull.1617 <- 389:440
pull.1718 <- 441:492
seasons <- list(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
rm(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
for (i in 1:length(seasons)) {
  seasons[[i]] <- seasons[[i]] + 3
}
seasons[[1]] <- 1:78
seasons <- seasons[2:9]

# Sum up each subtype/country for each season:
df.sums <- vector('list', length(pos.dat))
for (i in 1:length(pos.dat)) {
  pos.temp <- pos.dat[[i]]
  df.temp <- NULL
  
  for (season in 1:length(seasons)) {
    
    timespan <- seasons[[season]]
    sums.temp <- c()
    for (j in 1:length(countries)) {
      sums.temp <- c(sums.temp, sum(pos.temp[timespan, j + 1], na.rm = TRUE))
    }
    df.temp <- rbind(df.temp, c(season, sums.temp))
    
  }
  
  df.temp <- as.data.frame(df.temp)
  names(df.temp) <- c('season', countries)
  # df.temp$subtype <- i
  df.sums[[i]] <- df.temp
}
rm(i, season, j, df.temp, pos.temp, sums.temp, timespan)

# Melt and combine:
for (i in 1:length(df.sums)) {
  df.temp <- melt(df.sums[[i]], id.vars = 'season')
  names(df.temp) <- c('season', 'country', paste('value', i, sep = '_'))
  df.sums[[i]] <- df.temp
}
df.sums <- cbind(df.sums[[1]], df.sums[[2]], df.sums[[3]])
df.sums <- df.sums[, c(1:3, 6, 9)]

# Convert to proportions of total:
df.sums$value_tot <- df.sums$value_1 + df.sums$value_2 + df.sums$value_3
df.sums$value_1 <- df.sums$value_1 / df.sums$value_tot
df.sums$value_2 <- df.sums$value_2 / df.sums$value_tot
df.sums$value_3 <- df.sums$value_3 / df.sums$value_tot
df.sums <- df.sums[, 1:5]

# Improve labeling:
df.sums$season <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')[df.sums$season]
df.sums$season <- factor(df.sums$season)
names(df.sums)[3:5] <- c('H1', 'H3', 'B')

# Remove 4 seasons not included in analysis:
df.sums <- df.sums[!is.na(df.sums$H1), ]
df.sums <- df.sums[!(df.sums$country == 'CZ' & df.sums$season == '2013-14') & !(df.sums$country == 'PL' & df.sums$season == '2011-12'), ]

# Look at overall proportion for all 12 countries by season:
for (season in levels(df.sums$season)) {
  print(season)
  print(df.sums$B[df.sums$season == season])
  print('')
}


















