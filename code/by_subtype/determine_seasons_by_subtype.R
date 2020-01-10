
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

# Season delineations:
pull.2009 <- 1:75
pull.1011 <- 76:107
pull.1112 <- 128:159
pull.1213 <- 180:211
pull.1314 <- 232:263
pull.1415 <- 284:315
pull.1516 <- 336:368
pull.1617 <- 389:420
pull.1718 <- 441:472
seasons <- list(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
rm(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
for (i in 1:length(seasons)) {
  seasons[[i]] <- seasons[[i]] + 3
}
seasons[[1]] <- 1:78
seasons <- seasons[2:9]
# here limit to JUST the season b/c some countries don't have summer data

# Get some idea of a baseline by looking at percentiles (of non-0?) for all countries:
for (i in 2:13) {
  print(quantile(pos.dat[[3]][, i][pos.dat[[3]][, i] > 0 & !is.na(pos.dat[[3]][, i])], na.rm = TRUE))
}
# 25th %iles around 0.01-0.1; medians more around 0.15 (H1)
# similar for other two subtypes

# Don't consider PL 11-12 or CZ 13-14:
for (i in 1:3) {
  pos.dat[[i]]$Poland[seasons[[2]]] <- NA
  pos.dat[[i]]$Czechia[seasons[[4]]] <- NA
}

# Now find, for each subtype, the seasons positivity ever exceeds some baseline:
    # May need to say - for at least three weeks
source('cluster/functions/Util.R')

baseline <- 0.1
# for H1:
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[1]][season, ]
  counter <- 0
  for (i in 2:13) {
    # print(any(pos.temp[, i] > baseline, na.rm = TRUE))
    if (!is.na(findOnset(pos.temp[, i], baseline)$onset)) {
      counter <- counter + 1
    }
  }
  print(counter)
  cnt <- cnt + 1
  print('')
}

# for H3:
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[2]][season, ]
  counter <- 0
  for (i in 2:13) {
    # print(any(pos.temp[, i] > baseline, na.rm = TRUE))
    if (!is.na(findOnset(pos.temp[, i], baseline)$onset)) {
      counter <- counter + 1
    }
  }
  print(counter)
  cnt <- cnt + 1
  print('')
}

# for B:
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[3]][season, ]
  counter <- 0
  for (i in 2:13) {
    # print(any(pos.temp[, i] > baseline, na.rm = TRUE))
    if (!is.na(findOnset(pos.temp[, i], baseline)$onset)) {
      counter <- counter + 1
      # print(i)
      # print(findOnset(pos.temp[, i], baseline)$onset)
    }
  }
  print(counter)
  cnt <- cnt + 1
  print('')
}

# Might then be more honest to use a syn+ baseline, since positivity rates vary by country, too
# Still restrict to season proper since summer data are often missing

# Read in data:
who.dat.h1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
who.dat.h3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
who.dat.b <- read.csv('data/by_subtype/WHO_data_B.csv')

# Restrict to countries:
who.dat.h1 <- who.dat.h1[, c(1, count.indices + 1)]
who.dat.h3 <- who.dat.h3[, c(1, count.indices + 1)]
who.dat.b <- who.dat.b[, c(1, count.indices + 1)]

# Restrict to within-season data:
seasonal.dat.h1 <- who.dat.h1[unlist(seasons), ]
seasonal.dat.h3 <- who.dat.h3[unlist(seasons), ]
seasonal.dat.b <- who.dat.b[unlist(seasons), ]

# Calculate percentiles of non-0 values for each country:
# For tropical onset, wanted 33rd %ile; here, since we're looking for a looser baseline on when to start forecasting, maybe 25th?
temp.base.h1 <- c()
temp.base.h3 <- c()
temp.base.b <- c()
for (i in 2:13) {
  df <- seasonal.dat.h1[, i]
  if (i == 5) {
    df1 <- df[1:128]
    df2 <- df[129:257]
    temp.base.h1 <- c(temp.base.h1, quantile(df1[df1 > 0 & !is.na(df1)], prob = 0.50, na.rm = TRUE))
    temp.base.h1 <- c(temp.base.h1, quantile(df2[df2 > 0 & !is.na(df2)], prob = 0.50, na.rm = TRUE))
  } else {
    print(quantile(df[df > 0 & !is.na(df)], na.rm = TRUE))
    temp.base.h1 <- c(temp.base.h1, quantile(df[df > 0 & !is.na(df)], prob = 0.50, na.rm = TRUE))
  }
}
for (i in 2:13) {
  df <- seasonal.dat.h3[, i]
  if (i == 5) {
    df1 <- df[1:128]
    df2 <- df[129:257]
    temp.base.h3 <- c(temp.base.h3, quantile(df1[df1 > 0 & !is.na(df1)], prob = 0.50, na.rm = TRUE))
    temp.base.h3 <- c(temp.base.h3, quantile(df2[df2 > 0 & !is.na(df2)], prob = 0.50, na.rm = TRUE))
  } else {
    print(quantile(df[df > 0 & !is.na(df)], na.rm = TRUE))
    temp.base.h3 <- c(temp.base.h3, quantile(df[df > 0 & !is.na(df)], prob = 0.50, na.rm = TRUE))
  }
}
for (i in 2:13) {
  df <- seasonal.dat.b[, i]
  if (i == 5) {
    df1 <- df[1:128]
    df2 <- df[129:257]
    temp.base.b <- c(temp.base.b, quantile(df1[df1 > 0 & !is.na(df1)], prob = 0.50, na.rm = TRUE))
    temp.base.b <- c(temp.base.b, quantile(df2[df2 > 0 & !is.na(df2)], prob = 0.50, na.rm = TRUE))
  } else {
    print(quantile(df[df > 0 & !is.na(df)], na.rm = TRUE))
    temp.base.b <- c(temp.base.b, quantile(df[df > 0 & !is.na(df)], prob = 0.50, na.rm = TRUE))
  }
}
# somewhat similar across strains

# Now let's test them - say x countries must have exceed this value for at least 3 straight weeks:
source('cluster/functions/Util.R')
season.cnt <- 1
for (season in seasons) {
  print(season.cnt)
  temp.base <- temp.base.b
  
  if (season.cnt < 5) {
    temp.base <- temp.base[c(1:4, 6:13)]
  } else {
    temp.base <- temp.base[c(1:3, 5:13)]
  }
  
  dat.temp <- who.dat.b[season, ]
  cnt <- 0
  for (i in 2:13) {
    # print(findOnset(dat.temp[, i], baseline = temp.base[i - 1]))
    if (!is.na(findOnset(dat.temp[, i], baseline = temp.base[i - 1])$onset)) {
      cnt <- cnt + 1
    }
  }
  print(cnt)
  
  season.cnt <- season.cnt + 1
  print('')
}


cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[1]][season, ]
  for (i in 2:13) {
    # print(any(pos.temp[, i] > baseline, na.rm = TRUE))
    print(findOnset(pos.temp[, i], baseline)$onset)
  }
  cnt <- cnt + 1
  print('')
}








### Calculated proportion of total positive tests belonging to each subtype over each season for each country ###



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


















