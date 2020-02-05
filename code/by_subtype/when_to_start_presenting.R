
### Determine at which week results should begin to be considered for each season and subtype combination ###

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
seasons <- seasons[2:9]

# Don't consider PL 11-12 or CZ 13-14:
for (i in 1:3) {
  pos.dat[[i]]$Poland[seasons[[2]]] <- NA
  pos.dat[[i]]$Czechia[seasons[[4]]] <- NA
}

# For each subtype, need to look at each season forecasted for that subtype, and figure out at which week "at least 4 at least 10% for 3 consecutive weeks" met
# Need to wait for 3 consecutive, or start, when 4th and final hits 10%?
source('cluster/functions/Util.R')
baseline <- 0.08

# H1 (1, 3, 4, 5, 6, 8):
# ('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18')
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[1]][season, ]
  
  onsets <- c()
  for (i in 2:13) {
    onsets <- c(onsets, findOnset(pos.temp[, i], baseline)$onset + 40 - 1)
  }
  # print(sort(onsets))
  # print(sort(onsets)[4])
  print(sort(onsets)[2] + 2)
  print('')
  
  cnt <- cnt + 1
}
# 2010-11: 51 or 53
# 2012-13: 51 or 53
# 2013-14: 57 or 59
# 2014-15: 57 or 59
# 2015-16: 54 or 56
# 2017-18: 53 or 55

# H3 (2, 3, 4, 5, 7):
# ('2011-12', '2012-13', '2013-14', '2014-15', '2016-17')
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[2]][season, ]
  
  onsets <- c()
  for (i in 2:13) {
    onsets <- c(onsets, findOnset(pos.temp[, i], baseline)$onset + 40 - 1)
  }
  # print(sort(onsets))
  # print(sort(onsets)[4])
  print(sort(onsets)[2] + 2)
  print('')
  
  cnt <- cnt + 1
}
# 2011-12: 54 or 56
# 2012-13: 60 or 62
# 2013-14: 56 or 58
# 2014-15: 51 or 53
# 2016-17: 49 or 51

# B (1, 3, 5, 6, 7, 8):
# ('2010-11', '2012-13', '2014-15', '2015-16', '2016-17', '2017-18')
cnt <- 1
for (season in seasons) {
  print(cnt)
  pos.temp <- pos.dat[[3]][season, ]
  
  onsets <- c()
  for (i in 2:13) {
    onsets <- c(onsets, findOnset(pos.temp[, i], baseline)$onset + 40 - 1)
  }
  # print(sort(onsets))
  # print(sort(onsets)[4])
  print(sort(onsets)[2] + 2)
  print('')
  
  cnt <- cnt + 1
}
# 2010-11: 56 or 58
# 2012-13: 52 or 54
# 2014-15: 61 or 63
# 2015-16: 57 or 59
# 2016-17: 67 or 69
# 2017-18: 51 or 53
