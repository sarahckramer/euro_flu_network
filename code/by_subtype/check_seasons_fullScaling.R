
# Read in data scaled using "full" scalings:
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED_full.csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED_full.csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B_SCALED_full.csv')

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

# For each season and subtype, how many countries have an onset?
source('cluster/functions/Util.R')
for (season.count in 1:length(seasons)) {
  season <- seasons[[season.count]]
  
  onset.count <- 0
  for (i in 1:12) {
    if(!is.na(findOnset(as.numeric(as.character(iliiso.b[season, i])), baseline = 500.0)$onset)) {
      onset.count <- onset.count + 1
    }
  }
  
  print(onset.count)
}




