
# Read in data:
dat.A <- read.csv('data/by_subtype/WHO_data_A(all).csv')

# Get only countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

dat.A <- dat.A[, c(1, count.indices + 1)]

# Season delineations
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
for (i in 1:length(seasons)) {
  seasons[[i]] <- seasons[[i]] + 3
}
seasons[[1]] <- 1:78

# Note: included pandemic data in choosing scalings - want them to be valid for this, too

# First convert any 0s to NAs, just for the sake of this exercise - those "seasons" can obviously be removed:
dat.A[, 2:13][dat.A[, 2:13] == 0 & !is.na(dat.A[, 2:13])] <- NA

# Calculate scalings:
countries <- names(dat.A)[2:13]
scalings <- c()
for (count.index in 1:12) {
  dat.temp <- dat.A[, count.index + 1]
  min.scales = max.scales = c()
  
  for (season in 1:9) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
  
  overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
  no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
  
  scalings <- c(scalings, min(overlap, no.overlap))
}
# FR:
dat.temp <- dat.A$France
min.scales = max.scales = c()
for (season in 4:5) {
  ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
  min.scales <- c(min.scales, 15000 / ar)
  max.scales <- c(max.scales, 50000 / ar)
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale1 <- min(overlap, no.overlap)
min.scales = max.scales = c()
for (season in 6:9) {
  ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
  min.scales <- c(min.scales, 15000 / ar)
  max.scales <- c(max.scales, 50000 / ar)
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale2 <- min(overlap, no.overlap)
scalings[4] <- scale2; scalings <- c(scalings, scale1)
# so the scaling in FR's place is for the later 4 seasons; whereas the one "extra" value at the end is for the 2 earlier seasons (ARI)

# Save!:
save(scalings, file = 'data/by_subtype/scalings_Aall.RData')




