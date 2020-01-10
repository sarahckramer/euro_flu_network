
# Read in subtype-specific syndromic+:
dat.H1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
dat.H3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
dat.B <- read.csv('data/by_subtype/WHO_data_B.csv')

# Get only countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

dat.H1 <- dat.H1[, c(1, count.indices + 1)]
dat.H3 <- dat.H3[, c(1, count.indices + 1)]
dat.B <- dat.B[, c(1, count.indices + 1)]

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
# But calculate scalings both ways and see how different they are; do forecasting using both
# So: 2 sets of scalings: including and excluding pan (no need to calculate "alternatives" anymore)

# Additional complication: Need to NOT consider the small blips during seasons/countries where there is no outbreak of a given strain
# What metrics did I use before to remove outbreaks?:
    # eh, prefer below
# Or could simply say: where certain strain was <X% of infections that year
    # 10%, 15%, 20%?, even 25%? - could do SA

# First convert any 0s to NAs, just for the sake of this exercise - those "seasons" can obviously be removed:
dat.H1[, 2:13][dat.H1[, 2:13] == 0 & !is.na(dat.H1[, 2:13])] <- NA
dat.H3[, 2:13][dat.H3[, 2:13] == 0 & !is.na(dat.H3[, 2:13])] <- NA
dat.B[, 2:13][dat.B[, 2:13] == 0 & !is.na(dat.B[, 2:13])] <- NA

# Get list of data frames:
dat.list <- list(dat.H1, dat.H3, dat.B)

# Calculate proportion of each strain for each country/season pair:
cases.strain.season <- lapply(dat.list, function(dat.ix) {
  lapply(seasons, function(ix) {
    colSums(dat.ix[ix, 2:13], na.rm = TRUE)
  })
})
# use full week 40-week 39, since that seems to be what we've done for scaling in the past

total1 = vector('list', 9)
for (i in 1:9) {
  total1[[i]] <- cases.strain.season[[1]][[i]] + cases.strain.season[[2]][[i]] + cases.strain.season[[3]][[i]] # used for 3 types/subtypes
}

props.strain.season <- vector('list', 3)
for (j in 1:3) {
  props.temp <- vector('list', 9)
  for (i in 1:9) {
    props.temp[[i]] <- cases.strain.season[[j]][[i]] / total1[[i]]
  }
  props.strain.season[[j]] <- props.temp
}
rm(props.temp)

# List appropriate seasons by subtype:
seasons.h1 <- c(1:2, 4:7, 9) # only look at pandemic for H1N1
seasons.h3 <- c(3:6, 8)
seasons.b <- c(2, 4, 6:9)

# First, calculate scalings for ALL seasons used for each subtype:
countries <- names(dat.H1)[2:13]
scalings.new <- vector('list', 3)

# H1:
subtype.index <- 1
for (count.index in 1:12) {
  dat.temp <- dat.list[[subtype.index]][, count.index + 1]
  min.scales = max.scales = c()
  
  for (season in seasons.h1) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
    
    # if (props.strain.season[[subtype.index]][[season]][count.index] >= 0.25 & !is.na(props.strain.season[[subtype.index]][[season]][count.index])) {
    #   ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    #   min.scales <- c(min.scales, 15000 / ar)
    #   max.scales <- c(max.scales, 50000 / ar)
    # } else {
    #   print(names(dat.A)[count.index + 1])
    #   print(c('2009pdm', '10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18')[season])
    # }
  }
  
  overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
  no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
  
  scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], min(overlap, no.overlap))
}
# FR:
dat.temp <- dat.list[[subtype.index]]$France
min.scales = max.scales = c()
for (season in 4:5) {
  if (season %in% seasons.h1) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale1 <- min(overlap, no.overlap)
min.scales = max.scales = c()
for (season in 6:9) {
  if (season %in% seasons.h1) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale2 <- min(overlap, no.overlap)
scalings.new[[subtype.index]][4] <- scale2; scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], scale1)
# so the scaling in FR's place is for the later 4 seasons; whereas the one "extra" value at the end is for the 2 earlier seasons (ARI)

# H3:
subtype.index <- 2
for (count.index in 1:12) {
  dat.temp <- dat.list[[subtype.index]][, count.index + 1]
  min.scales = max.scales = c()
  
  for (season in seasons.h3) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
    
    # if (props.strain.season[[subtype.index]][[season]][count.index] >= 0.25 & !is.na(props.strain.season[[subtype.index]][[season]][count.index])) {
    #   ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    #   min.scales <- c(min.scales, 15000 / ar)
    #   max.scales <- c(max.scales, 50000 / ar)
    # } else {
    #   print(names(dat.A)[count.index + 1])
    #   print(c('2009pdm', '10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18')[season])
    # }
  }
  
  overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
  no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
  
  scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], min(overlap, no.overlap))
}
# FR:
dat.temp <- dat.list[[subtype.index]]$France
min.scales = max.scales = c()
for (season in 4:5) {
  if (season %in% seasons.h3) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale1 <- min(overlap, no.overlap)
min.scales = max.scales = c()
for (season in 6:9) {
  if (season %in% seasons.h3) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale2 <- min(overlap, no.overlap)
scalings.new[[subtype.index]][4] <- scale2; scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], scale1)

# B:
subtype.index <- 3
for (count.index in 1:12) {
  dat.temp <- dat.list[[subtype.index]][, count.index + 1]
  min.scales = max.scales = c()
  
  for (season in seasons.b) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
    
    # if (props.strain.season[[subtype.index]][[season]][count.index] >= 0.25 & !is.na(props.strain.season[[subtype.index]][[season]][count.index])) {
    #   ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    #   min.scales <- c(min.scales, 15000 / ar)
    #   max.scales <- c(max.scales, 50000 / ar)
    # } else {
    #   print(names(dat.A)[count.index + 1])
    #   print(c('2009pdm', '10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18')[season])
    # }
  }
  
  overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
  no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
  
  scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], min(overlap, no.overlap))
}
# FR:
dat.temp <- dat.list[[subtype.index]]$France
min.scales = max.scales = c()
for (season in 4:5) {
  if (season %in% seasons.b) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale1 <- min(overlap, no.overlap)
min.scales = max.scales = c()
for (season in 6:9) {
  if (season %in% seasons.b) {
    ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
    min.scales <- c(min.scales, 15000 / ar)
    max.scales <- c(max.scales, 50000 / ar)
  }
}
overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
scale2 <- min(overlap, no.overlap)
scalings.new[[subtype.index]][4] <- scale2; scalings.new[[subtype.index]] <- c(scalings.new[[subtype.index]], scale1)

# Round scalings
# Doubt this is super necessary - just use the values that were calculated

# Save:
save(scalings.new, file = 'code/by_subtype/scalings_SA/scalings_noCutoff_threeOverPointOne.RData')

# Compare.
scalings.newMet <- scalings.new
rm(scalings.new)
load('data/by_subtype/scalings_noCutoff.RData')
# very similar to before







