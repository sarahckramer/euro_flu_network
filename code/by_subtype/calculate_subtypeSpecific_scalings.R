
# Read in subtype-specific syndromic+:
dat.H1 <- read.csv('data/WHO_data_A(H1).csv')
dat.H3 <- read.csv('data/WHO_data_A(H3).csv')
dat.A <- read.csv('data/WHO_data_A(all).csv')
dat.B <- read.csv('data/WHO_data_B.csv')

# Get only countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

dat.H1 <- dat.H1[, c(1, count.indices + 1)]
dat.H3 <- dat.H3[, c(1, count.indices + 1)]
dat.A <- dat.A[, c(1, count.indices + 1)]
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

# First convert any 0s to NAs, just for the sake of this exercise - those "seasons" can obviouly be removed:
dat.A[, 2:13][dat.A[, 2:13] == 0 & !is.na(dat.A[, 2:13])] <- NA
dat.H1[, 2:13][dat.H1[, 2:13] == 0 & !is.na(dat.H1[, 2:13])] <- NA
dat.H3[, 2:13][dat.H3[, 2:13] == 0 & !is.na(dat.H3[, 2:13])] <- NA
dat.B[, 2:13][dat.B[, 2:13] == 0 & !is.na(dat.B[, 2:13])] <- NA

# Check that dat.A is really same as H1+H3:
# for (i in 2:13) {
#   for (row in 1:495) {
#     if (!(is.na(dat.H1[row, i]) & is.na(dat.H3[row, i]))) {
#       if (!all.equal(sum(dat.H1[row, i], dat.H3[row, i], na.rm = TRUE), dat.A[row, i])) {
#         print(sum(dat.H1[row, i], dat.H3[row, i], na.rm = TRUE))
#         print(dat.A[row, i])
#         print('')
#       }
#     }
#   }
# }
# # Fix accordingly? It seems like there are spots where dat.A is NA, even though there were either H1 or H3 involved
#     # But INF_A was only ever used for checking against other math I did - it wasn't edited; so I think we need to trust these NAs
all.equal(dat.H1[, 2:13] + dat.H3[, 2:13], dat.A[, 2:13])
# differences in NAs are all that are present
# seems extra NAs come in when proportions are over 1 when compared to # of tests conducted

# Get list of data frames:
dat.list <- list(dat.A, dat.H1, dat.H3, dat.B)

# Calculate proportion of each strain for each country/season pair:
cases.strain.season <- lapply(dat.list, function(dat.ix) {
  lapply(seasons, function(ix) {
    colSums(dat.ix[ix, 2:13], na.rm = TRUE)
  })
})
# use full week 40-week 39, since that seems to be what we've done for scaling in the past

total1 = total2 = vector('list', 9)
for (i in 1:9) {
  total1[[i]] <- cases.strain.season[[1]][[i]] + cases.strain.season[[4]][[i]] # used for full A
  total2[[i]] <- cases.strain.season[[2]][[i]] + cases.strain.season[[3]][[i]] + cases.strain.season[[4]][[i]] # used for other 3 types/subtypes
}

props.strain.season <- vector('list', 4)

props.temp <- vector('list', 9)
for (i in 1:9) {
  props.temp[[i]] <- cases.strain.season[[1]][[i]] / total1[[i]]
}
props.strain.season[[1]] <- props.temp

for (j in 2:4) {
  props.temp <- vector('list', 9)
  for (i in 1:9) {
    props.temp[[i]] <- cases.strain.season[[j]][[i]] / total2[[i]]
  }
  props.strain.season[[j]] <- props.temp
}
rm(props.temp)

# Start looping through to determine scalings:
countries <- names(dat.A)[2:13]
scalings.new <- vector('list', 4)

for (ix in 1:length(props.strain.season)) {
  for (count.index in 1:12) {
    dat.temp <- dat.list[[ix]][, count.index + 1]
    
    min.scales = max.scales = c()
    
    for (season in 1:9) {
      # first, determine wheter we use this season
      if (props.strain.season[[ix]][[season]][count.index] >= 0.25 & !is.na(props.strain.season[[ix]][[season]][count.index])) {
        ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
        min.scales <- c(min.scales, 15000 / ar)
        max.scales <- c(max.scales, 50000 / ar)
      }
    }
    # then we have min and max scales only for those seasons where strain made up at least 25% of infections
    
    print(names(dat.A)[count.index + 1])
    
    overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    no.overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    
    scalings.new[[ix]] <- c(scalings.new[[ix]], min(overlap, no.overlap))
    
    # print(min.scales); print(max.scales); print('') # never a case of the vectors being completely empty
  }
  
}

# Calculate appropriate scalings for France:
for (ix in 1:length(props.strain.season)) {
  dat.temp <- dat.list[[ix]]$France
  
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
  
  scalings.new[[ix]][4] <- scale2; scalings.new[[ix]] <- c(scalings.new[[ix]], scale1)
}
# so the scaling in FR's place is for the later 4 seasons; whereas the one "extra" value at the end is for the 2 earlier seasons (ARI)

# Round scalings
# Doubt this is super necessary - just use the values that were calculated

# Save:
save(scalings.new, file = 'data/scalings_by_subtype_120219.RData')












