
### Scale syndromic+ and save scaled observations ###

# Read in all syndromic+ data:
iliiso.H1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
iliiso.H3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
iliiso.B <- read.csv('data/by_subtype/WHO_data_B.csv')

# Specify countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Restrict all to relevant countries:
iliiso.H1 <- iliiso.H1[, c(1, count.indices + 1)]
iliiso.H3 <- iliiso.H3[, c(1, count.indices + 1)]
iliiso.B <- iliiso.B[, c(1, count.indices + 1)]

# Get scalings for each type/subtype:
load('data/by_subtype/scalings_leaveOneOut_06-17-20.RData')

# Create lists of syndromic+ to loop through:
syn.plus.dat <- list(iliiso.H1, iliiso.H3, iliiso.B)

# Season delineations
pull.1011 <- 76:127
pull.1112 <- 128:179
pull.1213 <- 180:231
pull.1314 <- 232:283
pull.1415 <- 284:335
pull.1516 <- 336:388
pull.1617 <- 389:440
pull.1718 <- 441:492
seasons <- list(pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
for (i in 1:length(seasons)) {
  seasons[[i]] <- seasons[[i]] + 3
}
rm(pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)

# Loop through data and seasons, and scale:
for (ix in 1:3) { # H1, H3, B
  
  for (season.index in 1:length(seasons)) { # loop through all 8 seasons
    
    if (length(scalings.new[[ix]][[season.index]]) > 0) { # if that (sub)type is run for the given season
      for (i in 2:13) {
        syn.plus.dat[[ix]][seasons[[season.index]], i] <- syn.plus.dat[[ix]][seasons[[season.index]], i] * scalings.new[[ix]][[season.index]][i - 1]
      }
    } else { # if not, replace the data with NAs - they aren't used anyway
      syn.plus.dat[[ix]][seasons[[season.index]], 2:13] <- NA
    }
    
  }
  
}

# Remove data from lists:
iliiso.H1 <- syn.plus.dat[[1]]
iliiso.H3 <- syn.plus.dat[[2]]
iliiso.B <- syn.plus.dat[[3]]

# # Check plots:
# for (i in 2:13) {
#   plot(iliiso.H1[, i], type = 'l', main = names(iliiso.H1)[i])
#   lines(iliiso.H3[, i], col = 'blue')
#   lines(iliiso.B[, i], col = 'red')
# }

# Remove pandemic:
iliiso.H1[1:78, 2:13] <- NA
iliiso.H3[1:78, 2:13] <- NA
iliiso.B[1:78, 2:13] <- NA

# Save scaled data:
write.csv(iliiso.H1, file = 'data/by_subtype/WHO_data_A(H1)_SCALED.csv', row.names = FALSE)
write.csv(iliiso.H3, file = 'data/by_subtype/WHO_data_A(H3)_SCALED.csv', row.names = FALSE)
write.csv(iliiso.B, file = 'data/by_subtype/WHO_data_B_SCALED.csv', row.names = FALSE)
rm(list = ls())











