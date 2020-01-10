
### Scale syndromic+ and syndromic data (all, and 4 types/subtypes), and save pre-scaled data ###

# Read in all syndromic+ data:
iliiso <- read.csv('data/WHO_data_05-09-19.csv')
# iliiso.A <- read.csv('data/by_subtype/WHO_data_A(all).csv')
iliiso.H1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
iliiso.H3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
iliiso.B <- read.csv('data/by_subtype/WHO_data_B.csv')

# Read in syndromic data:
syn.dat <- read.csv('data/synDatCounts_060519.csv')

# Specify countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Restrict all to relevant countries:
iliiso <- iliiso[, c(1, count.indices + 1)]
# iliiso.A <- iliiso.A[, c(1, count.indices + 1)]
iliiso.H1 <- iliiso.H1[, c(1, count.indices + 1)]
iliiso.H3 <- iliiso.H3[, c(1, count.indices + 1)]
iliiso.B <- iliiso.B[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]

# Get scalings for each type/subtype:
scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
scalings <- scalings[count.indices, ]
load('data/by_subtype/scalings_noCutoff_threeOverPointOne.RData')

# Create lists of syndromic+ and syndromic to loop through:
syn.plus.dat <- list(iliiso, iliiso.H1, iliiso.H3, iliiso.B)
syn.pure.dat <- list(syn.dat, syn.dat, syn.dat, syn.dat)

for (ix in 1:4) {
  
  if (ix == 1) {
    for (i in 2:13) {
      if (names(syn.plus.dat[[ix]])[i] == 'France') {
        syn.plus.dat[[ix]][1:286, i] <- syn.plus.dat[[ix]][1:286, i] * 1.3
        syn.plus.dat[[ix]][287:495, i] <- syn.plus.dat[[ix]][287:495, i] * scalings$gamma[scalings$country == names(syn.plus.dat[[ix]])[i]]
        syn.pure.dat[[ix]][1:286, i] <- syn.pure.dat[[ix]][1:286, i] * 1.3
        syn.pure.dat[[ix]][287:495, i] <- syn.pure.dat[[ix]][287:495, i] * scalings$gamma[scalings$country == names(syn.plus.dat[[ix]])[i]]
      } else {
        syn.plus.dat[[ix]][, i] <- syn.plus.dat[[ix]][, i] * scalings$gamma[scalings$country == names(syn.plus.dat[[ix]])[i]]
        syn.pure.dat[[ix]][, i] <- syn.pure.dat[[ix]][, i] * scalings$gamma[scalings$country == names(syn.plus.dat[[ix]])[i]]
      }
      
      syn.plus.dat[[ix]][, i][syn.plus.dat[[ix]][, i] < 0] <- NA # replace negatives with NAs
      syn.pure.dat[[ix]][, i][syn.pure.dat[[ix]][, i] < 0] <- NA
    }
    
  } else {
    for (i in 2:13) {
      if (names(syn.plus.dat[[ix]])[i] == 'France') {
        syn.plus.dat[[ix]][1:286, i] <- syn.plus.dat[[ix]][1:286, i] * scalings.new[[ix - 1]][13]
        syn.plus.dat[[ix]][287:495, i] <- syn.plus.dat[[ix]][287:495, i] * scalings.new[[ix - 1]][i - 1]
        syn.pure.dat[[ix]][1:286, i] <- syn.pure.dat[[ix]][1:286, i] * scalings.new[[ix - 1]][13]
        syn.pure.dat[[ix]][287:495, i] <- syn.pure.dat[[ix]][287:495, i] * scalings.new[[ix - 1]][i - 1]
        
      } else {
        syn.plus.dat[[ix]][, i] <- syn.plus.dat[[ix]][, i] * scalings.new[[ix - 1]][i - 1]
        syn.pure.dat[[ix]][, i] <- syn.pure.dat[[ix]][, i] * scalings.new[[ix - 1]][i - 1]
      }
      
      syn.plus.dat[[ix]][, i][syn.plus.dat[[ix]][, i] < 0] <- NA
      syn.pure.dat[[ix]][, i][syn.pure.dat[[ix]][, i] < 0] <- NA
    }
    
  }
  
}

# Remove data from lists:
iliiso <- syn.plus.dat[[1]]
# iliiso.A <- syn.plus.dat[[2]]
iliiso.H1 <- syn.plus.dat[[2]]
iliiso.H3 <- syn.plus.dat[[3]]
iliiso.B <- syn.plus.dat[[4]]

synDat <- syn.pure.dat[[1]]
# synDat.A <- syn.pure.dat[[2]]
synDat.H1 <- syn.pure.dat[[2]]
synDat.H3 <- syn.pure.dat[[3]]
synDat.B <- syn.pure.dat[[4]]

# Save:
write.csv(iliiso, file = 'data/WHO_data_05-09-19_SCALED.csv', row.names = FALSE)
write.csv(synDat, file = 'data/synDatCounts_060519_SCALED.csv', row.names = FALSE)

# write.csv(iliiso.A, file = 'data/by_subtype/WHO_data_A(all)_SCALED.csv', row.names = FALSE)
write.csv(iliiso.H1, file = 'data/by_subtype/WHO_data_A(H1)_SCALED.csv', row.names = FALSE)
write.csv(iliiso.H3, file = 'data/by_subtype/WHO_data_A(H3)_SCALED.csv', row.names = FALSE)
write.csv(iliiso.B, file = 'data/by_subtype/WHO_data_B_SCALED.csv', row.names = FALSE)

# write.csv(synDat.A, file = 'data/by_subtype/synDatCounts_A(all)_SCALED.csv', row.names = FALSE)
write.csv(synDat.H1, file = 'data/by_subtype/synDatCounts_A(H1)_SCALED.csv', row.names = FALSE)
write.csv(synDat.H3, file = 'data/by_subtype/synDatCounts_A(H3)_SCALED.csv', row.names = FALSE)
write.csv(synDat.B, file = 'data/by_subtype/synDatCounts_B_SCALED.csv', row.names = FALSE)

################################################################################################################################
################################################################################################################################
################################################################################################################################

# Get scalings_frame for all subtypes:
# scalings$gamma <- scalings.new[[1]][1:12]
# write.csv(scalings, 'data/by_subtype/scalings_frame_A(all).csv', row.names = FALSE)

scalings$gamma <- scalings.new[[1]][1:12]
write.csv(scalings, 'data/by_subtype/scalings_frame_A(H1).csv', row.names = FALSE)

scalings$gamma <- scalings.new[[2]][1:12]
write.csv(scalings, 'data/by_subtype/scalings_frame_A(H3).csv', row.names = FALSE)

scalings$gamma <- scalings.new[[3]][1:12]
write.csv(scalings, 'data/by_subtype/scalings_frame_B.csv', row.names = FALSE)












