
# Read in all files (network):
file.list = list.files(path = 'results/network/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/network/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 62, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 63, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 69, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/network/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

# Read in all files (isolated):
file.list = list.files(path = 'results/isolated/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/isolated/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 62, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 63, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 59, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 69, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/isolated/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

### ALTERNATIVE ###
# Still 4 countries, but only have to exceed 5%

# Read in all files (network):
file.list = list.files(path = 'results/network/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/network/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 60, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 65, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/network/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

# Read in all files (isolated):
file.list = list.files(path = 'results/isolated/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/isolated/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 56, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 52, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 60, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 57, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 65, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/isolated/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

### PLAY AROUND ###
# Still 4 countries, but only have to exceed 5%

# Read in all files (network):
file.list = list.files(path = 'results/network/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/network/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 48, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 63, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 49, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/network/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

# Read in all files (isolated):
file.list = list.files(path = 'results/isolated/')
res.list <- list()
for (i in 1:length(file.list)) {
  res.list[[i]] <- read.csv(paste0('results/isolated/', file.list[i]))
}

# Loop through files, seasons, subtypes, and keep only where fc_start is high enough:
for (i in 1:length(res.list)) {
  f <- res.list[[i]]
  # rebuild results file with appropriate pieces
  f.new <- NULL
  
  for (subtype in levels(f$subtype)) {
    for (season in levels(f$season)) {
      
      f.temp <- f[f$subtype == subtype & f$season == season, ]
      
      if (subtype == 'A(H1)' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H1)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H1)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H1)' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 53, ]
      } else if (subtype == 'A(H1)' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'A(H3)' & season == '2011-12') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'A(H3)' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'A(H3)' & season == '2013-14') {
        f.temp <- f.temp[f.temp$fc_start >= 54, ]
      } else if (subtype == 'A(H3)' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'A(H3)' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 48, ]
      } else if (subtype == 'B' & season == '2010-11') {
        f.temp <- f.temp[f.temp$fc_start >= 50, ]
      } else if (subtype == 'B' & season == '2012-13') {
        f.temp <- f.temp[f.temp$fc_start >= 51, ]
      } else if (subtype == 'B' & season == '2014-15') {
        f.temp <- f.temp[f.temp$fc_start >= 58, ]
      } else if (subtype == 'B' & season == '2015-16') {
        f.temp <- f.temp[f.temp$fc_start >= 55, ]
      } else if (subtype == 'B' & season == '2016-17') {
        f.temp <- f.temp[f.temp$fc_start >= 63, ]
      } else if (subtype == 'B' & season == '2017-18') {
        f.temp <- f.temp[f.temp$fc_start >= 49, ]
      } else {
        print(length(f.temp$fc_start)) # should always be 0
      }
      
      f.new <- rbind(f.new, f.temp)
      
    }
  }
  
  f.new <- as.data.frame(f.new)
  res.list[[i]] <- f.new
}

# Write new data frames to file:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/isolated/RED_', file.list[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())









