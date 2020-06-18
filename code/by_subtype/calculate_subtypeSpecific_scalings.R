
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
dat.list <- list(dat.H1, dat.H3, dat.B)

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
rm(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718, i)

# List appropriate seasons by subtype:
seasons.h1 <- c(1:2, 4:7, 9) # only look at pandemic for H1N1
seasons.h3 <- c(3:6, 8)
seasons.b <- c(2, 4, 6:9)

# First, calculate scalings for ALL seasons used for each subtype:
countries <- names(dat.H1)[2:13]

# H1:
subtype.index <- 1
scalings.h1 <- vector('list', 9)
for (season.out in seasons.h1[2:7]) {
  for (count.index in 1:12) {
    dat.temp <- dat.list[[subtype.index]][, count.index + 1]
    min.scales = max.scales = c()
    
    for (season in seasons.h1[seasons.h1 != season.out]) {
      ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
      min.scales <- c(min.scales, 15000 / ar)
      max.scales <- c(max.scales, 50000 / ar)
    }
    
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    
    scalings.h1[[season.out]] <- c(scalings.h1[[season.out]], min(overlap, no.overlap))
  }
  
  # FR:
  dat.temp <- dat.list[[subtype.index]]$France
  min.scales = max.scales = c()
  if (season.out %in% 4:5) {
    ar <- sum(dat.temp[seasons[[c(4:5)[c(4:5) != season.out]]]], na.rm = TRUE)
    min.scales <- 15000/ar; max.scales <- 50000/ar
    scalings.h1[[season.out]][4] <- min.scales
    
  } else if (season.out %in% 6:9) {
    for (season in c(6:9)[c(6:9) != season.out]) {
      if (season %in% seasons.h1) {
        ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
        min.scales <- c(min.scales, 15000 / ar)
        max.scales <- c(max.scales, 50000 / ar)
      }
    }
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    scalings.h1[[season.out]][4] <- min(overlap, no.overlap)
    
  } else {
    # print(season.out)
    scalings.h1[[season.out]][4] <- NA
  }
  
}
rm(subtype.index, season.out, count.index, no.overlap, ar, dat.temp, overlap, min.scales, max.scales, season)

# Two other NA seasons, right? (PL 11-12, CZ 13-14)
# scalings.h1[[3]][10] <- NA # no H1N1 epidemic
scalings.h1[[5]][3] <- NA

# And now look at range of scalings - if largest is >1.5x greater (more than 75% greater) than next largest, adjust down to 1.5x:
    # 1.75x didn't quite capture large 15-16 scaling in France
    # also it doesn't make sense for different seasons to have radically different scalings if all data from same surveillance system
for (i in 1:12) {
  scales <- c()
  for (j in seasons.h1[2:7]) {
    scales <- c(scales, scalings.h1[[j]][i])
  }
  
  if (i != 4) {
    check.len <- length(scales[scales == max(scales, na.rm = TRUE)])
    max.pos <- which.max(scales)
    max.scale <- max(scales, na.rm = TRUE)
    # scales.orig <- scales
    scales <- scales[scales < max.scale]
    
    # check.len.min <- length(scales[scales == min(scales, na.rm = TRUE) & !is.na(scales)])
    # if (check.len.min == 1) {
    #   min.pos <- which.min(scales)
    #   min.scale <- min(scales, na.rm = TRUE)
    #   scales2 <- scales.orig[scales.orig > min.scale]
    #   print(i)
    #   print(min.scale < 0.667 * min(scales2, na.rm = TRUE))
    # }
    
    if (length(scales) > 0) {
      if (max.scale > 1.5 * max(scales, na.rm = TRUE) & check.len == 1) {
        # print(i)
        # print(check.len)
        scalings.h1[[seasons.h1[2:7][max.pos]]][i] <- 1.5 * max(scales, na.rm = TRUE)
      }
    }
  } else {
    scales1 <- scales[which(seasons.h1[2:7] %in% 4:5)]
    scales2 <- scales[which(seasons.h1[2:7] %in% 6:9)]
    
    # print(max(scales1) / min(scales1)) # fine
    max.scale <- max(scales2)
    scales2 <- scales2[scales2 < max.scale]
    # print(max.scale / max(scales2)) # fine
  }
  
}
rm(i, j, max.pos, max.scale, scales, scales1, scales2)

# H3:
subtype.index <- 2
scalings.h3 <- vector('list', 9)
for (season.out in seasons.h3) {
  for (count.index in 1:12) {
    dat.temp <- dat.list[[subtype.index]][, count.index + 1]
    min.scales = max.scales = c()
    
    for (season in seasons.h3[seasons.h3 != season.out]) {
      ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
      min.scales <- c(min.scales, 15000 / ar)
      max.scales <- c(max.scales, 50000 / ar)
    }
    
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    
    scalings.h3[[season.out]] <- c(scalings.h3[[season.out]], min(overlap, no.overlap))
  }
  
  # FR:
  dat.temp <- dat.list[[subtype.index]]$France
  min.scales = max.scales = c()
  if (season.out %in% 4:5) {
    ar <- sum(dat.temp[seasons[[c(4:5)[c(4:5) != season.out]]]], na.rm = TRUE)
    min.scales <- 15000/ar; max.scales <- 50000/ar
    scalings.h3[[season.out]][4] <- min.scales
    
  } else if (season.out %in% 6:9) {
    for (season in c(6:9)[c(6:9) != season.out]) {
      if (season %in% seasons.h3) {
        ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
        min.scales <- c(min.scales, 15000 / ar)
        max.scales <- c(max.scales, 50000 / ar)
      }
    }
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    scalings.h3[[season.out]][4] <- min(overlap, no.overlap)
    
  } else {
    # print(season.out)
    scalings.h3[[season.out]][4] <- NA
  }
  
}
rm(subtype.index, season.out, count.index, no.overlap, ar, dat.temp, overlap, min.scales, max.scales, season)

# Two other NA seasons, right? (PL 11-12, CZ 13-14)
scalings.h3[[3]][10] <- NA
scalings.h3[[5]][3] <- NA

# If largest scaling >1.5x greater (more than 50% greater) than next largest, adjust down to 1.5x:
for (i in 1:12) {
  scales <- c()
  for (j in seasons.h3) {
    scales <- c(scales, scalings.h3[[j]][i])
  }
  
  if (i != 4) {
    check.len <- length(scales[scales == max(scales, na.rm = TRUE) & !is.na(scales)])
    max.pos <- which.max(scales)
    max.scale <- max(scales, na.rm = TRUE)
    # scales.orig <- scales
    scales <- scales[scales < max.scale]
    
    # check.len.min <- length(scales[scales == min(scales, na.rm = TRUE) & !is.na(scales)])
    # if (check.len.min == 1) {
    #   min.pos <- which.min(scales)
    #   min.scale <- min(scales, na.rm = TRUE)
    #   scales2 <- scales.orig[scales.orig > min.scale]
    #   print(i)
    #   print(min.scale < 0.667 * min(scales2, na.rm = TRUE))
    # } # DE only
    
    if (length(scales) > 0) {
      if (max.scale > 1.5 * max(scales, na.rm = TRUE) & check.len == 1) { # only do this if there's one single outlier
        # print(i)
        # print(check.len)
        scalings.h3[[seasons.h3[max.pos]]][i] <- 1.5 * max(scales, na.rm = TRUE)
      }
    }
  } else {
    scales1 <- scales[which(seasons.h3 %in% 4:5)]
    scales2 <- scales[which(seasons.h3 %in% 6:9)]
    
    # print(max(scales1) / min(scales1)) # fine
    # print(max(scales2) / min(scales2)) # fine
    # max.scale <- max(scales2)
    # scales2 <- scales2[scales2 < max.scale]
    # print(max.scale / max(scales2)) # fine
  }
  
}
rm(i, j, max.pos, max.scale, scales, scales1, scales2)

# B:
subtype.index <- 3
scalings.b <- vector('list', 9)
for (season.out in seasons.b) {
  for (count.index in 1:12) {
    dat.temp <- dat.list[[subtype.index]][, count.index + 1]
    min.scales = max.scales = c()
    
    for (season in seasons.b[seasons.b != season.out]) {
      ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
      min.scales <- c(min.scales, 15000 / ar)
      max.scales <- c(max.scales, 50000 / ar)
    }
    
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    
    scalings.b[[season.out]] <- c(scalings.b[[season.out]], min(overlap, no.overlap))
  }
  
  # FR:
  dat.temp <- dat.list[[subtype.index]]$France
  min.scales = max.scales = c()
  if (season.out == 4) {
    ar <- sum(dat.temp[seasons[[4]]], na.rm = TRUE)
    min.scales <- 15000/ar; max.scales <- 50000/ar
    scalings.b[[season.out]][4] <- min.scales
    
  } else if (season.out %in% 6:9) {
    for (season in c(6:9)[c(6:9) != season.out]) {
      if (season %in% seasons.b) {
        ar <- sum(dat.temp[seasons[[season]]], na.rm = TRUE)
        min.scales <- c(min.scales, 15000 / ar)
        max.scales <- c(max.scales, 50000 / ar)
      }
    }
    no.overlap <- max(min.scales[min.scales > 0 & min.scales != Inf])
    overlap <- min(max.scales[max.scales > 0 & max.scales != Inf])
    scalings.b[[season.out]][4] <- min(overlap, no.overlap)
    
  } else {
    # print(season.out)
    scalings.b[[season.out]][4] <- NA
  }
  
}
rm(subtype.index, season.out, count.index, no.overlap, ar, dat.temp, overlap, min.scales, max.scales, season)

# Two other NA seasons, right? (PL 11-12, CZ 13-14)
# scalings.b[[3]][10] <- NA # no B outbreak
# scalings.b[[5]][3] <- NA # no B outbreak

# If largest scaling >1.5x greater (more than 50% greater) than next largest, adjust down to 1.5x:
for (i in 1:12) {
  scales <- c()
  for (j in seasons.b) {
    scales <- c(scales, scalings.b[[j]][i])
  }
  
  if (i != 4) {
    check.len <- length(scales[scales == max(scales, na.rm = TRUE) & !is.na(scales)])
    max.pos <- which.max(scales)
    max.scale <- max(scales, na.rm = TRUE)
    # scales.orig <- scales
    scales <- scales[scales < max.scale]
    
    # check.len.min <- length(scales[scales == min(scales, na.rm = TRUE) & !is.na(scales)])
    # if (check.len.min == 1) {
    #   min.pos <- which.min(scales)
    #   min.scale <- min(scales, na.rm = TRUE)
    #   scales2 <- scales.orig[scales.orig > min.scale]
    #   print(i)
    #   print(min.scale < 0.667 * min(scales2, na.rm = TRUE))
    # } # LU only
    
    if (length(scales) > 0) {
      if (max.scale > 1.5 * max(scales, na.rm = TRUE) & check.len == 1) {
        # print(i)
        # print(check.len)
        scalings.b[[seasons.b[max.pos]]][i] <- 1.5 * max(scales, na.rm = TRUE)
      }
    }
  } else {
    scales1 <- scales[which(seasons.b %in% 4:5)]
    scales2 <- scales[which(seasons.b %in% 6:9)]
    
    # print(max(scales1) / min(scales1)) # only one season
    max.pos <- which.max(scales2)
    max.scale <- max(scales2)
    scales2 <- scales2[scales2 < max.scale]
    if (max.scale > 1.5 * max(scales2, na.rm = TRUE)) {
      print(i)
      scalings.b[[(6:9)[max.pos]]][i] <- 1.5 * max(scales2, na.rm = TRUE)
    }
    # print(max.scale / max(scales2)) # close but technically fine? Can set to 1.7x, but this is still going to be really high - in this case, maybe 1.5x is better?
  }
  
}
rm(i, j, max.pos, max.scale, scales, scales1, scales2)

# Compile and remove pandemic:
scalings.new <- list(scalings.h1[2:9], scalings.h3[2:9], scalings.b[2:9])

# Save:
save(scalings.new, file = 'data/by_subtype/scalings_leaveOneOut_06-17-20.RData')
rm(list = ls())

# Reformat for python:
load('data/by_subtype/scalings_leaveOneOut_06-17-20.RData')

# Get country names:
dat.H1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
dat.H1 <- dat.H1[, c(1, count.indices + 1)]
countries <- names(dat.H1)[2:13]
rm(dat.H1, count.indices)

# Get individual scaling frames for each (sub)type and season:
scale.h1.1011 <- as.data.frame(cbind(countries, scalings.new[[1]][[1]]))
scale.h1.1213 <- as.data.frame(cbind(countries, scalings.new[[1]][[3]]))
scale.h1.1314 <- as.data.frame(cbind(countries, scalings.new[[1]][[4]]))
scale.h1.1415 <- as.data.frame(cbind(countries, scalings.new[[1]][[5]]))
scale.h1.1516 <- as.data.frame(cbind(countries, scalings.new[[1]][[6]]))
scale.h1.1718 <- as.data.frame(cbind(countries, scalings.new[[1]][[8]]))

scale.h3.1112 <- as.data.frame(cbind(countries, scalings.new[[2]][[2]]))
scale.h3.1213 <- as.data.frame(cbind(countries, scalings.new[[2]][[3]]))
scale.h3.1314 <- as.data.frame(cbind(countries, scalings.new[[2]][[4]]))
scale.h3.1415 <- as.data.frame(cbind(countries, scalings.new[[2]][[5]]))
scale.h3.1617 <- as.data.frame(cbind(countries, scalings.new[[2]][[7]]))

scale.b.1011 <- as.data.frame(cbind(countries, scalings.new[[3]][[1]]))
scale.b.1213 <- as.data.frame(cbind(countries, scalings.new[[3]][[3]]))
scale.b.1415 <- as.data.frame(cbind(countries, scalings.new[[3]][[5]]))
scale.b.1516 <- as.data.frame(cbind(countries, scalings.new[[3]][[6]]))
scale.b.1617 <- as.data.frame(cbind(countries, scalings.new[[3]][[7]]))
scale.b.1718 <- as.data.frame(cbind(countries, scalings.new[[3]][[8]]))

# Rename columns:
names(scale.h1.1011) = names(scale.h1.1213) = names(scale.h1.1314) = names(scale.h1.1415) = names(scale.h1.1516) = names(scale.h1.1718) =
  names(scale.h3.1112) = names(scale.h3.1213) = names(scale.h3.1314) = names(scale.h3.1415) = names(scale.h3.1617) =
  names(scale.b.1011) = names(scale.b.1213) = names(scale.b.1415) = names(scale.b.1516) = names(scale.b.1617) = names(scale.b.1718) = c('country', 'gamma')

# Save csvs:
write.csv(scale.h1.1011, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2010-11.csv', row.names = FALSE)
write.csv(scale.h1.1213, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2012-13.csv', row.names = FALSE)
write.csv(scale.h1.1314, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2013-14.csv', row.names = FALSE)
write.csv(scale.h1.1415, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2014-15.csv', row.names = FALSE)
write.csv(scale.h1.1516, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2015-16.csv', row.names = FALSE)
write.csv(scale.h1.1718, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H1)_2017-18.csv', row.names = FALSE)

write.csv(scale.h3.1112, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H3)_2011-12.csv', row.names = FALSE)
write.csv(scale.h3.1213, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H3)_2012-13.csv', row.names = FALSE)
write.csv(scale.h3.1314, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H3)_2013-14.csv', row.names = FALSE)
write.csv(scale.h3.1415, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H3)_2014-15.csv', row.names = FALSE)
write.csv(scale.h3.1617, file = 'data/by_subtype/scaling_frames/scalings_frame_A(H3)_2016-17.csv', row.names = FALSE)

write.csv(scale.b.1011, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2010-11.csv', row.names = FALSE)
write.csv(scale.b.1213, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2012-13.csv', row.names = FALSE)
write.csv(scale.b.1415, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2014-15.csv', row.names = FALSE)
write.csv(scale.b.1516, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2015-16.csv', row.names = FALSE)
write.csv(scale.b.1617, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2016-17.csv', row.names = FALSE)
write.csv(scale.b.1718, file = 'data/by_subtype/scaling_frames/scalings_frame_B_2017-18.csv', row.names = FALSE)

rm(list = ls())











