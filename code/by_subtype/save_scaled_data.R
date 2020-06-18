
### Scale syndromic+ and syndromic data (all, and 4 types/subtypes), and save pre-scaled data ###

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

# Compare with old scaled data:
iliiso.H1 <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
iliiso.H1.old <- read.csv('data/by_subtype/old/WHO_data_A(H1)_SCALED.csv')

iliiso.H3 <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
iliiso.H3.old <- read.csv('data/by_subtype/old/WHO_data_A(H3)_SCALED.csv')

iliiso.B <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')
iliiso.B.old <- read.csv('data/by_subtype/old/WHO_data_B_SCALED.csv')

for (i in 2:13) {
  plot(iliiso.H1[, i], pch = 20, main = names(iliiso.H1)[i])
  lines(iliiso.H1.old[, i], col = 'blue')
}
for (i in 2:13) {
  plot(iliiso.H3[, i], pch = 20, main = names(iliiso.H1)[i])
  lines(iliiso.H3.old[, i], col = 'blue')
}
for (i in 2:13) {
  plot(iliiso.B[, i], pch = 20, main = names(iliiso.H1)[i])
  lines(iliiso.B.old[, i], col = 'blue')
}

# sometimes this seems to lead to impossibly high AR, particularly for 16-17/H3(PL and SK) and 17-18/B(AT,CZ,HU,IT,NL,PL, and SK) - check and see if there's something that can be done here

# could use only those seasons where vir. activity exceeds some value, but then more countries will be limited to having only one season to calculate from
    # and also - we don't know which seasons have "outbreaks" until we've applied the scalings!
# first see what the AR of each season is - which exceed 100,000/100,000?:
ars.h1 <- c()
for (i in 1:8) {
  for (j in 2:13) {
    dat.temp <- iliiso.H1[seasons[[i]], j]
    if (!all(is.na(dat.temp))) {
      ars.h1 <- c(ars.h1, sum(dat.temp, na.rm = TRUE))
    }
  }
}
# 2 above 50,000 (~65,000), and 1 above 100,000 (AT 10-11 - scaling about 2x higher than others)
# after fixing: max is 75,000

for (i in 1:12) {
  scales <- c()
  for (j in c(1, 3:6, 8)) {
    scales <- c(scales, scalings.new[[1]][[j]][i])
  }
  print(names(iliiso.B)[i + 1])
  # print(max(scales, na.rm = T) / min(scales, na.rm = T))
  max.scale <- max(scales, na.rm = T)
  scales <- scales[scales < max.scale]
  print(max.scale / max(scales, na.rm = T))
}

ars.h3 <- c()
for (i in 1:8) {
  for (j in 2:13) {
    dat.temp <- iliiso.H3[seasons[[i]], j]
    if (!all(is.na(dat.temp))) {
      ars.h3 <- c(ars.h3, sum(dat.temp, na.rm = TRUE))
    }
  }
}
# several over 50 but not over 75; two over 100,000: PL/SK 16-17 (scalings 9.4x/2.5x)
# after fixing: max is 75,000

for (i in 1:12) {
  scales <- c()
  for (j in c(2:5, 7)) {
    scales <- c(scales, scalings.new[[2]][[j]][i])
  }
  print(names(iliiso.B)[i + 1])
  # print(max(scales, na.rm = T) / min(scales, na.rm = T))
  max.scale <- max(scales, na.rm = T)
  scales <- scales[scales < max.scale]
  print(max.scale / max(scales, na.rm = T))
}

ars.b <- c()
for (i in 1:8) {
  for (j in 2:13) {
    dat.temp <- iliiso.B[seasons[[i]], j]
    if (!all(is.na(dat.temp))) {
      ars.b <- c(ars.b, sum(dat.temp, na.rm = TRUE))
    }
  }
}
# several over 50, including a couple near and over 100,000
# over: CZ/HU/NL/PL/SK 17-18
# 86+: AT/IT 17-18, FR 15-16 (~86)
# scalings for these: mostly >2 rule works, but this includes DE and excludes IT
# can do >2x next largest, which gets rid of DE; but excludes AT/IT (~1.8-1.9x) - 1.5x could work? or even 1.75x
# after fix: max is 75000

for (i in 1:12) {
  scales <- c()
  for (j in c(1, 3, 5:8)) {
    scales <- c(scales, scalings.new[[3]][[j]][i])
  }
  print(names(iliiso.B)[i + 1])
  # print(max(scales, na.rm = T) / min(scales, na.rm = T))
  max.scale <- max(scales, na.rm = T)
  scales <- scales[scales < max.scale]
  print(max.scale / max(scales, na.rm = T))
}

# then generate a rule - if more than 2x (or some other value) higher than all other scalings for that country and (sub)type, multiply by 2/3 or something
# alternatively, if scaled AR over 80,000 or something?
# honestly, some of these are giant, so reducing even by multiplying by 0.75 or something won't get them down to a reasonable level... if they're over 80,000, maybe
# just set them to 80,000?
# tends to happen for same countries/seasons anyway, so shouldn't mess with relative intensity too much
    # and still much higher than other seasons - think 68% is next highest anywhere
# or - if >1.75x higher than next highest, set to be 1.75x next highest (or even 1.5x? - this would better avoid large ARs)
    # and watch out for France!

# Two quick notes:
    # 1: I didn't fix particularly small (> 1/1.5x next smallest) outbreaks (one outbreak each for DE/H3 and LU/B), but the issue with the larger outbreaks was that they
    #    usually went over 100,000/100,000 cases, which makes no real sense; small outbreaks don't have this issue
    # 2: Why was the maximum outbreak size always 75,000 after fixing?? Using 1.5x the next largest scaling shouldn't have resulted in uniform outbreak sizes
    #    I guess it's b/c the remaining outbreaks all have overlap between 15% and 50% ranges, so chosen scaling is the one that makes the largest outbreak 50% (and all the others less)
    #    So when we multiply this by 1.5, we get an AR of 75%
    #    But this means that, when outbreaks are "too large," we're simply setting them equal to 75% AR - shouldn't it be more nuanced than this?
    #    We could try to make it so that the amount higher than the next highest is taken into account, but this ranges so much I don't think we could find a rule where all outbreaks < 100%
    #    So essentially, the rule is that if scaling >1.5x next highest, set to 1.5x, but essentially that when we see "too large" scalings, reduce AR to 75%












