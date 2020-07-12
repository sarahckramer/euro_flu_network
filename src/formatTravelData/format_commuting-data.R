### Format commuting data (not publicly available) ###
library(tgp)

# Ensure folder for formatted data exists:
if (!dir.exists('src/formatTravelData/formattedData/')) {
  dir.create('src/formatTravelData/formattedData/')
}

# Read in and format data:
c1 <- read.csv('src/formatTravelData/rawData/ESTA45672_190215.csv')

c1 <- c1[, c(1:2, 4, 6:8)] # remove unnecessary columns
c1$VALUE <- c1$VALUE * 1000 # convert to raw numbers

countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

c1 <- c1[c1$COUNTRY %in% countries, ]; c1 <- c1[c1$COUNTRYW %in% countries, ]
c1$COUNTRY <- factor(c1$COUNTRY); c1$COUNTRYW <- factor(c1$COUNTRYW)

# Remove where data supressed/below threshold a:
c1 <- c1[!is.na(c1$VALUE), ]

# Get yearly matrices:
c1$YEAR <- factor(c1$YEAR)
comm.by.year <- vector('list', 8)
for (ix in levels(c1$YEAR)) {
  comm.temp <- matrix(NA, nrow = 12, ncol = 12)
  for (i in 1:length(countries)) {
    for (j in 1:length(countries)) {
      if (length(c1$VALUE[c1$COUNTRY == countries[i] & c1$COUNTRYW == countries[j] & c1$YEAR == ix]) > 0) {
        comm.temp[i, j] <- c1$VALUE[c1$COUNTRY == countries[i] & c1$COUNTRYW == countries[j] & c1$YEAR == ix]
      }
    }
  }
  comm.by.year[[which(levels(c1$YEAR) == ix)]] <- comm.temp
}

# Where is 'b' a problem?:
unique(c1[c1$FLAG_BREAK == 'b', c('COUNTRY', 'YEAR')])
# BE 2011, 2017; CZ 2011; DE 2010, 2011; FR 2014; LU 2015; PL 2010; SK 2011 

# Get seasonal matrices:
comm.by.seas = vector('list', 8)
for (ix in 1:7) {
  comm.temp <- matrix(NA, nrow = 12, ncol = 12)
  
  for (i in 1:12) {
    for (j in 1:12) {
      
      vals.temp <- c(comm.by.year[[ix]][i, j], comm.by.year[[ix + 1]][i, j])
      if (length(vals.temp[!is.na(vals.temp)]) == 2) { # if neither are NA
        comm.temp[i, j] <- sum(vals.temp, na.rm = TRUE) / 2
      } else if (length(vals.temp[!is.na(vals.temp)]) == 1) {
        comm.temp[i, j] <- sum(vals.temp, na.rm = TRUE) # don't divide by two - save value as the only value with data
      } # else {
      #   print(length(vals.temp[!is.na(vals.temp)])) # should be 0
      # }
      
    }
  }
  
  comm.by.seas[[ix]] <- comm.temp
}
comm.by.seas[[8]] <- comm.by.year[[8]]
for (ix in 1:8) {
  rownames(comm.by.seas[[ix]]) <- countries
  colnames(comm.by.seas[[ix]]) <- countries
}

# Get threshold a:
threshold.a.list <- list(3000, 2000, 500, 5500, 5000, 2600, 1500, 500, 1500, 5000, 2000, 1000)
threshold.a.list[[4]] <- c(threshold.a.list[[4]], 500)
threshold.a.vec <- c(3000, 2000, 500, 5500, 5000, 2600, 1500, 500, 1500, 5000, 2000, 1000)

# Want to make 5 sets of 300 "ensemble members" of commuting ranges
# Are these involved in LHS of params/states? I don't think we need to do that
# Are they drawn using LHS themselves? That probably makes sense

# Make sure that diagonals aren't NAs and don't get drawn:
for (ix in 1:8) {
  diag(comm.by.seas[[ix]]) <- 0
}
# save(comm.by.seas, file = 'src/formatTravelData/formattedData/comm_mat_by_season_01-27.RData')

# FILLING ALL NAs:
# First determine how many draws needed, and what the lower and upper bounds are:
upper.bounds <- c()
for (ix in 1:8) {
  get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
  
  if (ix == 3) { # time to change FR threshold
    threshold.a.vec[4] <- 500
  }
  
  upper.bounds <- c(upper.bounds, threshold.a.vec[get.na[, 1]])
}

# Now draw 300, 5 times:
set.seed(1048993542)

comm.by.seas.ORIG <- comm.by.seas
comm.bound <- cbind(rep(0, length(upper.bounds)), upper.bounds)

fill.na1 <- t(lhs(300, comm.bound))
comm.list1 <- vector('list', 300)
for (i in 1:300) {
  comm.by.seas <- comm.by.seas.ORIG
  fill.vals <- fill.na1[, i]
  for (ix in 1:8) {
    get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
    comm.by.seas[[ix]][get.na] <- fill.vals[1:dim(get.na)[1]]
    if (length(fill.vals) > dim(get.na)[1]) {
      fill.vals <- fill.vals[(dim(get.na)[1] + 1):length(fill.vals)]
    } else if (length(fill.vals) == dim(get.na)[1]) {
      fill.vals <- c()
    } else {
      print('Error.')
    }
  }
  #check:
  if (length(fill.vals) != 0) {
    print('Error2.')
  }
  comm.list1[[i]] <- comm.by.seas
}
save(comm.list1, file = 'src/formatTravelData/formattedData/comm_mat1.RData')
rm(fill.na1, comm.list1)

fill.na2 <- t(lhs(300, comm.bound))
comm.list2 <- vector('list', 300)
for (i in 1:300) {
  comm.by.seas <- comm.by.seas.ORIG
  fill.vals <- fill.na2[, i]
  for (ix in 1:8) {
    get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
    comm.by.seas[[ix]][get.na] <- fill.vals[1:dim(get.na)[1]]
    if (length(fill.vals) > dim(get.na)[1]) {
      fill.vals <- fill.vals[(dim(get.na)[1] + 1):length(fill.vals)]
    } else if (length(fill.vals) == dim(get.na)[1]) {
      fill.vals <- c()
    } else {
      print('Error.')
    }
  }
  #check:
  if (length(fill.vals) != 0) {
    print('Error2.')
  }
  comm.list2[[i]] <- comm.by.seas
}
save(comm.list2, file = 'src/formatTravelData/formattedData/comm_mat2.RData')
rm(fill.na2, comm.list2)

fill.na3 <- t(lhs(300, comm.bound))
comm.list3 <- vector('list', 300)
for (i in 1:300) {
  comm.by.seas <- comm.by.seas.ORIG
  fill.vals <- fill.na3[, i]
  for (ix in 1:8) {
    get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
    comm.by.seas[[ix]][get.na] <- fill.vals[1:dim(get.na)[1]]
    if (length(fill.vals) > dim(get.na)[1]) {
      fill.vals <- fill.vals[(dim(get.na)[1] + 1):length(fill.vals)]
    } else if (length(fill.vals) == dim(get.na)[1]) {
      fill.vals <- c()
    } else {
      print('Error.')
    }
  }
  #check:
  if (length(fill.vals) != 0) {
    print('Error2.')
  }
  comm.list3[[i]] <- comm.by.seas
}
save(comm.list3, file = 'src/formatTravelData/formattedData/comm_mat3.RData')
rm(fill.na3, comm.list3)

fill.na4 <- t(lhs(300, comm.bound))
comm.list4 <- vector('list', 300)
for (i in 1:300) {
  comm.by.seas <- comm.by.seas.ORIG
  fill.vals <- fill.na4[, i]
  for (ix in 1:8) {
    get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
    comm.by.seas[[ix]][get.na] <- fill.vals[1:dim(get.na)[1]]
    if (length(fill.vals) > dim(get.na)[1]) {
      fill.vals <- fill.vals[(dim(get.na)[1] + 1):length(fill.vals)]
    } else if (length(fill.vals) == dim(get.na)[1]) {
      fill.vals <- c()
    } else {
      print('Error.')
    }
  }
  #check:
  if (length(fill.vals) != 0) {
    print('Error2.')
  }
  comm.list4[[i]] <- comm.by.seas
}
save(comm.list4, file = 'src/formatTravelData/formattedData/comm_mat4.RData')
rm(fill.na4, comm.list4)

fill.na5 <- t(lhs(300, comm.bound))
comm.list5 <- vector('list', 300)
for (i in 1:300) {
  comm.by.seas <- comm.by.seas.ORIG
  fill.vals <- fill.na5[, i]
  for (ix in 1:8) {
    get.na <- which(is.na(comm.by.seas[[ix]]), arr.ind = TRUE)
    comm.by.seas[[ix]][get.na] <- fill.vals[1:dim(get.na)[1]]
    if (length(fill.vals) > dim(get.na)[1]) {
      fill.vals <- fill.vals[(dim(get.na)[1] + 1):length(fill.vals)]
    } else if (length(fill.vals) == dim(get.na)[1]) {
      fill.vals <- c()
    } else {
      print('Error.')
    }
  }
  #check:
  if (length(fill.vals) != 0) {
    print('Error2.')
  }
  comm.list5[[i]] <- comm.by.seas
}
save(comm.list5, file = 'src/formatTravelData/formattedData/comm_mat5.RData')
rm(fill.na5, comm.list5)

rm(list = ls())
