
### Calculate TRUE values of beta, R0, Re at each time point ###
# Change: 3

# Get countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get humidity data:
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# Narrow to desired time frame:
tm_strt <- 273; tm_end <- 636; tm_step <- 1
tm.range <- tm_strt:tm_end
tmstep <- 7

# beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + tmstep), by = tmstep)
beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + 2 * tmstep), by = tmstep) # this is how it is in model running code
beta.range <- beta.range[2:53] # since S is taken first at day 8, after one week, and not as initial S
AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)

# Read in true parameters and S:
load('syntheticTests/syntheticData/parms_toKeep_070220.RData')
load('syntheticTests/syntheticData/synth_S_toKeep_070220.RData')

# Transpose S:
for (i in 1:length(synth.s)) {
  synth.s[[i]] <- t(synth.s[[i]])
}

# Calculate true beta/R0/Re:
true.betas = true.R0 = true.Re = vector('list', 5)
for (i in 1:length(true.betas)) {
  parms.temp <- parms.outbreaks[25:29, i]
  s.temp <- synth.s[[i]]
  
  betas.temp = r0.temp = re.temp = matrix(NA, nrow = dim(s.temp)[1], ncol = dim(s.temp)[2])
  
  b <- log(parms.temp[4])
  a <- -180
  
  r0.temp <- exp(a * AHpt + b) + (parms.temp[3] - parms.temp[4])
  beta.temp <- r0.temp / parms.temp[2]
  
  for (j in 1:n) {
    re.temp[, j] <- r0.temp[, j] * (s.temp[, j] / 100000)
  }
  
  true.betas[[i]] <- beta.temp
  true.R0[[i]] <- r0.temp
  true.Re[[i]] <- re.temp
}

# Save:
true.list <- list(true.betas, true.R0, true.Re)
save(true.list, file = 'syntheticTests/syntheticData/true_betaR0Re.RData')

rm(list = ls())

### Calculate SIMULATED values of beta, R0, Re at each time point ###

# Get countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get humidity data:
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# Narrow to desired time frame:
tm_strt <- 273; tm_end <- 636; tm_step <- 1
tm.range <- tm_strt:tm_end
tmstep <- 7

# beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + tmstep), by = tmstep)
beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + 2 * tmstep), by = tmstep) # this is how it is in model running code
beta.range <- beta.range[2:53] # since S is taken first at day 8, after one week, and not as initial S
AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)

# Read in results:
o <- read.csv('syntheticTests/outputOPParams_synth_070220.csv')
oStates <- read.csv('syntheticTests/outputOP_synth_070220.csv')

oStates$country <- countries[oStates$country + 1]
oStates$country <- factor(oStates$country)

oStates$beta = oStates$R0 = oStates$Re = NA
oStates$run <- factor(oStates$run)

load('syntheticTests/syntheticData/for_synthetic_testing/synth_rates_toKeep_070220.RData')
for (i in 1:length(synth.outbreaks)) {
  print(i)
  
  for (j in levels(oStates$run)) {
    o.temp <- o[o$season == i & o$run == j, ]
    b <- log(o.temp$R0diff); a <- -180
    
    for (country in countries) {
      oStates$R0[oStates$season == i & oStates$run == j & oStates$country == country] <- exp(a * AHpt[, which(countries == country)] + b) + (o.temp$R0mx - o.temp$R0diff)
      oStates$beta[oStates$season == i & oStates$run == j & oStates$country == country] <- oStates$R0[oStates$season == i & oStates$run == j & oStates$country == country] / o.temp$D
      oStates$Re[oStates$season == i & oStates$run == j & oStates$country == country] <- oStates$R0[oStates$season == i & oStates$run == j & oStates$country == country] *
        (oStates$S[oStates$season == i & oStates$run == j & oStates$country == country] / 100000)
    }
  }
}

# Store these results:
write.csv(oStates, file = 'syntheticTests/outputOP_SYNTH_beta-R0-Re_070220.csv', row.names = FALSE)

rm(list = ls())

# ##### ISOLATED #####
# 
# ### True Values ###
# # Get countries:
# countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
# n <- length(countries)
# 
# # Get humidity data:
# ah <- read.csv('data/ah_Europe_07142019.csv')
# AH <- rbind(ah[, count.indices], ah[, count.indices])
# 
# # Narrow to desired time frame:
# tm_strt <- 273; tm_end <- 636; tm_step <- 1
# tm.range <- tm_strt:tm_end
# tmstep <- 7
# 
# beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + 2 * tmstep), by = tmstep) # this is how it is in model running code
# beta.range <- beta.range[2:53] # since S is taken first at day 8, after one week, and not as initial S
# AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)
# 
# # Read in true parameters and S:
# load('syntheticTests/syntheticData/synth_rates_ISOL.RData')
# load('syntheticTests/syntheticData/params_ISOL.RData')
# S.true <- isol.keep[[2]]
# 
# # Calculate true beta/R0/Re:
# true.betas = true.R0 = true.Re = matrix(NA, nrow = nrow(S.true), ncol = ncol(S.true))
# for (i in 1:3) {
#   parms.temp <- params.keep[i, ]
#   s.temp <- S.true[, i]
#   
#   b <- log(parms.temp[6])
#   a <- -180
#   
#   true.R0[, i] <- exp(a * AHpt[, 4] + b) + (parms.temp[5] - parms.temp[6])
#   true.betas[, i] <- true.R0[, i] / parms.temp[4]
#   true.Re[, i] = true.R0[, i] * (s.temp / 100000)
# }
# rm(parms.temp, s.temp)
# 
# # Save:
# true.list <- list(true.betas, true.R0, true.Re)
# save(true.list, file = 'syntheticTests/syntheticData/true_betaR0Re_ISOLATED.RData')
# 
# rm(list = ls())
# 
# ### Simulated Values ###
# # Get countries:
# countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
# n <- length(countries)
# 
# # Get humidity data:
# ah <- read.csv('data/ah_Europe_07142019.csv')
# AH <- rbind(ah[, count.indices], ah[, count.indices])
# 
# # Narrow to desired time frame:
# tm_strt <- 273; tm_end <- 636; tm_step <- 1
# tm.range <- tm_strt:tm_end
# tmstep <- 7
# 
# beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + 2 * tmstep), by = tmstep) # this is how it is in model running code
# beta.range <- beta.range[2:53] # since S is taken first at day 8, after one week, and not as initial S
# AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)
# 
# # Read in results:
# o <- read.csv('syntheticTests/outputOP_ISOLATED_SYNTH.csv')
# o$run <- factor(o$run)
# o$beta = o$R0 = o$Re = NA
# 
# load('syntheticTests/syntheticData/synth_rates_ISOL.RData')
# newI <- isol.keep[[1]]
# 
# for (i in 1:3) {
#   for (j in levels(o$run)) {
#     o.temp <- o[o$season == i & o$run ==j, ]
#     b <- log(o.temp$R0diff); a <- -180
#     
#     o$R0[o$season == i & o$run == j] <- exp(a * AHpt[, 4] + b) + (o.temp$R0mx - o.temp$R0diff)
#     o$beta[o$season == i & o$run == j] <- o$R0[o$season == i & o$run == j] / o.temp$D
#     o$Re[o$season == i & o$run == j] <- o$R0[o$season == i & o$run == j] * (o$S[o$season == i & o$run == j] / 100000)
#   }
# }
# 
# # Store these results:
# write.csv(o, file = 'syntheticTests/outputOP_SYNTH_beta-R0-Re_ISOLATED.csv', row.names = FALSE)
# 
# rm(list = ls())






