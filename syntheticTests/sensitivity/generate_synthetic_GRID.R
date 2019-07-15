
# Basically this, but add section to look at how relation to first outbreak timing changes with parameter values and model type


# Setup:
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(viridis)
source('code/SIRS_network.R')
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

# Set parameter info:
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

# Countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:21)

# Get collection of params that produce "full" outbreaks:
load('syntheticTests/syntheticData/params_06-26.RData')
parms.USE <- select.parms[c(3, 5, 13), ]
rm(select.parms)

load('syntheticTests/syntheticData/initStates_06-26.RData')
init.states.USE <- init.states.SEL[, c(3, 5, 13)]
rm(init.states.SEL)

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### First: Run with AH-forcing ###

# Setup:
set.seed(10489436)

# Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

# Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

# Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# Set initial conditions and params:
parms <- rbind(init.states.USE, t(parms.USE))

# Repeat 6 times!:
count <- 1
while (count < 7) {
  parms <- cbind(parms, parms[, 1:3]); count <- count + 1
}

# Change R0mx and airScale where desired:
parms[46, 4:6] <- 2.5
parms[46, 7:9] <- 2.75
parms[46, 10:12] <- 3.0
parms[45, 13:15] <- 0.75
parms[45, 16:18] <- 1.00
parms[45, 19:21] <- 1.25

# Set initial S/I for all compartments:
num_ens <- dim(parms)[2]
S0.temp = I0.temp = vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
  I0.temp[[i]] <- I0.temp[[i]] * N
}

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[4, ] - parms[5, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[5, ix]) / parms[1, ix]
})

# Create vectors of initial parameters:
D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365; airScale.temp <- parms[3, ]

# Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE,
                   prohibAir = FALSE)
})

# Format results:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)

# Plot out results by param?
# 1/R0mx: 4, 7, 10; 2/R0mx: 5, 8, 11; 3/R0mx: 6, 9, 12; 1/aS: 13, 16, 19; 2/aS: 14, 17, 20; 3/aS: 15, 18, 21

pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_Base.pdf', height = 10, width = 8)

par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')

matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')

matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')

matplot(newI.ens[[13]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75')
matplot(newI.ens[[16]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[19]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')

matplot(newI.ens[[14]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75')
matplot(newI.ens[[17]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[20]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')

matplot(newI.ens[[15]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75')
matplot(newI.ens[[18]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[21]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')
# so R0mx increases synchrony, but little impact of airScale (can see very slight changes in some countries, but not like increased synchrony in Sen's model)

dev.off()

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### Same, but no AH-forcing ###

# Setup:
set.seed(10489436)

# Set initial conditions and params:
parms <- rbind(init.states.USE, t(parms.USE))

# Repeat 6 times!:
count <- 1
while (count < 7) {
  parms <- cbind(parms, parms[, 1:3]); count <- count + 1
}

# Change R0mx and airScale where desired:
parms[46, 4:6] <- 1.4
parms[46, 7:9] <- 1.6
parms[46, 10:12] <- 2.0
parms[45, 13:15] <- 0.75
parms[45, 16:18] <- 1.00
parms[45, 19:21] <- 1.25
parms[46, 13:21] <- parms[46, 13:21] - 0.9

# Set initial S/I for all compartments:
num_ens <- dim(parms)[2]
S0.temp = I0.temp = vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
  I0.temp[[i]] <- I0.temp[[i]] * N
}

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
beta <- lapply(1:num_ens, function(ix) {
  matrix(parms[4, ix] / parms[1, ix], nrow = length(beta.range), ncol = n)
})

# Create vectors of initial parameters:
D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365; airScale.temp <- parms[3, ]

# Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE,
                   prohibAir = FALSE)
})

# Format results:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)

pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noAH.pdf', height = 10, width = 8)

par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

matplot(newI.ens[[13]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
matplot(newI.ens[[16]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[19]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')

matplot(newI.ens[[14]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
matplot(newI.ens[[17]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[20]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')

matplot(newI.ens[[15]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
matplot(newI.ens[[18]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
matplot(newI.ens[[21]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')
# so R0mx increases synchrony, but little impact of airScale (can see very slight changes in some countries, but not like increased synchrony in Sen's model)

dev.off()

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### AH-forcing, but no travel ###

# Setup:
set.seed(10489436)

# Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]
t.comm[t.comm > 0] <- 0

# Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

# Set initial conditions and params:
parms <- rbind(init.states.USE, t(parms.USE))

# Repeat 4 times!:
count <- 1
while (count < 4) {
  parms <- cbind(parms, parms[, 1:3]); count <- count + 1
}

# Change R0mx and airScale where desired:
parms[46, 4:6] <- 2.5
parms[46, 7:9] <- 2.75
parms[46, 10:12] <- 3.0
# parms[45, 13:15] <- 0.75
# parms[45, 16:18] <- 1.00
# parms[45, 19:21] <- 1.25

# Set initial S/I for all compartments:
num_ens <- dim(parms)[2]
S0.temp = I0.temp = vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
  I0.temp[[i]] <- I0.temp[[i]] * N
}

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[4, ] - parms[5, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[5, ix]) / parms[1, ix]
})

# Create vectors of initial parameters:
D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365

# Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = 0, realdata = TRUE, prohibAir = TRUE)
})

# Format results:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)


pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noTravel.pdf', height = 10, width = 8)

par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')

matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')

matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')
# these certainly look a lot "ropier" than with commuting - less synchrony with later peaks; so commuting seems to play a role, but not air travel as much; that agrees with literature, though

dev.off()

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### No AH-forcing, no travel ###
# Here, dynamics should be due purely to start conditions

# Setup:
set.seed(10489436)

# Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]
t.comm[t.comm > 0] <- 0

# Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

# Set initial conditions and params:
parms <- rbind(init.states.USE, t(parms.USE))

# Repeat 4 times!:
count <- 1
while (count < 4) {
  parms <- cbind(parms, parms[, 1:3]); count <- count + 1
}

# Change R0mx and airScale where desired:
parms[46, 4:6] <- 1.4
parms[46, 7:9] <- 1.6
parms[46, 10:12] <- 2.0
# parms[45, 13:15] <- 0.75
# parms[45, 16:18] <- 1.00
# parms[45, 19:21] <- 1.25

# Set initial S/I for all compartments:
num_ens <- dim(parms)[2]
S0.temp = I0.temp = vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
  I0.temp[[i]] <- I0.temp[[i]] * N
}

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
beta <- lapply(1:num_ens, function(ix) {
  matrix(parms[4, ix] / parms[1, ix], nrow = length(beta.range), ncol = n)
})

# Create vectors of initial parameters:
D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365

# Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = 0, realdata = TRUE, prohibAir = TRUE)
})

# Format results:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)

pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noAH_noTravel.pdf', height = 10, width = 8)

par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')

dev.off()


