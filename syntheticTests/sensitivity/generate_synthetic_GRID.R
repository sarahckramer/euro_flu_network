
### First: Run with AH-forcing ###

# Setup:
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(viridis)
set.seed(10489436)
source('code/SIRS_network.R')
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

# Set parameter info:
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

L_low <- 1*365; Rmn_low <- 0.8; L_up <- 10*365; Rmn_up <- 1.2
theta_low <- c(L_low, Rmn_low); theta_up <- c(L_up, Rmn_up)

S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population # to 0.01% or 0.1%?

# Countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:21)

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

# Set initial conditions and L/R0mn using LHS:
num_ens <- 10
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

# Repeat 27 times!:
count <- 1
while (count < 27) {
  parms <- cbind(parms, parms[, 1:10]); count <- count + 1
}

# Add in combos of D, R0mx, and airScale:
parms <- rbind(parms, rbind(c(rep(2.5, 90), rep(4.25, 90), rep (6.0, 90)), rep(c(rep(1.5, 30), rep(2.0, 30), rep(2.5, 30)), 3), rep(c(rep(0.75, 10), rep(1.00, 10), rep(1.25, 10)), 9)))

# Set initial S/I for all compartments:
num_ens <- num_ens * 27
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

# Store initial S0/I0 conditions:
init.states.S <- parms[1:n, ]; init.states.I  <- parms[(n + 1):(2 * n), ]

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
print(parms[, 1:20])

# Store initial states and parameters:
save(init.states.S, file = 'syntheticTests/sensitivity/outputs_datasets/grid_initS.RData')
save(init.states.I, file = 'syntheticTests/sensitivity/outputs_datasets/grid_initI.RData')
save(parms, file = 'syntheticTests/sensitivity/outputs_datasets/grid_parms.RData')

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[4, ] - parms[2, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[2, ix]) / parms[3, ix]
})

# Create vectors of initial parameters:
D.temp <- parms[3, ]; L.temp <- parms[1, ]; airScale.temp <- parms[5, ]

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

# Save results:
newI.ens.BOTH <- newI.ens
save(newI.ens.BOTH, file = 'syntheticTests/sensitivity/outputs_datasets/grid_AH-Travel.RData')

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### Same, but no AH-forcing ###

# Setup:
set.seed(10489436)

# Set initial conditions and L/R0mn using LHS:
num_ens <- 10
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

# Repeat 27 times!:
count <- 1
while (count < 27) {
  parms <- cbind(parms, parms[, 1:10]); count <- count + 1
}

# Add in combos of D, R0, and airScale:
parms <- rbind(parms, rbind(c(rep(2.5, 90), rep(4.25, 90), rep (6.0, 90)), rep(c(rep(1.2, 30), rep(1.7, 30), rep(2.2, 30)), 3), rep(c(rep(0.75, 10), rep(1.00, 10), rep(1.25, 10)), 9)))

# Remove unnecessary parameters:
parms <- parms[c(1:43, 45:47), ]

# Set initial S/I for all compartments:
num_ens <- num_ens * 27
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
parms <- parms[(dim(parms)[1] - 3):(dim(parms)[1]), ]
print(parms[, 1:20])

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
beta <- lapply(1:num_ens, function(ix) {
  matrix(parms[3, ix] / parms[2, ix], nrow = length(beta.range), ncol = n)
})

# Create vectors of initial parameters:
D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[4, ]

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

# Save results:
newI.ens.noAH <- newI.ens
save(newI.ens.noAH, file = 'syntheticTests/sensitivity/outputs_datasets/grid_noAH_Travel.RData')

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### AH-forcing, but no travel ###

# Setup:
set.seed(10489436)
# source('code/SIRS_network.R') # HAVE TO CHANGE CODE!!

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

# Set initial conditions and L/R0mn using LHS:
num_ens <- 10
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

# Repeat 9 times!:
count <- 1
while (count < 9) {
  parms <- cbind(parms, parms[, 1:10]); count <- count + 1
}

# Add in combos of D and R0mx:
parms <- rbind(parms, rbind(c(rep(2.5, 30), rep(4.25, 30), rep (6.0, 30)), rep(c(rep(1.5, 10), rep(2.0, 10), rep(2.5, 10)), 3)))

# Set initial S/I for all compartments:
num_ens <- num_ens * 9
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
parms <- parms[(dim(parms)[1] - 3):(dim(parms)[1]), ]
print(parms[, 1:20])

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[4, ] - parms[2, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[2, ix]) / parms[3, ix]
})

# Create vectors of initial parameters:
D.temp <- parms[3, ]; L.temp <- parms[1, ]

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

# Save results:
newI.ens.noTravel <- newI.ens
save(newI.ens.noTravel, file = 'syntheticTests/sensitivity/outputs_datasets/grid_AH-noTravel.RData')

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### No AH-forcing, no travel ###
# Here, dynamics should be due purely to start conditions

# Setup:
set.seed(10489436)

# Set initial conditions and L/R0mn using LHS:
num_ens <- 10
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

# Repeat 9 times!:
count <- 1
while (count < 9) {
  parms <- cbind(parms, parms[, 1:10]); count <- count + 1
}

# Add in combos of D and R0mx:
parms <- rbind(parms, rbind(c(rep(2.5, 30), rep(4.25, 30), rep (6.0, 30)), rep(c(rep(1.2, 10), rep(1.7, 10), rep(2.2, 10)), 3)))

# Remove unnecessary parameters:
parms <- parms[c(1:43, 45:46), ]

# Set initial S/I for all compartments:
num_ens <- num_ens * 9
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
parms <- parms[(dim(parms)[1] - 2):(dim(parms)[1]), ]
print(parms[, 1:20])

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
beta <- lapply(1:num_ens, function(ix) {
  matrix(parms[3, ix] / parms[2, ix], nrow = length(beta.range), ncol = n)
})

# Create vectors of initial parameters:
D.temp <- parms[2, ]; L.temp <- parms[1, ]

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

# Save results:
newI.ens.None <- newI.ens
save(newI.ens.None, file = 'syntheticTests/sensitivity/outputs_datasets/grid_noAH-noTravel.RData')



