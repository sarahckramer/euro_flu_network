
### Run model to generate synthetic data for model testing ###

### Read in libraries
library("truncnorm")
library("tgp")
library("MASS")
library(reshape2)
library(plyr)
library(ggplot2)
library(viridis)

##########################################################################################

### Set seed
set.seed(10489436)

### Read in model function
source('code/SIRS_network.R')

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

### Set parameters
num_ens <- 5
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 1.5; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population

### Specify the countries for which we are performing a forecast
countries <- c('BE', 'DK', 'DE')
count.indices <- c(2, 5, 7)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

### Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Set initial conditions based on input parameters
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

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

### Store initial S0/I0 conditions:
init.states.S <- parms[1:n, ]; init.states.I  <- parms[(n + 1):(2 * n), ]

### Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

### Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[3, ] - parms[4, ])
a <- -180

beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[4, ix]) / parms[2, ix]
})

### Create vectors of initial parameters:
D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[5, ]

# ### Save files for use in python!
# # N, S0, I0, D, L, beta, airScale, airRand
# 
# write.csv(as.vector(t(N)), file = '../forecastsE/synthetic/python_comp/N.csv', row.names = FALSE)
# 
# S0.toSave <- NULL
# for (i in 1:num_ens) {
#   S0.toSave <- cbind(S0.toSave, as.vector(t(S0.temp[[i]])))
# }
# I0.toSave <- NULL
# for (i in 1:num_ens) {
#   I0.toSave <- cbind(I0.toSave, as.vector(t(I0.temp[[i]])))
# }
# write.csv(S0.toSave, file = '../forecastsE/synthetic/python_comp/S0.csv', row.names = FALSE)
# write.csv(I0.toSave, file = '../forecastsE/synthetic/python_comp/I0.csv', row.names = FALSE)
# 
# write.csv(D.temp, file = '../forecastsE/synthetic/python_comp/D.csv', row.names = FALSE)
# write.csv(L.temp, file = '../forecastsE/synthetic/python_comp/L.csv', row.names = FALSE)
# write.csv(airScale.temp, file = '../forecastsE/synthetic/python_comp/airScale.csv', row.names = FALSE)
# 
# write.csv(t(beta[[1]]), file = '../forecastsE/synthetic/python_comp/beta_1.csv')
# write.csv(t(beta[[2]]), file = '../forecastsE/synthetic/python_comp/beta_2.csv')
# write.csv(t(beta[[3]]), file = '../forecastsE/synthetic/python_comp/beta_3.csv')
# write.csv(t(beta[[4]]), file = '../forecastsE/synthetic/python_comp/beta_4.csv')
# write.csv(t(beta[[5]]), file = '../forecastsE/synthetic/python_comp/beta_5.csv')
# 
# rm(S0.toSave, I0.toSave)
# 
# for (i in 1:12) {
#   load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
#   a.temp <- a.temp.sym[countries, countries]
#   write.table(a.temp, file = paste0('../forecastsE/synthetic/python_comp/aRand', i, '.txt'), row.names = FALSE, col.names = FALSE)
# }
# rm(a.temp, a.temp.sym)

# ### Run single ens:
# ix <- 1
# S0 <- S0.temp[[ix]]
# I0 <- I0.temp[[ix]]
# D <- D.temp[ix]; L <- L.temp[ix]; airScale <- airScale.temp[ix]
# beta <- beta[[ix]]

### Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE)
})

### Calculate weekly incidence by compartment:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

### Aggregate to the country level:
newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

### Standardize results to per 100,000 population:
for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

# ### Save FULL results:
# save(newI.c.COUNT, file = 'syntheticTests/syntheticData/allRunsCOUNTS_1000_0523.RData')
# save(newI.c, file = 'syntheticTests/syntheticData/allRunsRATES_1000_0523.RData')

# Plot ALL simulations:
par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (ix in 1:num_ens) {
  newI.ens <- NULL
  for (i in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[i]][ix, ])
  }
  # newI.ens <- newI.ens[, 2:(dim(newI.ens)[2])]
  matplot(t(newI.ens), type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = ix, ylab = 'Cases / 100,000 Pop.')
}

