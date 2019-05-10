
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
num_ens <- 500
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 1.5; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 1.0; I0_up <- 50.0 # raw number

# ### Restricted parameter boundaries - try?
# D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rmn_low <- 0.8; airScale_low <- 0.75
# D_up <- 6; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
# theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
# theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
# S0_low <- 0.55; S0_up <- 0.90 # proportion of population
# I0_low <- 1.0; I0_up <- 50.0 # raw number

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:21)
# countries <- c('AT', 'BE', 'HR'); count.indices <- 1:3 # for code writing purposes

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
  # S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  # diag(S0.temp[[i]]) <- parms[1:n, i]
  # print(S0.temp[[i]])
  # set.seed(904)
  # for (j in 1:n) {
  #   S0.temp[[i]][j, (1:n)[-j]] <- rnorm(n - 1, mean = S0.temp[[i]][j, j], sd = 0.05)
  # }
  # print(S0.temp[[i]])
  
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
     rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  # for (j in 1:n) {
  #   I0.temp[[i]][j, ] <- I0.temp[[i]][j, j] * N[j, ] / sum(N[j, ])
  # }
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
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

# ### Run manually ###
# ens.mem <- 3
# S0 <- S0.temp[[ens.mem]]; I0 <- I0.temp[[ens.mem]]
# D <- D.temp[ens.mem]; L <- L.temp[ens.mem]; beta <- beta[[ens.mem]]
# 
# res <- propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
#                         S0 = S0, I0 = I0, N, D = D, L = L, beta,
#                         realdata = TRUE)
# # matplot(res$newI, type = 'b', pch = 20, cex = 0.25, lty = 1, col = viridis(3))
# 
# # newI <- cbind(rowSums(res$newI[, 1:3]), rowSums(res$newI[, 4:6]), rowSums(res$newI[, 7:9]))
# # matplot(newI, type = 'b', pch = 20, cex = 0.25, lty = 1, col = viridis(3))
# ####################

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

### Run through free simulations and check:
      # at least 19 countries/21 have 500+ cases in 3+ weeks (alt: all countries exceed 500/100,000 (onset))
      # 88% of peaks (18 countries) occur between weeks 13 and 25 (inclusive)
      # at least 75% of countries (15) have AR between 15-50% of 100,000
source('code/functions/Util.R')
ens.of.interest <- c()
for (ix in 1:num_ens) {
  newI.ens <- NULL
  for (country in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[country]][ix, ])
  }
  newI.ens <- t(newI.ens)#[, 2:(dim(newI.ens)[2])])
  
  # First: at least 19 countries have onset
  no.onset.count <- 0
  for (country in 1:n) {
    if (is.na(findOnset(newI.ens[, country], 500)$onset)) {
      no.onset.count <- no.onset.count + 1
    }
  }
  
  # Continue searching if enough outbreaks:
  if (no.onset.count <= 2) {
    # Get vectors of attack rates and peak timings:
    pts = ars = c()
    for (country in 1:n) {
      pts <- c(pts, which.max(newI.ens[, country]))
      ars <- c(ars, sum(newI.ens[, country]))
    }
    
    num.real.pt <- length(which(pts %in% c(13:25)))
    num.real.ar <- length(which(ars >= 15000 & ars <= 50000))
    
    if (num.real.pt >= 18 & num.real.ar >= 15) {
      ens.of.interest <- c(ens.of.interest, ix)
    }
  }
  
}
print(length(ens.of.interest))

# What are the parameters for these?
summary(D.temp[ens.of.interest]) # 1.8-6.3
summary(L.temp[ens.of.interest] / 365) # 1.75-10 years
summary(airScale.temp[ens.of.interest]) # 0.75-1.25 (so full range)
summary(parms[3, ens.of.interest]) # 2.0 - 3.4
summary(parms[4, ens.of.interest]) # 0.83 - 1.18
summary(init.states.S[, ens.of.interest])
select.parms <- as.data.frame(cbind(D.temp[ens.of.interest],
                                    L.temp[ens.of.interest] / 365,
                                    airScale.temp[ens.of.interest],
                                    parms[3, ens.of.interest],
                                    parms[4, ens.of.interest]))
names(select.parms) <- c('D', 'L', 'airScale', 'R0mx', 'R0mn')

plot(select.parms, pch = 20, cex = 1.2)
for (i in 1:4) {
  for (j in (i + 1):5) {
    print(names(select.parms)[c(i, j)])
    print(cor.test(select.parms[, i], select.parms[, j]))
  }
} # only D/R0mx sig
cor.test(select.parms$R0mx, select.parms$D) # p = 0.00055, cor = 0.6891197

# Plot chosen simulations:
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (ix in ens.of.interest) {
  newI.ens <- NULL
  for (i in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[i]][ix, ])
  }
  # newI.ens <- newI.ens[, 2:(dim(newI.ens)[2])]
  matplot(t(newI.ens), type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = ix, ylab = 'Cases / 100,000 Pop.')
}

# # Plot ALL simulations:
# par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (ix in 1:num_ens) {
#   newI.ens <- NULL
#   for (i in 1:n) {
#     newI.ens <- rbind(newI.ens, newI.c[[i]][ix, ])
#   }
#   # newI.ens <- newI.ens[, 2:(dim(newI.ens)[2])]
#   matplot(t(newI.ens), type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = ix, ylab = 'Cases / 100,000 Pop.')
# }

# Save these synthetic sets:
synth.runs.RATES = synth.runs.COUNTS = vector('list', length(ens.of.interest))
for (i in 1:length(synth.runs.RATES)) {
  ix <- ens.of.interest[i]
  
  newI.ens = newI.ens.count = NULL
  for (country in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[country]][ix, ])
    newI.ens.count <- rbind(newI.ens.count, newI.c.COUNT[[country]][ix, ])
  }
  
  synth.runs.RATES[[i]] <- newI.ens
  synth.runs.COUNTS[[i]] <- newI.ens.count
}
save(synth.runs.RATES, file = 'syntheticTests/syntheticData/synth_05-09_RATES1.RData')
save(synth.runs.COUNTS, file = 'syntheticTests/syntheticData/synth_05-09_COUNTS1.RData')

# Save these initial conditions so that sensitivity to I0 can be assessed:
# (Remember that this is an initial pass - might decide to do I0 in some other way)
init.states.SEL <- rbind(init.states.S[, ens.of.interest],
                         init.states.I[, ens.of.interest])
# each column is a run

save(init.states.SEL, file = 'syntheticTests/syntheticData/initStates_05-09_1.RData')
save(select.parms, file = 'syntheticTests/syntheticData/params_05-09_1.RData')











### Notes:
# low: PT, IT; high: SE, LU

# Assess by:
    # Different param ranges; travel inclusion; travel multipliers; random seeds; seed locations

### TO-DO ###
# [] Prescribe several (~20) sets of params/initial conditions, get synthetic free simulations
# [] Assess patterns in free simulations; compare with observed data
# [] Analyze patterns based on seeding: everywhere; each country; vary # seeded in each country
# [] Compare spatial patterns and synchrony to synthetic





# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/results_04-30-19.pdf',
#     width = 10, height = 10)
# dev.off()



