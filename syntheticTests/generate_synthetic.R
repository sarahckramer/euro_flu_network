
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
num_ens <- 100
tm_strt <- 270; tm_end <- 570; tm_step <- 1#; t <- 1 # 270 is early September
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 1.5; L_low <- 1*365; Rmx_low <- 1.3; Rmn_low <- 0.8;
D_up <- 7; L_up <- 10*365; Rmx_up <- 4; Rmn_up <- 1.2;
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)
S0_low <- 0.55; S0_up <- 0.80
I0_low <- 0.08 / 100000; I0_up <- 250 / 100000

### Specify the country for which we are performing a forecast
countries <- c('BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IS', 'IT',
               'LU', 'NL', 'PL', 'PT', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:19) # just those countries w/ train data, too

countries <- c('BE', 'FR')
count.indices <- c(1, 5) # just those countries w/ train data, too

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

### Read in random train data
load('formatTravelData/formattedData/train_05-07.RData')
t.rand <- t.rand[countries, countries]

### Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Set initial conditions based on input parameters
param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
                     c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))
So <- t(lhs(num_ens, param.bound))

S0.indices <- (1:((dim(So)[1] - 4) / 2)) # where S0 are stored
I0.indices <- S0.indices + S0.indices[length(S0.indices)] # where I0 are stored
param.indices <- (dim(So)[1] - 3):(dim(So)[1]) # where the epi parameters are stored

# ### Draw initial parameters from phi
# # if(!exists("phi")) {
# #   phi <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/data/phi.csv', header=F)
# #   params <- phi[,1:4]
# #   susceps = infects = NULL
# #   for(i in 5:31) {
# #     susceps <- append(susceps, phi[,i])
# #     infects <- append(infects, phi[,27+i])
# #   }
# # }
# 
# So <- matrix(0, n ** 2 * 2 + 4, num_ens) # last 4 rows are parameters
#
# rnd.S <- ceiling(1e4 * runif(num_ens))
# rnd <- cbind(t(do.call('rbind', replicate(n ** 2, rnd.S, simplify = FALSE))),
#              t(do.call('rbind', replicate(n ** 2, ceiling(27 * 1e4 * runif(num_ens)),
#                                           simplify = FALSE))),
#              ceiling(1e4 * runif(num_ens))) # determines which values to choose from phi
# # rnd[, S0.indices] <- rnd[, 1] # same S0 in all places
# 
# So[S0.indices, ] <- unlist(lapply(1:(length(S0.indices) * num_ens), function(ix) {
#   rnorm(1, mean = (susceps[t(rnd[, S0.indices])] / 5)[ix], sd = 5000)
#   })) # goes through columns, then rows; but so does the place it's being assigned to
# So[I0.indices, ] <- infects[t(rnd[, I0.indices])] / 5
# So[param.indices, ] <- t(params[t(rnd[, dim(rnd)[2]]), 1:4])
# So[param.indices[1:2], ] <- So[param.indices[1:2], ] * 365 # these should stay the same
# # Note: These aren't yet standardized by population size; these need to be turned into percentages of population in each compartment
# # So: (x / 100000) * N[i, j]

### Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(So[param.indices[3], ] - So[param.indices[4], ])
a <- -180

beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + So[param.indices[4], ix]) / So[param.indices[2], ix]
})

### Draw S0 and I0 from So
S0.temp <- lapply(1:num_ens, function(ix) {
  N * (matrix(So[S0.indices, ix], ncol = n, byrow = TRUE))# / 100000) # % of population susceptible, times # in population
})
I0.temp <- lapply(1:num_ens, function(ix) {
  N * (matrix(So[I0.indices, ix], ncol = n, byrow = TRUE))# / 100000)
})
D.temp <- So[param.indices[2], ]; L.temp <- So[param.indices[1], ]

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
                   realdata = TRUE)
})
# save(m, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/synthetic_full_100ens_04-30.RData')

# ### Save parameter ensembles
# save(S0.temp, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/all_S0.RData')
# save(I0.temp, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/all_I0.RData')
# all.params <- list(So[param.indices[3], ], So[param.indices[4], ], D.temp, L.temp)#, So[1, ] / 1000)
# save(all.params, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/all_params.RData')

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
# save(newI.c, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/by_country_COUNTS.RData')

### Standardize results to per 100,000 population:
for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}
# save(newI.c, file = '/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/04-30-19/by_country_RATES.RData')

# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/synthetic/results_04-30-19.pdf',
#     width = 10, height = 10)
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (ix in 1:num_ens) {
  newI.ens <- NULL
  for (i in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[i]][ix, ])
  }
  newI.ens <- newI.ens[, 2:(dim(newI.ens)[2])]
  matplot(t(newI.ens), type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = ix, ylab = 'Cases / 100,000 Pop.')
}
# dev.off()

# print(summary(So[S0.indices, ]))
# print(So[param.indices, ])

# to.keep <- c(2, 4, 9:11, 16:17, 19, 23, 27, 30, 38:40, 45, 51, 54:55, 57:58, 61, 64, 66, 71, 78:80, 88:89, 93, 98)
# print(summary(So[S0.indices, to.keep]))
# print(So[param.indices, to.keep])
# 
# # to.keep <- c(10, 27, 30, 38, 40, 78, 80, 88)
# to.keep <- c(10, 38, 40, 88)
# par(mfrow = c(2, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (ix in to.keep) {
#   newI.ens <- NULL
#   for (i in 1:n) {
#     newI.ens <- rbind(newI.ens, newI.c[[i]][ix, ])
#   }
#   newI.ens <- newI.ens[, 2:(dim(newI.ens)[2])]
#   matplot(t(newI.ens), type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = ix, ylab = 'Cases / 100,000 Pop.')
# }
# print(summary(So[S0.indices, to.keep])) # 76.095%, 87.951%, 74.703%, 72.173%
# print(So[param.indices, to.keep])
# # [1,] 3279.281923 2630.340714 1940.392595 2847.1341948
# # [2,]    4.462900    6.385129    4.275509    3.8597335
# # [3,]    3.174249    2.005894    2.846134    3.5250545
# # [4,]    1.291414    1.196737    1.112553    0.9070985

### Notes:
# low: PT, IT; high: SE, LU
# PT has early rise in humidity
# low AR in PT until S0 >~65000; 
# random I0 seeding seems okay, but AR highly dependent on S0
# usually 0.5-0.75, and highly sig
# and <40% usually doesn't lead to much (except in SE/LU)
# so do same S0 % in all places
# should I0 at least be the same in all compartments within a country? I don't think so, b/c travelers could have higher rates
# Anything else shown tomorrow is VERY preliminary; need to add IS and AT, for example
# Also using yearly average commuting; but could use by year
# Ensemble members with outbreaks in all/most countries had S0 > 55% and R0mx > 1.6, but otherwise spanned full ranges



