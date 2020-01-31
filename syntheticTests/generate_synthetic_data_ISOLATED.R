
### Run model to generate synthetic data for model testing ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(gridExtra); library(viridis)

##########################################################################################

### Set seed
set.seed(10489436)
# set.seed(10489437)
# set.seed(10489438)

### Read in model function
source('cluster/SIRS_indiv.R')

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

### Set parameters
num_ens <- 1000
tm_strt <- 273; tm_end <- 636; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 2; L_low <- 3*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25

S0_low <- 0.40; S0_up <- 0.90 # then again, we want scalings that will work when we use the ranges we're using for fitting, right?
I0_low <- 0; I0_up <- 0.00005

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up)

### Specify the countries for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set country populations
N <- 1e5

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Set initial conditions based on input parameters
param.bound <- cbind(c(S0_low, I0_low, theta_low), c(S0_up, I0_up, theta_up))
parms <- t(lhs(num_ens, param.bound))

### Read in functions to run model/format results:
source('cluster/functions/synth_functions.R')
source('cluster/functions/Util.R')

### Run model!
AH <- AH[, c(4, 7, 11)] # FR, IT, SK

beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), 3)
a <- -180
b <- log(parms[6, ])
D.temp <- parms[4, ]; L.temp <- parms[3, ]
S0.temp <- parms[1, ] * N; I0.temp <- parms[2, ] * N

res.list = S.list = vector('list', 3)
for (i in 1:3) {
  AH.temp <- AHpt[, i]
  ones1 <- matrix(1, length(AH.temp), 1) # matrix of 1 to make the dimension match
  ones2 <- matrix(1, 1, num_ens)
  AH.temp <- AH.temp %*% ones2
  BT1 <- exp(a * AH.temp + b) + ones1 %*% (parms[5, ] - parms[6, ])
  beta.temp <- BT1 / (ones1 %*% parms[4, ])
  
  m <- propagateParSIR(tm_strt = tm_strt, tm_end = tm_end, tm_step = dt,
                       S0 = S0.temp, I0 = I0.temp, N, D = D.temp, L = L.temp,
                       beta.temp, realdata = TRUE)
  
  nt <- floor((length(tm_strt:tm_end) + 1) / 7)
  newI <- t(m$newI[tmstep * (1:nt) + 1, ] - m$newI[tmstep * (0:(nt - 1)) + 1, ])
  S <- t(m$S[tmstep * (1:nt) + 1, ])
  
  res.list[[i]] <- newI
  S.list[[i]] <- S
}

# Get results:
res.rates <- res.list[[1]]; s.rates <- S.list[[1]]
which.onsets = which.real = c()
for (i in 1:num_ens) {
  dat.temp <- res.rates[i, ]
  
  if (!is.na(findOnset(dat.temp, 500)$onset)) {
    which.onsets <- c(which.onsets, i)
    pt.temp <- which.max(dat.temp)
    
    if (pt.temp <= 25 & pt.temp >= 13) {
      which.real <- c(which.real, i)
    }
  }
  
}
length(which.onsets)
length(which.real)
# fewest realistic for FR, but similar numbers for all

# Try to choose a few (3) for fitting:
newI.real <- newI[which.real, ]
S.real <- S[which.real, ]
parms.real <- parms[, which.real]
# for similar to network, want D 4-6, R0mx 2-2.6, R0diff 0.4-1.0

parms.df <- as.data.frame(cbind(1:length(which.real), t(parms.real)))
names(parms.df) <- c('run', 'S0', 'I0', 'L', 'D', 'R0mx', 'R0diff')

parms.df <- parms.df[parms.df$R0diff > 0.45 & parms.df$D > 4, ]
# 254 = 2; 301 = 3; 169 = 4
to.keep <- c(254, 301, 169)

# Save "data":
newI.keep <- t(newI.real[to.keep, ])
S.keep <- t(S.real[to.keep, ])
params.keep <- t(parms.real[, to.keep])

isol.keep <- list(newI.keep, S.keep)
save(isol.keep, file = 'syntheticTests/syntheticData/synth_rates_ISOL.RData')
save(params.keep, file = 'syntheticTests/syntheticData/params_ISOL.RData')

# Add noise:
load('syntheticTests/syntheticData/synth_rates_ISOL.RData')
newI.keep <- isol.keep[[1]]
oev_base <- 1e5; oev_denom <- 10.0
source('cluster/functions/calc_obsvars.R')

obs_vars <- matrix(NA, nrow = 52, ncol = 3)
for (i in 1:3) {
  obs_vars[, i] <- calc_obsvars(as.matrix(newI.keep[, i]), oev_base, oev_denom)
}

par(mfrow = c(2, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
newI.orig <- newI.keep
matplot(newI.orig, pch = 20, col = viridis(3), cex = 1.0, type = 'b', lty = 1)

for (i in 1:3) {
  for (j in 1:52) {
    newI.keep[j, i] <- rnorm(1, mean = newI.keep[j, i], sd = sqrt(obs_vars[j, i]))
  }
}
newI.keep[newI.keep < 0] <- 0

matplot(newI.keep, pch = 20, col = viridis(3), cex = 1.0, type = 'b', lty = 1)

save(newI.keep, file = 'syntheticTests/syntheticData/synth_rates_ISOL_error.RData')




