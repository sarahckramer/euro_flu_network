
### Look at how simulated outbreaks change based on seeding scheme

# Read in model function
source('code/SIRS_network.R')

# Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

# Set parameters
num_ens <- 20 # with 21 countries, this makes it easier to remember which dimension is which; can change easily
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Specify the country for which we are performing a forecast
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

# Read in parameter values and initial S0 values:
load('syntheticTests/syntheticData/params_05-09_1.RData')
load('syntheticTests/syntheticData/initStates_05-09_1.RData')

parms <- rbind(init.states.SEL, t(select.parms))
parms[22:42, ] <- 0
parms <- parms[, 1:num_ens]
rm(init.states.SEL, select.parms)

S0.temp <- vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] <- matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
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

# For each set of parameters/init S, loop through and seed:
    # Each country (1, 10, 50; dist or home-home only)
    # 100 sets of random countries (2, 3, 5 countries, draw I0 from dist)

# single.loc.res <- vector('list', num_ens * n * 2 * 2)
single.loc.res <- vector('list', n * 3 * 2)
res.index <- 1

ix <- 1 # use just a single value for now to work on code
pdf(paste0('syntheticTests/analyze/outputs/I0_sens_single_', ix, '.pdf'),
    width = 10, height = 10)

S0ix <- S0.temp[[ix]]; D <- D.temp[ix]; L <- L.temp[ix]; airScale <- airScale.temp[ix]; beta.ix <- beta[[ix]]

par(mfrow = c(2, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (start.country in 1:n) {
  for (seed.count in c(1, 10, 50)) {
    for (distrib in c(T, F)) {
      I0 <- matrix(0, nrow = n, ncol = n)
      
      if (distrib) {
        I0[start.country, start.country] <- seed.count
        I0 <- sweep(N / rowSums(N), 1, diag(I0), '*')
      } else {
        I0[start.country, start.country] <- seed.count
      }
      
      res <- propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                              S0 = S0ix, I0 = I0, N, D = D, L = L,
                              beta = beta.ix, airScale = airScale,
                              realdata = TRUE)
      nt <- floor((length(tm_strt:tm_end) + 1) / 7)
      newI <- lapply(1:(n ** 2), function(ix) {
        res$newI[tmstep * (1:nt) + 1, ix] - res$newI[tmstep * (0:(nt - 1)) + 1, ix]
      })
      newI.c <- vector('list', n)
      for (country in 1:n) {
        newI.c[[country]] <- Reduce('+', newI[(country * n - n + 1):(country * n)])
        newI.c[[country]] <- (newI.c[[country]] / pop.size$pop[country]) * 100000
      }
      newI.ens <- NULL
      for (country in 1:n) {
        newI.ens <- rbind(newI.ens, newI.c[[country]])
      }
      
      # matplot(t(newI.ens), type = 'b', lty = 1, pch = 20, col = viridis(n),
      #         main = paste(ix, start.country, seed.count, sep = '_'))
      
      single.loc.res[[res.index]] <- newI.ens
      res.index <- res.index + 1
    }
  }
}
# preliminary obs: not very sensitive to start conditions, but 50 might be too high - moves
# some outbreaks forward substantially; dist seems to make little difference

# QUESTION: To better explore, should I be setting all countries to have equal S0?
# Set dist to T?

# Quick evaluation:
source('code/functions/Util.R')
countries.no.onset = ar.dist = pt.dist = ot.dist = vector('list', length(single.loc.res))
for (i in 1:length(single.loc.res)) {
  dat.temp <- single.loc.res[[i]]
  
  noOn.temp = ar.temp = pt.temp = ot.temp = c()
  for (j in 1:length(countries)) {
    if (is.na(findOnset(dat.temp[j, ], 500)$onset)) {
      noOn.temp <- c(noOn.temp, countries[j])
      ot.temp <- c(ot.temp, NA)
    } else {
      ot.temp <- c(ot.temp, findOnset(dat.temp[j, ], 500)$onset)
    }
    
    ar.temp <- c(ar.temp, sum(dat.temp[j, ]))
    pt.temp <- c(pt.temp, which.max(dat.temp[j, ]) + 39)
  }
  
  countries.no.onset[[i]] <- noOn.temp
  ar.dist[[i]] <- ar.temp
  pt.dist[[i]] <- pt.temp
  ot.dist[[i]] <- ot.temp
}

# Create reference vectors of start countries, start I, distrib:
start.counts <- c()
for (i in 1:length(countries)) {
  start.counts <- c(start.counts, rep(countries[i], 4))
}
I0.inds <- rep(c(1, 1, 10, 10), 21)
distrib.inds <- rep(c(T, F), 21 * 2)

# Look at which/when countries w/ no onset:
for (i in 1:length(countries.no.onset)) {
  if (length(countries.no.onset[[i]]) > 0) {
    print(paste(start.counts[i], I0.inds[i], distrib.inds[i], sep = '_'))
    print(countries.no.onset[[i]]); print('')
  }
}
# distrib doesn't matter, but start of 10 can allow onsets where otherwise none

# Look at median AR/PT / PT ranges:
ar.meds <- unlist(lapply(1:length(ar.dist), function(ix) {
  median(ar.dist[[ix]])
}))
pt.meds <- unlist(lapply(1:length(pt.dist), function(ix) {
  median(pt.dist[[ix]])
}))
print(max(ar.meds) / min(ar.meds))
print(max(pt.meds) / min(pt.meds))

pt.ranges <- unlist(lapply(1:length(pt.dist), function(ix) {
  max(pt.dist[[ix]]) - min(pt.dist[[ix]])
}))
print(summary(pt.ranges))

df.sum <- as.data.frame(cbind(start.counts, I0.inds, distrib.inds, ar.meds, pt.meds, pt.ranges))
for (i in c(2, 4:6)) {
  df.sum[, i] <- as.numeric(as.character(df.sum[, i]))
}

par(mfrow = c(3, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(ar.meds ~ start.counts, data = df.sum, col = 'lightblue2')
boxplot(pt.meds ~ start.counts, data = df.sum, col = 'lightblue2')
boxplot(pt.ranges ~ start.counts, data = df.sum, col = 'lightblue2')

boxplot(ar.meds ~ I0.inds, data = df.sum, col = 'lightblue2')
boxplot(pt.meds ~ I0.inds, data = df.sum, col = 'lightblue2')
boxplot(pt.ranges ~ I0.inds, data = df.sum, col = 'lightblue2')

boxplot(ar.meds ~ distrib.inds, data = df.sum, col = 'lightblue2')
boxplot(pt.meds ~ distrib.inds, data = df.sum, col = 'lightblue2')
boxplot(pt.ranges ~ distrib.inds, data = df.sum, col = 'lightblue2')

# peaks realistically mostly occur by wk64
# pt ranges should be 8-11ish

# start country can widely impact all of these measures
# 10 start leads to larger, earlier outbreaks
# no change by distrib.inds

# Look at AR/PT/OT by country:




# Try:
# [x] Seed only one random country (or, same random pull of init conditions, run with each country as a start location)
    # [x] Seed 1, 10, 50
    # [x] Seed based on pop dist, or in home-home compartment only
# [] Select 2, 3, 5 countries to randomly seed

# Evaluate based on:
# [x] Number of countries w/ no onset
# [x] Median/range of AR
# [] Median/range of PT
# [] AR/PT/OT by country/longitude
# [] Synchrony



