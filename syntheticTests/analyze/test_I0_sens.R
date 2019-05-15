
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

ix <- 10 # use just a single value for now to work on code
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
save(single.loc.res, file = paste0('syntheticTests/analyze/outputs/single_loc_', ix, '.RData'))
# preliminary obs: not very sensitive to start conditions, but 50 might be too high - moves
# some outbreaks forward substantially; dist seems to make little difference

# Quick evaluation:
# countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
#                'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
# wk_start <- 40; n <- length(countries)
# ix <- 6
# load(paste0('syntheticTests/analyze/outputs/single_loc_', ix, '.RData'))

pdf(paste0('syntheticTests/analyze/outputs/I0_sens_single_', ix, '.pdf'),
    width = 12, height = 10)

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
  start.counts <- c(start.counts, rep(countries[i], 6))
}
I0.inds <- rep(c(1, 1, 10, 10, 50, 50), 21)
distrib.inds <- rep(c(T, F), 21 * 3)

# Look at which/when countries w/ no onset:
for (i in 1:length(countries.no.onset)) {
  if (length(countries.no.onset[[i]]) > 0) {
    print(paste(start.counts[i], I0.inds[i], distrib.inds[i], sep = '_'))
    print(countries.no.onset[[i]]); print('')
  }
}
# distrib doesn't matter
# start of 10 can allow onsets where otherwise none, always onsets w/ start 50
# for run 3/4: a lot of no-onset runs, but pretty much regardless of where start is

# Look at median AR/PT / PT ranges:
ar.meds <- unlist(lapply(1:length(ar.dist), function(ix) {
  median(ar.dist[[ix]])
}))
pt.meds <- unlist(lapply(1:length(pt.dist), function(ix) {
  median(pt.dist[[ix]])
}))
print(max(ar.meds) / min(ar.meds)) # wider for 3
print(max(pt.meds) / min(pt.meds))
# in general, not much variation between runs with different start/I0/distrib

pt.ranges <- unlist(lapply(1:length(pt.dist), function(ix) {
  max(pt.dist[[ix]]) - min(pt.dist[[ix]])
}))
print(summary(pt.ranges))
# pt.ranges can be a bit wide

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
# 10 start leads to larger, earlier outbreaks; almost need 50 to make them "realistic"
# no change by distrib.inds

# Look at AR/PT/OT by country:
df.bc <- as.data.frame(cbind(rep(countries, 126), unlist(ar.dist), unlist(pt.dist), unlist(ot.dist)))
names(df.bc) <- c('c1', 'ar', 'pt', 'ot')
df.bc$ar <- as.numeric(as.character(df.bc$ar))
df.bc$pt <- as.numeric(as.character(df.bc$pt))
df.bc$ot <- as.numeric(as.character(df.bc$ot))

cStart <- c()
for (i in 1:length(countries)) {
  cStart <- c(cStart, rep(countries[i], n * 3 * 2))
}
df.bc$cStart <- cStart
df.bc$I0Start <- rep(c(rep(1, 2 * length(countries)), rep(10, 2 * length(countries)),
                   rep(50, 2 * length(countries))), length(countries))
df.bc$distrib <- rep(c(rep(T, length(countries)), rep(F, length(countries))), length(countries) * 3)

# First, plot "original" run, for comparison:
load('syntheticTests/syntheticData/synth_05-09_RATES1.RData')
synth.curr <- synth.runs.RATES[[ix]]
pt.orig = ot.orig = c()
for (i in 1:n) {
  pt.orig <- c(pt.orig, which.max(synth.curr[i, ]))
  ot.orig <- c(ot.orig, findOnset(synth.curr[i, ], 500)$onset)
}

df.orig <- as.data.frame(cbind(countries, rowSums(synth.curr), pt.orig, ot.orig))
names(df.orig) <- c('country', 'ar', 'pt', 'ot')
for (i in 2:4) {
  df.orig[, i] <- as.numeric(as.character(df.orig[, i]))
}
df.orig$pt <- df.orig$pt + 39

par(mfrow = c(3, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(df.orig$country, df.orig$ar)
plot(df.orig$country, df.orig$pt)
plot(df.orig$country, df.orig$ot)

# Now plot metrics by country:
p1 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ar), fill = 'lightblue2') + theme_bw()
p2 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = pt), fill = 'lightblue2') + theme_bw()
p3 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ot), fill = 'lightblue2') + theme_bw()
grid.arrange(p1, p2, p3, ncol = 1)

# And by country/other factors:
p1 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ar), fill = 'lightblue2') +
  facet_wrap(~ cStart) + theme_bw()
p2 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ar), fill = 'lightblue2') +
  facet_grid(I0Start ~ distrib) + theme_bw()
print(p1); print(p2)

p1 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = pt), fill = 'lightblue2') +
  facet_wrap(~ cStart) + theme_bw()
p2 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = pt), fill = 'lightblue2') +
  facet_grid(I0Start ~ distrib) + theme_bw()
print(p1); print(p2)

p1 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ot), fill = 'lightblue2') +
  facet_wrap(~ cStart) + theme_bw()
p2 <- ggplot(data = df.bc) + geom_boxplot(aes(x = c1, y = ot), fill = 'lightblue2') +
  facet_grid(I0Start ~ distrib) + theme_bw()
print(p1); print(p2)

# patterns in AR are remarkably consistent regardless of I0 conditions - but I guess they change
# enough by other initial params/S0 that we don't need to worry too much about realism, right?

# Which countries are most impacted (in terms of AR, PT, OT) by differences in start locations?
p1 <- ggplot(data = df.bc) + geom_boxplot(aes(x = cStart, y = ar), fill = 'lightblue2') +
  facet_wrap(~ c1) + theme_bw()
p2 <- ggplot(data = df.bc) + geom_boxplot(aes(x = cStart, y = pt), fill = 'lightblue2') +
  facet_wrap(~ c1) + theme_bw()
p3 <- ggplot(data = df.bc) + geom_boxplot(aes(x = cStart, y = ot), fill = 'lightblue2') +
  facet_wrap(~ c1) + theme_bw()
print(p1); print(p2); print(p3)

df.med.ar <- aggregate(ar ~ c1 + cStart, data = df.bc, FUN = median)
df.sd.ar <- aggregate(ar ~ c1, data = df.med.ar, FUN = sd)

df.med.pt <- aggregate(pt ~ c1 + cStart, data = df.bc, FUN = median)
df.sd.pt <- aggregate(pt ~ c1, data = df.med.pt, FUN = sd)

df.med.ot <- aggregate(ot ~ c1 + cStart, data = df.bc, FUN = median)
df.sd.ot <- aggregate(ot ~ c1, data = df.med.ot, FUN = sd)

print(df.sd.ar[rev(order(df.sd.ar$ar)), ]) # RO, PL, SK, SI, CZ, PT (>3000); AT, DE, BE, IT (>2000); most consistent: IE, DK, SE, IS
print(df.sd.pt[rev(order(df.sd.pt$pt)), ])
print(df.sd.ot[rev(order(df.sd.ot$ot)), ])

# Also look at spatial patterns by start location:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
                   'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
                   'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                   'United Kingdom')
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'long')]
df.med.ot <- merge(df.med.ot, world.cities, by.x = 'c1', by.y = 'country.etc')
rm(world.cities)

p1 <- ggplot(data = df.med.ot) + geom_point(aes(x = ot, y = long, col = long)) +
  facet_wrap(~ cStart, ncol = 4) + theme_classic() + theme(legend.position = 'n') +
  scale_color_gradientn(colours = viridis(200)) +
  labs(x = 'Onset Week', y = 'Longitude', col = '')
print(p1)

# in general, they look more w-e here, although there's not a strong longitudinal gradient either way (ix=1)
# for ix=2, almost all are e-w, but less strong a pattern, esp. when started in the west
# for ix=3, almost all e-w (only >0.2 w-e if started in IE, IS, SE)
# ix=4: moderately e-w almost always
# ix=5: moderately e-w almost always
# ix=6: can be e-w or w-e, depending on start location
# ix=7: same as 6
# ix=8: strongly w-e (or just b/c I switched to using Spearman?)
# ix=9: mostly e-w
# ix=10: strongly e-w

df.med.ot$cStart <- factor(df.med.ot$cStart)
corrs <- c()
for (cs in levels(df.med.ot$cStart)) {
  corrs <- c(corrs, cor(df.med.ot$ot[df.med.ot$cStart == cs], df.med.ot$long[df.med.ot$cStart == cs], method = 'spearman'))
}
corrs <- as.data.frame(cbind(levels(df.med.ot$cStart), corrs))
print(corrs[rev(order(corrs$corrs)), ])
# tends to be w-e pattern regardless, but obviously stronger if seeded in west (ix=1)

dev.off()

# # From e-w to w-e: 10, 4/5, 2/3/9, 6/7 (depends on start), 1, 8
# load('syntheticTests/syntheticData/params_05-09_1.RData')
# load('syntheticTests/syntheticData/initStates_05-09_1.RData')
# 
# parms <- rbind(init.states.SEL[1:21, ], t(select.parms))
# parms <- parms[, 1:10]
# rm(init.states.SEL, select.parms)
# 
# # most dependent on start location (can be either w-e or e-w) have higher airScale (>1.1, vs. <= 1)
# # see no other obvious differences in parameters
# 
# countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
#                'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
# parms <- parms[1:21, ]
# parms <- as.data.frame(cbind(countries, parms))
# names(parms) <- c('country', 'V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10')
# parms <- merge(parms, world.cities, by.x = 'country', by.y = 'country.etc')
# rm(world.cities)
# 
# parms <- melt(parms, id.vars = c('country', 'long'))
# parms$value <- as.numeric(as.character(parms$value))
# p1 <- ggplot(data = parms) + geom_point(aes(x = long, y = value, col = long)) +
#   scale_color_gradientn(colours = viridis(200)) +
#   theme_classic() + theme(legend.position = 'n') +
#   labs(x = 'Longitude (W to E)', y = 'S0', col = '') +
#   facet_wrap(~ variable, ncol = 2)
# p1
# 
# for (ix in levels(parms$variable)) {
#   parms.red <- parms[parms$variable == ix, ]
#   print(cor(parms.red$long, parms.red$value, method = 'spearman'))
# }
# # for ix=1, E have lower S0; but this is true for 4 too... this doesn't seem to explain the pattern either
# # for 8, E have lower S0, and for 10, E have higher S0 - so it seems likely this partially drives things

# # Which countries are earliest to have onsets in observed data?:
# countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
#                'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
# onset.counts.first5 <- c('UK', 'PT', 'LU', 'BE', 'DK', 'IE', 'DE', 'IT', 'ES', 'AT', 'IS', 'CZ', 'FR', 'SI', 'NL', 'PL', 'HR', 'HU')
# onset.counts.firstWaves <- c('UK', 'PT', 'DE', 'IT', 'ES', 'AT', 'IS', 'LU', 'DK', 'SI', 'NL', 'IE', 'FR', 'PL') # all in vector above
# onset.counts.firstWk <- c('UK', 'DE', 'IT', 'ES', 'LU', 'PT', 'AT', 'IE', 'NL', 'SI', 'PL') 
#
# onset.counts.first5[which(!(onset.counts.first5 %in% onset.counts.firstWaves))] # BE, CZ, HR, HU
# 
# countries[which(!(countries %in% onset.counts.first5))] # RO, SK, SE
# countries[which(!(countries %in% onset.counts.firstWaves))] # BE, HR, CZ, HU, RO, SK, SE
# # may also want to eliminate IS from consideration...

# Try:
# [x] Seed only one random country (or, same random pull of init conditions, run with each country as a start location)
    # [x] Seed 1, 10, 50
    # [x] Seed based on pop dist, or in home-home compartment only
    # [-] Seed only 1, but look at only those in onset.counts.firstWk
# [-] Select 2, 3, 5 countries to randomly seed
    # [-] Narrow range of choices to those with early onsets in observed data

# Evaluate based on:
# [x] Number of countries w/ no onset
# [x] Median/range of AR
# [x] Median/range of PT
# [x] AR/PT/OT by country/longitude
# [x] Synchrony

# [] Turn evaluation into function
# [-] Choose countries where onset SHOULDN'T be "allowed?"
      # [-] Which countries never within first 5 or so to be infected?
      # [-] Choose probabilistically (based on how many seasons first)?
# [x] Which countries are most impacted by differences in initally-seeded country? - RO, IT, ES, SI, LU, NL, PL, HR, AT, BE, DE
# [x] Did Aim1 models fit differences in S0 by country?
      # [] How to incorporate this? (PT sig higher than IS, RO, DE, DK, CZ) - 10% boost or something? (but only for generating runs; filter should be agnostic)
# [x] Spatial patterns based on where epidemic started? (Which consistently show w-e vs. e-w patterns?)





