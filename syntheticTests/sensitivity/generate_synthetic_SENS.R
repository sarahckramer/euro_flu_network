
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
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 1.5; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low)#, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up)#, airScale_up)

# D_low <- 2; L_low <- 1*365; R_low <- 1.0; airScale_low <- 0.75
# D_up <- 7; L_up <- 10*365; R_up <- 2.5; airScale_up <- 1.25
# theta_low <- c(L_low, D_low, R_low, airScale_low)
# theta_up <- c(L_up, D_up, R_up, airScale_up)
S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population # to 0.01% or 0.1%?

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:21)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]
t.comm[t.comm > 0] <- 0

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

### Read in humidity data
ah <- read.csv('data/ah_05-07_formatted.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

#############################################################################
#############################################################################
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
#############################################################################
#############################################################################

# #############################################################################
# #############################################################################
# ### Set initial conditions (same in all compartments)
# param.bound <- cbind(c(S0_low, I0_low, theta_low),
#                       c(S0_up, I0_up, theta_up))
# parms <- t(lhs(num_ens, param.bound))
# 
# S0.temp = I0.temp = vector('list', num_ens)
# for (i in 1:num_ens) {
#   S0.temp[[i]] <- matrix(parms[1, i], nrow = n, ncol = n) * N
#   I0.temp[[i]] <- matrix(parms[2, i], nrow = n, ncol = n) * N
#   
# }
# 
# init.states <- parms[1:2, ]
# #############################################################################
# #############################################################################

### Reduce parms to hold just the parameters now:
# parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
parms <- parms[(dim(parms)[1] - 3):(dim(parms)[1]), ]

### Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[3, ] - parms[4, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[4, ix]) / parms[2, ix]
})
# beta <- lapply(1:num_ens, function(ix) {
#   matrix(parms[3, ix] / parms[2, ix], nrow = length(beta.range), ncol = n)
# })

### Create vectors of initial parameters:
D.temp <- parms[2, ]; L.temp <- parms[1, ]#; airScale.temp <- parms[5, ]
# D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[4, ]

### Run!
# m <- sapply(1:num_ens, function(ix) {
#   propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
#                    S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
#                    D = D.temp[ix], L = L.temp[ix], beta[[ix]],
#                    airScale = airScale.temp[ix], realdata = TRUE)
# })
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = 0, realdata = TRUE)
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

### Reformat results:
newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)

### Save results:
save(newI.ens, file = 'syntheticTests/sensitivity/outputs_datasets/synth_diffS0I0_noTravel.RData')

### Plot ###
pdf('syntheticTests/sensitivity/outputs_plots/sens_noTravel.pdf', width = 12, height = 10)

### Plot results (quick view):
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:num_ens) {
  matplot(newI.ens[[i]], type = 'b', pch = 20, cex = 0.7, col = viridis(n), main = i, ylab = 'Cases / 100,000 Pop.')
}
# little variation in patterns

### Calculate onset timings / peak timings / peak intensities for each outbreak/country:
source('code/functions/Util.R')
wk_start <- 1
m <- NULL
for (i in 1:num_ens) {
  for (j in 1:n) {
    m <- rbind(m, c(i, j, findOnset(newI.ens[[i]][, j], 500)$onset, which.max(newI.ens[[i]][, j]), max(newI.ens[[i]][, j])))
  }
}

parms.t <- as.data.frame(t(parms))
# names(parms.t) <- c('L', 'D', 'R0mx', 'R0mn', 'airScale')
# names(parms.t) <- c('L', 'D', 'R0', 'airScale')
names(parms.t) <- c('L', 'D', 'R0mx', 'R0mn')
parms.t$outbreak <- 1:100

m <- as.data.frame(m)
names(m) <- c('outbreak', 'country', 'ot', 'pt', 'pi')
m$country <- countries[m$country]; m$country <- factor(m$country)
m$pt[is.na(m$ot)] <- NA; m$pi[is.na(m$ot)] <- NA

m <- merge(m, parms.t, by = 'outbreak')

### Some measure of PT order? Or difference from first to peak?:
m$pt.rank <- NA
for (i in 1:num_ens) {
  m.temp <- m[m$outbreak == i, ]
  
  if (any(!is.na(m.temp$pt))) {
    m.temp$pt.rank[which(m.temp$pt == min(m.temp$pt, na.rm = T))] <- 0
    
    pt.base <- unique(m.temp$pt[m.temp$pt.rank == 0 & !is.na(m.temp$pt.rank)])
    m.temp$pt.rank[is.na(m.temp$pt.rank)] <- m.temp$pt[is.na(m.temp$pt.rank)] - pt.base
    
    m$pt.rank[m$outbreak == i] <- m.temp$pt.rank
  }
}

### Which countries vary the most?
p1 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = ot), fill = 'steelblue2') +
  theme_classic() + labs(x = '', y = 'Onset Timing')
p2 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt), fill = 'steelblue2') +
  theme_classic() + labs(x = '', y = 'Peak Timing')
p4 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt.rank), fill = 'steelblue2') +
  theme_classic() + labs(x = '', y = 'Weeks After Initial Seasonal Peak')
p3 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pi), fill = 'steelblue2') +
  theme_classic() + labs(x = '', y = 'Peak Intensity')
grid.arrange(p1, p2, p3, p4, ncol = 1)
# if a parameter reduces the "rank" of all/most countries, likely an increase in synchrony; if variable impact by country, likely changing the overall patterns

# m$airScale.b <- -1; m$airScale.b[m$airScale > 1.2] <- 1; m$airScale.b[m$airScale < 0.8] <- 0
# p5 <- ggplot(data = m) + geom_boxplot(aes(x = country, y = pt.rank), fill = 'steelblue2') +
#   theme_classic() + labs(x = '', y = 'Weeks After Initial Seasonal Peak') + facet_wrap(~ airScale.b)

### Okay, let's plot PT in order of when it happens by latitude and longitude
    # Do for airScale 0.75, 1.00, and 1.25?
# Another analysis...

# BASE #
# not seeing a whole lot of variation by country for timing, but intensity tends lowest for IE and PT
# IS and SE seem to have most variation; SK, HU, IE, PT, and UK all have less
# (note: no statistical analyses done here, just looking)

# 2 #
# little variation between countries for OT/PT; IE/PT still smallest; IS earliest
    # not as strong patterns by humidity as I expected...

### Plot results with changing parameter values:
print(ggplot(data = m) + geom_point(aes(x = D, y = pt)) +
  theme_classic() + labs(x = 'D', y = 'Peak Timing') +
  facet_wrap(~ country))
# Base: higher D = later PT; relationship looks strongest for PT and weakest for SE
print(ggplot(data = m) + geom_point(aes(x = D, y = log(pi))) +
  theme_classic() + labs(x = 'D', y = 'log(Peak Intensity)') +
  facet_wrap(~ country, scales = 'free_y'))
print(ggplot(data = m) + geom_point(aes(x = D, y = pt.rank)) +
  theme_classic() + labs(x = 'D', y = 'Peak Timing (Rel.)') +
  facet_wrap(~ country, scales = 'free_y'))

print(ggplot(data = m) + geom_point(aes(x = L, y = pt)) +
  theme_classic() + labs(x = 'L', y = 'Peak Timing') +
  facet_wrap(~ country))
print(ggplot(data = m) + geom_point(aes(x = L, y = log(pi))) +
  theme_classic() + labs(x = 'L', y = 'log(Peak Intensity)') +
  facet_wrap(~ country, scales = 'free_y'))
print(ggplot(data = m) + geom_point(aes(x = L, y = pt.rank)) +
  theme_classic() + labs(x = 'L', y = 'Peak Timing (Rel.)') +
  facet_wrap(~ country, scales = 'free_y'))

# print(ggplot(data = m) + geom_point(aes(x = R0, y = pt)) +
#         theme_classic() + labs(x = 'R0', y = 'Peak Timing') +
#         facet_wrap(~ country))
# print(ggplot(data = m) + geom_point(aes(x = R0, y = log(pi))) +
#         theme_classic() + labs(x = 'R0', y = 'log(Peak Intensity)') +
#         facet_wrap(~ country, scales = 'free_y'))
# print(ggplot(data = m) + geom_point(aes(x = R0, y = pt.rank)) +
#         theme_classic() + labs(x = 'R0', y = 'Peak Timing (Rel.)') +
#         facet_wrap(~ country, scales = 'free_y'))

print(ggplot(data = m) + geom_point(aes(x = R0mx, y = pt)) +
  theme_classic() + labs(x = 'R0mx', y = 'Peak Timing') +
  facet_wrap(~ country))
print(ggplot(data = m) + geom_point(aes(x = R0mx, y = log(pi))) +
  theme_classic() + labs(x = 'R0mx', y = 'log(Peak Intensity)') +
  facet_wrap(~ country, scales = 'free_y'))
# R0mx has pretty clear relationship with PT and PI
print(ggplot(data = m) + geom_point(aes(x = R0mx, y = pt.rank)) +
  theme_classic() + labs(x = 'R0mx', y = 'Peak Timing (Rel.)') +
  facet_wrap(~ country, scales = 'free_y'))

print(ggplot(data = m) + geom_point(aes(x = R0mn, y = pt)) +
  theme_classic() + labs(x = 'R0mn', y = 'Peak Timing') +
  facet_wrap(~ country))
print(ggplot(data = m) + geom_point(aes(x = R0mn, y = log(pi))) +
  theme_classic() + labs(x = 'R0mn', y = 'log(Peak Intensity)') +
  facet_wrap(~ country, scales = 'free_y'))
print(ggplot(data = m) + geom_point(aes(x = R0mn, y = pt.rank)) +
  theme_classic() + labs(x = 'R0mn', y = 'Peak Timing (Rel.)') +
  facet_wrap(~ country, scales = 'free_y'))

print(ggplot(data = m) + geom_point(aes(x = airScale, y = pt)) +
  theme_classic() + labs(x = 'airScale', y = 'Peak Timing') +
  facet_wrap(~ country))
print(ggplot(data = m) + geom_point(aes(x = airScale, y = log(pi))) +
  theme_classic() + labs(x = 'airScale', y = 'log(Peak Intensity)') +
  facet_wrap(~ country, scales = 'free_y'))
print(ggplot(data = m) + geom_point(aes(x = airScale, y = pt.rank)) +
  theme_classic() + labs(x = 'airScale', y = 'Peak Timing (Rel.)') +
  facet_wrap(~ country, scales = 'free_y'))
# PI lowest when airScale ~ 1.0?

dev.off()

# ggplot(data = m) + geom_point(aes(x = airScale, y = pt, col = country)) +
#   theme_classic() + labs(x = 'airScale', y = 'Peak Timing')
# ggplot(data = m) + geom_point(aes(x = airScale, y = pt.rank, col = country)) +
#   theme_classic() + labs(x = 'airScale', y = 'Peak Timing (Rel.)')

### Also check correlations:
cor.test(m$pt, m$D)
cor.test(m$pt, m$L)
cor.test(m$pt, m$R0mx)
cor.test(m$pt, m$R0mn)
cor.test(m$pt, m$airScale)
# all sig except R0mn, but D/R0mx strongest
m1 <- lm(pt ~ D + L + R0mx + R0mn + airScale, data = m) # D, R0mx, R0mn sig; all sig (D/L pos, rest neg)
m15 <- lm(pt ~ D + L + R0mx + R0mn + airScale + country, data = m) # PT later (**) for: BE, ES, FR, HR, IE, IT, NL, PT, UK, earlier for IS, SE; ES, IE, IT, PT

cor.test(m$pi, m$D)
cor.test(m$pi, m$L)
cor.test(m$pi, m$R0mx)
cor.test(m$pi, m$R0mn)
cor.test(m$pi, m$airScale)
# all sig except R0mn and airScale, but D/R0mx strongest again
m2 <- lm(pi ~ D + L + R0mx + R0mn + airScale, data = m) # all sig except airScale; all sig
m25 <- lm(pi ~ D + L + R0mx + R0mn + airScale + country, data = m) # smaller peaks in: BE, DK, ES, FR, IE, IT, LU, NL, PT, UK, larger in SE; ES, FR, IE, IT, NL, PT, UK

cor.test(m$pt.rank, m$D)
cor.test(m$pt.rank, m$L)
cor.test(m$pt.rank, m$R0mx)
cor.test(m$pt.rank, m$R0mn)
cor.test(m$pt.rank, m$airScale)
# D (pos), R0mn (neg); R0mx doesn't seem to impact it b/c, even though things might be more synchronous, some countries that wouldn't otherwise have outbreaks have them now
# not at all associated with airScale here when no humidity forcing
m3 <- lm(pt.rank ~ D + L + R0mx + R0mn + airScale, data = m) # D, R0mn, airScale sig (pos/neg/pos, respectively); D and R0mn sig (pos and neg, respectively)
m35 <- lm(pt.rank ~ D + L + R0mx + R0mn + airScale + country, data = m)
# controlling for country differences...
    # Base: larger D/airScale lead to greater distances from first peak; larger R0mx/R0mn lead to peaks being closer together
    # 2: larger D/L lead to greater distances from first peak; larger R0mx/R0mn/airScale lead to peaks being closer together
    # several sig relationships by country, but not super important to know specifically
    # note: controlling for country should give overall impact on synchrony; looking for differences by country might show if patterns are changed
    # 3 (no AH): no differences in pt.rank by country; all params sig except airScale

# Is transportation not getting to play as big a role as it should? airScale is never relevant
# Issue also that, while R0mx and D influnece PT/PI, they don't necessarily influence pattern of spread between country
    # Maybe measure this? Rank peak timing, or look at synchrony, and see how params influence that?

for (var1 in c(4:5, 11)) {
  for (var2 in 6:10) {
    for (country in 1:n) {
      if (cor.test(m[m$country == countries[country], var1], m[m$country == countries[country], var2])$p.value < 0.002) {
        print(paste(countries[country], names(m)[var1], names(m)[var2], sep = '_'))
        # print(cor.test(m[m$country == countries[country], var1], m[m$country == countries[country], var2]))
        print(''); print('')
      }
    }
  }
}
# Base: PT/R0mx/D for all; PI/D for some; PI/R0mx for all; PT.rank/D for IE and LU ()
# 2: PT/D for all countries; PT/R0mx for half-ish countries; PI/R0mx for all countries; PT.rank and... D (SE), R0mx (IS, SE), R0mn (HR, DE, HU, RO, SI)
      # As R0mx increases, IS gets earlier and SE later (although SE always 0 or 1); higher D = earlier SE
      # And these patterns are robust to higher p-cutoffs! maybe because they're just as late after the initial peak, even if they get closer together? (initial peak also gets earlier, of course)
      # Higher R0mn = less influence of humidity! And this changes how countries are behaving in peak timing!
# Without AH-forcing, pt.rank tends to vary by R0mx/D for most/some countries, respectively; with AH-forcing, they're more locked

# Base: Mostly doesn't look like there's many strong relationships between countries/params (other than R0mx/D) and dynamics; broader range for PT.rank than when all S0/I0 same - so starting conditions do allow for a much wider range of activity patterns
# 2: Same general pattern seems to occur most seasons; no strong differences in PT ranges by country, though; much more organized and consistent by how later after 1st peak they occur; suggests impact of humidity through impact of R0mn on relative timing of peak
# 3: Pattern in time from first peak disappears
# 4 (w/o travel): Spread is overall slower, but pattern driven by AH is the same



# [] Add plots by latitude? Longitude? Mean AH? % pop commuting? % pop in air network?
# [] Remove IS and maybe SE?






