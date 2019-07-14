
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
num_ens <- 1000
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 1.5; Rmn_low <- 0.8; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population # to 0.01% or 0.1%?

# ### Restricted parameter boundaries - try?
# D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rmn_low <- 0.8; airScale_low <- 0.75
# D_up <- 6; L_up <- 10*365; Rmx_up <- 3.5; Rmn_up <- 1.2; airScale_up <- 1.25
# theta_low <- c(L_low, D_low, Rmx_low, Rmn_low, airScale_low)
# theta_up <- c(L_up, D_up, Rmx_up, Rmn_up, airScale_up)
# S0_low <- 0.55; S0_up <- 0.90 # proportion of population
# I0_low <- 1.0; I0_up <- 50.0 # raw number

### Specify the country for which we are performing a forecast
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

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

### Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE,
                   prohibAir = FALSE)
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

### Save FULL results:
save(newI.c.COUNT, file = 'syntheticTests/syntheticData/allRunsCOUNTS_1000_0711.RData')
save(newI.c, file = 'syntheticTests/syntheticData/allRunsRATES_1000_0711.RData')

### Run through free simulations and check:
# at least 19 countries/21 have 500+ cases in 3+ weeks (alt: all countries exceed 500/100,000 (onset))
# 88% of peaks (18 countries) occur between weeks 13 and 25 (inclusive) (OR: 95% of peaks?)
# at least 75% of countries (15) have AR between 15-50% of 100,000 (OR: 5-50%?)
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
    
    # num.real.pt <- length(which(pts %in% c(13:25))) # rounding down
    num.real.pt <- length(which(pts %in% c(14:25))) # 95% CI (obs_pkwk - 39)
    num.real.ar <- length(which(ars >= 15000 & ars <= 50000))
    
    if (num.real.pt >= 18 & num.real.ar >= 15) {
      ens.of.interest <- c(ens.of.interest, ix)
    }
  }
  
}
print(length(ens.of.interest)) # 36/1000; 24/1000 if wider I0 start
# WOULD CHANGING ONE OF THESE METRICS LEAVE OUT SOME OF THE IS ONES? - doesn't seem so, these percentages/metrics seem fine

### How often early SE? / Plot:
par(mfrow = c(5, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
count.is <- 0
which.is.temp <- c()
for (val in ens.of.interest) {
  newI.ens <- NULL
  for (i in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[i]][val, ])
  }
  ot.temp <- c()
  for (i in 1:n) {
    ot.temp <- c(ot.temp, findOnset(newI.ens[i, ], baseline = 500)$onset)
  }
  
  if ('SE' %in% countries[which(ot.temp == min(ot.temp, na.rm = TRUE))]) {
    count.is <- count.is + 1
    which.is.temp <- c(which.is.temp, val)
  }
  
  matplot(t(newI.ens), pch = 20, col = viridis(n), type = 'b', xlab = 'Weeks Since Outbreak Start',
          ylab = 'Syndromic+ (per 100,000)', main = val)
  lines(newI.ens[19, ]) # Where is Sweden?
}
print(count.is)
# 7/20 for SE - 35%

### Look at differences in params, S0, and I0 between selected and not selected
ens.temp <- ens.of.interest
ens.notSelected <- c(1:num_ens)[!(c(1:num_ens) %in% ens.temp)]

pdf('syntheticTests/outputs/selected_v_not.pdf', width = 14, height = 7)
par(mfrow = c(1, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(D.temp[ens.temp], D.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'D')
boxplot(L.temp[ens.temp], L.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'L')
boxplot(parms[3, ens.temp], parms[3, ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'R0mx')
boxplot(parms[4, ens.temp], parms[4, ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'R0mn')
boxplot(airScale.temp[ens.temp], airScale.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'airScale')

par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
hist(D.temp[ens.temp], breaks = 25, xlab = 'D', xlim = c(2, 7))
hist(L.temp[ens.temp], breaks = 25, xlab = 'L', xlim = c(365, 10 * 365))
hist(parms[3, ens.temp], breaks = 25, xlab = 'R0mx', xlim = c(1.5, 3.5))
hist(parms[4, ens.temp], breaks = 25, xlab = 'R0mn', xlim = c(0.8, 1.2))
hist(airScale.temp[ens.temp], breaks = 25, xlab = 'airScale', xlim = c(0.75, 1.25))

par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
hist(parms[3, ens.temp] - parms[4, ens.temp], breaks = 25, xlab = 'R0diff', xlim = c(0.3, 2.7))
hist(parms[3, ens.notSelected] - parms[4, ens.notSelected], breaks = 50, xlab = 'R0diff', xlim = c(0.3, 2.7))

parms.df <- as.data.frame(t(parms))
names(parms.df) <- c('L', 'D', 'R0mx', 'R0mn', 'airScale')
parms.df$R0diff <- parms.df$R0mx - parms.df$R0mn
parms.df$sel <- 'No'
parms.df$sel[ens.temp] <- 'Yes'
parms.df$sel <- factor(parms.df$sel)

print(kruskal.test(L ~ sel, data = parms.df))
print(kruskal.test(D ~ sel, data = parms.df))
print(kruskal.test(R0mx ~ sel, data = parms.df)) # sig
print(kruskal.test(R0mn ~ sel, data = parms.df))
print(kruskal.test(R0diff ~ sel, data = parms.df)) # sig
print(kruskal.test(airScale ~ sel, data = parms.df))
# R0mx and R0diff both higher in selected (so higher R0mx, but also higher impact of humidity?)

initS.sel <- init.states.S[, ens.temp]; initS.notSel <- init.states.S[, ens.notSelected]
initI.sel <- init.states.I[, ens.temp]; initI.notSel <- init.states.I[, ens.notSelected]

initS <- melt(init.states.S); initI <- melt(init.states.I)
names(initS) = names(initI) = c('country', 'run', 'value')
initS$country <- factor(initS$country); initI$country <- factor(initI$country)
levels(initS$country) = levels(initI$country) = countries
initS$sel = initI$sel = 'No'
initS$sel[initS$run %in% ens.temp] <- 'Yes'; initI$sel[initI$run %in% ens.temp] <- 'Yes'
initS$sel <- factor(initS$sel); initI$sel <- factor(initI$sel)

p1 <- ggplot(data = initS[initS$sel == 'Yes', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + labs(x = '', y = 'S0 Prop.')
p2 <- ggplot(data = initI[initI$sel == 'Yes', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = '', y = 'I0 Prop.')
grid.arrange(p1, p2, ncol = 1)

s0.ps = i0.ps <- c()
for (j in countries) {
  initS.temp <- initS[initS$country == j, ]; initI.temp <- initI[initI$country == j, ]
  s0.ps <- c(s0.ps, kruskal.test(value ~ sel, data = initS.temp)$p.value)
  i0.ps <- c(i0.ps, kruskal.test(value ~ sel, data = initI.temp)$p.value)
}
print(countries[which(s0.ps < 0.05)]) # UK has higher S0 than in unselected
print(countries[which(i0.ps < 0.05)]) # no differences

meanS = varS = meanS.notSel = varS.notSel = c()
for (j in 1:num_ens) {
  if (j %in% ens.temp) {
    meanS <- c(meanS, mean(init.states.S[, j]))
    varS <- c(varS, var(init.states.S[, j]))
  } else {
    meanS.notSel <- c(meanS.notSel, mean(init.states.S[, j]))
    varS.notSel <- c(varS.notSel, var(init.states.S[, j]))
  }
}
hist(meanS, breaks = 25, xlab = 'Mean S0 (by run)', main = '')
hist(meanS.notSel, breaks = 50, xlab = 'Mean S0 (by run)', main = '')
hist(varS, breaks = 25, xlab = 'Variance of S0 (by run)', main = '')
hist(varS.notSel, breaks = 50, xlab = 'Variance of S0 (by run)', main = '')

meanI = varI = meanI.notSel = varI.notSel = c()
for (j in 1:num_ens) {
  if (j %in% ens.temp) {
    meanI <- c(meanI, mean(init.states.I[, j]))
    varI <- c(varI, var(init.states.I[, j]))
  } else {
    meanI.notSel <- c(meanI.notSel, mean(init.states.I[, j]))
    varI.notSel <- c(varI.notSel, var(init.states.I[, j]))
  }
}
hist(meanI, breaks = 25, xlab = 'Mean I0 (by run)', main = '')
hist(meanI.notSel, breaks = 50, xlab = 'Mean I0 (by run)', main = '')
hist(varI, breaks = 25, xlab = 'Variance of I0 (by run)', main = '')
hist(varI.notSel, breaks = 50, xlab = 'Variance of I0 (by run)', main = '')

si.df <- as.data.frame(cbind(c(meanS, meanS.notSel), c(varS, varS.notSel),
                             c(meanI, meanI.notSel), c(varI, varI.notSel),
                             c(rep('Yes', length(ens.temp)), rep('No', length(ens.notSelected)))))
names(si.df) <- c('meanS', 'varS', 'meanI', 'varI', 'sel')
print(kruskal.test(meanS ~ sel, data = si.df))
print(kruskal.test(varS ~ sel, data = si.df)) # sig - lower variance in S0 than in unselected
print(kruskal.test(meanI ~ sel, data = si.df))
print(kruskal.test(varI ~ sel, data = si.df)) # sig (but barely - p=0.04411) - lower variance in I0 than in unselected
dev.off()

# patterns:
# R0mx, R0diff sig higher in selected
# S0: sig higher than expected in UK
    # lower Var in selected (also for I0)

### Compare runs beginning in SE with those not beginning in SE:
not.is <- ens.of.interest[!(ens.of.interest %in% which.is.temp)]

pdf('syntheticTests/outputs/IS_v_not.pdf', width = 14, height = 7)
par(mfrow = c(1, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(D.temp[not.is], D.temp[which.is.temp], names = c('Other Start', 'IS Start'), ylab = 'D')
boxplot(L.temp[not.is], L.temp[which.is.temp], names = c('Other Start', 'IS Start'), ylab = 'L')
boxplot(parms[3, not.is], parms[3, which.is.temp], names = c('Other Start', 'IS Start'), ylab = 'R0mx')
boxplot(parms[4, not.is], parms[4, which.is.temp], names = c('Other Start', 'IS Start'), ylab = 'R0mn')
boxplot(airScale.temp[not.is], airScale.temp[which.is.temp], names = c('Other Start', 'IS Start'), ylab = 'airScale')

parms.df <- as.data.frame(t(parms))
names(parms.df) <- c('L', 'D', 'R0mx', 'R0mn', 'airScale')
parms.df$R0diff <- parms.df$R0mx - parms.df$R0mn
parms.df$is.start <- NA
parms.df$is.start[which.is.temp] <- 'Yes'
parms.df$is.start[not.is] <- 'No'
parms.df$is.start <- factor(parms.df$is.start)
parms.df <- parms.df[!is.na(parms.df$is.start), ]

print(kruskal.test(L ~ is.start, data = parms.df))
print(kruskal.test(D ~ is.start, data = parms.df))
print(kruskal.test(R0mx ~ is.start, data = parms.df))
print(kruskal.test(R0mn ~ is.start, data = parms.df))
print(kruskal.test(R0diff ~ is.start, data = parms.df))
print(kruskal.test(airScale ~ is.start, data = parms.df))
# small "sample size," but still none sig

initS.IS <- init.states.S[, not.is]; initS.notSel <- init.states.S[, which.is.temp]
initI.IS <- init.states.I[, not.is]; initI.notSel <- init.states.I[, which.is.temp]

initS <- melt(init.states.S); initI <- melt(init.states.I)
names(initS) = names(initI) = c('country', 'run', 'value')
initS$country <- factor(initS$country); initI$country <- factor(initI$country)
levels(initS$country) = levels(initI$country) = countries
initS$is.start = initI$is.start = NA
initS$is.start[initS$run %in% which.is.temp] <- 'Yes'; initI$is.start[initI$run %in% which.is.temp] <- 'Yes'
initS$is.start[initS$run %in% not.is] <- 'No'; initI$is.start[initI$run %in% not.is] <- 'No'
initS$is.start <- factor(initS$is.start); initI$is.start <- factor(initI$is.start)
initS <- initS[!is.na(initS$is.start), ]; initI <- initI[!is.na(initI$is.start), ]

p1 <- ggplot(data = initS[initS$is.start == 'No', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + labs(x = '', y = 'S0 Prop.', title = 'Other Start')
p2 <- ggplot(data = initS[initS$is.start == 'Yes', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + labs(x = '', y = 'S0 Prop.', title = 'IS Start')
p3 <- ggplot(data = initI[initI$is.start == 'No', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = '', y = 'I0 Prop.', title = 'Other Start')
p4 <- ggplot(data = initI[initI$is.start == 'Yes', ]) +
  geom_boxplot(aes(x = country, y = value), fill = 'lightblue2') +
  theme_classic() + theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = '', y = 'I0 Prop.', title = 'IS Start')
grid.arrange(p1, p2, ncol = 1)
grid.arrange(p3, p4, ncol = 1)

s0.ps = i0.ps <- c()
for (j in countries) {
  initS.temp <- initS[initS$country == j, ]; initI.temp <- initI[initI$country == j, ]
  s0.ps <- c(s0.ps, kruskal.test(value ~ is.start, data = initS.temp)$p.value)
  i0.ps <- c(i0.ps, kruskal.test(value ~ is.start, data = initI.temp)$p.value)
}
print(countries[which(s0.ps < 0.05)]) # IS sig higher S0 when IS is the first to peak
print(countries[which(i0.ps < 0.05)]) # none
dev.off()
# all results same for higher I0 maxs

### Look at parameters for chosen runs:
summary(D.temp[ens.of.interest]) # 2.7-7
summary(L.temp[ens.of.interest] / 365) # 1-10 years
summary(airScale.temp[ens.of.interest]) # 0.84-1.23 (so basically full range)
summary(parms[3, ens.of.interest]) # 2.213 - 3.454
summary(parms[4, ens.of.interest]) # 0.8 - 1.2
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
} # D/L, D/R0mx, L/R0mx (all pos); airScale/R0mn (neg)

cor.test(select.parms$R0mx, select.parms$D)
cor.test(select.parms$R0mn, select.parms$airScale)
cor.test(select.parms$R0mx, select.parms$L)
cor.test(select.parms$L, select.parms$D)
# Bonferroni: 0.005 - only first 2 are sig

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
save(synth.runs.RATES, file = 'syntheticTests/syntheticData/synth_07-11_RATES.RData')
save(synth.runs.COUNTS, file = 'syntheticTests/syntheticData/synth_07-11_COUNTS.RData')

# Save these initial conditions so that sensitivity to I0 can be assessed:
# (Remember that this is an initial pass - might decide to do I0 in some other way)
init.states.SEL <- rbind(init.states.S[, ens.of.interest],
                         init.states.I[, ens.of.interest])
# each column is a run

save(init.states.SEL, file = 'syntheticTests/syntheticData/initStates_07-11.RData')
save(select.parms, file = 'syntheticTests/syntheticData/params_07-11.RData')
# 5, 14, 24 (108, 467, 931) all seem to have later IS outbreaks and may be good candidates for fitting

### Narrowed down to 6 to check first:
to.run <- c(1, 2, 8:10, 15)
init.states.SEL[, to.run]
select.parms[to.run, ]
# might be good to find one with lower D/R0mx - Done!






