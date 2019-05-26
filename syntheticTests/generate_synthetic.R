
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
I0_low <- 0; I0_up <- 0.0001 # proportion of population

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

### Save FULL results:
save(newI.c.COUNT, file = 'syntheticTests/syntheticData/allRunsCOUNTS_1000_0523.RData')
save(newI.c, file = 'syntheticTests/syntheticData/allRunsRATES_1000_0523.RData')

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
    
    num.real.pt <- length(which(pts %in% c(13:25)))
    num.real.ar <- length(which(ars >= 10000 & ars <= 50000))
    
    if (num.real.pt >= 21 & num.real.ar >= 15) {
      ens.of.interest <- c(ens.of.interest, ix)
    }
  }
  
}
print(length(ens.of.interest)) # so consistently 4.2% are "realistic"
# bumping lower S0 bound down makes it so that 0 / 1000 are "realistic!"
# WOULD CHANGING ONE OF THESE METRICS LEAVE OUT SOME OF THE IS ONES?

ens.orig <- ens.of.interest # 49
# ens.lowerAR <- ens.of.interest # 96
ens.narrowPT <- ens.of.interest # 26
ens.comb <- ens.of.interest # 47 # 95% in PT range # 41 if AR only widened to 10%
ens.comb.narrow <- ens.of.interest # 16 # 100% in PT range # 14 if AR only widened to 10%

# ens.list <- list(ens.orig, ens.lowerAR, ens.narrowPT, ens.comb, ens.comb.narrow)
ens.list <- list(ens.orig, ens.narrowPT, ens.comb, ens.comb.narrow)

### Any overlap?:
ens.all <- c()
for (val in ens.list[[1]]) {
  if (val %in% ens.list[[4]] & val %in% ens.list[[3]] &
      val %in% ens.list[[2]]) {# & val %in% ens.list[[5]]) {
    ens.all <- c(ens.all, val)
  }
}
print(length(ens.all)) # 7
ens.list <- list(ens.orig, ens.narrowPT, ens.comb, ens.comb.narrow, ens.all)

### How often early IS? / Plot:
which.is <- vector('list', 5)

par(mfrow = c(3, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
count.is <- 0
which.is.temp <- c()
for (val in ens.list[[5]]) {
  newI.ens <- NULL
  for (i in 1:n) {
    newI.ens <- rbind(newI.ens, newI.c[[i]][val, ])
  }
  ot.temp <- c()
  for (i in 1:n) {
    ot.temp <- c(ot.temp, findOnset(newI.ens[i, ], baseline = 500)$onset)
  }
  # print(countries[which(ot.temp == min(ot.temp, na.rm = TRUE))])
  
  if ('IS' %in% countries[which(ot.temp == min(ot.temp, na.rm = TRUE))]) {
    count.is <- count.is + 1
    which.is.temp <- c(which.is.temp, val)
  }
  
  matplot(t(newI.ens), pch = 20, col = viridis(n), type = 'b', xlab = 'Weeks Since Outbreak Start',
          ylab = 'Syndromic+ (per 100,000)', main = val)
  lines(newI.ens[9, ]) # Where is Iceland?
}
print(count.is)
which.is[[5]] <- which.is.temp

# with proportional starts, definitely not as bad
# !1: 14 / 49 (28.571%)
### 2: 34 / 96 (35.417%)
# !3: 6 / 26 (23.077%)
# !4: 13 / __ (___) # 15 / 47 (31.915%) # so narrowing PT but widening AR has little impact on IS starts
# !5: 3 / 14 (___) # 4 / 16 (25.0%) # probably too narrow for PT, though
# !6: 0 / 7

### Potential criteria:
save(ens.list, file = 'syntheticTests/syntheticData/ensList_0523.RData')
# 1. Original
# 2. Narrow PT (20+ weeks in range)
# 3. Narrow PT and wider AR (10-50%)
# 4. Extra narrow PT (21 weeks in range) and wider AR
# 5. In all other 4

### Choose criteria to continue with:
# Or, for now, look at differences in params, S0, and I0 between selected and not selected
for (i in 1:5) {
  print(i)
  ens.temp <- ens.list[[i]]
  ens.notSelected <- c(1:num_ens)[!(c(1:num_ens) %in% ens.temp)]
  
  # par(mfrow = c(1, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  # boxplot(D.temp[ens.temp], D.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'D')
  # boxplot(L.temp[ens.temp], L.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'L')
  # boxplot(parms[3, ens.temp], parms[3, ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'R0mx')
  # boxplot(parms[4, ens.temp], parms[4, ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'R0mn')
  # boxplot(airScale.temp[ens.temp], airScale.temp[ens.notSelected], names = c('Incl.', 'Excl.'), ylab = 'airScale')
  
  par(mfrow = c(5, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  hist(D.temp[ens.temp], breaks = 25, xlab = 'D', xlim = c(2, 7))
  # hist(D.temp[ens.notSelected], breaks = 50, xlab = 'D (excluded)', xlim = c(2, 7))
  hist(L.temp[ens.temp], breaks = 25, xlab = 'L', xlim = c(365, 10 * 365))
  # hist(L.temp[ens.notSelected], breaks = 50, xlab = 'L (excluded)', xlim = c(365, 10 * 365))
  hist(parms[3, ens.temp], breaks = 25, xlab = 'R0mx', xlim = c(1.5, 3.5))
  # hist(parms[3, ens.notSelected], breaks = 50, xlab = 'R0mx (excluded)', xlim = c(1.5, 3.5))
  hist(parms[4, ens.temp], breaks = 25, xlab = 'R0mn', xlim = c(0.8, 1.2))
  # hist(parms[4, ens.notSelected], breaks = 50, xlab = 'R0mn (excluded)', xlim = c(0.8, 1.2))
  hist(airScale.temp[ens.temp], breaks = 25, xlab = 'airScale', xlim = c(0.75, 1.25))
  # hist(airScale.temp[ens.notSelected], breaks = 50, xlab = 'airScale (excluded)', xlim = c(0.75, 1.25))
  
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
  print(kruskal.test(R0mx ~ sel, data = parms.df))
  print(kruskal.test(R0mn ~ sel, data = parms.df))
  print(kruskal.test(R0diff ~ sel, data = parms.df))
  print(kruskal.test(airScale ~ sel, data = parms.df))
  
  initS.sel <- init.states.S[, ens.temp]; initS.notSel <- init.states.S[, ens.notSelected]
  initI.sel <- init.states.I[, ens.temp]; initI.notSel <- init.states.I[, ens.notSelected]
  
  initS <- melt(init.states.S); initI <- melt(init.states.I)
  names(initS) = names(initI) = c('country', 'run', 'value')
  initS$country <- factor(initS$country); initI$country <- factor(initI$country)
  levels(initS$country) = levels(initI$country) = countries
  initS$sel = initI$sel = 'No'
  initS$sel[initS$run %in% ens.temp] <- 'Yes'; initI$sel[initI$run %in% ens.temp] <- 'Yes'
  initS$sel <- factor(initS$sel); initI$sel <- factor(initI$sel)
  
  # p1 <- ggplot(data = initS) + geom_jitter(aes(x = country, y = value, col = sel, alpha = sel, size = sel)) +
  #   theme_classic() + theme(legend.position = 'bottom') +
  #   labs(x = '', y = 'S0 Prop.', fill = 'Realistic Outbreak?') +
  #   scale_size_discrete(range = c(0.5, 1.0))
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
  print(countries[which(s0.ps < 0.05)])
  print(countries[which(i0.ps < 0.05)])
  
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
  print(kruskal.test(varS ~ sel, data = si.df))
  print(kruskal.test(meanI ~ sel, data = si.df))
  print(kruskal.test(varI ~ sel, data = si.df))
}
# patterns:
# R0mx and R0mn sig higher in selected [1, 2, 3, 5 (R0mn only)]
# R0diff tends toward mid-range values (1.5-2ish) [1], higher [3]
# airScale sig but barely higher in selected [1, 2, 3]
# S0: sig lower than expected in IS [1, 2, 3], CZ [2, 3], HR [5]; sig higher in FR [2, 3], UK [3, 5], IT [5]
      # lower Var in selected [1, tend2, tend5]
# I0: sig lower than expected in IT [1], IS [3]; sig higher in SI [4, 5]
      # lower(?) mean in selected [3]

### Let's choose a way of selecting "realistic" runs before continuing
# 



### Run through selected and separate beginning in IS from not beginning in IS:





# What are the parameters for these?
summary(D.temp[ens.of.interest]) # 2.0-5.6
summary(L.temp[ens.of.interest] / 365) # 1-10 years
summary(airScale.temp[ens.of.interest]) # 0.75-1.25 (so full range)
summary(parms[3, ens.of.interest]) # 2.26 - 3.38
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
} # only D/R0mx, D/R0mn sig (both pos)
cor.test(select.parms$R0mx, select.parms$D) # 500 runs: p = 0.00055, cor = 0.6891197
cor.test(select.parms$R0mn, select.parms$D)

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
save(synth.runs.RATES, file = 'syntheticTests/syntheticData/synth_05-16_RATES2.RData')
save(synth.runs.COUNTS, file = 'syntheticTests/syntheticData/synth_05-16_COUNTS2.RData')

# Save these initial conditions so that sensitivity to I0 can be assessed:
# (Remember that this is an initial pass - might decide to do I0 in some other way)
init.states.SEL <- rbind(init.states.S[, ens.of.interest],
                         init.states.I[, ens.of.interest])
# each column is a run

save(init.states.SEL, file = 'syntheticTests/syntheticData/initStates_05-16_2.RData')
save(select.parms, file = 'syntheticTests/syntheticData/params_05-16_2.RData')













### TO-DO ###
# [] Prescribe several (~20) sets of params/initial conditions, get synthetic free simulations
# [x] Assess patterns in free simulations; compare with observed data
# [x] Analyze patterns based on seeding:
    # [x] Everywhere
    # [x] Each country
    # [-] 2-5 initial countries
    # [x] Vary # seeded in each country
# [x] Compare spatial patterns and synchrony to synthetic
# [-] Test impact of net inflow/outflow on synthetic AR/PT/OT?
    # [-] Control for S0? Set all S0 equal? (or not - could look at it over multiple runs and see if consistent)
    # [-] Look at this for observed, too? (Controlling for S0?)
# [] Also set S0 equal to see what spatial patterns look like then?
    # [x] Param/S0 differences between runs with w-e vs. e-w spread?
# [] Assess model sensitivity to different params by holding other 4 constant, then changing param of interest?
    # (At the very least, do this for airScale)
# [] Assess synthetic patterns when including/excluding different travel types (after getting more solid model)


