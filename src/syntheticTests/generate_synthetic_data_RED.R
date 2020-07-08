### Run model to generate synthetic data for model testing ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(gridExtra); library(viridis)

##########################################################################################

### Set seed
set.seed(10489436)
# set.seed(10489437)
# set.seed(10489438)

### Read in model function
source('src/mainCode/functions/SIRS_network.R')

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

S0_low <- 0.40; S0_up <- 0.90
I0_low <- 0; I0_up <- 0.00005
# I0_low <- 0; I0_up <- 0.000001

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

### Specify the countries for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('src/formatTravelData/formattedData/comm_mat_1000.RData') # requires comm_mat of same size and number of desired runs
t.comm <- lapply(comm.list1, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
for (i in 1:num_ens) {
  rownames(t.comm[[i]]) <- countries
  colnames(t.comm[[i]]) <- countries
}

# # No commuting:
# for (i in 1:num_ens) {
#   t.comm[[i]] <- matrix(0, nrow = 12, ncol = 12)
#   rownames(t.comm[[i]]) <- countries
#   colnames(t.comm[[i]]) <- countries
# }

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
for (i in 1:num_ens) {
  diag(N[[i]]) <- unlist(lapply(1:n, function(ix) {
    pop.size$pop[ix] - rowSums(N[[i]])[ix]
  }))
}

### Read in humidity data
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

### Set initial conditions based on input parameters
# param.bound <- cbind(c(S0_low, sd_low, rep(I0_low, n), theta_low),
#                      c(S0_up, sd_up, rep(I0_up, n), theta_up)) # if initiating all countries' S0 as normal dist around a single value
param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n), theta_low),
                     c(rep(S0_up, n), rep(I0_up, n), theta_up))
# param.bound <- cbind(c(rep(S0_low, n**2), rep(I0_low, n**2), theta_low),
#                      c(rep(S0_up, n**2), rep(I0_up, n**2), theta_up)) # for LHS over all compartments
parms <- t(lhs(num_ens, param.bound))

parms[, 28][parms[, 28] >= parms[, 27]] <- parms[, 27][parms[, 28] >= parms[, 27]] - 0.01

### Read in functions to run model/format results:
source('src/syntheticTests/analyze/synth_functions.R')
source('src/mainCode/functions/Util.R')

### Run model!
init.states <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'lhs')
# init.states <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'lhs_full')
res <- run_model(parms, init.states[[1]], init.states[[2]], AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, r0.mn = FALSE, multi = FALSE)
res.rates <- res[[1]]
s.rates <- res[[3]]

df.met <- check_realistic(res.rates)[[1]]
is.real.check <- check_realistic(res.rates)[2:3]

runs.onset <- which(is.real.check[[1]])
runs.realistic <- which(is.real.check[[2]])

runs.noOn <- (1:num_ens)[!((1:num_ens) %in% runs.onset)]
runs.noReal <- (1:num_ens)[!((1:num_ens) %in% runs.realistic)]
# runs.noReal <- runs.onset[!(runs.onset %in% runs.realistic)] # choose realistic vs. ALL, and compare to onset vs. ALL - easier to see what ranges to select

### Optional: Look at what params yieled onsets/realistic outbreaks and which did not:
# pdf('syntheticTests/outputs/boxplots_redMod_011620.pdf', height = 9, width = 14)

par(mfrow = c(2, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(parms[dim(parms)[1] - 4, runs.onset], parms[dim(parms)[1] - 4, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'L')
boxplot(parms[dim(parms)[1] - 3, runs.onset], parms[dim(parms)[1] - 3, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'D')
boxplot(parms[dim(parms)[1] - 2, runs.onset], parms[dim(parms)[1] - 2, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'R0max')
boxplot(parms[dim(parms)[1] - 1, runs.onset], parms[dim(parms)[1] - 1, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'R0diff')
boxplot(parms[dim(parms)[1], runs.onset], parms[dim(parms)[1], runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'airScale')
boxplot(parms[dim(parms)[1] - 2, runs.onset] - parms[dim(parms)[1] - 1, runs.onset], parms[dim(parms)[1] - 2, runs.noOn] - parms[dim(parms)[1] - 1, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'R0mn')
# boxplot(parms[1, runs.onset], parms[1, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'S0')
# boxplot(parms[2, runs.onset], parms[2, runs.noOn], names = c('Incl.', 'Excl.'), ylab = 'S0_sd')

boxplot(parms[dim(parms)[1] - 4, runs.realistic], parms[dim(parms)[1] - 4, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'L')
boxplot(parms[dim(parms)[1] - 3, runs.realistic], parms[dim(parms)[1] - 3, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'D')
boxplot(parms[dim(parms)[1] - 2, runs.realistic], parms[dim(parms)[1] - 2, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'R0max')
boxplot(parms[dim(parms)[1] - 1, runs.realistic], parms[dim(parms)[1] - 1, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'R0diff')
boxplot(parms[dim(parms)[1], runs.realistic], parms[dim(parms)[1], runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'airScale')
boxplot(parms[dim(parms)[1] - 2, runs.realistic] - parms[dim(parms)[1] - 1, runs.realistic], parms[dim(parms)[1] - 2, runs.noReal] - parms[dim(parms)[1] - 1, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'R0mn')
# boxplot(parms[1, runs.realistic], parms[1, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'S0')
# boxplot(parms[2, runs.realistic], parms[2, runs.noReal], names = c('Incl.', 'Excl.'), ylab = 'S0_sd')

### Check geographic patterns, too:
df.red <- df.met[df.met$onset, ]
df.red <- attach_lat_long(df.red, countries)
df.red <- calculate_geo_patterns(df.red)
print(table(df.red$patternSig) / (length(runs.onset) * n))
print(table(df.red$real, df.red$patternSig))
print(table(df.red$patternSig[df.red$real]) / (length(runs.realistic) * n))

### Check parameters and initial states by pattern, as well:
df.red$run <- factor(df.red$run)

df.red$L = df.red$D = df.red$R0mx = df.red$R0diff = df.red$airScale = df.red$S0 = df.red$S0_sd = df.red$I0 = NA
for (ix in unique(df.red$run)) {
  ix <- as.numeric(as.character(ix))
  
  df.red$L[df.red$run == ix] <- parms[dim(parms)[1] - 4, ix]
  df.red$D[df.red$run == ix] <- parms[dim(parms)[1] - 3, ix]
  df.red$R0mx[df.red$run == ix] <- parms[dim(parms)[1] - 2, ix]
  df.red$R0diff[df.red$run == ix] <- parms[dim(parms)[1] - 1, ix]
  df.red$airScale[df.red$run == ix] <- parms[dim(parms)[1], ix]
  
  df.red$S0[df.red$run == ix] <- parms[1, ix]
  df.red$S0_sd[df.red$run == ix] <- parms[2, ix]
  
  for (jx in 1:n) {
    # df.red$S0[df.red$run == ix & df.red$country == countries[jx]] <- parms[jx, ix]
    df.red$I0[df.red$run == ix & df.red$country == countries[jx]] <- parms[jx + 2, ix]
  }
  
}
df.red$R0mn <- df.red$R0mx - df.red$R0diff

df.red$patternSig <- factor(df.red$patternSig, levels = levels(df.red$patternSig)[c(2, 1, 3:4)])

boxplot(L ~ patternSig, data = df.red)
boxplot(D ~ patternSig, data = df.red)
boxplot(R0mx ~ patternSig, data = df.red)
boxplot(R0diff ~ patternSig, data = df.red)
boxplot(airScale ~ patternSig, data = df.red)
boxplot(R0mn ~ patternSig, data = df.red)
# boxplot(S0 ~ patternSig, data = df.red)
# boxplot(S0_sd ~ patternSig, data = df.red)

boxplot(L ~ patternSig, data = df.red[df.red$real, ])
boxplot(D ~ patternSig, data = df.red[df.red$real, ])
boxplot(R0mx ~ patternSig, data = df.red[df.red$real, ])
boxplot(R0diff ~ patternSig, data = df.red[df.red$real, ])
boxplot(airScale ~ patternSig, data = df.red[df.red$real, ])
boxplot(R0mn ~ patternSig, data = df.red[df.red$real, ])
# boxplot(S0 ~ patternSig, data = df.red[df.red$real, ])
# boxplot(S0_sd ~ patternSig, data = df.red[df.red$real, ])

s0.by.count <- init.states[[3]]
df.red$S0.ind <- NA
for (run in unique(df.red$run)) {
  run <- as.numeric(as.character(run))
  for (jx in 1:n) {
    df.red$S0.ind[df.red$run == run & df.red$country == countries[jx]] <- s0.by.count[run, jx]
  }
}

ggplot(data = df.red, aes(x = patternSig, y = S0.ind)) + geom_boxplot(fill = 'gray95') + theme_classic() + facet_wrap(~ country)
ggplot(data = df.red[df.red$real, ], aes(x = patternSig, y = S0.ind)) + geom_boxplot(fill = 'gray95') + theme_classic() + facet_wrap(~ country)

# # What do the strongly w-e ones look like?
# which.we <- unique(df.red$run[df.red$patternSig == 'westToEast_yes'])
# par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in which.we) {
#   i <- as.numeric(as.character(i))
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }

# # Try to choose a few to use for fitting:
# par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in runs.realistic) {
#   i <- as.numeric(as.character(i))
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }

df.real <- df.red[df.red$real, ]
df.real <- df.real[, c(1:4, 7:14, 18:24)]

# par(mfrow = c(1, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in sort(as.numeric(as.character(unique(df.real$run[df.real$patternSig == 'westToEast_yes']))))) {
#   i <- as.numeric(as.character(i))
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }
# 
# par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in sort(as.numeric(as.character(unique(df.real$run[df.real$patternSig == 'westToEast_no']))))) {
#   list.ot <- df.real$ot[df.real$run == i]
#   if (length(list.ot[!is.na(list.ot)]) >= 11) {
#     print(i)
#   }
#   i <- as.numeric(as.character(i))
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }
# 
# par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in sort(as.numeric(as.character(unique(df.real$run[df.real$patternSig == 'eastToWest_no']))))) {
#   list.ot <- df.real$ot[df.real$run == i]
#   if (length(list.ot[!is.na(list.ot)]) >= 10) {
#     print(i)
#   }
#   i <- as.numeric(as.character(i))
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }

# if (!dir.exists('src/syntheticTests/outputs/')) {
#   dir.create('src/syntheticTests/outputs/')
# }
# pdf('outputs/synthetic_outbreaks_070220.pdf', height = 8, width = 28)
# par(mfrow = c(1, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in c(109, 3, 653, 694, 599)) {
#   matplot(t(res.rates[[i]]), pch = 20, type = 'b', lty = 1, col = viridis(n), main = i)
#   abline(v = c(13, 25))
# }
# dev.off()

# Look at colinearity between parameters in onset/realistic/pattern:
p1 <- ggplot(data = df.red, aes(x = R0mx, y = R0mn)) + geom_point() + theme_classic() + facet_wrap(~ patternSig, nrow = 1)
p2 <- ggplot(data = df.red, aes(x = R0mx, y = R0diff)) + geom_point() + theme_classic() + facet_wrap(~ patternSig, nrow = 1)
# ggplot(data = df.red, aes(x = R0diff, y = R0mn)) + geom_point() + theme_classic() + facet_wrap(~ patternSig)
# ggplot(data = df.red, aes(x = R0mx, y = D)) + geom_point() + theme_classic() + facet_wrap(~ patternSig)
# ggplot(data = df.red, aes(x = R0mx, y = L)) + geom_point() + theme_classic() + facet_wrap(~ patternSig)
# ggplot(data = df.red, aes(x = R0mx, y = airScale)) + geom_point() + theme_classic() + facet_wrap(~ patternSig)
grid.arrange(p1, p2)

plot(df.red[df.red$real, 18:23])

################################################################################################################
# Save relevant outbreaks so we can look at patterns:
if (!dir.exists('src/syntheticTests/syntheticData/')) {
  dir.create('src/syntheticTests/syntheticData/')
}
if (!dir.exists('src/syntheticTests/syntheticData/for_synthetic_testing/')) {
  dir.create('src/syntheticTests/syntheticData/for_synthetic_testing/')
}

synth.runs.RATES <- res.rates
save(synth.runs.RATES, file = 'src/syntheticTests/syntheticData/synth_rates_ALL_1000_070220.RData')

synth.runs.RATES.onset <- synth.runs.RATES[runs.onset]
synth.runs.RATES.realistic <- synth.runs.RATES[runs.realistic]

save(synth.runs.RATES.onset, file = 'src/syntheticTests/syntheticData/synth_rates_ONSET_1000_070220.RData')
save(synth.runs.RATES.realistic, file = 'src/syntheticTests/syntheticData/synth_rates_REALISTIC_1000_070220.RData')

# Save parameters and S0/I0, too:
s0.list <- parms[1:12, ]
i0.list <- parms[13:24, ]
parms.list <- parms[25:29, ]

s0.list <- list(s0.list, s0.list[, runs.onset], s0.list[, runs.realistic])
i0.list <- list(i0.list, i0.list[, runs.onset], i0.list[, runs.realistic])
parms.list <- list(parms.list, parms.list[, runs.onset], parms.list[, runs.realistic])

save(parms.list, file = 'src/syntheticTests/syntheticData/params_1000_070220.RData')
save(i0.list, file = 'src/syntheticTests/syntheticData/I0_1000_070220.Rdata')
save(s0.list, file = 'src/syntheticTests/syntheticData/S0_1000_070220.Rdata')

# And save only the 5 to be used in synthetic testing:
to.keep <- c(109, 3, 653, 694, 599)
synth.outbreaks <- synth.runs.RATES[to.keep]
synth.s <- s.rates[to.keep]
parms.outbreaks <- parms[, to.keep]

save(synth.outbreaks, file = 'src/syntheticTests/syntheticData/for_synthetic_testing/synth_rates_toKeep_070220.RData')
save(synth.s, file = 'src/syntheticTests/syntheticData/for_synthetic_testing/synth_S_toKeep_070220.RData')
save(parms.outbreaks, file = 'src/syntheticTests/syntheticData/for_synthetic_testing/parms_toKeep_070220.RData')

################################################################################################################
################################################################################################################
################################################################################################################



