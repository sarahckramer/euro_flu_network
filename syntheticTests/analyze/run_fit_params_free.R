
# Read in TRUE parameter values:
load('syntheticTests/syntheticData/params_07-14.RData')
select.parms <- select.parms[c(1, 6, 9, 13), ]
select.parms$L <- select.parms$L * 365
select.parms <- as.data.frame(cbind(rep(c(1, 6, 9, 13), 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

# Probably only need to look at the range of fit parameters from the main run using a loop for the countries and restricted S0:

# Read in fit params:
load('syntheticTests/outputs/cluster/071519/res_loop_S0range.RData')
o <- res[[2]]

# Get range of params for each run at times 15, 20:
o <- o[o$week %in% c(15, 20), ]
wk15 = wk20 = vector('list', 4)
for (outbreak in unique(o$outbreak)) {
  o.temp <- o[o$outbreak == outbreak, ]
  
  # for now, let's just try out the median values at both times
  quantile(o.temp$L[o.temp$week == 15])
  quantile(o.temp$L[o.temp$week == 20])
  
  wk15.vals = wk20.vals = c()
  for (j in c(7, 9, 11, 13, 15)) {
    wk15.vals <- c(wk15.vals, median(o.temp[o.temp$week == 15, j]))
    wk20.vals <- c(wk20.vals, median(o.temp[o.temp$week == 20, j]))
  }
  
  print(outbreak)
  print(select.parms[select.parms$outbreak == outbreak, 'value'][c(2, 1, 4, 5, 3)])
  print(wk15.vals)
  print(wk20.vals)
  print(''); print('')
  
  wk15[[which(unique(o$outbreak) == outbreak)]] <- wk15.vals
  wk20[[which(unique(o$outbreak) == outbreak)]] <- wk20.vals
}

# Do we need the inferred S0 as well, or can those be selected randomly?




# Using the inferred params (two sets for each outbreak), run forward with several draws from S/I:
set.seed(10489436)
source('code/SIRS_network.R')

dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

num_ens <- 100
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

S0_low <- 0.55; S0_up <- 0.90 # proportion of population
I0_low <- 0; I0_up <- 0.0001 # proportion of population # to 0.01% or 0.1%?

countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

param.bound <- cbind(c(rep(S0_low, n), rep(I0_low, n)), c(rep(S0_up, n), rep(I0_up, n)))
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
init.states.S <- parms[1:n, ]; init.states.I  <- parms[(n + 1):(2 * n), ]

# parms <- rbind(rep(wk15[[1]][1], num_ens), rep(wk15[[1]][2], num_ens), rep(wk15[[1]][3], num_ens), rep(wk15[[1]][4], num_ens), rep(wk15[[1]][5], num_ens))
# parms <- rbind(rep(wk20[[1]][1], num_ens), rep(wk20[[1]][2], num_ens), rep(wk20[[1]][3], num_ens), rep(wk20[[1]][4], num_ens), rep(wk20[[1]][5], num_ens))

outbreak <- 1
parms <- rbind(rep(select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'L'], num_ens),
               rep(select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'D'], num_ens),
               rep(select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mx'], num_ens),
               rep(select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mn'], num_ens),
               rep(select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'airScale'], num_ens))

beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[3, ] - parms[4, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[4, ix]) / parms[2, ix]
})
D.temp <- parms[2, ]; L.temp <- parms[1, ]; airScale.temp <- parms[5, ]

m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE,
                   prohibAir = FALSE)
})

nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}
# And make sure to run again for week 20!
# newI.c.15 <- newI.c
# newI.c.20 <- newI.c

# Plot outbreaks from above runs with the 'true' outbreak patterns:
load('syntheticTests/syntheticData/synth_07-14_RATES.RData')

par(mfrow = c(4, 5), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(countries)) {
  plot(synth.runs.RATES[[1]][i, ], pch = 20, type = 'b', xlab = 'Week', ylab = 'Syn+', main = countries[i])
  matlines(t(newI.c.15[[i]]), lty = 1, col = 'steelblue2')
  matlines(t(newI.c.20[[i]]), lty = 1, col = 'coral')
  matlines(t(newI.c[[i]]), lty = 1, col = 'gray80')
  lines(synth.runs.RATES[[1]][i, ], type = 'b', pch = 20, lwd = 2.0, cex = 1.25)
}

# Either find the inferred start S0, or else run 100 runs, but this time with the same random S0/I0 but the 'true' parameters, to see if that's any better?

# For outbreak 1, it looks like (and visual inspection is tricky), with random S0/I0, the fit params actually are more likely to produce a similar outbreak for
# many countries
    # Try looking at runs where country-level S0 and I0 are also the 'true' values




# If this highly depends on S0/I0, we'll have to determine inferred values there, too - but either way, we learn something - if dependent on S0,
# we know the model can find several patterns, depending on how it fits S0






