
library(reshape2)

# Read in results:
m <- read.csv('syntheticTests/outputs/cluster/072319/outputMet_loop_reduceS0_reprobeEven.csv')
o <- read.csv('syntheticTests/outputs/cluster/072319/outputOP_loop_reduceS0_reprobeEven.csv')
oStates <- read.csv('syntheticTests/outputs/cluster/072319/outputOPStates_loop_reduceS0_reprobeEven.csv')

# Check what combos ran:
table(m$oev_base, m$oev_denom)

# Remove combos that lead to collapse:
m$group <- paste(m$oev_base, m$oev_denom, m$lambda, sep = '_'); m$group <- factor(m$group)
o$group <- paste(o$oev_base, o$oev_denom, o$lambda, sep = '_'); o$group <- factor(o$group)
oStates$group <- paste(oStates$oev_base, oStates$oev_denom, oStates$lambda, sep = '_'); oStates$group <- factor(oStates$group)

# o <- o[o$group %in% levels(m$group)[c(1, 9, 13:16, 18:24)], ]; o$group <- factor(o$group)
# oStates <- oStates[oStates$group %in% levels(m$group)[c(1, 9, 13:16, 18:24)], ]; oStates$group <- factor(oStates$group)
# m <- m[m$group %in% levels(m$group)[c(1, 9, 13:16, 18:24)], ]; m$group <- factor(m$group)
# o <- o[o$group %in% levels(m$group)[c(1:3, 5:6, 9:10, 13:14, 17:18, 21:22)], ]; o$group <- factor(o$group)
# oStates <- oStates[oStates$group %in% levels(m$group)[c(1:3, 5:6, 9:10, 13:14, 17:18, 21:22)], ]; oStates$group <- factor(oStates$group)
# m <- m[m$group %in% levels(m$group)[c(1:3, 5:6, 9:10, 13:14, 17:18, 21:22)], ]; m$group <- factor(m$group)
# S0 range: 1e4/5/1, 1e4/10/1 (maybe), 1e5/5/all, 1e5/10/all, 1e5/20/1.01-1.05 (1e5 way less likely to fail)
# I0 range: 1e4/5/1-1.01, 1e4/10/1-1.03, 1e4/20/1-1.01, 1e5/5/1-1.01, 1e5/10/1-1.01, 1e5/20/1-1.01

################################################################################################################################################################################################
################################################################################################################################################################################################

# Calculate "group" factors:
oStates$group <- paste(oStates$oev_base, oStates$oev_denom, sep = '_'); oStates$group <- factor(oStates$group)
oStates$group <- factor(oStates$group, levels = levels(oStates$group)[c(3, 1:2, 6, 4:5)])
oStates$group.plot <- paste(oStates$run, oStates$oev_base, oStates$oev_denom, oStates$lambda, sep = '_'); oStates$group.plot <- factor(oStates$group.plot)
oStates$lambda <- factor(oStates$lambda)

o$group <- paste(o$oev_base, o$oev_denom, sep = '_'); o$group <- factor(o$group)
o$group <- factor(o$group, levels = levels(o$group)[c(3, 1:2, 6, 4:5)])
o$group.plot <- paste(o$outbreak, o$run, o$oev_base, o$oev_denom, o$lambda, sep = '_'); o$group.plot <- factor(o$group.plot)
o$lambda <- factor(o$lambda)

# Get countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

# Get outbreaks:
to.keep <- c(1, 6, 9, 13)

# Calculate parameter error:
load('syntheticTests/syntheticData/params_07-14.RData')
select.parms <- select.parms[to.keep, ]
select.parms$L <- select.parms$L * 365
select.parms <- as.data.frame(cbind(rep(c(1, 6, 9, 13), 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')

# Read in TRUE values of beta, R0, Re at each time point:
load('syntheticTests/outputs/cluster/071519/true_betaR0Re.RData')
true.betas <- true.epi.params[[1]]
true.R0 <- true.epi.params[[2]]
true.Re <- true.epi.params[[3]]
rm(true.epi.params)

ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, c(1:8, 10:21)], ah[, c(1:8, 10:21)])

tm_strt <- 273; tm_end <- 573; tm_step <- 1 #; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end
tmstep <- 7
beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + tmstep), by = tmstep)

AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)

# Calculate SIMULATED values of beta, R0, Re at each time point:
oStates$beta = oStates$R0 = oStates$Re = NA
o$oev_base <- factor(o$oev_base); o$oev_denom <- factor(o$oev_denom)

for (i in 1:length(to.keep)) {
  print(i)

  for (j in 1:length(unique(o$run))) {
    print(j)

    for (oev_base in levels(o$oev_base)) {
      for (oev in levels(o$oev_denom)) {
        for (lambda in levels(o$lambda)) {

          o.temp <- o[o$outbreak == to.keep[i] & o$run == j & o$oev_base == oev_base & o$oev_denom == oev & o$lambda == lambda, ]
          oStates.temp <- oStates[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda, ]

          b <- log(o.temp$R0max - o.temp$R0min); a <- -180

          if (length(o.temp$outbreak) > 0) {
            for (country in 1:n) {
              # print(country)
              oStates$R0[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <- exp(a * AHpt[, country] + b) + o.temp$R0min
              oStates$beta[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <-
                oStates$R0[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] / o.temp$D
              oStates$Re[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] <-
                oStates$R0[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]] *
                oStates$S[oStates$outbreak == to.keep[i] & oStates$run == j & oStates$oev_base == oev_base & oStates$oev_denom == oev & oStates$lambda == lambda & oStates$country == countries[country]]
            }
          }

        }
      }
    }
  }
}

# Calculate PARAMETER error at three time points:
o.err <- o[o$week %in% c(10, 15, 20), c(1:7, 9, 11, 13, 15)]
# o.err <- o[o$week == 20, c(1:7, 9, 11, 13, 15)]
o.err$L.err = o.err$D.err = o.err$R0mx.err = o.err$R0mn.err = o.err$aS.err = NA

# First, subtract actual - fit, and divide by observed
for (outbreak in to.keep) {
  o.err$L.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'L'] -
       o.err$L[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'L']
  
  o.err$D.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'D'] -
       o.err$D[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'D']
  
  o.err$R0mx.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mx'] -
       o.err$R0max[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mx']
  
  o.err$R0mn.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mn'] -
       o.err$R0min[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'R0mn']
  
  o.err$aS.err[o.err$outbreak == outbreak] <-
    (select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'airScale'] -
       o.err$airScale[o.err$outbreak == outbreak]) * -1 /
    select.parms$value[select.parms$outbreak == outbreak & select.parms$parameter == 'airScale']
}

o.err$oev_denom <- factor(o.err$oev_denom)
o.err$oev_base <- factor(o.err$oev_base)

# Calculate relative error for S, beta, R0, Re for each country at t = 10, 15, 20:
load('syntheticTests/syntheticData/synth_07-14_S.RData')
synth.runs.S <- synth.S.RATES[to.keep]

oStates.err <- oStates[oStates$week %in% c(10, 15, 20), c(1:8, 10:14)]

oStates.err$Re.err = oStates.err$R0.err = oStates.err$beta.err = oStates.err$S.err = NA
oStates.err$S <- oStates.err$S * 100000
oStates.err$oev_denom <- factor(oStates.err$oev_denom)
oStates.err$oev_base <- factor(oStates.err$oev_base)

for (outbreak in 1:length(to.keep)) {
  print(outbreak)
  susc_i <- synth.runs.S[[outbreak]]
  beta.temp <- true.betas[[outbreak]]
  R0.temp <- true.R0[[outbreak]]
  Re.temp <- true.Re[[outbreak]]
  
  for (run in 1:length(unique(oStates.err$run))) {
    for (oev_base in levels(oStates.err$oev_base)) {
      for (oev in levels(oStates.err$oev_denom)) {
        for (lambda in levels(oStates.err$lambda)) {
          
          for (count.index in 1:n) {
            oStates.err.temp <- oStates.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$oev_base == oev_base & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index], ]
            
            if (length(oStates.err.temp$country) > 0) {
              
              oStates.err$S.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
                (-1 * (susc_i[c(10, 15, 20), count.index] - oStates.err.temp$S)) / susc_i[c(10, 15, 20), count.index]
              oStates.err$beta.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
                (-1 * (beta.temp[c(10, 15, 20), count.index] - oStates.err.temp$beta)) / beta.temp[c(10, 15, 20), count.index]
              oStates.err$R0.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
                (-1 * (R0.temp[c(10, 15, 20), count.index] - oStates.err.temp$R0)) / R0.temp[c(10, 15, 20), count.index]
              oStates.err$Re.err[oStates.err$outbreak == to.keep[outbreak] & oStates.err$run == run & oStates.err$oev_denom == oev & oStates.err$lambda == lambda & oStates.err$country == countries[count.index]] <-
                (-1 * (Re.temp[c(10, 15, 20), count.index] - oStates.err.temp$Re)) / Re.temp[c(10, 15, 20), count.index]
            }
            
          }
          
        }
      }
    }
  }
}
oStates.err$outbreak <- factor(oStates.err$outbreak)

# Write new files as list (m, o, o.err, oStates, oStates.err)
res <- list(m, o, o.err, oStates, oStates.err)
save(res, file = 'syntheticTests/outputs/cluster/072319/res_loop_S0range_reprobeEven.RData')

# true.epi.params <- list(true.betas, true.R0, true.Re)
# save(true.epi.params, file = 'syntheticTests/outputs/cluster/071519/true_betaR0Re.RData')

rm(list=ls())




