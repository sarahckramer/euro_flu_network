
### Run model to generate synthetic data for model testing ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(gridExtra); library(viridis)

# ##########################################################################################
# 
### Set seed
set.seed(10489436)

### Read in model function
source('code/SIRS_network.R')

### Global variables
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

### Set parameters
num_ens <- 500
tm_strt <- 273; tm_end <- 273 + 365 * 20 - 1; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end # should be length 7300 days, or 7300 / 365 = 20 years

### Parameter boundaries
D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
S0_low <- 0.55; S0_up <- 0.85
sd_low <- 0.05; sd_up <- 0.18
I0_low <- 0; I0_up <- 0.00005

# or do we want to start wider?
D_low <- 2; L_low <- 1*365; Rmx_low <- 1.5; Rdiff_low <- 0.0; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 3.0; Rdiff_up <- 1.5; airScale_up <- 1.25
S0_low <- 0.30; S0_up <- 0.90
sd_low <- 0.05; sd_up <- 0.20
I0_low <- 0; I0_up <- 0.0001
# ASK!

### Store boundaries
theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

# ### Specify the countries for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)
# count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
# 
# ### Set population sizes and # of countries used
# pop.size <- read.csv('data/popcounts_02-07.csv')
# pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
# pop.size <- pop.size[match(countries, pop.size$country), ]
# 
# ### Load commuting data
# load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
# t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
# t.comm <- t.comm[countries, countries]
# 
# ### Set country populations
# N <- t.comm; n <- length(countries) # w/ commuting
# diag(N) <- unlist(lapply(1:n, function(ix) {
#   pop.size$pop[ix] - rowSums(N)[ix]
# }))
# # note: this now results in more home-home people than before, since there are fewer countries to commute to
# 
# ### Read in humidity data
# ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
# AH <- rbind(ah[, count.indices], ah[, count.indices])
# for (i in 1:4) {
#   AH <- rbind(AH, AH)
# }
# AH <- AH[1:7665, ] # 21 years
# 
### Set initial conditions based on input parameters
param.bound <- cbind(c(S0_low, sd_low, rep(I0_low, n), theta_low),
                     c(S0_up, sd_up, rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))
# 
### Read in functions to run model/format results:
source('syntheticTests/synth_functions.R')
source('code/functions/Util.R')
# 
# ### Run model!
# res <- run_model(parms, AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, s0.method = 'dist', r0.mn = FALSE) # time: 50 ~ 30minutes
# # res <- run_model(parms, AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, s0.method = 'dist', r0.mn = TRUE)
# res.rates <- res[[1]][[1]]
# 
# ### Save raw outputs:
# # save(res.rates, file = 'syntheticTests/syntheticData/resRates_20yr_FULL_500.RData')
# 
# ### Remove the first 10 years for all runs:
# for (i in 1:num_ens) {
#   res.rates[[i]] <- res.rates[[i]][, 521:1043]
# }
# 
# # ### Visualize outputs:
# # par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# # for (i in 1:num_ens) {
# #   matplot(t(res.rates[[i]]), type = 'b', pch = 20, col = viridis(12), main = i, xlab = 'Time', ylab = 'Inc.')
# #   # abline(v = seq(0, 1040, by = 52), lty = 2)
# #   abline(v = seq(0, 520, by = 52), lty = 2)
# # }
# 
# ### Save outputs with last 10 years only:
# # save(res.rates, file = 'syntheticTests/syntheticData/resRates_20yr_last10_500.RData')
load('syntheticTests/syntheticData/resRates_20yr_last10_500.RData')

### Filter through to determine which runs are producing patterns of interest:

# First, any onsets in any of the 10 years:
any.onsets = no.onsets = c()
for (i in 1:length(res.rates)) {
  res.temp <- t(res.rates[[i]])
  # matplot(res.temp, type = 'b', pch = 20, col = viridis(12))
  colnames(res.temp) <- countries
  
  ots = pts = c()
  for (count.index in 1:length(countries)) {
    ots <- c(ots, findOnset(res.temp[, count.index], 500)$onset + 40 - 1)
  }
  
  if (all(is.na(ots))) {
    no.onsets <- c(no.onsets, i)
  } else {
    any.onsets <- c(any.onsets, i)
  }
  
}
# 378 have any onsets at all; 122 have none
parms.noOnsets <- parms[, no.onsets]
# save(parms.noOnsets, file = 'syntheticTests/syntheticData/parms_noOnsets.RData')

# Any onsets in all 10 years:
yr.breaks <- list(1:52, 53:104, 105:156, 157:208, 209:260, 261:312, 313:364, 365:416, 417:468, 469:520)
onAll10 = onSporadic = c()
df.main <- NULL
for (i in any.onsets) {
  res.temp <- t(res.rates[[i]])
  # matplot(res.temp, type = 'b', pch = 20, col = viridis(12))
  colnames(res.temp) <- countries
  
  df <- NULL
  for (yr in 1:10) {
    for (count.index in 1:length(countries)) {
      out.temp <- res.temp[yr.breaks[[yr]], count.index]
      
      ot.temp <- findOnset(out.temp, 500)$onset + 40 - 1
      pt.temp <- which.max(out.temp) + 40 - 1
      
      df <- rbind(df, c(countries[count.index], yr, ot.temp, pt.temp))
    }
  }
  df <- as.data.frame(df)
  names(df) <- c('country', 'year', 'ot', 'pt')
  df$ot <- as.numeric(as.character(df$ot)); df$pt <- as.numeric(as.character(df$pt))
  df$pt[is.na(df$ot)] <- NA
  df$run <- i
  
  df.main <- rbind(df.main, df)
  
  each.yr.true <- c()
  for (yr in 1:10) {
    if (all(is.na(df$ot[df$year == yr]))) {
      each.yr.true <- c(each.yr.true, F)
    } else {
      each.yr.true <- c(each.yr.true, T)
    }
  }
  
  if (all(each.yr.true)) {
    onAll10 <- c(onAll10, i)
  } else {
    onSporadic <- c(onSporadic, i)
  }
  
}
# 168 have onsets every year; 210 are more sporadic
parms.sporadic <- parms[, onSporadic]
# save(parms.sporadic, file = 'syntheticTests/syntheticData/parms_onSporadic.RData')

# At least 8/12 countries have onsets in all 10 years:
onsetsCheck = onsetsFew = c()
for (i in onAll10) {
  res.temp <- t(res.rates[[i]])
  # matplot(res.temp, type = 'b', pch = 20, col = viridis(12))
  colnames(res.temp) <- countries
  
  df <- df.main[df.main$run == i, ]
  
  each.yr.true <- c()
  for (yr in 1:10) {
    df.temp <- df[df$year == yr, ]
    
    if (length(which(is.na(df.temp$ot))) < 5) { # at least 8/12 countries have onsets
      each.yr.true <- c(each.yr.true, T)
    } else {
      each.yr.true <- c(each.yr.true, F)
    }
  }
  
  if (all(each.yr.true)) {
    onsetsCheck <- c(onsetsCheck, i)
  } else {
    onsetsFew <- c(onsetsFew, i)
  }
  
}
# 118 check out; only 50 have too few some years
parms.tooFew <- parms[, onsetsFew]
# save(parms.tooFew, file = 'syntheticTests/syntheticData/parms_onFew.RData')

# Limit df.main to only those with appropriate onsets:
df.main <- df.main[df.main$run %in% onsetsCheck, ]
parms.crit1 <- parms[, onsetsCheck]
# save(parms.crit1, file = 'syntheticTests/syntheticData/parms_Criteria1.RData')

# # Visualize outbreaks:
# pdf('syntheticTests/outputs/explore/outbreak_plots_criteria1.pdf', width = 16, height = 10)
# par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (i in onsetsCheck) {
#   matplot(t(res.rates[[i]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = i, xlab = 'Time', ylab = 'Inc.')
#   abline(v = seq(0, 520, by = 52), lty = 2)
# }
# dev.off()

# Quick parameter comparison:
parms.noOnsets <- as.data.frame(t(parms.noOnsets[c(1:2, 15:19), ]))
parms.sporadic <- as.data.frame(t(parms.sporadic[c(1:2, 15:19), ]))
parms.tooFew <- as.data.frame(t(parms.tooFew[c(1:2, 15:19), ]))
parms.crit1 <- as.data.frame(t(parms.crit1[c(1:2, 15:19), ]))

parms.noOnsets$group <- 'No Onsets'
parms.sporadic$group <- 'Sporadic On.'
parms.tooFew$group <- 'Too Few Yrly. On.'
parms.crit1$group <- 'Meet Crit. 1'

parms.df <- rbind(parms.noOnsets, parms.sporadic, parms.tooFew, parms.crit1)
names(parms.df) <- c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'aScale', 'group')

parms.df$group <- factor(parms.df$group)
parms.df$group <- factor(parms.df$group, levels = levels(parms.df$group)[c(2:4, 1)])

pdf('syntheticTests/outputs/explore/param_comp_criteria1.pdf', width = 16, height = 10)
par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(aScale ~ group, data = parms.df, xlab = '', col = 'gray95')
# biggest players are L (cap ~4.74 yrs, or just do 1-5 years), R0mx, R0diff
dev.off()

# Do they sync into the winter time?:
# So remove those with late (past week 19) peaks, or that onset "at" 40
df.main$run <- factor(df.main$run)

ot.early = pt.early = summer.out =c()

for (run in levels(df.main$run)) {
  df <- df.main[df.main$run == run, ]
  
  # if (any(df$ot == 40 & !is.na(df$ot))) {
  #   print(run)
  #   print(summary(df$ot))
  #   print(summary(df$pt))
  #   # check: 333, 304, 275, 268, 70, 36, 2
  #   # all but 268 and 36 also have very early peaks
  # }
  
  if (all(df$ot == 40 | is.na(df$ot))) {
    ot.early <- c(ot.early, run)
  }
  if (all(df$pt < 52 | is.na(df$pt))) {
    pt.early <- c(pt.early, run)
  }
  # look into: 124, 232, 240, 424, 460, 498 (but all very, very small fluctuations rather than real "outbreaks")
  
  if (any(df$pt > 72 & !is.na(df$pt))) {
    # print(run)
    summer.out <- c(summer.out, run)
  }
  # 36, 69, 129, 166, 268, 306, 319, 361, 467
  # depends on the year: 69, 306, 319, 361, 467
  # small summer followed by large winter: 36, 129, 166, 268
  
}
too.early <- unique(c(ot.early, pt.early))
# 31 too early, 9 too late

# parms.early <- parms[, as.numeric(as.character(too.early))]
# # save(parms.early, file = 'syntheticTests/syntheticData/parms_earlyOutbreaks.RData')

par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in summer.out) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
}
# remove the ones that have a small(er) summer-large winter, double outbreak pattern:
double.out <- c(36, 129, 166, 268, 467) # note that, for some of these, the "winter" outbreak is pushed pretty late
summer.out <- c(69, 306, 319, 361) # those with spring/summer peaks consistently

parms.double <- parms[, double.out]
parms.late <- parms[, summer.out]

# save(parms.double, file = 'syntheticTests/syntheticData/parms_doubleOutbreaks.RData')
# save(parms.late, file = 'syntheticTests/syntheticData/parms_lateOutbreaks.RData')

df.main <- df.main[!(df.main$run %in% c(too.early, double.out, summer.out)), ]
df.main$run <- factor(df.main$run)

# Check attack rates before proceeding:
df.main$ar <- NA
for (run in levels(df.main$run)) {
  res.temp <- t(res.rates[[as.numeric(as.character(run))]])
  
  for (yr in 1:10) {
    for (count.index in 1:length(countries)) {
      out.temp <- res.temp[yr.breaks[[yr]], count.index]
      df.main$ar[df.main$run == run & df.main$year == yr & df.main$country == countries[count.index]] <- sum(out.temp)
    }
  }
  
}
print(summary(df.main$ar)) # about right - tends to be between 10 and 40% of the population - not quite our 15-50%, but acceptable

# Look at patterns of peak timings:
crit2 = too.late = c()
for (run in levels(df.main$run)) {
  df <- df.main[df.main$run == run, ]
  
  crit2.temp <- c()
  for (yr in 1:10) {
    df.temp <- df[df$year == yr, ]
    
    if (length(df.temp$pt[!is.na(df.temp$pt) & !(df.temp$pt %in% 52:64)]) < 2) { # want 11/12 countries to fall into this range
      crit2.temp <- c(crit2.temp, T)
    } else {
      crit2.temp <- c(crit2.temp, F)
    }
  }
  
  print(run)
  print(crit2.temp)
  
  if (all(crit2.temp)) {
    crit2 <- c(crit2, run)
  } else {
    too.late <- c(too.late, run)
  }
  # this is temporary! need to explore!
  
}
# require all T: 16 work, 62 don't
# require any T: 24 work, 54 don't
# Any: 3, 26, 32, 83, 138, 140, 155, 162, 173, 174, 204, 222, 235, 252, 265, 271, 281, 283, 293, 357, 387, 421, 459, 475
# All: 3, 83, 155, 162, 204, 235, 252, 265, 271, 281, 283, 293, 357, 387, 459, 475 (all the best are included here)
# Removed: 26, 32, 138, 140, 173, 174, 222, 421
borderline <- c(26, 32, 138, 140, 173, 174, 222, 421)
# Still rare to find outbreaks where everything is well in this range; best are: 3, 83 (but pushing it), 162, 235, 265 (but small), 271, 293, 387, 459
# And even then, why so much synchrony?

# Plot out to explore:
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in too.late) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(12, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(24, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}
# these really all are too late, except 275, 292

pdf('syntheticTests/outputs/explore/outbreak_plots_criteria2.pdf', width = 16, height = 10)
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in crit2) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(12, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(24, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}
dev.off()

par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in borderline) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(12, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(24, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}
# honestly these are verging on too late anyway


too.early <- c(too.early, 275, 292)
too.late <- too.late[!(too.late %in% c(275, 292))]
too.late <- c(too.late, summer.out)

parms.late <- parms[, as.numeric(as.character(too.late))]
parms.early <- parms[, as.numeric(as.character(too.early))]

# save(parms.early, file = 'syntheticTests/syntheticData/parms_earlyOutbreaks.RData')
# save(parms.late, file = 'syntheticTests/syntheticData/parms_lateOutbreaks.RData')

df.main <- df.main[df.main$run %in% crit2, ]
df.main$run <- factor(df.main$run)

# Update parms.df:
parms.double <- as.data.frame(t(parms.double[c(1:2, 15:19), ]))
parms.late <- as.data.frame(t(parms.late[c(1:2, 15:19), ]))
parms.early <- as.data.frame(t(parms.early[c(1:2, 15:19), ]))

parms.crit2 <- parms[, as.numeric(as.character(crit2))]
parms.crit2 <- as.data.frame(t(parms.crit2[c(1:2, 15:19), ]))

parms.double$group <- 'Double Out.'
parms.late$group <- 'Too Late'
parms.early$group <- 'Too Early'
parms.crit2$group <- 'Meet Crit. 2'

names(parms.double) = names(parms.late) = names(parms.early) = names(parms.crit2) = c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'aScale', 'group')

parms.df <- rbind(parms.df, parms.double, parms.late, parms.early, parms.crit2)
parms.df$group <- factor(parms.df$group)

parms.df.all <- parms.df
parms.df.desc <- parms.df[parms.df$group != 'Meet Crit. 1', ]; parms.df.desc$group <- factor(parms.df.desc$group)
parms.df.crit1 <- parms.df[parms.df$group %in% levels(parms.df$group)[5:8], ]; parms.df.crit1$group <- factor(parms.df.crit1$group)

# Plot parameter ranges:
# par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# boxplot(S0_mean ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(S0_sd ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(L ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(D ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(R0mx ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(R0diff ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# boxplot(aScale ~ group, data = parms.df.all, xlab = '', col = 'gray95')
# 
# par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# boxplot(S0_mean ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(S0_sd ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(L ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(D ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(R0mx ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(R0diff ~ group, data = parms.df.desc, xlab = '', col = 'gray95')
# boxplot(aScale ~ group, data = parms.df.desc, xlab = '', col = 'gray95')

pdf('syntheticTests/outputs/explore/param_comp_criteria2.pdf', width = 16, height = 10)
par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(aScale ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
# correct timing still seems to want higher D; but low-ish R0diff seems to be most important
dev.off()

# Look at geographical patterns:
synch.ot = synch.pt = c() # number of weeks between latest and earliest onset/peak
for (run in levels(df.main$run)) {
  # print(run)
  
  for (yr in 1:10) {
    df.temp <- df.main[df.main$run == run & df.main$year == yr, ]
    # print(df.temp$pt)
    # print(max(df.temp$ot) - min(df.temp$ot)) # for some outbreaks, onsets have a bit more range
    synch.ot <- c(synch.ot, (max(df.temp$ot) - min(df.temp$ot)))
    synch.pt <- c(synch.pt, (max(df.temp$pt) - min(df.temp$pt)))
  }
  
}

p1 <- ggplot() + geom_histogram(aes(x = synch.ot), bins = 6, col = 'black', fill = 'gray90') + theme_classic() +
  labs(x = 'Onset Range (Latest - Earliest)', y = 'Count') + scale_x_continuous(breaks = 0:5)
p2 <- ggplot() + geom_histogram(aes(x = synch.pt), bins = 5, col = 'black', fill = 'gray90') + theme_classic() +
  labs(x = 'Peak Range (Latest - Earliest)', y = 'Count') + scale_x_continuous(breaks = 0:4)
grid.arrange(p1, p2, ncol = 1)
# vast majority are only 1-2 weeks away from each other

# At this point, I'm not sure there's any need to assess geographic patterns?

















