
### Run model to generate synthetic data for model testing ###

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(gridExtra); library(viridis)

# ##########################################################################################

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

### Specify the countries for which we are performing a forecast
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
n <- length(countries)
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))
# note: this now results in more home-home people than before, since there are fewer countries to commute to

### Read in humidity data
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])
for (i in 1:4) {
  AH <- rbind(AH, AH)
}
AH <- AH[1:7665, ] # 21 years

### Set initial conditions based on input parameters
param.bound <- cbind(c(S0_low, sd_low, rep(I0_low, n), theta_low),
                     c(S0_up, sd_up, rep(I0_up, n), theta_up))
parms <- t(lhs(num_ens, param.bound))

### RUN STOCHASTICALLY:
discrete <- TRUE

### Read in functions to run model/format results:
source('syntheticTests/synth_functions.R')
source('code/functions/Util.R')

### Run model!
init.states <- allocate_S0I0(parms, num_ens, n, N, s0.method = 'dist')
res <- run_model(parms, init.states[[1]], init.states[[2]], AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, r0.mn = FALSE) # time: 50 ~ 30minutes
res.rates <- res[[1]]

# ### Save raw outputs:
# # save(res.rates, file = 'syntheticTests/syntheticData/resRates_20yr_FULL_500.RData')
# 
# ### Remove the first 10 years for all runs:
# for (i in 1:num_ens) {
#   res.rates[[i]] <- res.rates[[i]][, 521:1043]
# }

### Visualize outputs:
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:num_ens) {
  matplot(t(res.rates[[i]]), type = 'b', pch = 20, col = viridis(12), main = i, xlab = 'Time', ylab = 'Inc.')
  # abline(v = seq(0, 1040, by = 52), lty = 2)
  abline(v = seq(0, 1040, by = 52), lty = 2)
}

# ### Save outputs with last 10 years only:
# # save(res.rates, file = 'syntheticTests/syntheticData/resRates_20yr_last10_500.RData')
load('syntheticTests/syntheticData/resRates_20yr_last10_500.RData')

######################################################################################################################################################

### Look at pattern over 10 years for each of 500 deterministic runs:
yr.breaks <- list(1:52, 53:104, 105:156, 157:208, 209:260, 261:312, 313:364, 365:416, 417:468, 469:520)
get_10yr_avg <- function(obs) {
  obs.avg <- matrix(0, nrow = nrow(obs), ncol = 52)
  for (yr in 1:10) {
    obs.avg <- obs.avg + obs[, yr.breaks[[yr]]]
  }
  obs.avg <- obs.avg / 10
  return(obs.avg)
}
res.avg <- lapply(res.rates, get_10yr_avg)

# Visualize:
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in 1:500) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(13, 25), lty = 1, col = 'red')
}

# How many with average peak within 52:64? (13:25)
df <- NULL
for (i in 1:500) {
  res.temp <- t(res.avg[[i]])
  for (count.index in 1:length(countries)) {
    pt <- which(res.temp[, count.index] == max(res.temp[, count.index]))
    if (length(pt) > 1) {
      pt <- NA
    }
    df <- rbind(df, c(i, countries[count.index], pt))
  }
}
df <- as.data.frame(df)
names(df) <- c('run', 'country', 'pt')
df$pt <- as.numeric(as.character(df$pt))

# Remove if all 0:
runs.to.remove <- c()
for (run in 1:500) {
  df.temp <- df[df$run == run, ]
  if (all(is.na(df.temp$pt))) {
    runs.to.remove <- c(runs.to.remove, run)
  }
}
df <- df[!(df$run %in% runs.to.remove), ]
df$run <- factor(df$run)

# Now check that within 13:25:
runs.in.range <- c()
for (run in levels(df$run)) {
  df.temp <- df[df$run == run, ]
  if (length(df.temp$pt[df.temp$pt %in% 13:25]) >= 10) {
    runs.in.range <- c(runs.in.range, run)
  }
}
# 92 / 500 have average peaks in this range

df <- df[df$run %in% runs.in.range, ]
df$run <- factor(df$run)

# Visualize just these:
runs.in.range <- sort(as.numeric(as.character(runs.in.range)))
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in runs.in.range) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(13, 25), lty = 1, col = 'red')
}

# Compare params:
parms.no <- as.data.frame(t(parms[c(1:2, 15:19), (1:500)[!((1:500) %in% runs.in.range)]]))
parms.yes <- as.data.frame(t(parms[c(1:2, 15:19), runs.in.range]))

colnames(parms.no) = colnames(parms.yes) = c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')

# parms.no <- melt(parms.no); parms.yes <- melt(parms.yes)
# names(parms.no) = names(parms.yes) = c('run', 'param', 'value')
parms.no$group <- 'No'; parms.yes$group <- 'Yes'

parms.df <- rbind(parms.yes, parms.no)
parms.df$group <- factor(parms.df$group)

par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(airScale ~ group, data = parms.df, xlab = '', col = 'gray95')
# not seeing any clear differences here

# Add longitudes:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy', 'Luxembourg',
                   'Netherlands', 'Poland', 'Slovakia', 'Spain')

world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)

levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'lat', 'long')]
df <- merge(df, world.cities, by.x = 'country', by.y = 'country.etc')

# Now look at geographical direction of these outbreaks:
df$corr = df$pval = NA
for (run in levels(df$run)) {
  corr.dat <- cor.test(df$long[df$run == run], df$pt[df$run == run], method = 'kendall')
  df$corr[df$run == run] <- corr.dat$estimate
  df$pval[df$run == run] <- corr.dat$p.value
  # print(corr.dat$estimate)
}
# one run (124) with a simultaneous peak, but really it's just a very small oscillation
df <- df[df$run != '124', ]
df$run <- factor(df$run)

df$pattern <- ifelse(df$corr > 0, 'westToEast', 'eastToWest')

df$pattern2 <- df$pattern
df$pattern2[df$pval > 0.05] <- 'noPatt'

df$sig <- ifelse(df$pval < 0.05, 'yes', 'no')

df$pattern <- factor(df$pattern)
df$pattern2 <- factor(df$pattern2)
df$sig <- factor(df$sig)

df$patternSig <- paste(df$pattern, df$sig, sep = '_')
df$patternSig <- factor(df$patternSig)

table(df$pattern) / 12 # 23/91 (~25%)
table(df$pattern2) / 12 # majority not sig (67/91); 6 are sig w-e, 18 sig e-w
table(df$patternSig) / 12 # most e-w/no
# 6/91 (~6.6%) sig w-e isn't bad

runs.ew.sig <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'eastToWest_yes']))))
runs.ew.not <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'eastToWest_no']))))
runs.we.not <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'westToEast_no']))))
runs.we.sig <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'westToEast_yes']))))

pdf('syntheticTests/outputs/explore/outbreak_plots_and_averages_west-to-east.pdf', width = 16, height = 10)
# Plot out w-e only:
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in c(runs.we.sig, runs.we.not)) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(13, 25), lty = 1, col = 'red')
}
# first 6 are sig; some of them don't have much of an outbreak pattern, and others are pretty synchronous

# Plot out full ranges for these:
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in c(runs.we.sig, runs.we.not)) {
  matplot(t(res.rates[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(13, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(25, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}
dev.off()

# Compare parameters based on average geographical pattern:
parms.ew.sig <- as.data.frame(t(parms[c(1:2, 15:19), runs.ew.sig]))
parms.ew.not <- as.data.frame(t(parms[c(1:2, 15:19), runs.ew.not]))
parms.we.not <- as.data.frame(t(parms[c(1:2, 15:19), runs.we.not]))
parms.we.sig <- as.data.frame(t(parms[c(1:2, 15:19), runs.we.sig]))

colnames(parms.ew.sig) = colnames(parms.ew.not) = colnames(parms.we.not) = colnames(parms.we.sig) = c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')
parms.ew.sig$group <- 'E-W Sig'; parms.ew.not$group <- 'E-W Not'; parms.we.not$group <- 'W-E Not'; parms.we.sig$group <- 'W-E Sig'

parms.df2 <- rbind(parms.ew.sig, parms.ew.not, parms.we.not, parms.we.sig)
parms.df2$group <- factor(parms.df2$group)
parms.df2$group <- factor(parms.df2$group, levels = levels(parms.df2$group)[c(2, 1, 3:4)])

pdf('syntheticTests/outputs/explore/param_comp_geo.pdf', width = 16, height = 10)
par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(airScale ~ group, data = parms.df2, xlab = '', col = 'gray95')
dev.off()

######################################################################################################################################################

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

# Any onsets in 3/10 years:
onAtLeast3 = onSporadic = c()
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
  
  if (length(each.yr.true[each.yr.true]) >= 3) { # at least 3 years with ANY onsets
    onAtLeast3 <- c(onAtLeast3, i)
  } else {
    onSporadic <- c(onSporadic, i)
  }
  
}
# 168 have onsets every year; 210 are more sporadic
# but 341 have onsets in at least 3 years; 37 more sporadic
parms.sporadic <- parms[, onSporadic]
# save(parms.sporadic, file = 'syntheticTests/syntheticData/parms_onSporadic2.RData')

# At least 8/12 countries have onsets in 3/10 years:
onsetsCheck = onsetsFew = c()
for (i in onAtLeast3) {
  res.temp <- t(res.rates[[i]])
  # matplot(res.temp, type = 'b', pch = 20, col = viridis(12))
  colnames(res.temp) <- countries
  
  df <- df.main[df.main$run == i, ]
  
  each.yr.true <- c()
  for (yr in 1:10) {
    df.temp <- df[df$year == yr, ]
    
    if (length(which(is.na(df.temp$ot))) < 3) { # at least 10/12 countries have onsets
      each.yr.true <- c(each.yr.true, T)
    } else {
      each.yr.true <- c(each.yr.true, F)
    }
  }
  
  if (length(each.yr.true[each.yr.true]) >= 3) {
    onsetsCheck <- c(onsetsCheck, i)
  } else {
    onsetsFew <- c(onsetsFew, i)
  }
  
}
# 118 check out; only 50 have too few some years
# but 291 check out if only 3 expected; only 50 have too few
# if 10/12, 263 check out, 78 too few
parms.tooFew <- parms[, onsetsFew]
# save(parms.tooFew, file = 'syntheticTests/syntheticData/parms_onFew2_10of12.RData')

# Limit df.main to only those with appropriate onsets:
df.main <- df.main[df.main$run %in% onsetsCheck, ]
parms.crit1 <- parms[, onsetsCheck]
# save(parms.crit1, file = 'syntheticTests/syntheticData/parms_Criteria1_3of10_10of12.RData')

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

pdf('syntheticTests/outputs/explore/param_comp_criteria1_3of10_10of12.pdf', width = 16, height = 10)
par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(aScale ~ group, data = parms.df, xlab = '', col = 'gray95')
# biggest players are L (cap ~4.74 yrs, or just do 1-5 years), R0mx, R0diff
# 3/10: still selects for lower L, but not as strongly; also for higher R0mx (although both still span full ranges)
dev.off()

# Remove individual years from df.main when onset not in at least 10/12 countries:
df.main$run <- factor(df.main$run)

# runs.and.years <- NULL
for (run in levels(df.main$run)) {
  for (yr in 1:10) {
    df <- df.main[df.main$run == run & df.main$year == yr, ]
    if (length(df$ot[!is.na(df$ot)]) < 10) {
      # runs.and.years <- rbind(runs.and.years, c(run, yr))
      df.main <- df.main[!(df.main$run == run & df.main$year == yr), ]
    }
  }
}

# Which years have at least 10/12 PT in "realistic" range (vs. too early or too late)?:
crit2 = too.late = too.early = out.double = c()
for (run in levels(df.main$run)) {
  df <- df.main[df.main$run == run, ]
  
  crit2.temp <- c()
  
  for (yr in 1:10) {
    df.temp <- df[df$year == yr, ]
    
    if (length(df.temp$country) > 0) {
      pt.temp <- df.temp$pt
      count.pt <- length(pt.temp[!(pt.temp %in% 52:64) & !is.na(pt.temp)])
      count.pt.early <- length(pt.temp[pt.temp < 52 & !is.na(pt.temp)])
      count.pt.late <- length(pt.temp[pt.temp > 64 & !is.na(pt.temp)])
      
      if (count.pt < 2) { # want 11/12 countries to fall in this range
        crit2.temp <- c(crit2.temp, 'yes')
      } else if (count.pt.early > count.pt.late) {
        crit2.temp <- c(crit2.temp, 'early')
      } else if (count.pt.late > count.pt.early) {
        crit2.temp <- c(crit2.temp, 'late')
      } else {
        print(run)
      }
      
    } else {
      crit2.temp <- c(crit2.temp, NA)
    }
    
  }
  
  crit2.temp <- crit2.temp[!is.na(crit2.temp)]
  
  # if (any(crit2.temp == 'early') & any(crit2.temp == 'late')) {
  #   print(crit2.temp)
  # }
  
  if (any(crit2.temp == 'yes')) { # can try all vs. any
    crit2 <- c(crit2, run)
  } else if (any(crit2.temp == 'early') & any(crit2.temp == 'late')) {
    out.double <- c(out.double, run)
  } else if (any(crit2.temp == 'early')) {
    too.early <- c(too.early, run)
  } else if (any(crit2.temp == 'late')) {
    too.late <- c(too.late, run)
  }
  
}
# still only 25 in crit2; 33 too early, 189 too late, 16 "double"
# if using any instead of all: 116 crit2 (!); 31 too early, 112 too late, 4 "double" (not necessarily double, just sporadic)
# many of the "too.early" ones are just really small oscillations; double aren't all double, some just have unusual patterns/vary by year

# Plot out to explore:
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in out.double) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(12, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(24, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}

# Plot out runs with any outbreaks matching 2 criteria:
pdf('syntheticTests/outputs/explore/outbreak_plots_criteria2_3of10.pdf', width = 16, height = 10)
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in crit2) {
  matplot(t(res.rates[[as.numeric(as.character(run))]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 520, by = 52), lty = 2)
  abline(v = seq(13, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
  abline(v = seq(25, 520, by = 52), lty = 1, col = 'red', lwd = 2.0)
}
dev.off()

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
print(summary(df.main$ar)) # some very low - look at where < 10,000; look much better once NA onsets also have NA AR

df.main$ar[is.na(df.main$ot)] <- NA # make NA when no outbreak

# Save and look at parms by outbreak pattern:
parms.double <- parms[, as.numeric(as.character(out.double))]
parms.late <- parms[, as.numeric(as.character(too.late))]
parms.early <- parms[, as.numeric(as.character(too.early))]

# save(parms.double, file = 'syntheticTests/syntheticData/parms_doubleOutbreaks_3of10.RData')
# save(parms.late, file = 'syntheticTests/syntheticData/parms_lateOutbreaks_3of10.RData')
# save(parms.early, file = 'syntheticTests/syntheticData/parms_earlyOutbreaks_3of10.RData')

df.main <- df.main[!(df.main$run %in% c(too.early, out.double, too.late)), ]
df.main$run <- factor(df.main$run)

# Now some with a little less synchrony, at least

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

pdf('syntheticTests/outputs/explore/param_comp_criteria2_3of10.pdf', width = 16, height = 10)
par(mfrow = c(2, 4), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(S0_mean ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(S0_sd ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
boxplot(aScale ~ group, data = parms.df.crit1, xlab = '', col = 'gray95')
# correct timing and too late have similar ranges; too early tend to have low L/R0diff
dev.off()

# # Look at geographical patterns:
# synch.ot = synch.pt = c() # number of weeks between latest and earliest onset/peak
# for (run in levels(df.main$run)) {
#   # print(run)
#   
#   for (yr in 1:10) {
#     df.temp <- df.main[df.main$run == run & df.main$year == yr, ]
#     # print(df.temp$pt)
#     # print(max(df.temp$ot) - min(df.temp$ot)) # for some outbreaks, onsets have a bit more range
#     synch.ot <- c(synch.ot, (max(df.temp$ot) - min(df.temp$ot)))
#     synch.pt <- c(synch.pt, (max(df.temp$pt) - min(df.temp$pt)))
#   }
#   
# }
# 
# synch.ot <- synch.ot[synch.ot != -Inf & !is.na(synch.ot)]
# synch.pt <- synch.pt[synch.pt != -Inf & !is.na(synch.pt)]
# 
# p1 <- ggplot() + geom_histogram(aes(x = synch.ot), bins = 6, col = 'black', fill = 'gray90') + theme_classic() +
#   labs(x = 'Onset Range (Latest - Earliest)', y = 'Count') + scale_x_continuous(breaks = 0:5)
# p2 <- ggplot() + geom_histogram(aes(x = synch.pt), bins = 5, col = 'black', fill = 'gray90') + theme_classic() +
#   labs(x = 'Peak Range (Latest - Earliest)', y = 'Count') + scale_x_continuous(breaks = 0:4)
# grid.arrange(p1, p2, ncol = 1)
# # vast majority are only 1-2 weeks away from each other
# 
# # At this point, I'm not sure there's any need to assess geographic patterns?

















