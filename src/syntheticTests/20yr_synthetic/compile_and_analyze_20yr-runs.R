### Assess results of 20-year synthetic multistrain runs ###

# # Process Raw Results
# # Load and compile 20-year runs:
# run.list1 = run.list2 = s.list1 = s.list2 = r.list1 = r.list2 = vector('list', 200)
# in.files <- list.files('src/syntheticTests/20yr_runs_raw/', pattern = '.RData')
# for (i in 1:length(in.files)) {
#   load(paste0('src/syntheticTests/20yr_runs_raw/resList_20yr_last10_', i, '.RData'))
#   run.list1[[i]] <- res.list[[1]]
#   run.list2[[i]] <- res.list[[2]]
#   s.list1[[i]] <- res.list[[3]]
#   s.list2[[i]] <- res.list[[4]]
#   r.list1[[i]] <- res.list[[5]]
#   r.list2[[i]] <- res.list[[6]]
# }
# 
# # # Save new list as one:
# # save(run.list, file = 'src/syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10.RData')
# 
# # Transform into lists of 10,000 instead of 200 lists of 50
# run.list1 <- do.call(c, run.list1)
# run.list2 <- do.call(c, run.list2)
# s.list1 <- do.call(c, s.list1)
# s.list2 <- do.call(c, s.list2)
# r.list1 <- do.call(c, r.list1)
# r.list2 <- do.call(c, r.list2)
# 
# # Compile by state:
# run.list <- list(run.list1, run.list2)
# s.list <- list(s.list1, s.list2)
# r.list <- list(r.list1, r.list2)
# 
# # Save:
# save(run.list, file = 'src/syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10_noAH.RData')
# save(s.list, file = 'src/syntheticTests/syntheticData/20yr_runs_cluster/resS_20yr_last10_noAH.RData')
# save(r.list, file = 'src/syntheticTests/syntheticData/20yr_runs_cluster/resR_20yr_last10_noAH.RData')
# 
# rm(list = ls())

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Format ### 

# Load in last 10 years:
load('src/syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10_noAH.RData')
# load('src/syntheticTests/syntheticData/20yr_runs_cluster/resS_20yr_last10_meanAH.RData')
# load('src/syntheticTests/syntheticData/20yr_runs_cluster/resR_20yr_last10_meanAH.RData')

# And read in accompanying parameter sets:
# load('src/syntheticTests/syntheticData/20yr_runs_cluster/init_parms_10000_NEW.RData')
# load('src/syntheticTests/syntheticData/20yr_runs_cluster/init_parms_10000_LHS.RData')
load('src/syntheticTests/syntheticData/20yr_runs_cluster/init_parms_10000_LHS_noAH.RData')

# List of countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# Are we looking at averages, or at full data streams?
# Let's look at the mean annual cycle for now:
yr.breaks <- list(1:52, 53:104, 105:156, 157:208, 209:260, 261:312, 313:364, 365:416, 417:468, 469:520)
get_10yr_avg <- function(obs) {
  obs.avg <- matrix(0, nrow = nrow(obs), ncol = 52)
  for (yr in 1:10) {
    obs.avg <- obs.avg + obs[, yr.breaks[[yr]]]
  }
  obs.avg <- obs.avg / 10
  return(obs.avg)
}

# Get yearly averages:
res.avg1 <- lapply(run.list[[1]], get_10yr_avg)
res.avg2 <- lapply(run.list[[2]], get_10yr_avg)
# could combine the two strains - eventually! but for now keep separate to check out?
    # concerned that if I just add them, years with both strains will have super large outbreaks?

res.list.comb <- vector('list', length(run.list[[1]]))
for (i in 1:10000) {
  res.list.comb[[i]] <- run.list[[1]][[i]] + run.list[[2]][[i]]
}
res.avg <- lapply(res.list.comb, get_10yr_avg)

# Compile data frame of mean peak timings/intensities:
df <- NULL
for (i in 1:10000) {
  if (i %% 1000 == 0) {
    print(i)
  }
  res.temp <- t(res.avg[[i]])
  for (count.index in 1:length(countries)) {
    pt <- which(res.temp[, count.index] == max(res.temp[, count.index]))
    if (length(pt) > 1) {
      pt <- NA
    }
    pi <- max(res.temp[, count.index])
    obs.min <- min(res.temp[, count.index])
    df <- rbind(df, c(i, countries[count.index], pt, pi, obs.min))
  }
}
df <- as.data.frame(df)
names(df) <- c('run', 'country', 'pt', 'pi', 'minimum')
df$pt <- as.numeric(as.character(df$pt))
df$pi <- as.numeric(as.character(df$pi))
df$minimum <- as.numeric(as.character(df$minimum))
df$osc.range <- df$pi - df$minimum

# Also get this as an ordered list of PTs by run:
pt.list <- lapply(1:10000, function(ix) {
  df$pt[df$run == ix]
})
# test.list <- pt.list[1:10]

# And for PI, min, and osc.range:
pi.list <- lapply(1:10000, function(ix) {
  df$pi[df$run == ix]
})
min.list <- lapply(1:10000, function(ix) {
  df$minimum[df$run == ix]
})
osc.list <- lapply(1:10000, function(ix) {
  df$osc.range[df$run == ix]
})

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Remove where no proper outbreaks ###

# Remove where all PI < 50 (500 / 10 years):
no.outbreaks <- unlist(lapply(pi.list, function(ix) {
  all(ix < 50)
}))
no.outbreaks <- (1:10000)[no.outbreaks]
have.outbreaks <- (1:10000)[-no.outbreaks]

# Also remove those with minimum >200, osc.range <200
# ??? these might be sensitive - test!
small.osc <- unlist(lapply(osc.list, function(ix) {
  all(ix < 50)
}))
small.osc <- (1:10000)[small.osc]
small.osc <- small.osc[small.osc %in% have.outbreaks]
have.outbreaks <- have.outbreaks[!(have.outbreaks %in% small.osc)] # down to 8888

high.min <- unlist(lapply(min.list, function(ix) {
  all(ix > 50)
}))
high.min <- (1:10000)[high.min] # 200: 726; 100: 1837; 500: 214; 400: 288; 300: 433 (so ~400 seems to be a cut-off point)
high.min <- high.min[high.min %in% have.outbreaks] # 200: 652 # 300: 394
# high.min <- high.min[!(high.min %in% no.outbreaks)] # obviously no overlap b/c "no outbreaks" have less than 50 at max
have.outbreaks <- have.outbreaks[!(have.outbreaks %in% high.min)] # 8494

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### STOP: Calculate RMSE for all with outbreaks ###
# Code will have to be adapted to compare results to (sub)type-specific data, or else to their sum

# iliiso <- read.csv('data/WHO_data_05-09-19.csv')
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# for (i in 2:22) {
#   if (names(iliiso)[i] == 'France') {
#     iliiso[1:283, i] <- iliiso[1:283, i] * 1.3
#     iliiso[284:495, i] <- iliiso[284:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   } else {
#     iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   }
#   
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
# }
# iliiso <- iliiso[, c(1:3, 5, 7:9, 12:15, 18, 20)]
# 
# season.names <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
# season.breaks <- list(79:130, 131:182, 183:234, 235:286, 287:338, 339:390, 392:443, 444:495)
# 
# obs.mats <- vector('list', length(season.names))
# for (i in 1:length(obs.mats)) {
#   obs.mats[[i]] <- as.matrix(iliiso[season.breaks[[i]], 2:13])
#   
# }
# obs.mat.avg <- apply(simplify2array(obs.mats), 1:2, mean, na.rm = TRUE)
# rownames(obs.mat.avg) <- NULL
# # since there are a lot of NAs as the season ends, may just use the first 35 or so, and ignore 0s?
# 
# # Organize this to be week 1-52, rather than 40 forward
# obs.mat.avg <- rbind(obs.mat.avg[14:52, ], obs.mat.avg[1:13, ])
# 
# # Is RMSE okay? Since we don't necessarily expect outbreaks every year anyway, we also don't necessarily expect the mean values to be as high
# # Maybe calculate the % relative to maximum value for that run (over all countries, not by country), then calculate RMSE against that?
# 
# res.outbreaks <- res.avg[have.outbreaks]
# res.outbreaks.rel <- lapply(res.outbreaks, function(ix) {
#   ix / max(ix) * 100
# })
# obs.mat.rel <- obs.mat.avg / max(obs.mat.avg, na.rm = TRUE) * 100
# 
# rmses1 <- lapply(res.outbreaks, function(ix) { # keep where 0 in obs.mat.avg - not worth worrying about taking all those out
#   sqrt(mean((obs.mat.avg[1:35, ] - t(ix)[1:35, ]) ** 2, na.rm = TRUE))
# })
# rmses2 <- lapply(res.outbreaks.rel, function(ix) {
#   sqrt(mean((obs.mat.rel[1:35, ] - t(ix)[1:35, ]) ** 2, na.rm = TRUE))
# })
# # calculate against raw numbers, and against relative to maximum value
# 
# rmses1 <- unlist(rmses1); rmses2 <- unlist(rmses2)
# cor.test(rmses1, rmses2, method = 'spearman')
# 
# df.outbreaks.rmses <- as.data.frame(cbind(have.outbreaks, rmses1, rmses2))
# names(df.outbreaks.rmses) <- c('run', 'rmse1', 'rmse2')
# df.outbreaks.rmses$run <- factor(df.outbreaks.rmses$run)
# 
# quantile(df.outbreaks.rmses$rmse1)
# quantile(df.outbreaks.rmses$rmse2)
# 
# # get top 1%:
# df.outbreaks.rmses$run[df.outbreaks.rmses$rmse1 < quantile(df.outbreaks.rmses$rmse1, probs = 0.01)]
# # 205  312  369  438  571  581  652  698  1080 1084 1610 1668 1747 1831 2352 2515 2768 2838 2846 2852 3490 3639 3651 4341 4440 4714 4803 4917 4995 5024 5152 5227 5279 5400 5438
# # 5537 5784 5854 5886 6150 6268 6324 6434 7205 7276 7327 7400 7590 7670 7709 8441 8727 8751 8827 8937 9869
# 
# df.outbreaks.rmses$run[df.outbreaks.rmses$rmse2 < quantile(df.outbreaks.rmses$rmse2, probs = 0.01)]
# # 438  571  622  785  1248 1286 1313 1431 1610 1815 1831 1875 2146 2153 2515 2591 2682 2838 2911 3029 3171 3490 4440 4714 4908 4974 5086 5089 5227 5312 5332 5400 5537 5663 5907
# # 6010 6268 6434 6982 7185 7400 7670 7758 8424 8441 8508 8567 8727 8932 8937 9146 9202 9307 9318 9702 9869

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Break down by PT/geo. pattern ### 

# Get those where 10+/12 are between 13:25:
to.keep <- unlist(lapply(pt.list, function(ix) {
  length(ix[!(ix %in% c(1:12, 52)) & !is.na(ix)]) < 3 # 1809
  # length(ix[ix %in% 13:25 & !is.na(ix)]) >= 10 # 1763
}))
to.keep <- (1:10000)[to.keep]
to.keep <- to.keep[to.keep %in% have.outbreaks] # reduces to 1410

# Check for onsets in at least 10/12 as well?:
insuff.onsets <- unlist(lapply(pi.list, function(ix) {
  length(ix[ix < 50]) > 2
}))
insuff.onsets <- (1:10000)[insuff.onsets]
to.keep <- to.keep[!(to.keep %in% insuff.onsets)]
# none of those were in to.keep, so fine to move on

# # Separate others into too early/too late:
# too.early <- unlist(lapply(pt.list, function(ix) {
#   length(ix[ix < 13]) > 2
# }))
# too.late <- unlist(lapply(pt.list, function(ix) {
#   length(ix[ix > 25]) > 2
# }))

# Store others as wrong timing for now:
# We can evaluate late/early/wrong season/etc. later, if we want
wrong.timing <- have.outbreaks[!(have.outbreaks %in% to.keep)]

# Now look at geographic patterns:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy', 'Luxembourg',
                   'Netherlands', 'Poland', 'Slovakia', 'Spain')

world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)

levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'lat', 'long')]

df$country[1:12] # order that we want world.cities to be in
long.vals <- world.cities[order(world.cities$country.etc, df$country[1:12]), 'long']

# have to make sure that PT < 40 is added appropriately
pt.list <- lapply(pt.list, function(ix) {
  ix[ix < 40 & !is.na(ix)] <- ix[ix < 40 & !is.na(ix)] + 52
  return(ix)
})

corrs <- lapply(pt.list, function(ix) {
  cor.test(long.vals, ix, method = 'kendall')$estimate
})
p.vals <- lapply(pt.list, function(ix) {
  cor.test(long.vals, ix, method = 'kendall')$p.value
})
# about 87.5% of all runs are east to west - what about just among "realistic" runs?

# Let's reduce to only those in to.keep, and look at patterns there:
df <- df[df$run %in% to.keep, ]
df$run <- factor(df$run)

corrs <- unlist(corrs); p.vals <- unlist(p.vals)

df$pval = df$corr = NA
for (run in levels(df$run)) {
  df$corr[df$run == run] <- corrs[as.numeric(as.character(run))]
  df$pval[df$run == run] <- p.vals[as.numeric(as.character(run))]
}
# 5 outbreaks have simultaneous peaks...
same.peak <- as.numeric(as.character(unique(df$run[is.na(df$corr)])))

df <- df[!is.na(df$corr), ]; df$run <- factor(df$run)
to.keep <- to.keep[!(to.keep %in% same.peak)]
# about 75% are east to west at all

# Assign patterns:
df$pattern <- ifelse(df$corr > 0, 'westToEast', 'eastToWest')

df$pattern2 <- df$pattern
df$pattern2[df$pval > 0.05] <- 'noPatt'

df$sig <- ifelse(df$pval < 0.05, 'yes', 'no')

df$pattern <- factor(df$pattern)
df$pattern2 <- factor(df$pattern2)
df$sig <- factor(df$sig)

df$patternSig <- paste(df$pattern, df$sig, sep = '_')
df$patternSig <- factor(df$patternSig)

table(df$pattern) / 12 # 327/1405 (~23.3%)
table(df$pattern2) / 12 # majority not sig (918/1405); 70 are sig w-e, 417 sig e-w
table(df$patternSig) / 12 # most e-w/no; more sig e-w than w-e total...
# 70/1405 (~5%) sig w-e

runs.ew.sig <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'eastToWest_yes']))))
runs.ew.not <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'eastToWest_no']))))
runs.we.not <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'westToEast_no']))))
runs.we.sig <- sort(as.numeric(as.character(unique(df$run[df$patternSig == 'westToEast_yes']))))

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Plots of time series ###

# Visualize runs with "correct" pattern:
pdf('src/syntheticTests/outputs/explore/outbreak_averages_west-to-east_10000_meanAH.pdf', width = 16, height = 10)
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in c(runs.we.sig, runs.we.not)) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(12, 52), lty = 1, col = 'red')
  # matplot(t(res.list[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
}
dev.off()

pdf('src/syntheticTests/outputs/explore/outbreak_plots_west-to-east_10000_meanAH.pdf', width = 16, height = 10)
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in c(runs.we.sig, runs.we.not)) {
  matplot(t(res.list.comb[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
}
dev.off()
# Actually quite a few where there are outbreaks every year - too much tendency to cause outbreaks?
# And how to decide which to keep? I'm guessing we don't want those with outbreaks every year; do we see which (if any) match observed patterns?

# And what do individual strains look like?
pdf('src/syntheticTests/outputs/explore/outbreak_plots_west-to-east_10000_byStrain_meanAH.pdf', width = 16, height = 10)
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in c(runs.we.sig, runs.we.not)) {
  matplot(t(run.list[[1]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(12, 523, by = 52), col = 'red', lwd = 1.5); abline(v = seq(0, 523, by = 52), col = 'red', lwd = 1.5)
  abline(v = seq(1, 523, by = 52), lty = 3, lwd = 2)
  matplot(t(run.list[[2]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(12, 523, by = 52), col = 'red', lwd = 1.5); abline(v = seq(0, 523, by = 52), col = 'red', lwd = 1.5)
  abline(v = seq(1, 523, by = 52), lty = 3, lwd = 2)
}
dev.off()

for (i in runs.we.not) {
  print(i)

  run.temp <- run.list[[1]][[i]]
  die.out <- c()
  for (j in 1:523) {
    # die.out <- c(die.out, all(run.temp[, j] == 0))
    die.out <- c(die.out, length(which(run.temp[, j] == 0)))
  }
  print(summary(die.out))

  # run.temp <- run.list[[2]][[i]]
  # die.out <- c()
  # for (j in 1:523) {
  #   die.out <- c(die.out, all(run.temp[, j] == 0))
  #   # die.out <- c(die.out, length(which(run.temp[, j] == 0)))
  # }
  # print(summary(die.out))

}

# Find examples in have.outbreaks where die-out occurs?
die.out.yes <- c()
for (i in c(runs.we.sig, runs.we.not)) {
  
  # check strain1 only for now
  run.temp <- run.list[[1]][[i]]
  die.out <- c()
  for (j in 1:523) {
    die.out <- c(die.out, all(run.temp[, j] == 0))
  }
  
  if (length(die.out[die.out] > 0)) {
    # print(i)
    die.out.yes <- c(die.out.yes, i)
  }
  
}

# # Check S and R:
# par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in c(runs.we.sig, runs.we.not)) {
#   matplot(t(s.list[[1]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
#   matplot(t(s.list[[2]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
# }
# 
# par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in c(runs.we.sig, runs.we.not)) {
#   matplot(t(r.list[[1]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
#   matplot(t(r.list[[2]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
# }
# # I think these look fine? the patterns at least are consistent

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Explore parameter patterns ###

params.no <- as.data.frame(t(parms[, no.outbreaks]))
params.osc <- as.data.frame(t(parms[, small.osc]))
params.min <- as.data.frame(t(parms[, high.min]))
# params.early <- as.data.frame(t(parms[, too.early]))
# params.late <- as.data.frame(t(parms[, too.late]))
# params.mix <- as.data.frame(t(parms[, mixed.pt]))
params.wrong <- as.data.frame(t(parms[, wrong.timing]))
params.keep <- as.data.frame(t(parms[, to.keep]))

params.no$group <- 'No Outbreaks'
params.osc$group <- 'Small Osc.'
params.min$group <- 'High Min.'
# params.early$group <- 'Early'
# params.late$group <- 'Late'
# params.mix$group <- 'Mix'
params.wrong$group <- 'Timing Off'
params.keep$group <- 'Realistic'

parms.df <- rbind(params.no, params.osc, params.min, params.wrong, params.keep)
names(parms.df)[25:30] <- c('L', 'D', 'R0mx', 'R0diff', 'airScale', 'group')
for (i in 1:length(countries)) {
  names(parms.df)[i] <- paste0('S0_', countries[i])
  names(parms.df)[i + length(countries)] <- paste0('I0_', countries[i])
}
parms.df$group <- factor(parms.df$group)
parms.df$group <- factor(parms.df$group, levels = levels(parms.df$group)[c(2, 4, 1, 5, 3)])#[c(5, 7, 2, 1, 3:4, 6)])

pdf('src/syntheticTests/outputs/explore/param_comp_10000_meanAH.pdf', width = 16, height = 10)
par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# boxplot(S0_mean ~ group, data = parms.df, xlab = '', col = 'gray95')
# boxplot(S0_sd ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df, xlab = '', col = 'gray95')
boxplot(airScale ~ group, data = parms.df, xlab = '', col = 'gray95')

par(mfrow = c(4, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(countries)) {
  boxplot(parms.df[, i] ~ parms.df$group, xlab = '', col = 'gray95', ylab = names(parms.df)[i])
}

for (i in 1:length(countries)) {
  boxplot(parms.df[, i + length(countries)] ~ parms.df$group, xlab = '', col = 'gray95', ylab = names(parms.df)[i + length(countries)])
}
dev.off()

# And by geography:
parms.ew.sig <- as.data.frame(t(parms[, runs.ew.sig]))
parms.ew.not <- as.data.frame(t(parms[, runs.ew.not]))
parms.we.not <- as.data.frame(t(parms[, runs.we.not]))
parms.we.sig <- as.data.frame(t(parms[, runs.we.sig]))

parms.ew.sig$group <- 'E-W Sig'; parms.ew.not$group <- 'E-W Not'; parms.we.not$group <- 'W-E Not'; parms.we.sig$group <- 'W-E Sig'

parms.df2 <- rbind(parms.ew.sig, parms.ew.not, parms.we.not, parms.we.sig)
parms.df2$group <- factor(parms.df2$group)
parms.df2$group <- factor(parms.df2$group, levels = levels(parms.df2$group)[c(2, 1, 3:4)])

colnames(parms.df2)[25:30] <- c('L', 'D', 'R0mx', 'R0diff', 'airScale', 'group')
for (i in 1:length(countries)) {
  names(parms.df2)[i] <- paste0('S0_', countries[i])
  names(parms.df2)[i + length(countries)] <- paste0('I0_', countries[i])
}

pdf('src/syntheticTests/outputs/explore/param_comp_geo_10000_meanAH.pdf', width = 16, height = 10)
par(mfrow = c(2, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# boxplot(S0_mean ~ group, data = parms.df2, xlab = '', col = 'gray95')
# boxplot(S0_sd ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(L ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(D ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(R0mx ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(R0diff ~ group, data = parms.df2, xlab = '', col = 'gray95')
boxplot(airScale ~ group, data = parms.df2, xlab = '', col = 'gray95')

par(mfrow = c(4, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:length(countries)) {
  boxplot(parms.df2[, i] ~ parms.df2$group, xlab = '', col = 'gray95', ylab = names(parms.df2)[i])
}

for (i in 1:length(countries)) {
  boxplot(parms.df2[, i + length(countries)] ~ parms.df2$group, xlab = '', col = 'gray95', ylab = names(parms.df2)[i + length(countries)])
}
dev.off()

# More complex parameter analysis? (2D and 3D co-variability?)
# Let's see if this is needed first, and whether I'm choosing "realistic" reasonably

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
### Calculate RMSEs ###

# # Compare to observed patterns:
# iliiso <- read.csv('data/WHO_data_05-09-19.csv')
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# for (i in 2:22) {
#   if (names(iliiso)[i] == 'France') {
#     iliiso[1:283, i] <- iliiso[1:283, i] * 1.3
#     iliiso[284:495, i] <- iliiso[284:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   } else {
#     iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   }
#   
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
# }
# 
# iliiso <- iliiso[, c(1:3, 5, 7:9, 12:15, 18, 20)]
# 
# season.names <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
# season.breaks <- list(79:130, 131:182, 183:234, 235:286, 287:338, 339:390, 392:443, 444:495)
# 
# obs.mats <- vector('list', length(season.names))
# for (i in 1:length(obs.mats)) {
#   obs.mats[[i]] <- as.matrix(iliiso[season.breaks[[i]], 2:13])
#   
# }
# obs.mat.avg <- apply(simplify2array(obs.mats), 1:2, mean, na.rm = TRUE)
# rownames(obs.mat.avg) <- NULL
# # since there are a lot of NAs as the season ends, may just use the first 35 or so, and ignore 0s?
# 
# # Organize this to be week 1-52, rather than 40 forward
# obs.mat.avg <- rbind(obs.mat.avg[14:52, ], obs.mat.avg[1:13, ])

# runs.we <- c(runs.we.sig, runs.we.not)
# res.avg.we <- res.avg[runs.we]
# 
# res.avg.we.rel <- lapply(res.avg.we, function(ix) {
#   ix / max(ix) * 100
# })
# 
# rmses1 <- lapply(res.avg.we, function(ix) { # keep where 0 in obs.mat.avg - not worth worrying about taking all those out
#   sqrt(mean((obs.mat.avg[1:35, ] - t(ix)[1:35, ]) ** 2, na.rm = TRUE))
# })
# rmses2 <- lapply(res.avg.we.rel, function(ix) { # keep where 0 in obs.mat.avg - not worth worrying about taking all those out
#   sqrt(mean((obs.mat.rel[1:35, ] - t(ix)[1:35, ]) ** 2, na.rm = TRUE))
# })
# rmses1 <- unlist(rmses1); rmses2 <- unlist(rmses2)
# 
# # matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+')
# 
# quantile(rmses1, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1))
# quantile(rmses2, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1))
# 
# pdf('src/syntheticTests/outputs/explore/best_by_RMSE1_10000_meanAH.pdf', width = 16, height = 10)
# par(mfrow = c(5, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in which(rmses1 < 700)) {
#   # matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = rmses1[run])
#   matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = run)
#   matlines(t(res.avg.we[[run]]), type = 'l', pch = 20, cex = 0.6, col = viridis(12))
#   abline(v = c(12, 52), lty = 1, col = 'red')
# }
# dev.off()
# pdf('src/syntheticTests/outputs/explore/best_by_RMSE2_10000_meanAH.pdf', width = 16, height = 10)
# par(mfrow = c(5, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in which(rmses2 < 16)) {
#   # matplot(obs.mat.rel, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = rmses2[run])
#   matplot(obs.mat.rel, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = run)
#   matlines(t(res.avg.we.rel[[run]]), type = 'l', pch = 20, cex = 0.6, col = viridis(12))
#   abline(v = c(12, 52), lty = 1, col = 'red')
# }
# dev.off()
# # Might need a better way to see if individual countries tend to line up
# # 174, 182 (not run numbers, but order in rmses1/rmses2!) in both; avg maybe looks better?
# 
# par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in runs.we[which(rmses2 < 16)]) {
#   # matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
#   matplot(t(run.list[[1]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
#   abline(v = seq(0, 550, by = 52), lty = 2)
#   matplot(t(run.list[[2]][[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
#   abline(v = seq(0, 550, by = 52), lty = 2)
# }
# # Although even the individual outbreaks seem pretty synchronous...
# 
# # How does this look by country?
# for (run in which(rmses1 < 700)) {
#   par(mfrow = c(4, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
#   for (i in 1:12) {
#     plot(obs.mat.avg[, i], pch = 20, xlab = 'Time', ylab = 'Syn.+', cex = 0.8, main = countries[i])
#     lines(res.avg.we[[run]][i, ], col = 'steelblue2')
#   }
# }
# 
# for (run in which(rmses2 < 16)) {
#   par(mfrow = c(4, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
#   for (i in 1:12) {
#     plot(obs.mat.rel[, i], pch = 20, xlab = 'Time', ylab = 'Syn.+ (Relative)', cex = 0.8, main = countries[i])
#     lines(res.avg.we.rel[[run]][i, ], col = 'steelblue2')
#   }
# }
# # I think this is fine, but there's always the option to look at RMSEs by each country, and add/average them or something
#     # Or even weight them by some measure of data quality
# 
# # What about RMSEs calculated for all "have.outbreaks" - are any better?
# summary(df.outbreaks.rmses$run[df.outbreaks.rmses$rmse1 < quantile(df.outbreaks.rmses$rmse1, probs = 0.01)] %in% runs.we) # 18 fall in runs.we; 14
# summary(df.outbreaks.rmses$run[df.outbreaks.rmses$rmse2 < quantile(df.outbreaks.rmses$rmse2, probs = 0.01)] %in% runs.we) # 21; 17
# 
# df.outbreaks.rmses$run[df.outbreaks.rmses$rmse1 < quantile(df.outbreaks.rmses$rmse1, probs = 0.01)]
# df.outbreaks.rmses$run[df.outbreaks.rmses$rmse2 < quantile(df.outbreaks.rmses$rmse2, probs = 0.01)]
# runs.we[which(rmses1 < 700)]
# runs.we[which(rmses2 < 16)]
# 
# df.outbreaks.rmses$we <- ifelse(df.outbreaks.rmses$run %in% runs.we, T, F)
# df.outbreaks.rmses$real <- ifelse(df.outbreaks.rmses$run %in% to.keep, T, F)
# boxplot(log(df.outbreaks.rmses$rmse1) ~ df.outbreaks.rmses$we)
# boxplot(df.outbreaks.rmses$rmse2 ~ df.outbreaks.rmses$we)
# # rmse2 are clearly better where T, rmse1 less clear/very similar
# # but there certainly are those with the "wrong" pattern that have lower RMSEs
# boxplot(log(df.outbreaks.rmses$rmse1) ~ df.outbreaks.rmses$real)
# boxplot(df.outbreaks.rmses$rmse2 ~ df.outbreaks.rmses$real)
# # same
# 
# par(mfrow = c(5, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# for (run in df.outbreaks.rmses$run[df.outbreaks.rmses$rmse1 < quantile(df.outbreaks.rmses$rmse1, probs = 0.01)]) {
#   # matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = rmses1[run])
#   matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = run)
#   matlines(t(res.avg[[as.numeric(as.character(run))]]), type = 'l', pch = 20, cex = 0.6, col = viridis(12))
#   abline(v = c(12, 52), lty = 1, col = 'red')
# }
# 
# # par(mfrow = c(5, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# # for (run in df.outbreaks.rmses$run[df.outbreaks.rmses$rmse2 < quantile(df.outbreaks.rmses$rmse2, probs = 0.01)]) {
# #   # matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = rmses1[run])
# #   matplot(obs.mat.avg, type = 'b', pch = 20, col = viridis(12), xlab = 'Time', ylab = 'Obs. Syn+', cex = 0.6, main = run)
# #   matlines(t(res.avg[[as.numeric(as.character(run))]]), type = 'l', pch = 20, cex = 0.6, col = viridis(12))
# #   abline(v = c(12, 52), lty = 1, col = 'red')
# # } # don't have relative for all runs, so skip this plot for now
# 
# # But I think it's fine to manually pull "best" out first, b/c RMSE can be quite limited in finding "best" fits
#     # Although some of the ones for RMSE1 look better than the w-e/real ones I think
# 
# # Also look at RMSE over parameter ranges:
# parms.we <- parms[, runs.we]
# parms.we.df <- as.data.frame(t(parms.we))
# names(parms.we.df)[25:29] <- c('L', 'D', 'R0mx', 'R0diff', 'airScale')
# parms.we.df$rmse1 <- rmses1; parms.we.df$rmse2 <- rmses2
# 
# par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# # plot(parms.we.df$S0_mean, parms.we.df$rmse1, pch = 20, xlab = 'S0_mean', ylab = 'RMSE')
# # plot(parms.we.df$S0_sd, parms.we.df$rmse1, pch = 20, xlab = 'S0_sd', ylab = 'RMSE')
# plot(parms.we.df$L, parms.we.df$rmse1, pch = 20, xlab = 'L', ylab = 'RMSE')
# plot(parms.we.df$D, parms.we.df$rmse1, pch = 20, xlab = 'D', ylab = 'RMSE')
# plot(parms.we.df$R0mx, parms.we.df$rmse1, pch = 20, xlab = 'R0mx', ylab = 'RMSE')
# plot(parms.we.df$R0diff, parms.we.df$rmse1, pch = 20, xlab = 'R0diff', ylab = 'RMSE')
# plot(parms.we.df$airScale, parms.we.df$rmse1, pch = 20, xlab = 'airScale', ylab = 'RMSE')
# # 
# par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# # plot(parms.we.df$S0_mean, parms.we.df$rmse2, pch = 20, xlab = 'S0_mean', ylab = 'RMSE')
# # plot(parms.we.df$S0_sd, parms.we.df$rmse2, pch = 20, xlab = 'S0_sd', ylab = 'RMSE')
# plot(parms.we.df$L, parms.we.df$rmse2, pch = 20, xlab = 'L', ylab = 'RMSE')
# plot(parms.we.df$D, parms.we.df$rmse2, pch = 20, xlab = 'D', ylab = 'RMSE')
# plot(parms.we.df$R0mx, parms.we.df$rmse2, pch = 20, xlab = 'R0mx', ylab = 'RMSE')
# plot(parms.we.df$R0diff, parms.we.df$rmse2, pch = 20, xlab = 'R0diff', ylab = 'RMSE')
# plot(parms.we.df$airScale, parms.we.df$rmse2, pch = 20, xlab = 'airScale', ylab = 'RMSE')
# 
# # cor.test(parms.we.df$S0_mean, parms.we.df$rmse, method = 'kendall') # sig pos, but very slight
# # cor.test(parms.we.df$S0_sd, parms.we.df$rmse, method = 'kendall') # not sig
# # cor.test(parms.we.df$L, parms.we.df$rmse, method = 'kendall') # sig pos (tau = 0.297)
# # cor.test(parms.we.df$D, parms.we.df$rmse, method = 'kendall') # sig pos (tau = 0.223)
# # cor.test(parms.we.df$R0mx, parms.we.df$rmse, method = 'kendall') # not sig
# # cor.test(parms.we.df$R0diff, parms.we.df$rmse, method = 'kendall') # sig neg (tau = -0.385)
# # cor.test(parms.we.df$airScale, parms.we.df$rmse, method = 'kendall') # not sig
# #
# # ggplot(data = parms.we.df) + geom_point(aes(x = L, y = R0diff, col = rmses1), cex = 5.0) + theme_classic() + scale_color_viridis() # combo of high L/low R0diff part. bad
# # ggplot(data = parms.we.df) + geom_point(aes(x = D, y = R0diff, col = rmses), cex = 5.0) + theme_classic() + scale_color_viridis() # no strong assoc.; impact of R0diff clearer; stronger impact of R0diff when D is higher? R0diff more flexible when D is low?
# # ggplot(data = parms.we.df) + geom_point(aes(x = L, y = D, col = rmses), cex = 5.0) + theme_classic() + scale_color_viridis() # combo of low L/low D? but L clearer
# 
# parms.we.df$L <- parms.we.df$L / 365
# a <- lm(rmse2 ~ R0mx + R0diff + D + L + airScale, data = parms.we.df)
# print(summary(a)) # all still sig; change in one day of D has slightly larger impact than a 1-year change in L
#  
# # parms.we.df[parms.we.df$rmse < 15, c('L', 'D', 'R0diff', 'airScale')] # a couple L 4,5, but mostly < 3; D under 5; R0diff at least 0.6 (usually higher R0diff can contribute to e-w...)
# # # among to.keep, restricting to R0diff > 0.6 leads to higher percentage e-w
# # # but back to original (even slightly higher) proportion w-e if we also limit to L < 1000 and D < 5 (although % of e-w sig still goes up quite a bit relative to e-w not)










