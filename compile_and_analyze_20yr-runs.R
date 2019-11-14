
# # Load and compile 20-year runs:
# run.list <- vector('list', 200)
# in.files <- list.files('syntheticTests/syntheticData/20yr_runs_cluster/', pattern = '.RData')
# for (i in 1:length(in.files)) {
#   load(paste0('syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10_', i, '.RData'))
#   run.list[[i]] <- res.rates
# }
# 
# # Save new list as one:
# save(run.list, file = 'syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10.RData')
# 
# # Transform into a list of 10,000 instead of 200 lists of 50
# res.list <- do.call(c, run.list)
# 
# # Save:
# save(res.list, file = 'syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10.RData')

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################

# Load in last 10 years:
load('syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10.RData')

# And read in accompanying parameter sets:
load('syntheticTests/syntheticData/init_parms_10000.RData')

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
res.avg <- lapply(res.list, get_10yr_avg)

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
    df <- rbind(df, c(i, countries[count.index], pt, pi))
  }
}
df <- as.data.frame(df)
names(df) <- c('run', 'country', 'pt', 'pi')
df$pt <- as.numeric(as.character(df$pt))
df$pi <- as.numeric(as.character(df$pi))


df$min.val <- NA
for (i in 1:10000) {
  if (i %% 1000 == 0) {
    print(i)
  }
  res.temp <- t(res.avg[[i]])
  for (count.index in 1:length(countries)) {
    pi <- min(res.temp[, count.index])
    df$min.val[df$run == i & df$country == countries[count.index]] <- pi
  }
}






# Also get this as an ordered list of PTs by run:
pt.list <- lapply(1:10000, function(ix) {
  df$pt[df$run == ix]
})
test.list <- pt.list[1:10]

# And for PI:
pi.list <- lapply(1:10000, function(ix) {
  df$pi[df$run == ix]
})

# Remove where all PI < 50 (500 / 10 years):
no.outbreaks <- unlist(lapply(pi.list, function(ix) {
  all(ix < 50)
}))
no.outbreaks <- (1:10000)[no.outbreaks] # 800
have.outbreaks <- (1:10000)[-no.outbreaks]

# Get those where 10+/12 are between 13:25:
to.keep <- unlist(lapply(pt.list, function(ix) {
  length(ix[!(ix %in% 13:25) & !is.na(ix)]) < 3 # 1809
  # length(ix[ix %in% 13:25 & !is.na(ix)]) >= 10 # 1763
}))
to.keep <- (1:10000)[to.keep]
to.keep <- to.keep[to.keep %in% have.outbreaks] # reduces to 1450

# Separate others into too early/too late:
too.early <- unlist(lapply(pt.list, function(ix) {
  length(ix[ix < 13]) > 2
}))
too.late <- unlist(lapply(pt.list, function(ix) {
  length(ix[ix > 25]) > 2
}))
# unlike with individual season runs, runs are more likely to be too late than too early
# any overlap?: yes, 13 runs (9 with high enough "peaks")
mixed.pt <- which(too.early & too.late); mixed.pt <- mixed.pt[mixed.pt %in% have.outbreaks] # 9
too.early <- (1:10000)[too.early]; too.early <- too.early[!(too.early %in% mixed.pt) & too.early %in% have.outbreaks] # 609
too.late <- (1:10000)[too.late]; too.late <- too.late[!(too.late %in% mixed.pt) & too.late %in% have.outbreaks] # 7130

length(too.early) + length(too.late) + length(mixed.pt) # 7748; this leaves 9200 - 7748 = 1452 within the "realistic" range
# does this agree with above? not quite... we're missing 1452 - 1450 = 2 runs

# What do these "missing" 2 runs look like?
missing.runs <- (1:10000)[!((1:10000) %in% c(to.keep, too.early, too.late, mixed.pt, no.outbreaks))]

par(mfrow = c(2, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in missing.runs) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(13, 25), lty = 1, col = 'red')
}

# df[df$run %in% missing.runs, ]
pt.list[missing.runs] # basically have some too early and some too late, but not enough to be caught above; put with "mixed"
mixed.pt <- c(mixed.pt, missing.runs)

# Plot out to.keep? How many have weak oscillations?
par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in to.keep[1:100]) {
  matplot(t(res.avg[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = run, xlab = 'Time', ylab = 'Inc.')
  abline(v = c(13, 25), lty = 1, col = 'red')
}
# honestly I think the PI step took most of those out, although some still do look like little oscillations
  # could cut it off at like 200? 500? but maybe not now; could also say minimum has to be below 500 or near zero or something
# a lot of the "too.early" runs are blips, though; not as often the case with "too.late"
# so it might be good to remove these before analyzing parameters!

# Get minimum values/oscillation strengths, and put small oscillations in their own list:











# Now look at geographical patterns:
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

corrs <- lapply(pt.list, function(ix) {
  cor.test(long.vals, ix, method = 'kendall')$estimate
})
p.vals <- lapply(pt.list, function(ix) {
  cor.test(long.vals, ix, method = 'kendall')$p.value
})
# about 87.5% of all runs are east to west - what about just among "realistic" runs?




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








# Visualize runs with "correct" pattern:






# Explore parameter patterns:






# Eventually may also want to filter out those with a very small annual cycle (small oscillations)?
# Or are these okay b/c they're averages and some years might have no outbreaks?
# As well as those with very small average PI (<10?)











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







































