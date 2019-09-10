
### Read in metrics files:
source('code/gridSearch/comp_netVsIndiv/readIn_metrics.R')

### Limit to obs. lead weeks 2-4:
# m <- m[m$FWeek_pkwk %in% 2:4, ]
m <- m[m$FWeek_pkwk == 3, ]

### Look at percentage error for each model/oev_base combo:
boxplot(log(abs_err_1wk_perc) ~ model + oev_base, data = m)
# these actually look pretty similar (see MAE plot); it's only the log scores that are having trouble

### Read in and format ensemble files:
e1 <- read.csv('code/gridSearch/outputs/outputEns_090119_1wk.csv')
e1.ind <- read.csv('code/individualCountries/outputs/outputEns_082819_1wk.csv')

e1$model <- 'Network'; e1.ind$model <- 'Individual'

m <- m[, c(1:12, 16, 20, 24:25, 36, 40)]
# Maybe it's specifically countries with late peaks being pulled away from the observations by other countries being
# over and already having very low cases?
    # See if the country/season patterns have late peaks

e1 <- merge(e1, m, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model'))
e1.ind <- merge(e1.ind, m, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model'))
e1.ind$gamma <- NULL

### Remove where no observed onset:
e1 <- e1[!is.na(e1$onsetObs5), ]
e1.ind <- e1.ind[!is.na(e1.ind$onsetObs5), ]
# already gone, b/c of how m was formatted

# ### Un-scale ens:
# for (i in 10:309) {
#   e1[, i] <- e1[, i] / e1$scaling
#   e1.ind[, i] <- e1.ind[, i] / e1.ind$scaling
# }; rm(i)

### Merge:
e1$metric <- NULL; e1.ind$metrics <- NULL
e1 <- rbind(e1, e1.ind)
rm(e1.ind)

### Remove unneeded columns:
e1 <- e1[, c(1:308, 310:318)]

# ### Melt?:
# e1 <- melt(e1, id.vars = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model', 'obs_1week', 'fcast_1week', 'onset5', 'FWeek_pkwk'))

### Remove where obs are NA (and 0?):
e1 <- e1[!is.na(e1$obs_1week), ]
# e1.red <- e1[e1$obs_1week > 0, ] # none where obs is 0!

# ### Calculate % error:
# e1$value <- (abs(e1$value - e1$obs_1week) + e1$obs_1week) / e1$obs_1week

# ### Limit to a single lead week, to make quicker:
# e1 <- e1[e1$FWeek_pkwk == 3, ]

### Calculate standard deviation of estimates:
vars <- sapply(1:dim(e1)[1], function(ix) {
  var(as.numeric(as.character(e1[ix, 9:308])))
})
e1$vars <- vars
e1.all <- e1
e1 <- e1[, c(1:8, 309:318)]

### Also read in log scores themselves:
log.1wk <- read.csv('code/gridSearch/outputs/logScores_1wk.csv')
log.1wk <- log.1wk[log.1wk$FWeek_pkwk == 3, ]

log.1wk.ind <- read.csv('code/individualCountries/outputs/logScores_1wk.csv')
log.1wk.ind <- log.1wk.ind[log.1wk.ind$FWeek_pkwk == 3, ]

### And merge:
log.1wk$metric <- NULL; log.1wk.ind$metrics <- NULL
log.1wk$model <- 'Network'; log.1wk.ind$model <- 'Individual'
log.1wk <- rbind(log.1wk, log.1wk.ind)
log.1wk <- log.1wk[, c(1:7, 9, 12)]
e1 <- merge(e1, log.1wk, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model'))

### Explore!:
boxplot(scores ~ model + oev_base, data = e1)
boxplot(vars ~ model + oev_base, data = e1) # now we're seeing that these look to be lower
boxplot(log(vars) ~ model + oev_base, data = e1) # definitely lower variances than the other combos

e1.low <- e1[e1$scores == -10, ] # this means that literally none of the 300 are within the boundaries
# this can't be due to increased ensemble variance alone - it's a complete miss
boxplot(log(vars) ~ model + oev_base, data = e1.low) # same pattern as overall...

e1.base <- e1[e1$scores > -10, ]

e10 <- e1.low[e1.low$model == 'Network' & e1.low$oev_base == 1e4, ]
e10.comp <- e1.base[e1.base$model == 'Network' & e1.base$oev_base == 1e4, ]

table(e10$country, e10$season)
table(e10.comp$country, e10.comp$season)

boxplot(e10$obs_pkwk, e10.comp$obs_pkwk) # a little later
boxplot(log(e10$obs_1week), log(e10.comp$obs_1week))
boxplot(e10$delta_pkwk_mean, e10.comp$delta_pkwk_mean)
boxplot(e10$abs_err_1wk_perc, e10.comp$abs_err_1wk_perc) # more error, as expected
boxplot(log(e10$vars), log(e10.comp$vars)) # less variance, but not necessarily by much

boxplot(e10$lambda, e10.comp$lambda) # less accurate are less likely to include lambda 1.05, it looks like

e10 <- e10[e10$lambda == 1.02 & e10$oev_denom == 10, ]
e10.comp <- e10.comp[e10.comp$lambda == 1.02 & e10.comp$oev_denom == 10, ]

# # Look at unique country/season combos to see what differences are:
# e10.red <- unique(e10[, c(1, 7, 9)])
# e10.comp.red <- unique(e10.comp[, c(1, 7, 9)])
# 
# e10.red$group <- paste(e10.red$country, e10.red$season, sep = '_'); e10.red$group <- factor(e10.red$group)
# e10.comp.red$group <- paste(e10.comp.red$country, e10.comp.red$season, sep = '_'); e10.comp.red$group <- factor(e10.comp.red$group)
# 
# e10.comp.red <- e10.comp.red[!(e10.comp.red$group %in% levels(e10.red$group)), ]
# 
# table(e10.red$obs_pkwk); table(e10.comp.red$obs_pkwk)
# # they don't even necessarily tend to be later - I'm not sure what the problem is

### Any correlations between scores and other metrics?
e10 <- rbind(e10, e10.comp)
plot(log(e10$vars), e10$scores) # doesn't seem to be any relationship between scores and variation

# Does ensemble variance ("vars") drop below OEV at peak? At lead week 3?
e10.red <- e10[e10$season == '2010-11', ]
unique(e10.red[, c('country', 'fc_start')])

# Calculate OEVs:
count.indices <- c(1:8, 10:21)
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

iliiso <- iliiso[, c(1, count.indices + 1)]
test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
scalings <- scalings[count.indices, ]
for (i in 2:21) {
  if (names(iliiso)[i] == 'France') {
    iliiso[1:286, i] <- iliiso[1:286, i] * 1.3
    iliiso[287:495, i] <- iliiso[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[1:286, i] <- syn.dat[1:286, i] * 1.3
    syn.dat[287:495, i] <- syn.dat[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  } else {
    iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
    syn.dat[, i] <- syn.dat[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
  }
  
  iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
  syn.dat[, i][syn.dat[, i] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

obs_i <- iliiso[79:110, 2:21]
test_i <- test.dat[79:110, 2:21]
syn_i <- syn.dat[79:110, 2:21]
pos_i <- pos.dat[79:110, 2:21]

source('code/functions/replaceLeadingLaggingNAs.R')
n <- 20
for (count.index in 1:n) {
  obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
  syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
  pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
}
test_i[test_i == 0 & !is.na(test_i)] <- NA

# Plot:
par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
matplot(obs_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(syn_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(pos_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
matplot(test_i, pch = 20, col = viridis(n), type = 'b', lty = 1, cex = 0.75)
par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

source('code/functions/calc_obsvars.R')
obs_vars <- calc_obsvars_nTest(obs = as.matrix(obs_i), syn_dat = as.matrix(syn_i), ntests = as.matrix(test_i), posprops = as.matrix(pos_i),
                               oev_base = 1e4, oev_denom = 10, tmp_exp = 2.0)

# Now match correct OEV with the right week by country to e10.red:
unique(e10.red[, c('country', 'fc_start')])
wk.want <- c(60, 56, 62, 62, 59, NA, 60, 61, 56, 60, 58, 58, 60, 58, 63, 61, 60, 57, 61, 56) - 40 + 1# - 3

oevs <- c()
for (i in 1:20) {
  oevs <- c(oevs, obs_vars[wk.want[i], i])
}
oevs <- as.data.frame(cbind(c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT', 'LU', 'NL',
                              'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK'), oevs))
names(oevs) <- c('country', 'OEV')

e10.red <- merge(e10.red, oevs, by = 'country')
e10.red$OEV <- as.numeric(as.character(e10.red$OEV))
e10.red$err.prop <- e10.red$vars / e10.red$OEV # if <<1, might be divergence
boxplot(err.prop ~ country, data = e10.red) # don't necessarily have lower variance than others
# HR has very low, but not terrible failure; SK and UK fail but have high var
boxplot(scores ~ country, data = e10.red)
boxplot(vars ~ country, data = e10.red)

# Look at: AT (low error, low acc), HR (low error, decent acc), ES (high var, high acc), UK (high var, low acc)

# Look at mean posterior - mean prior - is there adjustment?
    # I haven't been saving the priors, though, so we don't know this

### Choose representative season (2010-11), plot out data, then show average and distribution of cases for rep. countries at a single lead week
    # AT, CZ, DE, DK, IE, IT, RO, SK, UK - all in the "low accuracy" group
    # but sometimes okay: DK, IE, RO, UK (in other words, error only happens for some runs)
    # so always error: AT, CZ, DE, IT, SK
# Check on AT (low var, high error), DE (high var, high error), LU (low var, low error), PL (high var, low error)
o <- read.csv('code/gridSearch/outputs/outputOP_090119.csv')
o <- o[o$season == '2010-11' & o$oev_denom == 10 & o$lambda == 1.02 & o$oev_base == 1e4, ]
e1.red <- e1[e1$model == 'Network' & e1$oev_base == 1e4 & e1$oev_denom == 10 & e1$lambda == 1.02 & e1$season == '2010-11', ]
e <- read.csv('code/gridSearch/outputs/outputEns_090119_1wk.csv')
e <- e[e$season == '2010-11' & e$oev_base == 1e4 & e$oev_denom == 10 & e$lambda == 1.02, ]

par(mfrow = c(2, 2), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

# AT:
plot(obs_i$Austria, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Austria (Low Variance, Low Accuracy)')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(61 - 40 + 1, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(61 - 40 + 1 + 0.25, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(61 - 40 + 1 - 0.25, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 61 - 40 + 1, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 1], col = 'coral', pch = 20)
points(x = 61 - 40 + 1 + 0.25, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 2], col = 'coral', pch = 20)
points(x = 61 - 40 + 1 - 0.25, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 3], col = 'coral', pch = 20)

# UK:
plot(obs_i$United.Kingdom, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'UK (High Variance, Low Accuracy)')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(57 - 40 + 1, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(57 - 40 + 1 + 0.25, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(57 - 40 + 1 - 0.25, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 57 - 40 + 1, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 1], col = 'coral', pch = 20)
points(x = 57 - 40 + 1 + 0.25, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 2], col = 'coral', pch = 20)
points(x = 57 - 40 + 1 - 0.25, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 3], col = 'coral', pch = 20)
# and here the error in the variance was much bigger than the OEV - was that true in the previous time steps, as well?

# HR:
plot(obs_i$Croatia, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Croatia (Low Variance, Decent Accuracy)')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(63 - 40 + 1, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(63 - 40 + 1 + 0.25, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(63 - 40 + 1 - 0.25, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 63 - 40 + 1, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 1], col = 'coral', pch = 20)
points(x = 63 - 40 + 1 + 0.25, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 2], col = 'coral', pch = 20)
points(x = 63 - 40 + 1 - 0.25, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 3], col = 'coral', pch = 20)

# ES:
plot(obs_i$Spain, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Spain (High Variance, High Accuracy)')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(58 - 40 + 1, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(58 - 40 + 1 + 0.25, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(58 - 40 + 1 - 0.25, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 58 - 40 + 1, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 1], col = 'coral', pch = 20)
points(x = 58 - 40 + 1 + 0.25, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 2], col = 'coral', pch = 20)
points(x = 58 - 40 + 1 - 0.25, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 3], col = 'coral', pch = 20)

# Think I need the scaled variances, not the unscaled one! To match up with OEVs!

# What difference does oev_base 1e5 make?:
o <- read.csv('code/gridSearch/outputs/outputOP_090119.csv')
o <- o[o$season == '2010-11' & o$oev_denom == 10 & o$lambda == 1.02 & o$oev_base == 1e5, ]
e1.red <- e1[e1$model == 'Network' & e1$oev_base == 1e5 & e1$oev_denom == 10 & e1$lambda == 1.02 & e1$season == '2010-11', ]
e <- read.csv('code/gridSearch/outputs/outputEns_090119_1wk.csv')
e <- e[e$season == '2010-11' & e$oev_base == 1e5 & e$oev_denom == 10 & e$lambda == 1.02, ]

# AT:
plot(obs_i$Austria, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Austria (Low Variance, Low Accuracy)')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'AT' & o$result == 'train' & o$fc_start == 60 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(61 - 40 + 1, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(61 - 40 + 1 + 0.25, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(61 - 40 + 1 - 0.25, 300), e[e$country == 'AT' & e$fc_start == 60 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 61 - 40 + 1, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 1], col = 'coral', pch = 20)
points(x = 61 - 40 + 1 + 0.25, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 2], col = 'coral', pch = 20)
points(x = 61 - 40 + 1 - 0.25, y = o$Est[o$country == 'AT'& o$fc_start == 60 & o$result == 'fcast' & o$week == 61 & o$run == 3], col = 'coral', pch = 20)

# UK:
plot(obs_i$United.Kingdom, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'UK (High Variance, Low Accuracy)')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'UK' & o$result == 'train' & o$fc_start == 56 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(57 - 40 + 1, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(57 - 40 + 1 + 0.25, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(57 - 40 + 1 - 0.25, 300), e[e$country == 'UK' & e$fc_start == 56 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 57 - 40 + 1, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 1], col = 'coral', pch = 20)
points(x = 57 - 40 + 1 + 0.25, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 2], col = 'coral', pch = 20)
points(x = 57 - 40 + 1 - 0.25, y = o$Est[o$country == 'UK'& o$fc_start == 56 & o$result == 'fcast' & o$week == 57 & o$run == 3], col = 'coral', pch = 20)
# and here the error in the variance was much bigger than the OEV - was that true in the previous time steps, as well?

# HR:
plot(obs_i$Croatia, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Croatia (Low Variance, Decent Accuracy)')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'HR' & o$result == 'train' & o$fc_start == 62 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(63 - 40 + 1, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(63 - 40 + 1 + 0.25, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(63 - 40 + 1 - 0.25, 300), e[e$country == 'HR' & e$fc_start == 62 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 63 - 40 + 1, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 1], col = 'coral', pch = 20)
points(x = 63 - 40 + 1 + 0.25, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 2], col = 'coral', pch = 20)
points(x = 63 - 40 + 1 - 0.25, y = o$Est[o$country == 'HR'& o$fc_start == 62 & o$result == 'fcast' & o$week == 63 & o$run == 3], col = 'coral', pch = 20)

# ES:
plot(obs_i$Spain, pch = 4, cex = 1.0, xlab = 'Week of Outbreak', ylab = 'Incidence', main = 'Spain (High Variance, High Accuracy)')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 1], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 2], type = 'b', pch = 20, col = 'gray50')
lines(o$Est[o$country == 'ES' & o$result == 'train' & o$fc_start == 57 & o$run == 3], type = 'b', pch = 20, col = 'gray50')
points(x = rep(58 - 40 + 1, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 1, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(58 - 40 + 1 + 0.25, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 2, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = rep(58 - 40 + 1 - 0.25, 300), e[e$country == 'ES' & e$fc_start == 57 & e$run == 3, 9:308], pch = 20, col = 'gray90', cex = 0.5)
points(x = 58 - 40 + 1, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 1], col = 'coral', pch = 20)
points(x = 58 - 40 + 1 + 0.25, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 2], col = 'coral', pch = 20)
points(x = 58 - 40 + 1 - 0.25, y = o$Est[o$country == 'ES'& o$fc_start == 57 & o$result == 'fcast' & o$week == 58 & o$run == 3], col = 'coral', pch = 20)
# so higher OEV_base allows variance of ensembles to remain larger, presumably b/c it can't yank things around as much











### Repeat by predicted lead week?:
### Read in metrics files:
source('code/gridSearch/comp_netVsIndiv/readIn_metrics.R')

### Limit to obs. lead weeks 2-4:
m <- m[m$leadpkwk_mean %in% 2:4, ]

### Look at percentage error for each model/oev_base combo:
boxplot(log(abs_err_1wk_perc) ~ model + oev_base, data = m)
# these actually look pretty similar (see MAE plot); it's only the log scores that are having trouble

### Read in and format ensemble files:
e1 <- read.csv('code/gridSearch/outputs/outputEns_090119_1wk.csv')
e1.ind <- read.csv('code/individualCountries/outputs/outputEns_082819_1wk.csv')

e1$model <- 'Network'; e1.ind$model <- 'Individual'

m <- m[, c(1:12, 16, 20, 24:25, 27, 36, 40)]

e1.ind <- e1.ind[!is.na(e1.ind$X1), ]

e1 <- merge(e1, m, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model'))
e1.ind <- merge(e1.ind, m, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model'))
e1.ind$gamma <- NULL

### Remove where no observed onset:
e1 <- e1[!is.na(e1$onsetObs5), ]
e1.ind <- e1.ind[!is.na(e1.ind$onsetObs5), ]
# already gone, b/c of how m was formatted

### Un-scale ens:
for (i in 10:309) {
  e1[, i] <- e1[, i] / e1$scaling
  e1.ind[, i] <- e1.ind[, i] / e1.ind$scaling
}; rm(i)

### Merge:
e1$metric <- NULL; e1.ind$metrics <- NULL
e1 <- rbind(e1, e1.ind)
rm(e1.ind)

### Remove unneeded columns:
e1 <- e1[, c(1:308, 310:315, 317:318)]

### Melt?:
e1 <- melt(e1, id.vars = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'country', 'model', 'obs_pkwk', 'pkwk_mean', 'delta_pkwk_mean', 'leadpkwk_mean', 'obs_1week', 'fcast_1week', 'onset5', 'FWeek_pkwk'))

### Remove where obs are NA (and 0?):
e1 <- e1[!is.na(e1$obs_1week), ]
e1.red <- e1[e1$obs_1week > 0, ] # none where obs is 0!

### Calculate % error:
e1$value <- (abs(e1$value - e1$obs_1week) + e1$obs_1week) / e1$obs_1week

### Explore!
boxplot(value ~ model + oev_base, data = e1[e1$leadpkwk_mean == 2, ])
boxplot(log(value) ~ model + oev_base, data = e1[e1$leadpkwk_mean == 2, ]) # I'm not seeing the same pattern here?

quantile(log(e1$value[e1$leadpkwk_mean == 2 & e1$model == 'Individual' & e1$oev_base == 1e4]))
quantile(log(e1$value[e1$leadpkwk_mean == 2 & e1$model == 'Network' & e1$oev_base == 1e4]))
quantile(log(e1$value[e1$leadpkwk_mean == 2 & e1$model == 'Individual' & e1$oev_base == 1e5]))
quantile(log(e1$value[e1$leadpkwk_mean == 2 & e1$model == 'Network' & e1$oev_base == 1e5]))
# if anything, less spread in network model

e1.red <- e1[e1$leadpkwk_mean == 3, ]

# So the issue causing failure by predicted lead week isn't the same as by observed?
boxplot(value ~ model + oev_base, data = e1.red)
boxplot(log(value) ~ model + oev_base, data = e1.red) # oh, now there's a lot more spread in the network model!
# but that's true for both 1e4 and 1e5...

# Check how accurate the peak week predictions are for the four combos:
e1.red2 <- e1.red[e1.red$variable == 'X1', ]
boxplot(FWeek_pkwk ~ model + oev_base, data = e1.red2) # doesn't seem any less accurate?

hist(e1.red2$FWeek_pkwk[e1.red2$model == 'Individual' & e1.red2$oev_base == 1e4])
hist(e1.red2$FWeek_pkwk[e1.red2$model == 'Network' & e1.red2$oev_base == 1e4])
hist(e1.red2$FWeek_pkwk[e1.red2$model == 'Individual' & e1.red2$oev_base == 1e5])
hist(e1.red2$FWeek_pkwk[e1.red2$model == 'Network' & e1.red2$oev_base == 1e5])
# maybe observed lead week is a little later, but no major differences?

hist(e1.red2$delta_pkwk_mean[e1.red2$model == 'Individual' & e1.red2$oev_base == 1e4])
hist(e1.red2$delta_pkwk_mean[e1.red2$model == 'Network' & e1.red2$oev_base == 1e4])
hist(e1.red2$delta_pkwk_mean[e1.red2$model == 'Individual' & e1.red2$oev_base == 1e5])
hist(e1.red2$delta_pkwk_mean[e1.red2$model == 'Network' & e1.red2$oev_base == 1e5])
# yeah, they don't seem any less accurate

### Can look at where log scores are the worst, and see what's happening in Ens files there?:








