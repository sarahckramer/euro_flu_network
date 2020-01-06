
# Load packages:
library(ggplot2)

# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Set strain.
strain <- 'A(H1)'

# Read in necessary data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
syn.dat <- read.csv(paste0('data/by_subtype/synDatCounts_', strain, '_SCALED.csv'))
syn.dat.unscaled <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv(paste0('data/by_subtype/posprop_', strain, '.csv'))
test.dat <- read.csv('data/testCounts_052719.csv')
load('data/by_subtype/scalings_by_subtype_120219.RData')

syn.dat.unscaled <- syn.dat.unscaled[, c(1, count.indices + 1)]
test.dat <- test.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

for (i in 2:dim(iliiso)[2]) {
  syn.dat.unscaled[, i][syn.dat.unscaled[,i ] < 0] <- NA
  pos.dat[, i][pos.dat[, i] < 0] <- NA
  test.dat[, i][test.dat[, i] < 0] <- NA
}

if (strain == 'A(H1)') {
  seasons <- c('2010-11', '2012-13', '2013-14', '2014-15', '2015-16', '2017-18') # H1
} else if (strain == 'A(H3)') {
  seasons <- c('2011-12', '2012-13', '2013-14', '2014-15', '2016-17') # H3
} else if (strain == 'B') {
  seasons <- c('2010-11', '2011-12', '2012-13', '2014-15', '2015-16', '2017-18') # B
} else {
  print('AAAAGGGGGHHHHHH')
}

# We'll set the oev_base/oev_denom as we go

# Source relevant functions:
source('cluster/functions/Fn_initializations.R')
source('cluster/functions/calc_obsvars.R')
source('cluster/functions/replaceLeadingLaggingNAs.R')

# Initiate lists to store OEVs:
oev.new.post = oev.new = oev.old = vector('list', length(seasons))

# # If calculating using rates:
# iliiso[, 2:13] <- iliiso[, 2:13] / 100000
# syn.dat[, 2:13] <- syn.dat[, 2:13] / 100000

# Loop through seasons and calculate both "old" and "new" type of OEV:
for (season.index in 1:length(seasons)) {
  season <- seasons[season.index]
  
  # Get weeks for current season:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  
  # Reduce data to current season:
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  syn_i <- syn.dat[weeks, (1:length(countries) + 1)]
  syn_i_unscaled <- syn.dat.unscaled[weeks, (1:length(countries) + 1)]
  test_i <- test.dat[weeks, (1:length(countries) + 1)]
  pos_i <- pos.dat[weeks, (1:length(countries) + 1)]
  
  # Replace leading/lagging NAs:
  for (count.index in 1:length(countries)) {
    obs_i[, count.index] <- replaceLeadLag(obs_i[, count.index])
    syn_i[, count.index] <- replaceLeadLag(syn_i[, count.index])
    syn_i_unscaled[, count.index] <- replaceLeadLag(syn_i_unscaled[, count.index])
    pos_i[, count.index] <- replaceLeadLag(pos_i[, count.index])
  }
  
  # Replace 0s in test_i w/ NA (b/c can't divide by 0!):
  test_i[test_i == 0 & !is.na(test_i)] <- NA
  
  # Calculate OEVs used in network model:
  obs_vars <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i, ntests = test_i, posprops = pos_i, 0.6, 3.0, tmp_exp = 2.0)
  
  # Calculate OEVs used in individual country models:
  obs_vars.indiv <- calc_obsvars(obs = obs_i, 1e4, 10)
  # obs_vars.indiv <- calc_obsvars(obs = obs_i, 0, 10)
  
  # And calculate OEVs without scaling:
  obs_vars.alt <- calc_obsvars_nTest(obs = obs_i, syn_dat = syn_i_unscaled, ntests = test_i, posprops = pos_i, oev_base = 0.6, oev_denom = 3.0, tmp_exp = 2.0)
  
  # Store results in lists:
  oev.new[[season.index]] <- obs_vars
  oev.old[[season.index]] <- obs_vars.indiv
  oev.new.post[[season.index]] <- obs_vars.alt
}

# Format lists for plotting:
oev.new.post.real <- oev.new.post
oev.alt.df = oev.new.df = oev.old.df = oev.post.df = NULL
for (ix in 1:length(seasons)) {
  colnames(oev.new[[ix]]) <- countries
  oev.new[[ix]] <- melt(oev.new[[ix]])
  oev.new[[ix]]$season <- seasons[ix]
  oev.new.df <- rbind(oev.new.df, oev.new[[ix]])
  
  colnames(oev.old[[ix]]) <- countries
  oev.old[[ix]] <- melt(oev.old[[ix]])
  oev.old[[ix]]$season <- seasons[ix]
  oev.old.df <- rbind(oev.old.df, oev.old[[ix]])
  
  colnames(oev.new.post[[ix]]) <- countries
  oev.new.post[[ix]] <- melt(oev.new.post[[ix]])
  oev.new.post[[ix]]$season <- seasons[ix]
  oev.alt.df <- rbind(oev.alt.df, oev.new.post[[ix]])
  
  colnames(oev.new.post.real[[ix]]) <- countries
  for (jx in 1:12) {
    if (jx != 4) {
      oev.new.post.real[[ix]][, jx] <- oev.new.post.real[[ix]][, jx] * scalings.new[[4]][jx]
    } else {
      if (seasons[ix] %in% c('2012-13', '2013-14')) {
        oev.new.post.real[[ix]][, jx] <- oev.new.post.real[[ix]][, jx] * scalings.new[[4]][13]
      } else {
        oev.new.post.real[[ix]][, jx] <- oev.new.post.real[[ix]][, jx] * scalings.new[[4]][jx]
      }
    }
  }
  oev.new.post.real[[ix]] <- melt(oev.new.post.real[[ix]])
  oev.new.post.real[[ix]]$season <- seasons[ix]
  oev.post.df <- rbind(oev.post.df, oev.new.post.real[[ix]])
  
}

names(oev.new.df) = names(oev.old.df) = names(oev.alt.df) = names(oev.post.df) = c('time', 'country', 'value', 'season')

# Combine:
oev.new.df$model <- 'New'; oev.old.df$model <- 'Old'; oev.alt.df$model <- 'No Scaling'; oev.post.df$model <- 'Post Scaling'
oev.df <- rbind(oev.old.df, oev.alt.df, oev.new.df, oev.post.df)

### PLOTS ###

# Only for season?
p1 <- ggplot(data = oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'New') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p2 <- ggplot(data = oev.old.df[oev.old.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'Old') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p3 <- ggplot(data = oev.post.df[oev.post.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'Scaling Post') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()
p4 <- ggplot(data = oev.alt.df[oev.alt.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'OEV', title = 'No Scaling') +
  geom_line(aes(x = time, y = value, group = season, colour = season)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw()

# Save plots to pdf:
pdf('results/explore_oev/OEV_comp_H1.pdf', width = 12, height = 7)
print(p2)
print(p1)
print(p3)
print(p4)
# dev.off()

# Now plot by country, comparing new to old:
oev.df <- oev.df[oev.df$time < 33, ]
oev.df <- oev.df[oev.df$model != 'No Scaling', ]
# oev.df <- oev.df[oev.df$model != 'No Scaling' & oev.df$model != 'Post Scaling', ]
oev.df$group <- paste(oev.df$season, oev.df$model, sep = '_'); oev.df$group <- factor(oev.df$group)
p1 <- ggplot(data = oev.df, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p1)
# dev.off()

# But then also a comparison plot of scaling before vs. after:
oev.df <- oev.df[oev.df$model != 'Old', ]
p1 <- ggplot(data = oev.df, aes(x = time, y = value, group = group, colour = model)) + geom_line() +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Weeks Since Season Start', y = 'OEV', colour = '')
print(p1)

# What about the order at which the countries are weighted?
p1 <- ggplot(oev.alt.df[oev.alt.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Unscaled') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 3) + theme_bw() + scale_y_log10()
p2 <- ggplot(oev.new.df[oev.new.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Pre-Scaled') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 3) + theme_bw() + scale_y_log10()
p3 <- ggplot(oev.post.df[oev.post.df$time <= 33, ]) + labs(x = 'Weeks Since Season Start', y = 'log(OEV)', title = 'Post-Scaled') +
  geom_line(aes(x = time, y = value, group = country, colour = country)) +
  facet_wrap(~ season, scales = 'free_y', ncol = 3) + theme_bw() + scale_y_log10()
grid.arrange(p1, p2, p3, ncol = 1)

dev.off()












