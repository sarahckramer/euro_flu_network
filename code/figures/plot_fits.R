
### Plot model fits to observations ###

# Set strain:
strain <- 'A(H1)'

# Read in fits to observations:
op <- read.csv('results/fits/outputOP_A(H1)_fitsOnly.csv')

# Add country names:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
op$country <- countries[op$country + 1]
op$country <- factor(op$country)

# Remove unnecessary columns:
op <- op[, c(3, 5, 8, 11:13)]

# Read in observed data:
iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# note: when merging, we should automatically remove where no observed data for a given fit

# Form new data frame of observed by season/week/country
source('cluster/functions/Fn_initializations.R')
names(iliiso)[2:13] <- countries

obs_new <- data.frame()
seasons <- levels(op$season)

for (season in seasons) {
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  nsn <- tmp$nsn
  
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  obs_i$season <- season
  obs_i$week <- seq(40, length.out = nsn)
  obs_i <- melt(obs_i, id.vars = c('season', 'week'))
  names(obs_i)[3:4] <- c('country', 'Obs')
  obs_new <- rbind(obs_new, obs_i)
}; rm(tmp, weeks, nsn, obs_i, season)

# Merge fits and observations:
op <- merge(op, obs_new, by = c('season', 'week', 'country'))
rm(iliiso, obs_new)

# Remove where no data:
op <- op[!is.na(op$Obs), ]

# Restrict to influenza season? (wks 40-20):
op.all <- op
op <- op[op$week <= 73, ] # 2015-16 has length 53
op <- op[!(op$season != '2015-16' & op$week == 73), ] # but remove that week for all other seasons

# Plot!
pdf('results/plots/model_fits_A(H1).pdf', width = 13, height = 8)
for (season in seasons) {
  op.temp <- op[op$season == season, ]
  p1 <- ggplot(data = op.temp, aes(x = week, y = Est, group = run)) + geom_line(colour = '#377eb8', lwd = 0.71) + 
    geom_point(aes(x = week, y = Obs), colour = 'black', size = 1.2) + facet_wrap(~ country, ncol = 4) + #, scales = 'free_y') +
    theme_classic() + theme(aspect.ratio = 0.75, axis.text = element_text(size = 12), axis.title = element_text(size = 14),
                            strip.background = element_blank(), strip.text = element_blank()) +
    scale_x_continuous(breaks = seq(40, 75, by = 5)) +
    labs(x = 'Week Number', y = 'Observed/Fitted Incidence (Scaled)')
  dat.text <- data.frame(label = countries, country = countries, run = 0)
  print(p1 + geom_text(data = dat.text, mapping = aes(x = 42, y = max(max(op.temp$Obs), max(op.temp$Est)) + 1000, label = label), size = 5))
  # change country names; point size
}
dev.off()
rm(op.temp, dat.text, season, p1)

# Get RMSEs:
rmse.out <- NULL
for (season in levels(op$season)) {
  for (country in levels(op$country)) {
    for (run in unique(op$run)) {
      op.temp <- op[op$season == season & op$country == country & op$run == run, ]
      # will be empty if no data for season, but this is okay - will just report an NA
      rmse.calc <- sqrt(mean((op.temp$Est - op.temp$Obs) ** 2))
      rmse.out <- rbind(rmse.out, c(season, country, run, rmse.calc))
    }
  }
}
rm(op.temp, rmse.calc, season, country, run)

rmse.out <- as.data.frame(rmse.out)
names(rmse.out) <- c('season', 'country', 'run', 'rmse')
rmse.out$rmse <- as.numeric(as.character(rmse.out$rmse))

# Also break down by whether there's an onset or not:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- m[m$subtype == 'A(H1)', ]
m <- unique(m[, c(1, 8, 30)])

rmse.out <- merge(rmse.out, m, by = c('season', 'country'))
# this also removes any NAs in rmse

# Save these results temporarily:



# Read in RMSE for all subtypes and combine; delete old files:





# This will allow us to ultimately get:
    # Range and mean/median for chosen season
    # Overall mean/median
    # Mean/median by subtype





















