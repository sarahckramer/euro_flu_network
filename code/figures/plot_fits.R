
### Plot model fits to observations ###

# Set strain:
strain <- 'B'

# Read in fits to observations:
op <- read.csv(paste0('results/fits/outputOP_', strain,'_fitsOnly.csv'))

# Add country names:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
op$country <- countries[op$country + 1]
op$country <- factor(op$country)

# Remove unnecessary columns:
op <- op[, c(3, 5, 8, 11:13)]

# Read in observed data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
# note: when merging, we should automatically remove where no observed data for a given fit

# Form new data frame of observed by season/week/country
source('cluster/functions/Fn_initializations.R')
names(iliiso)[2:13] <- countries

obs_new <- data.frame()
seasons <- levels(op$season)

source('cluster/functions/replaceLeadingLaggingNAs.R')
for (season in seasons) {
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  nsn <- tmp$nsn
  
  obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
  
  # replace leading/lagging NAs:
  for (ix in 1:12) {
    obs_i[, ix] <- replaceLeadLag(obs_i[, ix])
  }; rm(ix)
  
  obs_i$season <- season
  obs_i$week <- seq(40, length.out = nsn)
  obs_i <- melt(obs_i, id.vars = c('season', 'week'))
  names(obs_i)[3:4] <- c('country', 'Obs')
  obs_new <- rbind(obs_new, obs_i)
}; rm(tmp, weeks, nsn, obs_i, season)

# Merge fits and observations:
op <- merge(op, obs_new, by = c('season', 'week', 'country'))
rm(iliiso, obs_new)

# Remove where no data FOR WHOLE SEASON:
# op <- op[!is.na(op$Obs), ]
op <- op[!(op$country == 'FR' & op$season %in% c('2010-11', '2011-12')) &
           !(op$country == 'CZ' & op$season == '2013-14') &
           !(op$country == 'PL' & op$season == '2011-12'), ]

# Restrict to influenza season? (wks 40-20):
op.all <- op
op <- op[op$week <= 73, ] # 2015-16 has length 53
op <- op[!(op$season != '2015-16' & op$week == 73), ] # but remove that week for all other seasons

# Calculate 95% confidence intervals:
op$lower.bound <- op$Est - 1.96 * (op$Est_sd)
op$upper.bound <- op$Est + 1.96 * (op$Est_sd)
op$lower.bound[op$lower.bound < 0] <- 0

# Plot!
pdf(paste0('results/plots/model_fits_', strain, '.pdf'), width = 13, height = 8)
for (season in seasons) {
  op.temp <- op[op$season == season, ]
  p1 <- ggplot(data = op.temp, aes(x = week, y = Est, group = run)) +
    geom_ribbon(aes(x = week, ymin = lower.bound, ymax = upper.bound, group = run), fill = 'gray50', alpha = 0.1) +
    geom_line(colour = '#377eb8', lwd = 0.71) +
    geom_point(aes(x = week, y = Obs), colour = 'black', size = 1.2) + facet_wrap(~ country, ncol = 4) + #, scales = 'free_y') +
    theme_classic() + theme(aspect.ratio = 0.75, axis.text = element_text(size = 12), axis.title = element_text(size = 14),
                            strip.background = element_blank(), strip.text = element_blank()) +
    scale_x_continuous(breaks = seq(40, 75, by = 5)) +
    labs(x = 'Week Number', y = 'Observed/Fitted Incidence (Scaled)')
  dat.text <- data.frame(label = countries, country = countries, run = 0)
  print(p1 + geom_text(data = dat.text, mapping = aes(x = 42, y = max(max(op.temp$Obs, na.rm = TRUE), max(op.temp$Est)) + 1000, label = label), size = 5))
  # change country names; point size
}
dev.off()

# # pdf('../drafts/NetworkModel/supplemental/FigS2B_NEW2.pdf', width = 13, height = 8)
season <- '2012-13'
op.temp <- op[op$season == season, ]
p1 <- ggplot(data = op.temp, aes(x = week, y = Est, group = run)) +
  geom_ribbon(aes(x = week, ymin = lower.bound, ymax = upper.bound, group = run), fill = 'gray50', alpha = 0.1) +
  geom_line(colour = '#377eb8', lwd = 0.71) +
  geom_point(aes(x = week, y = Obs), colour = 'black', size = 1.2) + facet_wrap(~ country, ncol = 4) + #, scales = 'free_y') +
  theme_classic() + theme(aspect.ratio = 0.75, axis.text = element_text(size = 12), axis.title = element_text(size = 14),
                          strip.background = element_blank(), strip.text = element_blank(), title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(40, 75, by = 5)) +
  labs(title = 'B', x = 'Week Number', y = 'Observed/Fitted Incidence (Scaled)')
dat.text <- data.frame(label = countries, country = countries, run = 0)
p1 <- p1 + geom_text(data = dat.text, mapping = aes(x = 42, y = max(max(op.temp$Obs, na.rm = TRUE), max(op.temp$Est)) + 1000, label = label), size = 5)
# print(p1 + geom_text(data = dat.text, mapping = aes(x = 42, y = max(max(op.temp$Obs, na.rm = TRUE), max(op.temp$Est)) + 1000, label = label), size = 5))
print(p1)
ggsave(file = 'results/plots/figS2B.svg', plot = p1, width = 13, height = 8)
# # dev.off()
# rm(op.temp, dat.text, season, p1)

############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################

# # Now, don't want to include NAs that were replaced with zeros in numerical assessment of fits:
# rm(list = ls())
# 
# strain <- 'B'
# op <- read.csv(paste0('results/fits/outputOP_', strain,'_fitsOnly.csv'))
# 
# countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# op$country <- countries[op$country + 1]
# op$country <- factor(op$country)
# 
# op <- op[, c(3, 5, 8, 11:13)]
# 
# iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
# source('cluster/functions/Fn_initializations.R')
# names(iliiso)[2:13] <- countries
# 
# obs_new <- data.frame()
# seasons <- levels(op$season)
# 
# for (season in seasons) {
#   tmp <- Fn_dates(season)
#   weeks <- tmp$weeks + 3
#   nsn <- tmp$nsn
#   
#   obs_i <- iliiso[weeks, (1:length(countries) + 1)] # extract relevant data for all countries
#   obs_i$season <- season
#   obs_i$week <- seq(40, length.out = nsn)
#   obs_i <- melt(obs_i, id.vars = c('season', 'week'))
#   names(obs_i)[3:4] <- c('country', 'Obs')
#   obs_new <- rbind(obs_new, obs_i)
# }; rm(tmp, weeks, nsn, obs_i, season)
# 
# op <- merge(op, obs_new, by = c('season', 'week', 'country'))
# rm(iliiso, obs_new)
# 
# op <- op[!(op$country == 'FR' & op$season %in% c('2010-11', '2011-12')) &
#            !(op$country == 'CZ' & op$season == '2013-14') &
#            !(op$country == 'PL' & op$season == '2011-12'), ]
# op <- op[op$week <= 73, ] # 2015-16 has length 53
# op <- op[!(op$season != '2015-16' & op$week == 73), ] # but remove that week for all other seasons
# 
# # Get RMSEs:
# rmse.out <- NULL
# for (season in levels(op$season)) {
#   for (country in levels(op$country)) {
#     for (run in unique(op$run)) {
#       op.temp <- op[op$season == season & op$country == country & op$run == run, ]
#       # will be empty if no data for season, but this is okay - will just report an NA
#       rmse.calc <- sqrt(mean((op.temp$Est - op.temp$Obs) ** 2, na.rm = TRUE))
#       rmse.out <- rbind(rmse.out, c(season, country, run, rmse.calc))
#     }
#   }
# }
# rm(op.temp, rmse.calc, season, country, run)
# 
# rmse.out <- as.data.frame(rmse.out)
# names(rmse.out) <- c('season', 'country', 'run', 'rmse')
# rmse.out$rmse <- as.numeric(as.character(rmse.out$rmse))
# rmse.out <- rmse.out[!is.na(rmse.out$rmse), ]
# 
# # Get Pearson correlations:
# corr.out <- NULL
# for (season in levels(op$season)) {
#   for (country in levels(op$country)) {
#     for (run in unique(op$run)) {
#       op.temp <- op[op$season == season & op$country == country & op$run == run, ]
#       if (dim(op.temp)[1] > 0) {
#         corr.out <- rbind(corr.out, c(season, country, run, cor.test(op.temp$Est, op.temp$Obs, method = 'pearson')$estimate))
#       }
#     }
#   }
# }
# 
# corr.out <- as.data.frame(corr.out)
# names(corr.out) <- c('season', 'country', 'run', 'corr')
# corr.out$corr <- as.numeric(as.character(corr.out$corr))
# 
# # Also break down by whether there's an onset or not:
# m <- read.csv('results/network/outputMet_pro_PROC.csv')
# m <- m[m$subtype == strain, ]
# m <- unique(m[, c(1, 8, 30)])
# 
# rmse.out <- merge(rmse.out, m, by = c('season', 'country'))
# corr.out <- merge(corr.out, m, by = c('season', 'country'))
# # this also removes any NAs in rmse
# 
# # Save these results temporarily:
# write.csv(rmse.out, file = paste0('results/fits/rmse_', strain, '.csv'), row.names = FALSE)
# write.csv(corr.out, file = paste0('results/fits/corr_', strain, '.csv'), row.names = FALSE)
#
# rm(list = ls())

# # Read in RMSE for all subtypes and combine; delete old files:
# r1 <- read.csv('results/fits/rmse_A(H1).csv')
# r2 <- read.csv('results/fits/rmse_A(H3).csv')
# r3 <- read.csv('results/fits/rmse_B.csv')
# # it seems to make sense to plot with all points used for fitting, but only judge RMSE against the actual data, right?
# 
# r1$subtype <- 'A(H1)'; r2$subtype <- 'A(H3)'; r3$subtype <- 'B'
# 
# r <- rbind(r1, r2, r3); rm(r1, r2, r3)
# write.csv(r, file = 'results/fits/rmse_COMB.csv', row.names = FALSE)

# # Read in corr for all subtypes and combine; delete old files:
# c1 <- read.csv('results/fits/corr_A(H1).csv')
# c2 <- read.csv('results/fits/corr_A(H3).csv')
# c3 <- read.csv('results/fits/corr_B.csv')
# 
# c1$subtype <- 'A(H1)'; c2$subtype <- 'A(H3)'; c3$subtype <- 'B'
# 
# corr <- rbind(c1, c2, c3); rm(c1, c2, c3)
# write.csv(corr, file = 'results/fits/corr_COMB.csv', row.names = FALSE)

# This will allow us to ultimately get:
    # Range and mean/median for chosen season
    # Overall mean/median
    # Mean/median by subtype
r <- read.csv('results/fits/rmse_COMB.csv')
corr <- read.csv('results/fits/corr_COMB.csv')

# Combine:
r <- merge(r, corr, by = c('season', 'country', 'run', 'subtype', 'onsetObs5'))
rm(corr)

summary(r$rmse) # 34.23-1825.89; mean = 370.25; median = 222.62
summary(r$rmse[!is.na(r$onsetObs5)]) # 55.21-1825.89; mean = 408.88; median = 257.19
summary(r$corr) # -0.268-0.997; mean = 0.913; median = 0.958
summary(r$corr[!is.na(r$onsetObs5)]) # 0.608-0.997; mean = 0.949; median = 0.965

r$subtype <- factor(r$subtype)
r.red <- r[!is.na(r$onsetObs5), ]

boxplot(rmse ~ subtype, data = r.red) # looks lower for A(H1), highest for A(H3)
boxplot(rmse ~ season, data = r.red) # lowest for 13-14, 15-16; higher for 11-12, 16-17, 17-18 - somewhat associated with which are H1 seasons
boxplot(rmse ~ country, data = r.red) # looks worse for LU; lowest for FR/NL; very wide range for SK/PL
boxplot(rmse ~ run, data = r.red) # no difference
boxplot(rmse ~ onsetObs5, data = r.red) # looks lower for later onsets - but that's probably just b/c it has to fit 0 for so long

boxplot(corr ~ subtype, data = r.red) # look similar, but all so high
boxplot(corr ~ season, data = r.red) # lower for 11-12, high for 15-16
boxplot(corr ~ country, data = r.red) # lower for LU, HU, PL, SK (but esp. LU); best for FR and IT (these tend to have smooth outbreaks right?)
boxplot(corr ~ run, data = r.red) # same
boxplot(corr ~ onsetObs5, data = r.red) # lower with later onsets - correlation w/ zero could really go either way?

# which of these associations are actually worth testing? subtype is probably the main thing, and maybe country, although there are so many it's hard to break down

kruskal.test(rmse ~ subtype, data = r.red) # highly sig - p=8.833e-16
kruskal.test(rmse ~ country, data = r.red) # highly sig - p<2.2e-16

kruskal.test(corr ~ subtype, data = r.red) # sig - p=0.0001874
kruskal.test(corr ~ country, data = r.red) # highly sig - p<2.2e-16

library(PMCMR)
library(PMCMRplus)

posthoc.kruskal.nemenyi.test(rmse ~ subtype, data = r.red) # H1, then B, then H3 (all p<0.0001, except h3vb < 0.03)
posthoc.kruskal.nemenyi.test(rmse ~ country, data = r.red) # LU sig different from all except PL (all p<0.0001); PL sig higher than NL
# for country: set p to 0.01/66 = 0.00015, round to 0.0001

posthoc.kruskal.nemenyi.test(corr ~ subtype, data = r.red) # H1 sig different than B and H3 (p < 0.01), but no difference between h3 and b
posthoc.kruskal.nemenyi.test(corr ~ country, data = r.red)














