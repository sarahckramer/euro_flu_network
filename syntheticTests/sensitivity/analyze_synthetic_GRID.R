
# Read in 4 results sets:
load('syntheticTests/sensitivity/outputs_datasets/grid_AH-Travel.RData')
load('syntheticTests/sensitivity/outputs_datasets/grid_noAH_Travel.RData')
load('syntheticTests/sensitivity/outputs_datasets/grid_AH-noTravel.RData')
load('syntheticTests/sensitivity/outputs_datasets/grid_noAH-noTravel.RData')

# Want: Plots of general geographic spread (so, mean or median PT, with spread), in order, colored by lat/long, by model type (1 of 4) as well as values of D/R0(mx)/airScale
# Want to see how geographic patterns, as well as synchrony, and absolute values of PT, change by model and parameter
    # How strong is the impact of travel? Of humidity forcing?
    # Also just want to look at sensitivity to seemingly important parameters - what impact of R0(mx) and D? Or does AH dominate?

# Countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
n <- length(countries)

# Calculate PT for each country in each outbreak of each model; create "m" data frame:
outbreaks <- list(newI.ens.BOTH, newI.ens.noTravel, newI.ens.noAH, newI.ens.None)
source('code/functions/Util.R'); wk_start <- 1

m <- NULL
for (i in 1:length(outbreaks)) {
  newI.ens <- outbreaks[[i]]
  
  for (ix in 1:length(newI.ens)) {
    obs_i <- newI.ens[[ix]]
    
    for (l in 1:n) {
      obs_i.temp <- obs_i[, l]
      ot <- findOnset(obs_i.temp, 500)$onset
      
      if (is.na(ot)) {
        pt <- NA
      } else {
        pt <- which.max(obs_i.temp)
      }
      
      m <- rbind(m, c(i, ix, countries[l], pt))
    }
    
  }
}

m <- as.data.frame(m)
names(m) <- c('model', 'outbreak', 'country', 'pt')

m$model <- c('Both', 'noTravel', 'noAH', 'Neither')[m$model]; m$model <- factor(m$model)
m$model <- factor(m$model, levels = levels(m$model)[c(1, 4, 3, 2)])

m <- m[!is.na(m$pt), ] # we don't care about those with no outbreak, right?

# Also calculate time after first peak of the "season" to each country's peak:
m$pt <- as.numeric(as.character(m$pt))
m$pt.rank <- NA
for (model in levels(m$model)) {
  for (i in levels(m$outbreak)) {
    m.temp <- m[m$model == model & m$outbreak == i, ]
    
    # if (length(m.temp$country) != 0) {
    if (length(m.temp$country) >= 19) {
      pt.base <- min(m.temp$pt)
      m.temp$pt.rank <- m.temp$pt - pt.base
      m$pt.rank[m$model == model & m$outbreak == i] <- m.temp$pt.rank
    }
    
  }
}; rm(m.temp)

m <- m[!is.na(m$pt.rank), ] # keep only outbreaks where most countries have an outbreak

# Add (relevant) param values:
m$D = m$R0val = m$airScale = NA
m$outbreak <- as.numeric(as.character(m$outbreak))

parms.rel1 <- as.data.frame(t(rbind(c(rep(2.5, 90), rep(4.25, 90), rep (6.0, 90)), rep(c(rep(1.5, 30), rep(2.0, 30), rep(2.5, 30)), 3), rep(c(rep(0.75, 10), rep(1.00, 10), rep(1.25, 10)), 9))))
names(parms.rel1) <- c('D', 'R0mx', 'airScale')

m$D[m$model == 'Both'] <- parms.rel1[, 1][m$outbreak[m$model == 'Both']]
m$R0val[m$model == 'Both'] <- parms.rel1[, 2][m$outbreak[m$model == 'Both']]
m$airScale[m$model == 'Both'] <- parms.rel1[, 3][m$outbreak[m$model == 'Both']]

parms.rel2 <- as.data.frame(t(rbind(c(rep(2.5, 30), rep(4.25, 30), rep (6.0, 30)), rep(c(rep(1.5, 10), rep(2.0, 10), rep(2.5, 10)), 3))))
names(parms.rel2) <- c('D', 'R0mx')

m$D[m$model == 'noTravel'] <- parms.rel2[, 1][m$outbreak[m$model == 'noTravel']]
m$R0val[m$model == 'noTravel'] <- parms.rel2[, 2][m$outbreak[m$model == 'noTravel']]

parms.rel3 <- as.data.frame(t(rbind(c(rep(2.5, 90), rep(4.25, 90), rep (6.0, 90)), rep(c(rep(1.2, 30), rep(1.7, 30), rep(2.2, 30)), 3), rep(c(rep(0.75, 10), rep(1.00, 10), rep(1.25, 10)), 9))))
names(parms.rel3) <- c('D', 'R0', 'airScale')

m$D[m$model == 'noAH'] <- parms.rel3[, 1][m$outbreak[m$model == 'noAH']]
m$R0val[m$model == 'noAH'] <- parms.rel3[, 2][m$outbreak[m$model == 'noAH']]
m$airScale[m$model == 'noAH'] <- parms.rel3[, 3][m$outbreak[m$model == 'noAH']]

parms.rel4 <- as.data.frame(t(rbind(c(rep(2.5, 30), rep(4.25, 30), rep (6.0, 30)), rep(c(rep(1.2, 10), rep(1.7, 10), rep(2.2, 10)), 3))))
names(parms.rel4) <- c('D', 'R0')

m$D[m$model == 'Neither'] <- parms.rel4[, 1][m$outbreak[m$model == 'Neither']]
m$R0val[m$model == 'Neither'] <- parms.rel4[, 2][m$outbreak[m$model == 'Neither']]

# Get country latitudes and longitudes (central):
l <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/travel_data_info/flight_data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% c(countries, 'GB'), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)

m <- merge(m, l, by.x = 'country', by.y = 'iso_a2')

# Plot results by: model, param value, country, latitude/longitude
# m1 <- m[m$model == 'Both', ]
# 
# pt.med <- aggregate(pt ~ country, data = m1, FUN = median)
# pt.med <- pt.med[order(pt.med$pt), ]
# 
# pt.rank.med <- aggregate(pt.rank ~ country, data = m1, FUN = median)
# pt.rank.med <- pt.rank.med[order(pt.rank.med$pt.rank), ]
# 
# m1$country <- factor(m1$country, levels = pt.med$country)
# p1 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt, fill = Latitude), outlier.shape = 'x') +
#   theme_classic() + labs(x = '', y = 'Peak Timing') + scale_fill_viridis()
# p2 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt, fill = Longitude), outlier.shape = 'x') +
#   theme_classic() + labs(x = '', y = 'Peak Timing') + scale_fill_viridis()
# 
# m1$country <- factor(m1$country, levels = pt.rank.med$country)
# lat.rank <- aggregate(Latitude ~ country, data = m1, FUN = median)
# lat.rank <- lat.rank[rev(order(lat.rank$Latitude)), ]
# m1$country <- factor(m1$country, levels = lat.rank$country)
# p3 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
#   theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
#   facet_wrap(~ R0val)
# print(p3)
# p4 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Longitude), outlier.shape = 'x') +
#   theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis()
# 
# # grid.arrange(p1, p3, p2, p4, ncol = 1)

pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_outbreaksOnly.pdf', height = 12, width = 10)

m1 <- m[m$model == 'Both', ]

lat.rank <- aggregate(Latitude ~ country, data = m1, FUN = median)
lat.rank <- lat.rank[rev(order(lat.rank$Latitude)), ]
m1$country <- factor(m1$country, levels = lat.rank$country)

p1 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
    theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
    facet_wrap(~ R0val, ncol = 1)
print(p1)

p2 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ D, ncol = 1)
print(p2)

p3 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ airScale, ncol = 1)
print(p3)

# When travel and AH: few outbreaks when R0mx low, but R0 2 vs. 2.5 seems to have earlier peaks in east (HU, SI) - AH not as strong b/c R0 not as high?; general north to south pattern, but
# note that there's not a strong pattern between countries in the mid-latitude region, and actually southern ones even tend to peak first (E before W); higher D also seems more E-to-W;
# no impact of airScale; I guess higher R0mx amplifies the impact of AH

# p4 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
#   theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
#   facet_grid(R0val ~ D)
# print(p4)
# not much added

m1 <- m[m$model == 'noTravel', ]
m1$country <- factor(m1$country, levels = lat.rank$country)

p1 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ R0val, ncol = 1)
print(p1)

p2 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ D, ncol = 1)
print(p2)

# IE peaks later w/o travel? maybe PT too?

m1 <- m[m$model == 'noAH', ]
m1$country <- factor(m1$country, levels = lat.rank$country)

p1 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ R0val, ncol = 1)
print(p1)

p2 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ D, ncol = 1)
print(p2)

p3 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ airScale, ncol = 1)
print(p3)

# higher R0 and lower D have a clear impact on increasing synchrony; no geographical patterns; airScale still does nothing

m1 <- m[m$model == 'Neither', ]
m1$country <- factor(m1$country, levels = lat.rank$country)

p1 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ R0val, ncol = 1)
print(p1)

p2 <- ggplot(data = m1) + geom_boxplot(aes(x = country, y = pt.rank, fill = Latitude), outlier.shape = 'x') +
  theme_classic() + labs(x = '', y = 'Relative PT (Weeks after first peak)') + scale_fill_viridis() +
  facet_wrap(~ D, ncol = 1)
print(p2)

dev.off()




















