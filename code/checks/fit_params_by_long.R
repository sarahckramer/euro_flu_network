
### Params ###
# Read in fit parameter values from Aim1:
fits1 <- read.csv('../drafts/Aim1/FinalEdits/Supporting Tables and Datasets/S5_Table.csv')
# note: params in network don't change by country, so can't look at params by longitude for network

# Keep only countries of interest:
fits1 <- fits1[fits1$Country %in% levels(fits1$Country)[c(2, 5, 14, 16:17, 21, 23, 26, 29, 31, 37, 41, 48:49, 51, 55:58, 62)], ]
fits1$Country <- factor(fits1$Country)

# Change country names:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT', 'LU', 'NL',
               'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
levels(fits1$Country) <- countries

# Add info on longitude:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany',
                   'Hungary', 'Ireland', 'Italy', 'Luxembourg', 'Netherlands',
                   'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                   'United Kingdom')
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic', 'UK')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'lat', 'long')]

fits1 <- merge(fits1, world.cities, by.x = 'Country', by.y = 'country.etc')
fits1 <- fits1[order(fits1$long), ]
fits1$Country <- factor(fits1$Country, levels = unique(fits1$Country))

# Plot parameter ranges by country/longitude:
p1 <- ggplot(data = fits1[fits1$Parameter == 'L', ], aes(x = Country, y = Parameter.Value, group = Country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', y = 'L', fill = 'Long.')
p2 <- ggplot(data = fits1[fits1$Parameter == 'D', ], aes(x = Country, y = Parameter.Value, group = Country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', y = 'D', fill = 'Long.')
p3 <- ggplot(data = fits1[fits1$Parameter == 'R0max', ], aes(x = Country, y = Parameter.Value, group = Country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', y = 'R0max', fill = 'Long.')
p4 <- ggplot(data = fits1[fits1$Parameter == 'R0min', ], aes(x = Country, y = Parameter.Value, group = Country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', y = 'R0min', fill = 'Long.')

pdf('syntheticTests/outputs/explore/params_by_long_Aim1.pdf', width = 9, height = 12)
grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()

plot(fits1$long[fits1$Parameter == 'D'], fits1$Parameter.Value[fits1$Parameter == 'D'], pch = 20)
plot(fits1$long[fits1$Parameter == 'R0max'], fits1$Parameter.Value[fits1$Parameter == 'R0max'], pch = 20)
plot(fits1$long[fits1$Parameter == 'R0min'], fits1$Parameter.Value[fits1$Parameter == 'R0min'], pch = 20)
# but this is basically the same thing we just plotted

### S0 ###
# Read in results from network:
o <- read.csv('code/gridSearch/outputs/outputOP_090119.csv')

# Limit to oev_base/oev_denom/lambda of interest?:

# Get final fit:
o <- o[o$fc_start == 69, ]

# Get training only:
o <- o[o$result == 'train', ]

# Reduce to necessary columns:
o <- o[, c(1:5, 8, 10:11)]

# Loop through all and get S0:
fits2 <- NULL
for (country in levels(o$country)) {
  for (season in levels(o$season)) {
    
    for (o1 in unique(o$oev_base)) {
      for (o2 in unique(o$oev_denom)) {
        for (lam in unique(o$lambda)) {
          
          for (run in unique(o$run)) {
            o.temp <- o[o$country == country & o$season == season & o$run == run & o$oev_base == o1 & o$oev_denom == o2 & o$lambda == lam, ]
            s.max <- max(o.temp$S)
            fits2 <- rbind(fits2, c(country, season, run, o1, o2, lam, s.max))
          }
          
        }
      }
    }
    
  }
}

fits2 <- as.data.frame(fits2)
names(fits2) <- c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'S0')
fits2$S0 <- as.numeric(as.character(fits2$S0))

# First, do values overall differ by o1/o2/lam?
boxplot(S0 ~ oev_base, data = fits2) # get much higher w/ 1e4
boxplot(S0 ~ oev_denom, data = fits2) # no pattern
boxplot(S0 ~ lambda, data = fits2) # trend higher as lambda increases

# Restrict to certain oev_denom/lambda values:
fits2.full <- fits2
fits2 <- fits2[fits2$oev_denom == 10 & fits2$lambda == 1.02, ]

# Add information on longitude:
fits2 <- merge(fits2, world.cities, by.x = 'country', by.y = 'country.etc')
fits2 <- fits2[order(fits2$long), ]
fits2$country <- factor(fits2$country, levels = unique(fits2$country))

# Now look at S0 by country for Aim1 and network:
p1 <- ggplot(data = fits1[fits1$Parameter == 'S', ], aes(x = Country, y = Parameter.Value, group = Country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', y = 'S0', fill = 'Long.')
p2 <- ggplot(data = fits2, aes(x = country, y = S0, group = country, fill = long)) + geom_boxplot() + theme_classic() +
  facet_wrap(~ oev_base, ncol = 1, scales = 'free_y') + scale_fill_viridis() + labs(x = '', fill = 'Long.')
p3 <- ggplot(data = fits2[fits2$oev_base == 1e4, ], aes(x = country, y = S0, group = country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', fill = 'Long.')
p4 <- ggplot(data = fits2[fits2$oev_base == 1e5, ], aes(x = country, y = S0, group = country, fill = long)) + geom_boxplot() +
  theme_classic() + scale_fill_viridis() + labs(x = '', fill = 'Long.')

pdf('syntheticTests/outputs/explore/fitS0_by_country_indiv-and-network.pdf', width = 12, height = 8)
grid.arrange(p1, p3, ncol = 1)
grid.arrange(p1, p4, ncol = 1)
print(p2)
dev.off()


