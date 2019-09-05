
# Read in results:
o <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/results/outputOP_TEMPERATE_new.csv')

# List out countries and seasons:
countries <- levels(o$country)[-c(1, 3, 5:7, 11:12, 14, 16, 20, 22:25, 27:28, 30:31, 34, 36:37, 42:43, 45:46)]
seasons <- levels(o$season)

# Reduce to countries of interest:
o <- o[o$country %in% countries, ]
o$country <- factor(o$country)

# Reduce to final training week:
for (season in seasons) {
  for (country in countries) {
    o.temp <- o[o$country == country & o$season == season, ]
    if (length(o.temp$fc_start) > 0) {
      if (max(o.temp$fc_start) != 69) {
        print(c(season, country, max(o.temp$fc_start)))
      }
    }
  }
}

o <- o[o$fc_start == 69 | (o$season == '2010-11' & o$country == 'Iceland' & o$fc_start == 67) |
         (o$season == '2011-12' & o$country == 'Portugal' & o$fc_start == 67) |
         (o$season == '2012-13' & o$country == 'France' & o$fc_start == 68), ]

# Reduce to training:
o <- o[o$result == 'train', ]

# Reduce to necessary columns:
o <- o[, c(1:3, 11, 14:15, 18:25)]

# Create new data frame of S0 for each country/season:
counts = seas = s0s = c()
for (season in seasons) {
  for (country in countries) {
    o.temp <- o[o$country == country & o$season == season, ]
    
    if (length(o.temp$S) > 0) {
      counts <- c(counts, country)
      seas <- c(seas, season)
      s0s <- c(s0s, max(o.temp$S))
    }
  }
}
res <- as.data.frame(cbind(counts, seas, s0s))
res$s0s <- as.numeric(as.character(res$s0s))

# Look at distribution for each season:
par(mfrow = c(4, 2))
for (season in seasons) {
  hist(res$s0s[res$seas == season], breaks = 40, xlab = 'S0', main = season)
}
# mostly 60-75%; extend down to 55% and up to 90%; not normally distributed, but certainly a narrow range we could start with

# Look at distribution for each COUNTRY:
p1 <- ggplot(data = res) + geom_boxplot(aes(x = counts, y = s0s), fill = 'lightblue2') +
  theme_classic() + labs(x = 'Country', y = 'S0')
print(p1)
# okay, so there look to be some differences here
# note that we're not including 17-18 or 18-19 here

kruskal.test(s0s ~ counts, data = res) # sig
posthoc.kruskal.nemenyi.test(s0s ~ counts, data = res) # PT vs. (RO, IS, DE, DK, CZ)

# Look at fitted values of L, D, R0mx, R0mn at end of season:
o$group <- paste(o$country, o$run, sep = '_')
ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = country)) +
  facet_wrap(~ season) + theme_classic() + theme(legend.position = 'n') +
  scale_color_viridis(21, discrete = TRUE)
ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = country)) +
  facet_wrap(~ season) + theme_classic() + theme(legend.position = 'n') +
  scale_color_viridis(21, discrete = TRUE)
ggplot(data = o) + geom_line(aes(x = week, y = R0max, group = group, col = country)) +
  facet_wrap(~ season) + theme_classic() + theme(legend.position = 'n') +
  scale_color_viridis(21, discrete = TRUE)
ggplot(data = o) + geom_line(aes(x = week, y = R0min, group = group, col = country)) +
  facet_wrap(~ season) + theme_classic() + theme(legend.position = 'n') +
  scale_color_viridis(21, discrete = TRUE)

# or even just look late-season once they start flattening out
o <- o[o$week == 69 | (o$country == 'Iceland' & o$season == '2010-11' & o$week == 67) |
         (o$country == 'Portugal' & o$season == '2011-12' & o$week == 67) |
         (o$country == 'France' & o$season == '2012-13' & o$week == 68), ]
summary(o$L / 365) # 4.276 to 9.2
summary(o$D) # 2.37 to 7.89
summary(o$R0max) # 2.25 to 4.78
summary(o$R0min) # 0.89 to 1.28








