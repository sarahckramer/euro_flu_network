
# Read in results
a <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputOP_TEMPERATE.csv')
c <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputOP_trop_nohumid_CONT.csv')

# Create separate north and south data frames
a.north <- a[!(a$country %in% c('Australia', 'New Zealand', 'Chile')), ]; a.north$country <- factor(a.north$country)
a.south <- a[a$country %in% c('Australia', 'New Zealand', 'Chile'), ]; a.south$country <- factor(a.south$country)

# Keep only final fits
a.north.new <- c()
for (country in levels(a.north$country)) {
  for (season in levels(a.north$season)) {
    a.temp <- a.north[a.north$country == country & a.north$season == season, ]
    if (length(a.temp$country) > 0) {
      a.north.new <- rbind(a.north.new, a.temp[a.temp$fc_start == max(a.temp$fc_start), ])
    }
  }
}

a.south.new <- c()
for (country in levels(a.south$country)) {
  for (season in levels(a.south$season)) {
    a.temp <- a.south[a.south$country == country & a.south$season == season, ]
    if (length(a.temp$country) > 0) {
      a.south.new <- rbind(a.south.new, a.temp[a.temp$fc_start == max(a.temp$fc_start), ])
    }
  }
}

# c.new <- c()
# for (country in levels(c$country)) {
#   a.temp <- c[c$country == country, ]
#   c.new <- rbind(c.new, a.temp[a.temp$fc_start == max(a.temp$fc_start), ])
# }

rm(a, a.north, a.south, a.temp)#, c)
rm(country, season)

# Limit to "training," and not forecasts
a.north.new <- a.north.new[a.north.new$result == 'train', ]
a.south.new <- a.south.new[a.south.new$result == 'train', ]
c.new <- c[c$result == 'train', ]

# Correct week numbers for southern hemisphere
a.south.new$week <- a.south.new$week - 26

# Plot state and parameter fits for each country/season

### Only for: Obs. lead -4:0
met.a <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_FIN.csv')
met.c <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_trop_nohumid_CONT_FIN_seasons.csv')

met.a <- unique(met.a[, c('country', 'season', 'obs_pkwk')])
met.c <- unique(met.c[, c('country', 'season', 'obs_pkwk')])

a.north.new <- merge(a.north.new, met.a, by = c('country', 'season'))
a.south.new <- merge(a.south.new, met.a, by = c('country', 'season'))
c.new <- merge(c.new, met.c, by = 'country')

a.north.new$FWeek_pkwk <- a.north.new$week - a.north.new$obs_pkwk
a.south.new$FWeek_pkwk <- a.south.new$week - a.south.new$obs_pkwk
c.new$FWeek_pkwk <- c.new$fc_start - c.new$obs_pkwk

a.north.new <- a.north.new[a.north.new$FWeek_pkwk > -5 & a.north.new$FWeek_pkwk <= 0,]
a.south.new <- a.south.new[a.south.new$FWeek_pkwk > -5 & a.south.new$FWeek_pkwk <= 0,]
c.new <- c.new[c.new$FWeek_pkwk > -5 & c.new$FWeek_pkwk <= 0 & !is.na(c.new$FWeek_pkwk),]




# p1 <- ggplot(a.south.new, aes(colour = country)) + geom_boxplot(aes(factor(FWeek_pkwk), R0max)) +
#   labs(x = 'Observed Lead Week') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country, ncol = 1)
# p1




# or: put all countries in same plot, but only for certain time points;
# or: separate boxes for each season
a.new <- rbind(a.north.new, a.south.new)
a.new <- a.new[a.new$FWeek_pkwk %in% c(-4, -2, 0), ]

# p1 <- ggplot(a.new, aes(colour = country)) + geom_boxplot(aes(country, R0max)) +
#   labs(x = 'Country', y = 'R0Max') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ FWeek_pkwk, ncol = 1)
# p1

# a.new.peak <- a.new[a.new$FWeek_pkwk == 0,]
# a.new.four <- a.new[a.new$FWeek_pkwk == -4,]
# 
# p1 <- ggplot(a.new.peak, aes(colour = country)) + geom_boxplot(aes(factor(season), R0max)) +
#   labs(x = 'Season', y = 'R0Max (Lead 0)') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country)
# p1
# p2 <- ggplot(a.new.peak, aes(colour = country)) + geom_boxplot(aes(factor(season), R0min)) +
#   labs(x = 'Season', y = 'R0Min (Lead 0)') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country)
# p2
# 
# p15 <- ggplot(a.new.peak, aes(colour = country)) + geom_boxplot(aes(factor(season), R0max)) +
#   labs(x = 'Season', y = 'R0Max (Lead 0)') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country)
# p15
# p25 <- ggplot(a.new.peak, aes(colour = country)) + geom_boxplot(aes(factor(season), R0min)) +
#   labs(x = 'Season', y = 'R0Max (Lead 0)') + theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country)
# p25


# Try: A row for each variable, with a single box in each for each country
a.new <- a.new[, c('country', 'S', 'L', 'D', 'R0max', 'R0min', 'FWeek_pkwk')]
a.new.0 <- a.new[a.new$FWeek_pkwk == 0, ]; a.new.0$FWeek_pkwk <- NULL
a.new.4 <- a.new[a.new$FWeek_pkwk == -4, ]; a.new.4$FWeek_pkwk <- NULL
a.new.0 <- melt(a.new.0, id.vars = 'country')
a.new.4 <- melt(a.new.4, id.vars = 'country')

c.new <- c.new[, c('country', 'S', 'L', 'D', 'R0', 'FWeek_pkwk')]
c.new <- c.new[c.new$S != 0,]
c.new.0 <- c.new[c.new$FWeek_pkwk == 0, ]; c.new.0$FWeek_pkwk <- NULL
c.new.4 <- c.new[c.new$FWeek_pkwk == -4, ]; c.new.4$FWeek_pkwk <- NULL
c.new.0 <- melt(c.new.0, id.vars = 'country')
c.new.4 <- melt(c.new.4, id.vars = 'country')

p1 <- ggplot(a.new.0, aes(colour = country)) + geom_boxplot(aes(country, value)) +
  labs(x = '', y = 'Parameter/State Value', title = 'Obs. Lead Week = 0') + theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ variable, ncol = 1, scales = 'free_y')
p1
p2 <- ggplot(a.new.4, aes(colour = country)) + geom_boxplot(aes(country, value)) +
  labs(x = '', y = 'Parameter/State Value', title = 'Obs. Lead Week = -4') + theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ variable, ncol = 1, scales = 'free_y')
p2

p3 <- ggplot(c.new.0, aes(colour = country)) + geom_boxplot(aes(country, value)) +
  labs(x = '', y = 'Parameter/State Value', title = 'Obs. Lead Week = 0') + theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ variable, ncol = 1, scales = 'free_y')
p3
p4 <- ggplot(c.new.4, aes(colour = country)) + geom_boxplot(aes(country, value)) +
  labs(x = '', y = 'Parameter/State Value', title = 'Obs. Lead Week = -4') + theme_bw() +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ variable, ncol = 1, scales = 'free_y')
p4



# ### Second pass:
# a.south.new$group <- paste(a.south.new$run, a.south.new$season, sep='_'); a.south.new$group <- factor(a.south.new$group)
# p1 <- ggplot(a.south.new, aes(colour = country)) + geom_boxplot(aes(factor(week), R0max)) +
#   facet_wrap(~ country, ncol = 1)
# p1
# 




# ### First pass:
# # Northern Hemisphere:
# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/plots/temp_north_params.pdf', height = 63, width = 14)
# p1 <- ggplot(a.north.new, aes(x = week, y = R0max, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'R0Max') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p1
# p2 <- ggplot(a.north.new, aes(x = week, y = R0min, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'R0Min') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p2
# p3 <- ggplot(a.north.new, aes(x = week, y = D, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'D') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p3
# p4 <- ggplot(a.north.new, aes(x = week, y = L, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'L') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p4
# p5 <- ggplot(a.north.new, aes(x = week, y = S, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'S') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p5
# dev.off()
# 
# # Southern Hemisphere:
# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/plots/temp_south_params.pdf')
# p1 <- ggplot(a.south.new, aes(x = week, y = R0max, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'R0Max') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p1
# p2 <- ggplot(a.south.new, aes(x = week, y = R0min, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'R0Min') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p2
# p3 <- ggplot(a.south.new, aes(x = week, y = D, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'D') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p3
# p4 <- ggplot(a.south.new, aes(x = week, y = L, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'L') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p4
# p5 <- ggplot(a.south.new, aes(x = week, y = S, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'S') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_grid(country ~ season)
# p5
# dev.off()
# 
# # Tropics:
# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecasts/plots/trop_params.pdf', height = 35)
# p1 <- ggplot(c.new, aes(x = time, y = R0, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'R0') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country, ncol = 1)
# p1
# p3 <- ggplot(c.new, aes(x = time, y = D, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'D') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country, ncol = 1)
# p3
# p4 <- ggplot(c.new, aes(x = time, y = L, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'L') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country, ncol = 1)
# p4
# p5 <- ggplot(c.new, aes(x = time, y = S, group = run, color = country)) +
#   geom_line() + geom_point(size = 0.5) + labs(x = 'Week', y = 'S') +
#   theme_bw() + theme(legend.position = 'none') +
#   facet_wrap(~ country, ncol = 1)
# p5
# dev.off()














