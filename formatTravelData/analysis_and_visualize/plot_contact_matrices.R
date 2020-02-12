# http://kateto.net/network-visualization

# Read in libraries
library(igraph)
library(network)
library(sna)
library(ndtv)
library(viridis)
library(ggmap)
library(maps)
library(geosphere)
library(reshape2)
library(gridExtra)
library(dplyr)
library(RColorBrewer)

# pdf('formatTravelData/outputs/travel_plots_09-24.pdf', width = 8, height = 7)
pdf('syntheticTests/outputs/explore/commuting_network_w-oLowReliability.pdf', width = 8, height = 7)

### HEAT MAPS ###
air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_01-31.RData'))
  air.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym)
a.mean <- apply(simplify2array(air.by.month), 1:2, mean);# rm(air.by.month)
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
a.mean <- a.mean[countries, countries]

countries <- colnames(a.mean)

a.temp <- melt(a.mean); names(a.temp) <- c('source', 'dest', 'w')
a.temp$w[a.temp$w == 0] <- NA
a.temp$w <- log(a.temp$w)

# a.temp$source <- factor(a.temp$source, levels(a.temp$source)[(c(17, 20, 9, 8, 19, 12, 15, 3, 1, 18, 5, 4, 13, 2, 14, 10, 21, 7, 6, 16, 11))])
# a.temp$dest <- factor(a.temp$dest, levels(a.temp$dest)[rev(c(17, 20, 9, 8, 19, 12, 15, 3, 1, 18, 5, 4, 13, 2, 14, 10, 21, 7, 6, 16, 11))])

a.temp$source <- factor(a.temp$source, levels(a.temp$source)[rev(c(12, 4, 9, 2, 8, 5, 1, 3, 10, 7, 6, 11))])
a.temp$dest <- factor(a.temp$dest, levels(a.temp$dest)[c(12, 4, 9, 2, 8, 5, 1, 3, 10, 7, 6, 11)])

# pdf('formatTravelData/outputs/air_by_month.pdf', width = 8, height = 7)
leg.labs <- c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
p <- ggplot(a.temp, aes(x = dest, y = source)) + geom_tile(aes(fill = w), colour = 'white') +
  scale_fill_gradientn(colours = viridis(10), na.value = 'gray80', limits = c(-1.5, 10.8), breaks = log(leg.labs), labels = leg.labs) +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Daily Airline Passengers', x = 'Destination', y = 'Source', fill = '') +
  geom_abline(intercept = 13, slope = -1, linetype = 2, col = 'white')
print(p)

# by month:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
for (i in 1:12) {
  a.temp <- air.by.month[[i]]
  a.temp <- a.temp[countries, countries]
  a.temp <- melt(a.temp); names(a.temp) <- c('source', 'dest', 'w')
  a.temp$w[a.temp$w == 0] <- NA
  a.temp$w <- log(a.temp$w)
  
  # print(min(a.temp$w, na.rm = TRUE))
  
  a.temp$source <- factor(a.temp$source, levels(a.temp$source)[rev(c(12, 4, 9, 2, 8, 5, 1, 3, 10, 7, 6, 11))])
  a.temp$dest <- factor(a.temp$dest, levels(a.temp$dest)[c(12, 4, 9, 2, 8, 5, 1, 3, 10, 7, 6, 11)])
  
  leg.labs <- c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
  p <- ggplot(a.temp, aes(x = dest, y = source)) + geom_tile(aes(fill = w), colour = 'white') +
    scale_fill_gradientn(colours = viridis(10), na.value = 'gray80', limits = c(-3.5, 10.8), breaks = log(leg.labs), labels = leg.labs) +
    theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    labs(title = paste0('Daily Airline Passengers (', c('Jan.', 'Feb.', 'Mar.', 'Apr.', 'May', 'June', 'July', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.')[i], ')'),
         x = 'Destination', y = 'Source', fill = '') +
    geom_abline(intercept = 13, slope = -1, linetype = 2, col = 'white')
  print(p)
  
}
dev.off()

### NETWORKS ###
# Read in data:
# load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
# 
# load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
# load('formatTravelData/formattedData/comm_mat_by_season_05-07_RELIABLE_ONLY.RData')
# comm.by.year <- comm.by.year2; rm(comm.by.year2)
load('formatTravelData/formattedData/comm_mat_by_season_01-27.RData')

# countries <- colnames(comm.by.year[[1]])
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# Set up latitude and longitude values:
l <- read.csv('../travel_data_info/flight_data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
# l <- l[l$iso_a2 %in% c(countries, 'GB'), ]
l <- l[l$iso_a2 %in% countries, ]
l$iso_a2 <- factor(l$iso_a2);# levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)
# l <- l[c(1:7, 9:18, 21:19, 8), ]

# move: France, Croatia, UK
# l$Longitude[c(7, 21)] <- c(2, -2)
# l$Latitude[c(7:8)] <- c(46.5, 45.7)
l$Longitude[6] <- 2
l$Latitude[6] <- 46.5
l <- l[c(1:3, 6, 4, 7:12, 5), ]

l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)
l <- cbind(countries, as.data.frame(l))
# l <- l[l$countries != 'IS', ]

# Set up base map:
countries.europe <- c('Austria', 'Belarus', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic',
                      'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece',
                      'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                      'Luxembourg', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova',
                      'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden',
                      'Switzerland', 'Ukraine', 'UK', 'Albania', 'Bosnia and Herzegovina',
                      'Montenegro', 'Kosovo', 'Macedonia')
world <- map_data('world')
eur <- world[world$region %in% countries.europe,]
rm(world)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Plot average matrix:
comm.by.year <- comm.by.seas
comm.mean <- apply(simplify2array(comm.by.year), 1:2, mean, na.rm = TRUE); rm(comm.by.year)
df <- melt(comm.mean); names(df) <- c('source', 'dest', 'w')
df <- df[df$source != 'IS' & df$dest != 'IS', ]
df <- merge(df, l, by.x = 'source', by.y = 'countries')
df <- merge(df, l, by.x = 'dest', by.y = 'countries')
names(df)[4:7] <- c('x2', 'y2', 'x1', 'y1')
df <- df[df$w > 0, ]; df$w <- log(df$w)
df <- df[, c(2, 1, 6:7, 4:5, 3)]
df <- df[order(df$w), ]

pdf('formatTravelData/outputs/commuting_by_season.pdf', width = 8, height = 7)
leg.labs <- c(100, 1000, 10000, 75000, 100000)
p <- ggplot() + geom_polygon(data = eur, aes(x=long, y = lat, group = group),
                             colour = 'black', fill = 'gray90') + coord_fixed(1.4) + 
  theme_classic() + ditch_the_axes + theme(plot.title = element_text(size = 24),
                                           legend.title = element_text(size = 12),
                                           legend.text = element_text(size = 12),
                                           legend.key.height = unit(0.75, 'cm'),
                                           legend.key.width = unit(0.75, 'cm')) +
  geom_curve(data = df, aes(x = x1, y = y1, xend = x2, yend = y2, color = w, size = w + 5.4),
             curvature = 0.3,
             arrow = arrow(angle = 15, type = 'closed', length = unit(0.15, 'inches'),
                           ends = 'first'), inherit.aes = TRUE) +
  geom_point(data = l, aes(x = V1, y = V2), shape = 21, size = 4, colour = 'black',
             fill = 'white', stroke = 1.2) +
  scale_color_gradientn(colours = viridis(100), name = '# of Commuters\n',
                        breaks = log(leg.labs), labels = leg.labs, limits = c(6.9, 11.32)) +
  scale_size_continuous(range = c(0, 1.5), guide = FALSE) +
  labs(title = '')
print(p)

# By season:
for (i in 1:8) {
  comm.temp <- comm.by.seas[[i]]
  
  df <- melt(comm.temp); names(df) <- c('source', 'dest', 'w')
  df <- merge(df, l, by.x = 'source', by.y = 'countries')
  df <- merge(df, l, by.x = 'dest', by.y = 'countries')
  names(df)[4:7] <- c('x2', 'y2', 'x1', 'y1')
  df <- df[df$w > 0, ]; df$w <- log(df$w)
  df <- df[, c(2, 1, 6:7, 4:5, 3)]
  df <- df[order(df$w), ]
  
  # print(summary(df$w))
  
  leg.labs <- c(100, 1000, 10000, 100000)
  p <- ggplot() + geom_polygon(data = eur, aes(x=long, y = lat, group = group),
                               colour = 'black', fill = 'gray90') + coord_fixed(1.4) + 
    theme_classic() + ditch_the_axes + theme(plot.title = element_text(size = 24),
                                             legend.title = element_text(size = 12),
                                             legend.text = element_text(size = 12),
                                             legend.key.height = unit(0.75, 'cm'),
                                             legend.key.width = unit(0.75, 'cm')) +
    geom_curve(data = df, aes(x = x1, y = y1, xend = x2, yend = y2, color = w, size = w + 5.4),
               curvature = 0.3,
               arrow = arrow(angle = 15, type = 'closed', length = unit(0.15, 'inches'),
                             ends = 'first'), inherit.aes = TRUE) +
    geom_point(data = l, aes(x = V1, y = V2), shape = 21, size = 4, colour = 'black',
               fill = 'white', stroke = 1.2) +
    scale_color_gradientn(colours = viridis(100), name = '# of Commuters\n',
                          breaks = log(leg.labs), labels = leg.labs, limits = c(6.9, 11.66)) +
    scale_size_continuous(range = c(0, 1.5), guide = FALSE) +
    labs(title = c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')[i])
  print(p)
}
# dev.off()

# Assess change over time:
comm.df <- NULL
for (i in 1:8) {
  a.temp <- melt(comm.by.seas[[i]] / comm.by.seas[[1]])
  names(a.temp)[1:2] <- c('source', 'dest')
  a.temp$season <- i
  comm.df <- rbind(comm.df, a.temp)
}
comm.df <- as.data.frame(comm.df)
comm.df$route <- paste(comm.df$source, comm.df$dest, sep = '_')
comm.df$route <- factor(comm.df$route)

comm.df <- comm.df[!is.na(comm.df$value), ]

ggplot(data = comm.df, aes(x = season, y = log(value))) + geom_line() + facet_wrap(~ route)

# Assess asymmetry:
asym.vals <- c()
for (i in 1:12) {
  for (j in i:12) {
    if (i != j) {
      
      if (!is.na(comm.mean[i, j]) & !is.na(comm.mean[j, i])) {
        print(row.names(comm.mean)[c(i, j)])
        print(comm.mean[i, j] / comm.mean[j, i])
        print('')
        asym.vals <- c(asym.vals, comm.mean[i, j] / comm.mean[j, i])
      }
      
    }
  }
}

# which routes are bidirectional? AT-DE, BE-FR, BE-LU, BE-NL, CZ-FR, CZ-PL, CZ-SK, FR-DE, FR-IT, FR-LU, FR-NL, FR-ES, DE-LU, DE-NL, DE-PL, HU-SK, IT-ES (17 routes)
# greatest differences: BE-LU (41.102); CZ-PL (0.1022 (9.785x)), CZ-SK (0.0593 (16.863x)), FR-LU (79.7), DE-LU (27.807), DE-PL (0.070 (14.286x))

asym.vals[asym.vals < 1] <- 1 / asym.vals[asym.vals < 1]
summary(asym.vals)

# Plot net inflow and outflow?
in.flow <- colSums(comm.mean, na.rm = TRUE)
out.flow <- rowSums(comm.mean, na.rm = TRUE)
net.influx <- in.flow - out.flow

# names(net.influx) <- c('Austria', 'Belgium', 'Czech Republic', 'Germany', 'Denmark',
#                        'Spain', 'France', 'Croatia', 'Hungary', 'Ireland',
#                        'Iceland', 'Italy', 'Luxembourg', 'Netherlands', 'Poland',
#                        'Portugal', 'Romania', 'Sweden', 'Slovenia', 'Slovakia', 'UK')
names(net.influx) <- c('Austria', 'Belgium', 'Czech Republic', 'France', 'Germany', 'Hungary',
                       'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia', 'Spain')
net.influx <- as.data.frame(net.influx)
net.influx <- cbind(rownames(net.influx), net.influx)
names(net.influx) <- c('region', 'val')

eur <- full_join(eur, net.influx, by = 'region')

p <- ggplot() + geom_polygon(data = eur, aes(x=long, y = lat, group = group, fill = val),
                             colour = 'black') + coord_fixed(1.4) + 
  theme_classic() + ditch_the_axes + theme(plot.title = element_text(size = 24),
                                           legend.title = element_text(size = 12),
                                           legend.text = element_text(size = 12),
                                           legend.key.height = unit(0.75, 'cm'),
                                           legend.key.width = unit(0.75, 'cm')) +
  scale_fill_distiller(palette = 'PiYG', na.value = 'gray90', name = 'Net Commuter Inflow\n(in thousands)\n',
                       breaks = c(-100000, -50000, 0, 50000, 100000, 150000), 
                       labels = c('-100', '-50', '0', '50', '100', '150')) +
  labs(title = '')
print(p)

dev.off()

