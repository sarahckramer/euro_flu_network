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

# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/travel_plots_04-16.pdf', width = 8, height = 7)

### NETWORK PLOTS ###
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/train_04-16.RData')
countries <- colnames(t.rand.sym)

l <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% c(countries, 'GB'), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[7] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)
l <- l[c(1:6, 8:15, 18:16, 7), ]

# move: France, Croatia, UK
l$Longitude[c(6, 18)] <- c(2, -2)
l$Latitude[c(6:7)] <- c(46.5, 45.7)

l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)

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

l <- cbind(countries, as.data.frame(l))

df <- t.rand.sym
df <- melt(df); names(df) <- c('source', 'dest', 'w')
df <- merge(df, l, by.x = 'source', by.y = 'countries')
df <- merge(df, l, by.x = 'dest', by.y = 'countries')
names(df)[4:7] <- c('x2', 'y2', 'x1', 'y1')
df <- df[df$w > 0, ]; df$w <- log(df$w)
df <- df[, c(2, 1, 6:7, 4:5, 3)]
df <- df[order(df$w), ]

leg.labs <- c(1, 10, 100, 1000, 10000, 100000)

p <- ggplot() + geom_polygon(data = eur, aes(x=long, y = lat, group = group),
                             colour = 'black', fill = 'gray90') + coord_fixed(1.4) + 
  theme_classic() + ditch_the_axes +
  geom_curve(data = df, aes(x = x1, y = y1, xend = x2, yend = y2, color = w, size = w + 5.4),
             curvature = 0.3,
             arrow = arrow(angle = 15, type = 'closed', length = unit(0.15, 'inches'),
                           ends = 'first'), inherit.aes = TRUE) +
  geom_point(data = l, aes(x = V1, y = V2), shape = 21, size = 4, colour = 'black',
             fill = 'white', stroke = 1.2) +
  scale_color_gradientn(colours = viridis(100), name = 'Daily Train Passengers',
                        breaks = log(leg.labs), labels = leg.labs, limits = c(1, 9.9)) +
  scale_size_continuous(range = c(0, 1.5), guide = FALSE)
print(p)

# adjacent only:
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/train_adj_04-16.RData')

df <- t.rand.adj.sym
df <- melt(df); names(df) <- c('source', 'dest', 'w')
df <- merge(df, l, by.x = 'source', by.y = 'countries')
df <- merge(df, l, by.x = 'dest', by.y = 'countries')
names(df)[4:7] <- c('x2', 'y2', 'x1', 'y1')
df <- df[df$w > 0, ]; df$w <- log(df$w)
df <- df[, c(2, 1, 6:7, 4:5, 3)]
df <- df[order(df$w), ]

leg.labs <- c(1, 10, 100, 1000, 10000, 100000)

p <- ggplot() + geom_polygon(data = eur, aes(x=long, y = lat, group = group),
                             colour = 'black', fill = 'gray90') + coord_fixed(1.4) + 
  theme_classic() + ditch_the_axes +
  geom_curve(data = df, aes(x = x1, y = y1, xend = x2, yend = y2, color = w, size = w + 5.4),
             curvature = 0.3,
             arrow = arrow(angle = 15, type = 'closed', length = unit(0.15, 'inches'),
                           ends = 'first'), inherit.aes = TRUE) +
  geom_point(data = l, aes(x = V1, y = V2), shape = 21, size = 4, colour = 'black',
             fill = 'white', stroke = 1.2) +
  scale_color_gradientn(colours = viridis(100), name = 'Daily Train Passengers',
                        breaks = log(leg.labs), labels = leg.labs, limits = c(1, 9.9)) +
  scale_size_continuous(range = c(0, 1.5), guide = FALSE)
print(p)

### HEAT MAPS ###
air.by.month <- vector('list', 12)
for (i in 1:12) {
  load(paste0('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/air_', i, '_04-16.RData'))
  air.by.month[[i]] <- a.temp.sym
}; rm(a.temp.sym)
a.mean <- apply(simplify2array(air.by.month), 1:2, mean); rm(air.by.month)

countries <- colnames(a.mean)

a.temp <- melt(a.mean); names(a.temp) <- c('source', 'dest', 'w')
a.temp$w[a.temp$w == 0] <- NA
a.temp$w <- log(a.temp$w)

a.temp$source <- factor(a.temp$source, levels(a.temp$source)[(c(19, 9, 8, 18, 12, 15, 3, 1, 17, 5, 4, 13, 2, 14, 10, 20, 7, 6, 16, 11))])
a.temp$dest <- factor(a.temp$dest, levels(a.temp$dest)[rev(c(19, 9, 8, 18, 12, 15, 3, 1, 17, 5, 4, 13, 2, 14, 10, 20, 7, 6, 16, 11))])

leg.labs <- c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
p <- ggplot(a.temp, aes(x = dest, y = source)) + geom_tile(aes(fill = w), colour = 'white') +
  scale_fill_gradientn(colours = viridis(10), na.value = 'gray80', limits = c(-1.5, 10.8), breaks = log(leg.labs), labels = leg.labs) +
  theme(axis.ticks = element_blank(), text = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Daily Airline Passengers', x = 'Destination', y = 'Source', fill = '') +
  geom_abline(intercept = 21, slope = -1, linetype = 2, col = 'white')
print(p)

# dev.off()





# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/commuting_network.pdf',
#     width = 14, height = 26)

# Load libraries:
library(reshape2); library(ggplot2); library(maps); library(viridis); library(gridExtra)

# Read in data:
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/matrices/comm_mat_by_year_ADJ_04-16.RData')
countries <- colnames(comm.by.year.adj[[1]])

# Set up latitude and longitude values:
l <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/raw_data/country_centroids_az8.csv')
l <- l[, c(47, 67:68)]
l <- l[l$iso_a2 %in% c(countries, 'GB'), ]
l$iso_a2 <- factor(l$iso_a2); levels(l$iso_a2)[8] <- 'UK'; l$iso_a2 <- factor(l$iso_a2)
l <- l[c(1:7, 9:17, 20:18, 8), ]

# move: France, Croatia, UK
l$Longitude[c(7, 20)] <- c(2, -2)
l$Latitude[c(7:8)] <- c(46.5, 45.7)

l <- l[, 2:3]
colnames(l) <- NULL
l <- as.matrix(l)
l <- cbind(countries, as.data.frame(l))
l <- l[l$countries != 'IS', ]

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
comm.mean <- apply(simplify2array(comm.by.year.adj), 1:2, mean); rm(comm.by.year.adj)
df <- melt(comm.mean); names(df) <- c('source', 'dest', 'w')
df <- df[df$source != 'IS' & df$dest != 'IS', ]
df <- merge(df, l, by.x = 'source', by.y = 'countries')
df <- merge(df, l, by.x = 'dest', by.y = 'countries')
names(df)[4:7] <- c('x2', 'y2', 'x1', 'y1')
df <- df[df$w > 0, ]; df$w <- log(df$w)
df <- df[, c(2, 1, 6:7, 4:5, 3)]
df <- df[order(df$w), ]

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
  scale_color_gradientn(colours = viridis(100), name = '# of Commuters',
                        breaks = log(leg.labs), labels = leg.labs, limits = c(6.5, 11.7)) +
  scale_size_continuous(range = c(0, 1.5), guide = FALSE) +
  labs(title = '')
print(p)

# pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/commuters_avg_04-16.pdf', width = 8, height = 7)
# print(p)
# dev.off()

# Plot net inflow and outflow?














# dev.off()

