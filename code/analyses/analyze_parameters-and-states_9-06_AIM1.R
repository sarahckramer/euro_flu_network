library(ggplot2); library(gridExtra)

# Read in results
a <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputOP_TEMPERATE_new.csv')
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

rm(a, a.north, a.south, a.temp)#, c)
rm(country, season)

# Limit to "training," and not forecasts
a.north.new <- a.north.new[a.north.new$result == 'train', ]
a.south.new <- a.south.new[a.south.new$result == 'train', ]
c.new <- c[c$result == 'train', ]

# Remove Mexico 10-11 from temperate results
a.north.new <- a.north.new[!(a.north.new$country == 'Mexico' & a.north.new$season == '2010-11'),]

# Remove Ukraine 11-12 ("peak" before "onset")
a.north.new <- a.north.new[!(a.north.new$country == 'Ukraine' & a.north.new$season == '2011-12'),]

# Get tropical "seasons"
# We don't limit these to "final fits" b/c all done step-by-step - not run several times w/
# different fc_start like with temperate
mc <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_trop_nohumid_CONT_FIN_seasons.csv')
source('/Users/sarahkramer/Dropbox/spatial_model/forecasts/code/functions/remove_seasons_tropical_FUNCTION.R')
ili <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/WHO_dat_ALL_05-12_NOPAN.csv')
mc <- rem_season_trop(mc, ili)
rm(ili)

mc <- mc[, c(1:2, 7, 79)]

c.new$fc_start[c.new$fc_start == 1] <- 3
c.new <- merge(c.new, mc, by = c('country', 'run', 'fc_start'), sort = FALSE)
c.new <- c.new[!is.na(c.new$season), ]

# Correct week numbers for southern hemisphere
a.south.new$week <- a.south.new$week - 26

# Cut down to columns of interest
a.north.new <- a.north.new[, c(1:3, 8:9, 11, 14:15, 18:25)]
a.south.new <- a.south.new[, c(1:3, 8:9, 11, 14:15, 18:25)]
c.new <- c.new[, c(1:3, 9, 12:13, 16:21, 24)]

# Calculate R0 for temperate
ah <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/ah_4-17_formatted.csv')
ah <- rbind(ah, ah)
colnames(ah)[c(42, 50, 52, 63:64)] <- c('New Zealand', 'Republic of Moldova',
                                        'Russian Federation', 'United Kingdom',
                                        'United States of America')

a.north.new$R0 = a.south.new$R0 = NA
for (country in levels(a.north.new$country)) {
  a.north.new$R0[a.north.new$country == country] <- a.north.new$R0min[a.north.new$country == country] +
    (a.north.new$R0max[a.north.new$country == country] - a.north.new$R0min[a.north.new$country == country]) *
    exp(-180 * ah[a.north.new$time[a.north.new$country == country], names(ah) == country])
}
for (country in levels(a.south.new$country)) {
  a.south.new$R0[a.south.new$country == country] <- a.south.new$R0min[a.south.new$country == country] +
    (a.south.new$R0max[a.south.new$country == country] - a.south.new$R0min[a.south.new$country == country]) *
    exp(-180 * ah[a.south.new$time[a.south.new$country == country], names(ah) == country])
}

# Calculate Re
a.north.new$Re <- a.north.new$R0 * a.north.new$S / 100000
a.south.new$Re <- a.south.new$R0 * a.south.new$S / 100000
c.new$Re <- c.new$R0 / c.new$S * 100000

# Limit data frames to: last estimate for each outbreak, and "maxes"
# temperate:
a.new <- rbind(a.north.new, a.south.new)
a.new$country <- factor(a.new$country)
# week.num.max = week.num.Smax = week.num.last = ss = cs = rs = c()
ss = cs = rs = S.max = L.max = L.last = D.max = D.last = R0.max = R0.last = Re.max = c()

for (season in levels(a.new$season)) {
  for (run in 1:5) {
    for (country in levels(a.new$country)) {
      o.temp <- a.new[a.new$season == season & a.new$country == country & a.new$run == run, ]
      
      if (length(o.temp$country) > 0) {
        ss <- c(ss, season); cs <- c(cs, country); rs <- c(rs, run)
        # week.num.max <- c(week.num.max, o.temp$week[o.temp$Re == max(o.temp$Re)])
        # week.num.Smax <- c(week.num.Smax, o.temp$week[o.temp$S == max(o.temp$S)])
        # week.num.last <- c(week.num.last, max(o.temp$week))
        
        S.max <- c(S.max, max(o.temp$S))
        Re.max <- c(Re.max, max(o.temp$Re))
        L.max <- c(L.max, o.temp$L[o.temp$Re == max(o.temp$Re)])
        D.max <- c(D.max, o.temp$D[o.temp$Re == max(o.temp$Re)])
        R0.max <- c(R0.max, o.temp$R0[o.temp$Re == max(o.temp$Re)])
        
        L.last <- c(L.last, o.temp$L[o.temp$week == max(o.temp$week)])
        D.last <- c(D.last, o.temp$D[o.temp$week == max(o.temp$week)])
        R0.last <- c(R0.last, o.temp$R0[o.temp$week == max(o.temp$week)])
      }
      
    }
  }
}

# a.df.max <- as.data.frame(cbind(cs, ss, rs, S.max, Re.max, R0.max, D.max, L.max))
# a.df.last <- as.data.frame(cbind(cs, ss, rs, R0.last, D.last, L.last))
# actually don't need S/Re at end, since of course these will be different than at max

a.df <- as.data.frame(cbind(cs, ss, rs, S.max, Re.max, R0.max, R0.last,
                            D.max, D.last, L.max, L.last))
# write.csv(a.df, file = '/Users/sarahkramer/Dropbox/spatial_model/forecasts/code/REVISION/temperate_params.csv', row.names = FALSE)

# a.df.max <- melt(a.df.max, id.vars = c('cs', 'ss', 'rs'))
# a.df.last <- melt(a.df.last, id.vars = c('cs', 'ss', 'rs'))
a.df <- melt(a.df, id.vars = c('cs', 'ss', 'rs'))
colnames(a.df) <- c('country', 'season', 'run', 'param', 'value')
a.df$value <- as.numeric(as.character(a.df$value))

# tropics:
c.new$season <- factor(c.new$season)
ss = cs = rs = S.max = L.max = L.last = D.max = D.last = R0.max = R0.last = Re.max = c()

for (season in levels(c.new$season)) {
  for (run in 1:5) {
    for (country in levels(c.new$country)) {
      o.temp <- c.new[c.new$season == season & c.new$country == country & c.new$run == run, ]
      
      if (length(o.temp$country) > 0) {
        ss <- c(ss, season); cs <- c(cs, country); rs <- c(rs, run)
        
        S.max <- c(S.max, max(o.temp$S))
        Re.max <- c(Re.max, max(o.temp$Re))
        L.max <- c(L.max, o.temp$L[o.temp$Re == max(o.temp$Re)])
        D.max <- c(D.max, o.temp$D[o.temp$Re == max(o.temp$Re)])
        R0.max <- c(R0.max, o.temp$R0[o.temp$Re == max(o.temp$Re)])
        
        L.last <- c(L.last, o.temp$L[o.temp$fc_start == max(o.temp$fc_start)])
        D.last <- c(D.last, o.temp$D[o.temp$fc_start == max(o.temp$fc_start)])
        R0.last <- c(R0.last, o.temp$R0[o.temp$fc_start == max(o.temp$fc_start)])
      }
      
    }
  }
}

c.df <- as.data.frame(cbind(cs, ss, rs, S.max, Re.max, R0.max, R0.last,
                            D.max, D.last, L.max, L.last))
# write.csv(c.df, file = '/Users/sarahkramer/Dropbox/spatial_model/forecasts/code/REVISION/tropics_params.csv', row.names = FALSE)

c.df <- melt(c.df, id.vars = c('cs', 'ss', 'rs'))
colnames(c.df) <- c('country', 'season', 'run', 'param', 'value')
c.df$value <- as.numeric(as.character(c.df$value))

# Should we plot these somehow, or just report them in a table?
# At least plot temp. vs. tropics, northern vs. southern
# Maybe also by data type, and by region

# p1 <- ggplot(data = a.df) + geom_boxplot(aes(x = country, y = value), fill = 'gray20') +
#   theme_classic() +
#   facet_grid(param ~ season, scales = 'free')
# p1

ma <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_new_FIN.csv')
mc <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_trop_nohumid_CONT_FIN_seasons.csv')

# ma <- ma[!is.na(ma$onsetObs5), ]
ma <- ma[!is.na(ma$onsetObs5) & !is.na(ma$onset5), ]
mc <- mc[!is.na(mc$onset) & !is.na(mc$onsetObs), ]

ma <- unique(ma[, c(1:2, 73:74, 76:77)])
mc <- unique(mc[, c(1, 72:73)])

a.df$lat <- 'temp'; c.df$lat <- 'trop'
a.df <- merge(a.df, ma, by = c('country', 'season'))
c.df <- merge(c.df, mc, by = 'country')
c.df$region3 <- NA; c.df$hemisphere <- NA

param.df <- rbind(a.df, c.df)
param.df$country <- factor(param.df$country)
param.df$data.type <- factor(param.df$data.type, levels = levels(param.df$data.type)[c(2, 1, 4, 3)])
param.df <- param.df[param.df$param %in% c('S.max', 'Re.max', 'R0.max', 'D.max', 'L.max'), ]
param.df$param <- factor(param.df$param)
levels(param.df$param) <- c('S', 'Re', 'R0', 'D', 'L')
param.df$scaling.range <- factor(param.df$scaling.range, levels = levels(param.df$scaling.range)[c(1:3, 6, 4, 7:8, 5, 9)])

# write.csv(param.df, file = '/Users/sarahkramer/Dropbox/spatial_model/forecasts/code/REVISION/params.csv', row.names = FALSE)

dat.text <- data.frame(label = 'A', x = 0.6, y = 87000, param = 'S')
p1 <- ggplot(data = param.df) + geom_boxplot(aes(x = lat, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = 'Value') +
  scale_x_discrete(labels = c('Temperate', 'Tropics')) +
  geom_text(data = dat.text, mapping = aes(x = x, y = y, label = label), size = 8)
# p2 <- ggplot(data = param.df[!is.na(param.df$region3), ]) +
#   geom_boxplot(aes(x = region3, y = value), fill = 'gray90') +
#   theme_classic() + theme(text = element_text(size = 18)) +
#   facet_wrap(~ param, ncol = 2, scales = 'free_y') +
#   labs(x = '', y = 'Value') +
#   scale_x_discrete(labels = c('EEur', 'NEur', 'NHem', 'SHem', 'SWEur'))
dat.text <- data.frame(label = 'B', x = 0.6, y = 87000, param = 'S')
p3 <- ggplot(data = param.df[!is.na(param.df$hemisphere), ]) +
  geom_boxplot(aes(x = hemisphere, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = '') +
  scale_x_discrete(labels = c('North', 'South')) +
  geom_text(data = dat.text, mapping = aes(x = x, y = y, label = label), size = 8)
p4 <- ggplot(data = param.df) + geom_boxplot(aes(x = data.type, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = '')
dat.text <- data.frame(label = 'C', x = 0.6, y = 87000, param = 'S')
p5 <- ggplot(data = param.df[param.df$lat == 'temp' & !is.na(param.df$lat), ]) +
  geom_boxplot(aes(x = data.type, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = '') +
  geom_text(data = dat.text, mapping = aes(x = x, y = y, label = label), size = 8)
dat.text <- data.frame(label = 'D', x = 4.25, y = 63200, param = 'S')
p6 <- ggplot(data = param.df[param.df$lat == 'trop', ]) +
  geom_boxplot(aes(x = data.type, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = '') + #scale_y_continuous(limits = c(50000, 70000))
  geom_text(data = dat.text, mapping = aes(x = x, y = y, label = label), size = 8) +
  scale_x_discrete(labels = c('ILI', 'ARI', 'SARI', 'Pneu'))

# dat.text <- data.frame(label = c('E', '', '', '', ''), x = c(0.5, 1, 1, 1, 1), y = c(40000, 3, 3, 3, 2000), param = c('S', 'Re', 'R0', 'D', 'L'))
dat.text <- data.frame(label = 'E', x = 0.6, y = 85000, param = 'S')
p7 <- ggplot(data = param.df) + geom_boxplot(aes(x = scaling.range, y = value), fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18)) +
  facet_wrap(~ param, ncol = 1, scales = 'free_y') +
  labs(x = '', y = 'Value') +
  geom_text(data = dat.text, mapping = aes(x = x, y = y, label = label), size = 8)
  # annotate('text', label = 'E', x = 0.5, y = 25000, size = 8)

# print(p1); print(p3); print(p4)
grid.arrange(p1, p3, p5, p6, nrow = 1)
print(p7)

# Differences by temp. vs. tropics? By hemisphere?
param.df$lat <- factor(param.df$lat)
param.df$group <- paste(param.df$country, param.df$season, sep = '_')
param.df$group <- factor(param.df$group)

# should do something like for main analysis, choosing a single run for each country/outbreak
dat.S <- param.df[param.df$param == 'S', ]
dat.S$param <- NULL
dat.S <- dat.S[order(dat.S$group, dat.S$run), ]
row.names(dat.S) <- 1:1695#1:1705

dat.Re <- param.df[param.df$param == 'Re', ]
dat.Re$param <- NULL
dat.Re <- dat.Re[order(dat.Re$group, dat.Re$run), ]
row.names(dat.Re) <- 1:1695

dat.R0 <- param.df[param.df$param == 'R0', ]
dat.R0$param <- NULL
dat.R0 <- dat.R0[order(dat.R0$group, dat.R0$run), ]
row.names(dat.R0) <- 1:1695

dat.D <- param.df[param.df$param == 'D', ]
dat.D$param <- NULL
dat.D <- dat.D[order(dat.D$group, dat.D$run), ]
row.names(dat.D) <- 1:1695

dat.L <- param.df[param.df$param == 'L', ]
dat.L$param <- NULL
dat.L <- dat.L[order(dat.L$group, dat.L$run), ]
row.names(dat.L) <- 1:1695
dat.L$value <- dat.L$value / 365 # change to years

rm(dat.text, ma, mc, o.temp, p1, p3, p4, p5, p6, p7, param.df, a.df, a.new, a.north.new,
   a.south.new, ah, c, c.df, c.new, country, cs, D.last, D.max, L.last, L.max, R0.last,
   R0.max, Re.max, rs, ss, run, S.max, season)

# permute.by.run <- function(dat) {
#   runs.to.use <- round(runif(341, 1, 5)) + 5 * (seq(1:341) - 1)
#   cols.to.use <- as.numeric(rownames(dat))[runs.to.use]
#   return(dat[cols.to.use,])
# }
permute.by.run <- function(dat) {
  runs.to.use <- round(runif(339, 1, 5)) + 5 * (seq(1:339) - 1)
  cols.to.use <- as.numeric(rownames(dat))[runs.to.use]
  return(dat[cols.to.use,])
}

stats.S = stats.Re = stats.R0 = stats.D = stats.L = c()
p.S = p.Re = p.R0 = p.D = p.L = c()
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.S)
  m <- kruskal.test(value ~ lat, data = dat.temp)
  stats.S <- c(stats.S, m$statistic); p.S <- c(p.S, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.Re)
  m <- kruskal.test(value ~ lat, data = dat.temp)
  stats.Re <- c(stats.Re, m$statistic); p.Re <- c(p.Re, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.R0)
  m <- kruskal.test(value ~ lat, data = dat.temp)
  stats.R0 <- c(stats.R0, m$statistic); p.R0 <- c(p.R0, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.D)
  m <- kruskal.test(value ~ lat, data = dat.temp)
  stats.D <- c(stats.D, m$statistic); p.D <- c(p.D, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.L)
  m <- kruskal.test(value ~ lat, data = dat.temp)
  stats.L <- c(stats.L, m$statistic); p.L <- c(p.L, m$p.value)
}
# S, Re, R0, L all highly sig; D only sig in 1/20 (<0.05) (so never at <0.01)

stats.S = stats.Re = stats.R0 = stats.D = stats.L = c()
p.S = p.Re = p.R0 = p.D = p.L = c()
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.S)
  m <- kruskal.test(value ~ hemisphere, data = dat.temp)
  stats.S <- c(stats.S, m$statistic); p.S <- c(p.S, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.Re)
  m <- kruskal.test(value ~ hemisphere, data = dat.temp)
  stats.Re <- c(stats.Re, m$statistic); p.Re <- c(p.Re, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.R0)
  m <- kruskal.test(value ~ hemisphere, data = dat.temp)
  stats.R0 <- c(stats.R0, m$statistic); p.R0 <- c(p.R0, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.D)
  m <- kruskal.test(value ~ hemisphere, data = dat.temp)
  stats.D <- c(stats.D, m$statistic); p.D <- c(p.D, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.L)
  m <- kruskal.test(value ~ hemisphere, data = dat.temp)
  stats.L <- c(stats.L, m$statistic); p.L <- c(p.L, m$p.value)
}
# only Re and R0 sig

stats.S = stats.Re = stats.R0 = stats.D = stats.L = c()
p.S = p.Re = p.R0 = p.D = p.L = c()
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.S[dat.S$lat == 'temp', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.S <- c(stats.S, m$statistic); p.S <- c(p.S, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.Re[dat.Re$lat == 'temp', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.Re <- c(stats.Re, m$statistic); p.Re <- c(p.Re, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.R0[dat.R0$lat == 'temp', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.R0 <- c(stats.R0, m$statistic); p.R0 <- c(p.R0, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.D[dat.D$lat == 'temp', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.D <- c(stats.D, m$statistic); p.D <- c(p.D, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.L[dat.L$lat == 'temp', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.L <- c(stats.L, m$statistic); p.L <- c(p.L, m$p.value)
}
# R0 sig, S maybe, D and L borderline

stats.S = stats.Re = stats.R0 = stats.D = stats.L = c()
p.S = p.Re = p.R0 = p.D = p.L = c()
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.S[dat.S$lat == 'trop', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.S <- c(stats.S, m$statistic); p.S <- c(p.S, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.Re[dat.Re$lat == 'trop', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.Re <- c(stats.Re, m$statistic); p.Re <- c(p.Re, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.R0[dat.R0$lat == 'trop', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.R0 <- c(stats.R0, m$statistic); p.R0 <- c(p.R0, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.D[dat.D$lat == 'trop', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.D <- c(stats.D, m$statistic); p.D <- c(p.D, m$p.value)
}
set.seed(105864)
for (i in 1:20) {
  dat.temp <- permute.by.run(dat.L[dat.L$lat == 'trop', ])
  m <- kruskal.test(value ~ data.type, data = dat.temp)
  stats.L <- c(stats.L, m$statistic); p.L <- c(p.L, m$p.value)
}
# R0 borderline












