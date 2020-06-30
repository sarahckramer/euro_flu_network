
### Read in fitting results:
oStates1 <- read.csv('results/fits/outputOP_A(H1)_fitsOnly.csv')
oStates2 <- read.csv('results/fits/outputOP_A(H3)_fitsOnly.csv')
oStates3 <- read.csv('results/fits/outputOP_B_fitsOnly.csv')

o1 <- read.csv('results/fits/outputOPParams_A(H1)_fitsOnly.csv')
o2 <- read.csv('results/fits/outputOPParams_A(H3)_fitsOnly.csv')
o3 <- read.csv('results/fits/outputOPParams_B_fitsOnly.csv')

oStates1$subtype <- 'A(H1)'; o1$subtype <- 'A(H1)'
oStates2$subtype <- 'A(H3)'; o2$subtype <- 'A(H3)'
oStates3$subtype <- 'B'; o3$subtype <- 'B'

o <- rbind(o1, o2, o3)
oStates <- rbind(oStates1, oStates2, oStates3)
rm(o1, o2, o3, oStates1, oStates2, oStates3)

o$subtype <- factor(o$subtype); oStates$subtype <- factor(oStates$subtype)

# S0 will just be the highest inferred S for each country/season/subtype (or S at time of first onset? or max from that timepoint on)
# but when to get the others? at highest R0? at time 15/20? week 20 appears like a decent choice

### Calculate R0/Re:
# Get countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get humidity data:
ah <- read.csv('data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])
rm(ah)

# Get season-specific starts:
source('cluster/functions/Fn_initializations.R')

# Format and prepare oStates:
oStates$country <- countries[oStates$country + 1]
oStates$country <- factor(oStates$country)

oStates$beta = oStates$R0 = oStates$Re = NA
oStates$run <- factor(oStates$run)

# Loop through seasons and get R0s:
for (season in levels(oStates$season)) {
  print(season)
  
  # Get appropriate time frame:
  tmp <- Fn_dates(season)
  weeks <- tmp$weeks + 3
  start_date <- tmp$start_date
  end_date <- tmp$end_date
  nsn <- tmp$nsn
  
  clim_start <- as.numeric(start_date - as.Date(paste('20',
                                                      substr(season, gregexpr('-', season)[[1]][1]-2,
                                                             gregexpr('-', season)[[1]][1]-1),
                                                      '-01-01', sep=''))) + 1 - 6
  clim_end <- as.numeric(end_date - as.Date(paste('20',
                                                  substr(season, gregexpr('-', season)[[1]][1]-2,
                                                         gregexpr('-', season)[[1]][1]-1),
                                                  '-01-01', sep=''))) + 1
  tm.ini <- clim_start - 1 # the end of the former week
  tm.range <- clim_start:clim_end
  tmstep <- 7
  
  # Now get range for betas and AH:
  beta.range <- seq(tm.range[1] + tmstep, (tail(tm.range, 1) + 2 * tmstep), by = tmstep) # this is how it is in model running code
  beta.range <- beta.range[2:length(beta.range)]
  AHpt <- AH[beta.range, ]; AHpt <- as.matrix(AHpt, length(AHpt), n)
  
  for (subtype in levels(oStates$subtype)) {
    oStates.temp <- oStates[oStates$season == season & oStates$subtype == subtype, ]
    if (dim(oStates.temp)[1] > 0) { # if that subtype had outbreaks that season
      rm(oStates.temp)
      
      for (j in levels(oStates$run)) {
        # oStates.temp <- oStates[oStates$season == season & oStates$subtype == subtype & oStates$run == j, ]
        o.temp <- o[o$season == season & o$subtype == subtype & o$run == j, ]
        b <- log(o.temp$R0diff); a <- -180
        
        for (country in countries) {
          oStates$R0[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] <-
            exp(a * AHpt[, which(countries == country)] + b) + (o.temp$R0mx - o.temp$R0diff)
          oStates$beta[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] <-
            oStates$R0[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] / o.temp$D
          oStates$Re[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] <-
            oStates$R0[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] *
            (oStates$S[oStates$season == season & oStates$subtype == subtype & oStates$run == j & oStates$country == country] / 100000)
        }
        
      }
      
    } else {
      rm(oStates.temp)
    }
    
  }
  
}
rm(j, a, b, AHpt, beta.range, tm.ini, clim_start, clim_end, tmstep, tmp, weeks, start_date, end_date, nsn, o.temp, tm.range)

### First, identify maximum fit S0/Re for each country/season/subtype/run
# Get maximum value from season/subtype's first onset (before that, still not really settled), and stopping at week 19
# Actually, do same for Re, and get R0 at the point where Re is maximized
# Can also remove any S0/R0 where no onsets observed
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 8, 30, 57)])
m <- m[!is.na(m$onsetObs5), ]

fits.df <- NULL
for (season in levels(oStates$season)) {
  for (subtype in levels(oStates$subtype)) {
    oStates.temp <- oStates[oStates$season == season & oStates$subtype == subtype, ]
    
    if (dim(oStates.temp)[1] > 0) {
      m.temp <- m[m$season == season & m$subtype == subtype, ]
      first.onset <- min(m.temp$onsetObs5)
      
      for (run in levels(oStates$run)) {
        for (country in countries) {
          oStates.temp <- oStates[oStates$season == season & oStates$subtype == subtype & oStates$run == run & oStates$country == country &
                                    oStates$week >= first.onset & oStates$week < 72, ]
          s0.temp <- max(oStates.temp$S)
          re.temp <- max(oStates.temp$Re)
          r0.temp <- oStates.temp$R0[which.max(oStates.temp$Re)]
          fits.df <- rbind(fits.df, c(season, country, subtype, run, s0.temp, re.temp, r0.temp))
        }
      }
      
    }
    
  }
}
rm(season, subtype, country, run, oStates.temp, first.onset, s0.temp, r0.temp, re.temp, m, m.temp)

fits.df <- as.data.frame(fits.df)
names(fits.df) <- c('season', 'country', 'subtype', 'run', 'S0', 'Re', 'R0.1')
for (i in 5:7) {
  fits.df[, i] <- as.numeric(as.character(fits.df[, i]))
}; rm(i)

### Now get param fits at time 20 (all 5 params, + R0):
o.red <- o[o$week == 59, ]
oStates.red <- oStates[oStates$week == 59, ]

fits.df$R0.2 = fits.df$R0mx = fits.df$R0diff = fits.df$L = fits.df$D = fits.df$airScale = NA
for (season in levels(oStates$season)) {
  for (subtype in levels(oStates$subtype)) {
    
    if (dim(oStates[oStates$season == season & oStates$subtype == subtype, ])[1] > 0) {
      for (run in levels(oStates$run)) {
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'R0.2'] <-
          oStates.red[oStates.red$season == season & oStates.red$subtype == subtype & oStates.red$run == run, 'R0']
        
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'R0mx'] <-
          o.red[o.red$season == season & o.red$subtype == subtype & o.red$run == run, 'R0mx']
        
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'R0diff'] <-
          o.red[o.red$season == season & o.red$subtype == subtype & o.red$run == run, 'R0diff']
        
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'L'] <-
          o.red[o.red$season == season & o.red$subtype == subtype & o.red$run == run, 'L']
        
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'D'] <-
          o.red[o.red$season == season & o.red$subtype == subtype & o.red$run == run, 'D']
        
        fits.df[fits.df$season == season & fits.df$subtype == subtype & fits.df$run == run, 'airScale'] <-
          o.red[o.red$season == season & o.red$subtype == subtype & o.red$run == run, 'airScale']
      }
    }
    
  }
}
rm(o.red, oStates.red, run, season, subtype)
fits.df <- fits.df[, c(1:7, 13:8)]

### Get region and onset timing data:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 8, 30, 57, 48:50)])
fits.df <- merge(fits.df, m, by = c('season', 'country', 'subtype')) # this also removes the 4 seasons where no data at all

### Compare S0/Re/(R0?) by subtype, country, region:
# Can retain even where onsetObs is NA, b/c we'll want to know that S0 can be fit low enough here
# But do see if results differ when no onsets are removed
cor.test(fits.df$R0.1, fits.df$R0.2, method = 'kendall') # highly sig, tau = 0.606 (old: 0.579)
# use 1 - looks to be point where better fit for synthetic, too

par(mfrow = c(3, 1), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(R0.1 ~ country, data = fits.df)
boxplot(Re ~ country, data = fits.df)
boxplot(S0 ~ country, data = fits.df)

par(mfrow = c(3, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
boxplot(R0.1 ~ subtype, data = fits.df)
boxplot(R0.1 ~ region, data = fits.df)
boxplot(Re ~ subtype, data = fits.df)
boxplot(Re ~ region, data = fits.df)
boxplot(S0 ~ subtype, data = fits.df)
boxplot(S0 ~ region, data = fits.df)

# fits.df <- fits.df[order(fits.df), ]

permute.by.run <- function(dat, choices) {
  runs.to.use <- round(runif(choices, 1, 5)) + 5 * (seq(1:choices) - 1)
  rows.to.use <- as.numeric(rownames(dat))[runs.to.use]
  return(dat[rows.to.use, ])
}

set.seed(1089437584)
p.vals.r0count = p.vals.recount = p.vals.s0count = p.vals.reregion = p.vals.s0region = p.vals.resub = p.vals.s0sub = c()
for (i in 1:100) {
  fits.df.red <- permute.by.run(fits.df, dim(fits.df)[1] / 5)
  
  p.vals.r0count <- c(p.vals.r0count, kruskal.test(R0.1 ~ country, data = fits.df.red)$p.value)
  p.vals.recount <- c(p.vals.recount, kruskal.test(Re ~ country, data = fits.df.red)$p.value)
  p.vals.s0count <- c(p.vals.s0count, kruskal.test(S0 ~ country, data = fits.df.red)$p.value)
  
  p.vals.reregion <- c(p.vals.reregion, kruskal.test(Re ~ region, data = fits.df.red)$p.value)
  p.vals.s0region <- c(p.vals.s0region, kruskal.test(S0 ~ region, data = fits.df.red)$p.value)
  
  p.vals.resub <- c(p.vals.resub, kruskal.test(Re ~ subtype, data = fits.df.red)$p.value)
  p.vals.s0sub <- c(p.vals.s0sub, kruskal.test(S0 ~ subtype, data = fits.df.red)$p.value)
}
rm(fits.df.red)

# First: Any differences in R0 by country?
summary(p.vals.r0count) # none < 0.05

# Look at Re/S0 by each factor:
summary(p.vals.resub) # 99% < 0.05
summary(p.vals.reregion) # none < 0.05
summary(p.vals.recount) # none < 0.05

summary(p.vals.s0sub) # none < 0.05
summary(p.vals.s0region) # 3% < 0.05
summary(p.vals.s0count) # 94% < 0.05

# differences in Re by subtype, S0 by country - how to do posthoc?
set.seed(1089437584)
p.list.resub = p.list.s0count = vector('list', 100)
for (i in 1:100) {
  fits.df.red <- permute.by.run(fits.df, dim(fits.df)[1] / 5)
  
  p.list.resub[[i]] <- which(posthoc.kruskal.nemenyi.test(Re ~ subtype, data = fits.df.red)$p.value < 0.05 / 3, arr.ind = T)
  p.list.s0count[[i]] <- which(posthoc.kruskal.nemenyi.test(S0 ~ country, data = fits.df.red)$p.value < 0.05 / 66, arr.ind = T)
}
rm(fits.df.red)
# none of the fits of S0 by country are significant when Bonferroni correction applied
# for subtypes/Re:
a <- sapply(p.list.resub, function(ix) {
  dim(ix)[1] > 0
})
# 49% have H3 higher than H1; never greater than B
# old: always higher than H1; only 36% have it greater than B

### Get latitude and longitude:
library(maps)
data("world.cities")
country.names <- c('Austria', 'Belgium', 'Czechia', 'France', 'Germany', 'Hungary', 'Italy', 'Luxembourg',
                   'Netherlands', 'Poland', 'Slovakia', 'Spain')
world.cities <- world.cities[(world.cities$country.etc %in% c(country.names, 'Czech Republic')) &
                               world.cities$capital == 1, ]
world.cities$country.etc <- factor(world.cities$country.etc)
levels(world.cities$country.etc) <- countries
world.cities <- world.cities[, c('country.etc', 'lat', 'long')]
fits.df <- merge(fits.df, world.cities, by.x = 'country', by.y = 'country.etc')

plot(fits.df$long, fits.df$S0)
plot(fits.df$lat, fits.df$S0)

set.seed(1089437584)
p.long = p.lat = val.long = val.lat = c()
for (i in 1:100) {
  fits.df.red <- permute.by.run(fits.df, dim(fits.df)[1] / 5)
  
  p.long <- c(p.long, cor.test(fits.df.red$long, fits.df.red$S0, method = 'kendall')$p.value)
  val.long <- c(val.long, cor.test(fits.df.red$long, fits.df.red$S0, method = 'kendall')$estimate)
  
  p.lat <- c(p.lat, cor.test(fits.df.red$lat, fits.df.red$S0, method = 'kendall')$p.value)
  val.lat <- c(val.lat, cor.test(fits.df.red$lat, fits.df.red$S0, method = 'kendall')$estimate)
}
summary(p.long)
summary(p.lat)
length(p.long[p.long < 0.05]) # 61% (old: 71%)
length(p.lat[p.lat < 0.05]) # 58% (old: 59%)
summary(val.long)
summary(val.lat)
# since we don't really talk about the geographical patterns of spread in the manuscript or supplement, this probably isn't needed, but it is interesting to know

### Compare parameter fits by subtype:
fits.df.full <- fits.df
fits.df <- fits.df[, c(1:4, 7, 9:13)]
r0s <- aggregate(R0.1 ~ season + subtype + run, data = fits.df, FUN = mean)
fits.df <- unique(fits.df[, c(2:4, 6:10)])
fits.df <- merge(fits.df, r0s)

boxplot(R0.1 ~ subtype, data = fits.df)
boxplot(R0mx ~ subtype, data = fits.df)
boxplot(R0diff ~ subtype, data = fits.df)
boxplot(D ~ subtype, data = fits.df)
boxplot(L ~ subtype, data = fits.df)
boxplot(airScale ~ subtype, data = fits.df)

boxplot(R0.1 ~ season, data = fits.df)
boxplot(R0mx ~ season, data = fits.df)
boxplot(R0diff ~ season, data = fits.df)
boxplot(D ~ season, data = fits.df)
boxplot(L ~ season, data = fits.df)
boxplot(airScale ~ season, data = fits.df)
# D by season is probably totally spurious, but makes it look like influenza is causing longer infections as time goes on...

set.seed(1089437584)
p.r0 = p.D = p.L = p.aS = c()
p.r0mx = p.r0diff = c()
for (i in 1:100) {
  fits.df.red <- permute.by.run(fits.df, dim(fits.df)[1] / 5)
  
  p.r0 <- c(p.r0, kruskal.test(R0.1 ~ subtype, data = fits.df.red)$p.value)
  p.D <- c(p.D, kruskal.test(D ~ subtype, data = fits.df.red)$p.value)
  p.L <- c(p.L, kruskal.test(L ~ subtype, data = fits.df.red)$p.value)
  p.aS <- c(p.aS, kruskal.test(airScale ~ subtype, data = fits.df.red)$p.value)
  
  p.r0mx <- c(p.r0mx, kruskal.test(R0mx ~ subtype, data = fits.df.red)$p.value)
  p.r0diff <- c(p.r0diff, kruskal.test(R0diff ~ subtype, data = fits.df.red)$p.value)
}
summary(p.r0) # none < 0.05
summary(p.D) # none < 0.05
summary(p.L) # 5%
summary(p.aS) # 3%
# R0mx and R0diff are never different

# Any need to compare these to fits from isolated model? Don't think so - there isn't anything crazy or notable here

### Plot!:
# S0, Re, R0, D, L, airScale
fits.df$country <- 'AT' # dummy
fits.df.full <- fits.df.full[, c(1:6, 14:19)]

fits.df.plot <- merge(fits.df.full, fits.df, all = TRUE)
fits.df.plot <- melt(fits.df.plot, id.vars = c('country', 'season', 'subtype', 'run', 'region', 'scaling.range', 'data.type', 'lat', 'long', 'onsetObs5'))
names(fits.df.plot)[11] <- 'param'
fits.df.plot <- fits.df.plot[!is.na(fits.df.plot$value), ]
fits.df.plot <- fits.df.plot[fits.df.plot$param %in% c('S0', 'Re', 'R0.1', 'D', 'L', 'airScale'), ]
fits.df.plot$param <- factor(fits.df.plot$param)
fits.df.plot$param <- factor(fits.df.plot$param, levels = levels(fits.df.plot$param)[c(1:2, 6, 4, 3, 5)])
levels(fits.df.plot$param)[2:3] <- c('Reff', 'R0')

dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F'),
                       param = c('S0', 'Reff', 'R0', 'D', 'L', 'airScale'),
                       y = c(93700, 2.045, 2.325, 10.62, 3400, 1.3))
p1 <- ggplot(data = fits.df.plot, aes(x = subtype, y = value)) + geom_boxplot(fill = 'gray90') +
  theme_classic() + theme(text = element_text(size = 18), aspect.ratio = 1.0,
                          strip.text = element_blank(), strip.background = element_blank(),
                          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  facet_wrap(~ param, ncol = 3, scales = 'free_y') +
  labs(x = '(Sub)type', y = 'Value') +
  geom_text(data = dat.text, aes(x = 0.62, y = y, label = label), size = 8)
print(p1)

ggsave(filename = 'results/plots/FigS11.svg', plot = p1, width = 12, height = 8)

