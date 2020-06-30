
### Assess Cross-Ensemble Covariability ###
# Here we're just looking at fit, so whether forecasts predict onset or not is not relevant here

# Plot:
    # Correlation against R0mx, R0diff, D, L, and airScale for all 12 countries
    # Correlation between each of 12 countries and: all S; only S "involving" country; only main S
    # Same for I and newI

# We'll have plots like this for several (sub)types and seasons and runs...

countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# Read in and combine results:
a <- read.csv('results/fits/outputCorrCoefs_A(H1).csv')
a$country <- countries[a$country + 1]
b <- read.csv('results/fits/outputCorrCoefs_A(H3).csv')
b$country <- countries[b$country + 1]
c <- read.csv('results/fits/outputCorrCoefs_B.csv')
c$country <- countries[c$country + 1]

a$subtype <- 'A(H1)'; b$subtype <- 'A(H3)'; c$subtype <- 'B'
a <- rbind(a, b, c)
rm(b, c)

# where to find different states
s.vals <- 3:146
i.vals <- 147:290
newi.vals <- 291:434

# limit to season (through week 20, or 20+52=72)
a <- a[a$week < 73, ]
a$country <- factor(a$country); a$subtype <- factor(a$subtype)

# for parameters, can get means/medians, as well as sds or quantiles, by time for the 5 parameters?
# then for state variables... do same, for each country separately? Average over all S, I, newI? Or just those that "involve" the country? or just main compartments? main vs. commuter?
# for each country, look just at the main compartment for S/I/newI
# 5 params + 3 states = 8 panels
# repeat for isolated?
# Do we separate these by country? subtype? It's going to get to be a lot of plotting if so

# ggplot(data = a, aes(x = week, y = X4, group = paste(subtype, season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
#   labs(x = 'Week', y = 'state') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
a.red <- aggregate(data = a, X434 ~ week + country, FUN = mean)
ggplot(data = a, aes(x = week, y = X434, group = week)) + geom_boxplot() + facet_wrap(~ country) + theme_bw()

get_quantiles <- function(dat, parm) {
  dat.temp <- dat[, c('week', 'country', 'season', 'run', 'subtype', parm)]
  dat.out <- NULL
  
  for (wk in 40:72) {
    for (country in levels(dat.temp$country)) {
      dat.temp2 <- dat.temp[dat.temp$week == wk & dat.temp$country == country, ]
      dat.out <- rbind(dat.out, c(wk, country, mean(dat.temp2[, parm], na.rm = TRUE), quantile(dat.temp2[, parm], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
    }
  }
  
  dat.out <- as.data.frame(dat.out)
  names(dat.out) <- c('week', 'country', 'mean', 'b95', 'b50', 'median', 't50', 't95')
  for (i in c(1, 3:8)) {
    dat.out[, i] <- as.numeric(as.character(dat.out[, i]))
  }
  return(dat.out)
}

dat.L <- get_quantiles(a, 'X433')
dat.D <- get_quantiles(a, 'X434')
dat.R0mx <- get_quantiles(a, 'X435')
dat.R0diff <- get_quantiles(a, 'X436')
dat.aScale <- get_quantiles(a, 'X437')

# Now limit S, I, newI to "main" compartments, and specify by country:
mat.coll <- matrix(0, 12, 12)
diag(mat.coll) <- 1
to.keep <- which(mat.coll == 1)
rm(mat.coll)

a.red <- a[, c(1:2, s.vals[to.keep], i.vals[to.keep], newi.vals[to.keep], 440:441, 445)] # only "main" compartments
a.red$country <- factor(a.red$country, levels = countries)

s.vals <- 3:14
i.vals <- 15:26
newi.vals <- 27:38

a.red$newI = a.red$I = a.red$S = NA
for (i in 1:length(countries)) {
  country <- countries[i]
  a.red$S[a.red$country == country] <- a.red[a.red$country == country, s.vals[i]]
  a.red$I[a.red$country == country] <- a.red[a.red$country == country, i.vals[i]]
  a.red$newI[a.red$country == country] <- a.red[a.red$country == country, newi.vals[i]]
}
a.red <- a.red[, c(1:2, 39:44)]

dat.S <- get_quantiles(a.red, 'S')
dat.I <- get_quantiles(a.red, 'I')
dat.newI <- get_quantiles(a.red, 'newI')

# pdf('results/plots/correlations_06-18.pdf', width = 18, height = 10)
ggplot(data = a, aes(x = week, y = X433, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'L') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a, aes(x = week, y = X434, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'D') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a, aes(x = week, y = X435, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'R0max') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a, aes(x = week, y = X436, group = paste(subtype, season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'R0diff') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a, aes(x = week, y = X437, group = paste(subtype, season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'aScale') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))

ggplot(data = a.red, aes(x = week, y = S, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'Susceptible') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a.red, aes(x = week, y = I, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'Infected') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = a.red, aes(x = week, y = newI, group = paste(subtype, season, run), col = subtype)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'Newly Infected') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))

ggplot(data = dat.L) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Week', y = 'L') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = dat.D) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Week', y = 'D') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = dat.R0mx) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Week', y = 'R0max') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = dat.R0diff) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Week', y = 'R0diff') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = dat.aScale) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Week', y = 'airAdj') + scale_y_continuous(limits = c(-1, 1))
# dev.off()
# violin plots are an alternative, but they don't look super informative

# potentially combine over all countries, since no clear patterns by country:
get_quantiles <- function(dat, parm) {
  dat.temp <- dat[, c('week', 'country', 'season', 'run', 'subtype', parm)]
  dat.out <- NULL
  
  for (wk in 40:72) {
    dat.temp2 <- dat.temp[dat.temp$week == wk, ]
    dat.out <- rbind(dat.out, c(wk, mean(dat.temp2[, parm], na.rm = TRUE), quantile(dat.temp2[, parm], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  }
  
  dat.out <- as.data.frame(dat.out)
  names(dat.out) <- c('week', 'mean', 'b95', 'b50', 'median', 't50', 't95')
  for (i in c(1, 2:7)) {
    dat.out[, i] <- as.numeric(as.character(dat.out[, i]))
  }
  return(dat.out)
}

dat.L <- get_quantiles(a, 'X433')
dat.D <- get_quantiles(a, 'X434')
dat.R0mx <- get_quantiles(a, 'X435')
dat.R0diff <- get_quantiles(a, 'X436')
dat.aScale <- get_quantiles(a, 'X437')
dat.S <- get_quantiles(a.red, 'S')
dat.I <- get_quantiles(a.red, 'I')
dat.newI <- get_quantiles(a.red, 'newI')

dat.L$metric <- 'L'; dat.D$metric <- 'D'; dat.R0mx$metric <- 'R0max'; dat.R0diff$metric <- 'R0diff'; dat.aScale$metric <- 'airAdj'
dat.S$metric <- 'S'; dat.I$metric <- 'I'; dat.newI$metric <- 'newI'

dat.plot <- rbind(dat.S, dat.I, dat.newI, dat.L, dat.D, dat.R0mx, dat.R0diff, dat.aScale)
dat.plot$metric <- factor(dat.plot$metric)
dat.plot$metric <- factor(dat.plot$metric, levels = levels(dat.plot$metric)[c(8, 3, 5, 4, 2, 7:6, 1)])

p1 <- ggplot(data = dat.plot) + geom_ribbon(aes(x = week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = week, y = median)) + theme_bw() + facet_wrap(~ metric, ncol = 2) + labs(x = 'Week', y = 'Correlation Coef.') + scale_y_continuous(limits = c(-1, 1))
p1

# edit text size; letters; 4x2 or 2x4?; see other plots for reference (anything else to include?); add values on x- and y-axes; keep facets?






# and check for isolated?:










rm(list = ls())

### Assess Filter Divergence Over Time ###
# For each country, show by observed lead week (no "predicted" since we're looking at fits, not forecasts)
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
a <- read.csv('results/fits/outputVarRatio_A(H1).csv')
b <- read.csv('results/fits/outputVarRatio_A(H3).csv')
c <- read.csv('results/fits/outputVarRatio_B.csv')

a$subtype <- 'A(H1)'; b$subtype <- 'A(H3)'; c$subtype <- 'B'
a <- rbind(a, b, c)
rm(b, c)

# Set country names:
a$country <- countries[a$country + 1]#; b$country <- countries[b$country + 1]
a$country <- factor(a$country, levels = countries)

# Get observed lead weeks:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 8:9, 30, 57)])
a <- merge(a, m, by = c('season', 'country', 'subtype')) # note that this does remove a few, since we don't include in m those forecasts not generated for the isolated model
# b <- merge(b, m, by = c('season', 'country'))
a <- a[!is.na(a$onsetObs5), ] # remove where no outbreak occurs
a$lead_week <- a$week - a$obs_pkwk
# b$lead_week <- b$week - b$obs_pkwk

# # Again, several (sub)types, seasons, and runs...
# ggplot(data = a, aes(x = week, y = ratio, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) + labs(x = 'Week', y = 'OEV / Ens. Var.') +
#   scale_y_continuous(trans = 'log10')

# By observed lead:
a1 <- a[a$lead_week >= -8 & a$lead_week <= 8 & a$week != 40, ] # remove week 40, too, since this is where everything is initiated - no fitting has occurred yet
a1$season <- factor(a1$season, levels = levels(a1$season)[c(1, 7, 2:5, 8, 6)])
# b1 <- b[b$lead_week >= -8 & b$lead_week <= 8, ]

# ggplot(data = a1, aes(x = lead_week, y = ratio, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) +
#   labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.') + scale_y_continuous(trans = 'log10')
# p1 <- ggplot(data = a1, aes(x = lead_week, y = ratio, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_grid(country ~ subtype) +
#   labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.', col = 'Season') + scale_x_continuous(breaks = -8:8) + scale_color_brewer(palette = 'Set2') +
#   scale_y_continuous(trans = 'log10')
# p1
# similar patterns by country and (sub)type - might do a similar thing again and collapse, or at least only show by country

get_quantiles <- function(dat, parm) {
  dat.temp <- dat[, c('country', 'season', 'run', 'subtype', 'lead_week', parm)]
  dat.out <- NULL
  
  for (wk in -8:8) {
    for (country in levels(dat.temp$country)) {
      dat.temp2 <- dat.temp[dat.temp$lead_week == wk & dat.temp$country == country, ]
      dat.out <- rbind(dat.out, c(wk, country, mean(dat.temp2[, parm], na.rm = TRUE), quantile(dat.temp2[, parm], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
    }
  }
  
  dat.out <- as.data.frame(dat.out)
  names(dat.out) <- c('lead_week', 'country', 'mean', 'b95', 'b50', 'median', 't50', 't95')
  for (i in c(1, 3:8)) {
    dat.out[, i] <- as.numeric(as.character(dat.out[, i]))
  }
  return(dat.out)
}

a.red <- get_quantiles(a1, 'ratio')

p1 <- ggplot(data = a.red) + geom_ribbon(aes(x = lead_week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = lead_week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = lead_week, y = median)) + theme_bw() + facet_wrap(~ country) + labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.') +
  scale_y_continuous(trans = 'log10')
p1

# but do we need to separate by country?
get_quantiles <- function(dat, parm) {
  dat.temp <- dat[, c('country', 'season', 'run', 'subtype', 'lead_week', parm)]
  dat.out <- NULL
  
  for (wk in -8:8) {
    dat.temp2 <- dat.temp[dat.temp$lead_week == wk, ]
    dat.out <- rbind(dat.out, c(wk, mean(dat.temp2[, parm], na.rm = TRUE), quantile(dat.temp2[, parm], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  }
  
  dat.out <- as.data.frame(dat.out)
  names(dat.out) <- c('lead_week', 'mean', 'b95', 'b50', 'median', 't50', 't95')
  for (i in c(1, 2:7)) {
    dat.out[, i] <- as.numeric(as.character(dat.out[, i]))
  }
  return(dat.out)
}

a.red <- get_quantiles(a1, 'ratio')

p1 <- ggplot(data = a.red) + geom_ribbon(aes(x = lead_week, ymin = b95, ymax = t95), fill = 'gray90') + geom_ribbon(aes(x = lead_week, ymin = b50, ymax = t50), fill = 'gray80') +
  geom_line(aes(x = lead_week, y = median)) + theme_bw() + labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.') +
  scale_y_continuous(trans = 'log10')
p1





# change x-axis ticks; y-axis label should say log scale?; edit text size; again, look at other plots for suggestions; do we even need all countries?


# check for isolated:








# remove "borders" of white around x-axis limits; work with text size; thicken lines in legend?

pdf('results/plots/divergence_06-18.pdf', width = 15, height = 18)
print(p1)
# print(p2)
dev.off()








