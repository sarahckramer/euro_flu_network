
### Explores the relationship between OEV and ensemble error variance over time ###

# Read in ensemble members for all 4 models:
e1 <- read.csv('results/original/outputDist_090119_ot.csv')
e2 <- read.csv('results/newScalings/outputDist_090919_ot.csv')
e3 <- read.csv('results/propRandTravel/outputDist_090919_ot.csv')
e4 <- read.csv('results/indivCountries/outputDist_082819_OT.csv'); e4$gamma <- NULL

# Read in and format metrics files:
m1 <- read.csv('results/original/outputMet_090119_pro.csv')
m2 <- read.csv('results/newScalings/outputMet_090919_pro.csv')
m3 <- read.csv('results/propRandTravel/outputMet_090919_pro.csv')
m4 <- read.csv('results/indivCountries/outputMet_082819_pro.csv')

m1 <- m1[, c(1:3, 7:8, 39, 43, 47, 99)]
m2 <- m2[, c(1:3, 7:8, 39, 43, 47, 99)]
m3 <- m3[, c(1:3, 7:8, 39, 43, 47, 99)]
m4 <- m4[, c(2:3, 5, 8, 1, 39, 43, 47, 67)]

# Merge with dist files:
e1 <- merge(e1, m1, by = c('season', 'run', 'oev_base', 'fc_start', 'country'))
e2 <- merge(e2, m2, by = c('season', 'run', 'oev_base', 'fc_start', 'country'))
e3 <- merge(e3, m3, by = c('season', 'run', 'oev_base', 'fc_start', 'country'))
e4 <- merge(e4, m4, by = c('season', 'run', 'oev_base', 'fc_start', 'country'))

# Note: Because network model has predictions even for countries without data, there may be more predictions than for the individual results
    # Although since we remove where no onset occurred, this might not actually be true

# Limit to lead weeks of interest (-6:6)
    # Note: Use observed lead, so we can look even at ones where most are NA (i.e., no onset predicted)
e1 <- e1[e1$FWeek_onwk >= -6 & e1$FWeek_onwk < 7 & !is.na(e1$FWeek_onwk), ]
e2 <- e2[e2$FWeek_onwk >= -6 & e2$FWeek_onwk < 7 & !is.na(e2$FWeek_onwk), ]
e3 <- e3[e3$FWeek_onwk >= -6 & e3$FWeek_onwk < 7 & !is.na(e3$FWeek_onwk), ]
e4 <- e4[e4$FWeek_onwk >= -6 & e4$FWeek_onwk < 7 & !is.na(e4$FWeek_onwk), ]

# Reformat data frames and column names:
e1 <- e1[, c(1:5, 9:14)]
e2 <- e2[, c(1:5, 9:14)]
e3 <- e3[, c(1:5, 9:14)]
e4 <- e4[, c(1:5, 9:14)]
names(e4)[6:7] <- c('bin', 'value')

# Correct bin values to reflect actual week:
e1$bin <- e1$bin + 40 - 1; e1$bin[e1$bin == 38] <- -1
e2$bin <- e2$bin + 40 - 1; e2$bin[e2$bin == 38] <- -1
e3$bin <- e3$bin + 40 - 1; e3$bin[e3$bin == 38] <- -1
e4$bin <- e4$bin + 40 - 1; e4$bin[e4$bin == 38] <- -1

# Calculate ensemble variance, as well as proportion non-NA, for each forecast:
calcVar_OT <- function(df) {
  ot.vals <- c()
  for (ix in 1:dim(df)[1]) {
    ot.vals <- c(ot.vals, rep(df$bin[ix], round(df$value[ix] * 300)))
  }
  num.nas <- length(which(ot.vals == -1))
  ot.vals <- ot.vals[ot.vals != -1]
  var.ot <- var(ot.vals)
  return(list(var.ot, num.nas))
}

v2 = v2 = v3 = v4 = NULL
for (season in unique(e1$season)) {
  for (o1 in unique(e1$oev_base)) {
    for (fc_start in unique(e1$fc_start)) {
      for (country in unique(e1$country)) {
        for (run in unique(e1$run)) {
          
          e.temp <- e1[e1$season == season & e1$oev_base == o1 & e1$fc_start == fc_start & e1$country == country & e1$run == run, ]
          if (length(e.temp$country) > 0) {
            res <- calcVar_OT(e.temp)
            v2 <- rbind(v2, c(country, season, run, o1, fc_start, unique(e.temp$onsetObs5), unique(e.temp$FWeek_onwk), res[[1]], res[[2]]))
          }
          
          e.temp <- e2[e2$season == season & e2$oev_base == o1 & e2$fc_start == fc_start & e2$country == country & e2$run == run, ]
          if (length(e.temp$country) > 0) {
            res <- calcVar_OT(e.temp)
            v2 <- rbind(v2, c(country, season, run, o1, fc_start, unique(e.temp$onsetObs5), unique(e.temp$FWeek_onwk), res[[1]], res[[2]]))
          }
          
          e.temp <- e3[e3$season == season & e3$oev_base == o1 & e3$fc_start == fc_start & e3$country == country & e3$run == run, ]
          if (length(e.temp$country) > 0) {
            res <- calcVar_OT(e.temp)
            v3 <- rbind(v3, c(country, season, run, o1, fc_start, unique(e.temp$onsetObs5), unique(e.temp$FWeek_onwk), res[[1]], res[[2]]))
          }
          
          e.temp <- e4[e4$season == season & e4$oev_base == o1 & e4$fc_start == fc_start & e4$country == country & e4$run == run, ]
          if (length(e.temp$country) > 0) {
            res <- calcVar_OT(e.temp)
            v4 <- rbind(v4, c(country, season, run, o1, fc_start, unique(e.temp$onsetObs5), unique(e.temp$FWeek_onwk), res[[1]], res[[2]]))
          }
          
        }
      }
    }
  }
}

v2 <- as.data.frame(v2)
names(v2) = c('country', 'season', 'run', 'oev_base', 'fc_start', 'onsetObs5', 'FWeek_onwk', 'ensvar', 'countNA')
for (i in c(5:6, 8:9)) {
  v2[, i] <- as.numeric(as.character(v2[, i]))
}

v2 <- as.data.frame(v2)
names(v2) = c('country', 'season', 'run', 'oev_base', 'fc_start', 'onsetObs5', 'FWeek_onwk', 'ensvar', 'countNA')
for (i in c(5:6, 8:9)) {
  v2[, i] <- as.numeric(as.character(v2[, i]))
}

v3 <- as.data.frame(v3)
names(v3) = c('country', 'season', 'run', 'oev_base', 'fc_start', 'onsetObs5', 'FWeek_onwk', 'ensvar', 'countNA')
for (i in c(5:6, 8:9)) {
  v3[, i] <- as.numeric(as.character(v3[, i]))
}

v4 <- as.data.frame(v4)
names(v4) = c('country', 'season', 'run', 'oev_base', 'fc_start', 'onsetObs5', 'FWeek_onwk', 'ensvar', 'countNA')
for (i in c(5:6, 8:9)) {
  v4[, i] <- as.numeric(as.character(v4[, i]))
}

# Convert number of NAs to number of non-NAs:
v1$countNA <- 300 - v1$countNA
v2$countNA <- 300 - v2$countNA
v3$countNA <- 300 - v3$countNA
v4$countNA <- 300 - v4$countNA

# Join all 4 data frames:
v1$model <- 'Network (Original)'; v2$model <- 'New Scalings'; v3$model <- 'Random Travel'; v4$model <- 'Individual'
dfv <- rbind(v1, v2, v3, v4)
dfv$model <- factor(dfv$model)
dfv$model <- factor(dfv$model, levels = levels(dfv$model)[c(2:4, 1)])
dfv$group <- paste(dfv$model, dfv$FWeek_onwk, sep = '_'); dfv$group <- factor(dfv$group)
dfv$FWeek_onwk <- as.numeric(as.character(dfv$FWeek_onwk))

# Calculate aggregates, too:
df.mean <- aggregate(ensvar ~ FWeek_onwk + model + oev_base, data = dfv, FUN = mean)
df.median <- aggregate(ensvar ~ FWeek_onwk + model + oev_base, data = dfv, FUN = median)
df.mean.nonNA <- aggregate(countNA ~ FWeek_onwk + model + oev_base, data = dfv, FUN = mean)
df.median.nonNA <- aggregate(countNA ~ FWeek_onwk + model + oev_base, data = dfv, FUN = median)

df.mean <- merge(df.mean, df.mean.nonNA, by = c('FWeek_onwk', 'model', 'oev_base'))
df.median <- merge(df.median, df.median.nonNA, by = c('FWeek_onwk', 'model', 'oev_base'))

# Plot:
p1 <- ggplot(data = dfv, aes(x = FWeek_onwk, y = ensvar, fill = model, group = group)) + geom_boxplot() +
  facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Ensemble Variance', fill = '') + scale_x_continuous(breaks = -6:6)
p2 <- ggplot(data = df.mean, aes(x = FWeek_onwk, y = ensvar, col = model)) + geom_line() + geom_point(aes(size = countNA)) +
  facet_wrap(~oev_base, ncol = 2) + theme_bw() + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:6) +
  labs(x = 'Observed Lead Week', y = 'Ensemble Variance (Mean)', col = '', size = '# Not NA\n(Mean)') + guides(col = FALSE)
p3 <- ggplot(data = df.median, aes(x = FWeek_onwk, y = ensvar, col = model)) + geom_line() + geom_point(aes(size = countNA)) +
  facet_wrap(~oev_base, ncol = 2) + theme_bw() + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = -6:6) +
  labs(x = 'Observed Lead Week', y = 'Ensemble Variance (Median)', col = '', size = '# Not NA\n(Median)') + guides(col = FALSE)
grid.arrange(p1, p2, p3, ncol = 1)
# Although network seems to gain some advantage a couple weeks before the peak, early on, the individual model is more certain and more likely to be correct about onset
    # This is not what we would expect - what is happening?

# Save:
pdf('results/plots/comp_ensVar_byObs.pdf', width = 14, height = 9)
grid.arrange(p1, p2, p3, ncol = 1)
dev.off()




