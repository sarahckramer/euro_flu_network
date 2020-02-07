
### Look at syndromic+ data quality by various metrices ###
# And compare with accuracy by country

# Results:
# Smoother data yield higher PT and PI accuracy overall, but not for 17-18 specifically
# Reporting more seasons, or a higher proportion of seasons being useable, is not sig
# A higher proportion of NAs and 0s is correlated with lower PI accuracy overall, but not
# for 17-18 specifically
# A higher % of the population being tested is associated with higher PT and PI accuracy,
# but not for 17-18. Adding 17-18 to retrospective results reduces strength of the association,
# but it is still significant.
#   Are the countries with higher % tested also the ones with higher attack rates?
#       Not sig. associated w/ observed PT or PI
# Wide range of % of timely observations, but not correlated with accuracy

###############################################################################################
# First, calculate overall accuracy by country, to be compared with metrics calculated here:
m <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/results/outputMetrics_RT1718_onset.csv')
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$onset5) & !is.na(m$onsetObs), ]

m.temp$accurate_pkwk <- as.numeric(m.temp$accurate_pkwk) - 1
m.temp$accurate_int <- as.numeric(m.temp$accurate_int) - 1

pt.m <- aggregate(m.temp$accurate_pkwk, by = list(m.temp$country), FUN = mean)
pi.m <- aggregate(m.temp$accurate_int, by = list(m.temp$country), FUN = mean)
names(pt.m) <- c('country', 'pt_acc'); names(pi.m) <- c('country', 'pi_acc')

# And read in metrics file from retrospective forecasts:
m.ret <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_new_FIN.csv')
m.ret <- m.ret[!(m.ret$country == 'Mexico' & m.ret$season == '2010-11'), ]
m.ret.temp <- m.ret[m.ret$leadpkwk_mean >= -6 & m.ret$leadpkwk_mean < 5 & !is.na(m.ret$onset5) & !is.na(m.ret$onsetObs5), ]

m.ret.temp$accurate_pkwk <- as.numeric(m.ret.temp$accurate_pkwk) - 1
m.ret.temp$accurate_int <- as.numeric(m.ret.temp$accurate_int) - 1

ret.pt.m <- aggregate(m.ret.temp$accurate_pkwk, by = list(m.ret.temp$country), FUN = mean)
ret.pi.m <- aggregate(m.ret.temp$accurate_int, by = list(m.ret.temp$country), FUN = mean)
names(ret.pt.m) <- c('country', 'pt_acc'); names(ret.pi.m) <- c('country', 'pi_acc')

###############################################################################################
###############################################################################################
###############################################################################################

### Smoothness ###
# We've already looked at this for past data overall, where smoothness is positively
# correlated with overall pt and pi accuracy; smoothness is also significantly lower
# for tropical than for temperate countries

# What about for 17-18 data?
ili <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/data/data_Week69.csv')
ili <- ili[, c(1:11, 13:36, 38:41)]

# All NAs: Croatia, Sweden, United Kingdom:
ili <- ili[, c(1:5, 7:34, 36:37, 39)]

# Change country names as needed:
names(ili)[c(27, 29)] <- c('Republic of Moldova', 'Russian Federation')

# Calculate "smoothness":
cors <- c()
for (i in 2:36) {
  x <- ili[, i]
  x[x < 0] <- NA
  cors <- c(cors, cor(x[-length(x)], x[-1], use = 'pairwise.complete.obs'))
}
smooth <- as.data.frame(matrix(c(names(ili)[-1], cors), ncol = 2))
smooth$V2 <- as.numeric(as.character(smooth$V2))
names(smooth) <- c('country', 'smoothness')

# Smoothness ranges from 0.695 to 0.968, w/ mean of 0.887 and median 0.889
# So it is fairly normally distributed
# Lowest (below 0.8): Kyrgyzstan, Luxembourg, Uzbekistan
# Highest (above 0.95): Russia, Latvia, Estonia

# Add accuracy, data type and see if they are related:
smooth <- merge(smooth, pt.m, by = 'country')
smooth <- merge(smooth, pi.m, by = 'country')

m.red <- unique(m[, c('country', 'data.type')])
smooth <- merge(smooth, m.red, by = 'country')
smooth$data.type <- relevel(smooth$data.type, ref = 'ILI')

cor.test(smooth$smoothness, smooth$pt_acc)
cor.test(smooth$smoothness, smooth$pi_acc)
summary(aov(smoothness ~ data.type, data = smooth))
# none of these are significant

rm(smooth)

###############################################################################################
###############################################################################################
###############################################################################################

# How many seasons had to be removed for a given country? How many were reported? How
# many were left?

# Number of seasons available:
print(levels(m.ret$season)) # 7 total

countries <- levels(m.ret$country)
season.count <- c()
for (country in levels(m.ret$country)) {
  season.count <- c(season.count, length(unique(m.ret$season[m.ret$country == country])))
}

season.count <- as.data.frame(cbind(countries, season.count))
names(season.count) <- c('country', 'count'); season.count$count <- factor(season.count$count)

season.count <- merge(season.count, ret.pt.m, by = 'country')
season.count <- merge(season.count, ret.pi.m, by = 'country')

# For all countries:
summary(aov(pt_acc ~ count, data = season.count)) # not sig
summary(aov(pi_acc ~ count, data = season.count)) # not sig

# For northern hemisphere only:
season.count <- season.count[!(season.count$country %in% c('Australia', 'New Zealand', 'Chile')), ]
season.count$count <- factor(season.count$count)
summary(aov(pt_acc ~ count, data = season.count)) # not sig
summary(aov(pi_acc ~ count, data = season.count)) # not sig

# For Europe only:
season.count <- season.count[!(season.count$country %in% c('Canada', 'Mexico', 'Morocco', 'United State of America')), ]
season.count$count <- factor(season.count$count)
summary(aov(pt_acc ~ count, data = season.count)) # not sig
summary(aov(pi_acc ~ count, data = season.count)) # not sig

rm(season.count)

# Proportion of total originally available that could be kept:
season.count <- c()
for (country in levels(m.ret$country)) {
  season.count <- c(season.count, length(unique(m.ret$season[m.ret$country == country])))
}

season.count <- as.data.frame(cbind(countries, season.count))
names(season.count) <- c('country', 'kept')

season.count$orig <- season.count$kept; season.count$orig <- as.character(season.count$orig)

season.count$orig[season.count$country == 'Canada'] <- 7
season.count$orig[season.count$country == 'Czechia'] <- 7
season.count$orig[season.count$country == 'Serbia'] <- 7
season.count$orig[season.count$country == 'Chile'] <- 3
season.count$orig[season.count$country == 'Morocco'] <- 7
season.count$orig[season.count$country == 'Greece'] <- 7
season.count$orig[season.count$country == 'Australia'] <- 7
season.count$orig[season.count$country == 'Lithuania'] <- 7
season.count$orig[season.count$country == 'Poland'] <- 7
season.count$orig[season.count$country == 'Norway'] <- 7
season.count$orig[season.count$country == 'New Zealand'] <- 6

season.count$orig <- as.numeric(season.count$orig)
season.count$kept <- as.numeric(as.character(season.count$kept))

season.count$prop <- season.count$kept / season.count$orig
season.count$prop.dis <- ifelse(season.count$prop == 1, 1, 0)
season.count$prop.dis <- as.factor(season.count$prop.dis)

season.count <- merge(season.count, ret.pt.m, by = 'country')
season.count <- merge(season.count, ret.pi.m, by = 'country')

# For all countries:
summary(aov(pt_acc ~ prop.dis, data = season.count)) # not sig
summary(aov(pi_acc ~ prop.dis, data = season.count)) # not sig
cor.test(season.count$prop, season.count$pt_acc, method = 'kendall', exact = F) # not sig
cor.test(season.count$prop, season.count$pi_acc, method = 'kendall', exact = F) # not sig

# For northern hemisphere only:
season.count <- season.count[!(season.count$country %in% c('Australia', 'New Zealand', 'Chile')), ]
summary(aov(pt_acc ~ prop.dis, data = season.count)) # not sig
summary(aov(pi_acc ~ prop.dis, data = season.count)) # not sig
cor.test(season.count$prop, season.count$pt_acc, method = 'kendall', exact = F) # not sig
cor.test(season.count$prop, season.count$pi_acc, method = 'kendall', exact = F) # not sig

# For Europe only:
season.count <- season.count[!(season.count$country %in% c('Canada', 'Mexico', 'Morocco', 'United State of America')), ]
summary(aov(pt_acc ~ prop.dis, data = season.count)) # not sig
summary(aov(pi_acc ~ prop.dis, data = season.count)) # not sig
cor.test(season.count$prop, season.count$pt_acc, method = 'kendall', exact = F) # not sig
cor.test(season.count$prop, season.count$pi_acc, method = 'kendall', exact = F) # not sig

rm(season.count)

###############################################################################################
###############################################################################################
###############################################################################################

# What percentage of data are NA? NA or 0?
ili.ret <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/WHO_dat_ALL_05-12_NOPAN.csv')
ili.ret <- ili.ret[, c(1:3, 5:6, 10, 12:13, 15, 17:18, 20:25, 27:28, 30:33, 35:38, 40:44,
                       49:54, 56:59, 62:66)]
# has Sweden and UK, but RT doesn't
# look only at in-season values

for (i in 2:36) {
  ili[, i][ili[, i] < 0] <- NA
}
for (i in 2:47) {
  ili.ret[, i][ili.ret[, i] < 0] <- NA
}

seasons.north <- list(79:110, 131:162, 183:214, 235:266, 287:318, 339:371, 392:423)
seasons.south <- lapply(1:7, function(ix) {seasons.north[[ix]] - 26})

# Retrospective data only:
countries <- levels(m.ret$country)

prop.na.retro <- c(); prop.na.plus.retro <- c()
prop.na <- c(); prop.na.plus <- c()
prop.na.curr <- c(); prop.na.plus.curr <- c()

for (count.index in 1:length(countries)) {
  country <- countries[count.index]
  seasons <- which(levels(m.ret$season) %in% unique(m.ret$season[m.ret$country == country]))
  
  # start with retro only:
  data.to.use <- c()
  
  if (country %in% c('Australia', 'New Zealand', 'Chile')) {
    for (season in seasons) {
      data.to.use <- c(data.to.use, ili.ret[seasons.south[[season]], count.index + 1])
    }
  } else {
    for (season in seasons) {
      data.to.use <- c(data.to.use, ili.ret[seasons.north[[season]], count.index + 1])
    }
  }
  
  prop.na.retro <- c(prop.na.retro, length(data.to.use[is.na(data.to.use)]) / length(data.to.use))
  prop.na.plus.retro <- c(prop.na.plus.retro, length(data.to.use[data.to.use == 0 | is.na(data.to.use)]) / length(data.to.use))
  
  # now include 17-18 data, if they exist:
  if (country %in% names(ili)) {
    data.to.use <- c(data.to.use, ili[, names(ili) == country])
    
    prop.na <- c(prop.na, length(data.to.use[is.na(data.to.use)]) / length(data.to.use))
    prop.na.plus <- c(prop.na.plus, length(data.to.use[data.to.use == 0 | is.na(data.to.use)]) / length(data.to.use))
    
    # while we're here, also do 17-18 alone:
    data.to.use <- ili[, names(ili) == country]
    prop.na.curr <- c(prop.na.curr, length(data.to.use[is.na(data.to.use)]) / length(data.to.use))
    prop.na.plus.curr <- c(prop.na.plus.curr, length(data.to.use[data.to.use == 0 | is.na(data.to.use)]) / length(data.to.use))
    
  } else {
    prop.na <- c(prop.na, NA); prop.na.plus <- c(prop.na.plus, NA)
    prop.na.curr <- c(prop.na.curr, NA); prop.na.plus.curr <- c(prop.na.plus.curr, NA)
  }
  
}

props <- as.data.frame(cbind(countries, prop.na, prop.na.plus, prop.na.retro,
                             prop.na.plus.retro, prop.na.curr, prop.na.plus.curr))
for (i in 2:7) {
  props[, i] <- as.numeric(as.character(props[, i]))
}
names(props)[1] <- 'country'

props <- merge(props, ret.pt.m, by = 'country'); props <- merge(props, ret.pi.m, by = 'country')
props <- merge(props, pt.m, by = 'country', all = T); props <- merge(props, pi.m, by = 'country', all = T)

# 17-18:
cor.test(props$prop.na.curr, props$pt_acc.y)
cor.test(props$prop.na.curr, props$pi_acc.y)
cor.test(props$prop.na.plus.curr, props$pt_acc.y)
cor.test(props$prop.na.plus.curr, props$pi_acc.y)

# Retrospective:
cor.test(props$prop.na.retro, props$pt_acc.x)
cor.test(props$prop.na.retro, props$pi_acc.x)
cor.test(props$prop.na.plus.retro, props$pt_acc.x)
cor.test(props$prop.na.plus.retro, props$pi_acc.x) # sig!! cor = -0.419

props <- props[!(props$country %in% c('Australia', 'New Zealand', 'Chile')), ]
cor.test(props$prop.na.retro, props$pt_acc.x)
cor.test(props$prop.na.retro, props$pi_acc.x)
cor.test(props$prop.na.plus.retro, props$pt_acc.x)
cor.test(props$prop.na.plus.retro, props$pi_acc.x) # sig!! cor = -0.462

props <- props[!(props$country %in% c('Canada', 'Mexico', 'Morocco', 'United States of America')), ]
cor.test(props$prop.na.retro, props$pt_acc.x)
cor.test(props$prop.na.retro, props$pi_acc.x)
cor.test(props$prop.na.plus.retro, props$pt_acc.x)
cor.test(props$prop.na.plus.retro, props$pi_acc.x) # sig!! cor = -0.457

# a higher proportion of NAs and 0s is correlated with lower PI accuracy, but that's all

###############################################################################################
###############################################################################################
###############################################################################################

# How many samples are tested throughout season, normalized by population size
vir.ret <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/WHO_data/data_4-12-18/vir_dat_temp.csv')
vir.ret <- vir.ret[vir.ret$Country %in% levels(m.ret$country), ]
vir.ret <- vir.ret[!(vir.ret$Country %in% c('Australia', 'New Zealand', 'Chile')), ]
vir.ret$Country <- factor(vir.ret$Country) # temperate only for now
vir.ret <- vir.ret[vir.ret$Week >= 40 | vir.ret$Week <= 20, ]
vir.ret <- vir.ret[vir.ret$Year >= 2010, ]
vir.ret <- vir.ret[!(vir.ret$Year == 2010 & vir.ret$Week < 40), ]
vir.ret <- vir.ret[, 1:4]

vir.ret$season[vir.ret$Year == 2010 | (vir.ret$Year == 2011 & vir.ret$Week <= 20)] <- '2010-11'
vir.ret$season[(vir.ret$Year == 2011 & vir.ret$Week >= 40) | (vir.ret$Year == 2012 & vir.ret$Week <= 20)] <- '2011-12'
vir.ret$season[(vir.ret$Year == 2012 & vir.ret$Week >= 40) | (vir.ret$Year == 2013 & vir.ret$Week <= 20)] <- '2012-13'
vir.ret$season[(vir.ret$Year == 2013 & vir.ret$Week >= 40) | (vir.ret$Year == 2014 & vir.ret$Week <= 20)] <- '2013-14'
vir.ret$season[(vir.ret$Year == 2014 & vir.ret$Week >= 40) | (vir.ret$Year == 2015 & vir.ret$Week <= 20)] <- '2014-15'
vir.ret$season[(vir.ret$Year == 2015 & vir.ret$Week >= 40) | (vir.ret$Year == 2016 & vir.ret$Week <= 20)] <- '2015-16'
vir.ret$season[(vir.ret$Year == 2016 & vir.ret$Week >= 40) | (vir.ret$Year == 2017 & vir.ret$Week <= 20)] <- '2016-17'
vir.ret$season <- factor(vir.ret$season)

vir.ret.red <- unique(m.ret[, c('country', 'season')])
vir.ret.red <- vir.ret.red[!(vir.ret.red$country %in% c('Australia', 'New Zealand', 'Chile')), ]
vir.ret.red$country <- factor(vir.ret.red$country)
tot.tested <- sapply(1:length(vir.ret.red$country), function(ix) {
  sum(vir.ret$SPEC_PROCESSED_NB[vir.ret$Country == vir.ret.red$country[ix] & vir.ret$season == vir.ret.red$season[ix]], na.rm = TRUE)
}, simplify = 'array')

vir.ret.red$tot.tested <- tot.tested
vir.ret.red$tot.tested <- as.numeric(as.character(vir.ret.red$tot.tested))

# now, break and move on to "current" - we'll join the two later

new.plus <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/data/FluNetWeek70.csv', header = FALSE)
colnames(new.plus) <- as.character(unlist(new.plus[3,])); new.plus <- new.plus[-c(1:3),]; rownames(new.plus) <- as.numeric(rownames(new.plus)) - 3
new.plus <- new.plus[,c(1,8:9)]; names(new.plus)[2:3] <- c('SP_RECEIVED', 'SP_PROCESSED')
new.plus$SP_PROCESSED <- as.numeric(as.character(new.plus$SP_PROCESSED)); new.plus$SP_RECEIVED <- as.numeric(as.character(new.plus$SP_RECEIVED))
new.plus <- new.plus[new.plus$Country %in% levels(vir.ret.red$country), ]; new.plus$Country <- factor(new.plus$Country)
new.plus$SP_PROCESSED[is.na(new.plus$SP_PROCESSED) & !is.na(new.plus$SP_RECEIVED)] <-
  new.plus$SP_RECEIVED[is.na(new.plus$SP_PROCESSED) & !is.na(new.plus$SP_RECEIVED)]
new.plus$SP_RECEIVED <- NULL

countries <- levels(new.plus$Country)
tot.tested <- sapply(1:length(countries), function(ix) {
  sum(new.plus$SP_PROCESSED[new.plus$Country == countries[ix]], na.rm = TRUE)
}, simplify = 'array')
new.plus <- as.data.frame(cbind(countries, tot.tested))
new.plus$season <- '2017-18'
names(new.plus)[1] <- 'country'

# finally, join, and add pop. sizes
vir.pop <- rbind(vir.ret.red, new.plus)
vir.pop$tot.tested <- as.numeric(as.character(vir.pop$tot.tested))

pop.sizes <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/popcounts.csv', header = F)
names(pop.sizes) <- c('country', 'size')

vir.pop <- merge(vir.pop, pop.sizes, by = 'country'); rm(pop.sizes)
vir.pop$size <- vir.pop$size * 1000

vir.pop$tot.test.prop <- vir.pop$tot.tested / vir.pop$size * 100000 # value per 100,000 pop
vir.pop <- vir.pop[, c(1:2, 5)]

boxplot(log(tot.test.prop) ~ country, data = vir.pop, las = 2)

vir.pop.ret <- vir.pop[vir.pop$season != '2017-18', ]
vir.pop <- vir.pop[vir.pop$season == '2017-18', ]

vir.pop.ret <- merge(vir.pop.ret, ret.pt.m, by = 'country'); vir.pop.ret <- merge(vir.pop.ret, ret.pi.m, by = 'country')
vir.pop <- merge(vir.pop, pt.m, by = 'country'); vir.pop <- merge(vir.pop, pi.m, by = 'country')

cor.test(vir.pop.ret$pt_acc, log(vir.pop.ret$tot.test.prop)) # sig; cor = 0.192
cor.test(vir.pop.ret$pi_acc, log(vir.pop.ret$tot.test.prop)) # sig; cor = 0.210

cor.test(vir.pop$pt_acc, log(vir.pop$tot.test.prop)) # not sig
cor.test(vir.pop$pi_acc, log(vir.pop$tot.test.prop)) # not sig

vir.pop <- rbind(vir.pop.ret, vir.pop)
cor.test(vir.pop$pt_acc, log(vir.pop$tot.test.prop)) # sig; cor = 0.134
cor.test(vir.pop$pi_acc, log(vir.pop$tot.test.prop)) # sig; cor = 0.158

m.ret.obs <- unique(m.ret[, c('country', 'season', 'obs_pkwk', 'obs_peak_int')])
m.obs <- unique(m[, c('country', 'obs_pkwk', 'obs_peak_int')]); m.obs$season <- '2017-18'
m.obs <- rbind(m.ret.obs, m.obs)

vir.pop <- merge(vir.pop, m.obs, by = c('country', 'season'))
cor.test(vir.pop$tot.test.prop, vir.pop$obs_pkwk) # not sig
cor.test(vir.pop$tot.test.prop, vir.pop$obs_peak_int) # not sig

###############################################################################################
###############################################################################################
###############################################################################################

# For 17-18 only: out of fcast weeks, how many weeks were reported on time?
on.time.mat <- matrix(NA, nrow = length(44:69), ncol = 44)
on.time.mat.no0 <- matrix(NA, nrow = length(44:69), ncol = 44)

for (fc_start in 44:69) {
  ili.temp <- read.csv(paste0('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/data/data_Week',fc_start,'.csv'))
  fc.dat <- tail(ili.temp, 1)[2:45]; colnames(fc.dat) <- NULL; fc.dat <- unlist(fc.dat)
  
  on.time <- ifelse(fc.dat < 0, 0, 1)
  on.time.no0 <- ifelse(fc.dat <= 0, 0, 1)
  
  on.time.mat[fc_start - 43, ] <- on.time
  on.time.mat.no0[fc_start - 43, ] <- on.time.no0
}

on.time.mat <- colSums(on.time.mat) / dim(on.time.mat)[1]
on.time.mat.no0 <- colSums(on.time.mat.no0) / dim(on.time.mat.no0)[1]

on.time <- as.data.frame(cbind(names(ili.temp)[2:45], on.time.mat, on.time.mat.no0))
colnames(on.time) <- c('country', 'X', 'X.0')
on.time$X <- as.numeric(as.character(on.time$X))
on.time$X.0 <- as.numeric(as.character(on.time$X.0))
on.time <- on.time[!(on.time$country %in% c('Sweden', 'United.Kingdom', 'Georgia', 'Canada',
                                            'Mexico', 'Morocco', 'United.States.of.America',
                                            'Switzerland')), ]
on.time$country <- factor(on.time$country)

# On-time w/o NA:
# mean: 0.733, median = 0.808
# max: 0.923 (), min: 0.077 (Kyrgyzstan)
# <50%: Belgium, Iceland, Kyrgyzstan
# >=50%: 91.7%; >75%: 63.9%
# >85%: Bulgaria, Greece, Netherlands, Poland, Portugal, Russia, Slovakia, Spain, Ukraine (25%)
# >90%: Portugal, Slovakia (5.6%)

# On-time w/o NA or 0:
# mean: 0.633, median: 0.692
# max: 0.885 (Russia/Spain), min: 0.038 (Kyrgyzstan)
# <50%: Belarus, Belgium, Czechia, Iceland, Kazakhstan, Kyrgyzstan, Uzbekistan (19.4%)
# >=50%: 80.6%; >75%: 27.8%
# >85%: Russia, Spain (5.6%)

# Are these values related to accuracy?
m <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/realtime_forecasts/results/outputMetrics_RT1718_onset.csv')
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$onset5) & !is.na(m$onsetObs), ]

levels(m.temp$country)[c(28, 30)] <- c('Republic.of.Moldova', 'Russian.Federation')

m.temp$accurate_pkwk <- as.numeric(m.temp$accurate_pkwk) - 1; m.temp$accurate_int <- as.numeric(m.temp$accurate_int) - 1

pt.m <- aggregate(m.temp$accurate_pkwk, by = list(m.temp$country), FUN = mean)
pi.m <- aggregate(m.temp$accurate_int, by = list(m.temp$country), FUN = mean)
names(pt.m) <- c('country', 'pt_acc'); names(pi.m) <- c('country', 'pi_acc')

on.time <- merge(on.time, pt.m, by = 'country'); on.time <- merge(on.time, pi.m, by = 'country')

cor.test(on.time$X, on.time$pt_acc)
cor.test(on.time$X, on.time$pi_acc)
cor.test(on.time$X.0, on.time$pt_acc)
cor.test(on.time$X.0, on.time$pi_acc)
# none sig

# What about if we remove values below 50%?
on.time <- on.time[on.time$X >= 0.5, ]
cor.test(on.time$X, on.time$pt_acc)
cor.test(on.time$X, on.time$pi_acc)

on.time <- on.time[on.time$X.0 >= 0.5, ]
cor.test(on.time$X.0, on.time$pt_acc)
cor.test(on.time$X.0, on.time$pi_acc)
# still none significant


###############################################################################################
###############################################################################################
###############################################################################################

