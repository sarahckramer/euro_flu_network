
### Look at syndromic+ data quality by various metrices ###
# And compare with accuracy by country

###############################################################################################
# First, read in metrics files:
a <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_TEMPERATE_new_FIN.csv')
a <- a[!(a$country == 'Mexico' & a$season == '2010-11') & !(a$country == 'Ukraine' & a$season == '2011-12'), ]
a.temp <- a[a$leadpkwk_mean >= -6 & a$leadpkwk_mean < 5 & !is.na(a$onset5) & !is.na(a$onsetObs5), ]

a.temp$accurate_pkwk <- as.numeric(a.temp$accurate_pkwk) - 1
a.temp$accurate_int <- as.numeric(a.temp$accurate_int) - 1

c <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/results/outputMet_trop_nohumid_CONT_FIN_seasons.csv')

##############################################################################################
# Determine where cutoffs are for each tropical season
seasons.Bangladesh <- list(1:31, 32:89)#
seasons.Bhutan <- list(1:5, 6:13, 14:41, 42:53, 54:69, 70:77, 78:123, 124:185, 186:211,
                       212:259, 260:290, 291:334)#
seasons.Bolivia <- list(1:35)#
seasons.Brazil <- list(1:27)#
seasons.Cambodia <- list(1:22, 23:61, 62:101)#
seasons.Colombia <- list(1:26, 27:55, 56:89, 90:179, 180:299)#
seasons.Cuba <- list(1:50, 51:102, 103:144)#
seasons.Ecuador <- list(1:65, 66:83, 84:129, 130:153, 154:290, 291:329)#
seasons.Honduras <- list(1:59, 60:164)#
seasons.Indonesia <- list(1:65, 66:112)#
seasons.Kenya <- list(1:14)#
seasons.Madagascar <- list(1:45, 46:78)#
seasons.Oman <- list(1:145, 146:189, 190:299)#
seasons.Pakistan <- list(1:24, 25:43, 44:61, 62:67, 68:82, 83:131, 132:184, 185:283)#
seasons.Paraguay <- list(1:33, 34:85, 86:139)#
seasons.Peru <- list(1:32, 33:109, 110:134)#
seasons.Singapore <- list(1:25, 26:309, 310:360)#
seasons.Thailand <- list(4:59, 60:112, 113:224, 225:255, 256:316, 317:346)#
# Note that these are from first data point, not from beginning
# So how to "translate" these?
# Change to remove dormant_ongoing phases from consideration:
# Cuba, Ecuador, Honduras, Indonesia, Kenya, Madagascar, Pakistan, Peru, Singapore
# actually, the dormant_ongoing thing only happens after these ranges?

ili <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/WHO_dat_ALL_05-12_NOPAN.csv')
# ili.trop <- ili[, c(1, 4, 7:9, 11, 14, 16, 19, 26, 29, 34, 39, 45:48, 55, 61)]
# for (i in 2:19) {
#   print(names(ili.trop)[i])
#   obs_i <- ili.trop[, i]
#   start.index <- 1
#   while(obs_i[start.index] < 1 | is.na(obs_i[start.index])) {
#     start.index <- start.index + 1
#   }
#   print(start.index) # first point at which not <1 or NA
# }

# We don't even necessarily have to do a by-season analysis for the tropics

# for (country in levels(c$country)) {
#   print(country)
#   print(summary(c$fc_start[c$country == country & c$season == 8 & !is.na(c$season)]))
#   print(unique(c$onsetObs[c$country == country & c$season == 8 & !is.na(c$season)]))
#   print(unique(c$obs_end[c$country == country & c$season == 8 & !is.na(c$season)]))
# }

source('/Users/sarahkramer/Dropbox/spatial_model/forecasts/code/functions/remove_seasons_tropical_FUNCTION.R')
c <- rem_season_trop(c, ili)

seasons.Bhutan <- list(1:5, 6:13, 42:53, 54:69, 70:77, 78:123, 124:185, 186:211,
                       212:259, 260:290, 291:334)#
seasons.Oman <- list(1:145, 146:189)#
seasons.Peru <- list(1:32, 110:134)#
seasons.Thailand <- list(4:59, 60:112, 113:224, 225:255, 317:346)#

seasons.Bangladesh <- unlist(seasons.Bangladesh) + 353 - 1
seasons.Bhutan <- unlist(seasons.Bhutan) + 109 - 1
seasons.Bolivia <- unlist(seasons.Bolivia) + 248 - 1
seasons.Brazil <- unlist(seasons.Brazil) + 405 - 1
seasons.Cambodia <- unlist(seasons.Cambodia) + 342 - 1
seasons.Colombia <- unlist(seasons.Colombia) + 144 - 1
seasons.Cuba <- unlist(seasons.Cuba) + 200 - 1
seasons.Ecuador <- unlist(seasons.Ecuador) + 102 - 1
seasons.Honduras <- unlist(seasons.Honduras) + 79 - 1
seasons.Indonesia <- unlist(seasons.Indonesia) + 313 - 1
seasons.Kenya <- unlist(seasons.Kenya) + 93 - 1
seasons.Madagascar <- unlist(seasons.Madagascar) + 353 - 1
seasons.Oman <- unlist(seasons.Oman) + 135 - 1
seasons.Pakistan <- unlist(seasons.Pakistan) + 79 - 1
seasons.Paraguay <- unlist(seasons.Paraguay) + 301 - 1
seasons.Peru <- unlist(seasons.Peru) + 306 - 1
seasons.Singapore <- unlist(seasons.Singapore) + 79 - 1
seasons.Thailand <- unlist(seasons.Thailand) + 97 - 1
##############################################################################################

c.temp <- c[c$leadpkwk_mean >= -6 & c$leadpkwk_mean < 5 & !is.na(c$leadpkwk_mean) & !is.na(c$onsetObs), ]
c.temp$accurate_pkwk <- as.numeric(c.temp$accurate_pkwk) - 1
c.temp$accurate_int <- as.numeric(c.temp$accurate_int) - 1

# Find aggregate pt/pi accuracy by country, and by country/season
# Or just by country?
pt.a <- aggregate(a.temp$accurate_pkwk, by = list(a.temp$country), FUN = mean)
pt.a.seas <- aggregate(a.temp$accurate_pkwk, by = list(a.temp$country, a.temp$season), FUN = mean)
pi.a <- aggregate(a.temp$accurate_int, by = list(a.temp$country), FUN = mean)
pi.a.seas <- aggregate(a.temp$accurate_int, by = list(a.temp$country, a.temp$season), FUN = mean)

pt.c <- aggregate(c.temp$accurate_pkwk, by = list(c.temp$country), FUN = mean)
# pt.c.seas <- aggregate(c.temp$accurate_pkwk, by = list(c.temp$country, c.temp$season), FUN = mean)
pi.c <- aggregate(c.temp$accurate_int, by = list(c.temp$country), FUN = mean)
# pi.c.seas <- aggregate(c.temp$accurate_int, by = list(c.temp$country, c.temp$season), FUN = mean)

# how are tropical seasons characterized? do temperate the same way?
# tropical seasons include all that aren't "dormant_ongoing"
# so tropical includes before onset and after end; for temp, just include weeks 40-20
print(summary(a.temp$fc_start[a.temp$hemisphere == 'north']))
print(summary(a.temp$fc_start[a.temp$hemisphere == 'south']))
# all are within influenza season timeframe

pt.a$lat <- 'Temp'; pi.a$lat <- 'Temp'#; pt.a.seas$lat <- 'Temp'; pi.a.seas$lat <- 'Temp'
pt.c$lat <- 'Trop'; pi.c$lat <- 'Trop'#; pt.c.seas$lat <- 'Trop'; pi.c.seas$lat <- 'Trop'
pt <- rbind(pt.a, pt.c); pi <- rbind(pi.a, pi.c)
# pt.a.seas$Group.2 <- as.character(pt.a.seas$Group.2)
# pi.a.seas$Group.2 <- as.character(pi.a.seas$Group.2)
pt.seas <- pt.a.seas; pi.seas <- pi.a.seas
rm(pt.a, pi.a, pt.a.seas, pi.a.seas, pt.c, pi.c)

names(pt) <- c('country', 'pt', 'lat')
names(pi) <- c('country', 'pi', 'lat')
names(pt.seas) <- c('country', 'season', 'pt')
names(pi.seas) <- c('country', 'season', 'pi')

acc <- merge(pt, pi, by = c('country', 'lat'))
acc.seas <- merge(pt.seas, pi.seas, by = c('country', 'season'))
rm(pt, pi, pt.seas, pi.seas)

acc$lat <- factor(acc$lat)#; acc.seas$lat <- factor(acc.seas$lat)
acc.seas$season <- factor(acc.seas$season)

levels(acc$country)[c(30, 34, 36, 44:45)] = levels(acc.seas$country)[c(30, 34, 36, 44:45)] =
  c('New.Zealand', 'Republic.of.Moldova', 'Russian.Federation', 'United.Kingdom',
    'United.States.of.America')

# Find number NA within season
# Can also analyze relationships separately for temperate and tropics
ili <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/WHO_dat_ALL_05-12_NOPAN.csv')
ili <- ili[, c(1:59, 61:66)]
ili$Mexico[1:130] <- NA; ili$Ukraine[131:182] <- NA
for (i in 2:65) {
  ili[, i][ili[, i] < 0] <- NA
}

seasons.north <- list(79:110, 131:162, 183:214, 235:266, 287:318, 339:371, 392:423)
seasons.south <- list(53:84, 105:136, 157:188, 209:240, 261:292, 313:344, 366:397)
seas.names <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17')

acc$prop.na <- NA; acc.seas$prop.na <- NA

for (country in levels(acc$country)) {
  
  if (acc$lat[acc$country == country] == 'Temp') { # temperate
    # Pull only seasonal data (so no out-of-season data included)
    ili.temp <- c()
    acc.temp <- acc.seas[acc.seas$country == country, ]
    seasons <- as.numeric(substr(as.vector(acc.temp$season), 7, 7))
    
    if (country %in% c('Australia', 'New.Zealand', 'Chile')) {
      for (seas in seasons) {
        ili.seas <- ili[seasons.south[[seas]], which(names(ili) == country)]
        ili.temp <- c(ili.temp, ili.seas)
        acc.seas$prop.na[acc.seas$country == country & acc.seas$season == seas.names[seas]] <-
          length(ili.seas[is.na(ili.seas)]) / length(ili.seas)
      }
      acc$prop.na[acc$country == country] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
      
    } else {
      for (seas in seasons) {
        ili.seas <- ili[seasons.north[[seas]], which(names(ili) == country)]
        ili.temp <- c(ili.temp, ili.seas)
        acc.seas$prop.na[acc.seas$country == country & acc.seas$season == seas.names[seas]] <-
          length(ili.seas[is.na(ili.seas)]) / length(ili.seas)
      }
      acc$prop.na[acc$country == country] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
    }
    
  }
}

ili.temp <- ili[seasons.Bangladesh, which(names(ili) == country)]
acc$prop.na[acc$country == 'Bangladesh'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Bhutan, which(names(ili) == country)]
acc$prop.na[acc$country == 'Bhutan'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Bolivia, which(names(ili) == country)]
acc$prop.na[acc$country == 'Bolivia'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Brazil, which(names(ili) == country)]
acc$prop.na[acc$country == 'Brazil'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Cambodia, which(names(ili) == country)]
acc$prop.na[acc$country == 'Cambodia'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Colombia, which(names(ili) == country)]
acc$prop.na[acc$country == 'Colombia'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Cuba, which(names(ili) == country)]
acc$prop.na[acc$country == 'Cuba'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Ecuador, which(names(ili) == country)]
acc$prop.na[acc$country == 'Ecuador'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Honduras, which(names(ili) == country)]
acc$prop.na[acc$country == 'Honduras'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Indonesia, which(names(ili) == country)]
acc$prop.na[acc$country == 'Indonesia'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Kenya, which(names(ili) == country)]
acc$prop.na[acc$country == 'Kenya'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Madagascar, which(names(ili) == country)]
acc$prop.na[acc$country == 'Madagascar'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Oman, which(names(ili) == country)]
acc$prop.na[acc$country == 'Oman'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Pakistan, which(names(ili) == country)]
acc$prop.na[acc$country == 'Pakistan'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Paraguay, which(names(ili) == country)]
acc$prop.na[acc$country == 'Paraguay'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Peru, which(names(ili) == country)]
acc$prop.na[acc$country == 'Peru'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Singapore, which(names(ili) == country)]
acc$prop.na[acc$country == 'Singapore'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)
ili.temp <- ili[seasons.Thailand, which(names(ili) == country)]
acc$prop.na[acc$country == 'Thailand'] <- length(ili.temp[is.na(ili.temp)]) / length(ili.temp)

# Correlation?
plot(acc$prop.na, acc$pt, pch = 20, col = acc$lat); plot(acc$prop.na, acc$pi, pch = 20, col = acc$lat)
cor.test(acc$prop.na, acc$pt, method = 'kendall') # not sig (p=0.2)
cor.test(acc$prop.na, acc$pi, method = 'kendall') # sig p = 0.004; tau = -0.246

# separate for temp and trop:
acc.temp <- acc[acc$lat == 'Temp', ]; acc.trop <- acc[acc$lat == 'Trop', ]
cor.test(acc.temp$prop.na, acc.temp$pt, method = 'kendall')
cor.test(acc.temp$prop.na, acc.temp$pi, method = 'kendall')
cor.test(acc.trop$prop.na, acc.trop$pt, method = 'kendall')
cor.test(acc.trop$prop.na, acc.trop$pi, method = 'kendall')
# none are significant anymore; so sig was just that tropics have more NA then temp?

# by season:
plot(acc.seas$prop.na, acc.seas$pt, pch = 20); plot(acc.seas$prop.na, acc.seas$pi, pch = 20)
cor.test(acc.seas$prop.na, acc.seas$pt, method = 'kendall') # not sig (p=0.2)
cor.test(acc.seas$prop.na, acc.seas$pi, method = 'kendall') # not sig (p=0.5)

# Is there an asymptote as fewer weeks are missing? When do we reach this?
# I'm not sure how to test this actually? PT max is 1.0, PI is 0.864
#   For countries; for individual seasons, can be 1.0 for both
# happens when prop.na 0 for PT, 0.05 (with 0 in second) for PI
# However, I don't think this necessarily implies an asymptote - # NA is not end all be all
# And doesn't lead to some required sampling:
# PT > 0.9 when NAs up to 16.58%; PI > 0.80 w/ up to 15.11% NA
# Can double-check with % pop sampled below to see if a "sufficient" sampling range emerges

###############################################################################################

### Smoothness ###
# We've already looked at this for past data overall, where smoothness is positively
# correlated with overall pt and pi accuracy; smoothness is also significantly lower
# for tropical than for temperate countries

# Calculate measure of data "smoothness" and add to results data frames
cors <- c()
for (i in 2:65) {
  x <- ili[, i]
  x[x < 0] <- NA
  cors <- c(cors, cor(x[-length(x)],x[-1], use = 'pairwise.complete.obs'))
}

smooth <- as.data.frame(matrix(c(names(ili)[-1], cors), ncol = 2))
smooth$V2 <- as.numeric(as.character(smooth$V2))
names(smooth) <- c('country', 'smoothness')
# levels(smooth$country)[c(42, 50, 52, 63:64)] <- c('New Zealand', 'Republic of Moldova',
#                                                   'Russian Federation', 'United Kingdom',
#                                                   'United States of America')

acc <- merge(acc, smooth, by = 'country')

plot(acc$smoothness, acc$pt, col = acc$lat, pch = 20)
plot(acc$smoothness, acc$pi, col = acc$lat, pch = 20)

acc.temp <- acc[acc$lat == 'Temp', ]; acc.trop <- acc[acc$lat == 'Trop', ]

cor.test(acc.temp$smoothness, acc.temp$pt, method = 'kendall')
cor.test(acc.temp$smoothness, acc.temp$pi, method = 'kendall') # sig
cor.test(acc.trop$smoothness, acc.trop$pt, method = 'kendall')
cor.test(acc.trop$smoothness, acc.trop$pi, method = 'kendall') # sig

###############################################################################################

# How many samples are tested throughout season, normalized by population size
countries.north <- c('Austria', 'Belarus', 'Belgium', 'Bulgaria', 'Croatia', 'Czechia',
                     'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece',
                     'Hungary', 'Iceland', 'Ireland', 'Israel', 'Italy', 'Kazakhstan',
                     'Kyrgyzstan', 'Latvia', 'Lithuania', 'Luxembourg', 'Netherlands', 'Norway',
                     'Poland', 'Portugal', 'Republic of Moldova', 'Romania', 'Russian Federation',
                     'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Turkey', 'Ukraine',
                     'United Kingdom', 'Uzbekistan', 'Canada', 'Mexico', 'Morocco',
                     'United States of America')
countries.south <- c('Australia', 'Chile', 'New Zealand')
countries.trop <- c('Bangladesh', 'Brazil', 'Colombia', 'Cuba', 'Ecuador', 'Honduras', 'Oman',
                    'Pakistan', 'Paraguay', 'Peru', 'Singapore', 'Thailand', 'Bhutan', 'Bolivia',
                    'Cambodia', 'Indonesia', 'Kenya', 'Madagascar')

vir.ret <- read.csv('/Users/sarahkramer/Desktop/Lab/spatial_transmission/WHO_data/data_4-12-18/vir_dat_temp.csv')
vir.ret <- vir.ret[vir.ret$Country != 'Switzerland', ]
vir.ret$Country <- factor(vir.ret$Country)

vir.ret <- vir.ret[vir.ret$Year >= 2010, ]
vir.ret <- vir.ret[!(vir.ret$Year == 2010 & vir.ret$Week < 14), ]

vir.ret.north <- vir.ret[vir.ret$Country %in% countries.north &
                           (vir.ret$Week >= 40 | vir.ret$Week <= 20), ]
vir.ret.north$Week[vir.ret.north$Year == 2010 & vir.ret.north$Week < 40] <- NA
vir.ret.north <- vir.ret.north[!is.na(vir.ret.north$Week), ]
vir.ret.south <- vir.ret[vir.ret$Country %in% countries.south &
                           (vir.ret$Week >= 14 & vir.ret$Week <= 46), ]
vir.ret.trop <- vir.ret[vir.ret$Country %in% countries.trop, ]

vir.ret <- rbind(vir.ret.north, vir.ret.south, vir.ret.trop)
rm(vir.ret.north, vir.ret.south, vir.ret.trop)

vir.ret <- vir.ret[, 1:4]

# might be a bit of a short cut, but remove where no data in final dataset
library(reshape2)
ili.df <- melt(ili)
ili.df <- ili.df[!is.na(ili.df$value), ]
ili.df$Year <- substr(ili.df$time, 1, 4)
ili.df$Week <- substr(ili.df$time, 6, 7)
ili.df <- ili.df[, c(2, 4:5)]
names(ili.df)[1] <- 'Country'
ili.df <- unique(ili.df)

vir.ret <- merge(vir.ret, ili.df, by = c('Country', 'Year', 'Week'))

# Now calculate the mean # of people tested each week
vir.avg <- aggregate(vir.ret$SPEC_PROCESSED_NB, by = list(vir.ret$Country), FUN = median)
names(vir.avg) <- c('country', 'tested')

pop.sizes <- read.csv('/Users/sarahkramer/Dropbox/spatial_model/forecasts/data/popcounts.csv', header = F)
names(pop.sizes) <- c('country', 'size')
levels(pop.sizes$country)[25] <- 'Bolivia'

vir.pop <- merge(vir.avg, pop.sizes, by = 'country'); rm(pop.sizes)
vir.pop$size <- vir.pop$size * 1000

vir.pop$tot.test.prop <- vir.pop$tested / vir.pop$size * 100000 # value per 100,000 pop per week
vir.pop$prop.seas <- vir.pop$tot.test.prop * 23 # about value per 100,000 per standard season length
vir.pop <- vir.pop[, c(1, 5)]

boxplot(log(prop.seas) ~ country, data = vir.pop, las = 2)

vir.pop <- merge(vir.pop, acc, by = 'country')
vir.pop$prop.na <- NULL

boxplot(log(prop.seas) ~ lat, data = vir.pop)
summary(aov(log(prop.seas) ~ lat, data = vir.pop)) # not sig

plot(log(vir.pop$prop.seas), vir.pop$pt, col = vir.pop$lat, pch = 20)
plot(log(vir.pop$prop.seas), vir.pop$pi, col = vir.pop$lat, pch = 20)

cor.test(vir.pop$prop.seas, vir.pop$pt, method = 'kendall') # not sig; p = 0.05
cor.test(vir.pop$prop.seas, vir.pop$pi, method = 'kendall') # sig; p = 0.03; tau = 0.191

vir.pop.temp <- vir.pop[vir.pop$lat == 'Temp', ]; vir.pop.trop <- vir.pop[vir.pop$lat == 'Trop', ]

cor.test(vir.pop.temp$prop.seas, vir.pop.temp$pt, method = 'kendall')
cor.test(vir.pop.temp$prop.seas, vir.pop.temp$pi, method = 'kendall')

cor.test(vir.pop.trop$prop.seas, vir.pop.trop$pt, method = 'kendall')
cor.test(vir.pop.trop$prop.seas, vir.pop.trop$pi, method = 'kendall')
# none sig

###############################################################################################







