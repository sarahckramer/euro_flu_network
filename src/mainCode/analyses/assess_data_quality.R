
### Look at syndromic+ data quality by various metrices ###
# And compare with accuracy by country/subtype
library(PMCMR)

# Read in unscaled data:
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B.csv')

# Reduce and rename:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

iliiso.h1 <- iliiso.h1[, count.indices + 1]; names(iliiso.h1) <- countries
iliiso.h3 <- iliiso.h3[, count.indices + 1]; names(iliiso.h3) <- countries
iliiso.b <- iliiso.b[, count.indices + 1]; names(iliiso.b) <- countries

# Get season cutoffs:
pull.2009 <- 1:75
pull.1011 <- 76:107
pull.1112 <- 128:159
pull.1213 <- 180:211
pull.1314 <- 232:263
pull.1415 <- 284:315
pull.1516 <- 336:368
pull.1617 <- 389:420
pull.1718 <- 441:472
seasons.list <- list(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
rm(pull.2009, pull.1011, pull.1112, pull.1213, pull.1314, pull.1415, pull.1516, pull.1617, pull.1718)
for (i in 1:length(seasons.list)) {
  seasons.list[[i]] <- seasons.list[[i]] + 3
}; rm(i)
seasons.list[[1]] <- 1:78
seasons.list <- seasons.list[2:9]

# Also determine which country/season/subtypes have any onset:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 8, 30, 57)])
m <- m[!is.na(m$onsetObs5), ]
m$group <- paste(m$country, m$season, m$subtype, sep = '_'); m$group <- factor(m$group)

# Metric 1: Proportion of weeks within an outbreak for which no data were available:
seasons <- levels(m$season)
subtypes <- levels(m$subtype)

met1.dat <- NULL
for (country in countries) {
  for (season in seasons) {
   for (subtype in subtypes) {
     group.temp <- paste(country, season, subtype, sep = '_')
     
     if (group.temp %in% levels(m$group)) {
       
       if (subtype == 'A(H1)') {
         dat.temp <- iliiso.h1[seasons.list[[which(seasons == season)]], country]
       } else if (subtype == 'A(H3)') {
         dat.temp <- iliiso.h3[seasons.list[[which(seasons == season)]], country]
       } else if (subtype == 'B') {
         dat.temp <- iliiso.b[seasons.list[[which(seasons == season)]], country]
       } else {
         print('ERROR')
       }
       
       met1.dat <- rbind(met1.dat, c(country, season, subtype,
                                     length(which(is.na(dat.temp))) / length(dat.temp),
                                     length(which(is.na(dat.temp) | dat.temp == 0)) / length(dat.temp)))
     }
     
   } 
  }
}
rm(group.temp, dat.temp, country, season, subtype)
met1.dat <- as.data.frame(met1.dat)
names(met1.dat) <- c('country', 'season', 'subtype', 'prop.NA', 'prop.NA0')
met1.dat$prop.NA <- as.numeric(as.character(met1.dat$prop.NA))
met1.dat$prop.NA0 <- as.numeric(as.character(met1.dat$prop.NA0))

# Metric 2: Lag-one autocorrelation:
# Normally we would do this over whole data stream, but since some seasons don't have outbreaks... seems to make more sense to do by outbreak
met2.dat <- NULL
for (country in countries) {
  for (season in seasons) {
    for (subtype in subtypes) {
      group.temp <- paste(country, season, subtype, sep = '_')
      
      if (group.temp %in% levels(m$group)) {
        
        if (subtype == 'A(H1)') {
          dat.temp <- iliiso.h1[seasons.list[[which(seasons == season)]], country]
        } else if (subtype == 'A(H3)') {
          dat.temp <- iliiso.h3[seasons.list[[which(seasons == season)]], country]
        } else if (subtype == 'B') {
          dat.temp <- iliiso.b[seasons.list[[which(seasons == season)]], country]
        } else {
          print('ERROR')
        }

        met2.dat <- rbind(met2.dat, c(country, season, subtype,
                                      cor(dat.temp[-length(dat.temp)], dat.temp[-1], use = 'pairwise.complete.obs')))
      }
      
    } 
  }
}
rm(group.temp, dat.temp, country, season, subtype)
met2.dat <- as.data.frame(met2.dat)
names(met2.dat) <- c('country', 'season', 'subtype', 'smoothness')
met2.dat$smoothness <- as.numeric(as.character(met2.dat$smoothness))

# Save metrics:
met.dat <- merge(met1.dat, met2.dat, by = c('country', 'season', 'subtype'))
# write.csv(met.dat, file = 'results/supplemental/data_quality.csv', row.names = FALSE)
rm(met1.dat, met2.dat)

# Assess metrics by country/subtype:
# boxplot(prop.NA ~ country, data = met.dat)
# boxplot(prop.NA ~ season, data = met.dat)
# boxplot(prop.NA ~ subtype, data = met.dat)
# 
# boxplot(prop.NA0 ~ country, data = met.dat)
# boxplot(prop.NA0 ~ season, data = met.dat)
# boxplot(prop.NA0 ~ subtype, data = met.dat)
# 
# boxplot(smoothness ~ country, data = met.dat)
# boxplot(smoothness ~ season, data = met.dat)
# boxplot(smoothness ~ subtype, data = met.dat)

summary(met.dat)

kruskal.test(prop.NA ~ country, data = met.dat) # p = 1.391e-13
# kruskal.test(prop.NA0 ~ country, data = met.dat)
kruskal.test(smoothness ~ country, data = met.dat) # p = 2.244e-11
# all highly sig

kruskal.test(prop.NA ~ subtype, data = met.dat) # p = 0.002954
# kruskal.test(prop.NA0 ~ subtype, data = met.dat)
kruskal.test(smoothness ~ subtype, data = met.dat) # p = 0.4477
# only first line is sig

# kruskal.test(prop.NA ~ season, data = met.dat)
# kruskal.test(prop.NA0 ~ season, data = met.dat)
# kruskal.test(smoothness ~ season, data = met.dat)
# # none sig - good! we can ignore this

# cutoff: 0.05/66 = 0.000758; 66 + 66 + 3 = 135; 0.05/135 = 0.00037
posthoc.kruskal.nemenyi.test(prop.NA ~ country, data = met.dat) # DE vs. CZ, FR, IT; ES, HU, NL vs. IT (DE lower than CZ, FR, IT; IT higher than ES, HU, NL)
posthoc.kruskal.nemenyi.test(smoothness ~ country, data = met.dat) # LU vs. DE, ES, FR, NL (LU is sig lower than these - these are the only significant differences)
posthoc.kruskal.nemenyi.test(prop.NA ~ subtype, data = met.dat) # none below the above cutoff; but with cutoff 0.05/3=0.0167, B lower than both H1 (p0.0082) and H3(p0.0124)

cor.test(met.dat$prop.NA, met.dat$smoothness, method = 'kendall') # not sig (p = 0.06074, but borderline negative (-0.145) relationship)
# kendall: p = 0.06466, tau = -0.101

# Read in and format RMSE/log score results:
rmses <- read.csv('results/fits/rmse_COMB.csv')

# Read in and format d:
d1 <- read.csv('results/network/PROC_logScores_pt_ot.csv')
d2 <- read.csv('results/network/PROC_logScores_pi_bin.csv')
d1.isol <- read.csv('results/isolated/logScores_pt_ot.csv')
d2.isol <- read.csv('results/isolated/logScores_pi_bin.csv')
d1$model <- 'Network'; d2$model <- 'Network'; d1.isol$model <- 'Isolated'; d2.isol$model <- 'Isolated'
d1 <- rbind(d1, d1.isol); d2 <- rbind(d2, d2.isol); rm(d1.isol, d2.isol)
d1 <- d1[, c(1:4, 8, 10:17)]
d2 <- d2[, c(1:4, 8, 10:17)]
d <- rbind(d1, d2); rm(d1, d2)
d.ot <- d[d$metric == 'onset5', ]
d <- d[d$metric != 'onset5', ]
d <- d[, c(1:8, 10:13)]
d.ot <- d.ot[, c(1:7, 9:13)]
d.ot$leadpkwk_mean <- d.ot$leadonset5
names(d)[7:8] = names(d.ot)[7:8] = c('lead_mean', 'FWeek')
d <- rbind(d, d.ot); rm(d.ot)
d$metric <- factor(d$metric); d$model <- factor(d$model)
levels(d$metric) <- c('Onset Timing', 'Peak Timing', 'Peak Intensity')
d$metric <- factor(d$metric, levels = levels(d$metric)[c(2:3, 1)])
d$model <- factor(d$model, levels = levels(d$model)[2:1])

d.temp <- d[d$lead_mean >= -6 & d$lead_mean < 5 & !is.na(d$leadonset5), ]
d.net <- d.temp[d.temp$model == 'Network', ]
d.isol <- d.temp[d.temp$model == 'Isolated', ]

rmse.mean <- aggregate(rmse ~ country + season + subtype, data = rmses, FUN = mean)
log.PT.mean <- aggregate(score ~ country + season + subtype, data = d.net[d.net$metric == 'Peak Timing', ], FUN = mean)
log.PI.mean <- aggregate(score ~ country + season + subtype, data = d.net[d.net$metric == 'Peak Intensity', ], FUN = mean)
log.PT.mean.pre <- aggregate(score ~ country + season + subtype, data = d.net[d.net$metric == 'Peak Timing' & d.net$lead_mean < 0, ], FUN = mean)
log.PI.mean.pre <- aggregate(score ~ country + season + subtype, data = d.net[d.net$metric == 'Peak Intensity' & d.net$lead_mean < 0, ], FUN = mean)
log.PT.mean.ISOL <- aggregate(score ~ country + season + subtype, data = d.isol[d.isol$metric == 'Peak Timing', ], FUN = mean)
log.PI.mean.ISOL <- aggregate(score ~ country + season + subtype, data = d.isol[d.isol$metric == 'Peak Intensity', ], FUN = mean)
log.PT.mean.pre.ISOL <- aggregate(score ~ country + season + subtype, data = d.isol[d.isol$metric == 'Peak Timing' & d.isol$lead_mean < 0, ], FUN = mean)
log.PI.mean.pre.ISOL <- aggregate(score ~ country + season + subtype, data = d.isol[d.isol$metric == 'Peak Intensity' & d.isol$lead_mean < 0, ], FUN = mean)

# Merge all:
res.dat <- merge(met.dat, rmse.mean)
res.dat <- merge(res.dat, log.PT.mean, all = TRUE) # 4 missing values for country/season/subtypes where onset was never predicted
res.dat <- merge(res.dat, log.PI.mean, by = c('country', 'season', 'subtype'), all = TRUE)
names(res.dat)[8:9] <- c('score.PT', 'score.PI')
res.dat <- merge(res.dat, log.PT.mean.pre, by = c('country', 'season', 'subtype'), all = TRUE)
res.dat <- merge(res.dat, log.PI.mean.pre, by = c('country', 'season', 'subtype'), all = TRUE)
names(res.dat)[10:11] <- c('score.PT.pre', 'score.PI.pre')
res.dat <- merge(res.dat, log.PT.mean.ISOL, by = c('country', 'season', 'subtype'), all = TRUE)
res.dat <- merge(res.dat, log.PI.mean.ISOL, by = c('country', 'season', 'subtype'), all = TRUE)
names(res.dat)[12:13] <- c('score.PT.isol', 'score.PI.isol')
res.dat <- merge(res.dat, log.PT.mean.pre.ISOL, by = c('country', 'season', 'subtype'), all = TRUE)
res.dat <- merge(res.dat, log.PI.mean.pre.ISOL, by = c('country', 'season', 'subtype'), all = TRUE)
names(res.dat)[14:15] <- c('score.PT.pre.isol', 'score.PI.pre.isol')
rm(log.PT.mean, log.PI.mean, log.PT.mean.pre, log.PI.mean.pre, log.PT.mean.ISOL, log.PI.mean.ISOL, log.PT.mean.pre.ISOL, log.PI.mean.pre.ISOL, rmse.mean)

# Calculate difference between net and isol mean scores:
res.dat$diff.PT <- res.dat$score.PT - res.dat$score.PT.isol
res.dat$diff.PI <- res.dat$score.PI - res.dat$score.PI.isol
res.dat$diff.PT.pre <- res.dat$score.PT.pre - res.dat$score.PT.pre.isol
res.dat$diff.PI.pre <- res.dat$score.PI.pre - res.dat$score.PI.pre.isol

boxplot(diff.PT.pre ~ country, data = res.dat)

kruskal.test(diff.PT ~ country, data = res.dat) # not sig
kruskal.test(diff.PT.pre ~ country, data = res.dat) # not sig
kruskal.test(diff.PI ~ country, data = res.dat) # not sig
kruskal.test(diff.PI.pre ~ country, data = res.dat) # not sig

kruskal.test(diff.PT ~ subtype, data = res.dat) # not sig
kruskal.test(diff.PT.pre ~ subtype, data = res.dat) # not sig
kruskal.test(diff.PI ~ subtype, data = res.dat) # not sig
kruskal.test(diff.PI.pre ~ subtype, data = res.dat) # not sig

# Assess fit and forecast accuracy by metrics 1 and 2:
# 0.05 / ~20 = 0.0025
cor.test(res.dat$prop.NA, res.dat$rmse, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PT, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PT.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PI, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PI.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$diff.PT, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$diff.PT.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$diff.PI, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$diff.PI.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PT.isol, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PT.pre.isol, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PI.isol, data = res.dat, method = 'kendall')
cor.test(res.dat$prop.NA, res.dat$score.PI.pre.isol, data = res.dat, method = 'kendall')
# none are sig

cor.test(res.dat$smoothness, res.dat$rmse, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$score.PT, data = res.dat, method = 'kendall') # p = 0.09636
cor.test(res.dat$smoothness, res.dat$score.PT.pre, data = res.dat, method = 'kendall') # p = 0.07128
cor.test(res.dat$smoothness, res.dat$score.PI, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$score.PI.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$diff.PT, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$diff.PT.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$diff.PI, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$diff.PI.pre, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$score.PT.isol, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$score.PT.pre.isol, data = res.dat, method = 'kendall') # p = 0.01867; tau = 0.1241385
cor.test(res.dat$smoothness, res.dat$score.PI.isol, data = res.dat, method = 'kendall')
cor.test(res.dat$smoothness, res.dat$score.PI.pre.isol, data = res.dat, method = 'kendall')

