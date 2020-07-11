### Some assessment of how well forecasts based purely on historical expectance perform ###
# Look at historical PT/PI/OT and simply assign probabilities that way (by country and subtype); then calculate log scores
library(PMCMR)

# Read in metrics file and reduce to desired columns:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 6, 8:9, 14, 30, 57)]) # 198 combinations

# # Set PT/PI to NA where no onset:
# m$obs_pkwk[is.na(m$onsetObs5)] <- NA
# m$obs_peak_int[is.na(m$onsetObs5)] <- NA

# Or else just remove where no onset, and only consider outbreaks?:
m <- m[!is.na(m$onsetObs5), ] # removes 29

# Scale peak intensity values:
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Loop through countries and subtypes to get prob. distribution of observed PT/PI/OT:
dat <- NULL
for (country in levels(m$country)) {
  for (strain in levels(m$subtype)) {
    m.base <- m[m$country == country & m$subtype == strain, ]
    
    for (season in unique(m.base$season)) {
      obs.pt <- m.base$obs_pkwk[m.base$season == season]
      obs.pi <- m.base$obs_peak_int[m.base$season == season]
      obs.ot <- m.base$onsetObs5[m.base$season == season]
      
      m.temp <- m.base[m.base$season != season, ]
      
      prob.pt <- length(m.temp$obs_pkwk[m.temp$obs_pkwk == obs.pt]) / length(m.temp$obs_pkwk)
      prob.ot <- length(m.temp$onsetObs5[m.temp$onsetObs5 == obs.ot]) / length(m.temp$onsetObs5)
      
      obs.pi.binned <- cut(obs.pi, c(seq(0, 14000, by = 500), 100000))
      pred.pi.binned <- cut(m.temp$obs_peak_int, c(seq(0, 14000, by = 500), 100000))
      prob.pi <- length(pred.pi.binned[obs.pi.binned == pred.pi.binned]) / length(pred.pi.binned)
      
      dat <- rbind(dat, cbind(country, strain, season, rbind(c('pt', log(prob.pt)), c('pi', log(prob.pi)), c('ot', log(prob.ot)))))
    }
    rm(obs.pt, obs.pi, obs.ot, m.base, m.temp, prob.pt, prob.ot, obs.pi.binned, pred.pi.binned, prob.pi)
    
  }
}
rm(country, strain, season)

dat <- as.data.frame(dat)
names(dat) <- c('country', 'subtype', 'season', 'metric', 'score')
dat$score <- as.numeric(as.character(dat$score))

# Set -Infs to -10:
dat$score[dat$score == -Inf & !is.na(dat$score)] <- -10

# Look at mean/median of scores:
summary(dat$score[dat$metric == 'pt']) # mean: -7.9494 # 129/169 are -10
summary(dat$score[dat$metric == 'pi']) # mean: -7.7910 # 127/169 are -10
summary(dat$score[dat$metric == 'ot']) # mean: -7.4501 # 119/169 are -10
# median -10 for all forecasts

# Scores by subtype:
for (metric in levels(dat$metric)) {
  for (strain in levels(dat$subtype)) {
    print(paste(strain, metric, sep = '_'))
    print(summary(dat$score[dat$subtype == strain & dat$metric == metric]))
    print('')
  }
}

kruskal.test(score ~ subtype, data = dat[dat$metric == 'pt', ])
kruskal.test(score ~ subtype, data = dat[dat$metric == 'pi', ])
kruskal.test(score ~ subtype, data = dat[dat$metric == 'ot', ])

posthoc.kruskal.nemenyi.test(score ~ subtype, data = dat[dat$metric == 'pi', ])

# Any patterns by country or season?:
kruskal.test(score ~ country, data = dat[dat$metric == 'pt', ])
kruskal.test(score ~ country, data = dat[dat$metric == 'pi', ]) # sig
kruskal.test(score ~ country, data = dat[dat$metric == 'ot', ]) # sig
kruskal.test(score ~ season, data = dat[dat$metric == 'pt', ])
kruskal.test(score ~ season, data = dat[dat$metric == 'pi', ])
kruskal.test(score ~ season, data = dat[dat$metric == 'ot', ])

boxplot(score ~ country, data = dat[dat$metric == 'pi', ])
boxplot(score ~ country, data = dat[dat$metric == 'ot', ])

posthoc.kruskal.nemenyi.test(score ~ country, data = dat[dat$metric == 'pi', ]) # none sig
posthoc.kruskal.nemenyi.test(score ~ country, data = dat[dat$metric == 'ot', ]) # none sig

# Do we allow in some way for predictions of no onset? Here, no - allow all forecasts to "predict an onset;" if no onset occurs, forecasts are removed from consideration anyway

rm(list = ls())
