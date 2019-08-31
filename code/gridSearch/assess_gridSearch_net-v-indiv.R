
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Save plots?:
outputPlots <- FALSE

# Read in all plotting code:
source('code/gridSearch/comp_netVsIndiv/plotting_functions.R')

# Read in and format metrics files:
source('code/gridSearch/comp_netVsIndiv/readIn_metrics.R')

# Plot overall PT, PI, and OT by PREDICTED lead week:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_byPred.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/by_pred.R')
  print(plots.by.pred)
}

# Plot overall PT, PI, and OT by OBSERVED lead week:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_byObs.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/by_obs.R')
  print(plots.by.obs)
}

# Plot MAEs:
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_MAE.pdf', width = 14, height = 9)
  source('code/gridSearch/comp_netVsIndiv/plot_MAE.R')
  dev.off()
} else {
  source('code/gridSearch/comp_netVsIndiv/plot_MAE.R')
}

# Read in all log scores files:
source('code/gridSearch/comp_netVsIndiv/readIn_logScores.R')

# Plot log scores for PT, PI, OT, 1-4 weeks, by PREDICTED lead week:
# Question: Should 1-4 weeks be plotted some other way?
# Question: Remove where obs are 0 for 1-4 weeks? Or where obs below some value?
# Question: Remove where no onset predicted before calculating these?
if (outputPlots) {
  pdf('code/gridSearch/plots/comp_logScores_byPred.pdf', width = 14, height = 9)
  byWeek <- 'Predicted'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  dev.off()
  pdf('code/gridSearch/plots/comp_logScores_byObs.pdf', width = 14, height = 9)
  byWeek <- 'Observed'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  dev.off()
} else {
  byWeek <- 'Predicted'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
  byWeek <- 'Observed'
  source('code/gridSearch/comp_netVsIndiv/plot_logScores.R')
}

# Plot calibration for PT, PI, OT, and 1-4 weeks:























### Check calibration (method 2):
# Start with PT:
library(Hmisc)
a.dist <- read.csv('code/gridSearch/outputs/outputDist_081919_PT.csv')

wtd.quantile.new <- function(x) {
  if (round(sum(x[, 2])) != 1) {
    print('ERROR 1')
  }
  
  if (any(round(x[, 2] * 300, 0) < 1)) {
    print('ERROR 2')
  }
  
  y <- wtd.quantile(x = x[, 1], weights = round(x[, 2] * 300, 0), probs = p)
  return(y)
}
p <- c(0.005, 0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.375, 0.625, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.995)

countries <- levels(a.dist$country) # note: different order than that in which forecasts run!
a <- read.csv('code/gridSearch/outputs/outputMet_081919_pro.csv')
a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk')]

a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_pkwk <- a.dist$bin - a.dist$obs_pkwk

a.pkwk <- a.dist[a.dist$leadpkwk_mean >= -8 & a.dist$leadpkwk_mean < 4 & !is.na(a.dist$leadpkwk_mean) & !is.na(a.dist$onset5) & !is.na(a.dist$onsetObs5), ]
# if it doesn't predict an onset, we're not interested in its predictions of peak timing
a.pkwk$leadpkwk_bin <- cut(a.pkwk$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

a.pkwk <- a.pkwk[a.pkwk$bin != -1, ]
a.pkwk <- a.pkwk[a.pkwk$value > 0, ] # don't need weeks with 0% confidence

a.pkwk$oev_base <- factor(a.pkwk$oev_base)
a.pkwk$oev_denom <- factor(a.pkwk$oev_denom)
a.pkwk$lambda <- factor(a.pkwk$lambda)
levels(a.pkwk$lambda)[1] <- '1.00'

dat.temp.all <- data.frame()
for (lead in levels(a.pkwk$leadpkwk_bin)) {
  print(lead)
  
  a.temp <- a.pkwk[a.pkwk$leadpkwk_bin == lead, ]# & a.pkwk$lambda == lam, ]
  c1 <- split(a.temp[, c('bin', 'value')], a.temp[, c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda')])
  c15 <- sapply(1:length(c1), function(ix) {
    unlist(c1[ix])
  })
  c1 <- c1[lapply(c15, length) > 0]
  c2 <- lapply(c1, wtd.quantile.new)
  c3 <- as.data.frame(unlist(c2))
  c35 <- matrix(unlist(strsplit(rownames(c3), split = '[.]')), nrow = length(c3$`unlist(c2)`), byrow = TRUE)
  c4 <- cbind(c3, c35)
  rownames(c4) <- NULL
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'), paste(c4[, 10], c4[, 11], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'quantile')
  c5 <- dcast(c4, country + season + run + fc_start + oev_base + oev_denom + lambda ~ quantile, value.var = 'value')
  # a.new <- merge(c5, unique(a.temp[, c(1:7, 11)]), by = c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
  a.new <- merge(c5, unique(a.temp[, c(1:2, 11)]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
    for (lam in unique(a.new$lambda)) {
      a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
      # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
      denom <- length(a.new.temp$country)
      
      if (denom > 0) {
        
        p25 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`37.5%` & a.new.temp$obs_pkwk <= a.new.temp$`62.5%`)
        p50 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`25.0%` & a.new.temp$obs_pkwk <= a.new.temp$`75.0%`)
        p60 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`20.0%` & a.new.temp$obs_pkwk <= a.new.temp$`80.0%`)
        p70 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`15.0%` & a.new.temp$obs_pkwk <= a.new.temp$`85.0%`)
        p80 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`10.0%` & a.new.temp$obs_pkwk <= a.new.temp$`90.0%`)
        p90 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 5.0%` & a.new.temp$obs_pkwk <= a.new.temp$`95.0%`)
        p95 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 2.5%` & a.new.temp$obs_pkwk <= a.new.temp$`97.5%`)
        p99 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 0.5%` & a.new.temp$obs_pkwk <= a.new.temp$`99.5%`)
        
        y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
        
        # also calculate RMSE for lead/oev/lambda combo:
        rmse <- sqrt(sum((y - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        # dat.temp <- as.data.frame(cbind(c(25, 50, 80, 90, 95, 99), y, rep(lead, 6), rep(rmse, 6), rep(denom, 6), rep(o1, 6), rep(o2, 6), rep(lam, 6)))
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8)))
        dat.temp.all <- rbind(dat.temp.all, dat.temp)
        
      }
      
    }
    # }
  }
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.pt.temp <- dat.temp.all
dat.pt.temp$metric <- 'Peak Timing'

names(dat.pt.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'metric')
dat.pt.temp$group <- paste(dat.pt.temp$lead, dat.pt.temp$oev_base, dat.pt.temp$lambda, sep = '_'); dat.pt.temp$group <- factor(dat.pt.temp$group)

p1 <- ggplot(data = dat.pt.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = 'Peak Timing') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p1

# Now do OT:
a.dist <- read.csv('code/gridSearch/outputs/outputDist_081919_OT.csv')
a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_onwk <- a.dist$bin - a.dist$onsetObs5
a.dist$leadonwk_mean <- a.dist$fc_start - a.dist$onset5

a.onwk <- a.dist[a.dist$leadonwk_mean >= -6 & a.dist$leadonwk_mean < 4 & !is.na(a.dist$leadonwk_mean) & !is.na(a.dist$onset5) & !is.na(a.dist$onsetObs5), ]
a.onwk$leadonwk_bin <- cut(a.onwk$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

a.onwk <- a.onwk[a.onwk$bin != -1, ] # can only consider the weighted quantiles among ensemble members that actually predict an onset
a.onwk <- a.onwk[a.onwk$value > 0, ] # don't need weeks with 0% confidence

a.onwk$oev_base <- factor(a.onwk$oev_base)
a.onwk$oev_denom <- factor(a.onwk$oev_denom)
a.onwk$lambda <- factor(a.onwk$lambda)
levels(a.onwk$lambda)[1] <- '1.00'

wtd.quantile.onset <- function(x) {
  # if (round(sum(x[, 2])) != 1) {
  #   print('ERROR 1')
  # }
  
  if (any(round(x[, 2] * 300, 0) < 1)) {
    print('ERROR 2')
  }
  
  tot.mem <- round(sum(x[, 2]) * 300, 0)
  
  y <- wtd.quantile(x = x[, 1], weights = round(x[, 2] * tot.mem, 0), probs = p) # does this actually make any difference?
  return(y)
}

dat.temp.all <- data.frame()
for (lead in levels(a.onwk$leadonwk_bin)) {
  print(lead)
  
  a.temp <- a.onwk[a.onwk$leadonwk_bin == lead, ]
  c1 <- split(a.temp[, c('bin', 'value')], a.temp[, c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda')])
  c15 <- sapply(1:length(c1), function(ix) {
    unlist(c1[ix])
  })
  c1 <- c1[lapply(c15, length) > 0]
  c2 <- lapply(c1, wtd.quantile.onset)
  c3 <- as.data.frame(unlist(c2))
  c35 <- matrix(unlist(strsplit(rownames(c3), split = '[.]')), nrow = length(c3$`unlist(c2)`), byrow = TRUE)
  c4 <- cbind(c3, c35)
  rownames(c4) <- NULL
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'), paste(c4[, 10], c4[, 11], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'quantile')
  c5 <- dcast(c4, country + season + run + fc_start + oev_base + oev_denom + lambda ~ quantile, value.var = 'value')
  a.new <- merge(c5, unique(a.temp[, c(1:2, 13)]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
    for (lam in unique(a.new$lambda)) {
      a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
      # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
      
      denom <- length(a.new.temp$country)
      
      if (denom > 0) {
        
        p25 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`37.5%` & a.new.temp$onsetObs5 <= a.new.temp$`62.5%`)
        p50 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`25.0%` & a.new.temp$onsetObs5 <= a.new.temp$`75.0%`)
        p60 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`20.0%` & a.new.temp$onsetObs5 <= a.new.temp$`80.0%`)
        p70 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`15.0%` & a.new.temp$onsetObs5 <= a.new.temp$`85.0%`)
        p80 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`10.0%` & a.new.temp$onsetObs5 <= a.new.temp$`90.0%`)
        p90 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 5.0%` & a.new.temp$onsetObs5 <= a.new.temp$`95.0%`)
        p95 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 2.5%` & a.new.temp$onsetObs5 <= a.new.temp$`97.5%`)
        p99 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 0.5%` & a.new.temp$onsetObs5 <= a.new.temp$`99.5%`)
        
        y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
        
        # also calculate RMSE for lead/oev/lambda combo:
        rmse <- sqrt(sum((y - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8)))
        dat.temp.all <- rbind(dat.temp.all, dat.temp)
        
      }
      
    }
    # }
  }
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.ot.temp <- dat.temp.all
dat.ot.temp$metric <- 'Onset Timing'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'metric')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)

p2 <- ggplot(data = dat.ot.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = 'Onset Timing') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p2

# Finally, PI:
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081919_PI.csv')
a.int <- a.dist

a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'scaling', 'fc_start', 'obs_peak_int', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk')]
a.int <- merge(a.int, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.int[, c(9:308)] <- a.int[, c(9:308)] / a.int[, 309]
a.int$scaling <- NULL

a.int <- a.int[a.int$leadpkwk_mean >= -8 & a.int$leadpkwk_mean < 4 & !is.na(a.int$leadpkwk_mean) & !is.na(a.int$onset5) & !is.na(a.int$onsetObs5),]
a.int$leadpkwk_bin <- cut(a.int$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

a.int$oev_base <- factor(a.int$oev_base)
a.int$oev_denom <- factor(a.int$oev_denom)
a.int$lambda <- factor(a.int$lambda)
levels(a.int$lambda)[1] <- '1.00'

dat.temp.all <- data.frame()
for (lead in levels(a.int$leadpkwk_bin)) {
  print(lead)
  
  a.temp <- a.int[a.int$leadpkwk_bin == lead, ]
  len <- length(a.temp$country)
  
  if (len > 0) {
    a.new <- sapply(1:length(a.temp$leadpkwk_mean), function(ix) {
      quantile(a.temp[ix, 9:308], probs = p)
    })
    a.new <- t(a.new); a.new <- cbind(a.temp, a.new); a.new <- a.new[, -c(9:308)]
    
    
    for (o1 in levels(a.temp$oev_base)) {
      # for (o2 in levels(a.temp$oev_denom)) {
      for (lam in levels(a.temp$lambda)) {
        a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
        denom <- length(a.new.temp$country)
        
        if (denom > 0) {
          p25 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`37.5%` & a.new.temp$obs_peak_int <= a.new.temp$`62.5%`)
          p50 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`25%` & a.new.temp$obs_peak_int <= a.new.temp$`75%`)
          p60 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`20%` & a.new.temp$obs_peak_int <= a.new.temp$`80%`)
          p70 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`15%` & a.new.temp$obs_peak_int <= a.new.temp$`85%`)
          p80 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`10%` & a.new.temp$obs_peak_int <= a.new.temp$`90%`)
          p90 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`5%` & a.new.temp$obs_peak_int <= a.new.temp$`95%`)
          p95 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`2.5%` & a.new.temp$obs_peak_int <= a.new.temp$`97.5%`)
          p99 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`0.5%` & a.new.temp$obs_peak_int <= a.new.temp$`99.5%`)
          
          y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
          rmse <- sqrt(sum((c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
          
          dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8)))
          dat.temp.all <- rbind(dat.temp.all, dat.temp)
        }
        
      }
      # }
    }
    
  }
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.pi.temp <- dat.temp.all
dat.pi.temp$metric <- 'Peak Intensity'

names(dat.pi.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'metric')
dat.pi.temp$group <- paste(dat.pi.temp$lead, dat.pi.temp$oev_base, dat.pi.temp$lambda, sep = '_'); dat.pi.temp$group <- factor(dat.pi.temp$group)

p3 <- ggplot(data = dat.pi.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = 'Peak Intensity') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p3

# grid.arrange(p1, p3, p2, ncol = 1)
print(p1)
print(p3)
print(p2)

# Can we also look at calibration for the 1-4 week ahead predictions?:
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081919_1wk.csv')
a1 <- a.dist
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081919_2wk.csv')
a2 <- a.dist
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081919_3wk.csv')
a3 <- a.dist
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081919_4wk.csv')
a4 <- a.dist

a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'scaling', 'fc_start', 'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk')]
a1 <- merge(a1, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
a2 <- merge(a2, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
a3 <- merge(a3, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))
a4 <- merge(a4, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a1[, c(9:308)] <- a1[, c(9:308)] / a1[, 309]; a1$scaling <- NULL
a2[, c(9:308)] <- a2[, c(9:308)] / a2[, 309]; a2$scaling <- NULL
a3[, c(9:308)] <- a3[, c(9:308)] / a3[, 309]; a3$scaling <- NULL
a4[, c(9:308)] <- a4[, c(9:308)] / a4[, 309]; a4$scaling <- NULL

# remove where current value is 0? (or NA):
a1 <- a1[!is.na(a1$obs_1week) & a1$obs_1week > 0, ]
a2 <- a2[!is.na(a2$obs_2week) & a2$obs_2week > 0, ]
a3 <- a3[!is.na(a3$obs_3week) & a3$obs_3week > 0, ]
a4 <- a4[!is.na(a4$obs_4week) & a4$obs_4week > 0, ]

a1 <- a1[a1$leadpkwk_mean >= -8 & a1$leadpkwk_mean < 4 & !is.na(a1$leadpkwk_mean) & !is.na(a1$onsetObs5),]
a1$leadpkwk_bin <- cut(a1$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
a2 <- a2[a2$leadpkwk_mean >= -8 & a2$leadpkwk_mean < 4 & !is.na(a2$leadpkwk_mean) & !is.na(a2$onsetObs5),]
a2$leadpkwk_bin <- cut(a2$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
a3 <- a3[a3$leadpkwk_mean >= -8 & a3$leadpkwk_mean < 4 & !is.na(a3$leadpkwk_mean) & !is.na(a3$onsetObs5),]
a3$leadpkwk_bin <- cut(a3$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
a4 <- a4[a4$leadpkwk_mean >= -8 & a4$leadpkwk_mean < 4 & !is.na(a4$leadpkwk_mean) & !is.na(a4$onsetObs5),]
a4$leadpkwk_bin <- cut(a4$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

# # or: also remove where no onset predicted?
# a1 <- a1[a1$leadpkwk_mean >= -8 & a1$leadpkwk_mean < 4 & !is.na(a1$leadpkwk_mean) & !is.na(a1$onset5) & !is.na(a1$onsetObs5),]
# a1$leadpkwk_bin <- cut(a1$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
# a2 <- a2[a2$leadpkwk_mean >= -8 & a2$leadpkwk_mean < 4 & !is.na(a2$leadpkwk_mean) & !is.na(a2$onset5) & !is.na(a2$onsetObs5),]
# a2$leadpkwk_bin <- cut(a2$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
# a3 <- a3[a3$leadpkwk_mean >= -8 & a3$leadpkwk_mean < 4 & !is.na(a3$leadpkwk_mean) & !is.na(a3$onset5) & !is.na(a3$onsetObs5),]
# a3$leadpkwk_bin <- cut(a3$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
# a4 <- a4[a4$leadpkwk_mean >= -8 & a4$leadpkwk_mean < 4 & !is.na(a4$leadpkwk_mean) & !is.na(a4$onset5) & !is.na(a4$onsetObs5),]
# a4$leadpkwk_bin <- cut(a4$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

a1$oev_base <- factor(a1$oev_base)
a1$oev_denom <- factor(a1$oev_denom)
a1$lambda <- factor(a1$lambda)
levels(a1$lambda)[1] <- '1.00'

a2$oev_base <- factor(a2$oev_base)
a2$oev_denom <- factor(a2$oev_denom)
a2$lambda <- factor(a2$lambda)
levels(a2$lambda)[1] <- '1.00'

a3$oev_base <- factor(a3$oev_base)
a3$oev_denom <- factor(a3$oev_denom)
a3$lambda <- factor(a3$lambda)
levels(a3$lambda)[1] <- '1.00'

a4$oev_base <- factor(a4$oev_base)
a4$oev_denom <- factor(a4$oev_denom)
a4$lambda <- factor(a4$lambda)
levels(a4$lambda)[1] <- '1.00'

dat.in.list <- list(a1, a2, a3, a4)

dat.temp.all <- data.frame()
for (fcast_wk in 1:4) {
  a.int <- dat.in.list[[fcast_wk]]
  
  if (fcast_wk == 1) {
    print(1)
    names(a.int)[309] <- 'obs_wk'
  } else if (fcast_wk == 2) {
    print(2)
    names(a.int)[310] <- 'obs_wk'
  } else if (fcast_wk == 3) {
    print(3)
    names(a.int)[311] <- 'obs_wk'
  } else if (fcast_wk == 4) {
    print(4)
    names(a.int)[312] <- 'obs_wk'
  }
  
  for (lead in levels(a.int$leadpkwk_bin)) {
    print(lead)
    
    a.temp <- a.int[a.int$leadpkwk_bin == lead, ]
    len <- length(a.temp$country)
    
    if (len > 0) {
      a.new <- sapply(1:length(a.temp$leadpkwk_mean), function(ix) {
        quantile(a.temp[ix, 9:308], probs = p)
      })
      a.new <- t(a.new); a.new <- cbind(a.temp, a.new); a.new <- a.new[, -c(9:308)]
      
      for (o1 in levels(a.temp$oev_base)) {
        for (lam in levels(a.temp$lambda)) {
          a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
          denom <- length(a.new.temp$country)
          
          if (denom > 0) {
            p25 <- sum(a.new.temp$obs_wk >= a.new.temp$`37.5%` & a.new.temp$obs_wk <= a.new.temp$`62.5%`)
            p50 <- sum(a.new.temp$obs_wk >= a.new.temp$`25%` & a.new.temp$obs_wk <= a.new.temp$`75%`)
            p60 <- sum(a.new.temp$obs_wk >= a.new.temp$`20%` & a.new.temp$obs_wk <= a.new.temp$`80%`)
            p70 <- sum(a.new.temp$obs_wk >= a.new.temp$`15%` & a.new.temp$obs_wk <= a.new.temp$`85%`)
            p80 <- sum(a.new.temp$obs_wk >= a.new.temp$`10%` & a.new.temp$obs_wk <= a.new.temp$`90%`)
            p90 <- sum(a.new.temp$obs_wk >= a.new.temp$`5%` & a.new.temp$obs_wk <= a.new.temp$`95%`)
            p95 <- sum(a.new.temp$obs_wk >= a.new.temp$`2.5%` & a.new.temp$obs_wk <= a.new.temp$`97.5%`)
            p99 <- sum(a.new.temp$obs_wk >= a.new.temp$`0.5%` & a.new.temp$obs_wk <= a.new.temp$`99.5%`)
            
            y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
            rmse <- sqrt(sum((c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
            
            dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8), rep(fcast_wk, 8)))
            dat.temp.all <- rbind(dat.temp.all, dat.temp)
          }
          
        }
      }
      
    }
    
  }
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.wk.temp <- dat.temp.all

names(dat.wk.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'metric')
dat.wk.temp$group <- paste(dat.wk.temp$lead, dat.wk.temp$oev_base, dat.wk.temp$lambda, sep = '_'); dat.wk.temp$group <- factor(dat.wk.temp$group)

p1 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 1, ], aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = '1 Week Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p2 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 2, ], aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = '2 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p3 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 3, ], aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = '3 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p4 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 4, ], aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = '4 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
print(p1); print(p2); print(p3); print(p4)










### Check inferred param. values at each time step as well (1e4 normally is a little unrealistic):
o <- read.csv('code/gridSearch/outputs/outputOPParams_081919.csv')
o$group <- paste(o$oev_base, o$oev_denom, o$lambda, o$season, o$run, o$fc_start, sep = '_')
o$group <- factor(o$group)
o$oev_base <- factor(o$oev_base)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'L') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'D') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'R0max') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mn, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'R0min') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'airScale') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

# p1 <- ggplot(data = o[o$oev_base == 1e4, ]) + geom_line(aes(x = week, y = airScale, group = group)) +
#   theme_classic() + labs(x = 'Week', y = 'airScale') + facet_grid(lambda ~ oev_denom, scales = 'free_y')
# print(p1)

dev.off()




