
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra); library(Hmisc)

# Read in and compile metrics files:
source('code/compareModels/readIn_metrics.R')

# Read in and format dist/ens files:
source('code/compareModels/formatDist.R')

# Read in plotting functions:
source('code/gridSearch/comp_netVsIndiv/plotting_functions.R')

# Get countries:
countries <- levels(dat.list.pred[[1]]$country)

# Set probabilities:
p <- c(0.005, 0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.375, 0.625, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.995)

### Plotting ###
dat.temp.pt = dat.temp.pi = dat.temp.ot = dat.temp.wks = data.frame()
# dat.temp1 = dat.temp2 = dat.temp3 = dat.temp4 = data.frame()
lead.vals <- unique(dat.list.pred[[1]]$leadpkwk_bin); lead.vals.onset <- unique(dat.list.pred[[3]]$leadonwk_bin)

for (lead in lead.vals) {
  print(lead)
  d.temp.pt <- process_Lead(lead, dat.list.pred[[1]], 'pt')
  d.temp.pi <- dat.list.pred[[2]]; d.temp.pi <- d.temp.pi[d.temp.pi$leadpkwk_bin == lead, ]
  # len <- length(d.temp.pi$country)
  if (lead %in% lead.vals.onset) {
    d.temp.ot <- process_Lead(lead, dat.list.pred[[3]], 'ot')
  }
  
  d.temp1 <- dat.list.pred[[4]]; d.temp1 <- d.temp1[d.temp1$leadpkwk_bin == lead, ]
  d.temp2 <- dat.list.pred[[5]]; d.temp2 <- d.temp2[d.temp2$leadpkwk_bin == lead, ]
  d.temp3 <- dat.list.pred[[6]]; d.temp3 <- d.temp3[d.temp3$leadpkwk_bin == lead, ]
  d.temp4 <- dat.list.pred[[7]]; d.temp4 <- d.temp4[d.temp4$leadpkwk_bin == lead, ]
  
  len.pi <- length(d.temp.pi$country)
  if (len.pi > 0) {
    d.new.pi <- sapply(1:length(d.temp.pi$leadpkwk_mean), function(ix) {
      quantile(d.temp.pi[ix, 10:309], probs = p)
    })
    d.new.pi <- t(d.new.pi); d.new.pi <- cbind(d.temp.pi, d.new.pi); d.new.pi <- d.new.pi[, -c(10:309)]
  }
  
  len1 <- length(d.temp1$country); len2 <- length(d.temp2$country); len3 <- length(d.temp3$country); len4 <- length(d.temp4$country)
  if (len1 > 0) {
    d.new1 <- sapply(1:length(d.temp1$leadpkwk_mean), function(ix) {
      quantile(d.temp1[ix, 10:309], probs = p)
    })
    d.new1 <- t(d.new1); d.new1 <- cbind(d.temp1, d.new1); d.new1 <- d.new1[, -c(10:309)]
  }
  if (len2 > 0) {
    d.new2 <- sapply(1:length(d.temp2$leadpkwk_mean), function(ix) {
      quantile(d.temp2[ix, 10:309], probs = p)
    })
    d.new2 <- t(d.new2); d.new2 <- cbind(d.temp2, d.new2); d.new2 <- d.new2[, -c(10:309)]
  }
  if (len3 > 0) {
    d.new3 <- sapply(1:length(d.temp3$leadpkwk_mean), function(ix) {
      quantile(d.temp3[ix, 10:309], probs = p)
    })
    d.new3 <- t(d.new3); d.new3 <- cbind(d.temp3, d.new3); d.new3 <- d.new3[, -c(10:309)]
  }
  if (len4 > 0) {
    d.new4 <- sapply(1:length(d.temp4$leadpkwk_mean), function(ix) {
      quantile(d.temp4[ix, 10:309], probs = p)
    })
    d.new4 <- t(d.new4); d.new4 <- cbind(d.temp4, d.new4); d.new4 <- d.new4[, -c(10:309)]
  }
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (model.type in unique(d.temp.pt$model)) {
    for (o1 in unique(d.temp.pt$oev_base)) {
      # Get reduced data:
      d.temp.pt2 <- d.temp.pt[d.temp.pt$model == model.type & d.temp.pt$oev_base == o1, ]
      denom.pt <- length(d.temp.pt2$country)
      if (len.pi > 0) {
        d.temp.pi2 <- d.new.pi[d.new.pi$model == model.type & d.new.pi$oev_base == o1, ]
        denom.pi <- length(d.temp.pi2$country)
      } else {
        d.temp.pi2 <- NULL
        denom.pi <- 0
      }
      if (lead %in% lead.vals.onset) {
        d.temp.ot2 <- d.temp.ot[d.temp.ot$model == model.type & d.temp.ot$oev_base == o1, ]
        denom.ot <- length(d.temp.ot2$country)
      }
      
      if (len1 > 0) {
        d.temp1.red <- d.new1[d.new1$model == model.type & d.new1$oev_base == o1, ]
        denom1 <- length(d.temp1.red$country)
      } else {
        d.temp1.red <- NULL
        denom1 <- 0
      }
      if (len2 > 0) {
        d.temp2.red <- d.new2[d.new2$model == model.type & d.new2$oev_base == o1, ]
        denom2 <- length(d.temp2.red$country)
      } else {
        d.temp2.red <- NULL
        denom2 <- 0
      }
      if (len3 > 0) {
        d.temp3.red <- d.new3[d.new3$model == model.type & d.new3$oev_base == o1, ]
        denom3 <- length(d.temp3.red$country)
      } else {
        d.temp3.red <- NULL
        denom3 <- 0
      }
      if (len4 > 0) {
        d.temp4.red <- d.new4[d.new4$model == model.type & d.new4$oev_base == o1, ]
        denom4 <- length(d.temp4.red$country)
      } else {
        d.temp4.red <- NULL
        denom4 <- 0
      }
      
      # Calculate calibration:
      if (denom.pt > 0) {
        dat.temp <- get_calibration_Dist(d.temp.pt2, 'obs_pkwk', denom.pt, lead, o1, lam = 1.02, model.type)
        dat.temp.pt <- rbind(dat.temp.pt, dat.temp)
      }
      
      if (denom.pi > 0) {
        dat.temp <- get_calibration_Dist_int(d.temp.pi2, 'obs_peak_int', denom.pi, lead, o1, lam = 1.02, model.type)
        dat.temp.pi <- rbind(dat.temp.pi, dat.temp)
      }
      
      if (denom.ot > 0) {
        if (lead %in% lead.vals.onset) {
          dat.temp <- get_calibration_Dist(d.temp.ot2, 'onsetObs5', denom.ot, lead, o1, lam = 1.02, model.type)
          dat.temp.ot <- rbind(dat.temp.ot, dat.temp)
        }
      }
      
      if (denom1 > 0) {
        p25 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`37.5%` & d.temp1.red$obs_1week <= d.temp1.red$`62.5%`)
        p50 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`25%` & d.temp1.red$obs_1week <= d.temp1.red$`75%`)
        p60 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`20%` & d.temp1.red$obs_1week <= d.temp1.red$`80%`)
        p70 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`15%` & d.temp1.red$obs_1week <= d.temp1.red$`85%`)
        p80 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`10%` & d.temp1.red$obs_1week <= d.temp1.red$`90%`)
        p90 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`5%` & d.temp1.red$obs_1week <= d.temp1.red$`95%`)
        p95 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`2.5%` & d.temp1.red$obs_1week <= d.temp1.red$`97.5%`)
        p99 <- sum(d.temp1.red$obs_1week >= d.temp1.red$`0.5%` & d.temp1.red$obs_1week <= d.temp1.red$`99.5%`)
        
        y <- c(p25 / denom1, p50 / denom1, p60 / denom1, p70 / denom1, p80 / denom1, p90 / denom1, p95 / denom1, p99 / denom1) * 100
        rmse <- sqrt(sum((c(p25 / denom1, p50 / denom1, p60 / denom1, p70 / denom1, p80 / denom1, p90 / denom1, p95 / denom1, p99 / denom1) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom1, 8), rep(model.type, 8), rep(o1, 8), rep(1, 8)))
        dat.temp.wks <- rbind(dat.temp.wks, dat.temp)
      }
      if (denom2 > 0) {
        p25 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`37.5%` & d.temp2.red$obs_2week <= d.temp2.red$`62.5%`)
        p50 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`25%` & d.temp2.red$obs_2week <= d.temp2.red$`75%`)
        p60 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`20%` & d.temp2.red$obs_2week <= d.temp2.red$`80%`)
        p70 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`15%` & d.temp2.red$obs_2week <= d.temp2.red$`85%`)
        p80 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`10%` & d.temp2.red$obs_2week <= d.temp2.red$`90%`)
        p90 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`5%` & d.temp2.red$obs_2week <= d.temp2.red$`95%`)
        p95 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`2.5%` & d.temp2.red$obs_2week <= d.temp2.red$`97.5%`)
        p99 <- sum(d.temp2.red$obs_2week >= d.temp2.red$`0.5%` & d.temp2.red$obs_2week <= d.temp2.red$`99.5%`)
        
        y <- c(p25 / denom2, p50 / denom2, p60 / denom2, p70 / denom2, p80 / denom2, p90 / denom2, p95 / denom2, p99 / denom2) * 100
        rmse <- sqrt(sum((c(p25 / denom2, p50 / denom2, p60 / denom2, p70 / denom2, p80 / denom2, p90 / denom2, p95 / denom2, p99 / denom2) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom2, 8), rep(model.type, 8), rep(o1, 8), rep(2, 8)))
        dat.temp.wks <- rbind(dat.temp.wks, dat.temp)
      }
      if (denom3 > 0) {
        p25 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`37.5%` & d.temp3.red$obs_3week <= d.temp3.red$`62.5%`)
        p50 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`25%` & d.temp3.red$obs_3week <= d.temp3.red$`75%`)
        p60 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`20%` & d.temp3.red$obs_3week <= d.temp3.red$`80%`)
        p70 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`15%` & d.temp3.red$obs_3week <= d.temp3.red$`85%`)
        p80 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`10%` & d.temp3.red$obs_3week <= d.temp3.red$`90%`)
        p90 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`5%` & d.temp3.red$obs_3week <= d.temp3.red$`95%`)
        p95 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`2.5%` & d.temp3.red$obs_3week <= d.temp3.red$`97.5%`)
        p99 <- sum(d.temp3.red$obs_3week >= d.temp3.red$`0.5%` & d.temp3.red$obs_3week <= d.temp3.red$`99.5%`)
        
        y <- c(p25 / denom3, p50 / denom3, p60 / denom3, p70 / denom3, p80 / denom3, p90 / denom3, p95 / denom3, p99 / denom3) * 100
        rmse <- sqrt(sum((c(p25 / denom3, p50 / denom3, p60 / denom3, p70 / denom3, p80 / denom3, p90 / denom3, p95 / denom3, p99 / denom3) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom3, 8), rep(model.type, 8), rep(o1, 8), rep(3, 8)))
        dat.temp.wks <- rbind(dat.temp.wks, dat.temp)
      }
      if (denom4 > 0) {
        p25 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`37.5%` & d.temp4.red$obs_4week <= d.temp4.red$`62.5%`)
        p50 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`25%` & d.temp4.red$obs_4week <= d.temp4.red$`75%`)
        p60 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`20%` & d.temp4.red$obs_4week <= d.temp4.red$`80%`)
        p70 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`15%` & d.temp4.red$obs_4week <= d.temp4.red$`85%`)
        p80 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`10%` & d.temp4.red$obs_4week <= d.temp4.red$`90%`)
        p90 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`5%` & d.temp4.red$obs_4week <= d.temp4.red$`95%`)
        p95 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`2.5%` & d.temp4.red$obs_4week <= d.temp4.red$`97.5%`)
        p99 <- sum(d.temp4.red$obs_4week >= d.temp4.red$`0.5%` & d.temp4.red$obs_4week <= d.temp4.red$`99.5%`)
        
        y <- c(p25 / denom4, p50 / denom4, p60 / denom4, p70 / denom4, p80 / denom4, p90 / denom4, p95 / denom4, p99 / denom4) * 100
        rmse <- sqrt(sum((c(p25 / denom4, p50 / denom4, p60 / denom4, p70 / denom4, p80 / denom4, p90 / denom4, p95 / denom4, p99 / denom4) * 100 - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
        
        dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom4, 8), rep(model.type, 8), rep(o1, 8), rep(4, 8)))
        dat.temp.wks <- rbind(dat.temp.wks, dat.temp)
      }
      
    }
  }
  
}
rm(dat.temp, d.new.pi, d.new1, d.new2, d.new3, d.new4, d.temp.pt, d.temp.pt2, d.temp.pi, d.temp.pi2, d.temp.ot, d.temp.ot2, d.temp1, d.temp1.red, d.temp2, d.temp2.red,
   d.temp3, d.temp3.red, d.temp4, d.temp4.red, denom.pt, denom.pi, denom.ot, denom1, denom2, denom3, denom4, lead, len.pi, len1, len2, len3, len4, model.type, o1,
   p25, p50, p60, p70, p80, p90, p95, p99, rmse, y)

# Format outputs:
dat.temp.pt <- format_calib_output(dat.temp.pt, 'Peak Timing')
dat.temp.pi <- format_calib_output(dat.temp.pi, 'Peak Intensity')
dat.temp.ot <- format_calib_output(dat.temp.ot, 'Onset Timing'); dat.temp.ot$model <- factor(dat.temp.ot$model, levels = levels(dat.temp.ot$model)[c(3, 1, 4, 2)])

for (i in c(1:2, 4:5)) {
  dat.temp.wks[, i] <- as.numeric(as.character(dat.temp.wks[, i]))
}
names(dat.temp.wks) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'model', 'oev_base', 'metric')
dat.temp.wks$model <- factor(dat.temp.wks$model, levels = levels(dat.temp.wks$model)[c(3:4, 2, 1)])

# Relevel leads:
dat.temp.pt$lead <- factor(dat.temp.pt$lead, levels = levels(dat.temp.pt$lead)[c(2, 1, 3:5)])
dat.temp.pi$lead <- factor(dat.temp.pi$lead, levels = levels(dat.temp.pi$lead)[c(2, 1, 3:5)])
dat.temp.wks$lead <- factor(dat.temp.wks$lead, levels = levels(dat.temp.wks$lead)[c(2, 1, 3:5)])

# Relevel OEVS:
dat.temp.pt$oev_base <- factor(dat.temp.pt$oev_base, levels = levels(dat.temp.pt$oev_base)[2:1])
dat.temp.pi$oev_base <- factor(dat.temp.pi$oev_base, levels = levels(dat.temp.pi$oev_base)[2:1])
# dat.temp.ot$oev_base <- factor(dat.temp.ot$oev_base, levels = levels(dat.temp.ot$oev_base)[2:1])
dat.temp.wks$oev_base <- factor(dat.temp.wks$oev_base, levels = levels(dat.temp.wks$oev_base)[2:1])

# Plot:
p1 <- ggplot(data = dat.temp.pt, aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = 'Peak Timing') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
p2 <- ggplot(data = dat.temp.pi, aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = 'Peak Intensity') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
p3 <- ggplot(data = dat.temp.ot, aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = 'Onset Timing') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)

p5 <- ggplot(data = dat.temp.wks[dat.temp.wks$metric == 1, ], aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = '1 Week Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
p6 <- ggplot(data = dat.temp.wks[dat.temp.wks$metric == 2, ], aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = '2 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
p7 <- ggplot(data = dat.temp.wks[dat.temp.wks$metric == 3, ], aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = '3 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
p8 <- ggplot(data = dat.temp.wks[dat.temp.wks$metric == 4, ], aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = '4 Weeks Ahead') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)

# And save:
pdf('results/plots/comp_calib.pdf', width = 14, height = 9)
grid.arrange(p1, p2, p3, ncol = 1)
grid.arrange(p5, p6, ncol = 1)
grid.arrange(p7, p8, ncol = 1)
dev.off()






