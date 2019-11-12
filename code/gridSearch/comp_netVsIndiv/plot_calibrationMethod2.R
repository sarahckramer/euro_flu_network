
# Read in libraries:
library(Hmisc)

# Set probability values:
p <- c(0.005, 0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.375, 0.625, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.995)

### Peak Timing ###
# Read in and compile dist files (PT):
a.dist <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Dist.*_pt')))
b.dist <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Dist.*_PT')))
m.red <- m[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk', 'model')]

b.dist <- b.dist[b.dist$country %in% levels(a.dist$country) & b.dist$lambda == 1.02 & b.dist$oev_denom == 10, ]

d.pkwk <- format_Dist(a.dist, b.dist, m.red)
d.pkwk$country <- factor(d.pkwk$country)
countries <- levels(d.pkwk$country) # note: different order than that in which forecasts run!

# Calculate calibration over all leads / models / OEVs / lambdas:
dat.temp.all <- data.frame()
for (lead in levels(d.pkwk$leadpkwk_bin)) {
  print(lead)
  d.temp1 <- process_Lead(lead, d.pkwk, 'pt')
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (model.type in unique(d.temp1$model)) {
    for (o1 in unique(d.temp1$oev_base)) {
      # for (lam in unique(d.temp1$lambda)) {
        d.temp2 <- d.temp1[d.temp1$model == model.type & d.temp1$oev_base == o1, ]# & d.temp1$lambda == lam, ]
        denom <- length(d.temp2$country)
        
        if (denom > 0) {
          dat.temp <- get_calibration_Dist(d.temp2, 'obs_pkwk', denom, lead, o1, lam = 1.00, model.type)
          dat.temp.all <- rbind(dat.temp.all, dat.temp)
        }
        
      }
    # }
  }
  
}

# Format dat.temp.all:
dat.pt.temp <- format_calib_output(dat.temp.all, 'Peak Timing')

# Get plot!:
p5 <- ggplot(data = dat.pt.temp, aes(x = quantile, y = y, color = model, group = model)) +
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
# print(p5)

### Onset Timing ###
# Read in and compile dist files (PT):
a.dist <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Dist.*_ot')))
b.dist <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Dist.*_OT')))

b.dist <- b.dist[b.dist$country %in% levels(a.dist$country) & b.dist$lambda == 1.02 & b.dist$oev_denom == 10, ]

d.onwk <- format_Dist_OT(a.dist, b.dist, m.red)

# Calculate calibration over all leads / models / OEVs / lambdas:
dat.temp.all <- data.frame()
for (lead in levels(d.onwk$leadonwk_bin)) {
  print(lead)
  d.temp1 <- process_Lead(lead, d.onwk, 'ot')
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (model.type in unique(d.temp1$model)) {
    for (o1 in unique(d.temp1$oev_base)) {
      # for (lam in unique(d.temp1$lambda)) {
      d.temp2 <- d.temp1[d.temp1$model == model.type & d.temp1$oev_base == o1, ]# & d.temp1$lambda == lam, ]
      denom <- length(d.temp2$country)
      
      if (denom > 0) {
        dat.temp <- get_calibration_Dist(d.temp2, 'onsetObs5', denom, lead, o1, lam = 1.00, model.type)
        dat.temp.all <- rbind(dat.temp.all, dat.temp)
      }
      
    }
    # }
  }
  
}

# Format dat.temp.all:
dat.ot.temp <- format_calib_output(dat.temp.all, 'Onset Timing')

# Get plot!:
p7 <- ggplot(data = dat.ot.temp, aes(x = quantile, y = y, color = model, group = model)) +
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
# print(p7)

### Peak Intensity ###
# Read in and compile files:
a.dist <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Ens.*_PI')))
b.dist <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Ens.*_PI')))
m.red <- m[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'scaling', 'obs_peak_int', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk', 'model')]

b.dist <- b.dist[b.dist$country %in% levels(a.dist$country) & b.dist$lambda == 1.02 & b.dist$oev_denom == 10, ]
b.dist$gamma <- NULL

d.int <- format_Dist_qual(a.dist, b.dist, m.red)

# Calculate calibration over all leads / models / OEVs / lambdas:
dat.temp.all <- data.frame()
for (lead in levels(d.int$leadpkwk_bin)) {
  print(lead)
  
  d.temp1 <- d.int[d.int$leadpkwk_bin == lead, ]
  len <- length(d.temp1$country)
  
  if (len > 0) {
    d.new <- sapply(1:length(d.temp1$leadpkwk_mean), function(ix) {
      quantile(d.temp1[ix, 10:309], probs = p)
    })
    d.new <- t(d.new); d.new <- cbind(d.temp1, d.new); d.new <- d.new[, -c(10:309)]
    
    for (model.type in unique(d.temp1$model)) {
      for (o1 in unique(d.temp1$oev_base)) {
        d.temp2 <- d.new[d.new$model == model.type & d.new$oev_base == o1, ]
        denom <- length(d.temp2$country)
        
        if (denom > 0) {
          dat.temp <- get_calibration_Dist_int(d.temp2, 'obs_peak_int', denom, lead, o1, lam = 1.02, model.type)
          dat.temp.all <- rbind(dat.temp.all, dat.temp)
        }
        
      }
    }
    
  }
  
}

# Format dat.temp.all:
dat.pi.temp <- format_calib_output(dat.temp.all, 'Peak Intensity')

# Get plot!:
p6 <- ggplot(data = dat.pi.temp, aes(x = quantile, y = y, color = model, group = model)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(aes(size = len)) + theme_bw() +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = '', len = '# Fcasts', title = 'Peak Intensity') +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61')) +#, '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lead)
# print(p6)

# grid.arrange(p5, p6, p7, ncol = 1)
print(p5)
print(p6)
print(p7)

# Clean up!:
rm(a.dist, b.dist, d.int, d.new, d.onwk, d.pkwk, d.temp1, d.temp2, dat.ot.temp, dat.pi.temp, dat.pt.temp, dat.temp, dat.temp.all)

# Can we also look at calibration for the 1-4 week ahead predictions?:
a1 <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Ens.*_1wk')))
a2 <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Ens.*_2wk')))
a3 <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Ens.*_3wk')))
a4 <- read.csv(paste0(model1, list.files(path = model1, pattern = 'Ens.*_4wk')))

a1.ind <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Ens.*_1wk')))
a2.ind <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Ens.*_2wk')))
a3.ind <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Ens.*_3wk')))
a4.ind <- read.csv(paste0(model2, list.files(path = model2, pattern = 'Ens.*_4wk')))

a1.ind <- a1.ind[a1.ind$country %in% levels(a1$country) & a1.ind$lambda == 1.02 & a1.ind$oev_denom == 10, ]; a1.ind$country <- factor(a1.ind$country)
a2.ind <- a2.ind[a2.ind$country %in% levels(a1$country) & a2.ind$lambda == 1.02 & a2.ind$oev_denom == 10, ]; a2.ind$country <- factor(a2.ind$country)
a3.ind <- a3.ind[a3.ind$country %in% levels(a1$country) & a3.ind$lambda == 1.02 & a3.ind$oev_denom == 10, ]; a3.ind$country <- factor(a3.ind$country)
a4.ind <- a4.ind[a4.ind$country %in% levels(a1$country) & a4.ind$lambda == 1.02 & a4.ind$oev_denom == 10, ]; a4.ind$country <- factor(a4.ind$country)

a1.ind <- a1.ind[!is.na(a1.ind$X1), ]
a2.ind <- a2.ind[!is.na(a2.ind$X1), ]
a3.ind <- a3.ind[!is.na(a3.ind$X1), ]
a4.ind <- a4.ind[!is.na(a4.ind$X1), ]

m.red <- m[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'scaling', 'fc_start', 'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk', 'model')]

a1 <- format_Dist_Wks(a1, a1.ind, m.red, 'obs_1week')
a2 <- format_Dist_Wks(a2, a2.ind, m.red, 'obs_2week')
a3 <- format_Dist_Wks(a3, a3.ind, m.red, 'obs_3week')
a4 <- format_Dist_Wks(a4, a4.ind, m.red, 'obs_4week')
rm(a1.ind, a2.ind, a3.ind, a4.ind)

dat.in.list <- list(a1, a2, a3, a4)

# STOP. Some positions in individual files where all ensemble members predict "NA"
    # Where either observation NA, or no onset predicted?
    # Or just where obs_vars is 0? But why does that only happen for these couple of country/season combos? - rarely 0, more likely to be NA

dat.temp.all <- data.frame()
for (fcast_wk in 1:4) {
  a.int <- dat.in.list[[fcast_wk]]
  
  if (fcast_wk == 1) {
    print(1)
    names(a.int)[310] <- 'obs_wk'
  } else if (fcast_wk == 2) {
    print(2)
    names(a.int)[311] <- 'obs_wk'
  } else if (fcast_wk == 3) {
    print(3)
    names(a.int)[312] <- 'obs_wk'
  } else if (fcast_wk == 4) {
    print(4)
    names(a.int)[313] <- 'obs_wk'
  }
  
  for (lead in levels(a.int$leadpkwk_bin)) {
    print(lead)
    
    a.temp <- a.int[a.int$leadpkwk_bin == lead, ]
    len <- length(a.temp$country)
    
    if (len > 0) {
      a.new <- sapply(1:length(a.temp$leadpkwk_mean), function(ix) {
        quantile(a.temp[ix, 10:309], probs = p)
      })
      a.new <- t(a.new); a.new <- cbind(a.temp, a.new); a.new <- a.new[, -c(10:309)]
      
      for (model.type in unique(a.temp$model)) {
        for (o1 in levels(a.temp$oev_base)) {
          a.new.temp <- a.new[a.new$oev_base == o1 & a.new$model == model.type, ]
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
            
            dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(model.type, 8), rep(o1, 8), rep(fcast_wk, 8)))
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
names(dat.wk.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'model', 'oev_base', 'metric')
dat.wk.temp$model <- factor(dat.wk.temp$model, levels = levels(dat.wk.temp$model)[2:1])

p1 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 1, ], aes(x = quantile, y = y, color = model, group = model)) +
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
p2 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 2, ], aes(x = quantile, y = y, color = model, group = model)) +
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
p3 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 3, ], aes(x = quantile, y = y, color = model, group = model)) +
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
p4 <- ggplot(data = dat.wk.temp[dat.wk.temp$metric == 4, ], aes(x = quantile, y = y, color = model, group = model)) +
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
print(p1); print(p2); print(p3); print(p4)

# Clean up!:
rm(a.int, a.new, a.new.temp, a.temp, a1, a2, a3, a4, dat.in.list, dat.wk.temp, m.red, p1, p2, p3, p4, p5, p6, p7, denom, fcast_wk, i, lead, len,
   model.type, o1, p, p25, p50, p60, p70, p80, p90, p95, p99, rmse, y)





