### Generate calibration plots ###

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

# Functions needed:

# weighted quantiles for peak timing/peak intensity
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

# weighted quantiles for onset timing
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

# get credible intervals from model results for a given metric and range of lead weeks
processLead <- function(lead.bin, d, metric) {
  if (metric == 'pt') {
    d.temp <- d[d$leadpkwk_bin == lead.bin, ]
  } else if (metric == 'ot') {
    d.temp <- d[d$leadonwk_bin == lead.bin, ]
  }
  
  c1 <- split(d.temp[, c('bin', 'value')], d.temp[, c('country', 'season', 'run', 'fc_start', 'model', 'subtype')])
  c15 <- sapply(1:length(c1), function(ix) {
    unlist(c1[ix])
  })
  c1 <- c1[lapply(c15, length) > 0]
  
  if (metric == 'pt') {
    c2 <- lapply(c1, wtd.quantile.new)
  } else if (metric == 'ot') {
    c2 <- lapply(c1, wtd.quantile.onset)
  }
  
  c3 <- as.data.frame(unlist(c2))
  c35 <- matrix(unlist(strsplit(rownames(c3), split = '[.]')), nrow = length(c3$`unlist(c2)`), byrow = TRUE)
  c4 <- cbind(c3, c35)
  rownames(c4) <- NULL
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'model', 'subtype', 'quantile')
  c5 <- dcast(c4, model + country + season + run + fc_start + subtype ~ quantile, value.var = 'value')
  
  if (metric == 'pt') {
    d.new <- merge(c5, unique(d.temp[, c(1:2, 13)]), by = c('country', 'season'))
  } else if (metric == 'ot') {
    d.new <- merge(c5, unique(d.temp[, c(1:2, 15)]), by = c('country', 'season'))
  }
  
  return(d.new)
}

# check how often observed falls within credible intervals (PT and OT)
get_calibration_Dist <- function(d, met, denom, lead, model) {
  
  p25 <- sum(d[, met] >= d$`37.5%` & d[, met] <= d$`62.5%`)
  p50 <- sum(d[, met] >= d$`25.0%` & d[, met] <= d$`75.0%`)
  p60 <- sum(d[, met] >= d$`20.0%` & d[, met] <= d$`80.0%`)
  p70 <- sum(d[, met] >= d$`15.0%` & d[, met] <= d$`85.0%`)
  p80 <- sum(d[, met] >= d$`10.0%` & d[, met] <= d$`90.0%`)
  p90 <- sum(d[, met] >= d$` 5.0%` & d[, met] <= d$`95.0%`)
  p95 <- sum(d[, met] >= d$` 2.5%` & d[, met] <= d$`97.5%`)
  p99 <- sum(d[, met] >= d$` 0.5%` & d[, met] <= d$`99.5%`)
  
  y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
  rmse <- sqrt(sum((y - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
  
  dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(model, 8)))
  return(dat.temp)
}

# format calibration results for plotting
format_calib_output <- function(d, met.name) {
  d[, 1] <- as.numeric(as.character(d[, 1]))
  d[, 2] <- as.numeric(as.character(d[, 2]))
  d[, 4] <- as.numeric(as.character(d[, 4]))
  d[, 5] <- as.numeric(as.character(d[, 5]))
  
  d$metric <- met.name
  names(d) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'model', 'metric')
  
  # d$model <- factor(d$model, levels = levels(d$model)[c(3:4, 2, 1)])
  
  return(d)
}

# check how often observed falls within credible intervals (PI)
get_calibration_Dist_int <- function(d, met, denom, lead,  model) {
  
  p25 <- sum(d[, met] >= d$`37.5%` & d[, met] <= d$`62.5%`)
  p50 <- sum(d[, met] >= d$`25%` & d[, met] <= d$`75%`)
  p60 <- sum(d[, met] >= d$`20%` & d[, met] <= d$`80%`)
  p70 <- sum(d[, met] >= d$`15%` & d[, met] <= d$`85%`)
  p80 <- sum(d[, met] >= d$`10%` & d[, met] <= d$`90%`)
  p90 <- sum(d[, met] >= d$`5%` & d[, met] <= d$`95%`)
  p95 <- sum(d[, met] >= d$`2.5%` & d[, met] <= d$`97.5%`)
  p99 <- sum(d[, met] >= d$`0.5%` & d[, met] <= d$`99.5%`)
  
  y <- c(p25 / denom, p50 / denom, p60 / denom, p70 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
  rmse <- sqrt(sum((y - c(25, 50, 60, 70, 80, 90, 95, 99)) ** 2) / 8)
  
  dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(model, 8)))
  return(dat.temp)
}

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

# Read in libraries:
library(Hmisc)
library(reshape2)

# Set probability values:
p <- c(0.005, 0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.375, 0.625, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.995)

### Peak and Onset Timing ###
# Read in and compile dist files (PT):
d1 <- read.csv('results/network/PROC_outputDist_pt_ot.csv')
d2 <- read.csv('results/isolated/outputDist_pt_ot.csv')

# Reformat bins:
d <- rbind(d1, d2); rm(d1, d2)
d$bin <- d$bin + 40 - 1
d$bin[d$bin == 38] <- -1

# Get lead weeks:
m1 <- read.csv('results/network/outputMet_pro_PROC.csv')
m2 <- read.csv('results/isolated/outputMet_pro_PROC.csv')

m1.red <- unique(m1[, c(1:2, 7:9, 12, 30:31, 38, 45, 56:57)])
m2.red <- unique(m2[, c(1:2, 7:9, 12, 30:31, 38, 45, 56:57)])

m1.red$model <- 'Network'; m2.red$model <- 'Isolated'
m.red <- rbind(m1.red, m2.red)
rm(m1, m2, m1.red, m2.red)

# d.try <- merge(d, m.red, by = c('country', 'season', 'run', 'fc_start', 'subtype', 'model'))
d <- merge(d, m.red, by = c('country', 'season', 'run', 'fc_start', 'subtype', 'model'))

# Calculate differences between bins and observed values:
d$delta_pkwk <- d$bin - d$obs_pkwk
d$delta_onwk <- d$bin - d$onsetObs5

# Remove where onsetObs is NA:
d <- d[!is.na(d$onsetObs5), ]

# Don't need NA bins or bins with 0% confidence:
d <- d[d$bin != -1, ]
d <- d[d$value > 0, ]

# Separate now into pt and ot:
d.pt <- d[d$metric == 'pw', ]; d.pt$metric <- factor(d.pt$metric)
d.ot <- d[d$metric == 'onset5', ]; d.ot$metric <- factor(d.ot$metric)

d.pt <- d.pt[d.pt$leadpkwk_mean >= -8 & d.pt$leadpkwk_mean < 4 & !is.na(d.pt$leadpkwk_mean) & !is.na(d.pt$onset5), ]
d.ot <- d.ot[d.ot$leadonset5 >= -6 & d.ot$leadonset5 < 4 & !is.na(d.ot$leadonset5) & !is.na(d.ot$onset5), ]

d.pt$leadpkwk_bin <- cut(d.pt$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
d.ot$leadonwk_bin <- cut(d.ot$leadonset5, c(-7, -5, -3, -1, 1, 3))

# Get list of countries:
countries <- levels(d$country) # note: different order than that in which forecasts run!

# Calculate calibration over all leads / models (eventually subtypes too?):
dat.pt.temp <- data.frame()
dat.ot.temp <- data.frame()

for (lead in levels(d.pt$leadpkwk_bin)) {
  print(lead)
  d.temp <- processLead(lead, d.pt, 'pt')
  
  # Now check how often observed values fall into certain credible intervals for each lead/model combo:
  for (model.type in unique(d.temp$model)) {
    d.temp2 <- d.temp[d.temp$model == model.type, ]
    denom <- length(d.temp2$country)
    
    if (denom > 0) {
      dat.temp <- get_calibration_Dist(d.temp2, 'obs_pkwk', denom, lead, model.type)
      dat.pt.temp <- rbind(dat.pt.temp, dat.temp)
    }
    
  }
  
}
rm(d.temp, d.temp2, dat.temp)

for (lead in levels(d.ot$leadonwk_bin)) {
  print(lead)
  d.temp <- processLead(lead, d.ot, 'ot')
  
  # Now check how often observed values fall into certain credible intervals for each lead/model combo:
  for (model.type in unique(d.temp$model)) {
    d.temp2 <- d.temp[d.temp$model == model.type, ]
    denom <- length(d.temp2$country)
      
    if (denom > 0) {
      dat.temp <- get_calibration_Dist(d.temp2, 'onsetObs5', denom, lead, model.type)
      dat.ot.temp <- rbind(dat.ot.temp, dat.temp)
    }
      
  }
  
}
rm(d.temp, d.temp2, dat.temp)

# Format results:
dat.pt.temp <- format_calib_output(dat.pt.temp, 'Peak Timing')
dat.ot.temp <- format_calib_output(dat.ot.temp, 'Onset Timing')

# Clean up:
rm(d, d.ot, d.pt, m.red, denom, lead, model.type)

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

### Peak Intensity ###
# Read in results:
d1 <- read.csv('results/network/PROC_outputEns_pi.csv')
d2 <- read.csv('results/isolated/outputEns_pi.csv')
d <- rbind(d1, d2); rm(d1, d2)

# Get lead weeks:
m1 <- read.csv('results/network/outputMet_pro_PROC.csv')
m2 <- read.csv('results/isolated/outputMet_pro_PROC.csv')

m1.red <- unique(m1[, c(1:2, 6:9, 12, 14, 30:31, 38, 45, 56:57)])
m2.red <- unique(m2[, c(1:2, 6:9, 12, 14, 30:31, 38, 45, 56:57)])

m1.red$model <- 'Network'; m2.red$model <- 'Isolated'
m.red <- rbind(m1.red, m2.red)
rm(m1, m2, m1.red, m2.red)

# d.try <- merge(d, m.red, by = c('country', 'season', 'run', 'fc_start', 'subtype', 'model'))
d <- merge(d, m.red, by = c('country', 'season', 'run', 'fc_start', 'subtype', 'model'))

# Unscale estimates:
d[, c(11:310)] <- d[, c(11:310)] / d[, 311]
d$scaling <- NULL

# Remove where onsetObs is NA:
d <- d[!is.na(d$onsetObs5), ]

# Limit to lead weeks of interest:
d.red <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 4 & !is.na(d$leadpkwk_mean) & !is.na(d$onset5), ]
d.red$leadpkwk_bin <- cut(d.red$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

# Calucate calibration over all leads / models (PI):
dat.pi.temp <- data.frame()
for (lead in levels(d.red$leadpkwk_bin)) {
  print(lead)
  
  d.temp1 <- d.red[d.red$leadpkwk_bin == lead, ]
  len <- length(d.temp1$country)
  
  if (len > 0) {
    d.new <- sapply(1:len, function(ix) {
      quantile(as.vector(unlist(d.temp1[ix, 11:310])), probs = p)
    })
    d.new <- t(d.new); d.new <- cbind(d.temp1, d.new); d.new <- d.new[, -c(11:310)]
    
    for (model.type in unique(d.temp1$model)) {
      d.temp2 <- d.new[d.new$model == model.type, ]
      denom <- length(d.temp2$country)
      
      if (denom > 0) {
        dat.temp <- get_calibration_Dist_int(d.temp2, 'obs_peak_int', denom, lead, model.type)
        dat.pi.temp <- rbind(dat.pi.temp, dat.temp)
      }
    }
  }
  
}
rm(d.temp1, d.temp2, d.new, dat.temp)

# Format results:
dat.pi.temp <- format_calib_output(dat.pi.temp, 'Peak Intensity')

# Clean up!:
rm(d, d.red, m.red, denom, lead, len, model.type, p)

# Now plot out as in Aim 1:
dat.fig <- rbind(dat.pt.temp, dat.pi.temp, dat.ot.temp)
dat.fig$metric <- factor(dat.fig$metric)
dat.fig$metric <- factor(dat.fig$metric, levels = levels(dat.fig$metric)[3:1])

dat.fig <- dat.fig[dat.fig$lead %in% levels(dat.fig$lead)[2:4], ]

p.full <- ggplot(data = dat.fig, aes(x = quantile, y = y, colour = lead)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 0.6) +
  geom_line() + geom_point(size = 5) + #geom_point(aes(size = len)) +
  labs(x = 'Prediction Interval (%)', y = '% of Obs within Pred. Interval', colour = 'Predicted\nLead Week:') + theme_bw() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 14), axis.text = element_text(size = 12),
        strip.text = element_blank(), axis.title = element_text(size = 14),
        legend.title = element_text(size = 14), strip.background = element_blank()) +
  # scale_color_brewer(palette = 'Set1') +
  scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(model ~ metric)
# p.full
dat.text <- data.frame(label = c('A', 'B', 'C', 'D', 'E', 'F'), model = c(rep('Network', 3), rep('Isolated', 3)),
                       metric = c('Peak Timing', 'Peak Intensity', 'Onset Timing',
                                  'Peak Timing', 'Peak Intensity', 'Onset Timing'),
                       y = c(96, 96, 81.5, 96, 96, 96))
p1 <- p.full + geom_text(data = dat.text, mapping = aes(x = 22.25, y = y, label = label), size = 11, color = 'black')
# p1
ggsave(filename = 'results/plots/Fig4.svg', plot = p1, width = 12, height = 7)

# pdf(file = 'results/plots/Fig4.pdf', width = 12, height = 7)
# print(p1)
# dev.off()

# Clean up:
rm(list = ls())

