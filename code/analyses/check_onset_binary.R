
### Assess sensitivity/specificity of binary onset predictions once European outbreak has started (by calendar week) ###

# Read in metrics files:
m <- read.csv('results/network/outputMet_pro_PROC.csv')
m.isol <- read.csv('results/isolated/outputMet_pro_PROC.csv')

# Reduce to required columns:
m <- m[, c(1:2, 7:8, 30:31, 45, 57)]
m.isol <- m.isol[, c(1:2, 7:8, 30:31, 45, 57)]

# For each season/subtype, determine first observed onset, and remove all fcasts before it:
m.new = m.isol.new = NULL
for (season in levels(m$season)) {
  print(season)
  
  for (strain in levels(m$subtype)) {
    m.temp <- m[m$season == season & m$subtype == strain, ]
    # m1.temp <- m.isol[m.isol$season == season & m.isol$subtype == strain, ]
    
    if (dim(m.temp)[1] > 0) {
      print(strain)
      
      first.on <- min(m.temp$onsetObs5, na.rm = TRUE)
      print(first.on)
      
      m.temp <- m.temp[m.temp$fc_start >= first.on, ]
      m1.temp <- m.isol[m.isol$season == season & m.isol$subtype == strain & m.isol$fc_start >= first.on, ]
      
      m.new <- rbind(m.new, m.temp)
      m.isol.new <- rbind(m.isol.new, m1.temp)
      print('')
    }
    
  }
}
rm(m.temp, m1.temp, season, strain, first.on)

m <- as.data.frame(m.new)
m.isol <- as.data.frame(m.isol.new)
rm(m.new, m.isol.new)

# Combine data frames
m$model <- 'Network'; m.isol$model <- 'Isolated'
m <- rbind(m, m.isol)
m$model <- factor(m$model)
rm(m.isol)

# Designate true pos, false pos, true neg, false neg:
m$result <- NA

m$result[!is.na(m$onsetObs5) & !is.na(m$onset5)] <- 'True Pos'
m$result[is.na(m$onsetObs5) & !is.na(m$onset5)] <- 'False Pos'
m$result[!is.na(m$onsetObs5) & is.na(m$onset5)] <- 'False Neg'
m$result[is.na(m$onsetObs5) & is.na(m$onset5)] <- 'True Neg'
m$result <- factor(m$result)

# Calculate sens and spec by fc_start (week):
res.dat <- NULL
for (model in levels(m$model)) {
  for (fc in unique(m$fc_start)) {
    m.temp <- m[m$model == model & m$fc_start == fc, ]
    
    true_pos <- length(m.temp$result[m.temp$result == 'True Pos'])
    true_neg <- length(m.temp$result[m.temp$result == 'True Neg'])
    false_pos <- length(m.temp$result[m.temp$result == 'False Pos'])
    false_neg <- length(m.temp$result[m.temp$result == 'False Neg'])
    
    # print(table(m.temp$result))
    
    sens <- true_pos / (true_pos + false_neg)
    spec <- true_neg / (true_neg + false_pos)
    
    num_pos <- true_pos + false_neg
    num_neg <- true_neg + false_pos
    
    res.dat <- rbind(res.dat, c(model, fc, sens, spec, num_pos, num_neg))
  }
}
rm(true_pos, true_neg, false_pos, false_neg, sens, spec, model, fc, num_pos, num_neg)

res.dat <- as.data.frame(res.dat)
names(res.dat) <- c('model', 'fc_start', 'sens', 'spec', 'n_pos', 'n_neg')
# note: not really a clear pattern by country; also probably not enough forecasts (esp. for spec.) to really tell

# Convert to numeric:
res.dat$sens <- as.numeric(as.character(res.dat$sens))
res.dat$spec <- as.numeric(as.character(res.dat$spec))
res.dat$n_pos <- as.numeric(as.character(res.dat$n_pos))
res.dat$n_neg <- as.numeric(as.character(res.dat$n_neg))

# Convert to percentages:
res.dat$sens <- res.dat$sens * 100
res.dat$spec <- res.dat$spec * 100

# Update levels:
res.dat$model <- factor(res.dat$model, levels = levels(res.dat$model)[2:1])

# Plot up sens/spec by model:
# p1 <- ggplot(data = res.dat, aes(x = fc_start, y = sens, group = model, col = model)) + geom_line() + geom_point(aes(size = n_pos)) + theme_classic() +
#   labs(x = 'Calendar Week', y = 'Sensitivity', col = 'Model:', size = 'Total w/ Onset:') + scale_color_brewer(palette = 'Set1')
# p2 <- ggplot(data = res.dat, aes(x = fc_start, y = spec, group = model, col = model)) + geom_line() + geom_point(aes(size = n_neg)) + theme_classic() +
#   labs(x = 'Calendar Week', y = 'Specificity', col = 'Model:', size = 'Total w/o Onset') + scale_color_brewer(palette = 'Set1') +
#   scale_size_continuous(limits = c(0, 150), breaks = c(10, 50, 100, 150))
# grid.arrange(p1, p2)
# # false positives are incredibly rare

p1 <- ggplot(data = res.dat, aes(x = fc_start, y = sens, group = model, col = model)) + geom_line() + geom_point(size = 3) + theme_classic() +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 14), axis.title = element_text(size = 16),
        axis.text = element_text(size = 12), legend.position = 'bottom') +
  labs(x = 'Calendar Week', y = 'Sensitivity', col = 'Model:') + scale_color_brewer(palette = 'Set1') + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_text(mapping = aes(x = 1.5, y = 97, label = 'A'), col = 'black', size = 11)
p2 <- ggplot(data = res.dat, aes(x = fc_start, y = spec, group = model, col = model)) + geom_line() + geom_point(size = 3) + theme_classic() +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 14), axis.title = element_text(size = 16),
        axis.text = element_text(size = 12), legend.position = 'bottom') +
  labs(x = 'Calendar Week', y = 'Specificity', col = 'Model:') + scale_color_brewer(palette = 'Set1') + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_text(mapping = aes(x = 1.5, y = 90, label = 'B'), col = 'black', size = 11)
grid.arrange(p1, p2, ncol = 2)

ggsave(filename = 'results/plots/S13_Fig.svg', plot = arrangeGrob(p1, p2, ncol = 2), width = 13.25, height = 5)

###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################

# Or should we have removed anything once the actual onset does occur, b/c at that point the prediction's no longer relevant?:
m <- m[m$onsetObs5 >= m$fc_start | is.na(m$onsetObs5), ]

# Calculate sens and spec by fc_start (week):
res.dat <- NULL
for (model in levels(m$model)) {
  for (fc in unique(m$fc_start)) {
    m.temp <- m[m$model == model & m$fc_start == fc, ]
    
    true_pos <- length(m.temp$result[m.temp$result == 'True Pos'])
    true_neg <- length(m.temp$result[m.temp$result == 'True Neg'])
    false_pos <- length(m.temp$result[m.temp$result == 'False Pos'])
    false_neg <- length(m.temp$result[m.temp$result == 'False Neg'])
    
    # print(table(m.temp$result))
    
    sens <- true_pos / (true_pos + false_neg)
    spec <- true_neg / (true_neg + false_pos)
    
    num_pos <- true_pos + false_neg
    num_neg <- true_neg + false_pos
    
    res.dat <- rbind(res.dat, c(model, fc, sens, spec, num_pos, num_neg))
  }
}
rm(true_pos, true_neg, false_pos, false_neg, sens, spec, model, fc, num_pos, num_neg)

res.dat <- as.data.frame(res.dat)
names(res.dat) <- c('model', 'fc_start', 'sens', 'spec', 'n_pos', 'n_neg')

# Convert to numeric:
res.dat$sens <- as.numeric(as.character(res.dat$sens))
res.dat$spec <- as.numeric(as.character(res.dat$spec))
res.dat$n_pos <- as.numeric(as.character(res.dat$n_pos))
res.dat$n_neg <- as.numeric(as.character(res.dat$n_neg))
res.dat$fc_start <- as.numeric(as.character(res.dat$fc_start))

# Update levels:
res.dat$model <- factor(res.dat$model, levels = levels(res.dat$model)[2:1])

# Plot up sens/spec by model/country:
p1 <- ggplot(data = res.dat, aes(x = fc_start, y = sens, group = model, col = model)) + geom_line() + geom_point(aes(size = n_pos)) + theme_classic() +
  labs(x = 'Calendar Week', y = 'Sensitivity', col = 'Model:', size = 'Total w/ Onset:') + scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = res.dat, aes(x = fc_start, y = spec, group = model, col = model)) + geom_line() + geom_point(aes(size = n_neg)) + theme_classic() +
  labs(x = 'Calendar Week', y = 'Specificity', col = 'Model:', size = 'Total w/o Onset') + scale_color_brewer(palette = 'Set1') +
  scale_size_continuous(limits = c(0, 150), breaks = c(10, 50, 100, 150))
grid.arrange(p1, p2)
# false positives are incredibly rare

p1 <- ggplot(data = res.dat, aes(x = fc_start, y = sens, group = model, col = model)) + geom_line() + geom_point(size = 2) + theme_classic() +
  labs(x = 'Calendar Week', y = 'Sensitivity', col = 'Model:') + scale_color_brewer(palette = 'Set1') + scale_y_continuous(limits = c(0, 1))
p2 <- ggplot(data = res.dat, aes(x = fc_start, y = spec, group = model, col = model)) + geom_line() + geom_point(size = 2) + theme_classic() +
  labs(x = 'Calendar Week', y = 'Specificity', col = 'Model:') + scale_color_brewer(palette = 'Set1') + scale_y_continuous(limits = c(0, 1))
grid.arrange(p1, p2)
# note: this doesn't change specificity
# sensitivity gets much lower, which makes sense; also stops at week 63, since that is the latest observed onset

res.temp <- res.dat[res.dat$fc_start < 64, ]
summary(res.temp$sens[res.temp$model == 'Isolated']) # 0 to 0.29412 (mean = 0.123) # include 63: max 0.80, mean = 0.16258, median = 0.1125
summary(res.temp$sens[res.temp$model == 'Network']) # 0 to 0.2617 (mean = 0.151) # include 63: max 0.60, mean = 0.1775, median = 0.1356
summary(res.temp$sens) # 0 to 0.2941; mean = 0.1369, median = 0.1209 # include 63: 0 to 0.8; mean = 0.1700, median = 0.1273
# maybe mention these numbers as a qualification, but use other plot

#########################################################################################################################################################
#########################################################################################################################################################

# Old, but interesting idea:

# # Thought about calculating log score for those w/o onset - log(prob NA)
# # But can't plot this out by observed lead week or anything... plot out just by time?
# 
# d1 <- read.csv('results/A(H1)/indiv_narrow/outputDist_10000_10_1.02_H1_narrow.csv'); d1 <- d1[d1$metric == 'onset5', ]
# d2 <- read.csv('results/A(H1)/indiv_mid/outputDist_10000_10_1.02_H1_mid.csv'); d2 <- d2[d2$metric == 'onset5', ]
# d3 <- read.csv('results/A(H1)/indiv_wide/outputDist_10000_10_1.02_H1_wide.csv'); d3 <- d3[d3$metric == 'onset5', ]
# 
# m1 <- read.csv(file = paste0(model1, list.files(path = model1, pattern = 'Met_pro')))
# m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = 'Met_pro')))
# m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = 'Met_pro')))
# 
# m1 <- m1[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
# m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
# m3 <- m3[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
# 
# m1$model <- m1.lab; m2$model <- m2.lab; m3$model <- m3.lab
# 
# m1 <- unique(m1[, c(1, 7:8, 24, 36)])
# m2 <- unique(m2[, c(1, 7:8, 24, 36)])
# m3 <- unique(m3[, c(1, 7:8, 24, 36)])
# 
# # Join observed values:
# d1 <- merge(d1, m1, by = c('country', 'season', 'fc_start'))
# d2 <- merge(d2, m2, by = c('country', 'season', 'fc_start'))
# d3 <- merge(d3, m3, by = c('country', 'season', 'fc_start'))
# 
# # Correct bins:
# d1$week <- d1$week + 40 - 1
# d2$week <- d2$week + 40 - 1
# d3$week <- d3$week + 40 - 1
# 
# # NA bins:
# d1$week[d1$week == 38] <- NA
# d2$week[d2$week == 38] <- NA
# d3$week[d3$week == 38] <- NA
# 
# # Keep only when bin == obs:
# d1 <- d1[(d1$week == d1$onsetObs5 & !is.na(d1$week) & !is.na(d1$onsetObs5)) | (is.na(d1$week) & is.na(d1$onsetObs5)), ]
# d2 <- d2[(d2$week == d2$onsetObs5 & !is.na(d2$week) & !is.na(d2$onsetObs5)) | (is.na(d2$week) & is.na(d2$onsetObs5)), ]
# d3 <- d3[(d3$week == d3$onsetObs5 & !is.na(d3$week) & !is.na(d3$onsetObs5)) | (is.na(d3$week) & is.na(d3$onsetObs5)), ]
# 
# # Calculate scores with and without no observed onsets:
# d1$score <- log(d1$prob); d1$score[d1$score == -Inf] <- -10
# d2$score <- log(d2$prob); d2$score[d2$score == -Inf] <- -10
# d3$score <- log(d3$prob); d3$score[d3$score == -Inf] <- -10
# 
# d1.red <- d1[!is.na(d1$onsetObs5), ]
# d2.red <- d2[!is.na(d2$onsetObs5), ]
# d3.red <- d3[!is.na(d3$onsetObs5), ]
# 
# # Plot out scores over time both with and without NAs:
# d <- rbind(d1, d2, d3)
# d.red <- rbind(d1.red, d2.red, d3.red)
# d.noOn <- d[is.na(d$onsetObs5), ]
# 
# d.agg <- aggregate(score ~ fc_start + model, data = d, FUN = median)
# d.agg.red <- aggregate(score ~ fc_start + model, data = d.red, FUN = median)
# d.agg.noOn <- aggregate(score ~ fc_start + model, data = d.noOn, FUN = median)
# 
# p1 <- ggplot(data = d.agg, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'All')
# p2 <- ggplot(data = d.agg.red, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'Onset Only')
# p3 <- ggplot(data = d.agg.noOn, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'No Onset')
# grid.arrange(p1, p2, p3, ncol = 1)
# # looking at only those countries with no onset, narrow obviously does much worse, even late in the outbreak; but that's the only real difference

















