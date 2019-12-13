
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Set model labels:
m1.lab <- 'Indiv. (Narrow)'
m2.lab <- 'Indiv. (Mid)'
m3.lab <- 'Indiv. (Wide)'

# Set locations of model results to be compared:
model1 <- 'results/A(H1)/indiv_narrow/'
model2 <- 'results/A(H1)/indiv_mid/'
model3 <- 'results/A(H1)/indiv_wide/'

#########################################################################################################################################################
#########################################################################################################################################################

# Read in and format metrics files:
m1 <- read.csv(file = paste0(model1, list.files(path = model1, pattern = 'Met_pro')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = 'Met_pro')))
m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = 'Met_pro')))

m1 <- m1[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
m3 <- m3[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]

m1$model <- m1.lab; m2$model <- m2.lab; m3$model <- m3.lab

m <- rbind(m1, m2, m3)
m$model <- factor(m$model, levels = c(m1.lab, m2.lab, m3.lab))
rm(m1, m2, m3)

#########################################################################################################################################################
#########################################################################################################################################################

# Look at "mean" results:
m$onsetBi <- NA
m$onsetBi[(is.na(m$onset5) & is.na(m$onsetObs5)) | (!is.na(m$onset5) & !is.na(m$onsetObs5))] <- '1'
m$onsetBi[(is.na(m$onset5) & !is.na(m$onsetObs5)) | (!is.na(m$onset5) & is.na(m$onsetObs5))] <- '0'
m$onsetBi <- factor(m$onsetBi)

# m.temp <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ]

table(m$onsetBi, m$model) / colSums(table(m$onsetBi, m$model)) # narrow does best here

# What if we classify four ways? True pos, false pos, etc.:
m$classify <- NA
m$classify[is.na(m$onset5) & is.na(m$onsetObs5)] <- 'True Neg.'
m$classify[is.na(m$onset5) & !is.na(m$onsetObs5)] <- 'False Neg.'
m$classify[!is.na(m$onset5) & is.na(m$onsetObs5)] <- 'False Pos.'
m$classify[!is.na(m$onset5) & !is.na(m$onsetObs5)] <- 'True Pos.'
m$classify <- factor(m$classify)
print(length(m$classify[is.na(m$classify)]))

# Then can calculate sensitivity, specificity, PPV, NPV.
# Although how valuable are these? Most of the time we know that there WILL be an onset

# Most important might just be: How often are observed NAs correctly predicted?
table(m$classify, m$model) / colSums(table(m$classify, m$model))
# Narrow less likely to have false negatives, about as likely (only a little less) to get true negatives
# In general true negatives are pretty rare I guess

# m.temp <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 5, ]
# table(m.temp$classify, m.temp$model) / colSums(table(m.temp$classify, m.temp$model))
# no, because of course then we get rid of any w/o onsets

# Calculate NPV and sensitivity:
npvs = spec = c()
for (i in 1:length(levels(m$model))) {
  print(levels(m$model)[i])
  m.temp <- m[m$model == levels(m$model)[i], ]
  
  npvs <- c(npvs, length(m.temp$country[m.temp$classify == 'True Neg.']) / length(m.temp$country[m.temp$classify %in% c('True Neg.', 'False Neg.')]))
  spec <- c(spec, length(m.temp$country[m.temp$classify == 'True Neg.']) / length(m.temp$country[m.temp$classify %in% c('True Neg.', 'False Pos.')]))
}

# NPV: Of those that are predicted to be negative, how many really are?
# Specificity: Of those that are truly negative in reality, how many are also predicted to be negative?

# So narrow has higher NPV, but others have higher specificity

#########################################################################################################################################################
#########################################################################################################################################################

# Thought about calculating log score for those w/o onset - log(prob NA)
# But can't plot this out by observed lead week or anything... plot out just by time?

d1 <- read.csv('results/A(H1)/indiv_narrow/outputDist_10000_10_1.02_H1_narrow.csv'); d1 <- d1[d1$metric == 'onset5', ]
d2 <- read.csv('results/A(H1)/indiv_mid/outputDist_10000_10_1.02_H1_mid.csv'); d2 <- d2[d2$metric == 'onset5', ]
d3 <- read.csv('results/A(H1)/indiv_wide/outputDist_10000_10_1.02_H1_wide.csv'); d3 <- d3[d3$metric == 'onset5', ]

m1 <- read.csv(file = paste0(model1, list.files(path = model1, pattern = 'Met_pro')))
m2 <- read.csv(file = paste0(model2, list.files(path = model2, pattern = 'Met_pro')))
m3 <- read.csv(file = paste0(model3, list.files(path = model3, pattern = 'Met_pro')))

m1 <- m1[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
m2 <- m2[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]
m3 <- m3[, c(1:9, 12:13, 15, 17:19, 25:32, 39, 43, 47, 60:63, 65, 67:69, 78)]

m1$model <- m1.lab; m2$model <- m2.lab; m3$model <- m3.lab

m1 <- unique(m1[, c(1, 7:8, 24, 36)])
m2 <- unique(m2[, c(1, 7:8, 24, 36)])
m3 <- unique(m3[, c(1, 7:8, 24, 36)])

# Join observed values:
d1 <- merge(d1, m1, by = c('country', 'season', 'fc_start'))
d2 <- merge(d2, m2, by = c('country', 'season', 'fc_start'))
d3 <- merge(d3, m3, by = c('country', 'season', 'fc_start'))

# Correct bins:
d1$week <- d1$week + 40 - 1
d2$week <- d2$week + 40 - 1
d3$week <- d3$week + 40 - 1

# NA bins:
d1$week[d1$week == 38] <- NA
d2$week[d2$week == 38] <- NA
d3$week[d3$week == 38] <- NA

# Keep only when bin == obs:
d1 <- d1[(d1$week == d1$onsetObs5 & !is.na(d1$week) & !is.na(d1$onsetObs5)) | (is.na(d1$week) & is.na(d1$onsetObs5)), ]
d2 <- d2[(d2$week == d2$onsetObs5 & !is.na(d2$week) & !is.na(d2$onsetObs5)) | (is.na(d2$week) & is.na(d2$onsetObs5)), ]
d3 <- d3[(d3$week == d3$onsetObs5 & !is.na(d3$week) & !is.na(d3$onsetObs5)) | (is.na(d3$week) & is.na(d3$onsetObs5)), ]

# Calculate scores with and without no observed onsets:
d1$score <- log(d1$prob); d1$score[d1$score == -Inf] <- -10
d2$score <- log(d2$prob); d2$score[d2$score == -Inf] <- -10
d3$score <- log(d3$prob); d3$score[d3$score == -Inf] <- -10

d1.red <- d1[!is.na(d1$onsetObs5), ]
d2.red <- d2[!is.na(d2$onsetObs5), ]
d3.red <- d3[!is.na(d3$onsetObs5), ]

# Plot out scores over time both with and without NAs:
d <- rbind(d1, d2, d3)
d.red <- rbind(d1.red, d2.red, d3.red)
d.noOn <- d[is.na(d$onsetObs5), ]

d.agg <- aggregate(score ~ fc_start + model, data = d, FUN = median)
d.agg.red <- aggregate(score ~ fc_start + model, data = d.red, FUN = median)
d.agg.noOn <- aggregate(score ~ fc_start + model, data = d.noOn, FUN = median)

p1 <- ggplot(data = d.agg, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'All')
p2 <- ggplot(data = d.agg.red, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'Onset Only')
p3 <- ggplot(data = d.agg.noOn, aes(x = fc_start, y = score, col = model)) + geom_point() + geom_line() + theme_bw() + labs(x = 'Week', y = 'Score', title = 'No Onset')
grid.arrange(p1, p2, p3, ncol = 1)
# looking at only those countries with no onset, narrow obviously does much worse, even late in the outbreak; but that's the only real difference

















