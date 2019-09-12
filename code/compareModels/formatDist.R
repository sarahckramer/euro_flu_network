
### Read in files of interest:
# Peak timing:
d1 <- read.csv('results/original/outputDist_090119_pt.csv')
d2 <- read.csv('results/newScalings/outputDist_090919_pt.csv')
d3 <- read.csv('results/propRandTravel/outputDist_090919_pt.csv')
d4 <- read.csv('results/indivCountries/outputDist_082819_PT.csv')

m.red <- m[, c('country', 'season', 'run', 'oev_base', 'fc_start', 'obs_pkwk', 'onset5', 'onsetObs5',
               'leadpkwk_mean', 'FWeek_pkwk', 'model')]

d1$model <- 'Original'; d2$model <- 'New Scale'; d3$model <- 'Random Travel'; d4$model <- 'Individual'

d1$bin <- d1$bin + 40 - 1; d1$bin[d1$bin == 38] <- -1
d2$bin <- d2$bin + 40 - 1; d2$bin[d2$bin == 38] <- -1
d3$bin <- d3$bin + 40 - 1; d3$bin[d3$bin == 38] <- -1

names(d1)[c(8, 10)] = names(d2)[c(8, 10)] = names(d3)[c(8, 10)] = c('week', 'prob')
d4$gamma <- NULL

d.pt <- rbind(d1, d2, d3, d4)
d.pt <- merge(d.pt, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))

d.pt$delta_pkwk <- d.pt$week - d.pt$obs_pkwk

d.pt <- d.pt[d.pt$week != -1, ]
d.pt <- d.pt[d.pt$prob > 0, ] # don't need weeks with 0% confidence

d.pt$oev_base <- factor(d.pt$oev_base)
d.pt$model <- factor(d.pt$model); d.pt$model <- factor(d.pt$model, levels = levels(d.pt$model)[c(3, 2, 4, 1)])

# Peak intensity:
d1 <- read.csv('results/original/outputEns_090119_PI.csv')
d2 <- read.csv('results/newScalings/outputEns_090919_PI.csv')
d3 <- read.csv('results/propRandTravel/outputEns_090919_PI.csv')
d4 <- read.csv('results/indivCountries/outputEns_082819_PI.csv')

m.red <- m[, c('country', 'season', 'run', 'oev_base', 'fc_start', 'scaling', 'obs_peak_int', 'obs_pkwk',
               'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk', 'model')]

d1$model <- 'Original'; d2$model <- 'New Scale'; d3$model <- 'Random Travel'; d4$model <- 'Individual'

d4$gamma <- NULL; names(d4)[7] <- 'metric'

d.pi <- rbind(d1, d2, d3, d4)
d.pi <- merge(d.pi, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))

d.pi[, c(10:309)] <- d.pi[, c(10:309)] / d.pi[, 310]
d.pi$scaling <- NULL

d.pi$oev_base <- factor(d.pi$oev_base)
d.pi$model <- factor(d.pi$model); d.pi$model <- factor(d.pi$model, levels = levels(d.pi$model)[c(3, 2, 4, 1)])

# Onset timing:
d1 <- read.csv('results/original/outputDist_090119_ot.csv')
d2 <- read.csv('results/newScalings/outputDist_090919_ot.csv')
d3 <- read.csv('results/propRandTravel/outputDist_090919_ot.csv')
d4 <- read.csv('results/indivCountries/outputDist_082819_oT.csv')

m.red <- m[, c('country', 'season', 'run', 'oev_base', 'fc_start', 'obs_pkwk', 'onset5', 'onsetObs5',
               'leadpkwk_mean', 'FWeek_pkwk', 'model')]

d1$model <- 'Original'; d2$model <- 'New Scale'; d3$model <- 'Random Travel'; d4$model <- 'Individual'

d1$bin <- d1$bin + 40 - 1; d1$bin[d1$bin == 38] <- -1
d2$bin <- d2$bin + 40 - 1; d2$bin[d2$bin == 38] <- -1
d3$bin <- d3$bin + 40 - 1; d3$bin[d3$bin == 38] <- -1
d4$week <- d4$week + 40 - 1; d4$week[d4$week == 38] <- -1

names(d1)[c(8, 10)] = names(d2)[c(8, 10)] = names(d3)[c(8, 10)] = c('week', 'prob')
d4$gamma <- NULL

d.ot <- rbind(d1, d2, d3, d4)
d.ot <- merge(d.ot, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))

d.ot$delta_onwk <- d.ot$week - d.ot$onsetObs5
d.ot$leadonwk_mean <- d.ot$fc_start - d.ot$onset5
d.ot$FWeek_onwk <- d.ot$fc_start - d.ot$onsetObs5

d.ot <- d.ot[d.ot$week != -1, ]
d.ot <- d.ot[d.ot$prob > 0, ] # don't need weeks with 0% confidence

d.ot$oev_base <- factor(d.ot$oev_base)
d.ot$model <- factor(d.ot$model); d.ot$model <- factor(d.ot$model, levels = levels(d.ot$model)[c(3, 2, 4, 1)])

# 1-4 weeks:
a1 <- read.csv('results/original/outputEns_090119_1wk.csv')
a2 <- read.csv('results/original/outputEns_090119_2wk.csv')
a3 <- read.csv('results/original/outputEns_090119_3wk.csv')
a4 <- read.csv('results/original/outputEns_090119_4wk.csv')

b1 <- read.csv('results/newScalings/outputEns_090919_1wk.csv')
b2 <- read.csv('results/newScalings/outputEns_090919_2wk.csv')
b3 <- read.csv('results/newScalings/outputEns_090919_3wk.csv')
b4 <- read.csv('results/newScalings/outputEns_090919_4wk.csv')

c1 <- read.csv('results/propRandTravel/outputEns_090919_1wk.csv')
c2 <- read.csv('results/propRandTravel/outputEns_090919_2wk.csv')
c3 <- read.csv('results/propRandTravel/outputEns_090919_3wk.csv')
c4 <- read.csv('results/propRandTravel/outputEns_090919_4wk.csv')

d1 <- read.csv('results/indivCountries/outputEns_082819_1wk.csv')
d2 <- read.csv('results/indivCountries/outputEns_082819_2wk.csv')
d3 <- read.csv('results/indivCountries/outputEns_082819_3wk.csv')
d4 <- read.csv('results/indivCountries/outputEns_082819_4wk.csv')

d1 <- d1[!is.na(d1$X1), ]
d2 <- d2[!is.na(d2$X1), ]
d3 <- d3[!is.na(d3$X1), ]
d4 <- d4[!is.na(d4$X1), ]

m.red <- m[, c('country', 'season', 'run', 'oev_base', 'scaling', 'fc_start',
               'obs_1week', 'obs_2week', 'obs_3week', 'obs_4week', 'obs_pkwk',
               'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk', 'model')]

a1$model = a2$model = a3$model = a4$model = 'Original'
b1$model = b2$model = b3$model = b4$model = 'New Scale'
c1$model = c2$model = c3$model = c4$model = 'Random Travel'
d1$model = d2$model = d3$model = d4$model = 'Individual'

d1$gamma = d2$gamma = d3$gamma = d4$gamma = NULL
names(d1)[7] = names(d2)[7] = names(d3)[7] = names(d4)[7] = 'metric'

d1 <- rbind(a1, b1, c1, d1)
d2 <- rbind(a2, b2, c2, d2)
d3 <- rbind(a3, b3, c3, d3)
d4 <- rbind(a4, b4, c4, d4)
rm(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4)

d1 <- merge(d1, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))
d2 <- merge(d2, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))
d3 <- merge(d3, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))
d4 <- merge(d4, m.red, by = c('country', 'season', 'run', 'oev_base', 'fc_start', 'model'))

d1[, c(10:309)] <- d1[, c(10:309)] / d1[, 310]
d2[, c(10:309)] <- d2[, c(10:309)] / d2[, 310]
d3[, c(10:309)] <- d3[, c(10:309)] / d3[, 310]
d4[, c(10:309)] <- d4[, c(10:309)] / d4[, 310]

d1$scaling <- NULL; d2$scaling <- NULL; d3$scaling <- NULL; d4$scaling <- NULL

d1 <- d1[!is.na(d1$obs_1week) & d1$obs_1week > 0, ]
d2 <- d2[!is.na(d2$obs_2week) & d2$obs_2week > 0, ]
d3 <- d3[!is.na(d3$obs_3week) & d3$obs_3week > 0, ]
d4 <- d4[!is.na(d4$obs_4week) & d4$obs_4week > 0, ]

d1$oev_base <- factor(d1$oev_base)
d1$model <- factor(d1$model); d1$model <- factor(d1$model, levels = levels(d1$model)[c(3, 2, 4, 1)])

d2$oev_base <- factor(d2$oev_base)
d2$model <- factor(d2$model); d2$model <- factor(d2$model, levels = levels(d2$model)[c(3, 2, 4, 1)])

d3$oev_base <- factor(d3$oev_base)
d3$model <- factor(d3$model); d3$model <- factor(d3$model, levels = levels(d3$model)[c(3, 2, 4, 1)])

d4$oev_base <- factor(d4$oev_base)
d4$model <- factor(d4$model); d4$model <- factor(d4$model, levels = levels(d4$model)[c(3, 2, 4, 1)])

dat.wks.list <- list(d1, d2, d3, d4)
rm(m.red)

### Format for predicted and observed lead weeks:
d.pt.red <- d.pt[d.pt$leadpkwk_mean >= -6 & d.pt$leadpkwk_mean < 4 & !is.na(d.pt$leadpkwk_mean) & !is.na(d.pt$onset5), ]
d.pt.red$leadpkwk_bin <- cut(d.pt.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))

d.pi.red <- d.pi[d.pi$leadpkwk_mean >= -6 & d.pi$leadpkwk_mean < 4 & !is.na(d.pi$leadpkwk_mean) & !is.na(d.pi$onset5), ]
d.pi.red$leadpkwk_bin <- cut(d.pi.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))

d.ot.red <- d.ot[d.ot$leadonwk_mean >= -6 & d.ot$leadonwk_mean < 4 & !is.na(d.ot$leadonwk_mean) & !is.na(d.ot$onset5), ]
d.ot.red$leadonwk_bin <- cut(d.ot.red$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

d1.red <- d1[d1$leadpkwk_mean >= -6 & d1$leadpkwk_mean < 4 & !is.na(d1$leadpkwk_mean) & !is.na(d1$onset5), ]
d1.red$leadpkwk_bin <- cut(d1.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))
d2.red <- d2[d2$leadpkwk_mean >= -6 & d2$leadpkwk_mean < 4 & !is.na(d2$leadpkwk_mean) & !is.na(d2$onset5), ]
d2.red$leadpkwk_bin <- cut(d2.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))
d3.red <- d3[d3$leadpkwk_mean >= -6 & d3$leadpkwk_mean < 4 & !is.na(d3$leadpkwk_mean) & !is.na(d3$onset5), ]
d3.red$leadpkwk_bin <- cut(d3.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))
d4.red <- d4[d4$leadpkwk_mean >= -6 & d4$leadpkwk_mean < 4 & !is.na(d4$leadpkwk_mean) & !is.na(d4$onset5), ]
d4.red$leadpkwk_bin <- cut(d4.red$leadpkwk_mean, c(-7, -5, -3, -1, 1, 3))

dat.list.pred <- list(d.pt.red, d.pi.red, d.ot.red, d1.red, d2.red, d3.red, d4.red)

d.pt.red <- d.pt[d.pt$FWeek_pkwk >= -6 & d.pt$FWeek_pkwk < 4 & !is.na(d.pt$FWeek_pkwk) & !is.na(d.pt$onset5), ]
d.pt.red$FWeek_pkwk_bin <- cut(d.pt.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))

d.pi.red <- d.pi[d.pi$FWeek_pkwk >= -6 & d.pi$FWeek_pkwk < 4 & !is.na(d.pi$FWeek_pkwk) & !is.na(d.pi$onset5), ]
d.pi.red$FWeek_pkwk_bin <- cut(d.pi.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))

d.ot.red <- d.ot[d.ot$FWeek_onwk >= -6 & d.ot$FWeek_onwk < 4 & !is.na(d.ot$FWeek_onwk) & !is.na(d.ot$onset5), ]
d.ot.red$FWeek_onwk_bin <- cut(d.ot.red$FWeek_onwk, c(-7, -5, -3, -1, 1, 3))

d1.red <- d1[d1$FWeek_pkwk >= -6 & d1$FWeek_pkwk < 4 & !is.na(d1$FWeek_pkwk) & !is.na(d1$onset5), ]
d1.red$FWeek_pkwk_bin <- cut(d1.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))
d2.red <- d2[d2$FWeek_pkwk >= -6 & d2$FWeek_pkwk < 4 & !is.na(d2$FWeek_pkwk) & !is.na(d2$onset5), ]
d2.red$FWeek_pkwk_bin <- cut(d2.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))
d3.red <- d3[d3$FWeek_pkwk >= -6 & d3$FWeek_pkwk < 4 & !is.na(d3$FWeek_pkwk) & !is.na(d3$onset5), ]
d3.red$FWeek_pkwk_bin <- cut(d3.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))
d4.red <- d4[d4$FWeek_pkwk >= -6 & d4$FWeek_pkwk < 4 & !is.na(d4$FWeek_pkwk) & !is.na(d4$onset5), ]
d4.red$FWeek_pkwk_bin <- cut(d4.red$FWeek_pkwk, c(-7, -5, -3, -1, 1, 3))

dat.list.obs <- list(d.pt.red, d.pi.red, d.ot.red, d1.red, d2.red, d3.red, d4.red)

# Clean-up:
rm(d.pt, d.pt.red, d.pi, d.pi.red, d.ot, d.ot.red, d1, d1.red, d2, d2.red, d3, d3.red, d4, d4.red, dat.wks.list)



