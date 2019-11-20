
# setwd('results/PROCESS/')
getwd()

### Deal with PT and OT first - can use Dist:
d.pt <- read.csv(file = list.files(pattern = '_pt'))
d.ot <- read.csv(file = list.files(pattern = '_ot'))

# Join observed values:
m <- read.csv(file = list.files(pattern = '_pro'))
m <- unique(m[, c(1, 8:9, 39)])

d.pt <- merge(d.pt, m, by = c('season', 'country'))
d.ot <- merge(d.ot, m, by = c('season', 'country'))

# Remove where no onset observed:
d.pt <- d.pt[!is.na(d.pt$onsetObs5), ]
d.ot <- d.ot[!is.na(d.ot$onsetObs5), ]

# Create factors:
d.pt$oev_base <- factor(d.pt$oev_base); d.pt$oev_denom <- factor(d.pt$oev_denom); d.pt$lambda <- factor(d.pt$lambda); d.pt$run <- factor(d.pt$run)
d.ot$oev_base <- factor(d.ot$oev_base); d.ot$oev_denom <- factor(d.ot$oev_denom); d.ot$lambda <- factor(d.ot$lambda); d.ot$run <- factor(d.ot$run)

# Correct bins:
# d.pt$bin <- d.pt$bin + 40 - 1 # already corrected!
d.ot$week <- d.ot$week + 40 - 1

# Keep only results for bins SAME WEEK week as the observed values:
d.pt <- d.pt[d.pt$week == d.pt$obs_pkwk, ]
d.ot <- d.ot[d.ot$week == d.ot$onsetObs5, ]

# Search through combinations of country, season, run, oev_base, oev_denom, lambda, fc_start to calculate log score for PT and store:
countries <- levels(d.pt$country) # note: DIFFERENT ORDER than used in forecasting!
log.pt = log.ot = NULL

for (i in 1:length(countries)) {
  print(countries[i])
  
  for (o1 in levels(d.pt$oev_base)) {
    for (o2 in levels(d.pt$oev_denom)) {
      for (l in levels(d.pt$lambda)) {
        
        for (season in levels(d.pt$season)) {
          for (run in levels(d.pt$run)) {
          
          d.temp <- d.pt[d.pt$country == countries[i] & d.pt$oev_base == o1 & d.pt$oev_denom == o2 & d.pt$lambda == l & d.pt$season == season & d.pt$run == run, ]
          if (length(d.temp$season) > 0) {
            scores <- unlist(lapply(sort(unique(d.temp$fc_start)), function(ix) {
              log(sum(d.temp[d.temp$fc_start == ix, 'prob']))
            }))
            log.pt <- rbind(log.pt, cbind(season, countries[i], run, o1, o2, l, sort(unique(d.temp$fc_start)), scores))
          }
          
          d.temp <- d.ot[d.ot$country == countries[i] & d.ot$oev_base == o1 & d.ot$oev_denom == o2 & d.ot$lambda == l & d.ot$season == season & d.ot$run == run, ]
          if (length(d.temp$season) > 0) {
            scores <- unlist(lapply(sort(unique(d.temp$fc_start)), function(ix) {
              log(sum(d.temp[d.temp$fc_start == ix, 'prob']))
            }))
            log.ot <- rbind(log.ot, cbind(season, countries[i], run, o1, o2, l, sort(unique(d.temp$fc_start)), scores))
          }
          
          }
        }
        
      }
    }
  }

}

log.pt <- as.data.frame(log.pt)
log.ot <- as.data.frame(log.ot)

# Set colnames:
names(log.pt) <- c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'score')
names(log.ot) <- c('season', 'country', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'score')

# Make scores numeric:
log.pt$score <- as.numeric(as.character(log.pt$score))
log.ot$score <- as.numeric(as.character(log.ot$score))

# Replace -Infs with -10:
log.pt$score[log.pt$score == -Inf] <- -10
log.ot$score[log.ot$score == -Inf] <- -10

# Determine predicted and observed lead weeks:
m <- read.csv(file = list.files(pattern = '_pro'))
m$leadonset5 <- m$fc_start - m$onset5
m <- unique(m[, c(1:3, 7:9, 15, 39, 60, 67, 78)])
m <- m[!is.na(m$onsetObs5), ]

log.pt <- merge(log.pt, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
log.ot <- merge(log.ot, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Label with relevant metric:
log.pt$metric <- 'pt'
log.ot$metric <- 'ot'

# Combine and save:
d <- rbind(d.pt, d.ot)
write.csv(d, file = 'logScores_pt_ot.csv', row.names = FALSE)

rm(list = ls())

# Method 1: Bins
# Note: Don't have results broken down into bins of 250/500 at this point and it takes a long time to process those, so just use bins of 1000
d <- read.csv(list.files(pattern = 'Dist_11'))
d.wks <- d[d$metric %in% levels(d$metric)[1:4], ]; d.wks$metric <- factor(d.wks$metric)
d <- d[d$metric == 'pi', ]

# Get observed peak intensity:
m <- read.csv(file = list.files(pattern = '_pro'))
m <- unique(m[, c(1, 6, 8, 17, 39)])

# Scale peak intensity values:
m$obs_peak_int <- m$obs_peak_int * m$scaling

# Merge:
d <- merge(d, m, by = c('season', 'country'))
d <- d[!is.na(d$onsetObs5), ]
d$gamma <- NULL; d$metric <- NULL

# Categorize obs_peak_int by bin:
d$obs_peak_int_bin <- cut(d$obs_peak_int, c(seq(0, 10000, by = 1000), 100000))
levels(d$obs_peak_int_bin) <- c(seq(1000, 10000, by = 1000), 1e5)

# Remove where bin not equal to obs_peak_int:
d <- d[d$week == d$obs_peak_int_bin, ]

# Now calculate log score as log of the % of ensemble members within the "correct" bin:
d$score <- log(d$prob)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:7, 12:14)]

# Get lead weeks:
m <- read.csv(file = list.files(pattern = '_pro'))
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:3, 7:9, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'logScores_pi_alt1_1000.csv', row.names = FALSE)
rm(d)

# And for 1-4 weeks:
levels(d.wks$metric) <- c('1week', '2week', '3week', '4week')
d1 <- d.wks[d.wks$metric == '1week', ]; d1$metric <- NULL
d2 <- d.wks[d.wks$metric == '2week', ]; d2$metric <- NULL
d3 <- d.wks[d.wks$metric == '3week', ]; d3$metric <- NULL
d4 <- d.wks[d.wks$metric == '4week', ]; d4$metric <- NULL

m <- read.csv(file = list.files(pattern = '_pro'))
m1 <- unique(m[, c(1:3, 6:8, 25, 39)])
m2 <- unique(m[, c(1:3, 6:8, 26, 39)])
m3 <- unique(m[, c(1:3, 6:8, 27, 39)])
m4 <- unique(m[, c(1:3, 6:8, 28, 39)])

# Rescale values:
m1$obs_1week <- m1$obs_1week * m1$scaling
m2$obs_2week <- m2$obs_2week * m2$scaling
m3$obs_3week <- m3$obs_3week * m3$scaling
m4$obs_4week <- m4$obs_4week * m4$scaling

# Add metrics marker:
m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'

# Merge:
d1 <- merge(d1, m1, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d2 <- merge(d2, m2, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d3 <- merge(d3, m3, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
d4 <- merge(d4, m4, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

names(d1)[12] = names(d2)[12] = names(d3)[12] = names(d4)[12] = 'obs_xweek'

# Combine into one data frame:
d <- rbind(d1, d2, d3, d4)
d$metric <- factor(d$metric)

# Remove where no onset:
d <- d[!is.na(d$onsetObs5), ]

# Categorize obs_peak_int by bin:
d$obs_xweek_bin <- cut(d$obs_xweek, c(seq(0, 10000, by = 1000), 100000))
levels(d$obs_xweek_bin) <- c(seq(1000, 10000, by = 1000), 1e5)
d$obs_xweek_bin[d$obs_xweek == 0 & !is.na(d$obs_xweek)] <- '1000'

# Remove where obs is NA:
d <- d[!is.na(d$obs_xweek), ]

# And where prob NA?
d <- d[!is.na(d$prob), ] # these are all where data are 0, but there are many, many 0s left after removing these

# Remove where bin not equal to obs_xweek_bin:
d <- d[d$week == d$obs_xweek_bin, ]

# Calculate log scores:
d$score <- log(d$prob)
d$score[d$score == -Inf] <- -10

# Reduce appropriately:
d <- d[, c(1:5, 7:8, 11:14, 16)]

# Get lead weeks:
m <- read.csv(file = list.files(pattern = '_pro'))
m$leadonset5 <- m$fc_start - m$onset5
m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
m <- unique(m[, c(1:3, 7:9, 15, 60, 67, 78)])

d <- merge(d, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))

# Save:
write.csv(d, file = 'logScores_1-4wk_alt1_1000.csv', row.names = FALSE)

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

### Kernel Density ###

for (bin.size in c(250, 500, 1000)) {
  
  # Read in ens file:
  e <- read.csv(file = list.files(pattern = '_PI'))
  
  # Determine mean and sd for each row:
  means <- sapply(1:dim(e)[1], function(ix) {
    mean(as.numeric(e[ix, 10:309]))
  })
  sds <- sapply(1:dim(e)[1], function(ix) {
    sd(as.numeric(e[ix, 10:309]))
  })
  e$mean <- means; e$sd <- sds
  
  # Reduce appropriately:
  e <- e[, c(1:3, 5:9, 310:311)]
  
  # Remove where mean/sd NA:
  e <- e[!is.na(e$mean), ]
  
  # Get observed peak intensity and bin:
  # m <- read.csv(file = list.files(pattern = '_pro'))
  m <- read.csv(file = '../../results/R0diff_OEVnew/denom10lam102/outputMet_111219_INDIV_pro.csv')
  m <- unique(m[, c(1, 6, 8, 17, 39)])
  m$obs_peak_int <- m$obs_peak_int * m$scaling
  
  m$obs_peak_int_bin <- cut(m$obs_peak_int, c(seq(0, 14000, by = bin.size), 100000))
  levels(m$obs_peak_int_bin) <- seq(0, 14000, by = bin.size)
  m$obs_peak_int_bin <- as.numeric(as.character(m$obs_peak_int_bin))
  
  # Merge:
  e <- merge(e, m, by = c('season', 'country'))
  
  # Find % of normal distribution within observed bin:
  e$lower <- e$obs_peak_int_bin; e$upper <- e$obs_peak_int_bin + bin.size
  e$upper[e$upper == 14000] <- 1e5
  probs <- sapply(1:dim(e)[1], function(ix) {
    pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
  })
  e$prob <- probs
  
  # Calculate scores:
  e$score <- log(e$prob)
  print(e$score[e$score == -Inf])
  hist(e$score)
  
  # Reduce appropriately:
  e <- e[, c(1:8, 12:13, 18)]
  
  # Get lead weeks:
  m <- read.csv(file = list.files(pattern = '_pro'))
  m$leadonset5 <- m$fc_start - m$onset5
  m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
  m <- unique(m[, c(1:5, 7:9, 15, 60, 67, 78)])
  
  e <- merge(e, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
  
  # Save:
  write.csv(e, file = paste0('logScores_pi_alt2_', bin.size,'.csv'), row.names = FALSE)
  rm(e)
  
  # Now repeat for 1-4 weeks:
  e1 <- read.csv(list.files(pattern = '_1wk'))
  e2 <- read.csv(list.files(pattern = '_2wk'))
  e3 <- read.csv(list.files(pattern = '_3wk'))
  e4 <- read.csv(list.files(pattern = '_4wk'))
  e <- rbind(e1, e2, e3, e4)
  rm(e1, e2, e3, e4)
  
  # Determine mean and sd for each row:
  means <- sapply(1:dim(e)[1], function(ix) {
    mean(as.numeric(e[ix, 10:309]))
  })
  sds <- sapply(1:dim(e)[1], function(ix) {
    sd(as.numeric(e[ix, 10:309]))
  })
  e$mean <- means; e$sd <- sds
  
  # Reduce appropriately:
  e <- e[, c(1:3, 5:9, 310:311)]
  
  # Remove where mean/sd NA:
  e <- e[!is.na(e$mean), ]
  
  # Get observed values and bin:
  m <- read.csv(file = list.files(pattern = '_pro'))
  m1 <- unique(m[, c(1:9, 25, 39)])
  m2 <- unique(m[, c(1:9, 26, 39)])
  m3 <- unique(m[, c(1:9, 27, 39)])
  m4 <- unique(m[, c(1:9, 28, 39)])
  
  m1$obs_1week <- m1$obs_1week * m1$scaling
  m2$obs_2week <- m2$obs_2week * m2$scaling
  m3$obs_3week <- m3$obs_3week * m3$scaling
  m4$obs_4week <- m4$obs_4week * m4$scaling
  
  names(m1)[10] = names(m2)[10] = names(m3)[10] = names(m4)[10] = 'obs_xweek'
  m1$metric <- '1week'; m2$metric <- '2week'; m3$metric <- '3week'; m4$metric <- '4week'
  m <- rbind(m1, m2, m3, m4)
  rm(m1, m2, m3, m4)
  
  m$obs_xweek_bin <- cut(m$obs_xweek, c(seq(0, 14000, by = bin.size), 100000))
  levels(m$obs_xweek_bin) <- seq(0, 14000, by = bin.size)
  m$obs_xweek_bin <- as.numeric(as.character(m$obs_xweek_bin))
  
  m <- m[!is.na(m$obs_xweek), ]
  m$obs_xweek_bin[m$obs_xweek == 0] <- 0
  
  # Merge:
  m <- unique(m[, c(1, 7:8, 12:13)])
  names(m)[4] <- 'metrics'
  e <- merge(e, m, by = c('season', 'country', 'fc_start', 'metrics'))
  
  # Find % of normal distribution within observed bin:
  e$lower <- e$obs_xweek_bin; e$upper <- e$obs_xweek_bin + bin.size
  e$upper[e$upper == 14000] <- 1e5
  probs <- sapply(1:dim(e)[1], function(ix) {
    pnorm(e[ix, 'upper'], mean = e[ix, 'mean'], sd = e[ix, 'sd']) - pnorm(e[ix, 'lower'], mean = e[ix, 'mean'], sd = e[ix, 'sd'])
  })
  e$prob <- probs
  
  # Calculate scores:
  e$score <- log(e$prob)
  print(e$score[e$score == -Inf])
  hist(e$score)
  
  # Reduce appropriately:
  e <- e[, c(1:8, 15)]
  
  # Get lead weeks:
  m <- read.csv(file = list.files(pattern = '_pro'))
  m$leadonset5 <- m$fc_start - m$onset5
  m <- m[!is.na(m$onsetObs5), ] # remove if no onset observed
  m <- unique(m[, c(1:5, 7:9, 15, 60, 67, 78)])
  
  e <- merge(e, m, by = c('season', 'country', 'run', 'oev_base', 'fc_start'))
  
  # Save:
  write.csv(e, file = paste0('logScores_1-4wk_alt2_', bin.size, '.csv'), row.names = FALSE)
  rm(e)
  
}





