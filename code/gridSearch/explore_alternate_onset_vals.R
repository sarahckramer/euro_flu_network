
library(reshape2); library(ggplot2)

# Focus mainly on metrics file for now:
m <- read.csv('code/gridSearch/outputs/outputMet_081919_pro.csv')

# Limit to needed columns:
m <- m[, c(1:8, 37:48, 99:101)]

m$FWeek_onwk3 <- m$fc_start - m$onsetObs3
m$FWeek_onwk4 <- m$fc_start - m$onsetObs4
m$FWeek_onwk5 <- m$fc_start - m$onsetObs5
m$FWeek_onwk6 <- m$fc_start - m$onsetObs6
m$FWeek_onwk <- NULL

m$leadonset3 <- m$fc_start - m$onset3
m$leadonset4 <- m$fc_start - m$onset4
m$leadonset5 <- m$fc_start - m$onset5
m$leadonset6 <- m$fc_start - m$onset6

m$abs_delta_onset3 <- abs(m$onset3 - m$onsetObs3)
m$abs_delta_onset4 <- abs(m$onset3 - m$onsetObs4)
m$abs_delta_onset5 <- abs(m$onset3 - m$onsetObs5)
m$abs_delta_onset6 <- abs(m$onset3 - m$onsetObs6)
m$abs_delta_onset <- NULL

# Make relevant values factors:
m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)
m$run <- factor(m$run)

# Now do the relevant analyses for each onset value
# (Can skip 500, since this was the "default")
by.pred = by.obs = maes = calib = by.country = vector('list', 4)

### First, onset 300:
# by predicted lead:
m3 <- m[!is.na(m$onsetObs3), ]

m.temp1 <- m3[m3$leadonset3 >= -6 & m3$leadonset3 < 6 & !is.na(m3$onset3), ]
m.temp1$leadonset3 <- factor(m.temp1$leadonset3)

res.all <- NULL
for (o1 in levels(m3$oev_base)) {
  for (l in levels(m3$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$leadonset3, m.temp$abs_delta_onset3)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset3 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '300'
by.pred[[1]] <- res

# and by obs:
m.temp1 <- m3[m3$FWeek_onwk3 >= -6 & m3$FWeek_onwk3 < 6 & !is.na(m3$onset3), ]
m.temp1$FWeek_onwk3 <- factor(m.temp1$FWeek_onwk3)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$FWeek_onwk3, m.temp$abs_delta_onset3)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk3 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '300'
by.obs[[1]] <- res

# log scores were calculated for onset 500 only; let's leave these for now

# MAEs:
m3.ot <- m3[m3$leadonset3 >= -6 & m3$leadonset3 < 7 & !is.na(m3$leadonset3), ]
m3.ot.agg <- aggregate(abs_delta_onset3 ~ leadonset3 + oev_base + lambda, data = m3.ot, FUN = mean)
names(m3.ot.agg)[c(1, 4)] <- c('lead', 'mae')
maes[[1]] <- m3.ot.agg

# calibration:
library(Hmisc)
a.dist.full <- read.csv('code/gridSearch/outputs/outputDist_081919.csv')
a.dist <- a.dist.full[a.dist.full$metric == 'onset3', ]

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

p <- c(0.005, 0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.375, 0.625, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.995)

countries <- levels(a.dist$country) # note: different order than that in which forecasts run!
a <- read.csv('code/gridSearch/outputs/outputMet_081919_pro.csv')
a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start',
               'onset3', 'onset4', 'onset5', 'onset6', 'onsetObs3', 'onsetObs4', 'onsetObs5', 'onsetObs6')]

a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_onwk <- a.dist$bin - a.dist$onsetObs3
a.dist$leadonwk_mean <- a.dist$fc_start - a.dist$onset3

a.onwk <- a.dist[a.dist$leadonwk_mean >= -6 & a.dist$leadonwk_mean < 4 & !is.na(a.dist$leadonwk_mean) & !is.na(a.dist$onset3) & !is.na(a.dist$onsetObs3), ]
a.onwk$leadonwk_bin <- cut(a.onwk$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

a.onwk <- a.onwk[a.onwk$bin != -1, ] # can only consider the weighted quantiles among ensemble members that actually predict an onset
a.onwk <- a.onwk[a.onwk$value > 0, ] # don't need weeks with 0% confidence

a.onwk$oev_base <- factor(a.onwk$oev_base)
a.onwk$oev_denom <- factor(a.onwk$oev_denom)
a.onwk$lambda <- factor(a.onwk$lambda)
levels(a.onwk$lambda)[1] <- '1.00'

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
  a.new <- merge(c5, unique(a.temp[, c('country', 'season', 'onsetObs3')]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
    for (lam in unique(a.new$lambda)) {
      a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
      # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
      
      denom <- length(a.new.temp$country)
      
      if (denom > 0) {
        
        p25 <- sum(a.new.temp$onsetObs3 >= a.new.temp$`37.5%` & a.new.temp$onsetObs3 <= a.new.temp$`62.5%`)
        p50 <- sum(a.new.temp$onsetObs3 >= a.new.temp$`25.0%` & a.new.temp$onsetObs3 <= a.new.temp$`75.0%`)
        p60 <- sum(a.new.temp$onsetObs3 >= a.new.temp$`20.0%` & a.new.temp$onsetObs3 <= a.new.temp$`80.0%`)
        p70 <- sum(a.new.temp$onsetObs3 >= a.new.temp$`15.0%` & a.new.temp$onsetObs3 <= a.new.temp$`85.0%`)
        p80 <- sum(a.new.temp$onsetObs3 >= a.new.temp$`10.0%` & a.new.temp$onsetObs3 <= a.new.temp$`90.0%`)
        p90 <- sum(a.new.temp$onsetObs3 >= a.new.temp$` 5.0%` & a.new.temp$onsetObs3 <= a.new.temp$`95.0%`)
        p95 <- sum(a.new.temp$onsetObs3 >= a.new.temp$` 2.5%` & a.new.temp$onsetObs3 <= a.new.temp$`97.5%`)
        p99 <- sum(a.new.temp$onsetObs3 >= a.new.temp$` 0.5%` & a.new.temp$onsetObs3 <= a.new.temp$`99.5%`)
        
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
dat.ot.temp$onset <- '300'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'onset')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)

calib[[1]] <- dat.ot.temp

# finally, by country:
m3.ot.agg.count <- aggregate(abs_delta_onset3 ~ leadonset3 + oev_base + lambda + country, data = m3.ot, FUN = mean)
m3.ot.agg.count$onset <- '300'
m3.ot.agg.count$group <- paste(m3.ot.agg.count$oev_base, m3.ot.agg.count$lambda, sep = '_'); m3.ot.agg.count$group <- factor(m3.ot.agg.count$group)
names(m3.ot.agg.count)[c(1, 5)] <- c('lead', 'abs_delta_onset')
by.country[[1]] <- m3.ot.agg.count

### 400:
m4 <- m[!is.na(m$onsetObs4), ]

m.temp1 <- m4[m4$leadonset4 >= -6 & m4$leadonset4 < 6 & !is.na(m4$onset4), ]
m.temp1$leadonset4 <- factor(m.temp1$leadonset4)

res.all <- NULL
for (o1 in levels(m4$oev_base)) {
  for (l in levels(m4$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$leadonset4, m.temp$abs_delta_onset4)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset4 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '400'
by.pred[[2]] <- res

# and by obs:
m.temp1 <- m4[m4$FWeek_onwk4 >= -6 & m4$FWeek_onwk4 < 6 & !is.na(m4$onset4), ]
m.temp1$FWeek_onwk4 <- factor(m.temp1$FWeek_onwk4)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$FWeek_onwk4, m.temp$abs_delta_onset4)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk4 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '400'
by.obs[[2]] <- res

# log scores were calculated for onset 500 only; let's leave these for now

# MAEs:
m4.ot <- m4[m4$leadonset4 >= -6 & m4$leadonset4 < 7 & !is.na(m4$leadonset4), ]
m4.ot.agg <- aggregate(abs_delta_onset4 ~ leadonset4 + oev_base + lambda, data = m4.ot, FUN = mean)
names(m4.ot.agg)[c(1, 4)] <- c('lead', 'mae')
maes[[2]] <- m4.ot.agg

# calibration:
a.dist <- a.dist.full[a.dist.full$metric == 'onset4', ]
a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_onwk <- a.dist$bin - a.dist$onsetObs4
a.dist$leadonwk_mean <- a.dist$fc_start - a.dist$onset4

a.onwk <- a.dist[a.dist$leadonwk_mean >= -6 & a.dist$leadonwk_mean < 4 & !is.na(a.dist$leadonwk_mean) & !is.na(a.dist$onset4) & !is.na(a.dist$onsetObs4), ]
a.onwk$leadonwk_bin <- cut(a.onwk$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

a.onwk <- a.onwk[a.onwk$bin != -1, ] # can only consider the weighted quantiles among ensemble members that actually predict an onset
a.onwk <- a.onwk[a.onwk$value > 0, ] # don't need weeks with 0% confidence

a.onwk$oev_base <- factor(a.onwk$oev_base)
a.onwk$oev_denom <- factor(a.onwk$oev_denom)
a.onwk$lambda <- factor(a.onwk$lambda)
levels(a.onwk$lambda)[1] <- '1.00'

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
  a.new <- merge(c5, unique(a.temp[, c('country', 'season', 'onsetObs4')]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
    for (lam in unique(a.new$lambda)) {
      a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
      # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
      
      denom <- length(a.new.temp$country)
      
      if (denom > 0) {
        
        p25 <- sum(a.new.temp$onsetObs4 >= a.new.temp$`37.5%` & a.new.temp$onsetObs4 <= a.new.temp$`62.5%`)
        p50 <- sum(a.new.temp$onsetObs4 >= a.new.temp$`25.0%` & a.new.temp$onsetObs4 <= a.new.temp$`75.0%`)
        p60 <- sum(a.new.temp$onsetObs4 >= a.new.temp$`20.0%` & a.new.temp$onsetObs4 <= a.new.temp$`80.0%`)
        p70 <- sum(a.new.temp$onsetObs4 >= a.new.temp$`15.0%` & a.new.temp$onsetObs4 <= a.new.temp$`85.0%`)
        p80 <- sum(a.new.temp$onsetObs4 >= a.new.temp$`10.0%` & a.new.temp$onsetObs4 <= a.new.temp$`90.0%`)
        p90 <- sum(a.new.temp$onsetObs4 >= a.new.temp$` 5.0%` & a.new.temp$onsetObs4 <= a.new.temp$`95.0%`)
        p95 <- sum(a.new.temp$onsetObs4 >= a.new.temp$` 2.5%` & a.new.temp$onsetObs4 <= a.new.temp$`97.5%`)
        p99 <- sum(a.new.temp$onsetObs4 >= a.new.temp$` 0.5%` & a.new.temp$onsetObs4 <= a.new.temp$`99.5%`)
        
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
dat.ot.temp$onset <- '400'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'onset')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)

calib[[2]] <- dat.ot.temp

# finally, by country:
m4.ot.agg.count <- aggregate(abs_delta_onset4 ~ leadonset4 + oev_base + lambda + country, data = m4.ot, FUN = mean)
m4.ot.agg.count$onset <- '400'
m4.ot.agg.count$group <- paste(m4.ot.agg.count$oev_base, m4.ot.agg.count$lambda, sep = '_'); m4.ot.agg.count$group <- factor(m4.ot.agg.count$group)
names(m4.ot.agg.count)[c(1, 5)] <- c('lead', 'abs_delta_onset')
by.country[[2]] <- m4.ot.agg.count

### 500 (for comparison):
m5 <- m[!is.na(m$onsetObs5), ]

m.temp1 <- m5[m5$leadonset5 >= -6 & m5$leadonset5 < 6 & !is.na(m5$onset5), ]
m.temp1$leadonset5 <- factor(m.temp1$leadonset5)

res.all <- NULL
for (o1 in levels(m5$oev_base)) {
  for (l in levels(m5$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$leadonset5, m.temp$abs_delta_onset5)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset5 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '500'
by.pred[[3]] <- res

# and by obs:
m.temp1 <- m5[m5$FWeek_onwk5 >= -6 & m5$FWeek_onwk5 < 6 & !is.na(m5$onset5), ]
m.temp1$FWeek_onwk5 <- factor(m.temp1$FWeek_onwk5)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$FWeek_onwk5, m.temp$abs_delta_onset5)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk5 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '500'
by.obs[[3]] <- res

# log scores were calculated for onset 500 only; let's leave these for now

# MAEs:
m5.ot <- m5[m5$leadonset5 >= -6 & m5$leadonset5 < 7 & !is.na(m5$leadonset5), ]
m5.ot.agg <- aggregate(abs_delta_onset5 ~ leadonset5 + oev_base + lambda, data = m5.ot, FUN = mean)
names(m5.ot.agg)[c(1, 4)] <- c('lead', 'mae')
maes[[3]] <- m5.ot.agg

# calibration:
a.dist <- a.dist.full[a.dist.full$metric == 'onset5', ]
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
  a.new <- merge(c5, unique(a.temp[, c('country', 'season', 'onsetObs5')]), by = c('country', 'season'))
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
dat.ot.temp$onset <- '500'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'onset')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)

calib[[3]] <- dat.ot.temp

# finally, by country:
m5.ot.agg.count <- aggregate(abs_delta_onset5 ~ leadonset5 + oev_base + lambda + country, data = m5.ot, FUN = mean)
m5.ot.agg.count$onset <- '500'
m5.ot.agg.count$group <- paste(m5.ot.agg.count$oev_base, m5.ot.agg.count$lambda, sep = '_'); m5.ot.agg.count$group <- factor(m5.ot.agg.count$group)
names(m5.ot.agg.count)[c(1, 5)] <- c('lead', 'abs_delta_onset')
by.country[[3]] <- m5.ot.agg.count

### 600:
m6 <- m[!is.na(m$onsetObs6), ]

m.temp1 <- m6[m6$leadonset6 >= -6 & m6$leadonset6 < 6 & !is.na(m6$onset6), ]
m.temp1$leadonset6 <- factor(m.temp1$leadonset6)

res.all <- NULL
for (o1 in levels(m6$oev_base)) {
  for (l in levels(m6$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$leadonset6, m.temp$abs_delta_onset6)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset6 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '600'
by.pred[[4]] <- res

# and by obs:
m.temp1 <- m6[m6$FWeek_onwk6 >= -6 & m6$FWeek_onwk6 < 6 & !is.na(m6$onset6), ]
m.temp1$FWeek_onwk6 <- factor(m.temp1$FWeek_onwk6)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l, ]
    
    mytable <- table(m.temp$FWeek_onwk6, m.temp$abs_delta_onset6)
    mytable <- prop.table(mytable, 1)[, 1:2]
    mytable <- melt(rowSums(mytable))
    mytable$lead <- as.numeric(rownames(mytable))
    res <- mytable
    
    num.fcasts.temp <- c()
    for (lead in res$lead) {
      num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk6 == lead]))
    }
    res$len <- num.fcasts.temp
    
    res$oev_base <- o1; res$lambda <- l
    res.all <- rbind(res.all, res)
  }
}

res <- melt(res.all, id.vars = c('lead', 'oev_base', 'lambda', 'len'))
res$group <- paste(res$oev_base, res$lambda, sep = '_'); res$group <- factor(res$group)
res$onset <- '600'
by.obs[[4]] <- res

# log scores were calculated for onset 500 only; let's leave these for now

# MAEs:
m6.ot <- m6[m6$leadonset6 >= -6 & m6$leadonset6 < 7 & !is.na(m6$leadonset6), ]
m6.ot.agg <- aggregate(abs_delta_onset6 ~ leadonset6 + oev_base + lambda, data = m6.ot, FUN = mean)
names(m6.ot.agg)[c(1, 4)] <- c('lead', 'mae')
maes[[4]] <- m6.ot.agg

# calibration:
a.dist <- a.dist.full[a.dist.full$metric == 'onset6', ]
a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_onwk <- a.dist$bin - a.dist$onsetObs6
a.dist$leadonwk_mean <- a.dist$fc_start - a.dist$onset6

a.onwk <- a.dist[a.dist$leadonwk_mean >= -6 & a.dist$leadonwk_mean < 4 & !is.na(a.dist$leadonwk_mean) & !is.na(a.dist$onset6) & !is.na(a.dist$onsetObs6), ]
a.onwk$leadonwk_bin <- cut(a.onwk$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

a.onwk <- a.onwk[a.onwk$bin != -1, ] # can only consider the weighted quantiles among ensemble members that actually predict an onset
a.onwk <- a.onwk[a.onwk$value > 0, ] # don't need weeks with 0% confidence

a.onwk$oev_base <- factor(a.onwk$oev_base)
a.onwk$oev_denom <- factor(a.onwk$oev_denom)
a.onwk$lambda <- factor(a.onwk$lambda)
levels(a.onwk$lambda)[1] <- '1.00'

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
  a.new <- merge(c5, unique(a.temp[, c('country', 'season', 'onsetObs6')]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
    for (lam in unique(a.new$lambda)) {
      a.new.temp <- a.new[a.new$oev_base == o1 & a.new$lambda == lam, ]
      # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
      
      denom <- length(a.new.temp$country)
      
      if (denom > 0) {
        
        p25 <- sum(a.new.temp$onsetObs6 >= a.new.temp$`37.5%` & a.new.temp$onsetObs6 <= a.new.temp$`62.5%`)
        p50 <- sum(a.new.temp$onsetObs6 >= a.new.temp$`25.0%` & a.new.temp$onsetObs6 <= a.new.temp$`75.0%`)
        p60 <- sum(a.new.temp$onsetObs6 >= a.new.temp$`20.0%` & a.new.temp$onsetObs6 <= a.new.temp$`80.0%`)
        p70 <- sum(a.new.temp$onsetObs6 >= a.new.temp$`15.0%` & a.new.temp$onsetObs6 <= a.new.temp$`85.0%`)
        p80 <- sum(a.new.temp$onsetObs6 >= a.new.temp$`10.0%` & a.new.temp$onsetObs6 <= a.new.temp$`90.0%`)
        p90 <- sum(a.new.temp$onsetObs6 >= a.new.temp$` 5.0%` & a.new.temp$onsetObs6 <= a.new.temp$`95.0%`)
        p95 <- sum(a.new.temp$onsetObs6 >= a.new.temp$` 2.5%` & a.new.temp$onsetObs6 <= a.new.temp$`97.5%`)
        p99 <- sum(a.new.temp$onsetObs6 >= a.new.temp$` 0.5%` & a.new.temp$onsetObs6 <= a.new.temp$`99.5%`)
        
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
dat.ot.temp$onset <- '600'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'onset')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)

calib[[4]] <- dat.ot.temp

# finally, by country:
m6.ot.agg.count <- aggregate(abs_delta_onset6 ~ leadonset6 + oev_base + lambda + country, data = m6.ot, FUN = mean)
m6.ot.agg.count$onset <- '600'
m6.ot.agg.count$group <- paste(m6.ot.agg.count$oev_base, m6.ot.agg.count$lambda, sep = '_'); m6.ot.agg.count$group <- factor(m6.ot.agg.count$group)
names(m6.ot.agg.count)[c(1, 5)] <- c('lead', 'abs_delta_onset')
by.country[[4]] <- m6.ot.agg.count

### Clean up environment:
rm(mytable, res, res.all, denom, i, l, lam, lead, num.fcasts.temp, o1, p, p25, p50, p60, p70, p80, p90, p95, p99, rmse, y, a.new, a.new.temp, a.onwk, a.temp,
   dat.ot.temp, dat.temp, dat.temp.all, a, m.temp, m.temp1, m3.ot, m3.ot.agg, m3.ot.agg.count, m4.ot, m4.ot.agg, m4.ot.agg.count, m5.ot, m5.ot.agg, m5.ot.agg.count,
   m6.ot, m6.ot.agg, m6.ot.agg.count)

### Compile and plot:
res.pred = res.obs = res.count = res.calib = res.mae = NULL
for (i in 1:4) {
  res.pred <- rbind(res.pred, by.pred[[i]])
  res.obs <- rbind(res.obs, by.obs[[i]])
  res.count <- rbind(res.count, by.country[[i]])
  res.calib <- rbind(res.calib, calib[[i]])
  
  mae.temp <- maes[[i]]
  mae.temp$onset <- c(300, 400, 500, 600)[i]
  res.mae <- rbind(res.mae, mae.temp)
}; rm(mae.temp)
res.mae$onset <- factor(res.mae$onset)

p1 <- ggplot(data = res.pred, aes(x = lead, y = value, group = onset, col = onset)) + geom_line() + geom_point(aes(size = len)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)', col = 'Onset', size = '# of Fcasts') +
  scale_color_brewer(palette = 'Set1')
p2 <- ggplot(data = res.obs, aes(x = lead, y = value, group = onset, col = onset)) + geom_line() + geom_point(aes(size = len)) +
  theme_bw() + facet_grid(lambda ~ oev_base) + labs(x = 'Observed Lead Week', y = '% Accurate (OT)', col = 'Onset', size = '# of Fcasts') +
  scale_color_brewer(palette = 'Set1')
p3 <- ggplot(data = res.mae, aes(x = lead, y = mae, group = onset, col = onset)) + geom_line() + geom_point(size = 2.0) +
  theme_bw() + facet_grid(lambda ~ oev_base) + labs(x = 'Predicted Lead Week', y = 'MAE (OT)', col = 'Onset') +
  scale_color_brewer(palette = 'Set1')
p5 <- ggplot(data = res.count, aes(x = lead, y = abs_delta_onset, group = group, col = group)) + geom_line() + geom_point() +
  theme_bw() + facet_grid(country ~ onset) + labs(x = 'Predicted Lead Week', y = 'MAE (OT)', col = 'OEV_base/Lambda')# +
  # scale_color_brewer(palette = 'Set1')

pdf('code/gridSearch/plots/alt_onsets_082119.pdf', width = 14, height = 9)
print(p1) # lower onsets a bit better after onset, but no strong patterns except at higher lambdas, where lower onsets consistently better; still more fcasts w/ 1e4
print(p2) # similar; lower onsets better across all lambdas, esp. for oev_base 1e4
print(p3) # errors lower for lower values, esp for oev_base 1e5 and at higher lambda

res.calib$onset <- factor(res.calib$onset)
for (baseline in levels(res.calib$onset)) {
  print(baseline)
  res.calib.temp <- res.calib[res.calib$onset == baseline, ]
  
  p4 <- ggplot(data = res.calib.temp, aes(x = quantile, y = y, color = lead, group = lead)) +
    geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
    geom_line() + geom_point(aes(size = len)) + theme_bw() +
    labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:', len = '# Fcasts', title = paste0('OT (', baseline, ')')) +
    theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
          strip.text = element_text(size = 10), axis.title = element_text(size = 12),
          legend.title = element_text(size = 12)) +
    scale_color_brewer(palette = 'Set1') +
    scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    facet_grid(oev_base ~ lambda)
  print(p4)
}
# calibration generally better for oev_base 1e5, but difference becomes less at lower onset

# print(p5) # either no difference, or larger worse; difference in onset makes larger change when oev_base is 1e5
dev.off()

# In general, I really don't see a reason to change our baseline value at this point






