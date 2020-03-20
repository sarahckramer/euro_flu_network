
### Function for basic "accuracy" plotting:
format_for_plot <- function(df, xvar, yvar) {
  mytable <- table(df[, xvar], df[, yvar])
  mytable <- prop.table(mytable, 1)[, 1:2]
  mytable <- melt(rowSums(mytable))
  mytable$lead <- as.numeric(rownames(mytable))
  res <- mytable
  
  num.fcasts.temp <- c()
  for (lead in res$lead) {
    num.fcasts.temp <- c(num.fcasts.temp, length(df[, 1][df[, xvar] == lead]))
  }
  res$len <- num.fcasts.temp
  
  return(res)
}

### Functions to calculate calibration from "dist" files:
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

### Functions to format dist (PT/OT) data for plotting:
format_Dist <- function(d1, d2, m_red) {
  d1$model <- 'Network'
  d2$model <- 'Individual'
  
  d1$bin <- d1$bin + 40 - 1
  d1$bin[d1$bin == 38] <- -1
  names(d1)[c(8, 10)] <- c('week', 'prob')
  
  d2$gamma <- NULL
  
  d <- rbind(d1, d2)
  d <- merge(d, m_red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'model'))
  
  d$delta_pkwk <- d$week - d[, 'obs_pkwk']
  
  d.red <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 4 & !is.na(d$leadpkwk_mean) & !is.na(d$onset5), ]
  d.red$leadpkwk_bin <- cut(d.red$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
  
  d.red <- d.red[d.red$week != -1, ]
  d.red <- d.red[d.red$prob > 0, ] # don't need weeks with 0% confidence
  
  d.red$oev_base <- factor(d.red$oev_base)
  d.red$oev_denom <- factor(d.red$oev_denom)
  d.red$lambda <- factor(d.red$lambda)
  levels(d.red$lambda)[1] <- '1.02'
  
  return(d.red)
}

format_Dist_OT <- function(d1, d2, m_red) {
  d1$model <- 'Network'
  d2$model <- 'Individual'
  
  d1$bin <- d1$bin + 40 - 1
  d1$bin[d1$bin == 38] <- -1
  names(d1)[c(8, 10)] <- c('week', 'prob')
  
  d2$week <- d2$week + 40 - 1
  d2$week[d2$week == 38] <- -1
  d2$gamma <- NULL
  
  d <- rbind(d1, d2)
  d <- merge(d, m_red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'model'))
  
  d$delta_onwk <- d$week - d[, 'onsetObs5']
  d$leadonwk_mean <- d$fc_start - d[, 'onset5']
  
  d.red <- d[d$leadonwk_mean >= -6 & d$leadonwk_mean < 4 & !is.na(d$leadonwk_mean) & !is.na(d$onset5), ]
  d.red$leadonwk_bin <- cut(d.red$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))
  
  d.red <- d.red[d.red$week != -1, ]
  d.red <- d.red[d.red$prob > 0, ] # don't need weeks with 0% confidence
  
  d.red$oev_base <- factor(d.red$oev_base)
  d.red$oev_denom <- factor(d.red$oev_denom)
  d.red$lambda <- factor(d.red$lambda)
  levels(d.red$lambda)[1] <- '1.02'
  
  return(d.red)
}

format_Dist_qual <- function(d1, d2, m_red) {
  d1$model <- 'Network'
  d2$model <- 'Individual'
  
  d2$gamma <- NULL
  names(d2)[7] <- 'metric'
  
  d <- rbind(d1, d2)
  d <- merge(d, m_red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'model'))
  
  d[, c(10:309)] <- d[, c(10:309)] / d[, 310]
  d$scaling <- NULL
  
  d.red <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 4 & !is.na(d$leadpkwk_mean) & !is.na(d$onset5), ]
  d.red$leadpkwk_bin <- cut(d.red$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
  
  d.red$oev_base <- factor(d.red$oev_base)
  d.red$oev_denom <- factor(d.red$oev_denom)
  d.red$lambda <- factor(d.red$lambda)
  levels(d.red$lambda)[1] <- '1.02'
  
  return(d.red)
}

format_Dist_Wks <- function(d1, d2, m_red, met) {
  d1$model <- 'Network'
  d2$model <- 'Individual'
  
  d2$gamma <- NULL
  names(d2)[7] <- 'metric'
  
  d <- rbind(d1, d2)
  d <- merge(d, m_red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'model'))
  
  d[, c(10:309)] <- d[, c(10:309)] / d[, 310]
  d$scaling <- NULL
  
  d <- d[!is.na(d[, met]) & d[, met] > 0, ]
  d <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 4 & !is.na(d$leadpkwk_mean), ]
  d$leadpkwk_bin <- cut(d$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))
  
  d$oev_base <- factor(d$oev_base)
  d$oev_denom <- factor(d$oev_denom)
  d$lambda <- factor(d$lambda)
  levels(d$lambda)[1] <- '1.02'
  
  return(d)
}

process_Lead <- function(lead.bin, d, metric) {
  if (metric == 'pt') {
    d.temp <- d[d$leadpkwk_bin == lead.bin, ]
  } else if (metric == 'ot') {
    d.temp <- d[d$leadonwk_bin == lead.bin, ]
  }
  
  c1 <- split(d.temp[, c('week', 'prob')], d.temp[, c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'model')])
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
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'), c4[, 10], paste(c4[, 11], c4[, 12], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'model', 'quantile')
  c5 <- dcast(c4, model + country + season + run + fc_start + oev_base + oev_denom + lambda ~ quantile, value.var = 'value')
  
  if (metric == 'pt') {
    d.new <- merge(c5, unique(d.temp[, c(1:2, 12)]), by = c('country', 'season'))
  } else if (metric == 'ot') {
    d.new <- merge(c5, unique(d.temp[, c(1:2, 14)]), by = c('country', 'season'))
  }
  
  return(d.new)
}

get_calibration_Dist <- function(d, met, denom, lead, o1, lam, model) {
  
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
  
  dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8), rep(model, 8)))
  return(dat.temp)
}

get_calibration_Dist_int <- function(d, met, denom, lead, o1, lam, model) {
  
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
  
  dat.temp <- as.data.frame(cbind(c(25, 50, 60, 70, 80, 90, 95, 99), y, rep(lead, 8), rep(rmse, 8), rep(denom, 8), rep(o1, 8), rep(lam, 8), rep(model, 8)))
  return(dat.temp)
}

format_calib_output <- function(d, met.name) {
  d[, 1] <- as.numeric(as.character(d[, 1]))
  d[, 2] <- as.numeric(as.character(d[, 2]))
  d[, 4] <- as.numeric(as.character(d[, 4]))
  d[, 5] <- as.numeric(as.character(d[, 5]))
  
  d$metric <- met.name
  names(d) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'lambda', 'model', 'metric')
  d$lambda <- NULL
  
  d$model <- factor(d$model, levels = levels(d$model)[c(3:4, 2, 1)])
  
  return(d)
}



