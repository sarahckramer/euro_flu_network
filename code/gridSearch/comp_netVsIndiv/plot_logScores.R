
if (byWeek == 'Predicted') {
  # Start with PT:
  # Think for most of these, we can collapse the lambdas, since they don't make much of a difference
  d.temp <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
  # d.temp <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 5 & d$metric == 'pt', ]
  d.temp$group <- paste(d.temp$leadpkwk_mean, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  d.agg <- aggregate(score ~ leadpkwk_mean + oev_base + model, data = d.temp, FUN = median)
  
  p1 <- ggplot(data = d.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Log Score', title = 'Peak Timing') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = d.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = d.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = model)) +
    geom_point(aes(x = leadpkwk_mean, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # PI:
  e.temp <- e.pi[e.pi$leadpkwk_mean >= -8 & e.pi$leadpkwk_mean < 5 & !is.na(e.pi$leadonset5), ]
  # e.temp <- e.pi[e.pi$leadpkwk_mean >= -8 & e.pi$leadpkwk_mean < 5, ]
  e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
  e.agg <- aggregate(score ~ leadpkwk_mean + oev_base + model, data = e.temp, FUN = median)
  
  p1 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Log Score', title = 'Peak Intensity') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = model)) +
    geom_point(aes(x = leadpkwk_mean, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # OT:
  # d.temp <- d[d$leadonset5 >= -6 & d$leadonset5 < 7 & d$metric == 'ot' & !is.na(d$leadonset5), ]
  d.temp <- d[((d$leadonset5 >= -6 & d$leadonset5 < 7) | is.na(d$leadonset5)) & d$metric == 'ot', ]
  d.temp$group <- paste(d.temp$leadonset5, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  d.agg <- aggregate(score ~ leadonset5 + oev_base + model, data = d.temp, FUN = median)
  
  p1 <- ggplot(data = d.temp) + geom_boxplot(aes(x = leadonset5, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Log Score', title = 'Onset Timing') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = d.temp) + geom_boxplot(aes(x = leadonset5, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = d.agg) + geom_line(aes(x = leadonset5, y = score, col = model)) +
    geom_point(aes(x = leadonset5, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # 1-4 weeks:
  for (wk in levels(e$metric)) {
    e.temp <- e[e$metric == wk & e$leadpkwk_mean >= -8 & e$leadpkwk_mean < 5, ]
    names(e.temp)[8] <- 'score'
    e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
    e.agg <- aggregate(score ~ leadpkwk_mean + oev_base + model, data = e.temp, FUN = median)
    
    p1 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = group, fill = model)) +
      facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Predicted Lead Week', y = 'Log Score', title = wk) + scale_x_continuous(breaks = -8:4)
    p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = model)) +
      facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Predicted Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
    p3 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = model)) +
      geom_point(aes(x = leadpkwk_mean, y = score, col = model)) + facet_wrap(~oev_base) +
      theme_bw() + scale_color_brewer(palette = 'Set1') +
      labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
    grid.arrange(p1, p2, p3)
    
  }
  
  # # FOR PRESENTATION:
  # d.temp <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
  # d.temp$group <- paste(d.temp$leadpkwk_mean, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  # d.agg <- aggregate(score ~ leadpkwk_mean + model, data = d.temp, FUN = median)
  # d.agg.c <- aggregate(score ~ leadpkwk_mean + model, data = d.temp, FUN = length)
  # d.agg <- merge(d.agg, d.agg.c, by = c('leadpkwk_mean', 'model'))
  # 
  # p1 <- ggplot(data = d.agg) + geom_line(aes(x = leadpkwk_mean, y = score.x, col = model)) +
  #   geom_point(aes(x = leadpkwk_mean, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # e.temp <- e.pi[e.pi$leadpkwk_mean >= -8 & e.pi$leadpkwk_mean < 5 & !is.na(e.pi$leadonset5), ]
  # e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
  # e.agg <- aggregate(score ~ leadpkwk_mean + model, data = e.temp, FUN = median)
  # e.agg.c <- aggregate(score ~ leadpkwk_mean + model, data = e.temp, FUN = length)
  # e.agg <- merge(e.agg, e.agg.c, by = c('leadpkwk_mean', 'model'))
  # 
  # p2 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score.x, col = model)) +
  #   geom_point(aes(x = leadpkwk_mean, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # d.temp <- d[((d$leadonset5 >= -6 & d$leadonset5 < 7) | is.na(d$leadonset5)) & d$metric == 'ot', ]
  # d.temp$group <- paste(d.temp$leadonset5, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  # d.agg <- aggregate(score ~ leadonset5 + model, data = d.temp, FUN = median)
  # d.agg.c <- aggregate(score ~ leadonset5 + model, data = d.temp, FUN = length)
  # d.agg <- merge(d.agg, d.agg.c, by = c('leadonset5', 'model'))
  # 
  # p3 <- ggplot(data = d.agg) + geom_line(aes(x = leadonset5, y = score.x, col = model)) +
  #   geom_point(aes(x = leadonset5, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Predicted Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # pdf('code/gridSearch/plots/pres_comp_logScores_byPred.pdf', width = 8, height = 9)
  # grid.arrange(p1, p2, p3)
  # dev.off()
  
} else if (byWeek == 'Observed') {
  # Start with PT:
  # d.temp <- d[d$FWeek_pkwk >= -8 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$FWeek_onwk), ]
  d.temp <- d[d$FWeek_pkwk >= -8 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
  d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  d.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model, data = d.temp, FUN = median)
  
  p1 <- ggplot(data = d.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Timing') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = d.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = d.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = model)) +
    geom_point(aes(x = FWeek_pkwk, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # PI:
  # e.temp <- e.pi[e.pi$FWeek_pkwk >= -8 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$FWeek_onwk), ]
  e.temp <- e.pi[e.pi$FWeek_pkwk >= -8 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$leadonset5), ]
  e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
  e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model, data = e.temp, FUN = median)
  
  p1 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Peak Intensity') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = model)) +
    geom_point(aes(x = FWeek_pkwk, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # OT:
  d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot' & !is.na(d$FWeek_onwk), ]
  d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  d.agg <- aggregate(score ~ FWeek_onwk + oev_base + model, data = d.temp, FUN = median)
  
  p1 <- ggplot(data = d.temp) + geom_boxplot(aes(x = FWeek_onwk, y = score, group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Log Score', title = 'Onset Timing') + scale_x_continuous(breaks = -8:4)
  p2 <- ggplot(data = d.temp) + geom_boxplot(aes(x = FWeek_onwk, y = exp(score), group = group, fill = model)) +
    facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
  p3 <- ggplot(data = d.agg) + geom_line(aes(x = FWeek_onwk, y = score, col = model)) +
    geom_point(aes(x = FWeek_onwk, y = score, col = model)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  grid.arrange(p1, p2, p3)
  
  # 1-4 weeks:
  for (wk in levels(e$metric)) {
    e.temp <- e[e$metric == wk & e$FWeek_pkwk >= -8 & e$FWeek_pkwk < 5, ]
    names(e.temp)[8] <- 'score'
    e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
    e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + model, data = e.temp, FUN = median)
    
    p1 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = group, fill = model)) +
      facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Observed Lead Week', y = 'Log Score', title = wk) + scale_x_continuous(breaks = -8:4)
    p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = group, fill = model)) +
      facet_wrap(~ oev_base, ncol = 2) + theme_bw() + scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Observed Lead Week', y = 'e^(Log Score)') + scale_x_continuous(breaks = -8:4)
    p3 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = model)) +
      geom_point(aes(x = FWeek_pkwk, y = score, col = model)) + facet_wrap(~oev_base) +
      theme_bw() + scale_color_brewer(palette = 'Set1') +
      labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
    grid.arrange(p1, p2, p3)
    
  }
  # these include where no onset is predicted; don't have predicted onset recorded in e
  
  # # FOR PRESENTATION:
  # d.temp <- d[d$FWeek_pkwk >= -8 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
  # d.temp$group <- paste(d.temp$FWeek_pkwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  # d.agg <- aggregate(score ~ FWeek_pkwk + model, data = d.temp, FUN = median)
  # d.agg.c <- aggregate(score ~ FWeek_pkwk + model, data = d.temp, FUN = length)
  # d.agg <- merge(d.agg, d.agg.c, by = c('FWeek_pkwk', 'model'))
  # 
  # p1 <- ggplot(data = d.agg) + geom_line(aes(x = FWeek_pkwk, y = score.x, col = model)) +
  #   geom_point(aes(x = FWeek_pkwk, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # e.temp <- e.pi[e.pi$FWeek_pkwk >= -8 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$leadonset5), ]
  # e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$model, sep = '_'); e.temp$group <- factor(e.temp$group)
  # e.agg <- aggregate(score ~ FWeek_pkwk + model, data = e.temp, FUN = median)
  # e.agg.c <- aggregate(score ~ FWeek_pkwk + model, data = e.temp, FUN = length)
  # e.agg <- merge(e.agg, e.agg.c, by = c('FWeek_pkwk', 'model'))
  # 
  # p2 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score.x, col = model)) +
  #   geom_point(aes(x = FWeek_pkwk, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # # d.temp <- d[((d$FWeek_onwk >= -6 & d$FWeek_onwk < 7) | is.na(d$leadonset5)) & d$metric == 'ot', ]
  # d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot', ]
  # d.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & d$metric == 'ot' & !is.na(d$leadonset5), ]
  # d.temp$group <- paste(d.temp$FWeek_onwk, d.temp$model, sep = '_'); d.temp$group <- factor(d.temp$group)
  # d.agg <- aggregate(score ~ FWeek_onwk + model, data = d.temp, FUN = median)
  # d.agg.c <- aggregate(score ~ FWeek_onwk + model, data = d.temp, FUN = length)
  # d.agg <- merge(d.agg, d.agg.c, by = c('FWeek_onwk', 'model'))
  # 
  # p3 <- ggplot(data = d.agg) + geom_line(aes(x = FWeek_onwk, y = score.x, col = model)) +
  #   geom_point(aes(x = FWeek_onwk, y = score.x, col = model, size = score.y)) +
  #   theme_bw() + scale_color_brewer(palette = 'Set1') + guides(size = FALSE) +
  #   labs(x = 'Observed Lead Week', y = 'Median Log Score') + scale_x_continuous(breaks = -8:4)
  # 
  # pdf('code/gridSearch/plots/pres_comp_logScores_byObs_remNoOnset.pdf', width = 8, height = 9)
  # grid.arrange(p1, p2, p3)
  # dev.off()
  
} else {
  print('Error.')
}
rm(d.agg, d.temp, e.agg, e.temp, p1, p2, p3, wk)
