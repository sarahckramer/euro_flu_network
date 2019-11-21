
# Plot accuracy by observed lead by model/oev_base/lambda:
m.temp1 <- m[m$FWeek_pkwk >= -8 & m$FWeek_pkwk < 5 & !is.na(m$onset5), ]
m.temp1$FWeek_pkwk <- factor(m.temp1$FWeek_pkwk)

m.temp2 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7 & !is.na(m$onset5), ]
m.temp2$FWeek_onwk <- factor(m.temp2$FWeek_onwk)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    for (model.type in levels(m$model)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l  & m.temp1$model == model.type, ]
      m.temp.ot <- m.temp2[m.temp2$oev_base == o1 & m.temp2$lambda == l  & m.temp2$model == model.type,]
      
      if (dim(m.temp)[1] > 0) {
        res.pt <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_pkwk_mean')
        res.pt$metric <- 'pt'
        
        res.pi <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_peak_int_bin')
        res.pi$metric <- 'pi'
        
        res.ot <- format_for_plot(m.temp.ot, 'FWeek_onwk', 'abs_delta_onset')
        res.ot$metric <- 'ot'
        
        res <- rbind(res.pt, res.pi, res.ot)
        res$oev_base <- o1; res$lambda <- l; res$model <- model.type
        res.all <- rbind(res.all, res)
      }
      
    }
    
  }
}

res.all$group <- paste(res.all$metric, res.all$oev_base, res.all$lambda, res.all$model, sep = '_'); res.all$group <- factor(res.all$group)

p1 <- ggplot(data = res.all[res.all$metric == 'pt', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p2 <- ggplot(data = res.all[res.all$metric == 'pi', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p3 <- ggplot(data = res.all[res.all$metric == 'ot', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))

plots.by.obs <- list(p1, p2, p3)
rm(m.temp, m.temp.ot, m.temp1, m.temp2, res, res.all, res.ot, res.pt, res.pi, o1, l, model.type, p1, p2, p3)
