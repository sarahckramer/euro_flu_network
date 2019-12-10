
# # set all lambdas equal, so that loops still work
# m$lambda <- 1.02; m$lambda <- factor(m$lambda)

# Plot accuracy by predicted lead by model/oev_base/lambda:
m.temp1 <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$onset5), ]
m.temp1$leadpkwk_mean <- factor(m.temp1$leadpkwk_mean)

m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$onset5), ]
m.temp2$leadonset5 <- factor(m.temp2$leadonset5)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (l in levels(m$lambda)) {
    for (model.type in levels(m$model)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$lambda == l  & m.temp1$model == model.type, ]
      m.temp.ot <- m.temp2[m.temp2$oev_base == o1 & m.temp2$lambda == l  & m.temp2$model == model.type,]
      
      if (dim(m.temp)[1] > 0) {
        res.pt <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_pkwk_mean')
        res.pt$metric <- 'pt'
        
        res.pi <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_peak_int_bin')
        res.pi$metric <- 'pi'
        
        res.ot <- format_for_plot(m.temp.ot, 'leadonset5', 'abs_delta_onset')
        res.ot$metric <- 'ot'
        
        res <- rbind(res.pt, res.pi, res.ot)
        res$oev_base <- o1; res$lambda <- l; res$model <- model.type
        res.all <- rbind(res.all, res)
      }
      
    }
    
  }
}

res.all$model <- factor(res.all$model, levels = c(m1.lab, m2.lab, m3.lab))
res.all$group <- paste(res.all$metric, res.all$oev_base, res.all$lambda, res.all$model, sep = '_'); res.all$group <- factor(res.all$group)

p1 <- ggplot(data = res.all[res.all$metric == 'pt', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p2 <- ggplot(data = res.all[res.all$metric == 'pi', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))
p3 <- ggplot(data = res.all[res.all$metric == 'ot', ], aes(x = lead, y = value, group = group, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_grid(lambda ~ oev_base) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.all$lead), labels = unique(res.all$lead))

plots.by.pred <- list(p1, p2, p3)
rm(m.temp, m.temp.ot, m.temp1, m.temp2, res, res.all, res.ot, res.pt, res.pi, o1, l, model.type, p1, p2, p3)
