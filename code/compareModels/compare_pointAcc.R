
# Read in libraries:
library(reshape2); library(ggplot2); library(gridExtra)

# Read in and compile metrics files:
source('code/compareModels/readIn_metrics.R')

# Read in functions for plotting:
source('code/gridSearch/comp_netVsIndiv/plotting_functions.R')

### Plotting ###
# Get subsets of interest for PT/PI, and for OT:
m.temp1 <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$onset5), ]
m.temp1$leadpkwk_mean <- factor(m.temp1$leadpkwk_mean)

m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 7 & !is.na(m$onset5), ]
m.temp2$leadonset5 <- factor(m.temp2$leadonset5)

# Same for observed leads:
m.temp3 <- m[m$FWeek_pkwk >= -6 & m$FWeek_pkwk < 5 & !is.na(m$onset5), ]
m.temp3$FWeek_pkwk <- factor(m.temp3$FWeek_pkwk)

m.temp4 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 7 & !is.na(m$onset5), ]
m.temp4$FWeek_onwk <- factor(m.temp4$FWeek_onwk)

# Format all results for plotting:
res.pred = res.obs = NULL
for (o1 in levels(m$oev_base)) {
  for (model.type in levels(m$model)) {
    m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$model == model.type, ]
    res.pt <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_pkwk_mean')
    res.pt$metric <- 'pt'
    res.pi <- format_for_plot(m.temp, 'leadpkwk_mean', 'abs_delta_peak_int_bin')
    res.pi$metric <- 'pi'
    
    m.temp <- m.temp2[m.temp2$oev_base == o1 & m.temp2$model == model.type, ]
    res.ot <- format_for_plot(m.temp, 'leadonset5', 'abs_delta_onset')
    res.ot$metric <- 'ot'
    
    res <- rbind(res.pt, res.pi, res.ot)
    res$oev_base <- o1; res$model <- model.type
    res.pred <- rbind(res.pred, res)
    
    m.temp <- m.temp3[m.temp3$oev_base == o1 & m.temp3$model == model.type, ]
    res.pt <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_pkwk_mean')
    res.pt$metric <- 'pt'
    res.pi <- format_for_plot(m.temp, 'FWeek_pkwk', 'abs_delta_peak_int_bin')
    res.pi$metric <- 'pi'
    
    m.temp <- m.temp4[m.temp4$oev_base == o1 & m.temp4$model == model.type, ]
    res.ot <- format_for_plot(m.temp, 'FWeek_onwk', 'abs_delta_onset')
    res.ot$metric <- 'ot'
    
    res <- rbind(res.pt, res.pi, res.ot)
    res$oev_base <- o1; res$model <- model.type
    res.obs <- rbind(res.obs, res)
  }
}
res.pred$model <- factor(res.pred$model); res.pred$model <- factor(res.pred$model, levels = levels(res.pred$model)[c(3, 2, 4, 1)])
res.obs$model <- factor(res.obs$model); res.obs$model <- factor(res.obs$model, levels = levels(res.obs$model)[c(3, 2, 4, 1)])

# Clean-up:
rm(m.temp, res, res.pt, res.pi, res.ot, model.type, o1)

# Plots by predicted:
p1 <- ggplot(data = res.pred[res.pred$metric == 'pt', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.pred$lead), labels = unique(res.pred$lead))
p2 <- ggplot(data = res.pred[res.pred$metric == 'pi', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.pred$lead), labels = unique(res.pred$lead))
p3 <- ggplot(data = res.pred[res.pred$metric == 'ot', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.pred$lead), labels = unique(res.pred$lead))

pdf('results/plots/comp_byPred.pdf', width = 14, height = 9)
print(p1); print(p2); print(p3)
dev.off()

# Plots by observed:
p1 <- ggplot(data = res.obs[res.obs$metric == 'pt', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Peak Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.obs$lead), labels = unique(res.obs$lead))
p2 <- ggplot(data = res.obs[res.obs$metric == 'pi', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 25%)', title = 'Peak Intensity Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.obs$lead), labels = unique(res.obs$lead))
p3 <- ggplot(data = res.obs[res.obs$metric == 'ot', ], aes(x = lead, y = value, group = model, col = model)) + geom_line() + 
  geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') + facet_wrap(~ oev_base, ncol = 1) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (within 1 week)', title = 'Onset Timing Accuracy') +
  coord_cartesian(ylim = c(0,1)) + scale_x_continuous(breaks = unique(res.obs$lead), labels = unique(res.obs$lead))

pdf('results/plots/comp_byObs.pdf', width = 14, height = 9)
print(p1); print(p2); print(p3)
dev.off()

# Clean up:
rm(list = ls())

