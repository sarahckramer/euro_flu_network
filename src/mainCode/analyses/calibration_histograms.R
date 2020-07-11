### Alternative calibration assessment ###
library(ggplot2); library(gridExtra)

# Read in results:
m1 <- read.csv('results/network/outputMet_pro_PROC.csv')
m2 <- read.csv('results/isolated/outputMet_pro_PROC.csv')

# Combine files:
m1$model <- 'Network'; m2$model <- 'Isolated'
m <- rbind(m1, m2); rm(m1, m2)

# Remove where no observed onset:
m <- m[!is.na(m$onsetObs5), ]

# Reduce data frame:
m <- m[, c(1:2, 6:12, 14:16, 30:32, 38, 45, 48:50, 56:58)]
m$model <- factor(m$model)
m$model <- factor(m$model, levels = levels(m$model)[2:1])

# Restrict to relevant lead weeks:
m.temp <- m[m$leadpkwk_mean >= -6 & m$leadpkwk_mean < 5 & !is.na(m$onset5), ]
m.temp2 <- m[m$leadonset5 >= -6 & m$leadonset5 < 5 & !is.na(m$leadonset5), ]

# Bin lead weeks:
m.temp$leadpkwk_bin <- cut(m.temp$leadpkwk_mean, c(-7, -5, -3, -1, 2, 4))
m.temp2$leadonset_bin <- cut(m.temp2$leadonset5, c(-7, -5, -3, -1, 2, 4))

# Calculate relative error for PI:
m.temp$delta_int <- m.temp$intensity_err / m.temp$obs_peak_int

# Plot!
p1 <- ggplot(data = m.temp, aes(x = delta_pkwk_mean, y = ..density.., group = model, fill = model)) +
  geom_histogram(binwidth = 1.0, position = 'identity', alpha = 0.5) +
  # geom_freqpoly(binwidth = 1.0) +
  theme_bw() + theme(aspect.ratio = 1,
                     axis.text = element_text(size = 11),
                     strip.text = element_text(size = 13),
                     axis.title = element_text(size = 13),
                     plot.title = element_text(size = 16)) +
  labs(title = 'A     (Peak Timing)', x = 'Error (Fcast - Obs)', y = 'Proportion of Fcasts', fill = 'Model:', col = 'Model:') +
  facet_grid(~ leadpkwk_bin) + scale_fill_brewer(palette = 'Set1') + scale_color_brewer(palette = 'Set1')

p2 <- ggplot(data = m.temp, aes(x = delta_int, y = 0.1 * ..density.., group = model, fill = model)) +
  geom_histogram(binwidth = 0.1, position = 'identity', alpha = 0.5, lwd = 0.5) +
  theme_bw() + theme(aspect.ratio = 1,
                     axis.text = element_text(size = 11),
                     strip.text = element_text(size = 13),
                     axis.title = element_text(size = 13),
                     plot.title = element_text(size = 16)) +
  labs(title = 'B     (Peak Intensity)', x = 'Error ((Fcast - Obs) / Obs)', y = 'Proportion of Fcasts', fill = 'Model:', col = 'Model:') +
  facet_grid(~ leadpkwk_bin) + scale_fill_brewer(palette = 'Set1') + scale_color_brewer(palette = 'Set1')

# grid.arrange(p1, p2)

# p3 <- ggplot(data = m.temp2, aes(x = delta_onset5, y = ..density.., group = model, fill = model, col = model)) +
#   geom_histogram(binwidth = 1.0, position = 'identity', alpha = 0.5) +
#   theme_bw() + theme(aspect.ratio = 1,
#                      axis.text = element_text(size = 11),
#                      strip.text = element_text(size = 13),
#                      axis.title = element_text(size = 13),
#                      plot.title = element_text(size = 16)) +
#   labs(title = 'C     (Onset Timing)', x = 'Error (Fcast - Obs)', y = 'Proportion of Fcasts', fill = 'Model', col = 'Model') +
#   facet_grid(~ leadonset_bin) + scale_fill_brewer(palette = 'Set1') + scale_color_brewer(palette = 'Set1')

pdf('results/plots/FigS10.pdf', width = 14, height = 6)
grid.arrange(p1, p2)
dev.off()

# ggsave('results/plots/FigS10.svg', plot = arrangeGrob(p1, p2, ncol = 1), width = 14, height = 6)
# for some reason svg seems to squish the plot - use pdf and convert

rm(list = ls())
