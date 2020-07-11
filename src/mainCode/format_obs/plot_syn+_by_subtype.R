### Plot all (sub)type-specific syndromic+ data (unscaled) ###
library(reshape2)

# Read in data:
iliiso.h1 <- read.csv('data/WHO_data_A(H1).csv')
iliiso.h3 <- read.csv('data/WHO_data_A(H3).csv')
iliiso.b <- read.csv('data/WHO_data_B.csv')

# iliiso.h1 <- read.csv('data/WHO_data_A(H1)_SCALED.csv')
# iliiso.h3 <- read.csv('data/WHO_data_A(H3)_SCALED.csv')
# iliiso.b <- read.csv('data/WHO_data_B_SCALED.csv')

# Limit to countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

iliiso.h1 <- iliiso.h1[, c(1, count.indices + 1)]
iliiso.h3 <- iliiso.h3[, c(1, count.indices + 1)]
iliiso.b <- iliiso.b[, c(1, count.indices + 1)]
rm(count.indices)

# Also limit to 2010-11 season and forward:
iliiso.h1 <- iliiso.h1[79:495, ]
iliiso.h3 <- iliiso.h3[79:495, ]
iliiso.b <- iliiso.b[79:495, ]

# Replace "time" with integer marker:
iliiso.h1$time <- 1:417
iliiso.h3$time <- 1:417
iliiso.b$time <- 1:417

# Melt and combine:
iliiso.h1 <- melt(iliiso.h1, id.vars = 'time')
iliiso.h3 <- melt(iliiso.h3, id.vars = 'time')
iliiso.b <- melt(iliiso.b, id.vars = 'time')

iliiso.h1$subtype <- 'H1'
iliiso.h3$subtype <- 'H3'
iliiso.b$subtype <- 'B'

iliiso <- rbind(iliiso.h1, iliiso.h3, iliiso.b)
rm(iliiso.h1, iliiso.h3, iliiso.b)

iliiso$subtype <- factor(iliiso$subtype)
iliiso$subtype <- factor(iliiso$subtype, levels = levels(iliiso$subtype)[c(2:3, 1)])

# Main plot:
p1 <- ggplot(data = iliiso, aes(x = time, y = value, col = subtype)) + geom_line(size = 0.75) + facet_wrap(~ variable, ncol = 1, scales = 'free_y') +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size = 14),
                     legend.text = element_text(size = 12), strip.text = element_text(size = 12)) +
  labs(x = '', y = 'Syndromic+', col = '(Sub)type:') + scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = c(1, 53, 105, 157, 209, 261, 314, 366), labels = c('10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18'))
# p1

# Inset:
fr <- iliiso[iliiso$variable == 'France' & iliiso$time >= 105 & iliiso$time < 209, ]
fr$time <- fr$time - 104
p2 <- ggplot(data = fr, aes(x = time, y = value, col = subtype)) + geom_line(size = 0.5) + theme_bw() +
  theme(axis.text = element_text(size = 6), legend.position = 'n', plot.margin = margin(0, 0, 0, 0), axis.title = element_blank()) +
  labs(x = '', y = '') + scale_color_brewer(palette = 'Set1') + scale_x_continuous(breaks = c(1, 53), labels = c('12-13', '13-14'))
# p2

# Combine:
g2 <- ggplotGrob(p2)

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}
# Source: https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/

p3 <- p1 + annotation_custom2(grob = g2, data = data.frame(variable = 'France', value = 1000, subtype = 'H1', time = 50), ymin = 0, ymax = 137000, xmin = 30, xmax = 95)
# p3

ggsave(filename = 'results/plots/FigS1.svg', plot = p3, width = 12.25, height = 13)

rm(list = ls())








