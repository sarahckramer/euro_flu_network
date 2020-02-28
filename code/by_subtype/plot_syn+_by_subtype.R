
# Read in data:
iliiso.a <- read.csv('data/by_subtype/WHO_data_A(all).csv')
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B.csv')

iliiso.a <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED.csv')
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED.csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B_SCALED.csv')

iliiso.a <- read.csv('data/by_subtype/WHO_data_A(all)_SCALED_full.csv')
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED_full.csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3)_SCALED_full.csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B_SCALED_full.csv')

# Limit to countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

iliiso.a <- iliiso.a[, c(1, count.indices + 1)]
iliiso.h1 <- iliiso.h1[, c(1, count.indices + 1)]
iliiso.h3 <- iliiso.h3[, c(1, count.indices + 1)]
iliiso.b <- iliiso.b[, c(1, count.indices + 1)]

# Also limit to 2010-11 season and forward:
iliiso.a <- iliiso.a[79:495, ]
iliiso.h1 <- iliiso.h1[79:495, ]
iliiso.h3 <- iliiso.h3[79:495, ]
iliiso.b <- iliiso.b[79:495, ]

# Replace "time" with integer marker:
iliiso.a$time <- 1:417
iliiso.h1$time <- 1:417
iliiso.h3$time <- 1:417
iliiso.b$time <- 1:417

# Melt and combine:
iliiso.a <- melt(iliiso.a, id.vars = 'time')
iliiso.h1 <- melt(iliiso.h1, id.vars = 'time')
iliiso.h3 <- melt(iliiso.h3, id.vars = 'time')
iliiso.b <- melt(iliiso.b, id.vars = 'time')

iliiso.a$subtype <- 'A(all)'
iliiso.h1$subtype <- 'H1'
iliiso.h3$subtype <- 'H3'
iliiso.b$subtype <- 'B'

iliiso <- rbind(iliiso.a, iliiso.h1, iliiso.h3, iliiso.b)
iliiso$subtype <- factor(iliiso$subtype)
iliiso$subtype <- factor(iliiso$subtype, levels = levels(iliiso$subtype)[c(1, 3:4, 2)])

# Plot!:
# pdf('code/checks/analyzeDataRetro/outputs/plot_syndromic+_bySubtype2_wA_fullScalings.pdf', width = 15, height = 16)
pdf('../drafts/NetworkModel/supplemental/FigS1.pdf', width = 12, height = 13)
p1 <- ggplot(data = iliiso, aes(x = time, y = value, col = subtype)) + geom_line(size = 0.75) + facet_wrap(~ variable, ncol = 1, scales = 'free_y') +
  theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.title = element_text(size = 14),
                     legend.text = element_text(size = 12), strip.text = element_text(size = 12)) +
  labs(x = '', y = 'Syndromic+', col = 'Subtype') + scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = c(1, 53, 105, 157, 209, 261, 314, 366), labels = c('10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18'))
p1
dev.off()








