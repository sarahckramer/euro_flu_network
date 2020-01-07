
# Read in data:
iliiso.h1 <- read.csv('data/by_subtype/WHO_data_A(H1).csv')
iliiso.h3 <- read.csv('data/by_subtype/WHO_data_A(H3).csv')
iliiso.b <- read.csv('data/by_subtype/WHO_data_B.csv')

# Limit to countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

iliiso.h1 <- iliiso.h1[, c(1, count.indices + 1)]
iliiso.h3 <- iliiso.h3[, c(1, count.indices + 1)]
iliiso.b <- iliiso.b[, c(1, count.indices + 1)]

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
iliiso$subtype <- factor(iliiso$subtype)
iliiso$subtype <- factor(iliiso$subtype, levels = levels(iliiso$subtype)[c(2:3, 1)])

# Plot!:
pdf('code/checks/analyzeDataRetro/outputs/plot_syndromic+_bySubtype2.pdf', width = 15, height = 16)
p1 <- ggplot(data = iliiso, aes(x = time, y = value, col = subtype)) + geom_line(size = 0.75) + facet_wrap(~ variable, ncol = 1, scales = 'free_y') +
  theme_classic() + labs(x = '', y = 'Syn.+', col = 'Subtype') +
  scale_x_continuous(breaks = c(1, 53, 105, 157, 209, 261, 314, 366), labels = c('10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18'))
p1
dev.off()








