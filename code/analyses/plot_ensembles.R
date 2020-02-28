
library(ggplot2); library(data.table); library(magrittr); library(plyr); library(plot3D); library(gmodels);
library(gtable); library(gridExtra); library(reshape2); library(stringr)

# Specify output folder:
dir.save <- 'results/plots/outputs/'

# Specify subtype:
strain <- 'A(H1)'

# Read in results:
output <- read.csv('results/isolated/outputOP_PROC.csv')
output <- output[output$subtype == strain, ]

output$year <- as.numeric(as.character(substr(output$season, start = 1, stop = 4)))
output$year[output$week > 53] <- output$year[output$week > 53] + 1
output$year[output$week == 53 & output$year != 2015] <- output$year[output$week == 53 & output$year != 2015] + 1
output$week2 <- output$week
output$week2[!(output$year %in% c(2015, 2016)) & output$week2 > 52] <- output$week2[!(output$year %in% c(2015, 2016)) & output$week2 > 52] - 52
output$week2[output$year == 2016 & output$week2 > 53] <- output$week2[output$year == 2016 & output$week2 > 53] - 53
output$week2[output$year == 2015 & output$week2 > 52] <- output$week2[output$year == 2015 & output$week2 > 52] - 52
output$week2[output$year == 2015 & output$week2 == 1 & output$season == '2015-16'] <- 53

output$week2 <- str_pad(output$week2, width = 2, pad = '0')
output$time2 <- paste(output$year, output$week2, sep = '_')

# Reduce to seasons of interest:
output <- output[output$season %in% c('2012-13', '2014-15'), ]

# Reduce data frame:
# output <- output[, c(1:2, 6:10, 13, 16:20)]
output <- output[, c(1:2, 6:10, 17, 24:28)]

# First maybe just try to plot these with sd incorporated?

# Read in data:
iliiso <- read.csv(paste0('data/by_subtype/WHO_data_', strain, '_SCALED.csv'))
names(iliiso) <- c('time', 'AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
iliiso <- melt(iliiso)
names(iliiso)[3] <- 'observed'

output <- merge(output, iliiso, by.x = c('country', 'time2'), by.y = c('variable', 'time'))

# # Calculate 95% CIs:
# output$lower.val <- output$Est - 1.96 * (output$Est_sd / sqrt(300))
# output$upper.val <- output$Est + 1.96 * (output$Est_sd / sqrt(300))
# output$lower.val[output$lower.val < 0] <- 0

# Calculate lower/upper bounds (just 1 sd):
output$lower.val <- output$Est - output$Est_sd
output$upper.val <- output$Est + output$Est_sd
output$lower.val[output$lower.val < 0] <- 0

# Plot!
countries <- unique(output$country)
seasons <- unique(output$season)
fc_starts <- sort(unique(output$fc_start))

for (season in seasons) {
  print(season)
  graphs <- list()
  
  num.graphs <- 1
  for (wk in fc_starts) {
    sub <- output[output$season == season & output$fc_start == wk, ]
    
    if (length(sub$country) > 0) {
      g <- ggplot(sub, aes(x = time, y = Est, group = factor(run), colour = result)) + geom_line() +
        geom_point(data = sub, aes(x = time, y = observed), colour = 'black', cex = 0.9) +
        geom_pointrange(data = sub, aes(x = time, y = Est, ymin = lower.val, ymax = upper.val), colour = '#e41a1c', cex = 0.2) +
        facet_wrap(~ country, ncol = 3, scale = 'free_y') + scale_color_brewer(palette = 'Set1') +
        labs(title = wk, x = 'Time', y = 'Syn.+', colour = '') + theme_bw()
      # print(g)
      
      graphs[[num.graphs]] <- g
      num.graphs <- num.graphs + 1
    }
  }
  
  print('Graph list completed.')
  glist <- marrangeGrob(grobs = graphs, nrow = 1, ncol = 1)
  ggsave(paste(dir.save, 'sd_output_', season, '_', strain, '_isolated.pdf', sep = ''), glist, width = 25, height = 9, dpi = 600)
  print('Done.')
}

# # Read in and format ens file:
# e <- read.csv(paste0('results/by_subtype/network_', strain, '/original/outputEns_', strain, '_testSet1.csv'))
# e <- e[, 1:305]
# e <- e[e$metric == 'pi', ]








