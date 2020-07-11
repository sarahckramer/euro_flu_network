
### Produce plots to show fit/forecasts over time ###

library(reshape2); library(stringr); library(ggplot2); library(gridExtra)

# Specify (sub)type:
strain <- 'A(H3)'

# Specify model:
model.type <- 'network'

# Specify output folder:
if (!dir.exists('results/plots/outputs/')) {
  dir.create('results/plots/outputs/')
}
dir.save <- 'results/plots/outputs/'

# Read in results:
output <- read.csv(paste0('results/', model.type, '/outputOP_PROC.csv'))
output <- output[output$subtype == strain, ]

output.fits <- read.csv(paste0('results/fits/outputOP_', strain, '_fitsOnly.csv'))

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

# First need to add observed to output data frame:
iliiso <- read.csv(paste0('data/WHO_data_', strain, '_SCALED.csv'))
names(iliiso) <- c('time', 'AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
iliiso <- melt(iliiso)
names(iliiso)[3] <- 'observed'

output <- merge(output, iliiso, by.x = c('country', 'time2'), by.y = c('variable', 'time'))

#### OUTPUT ####
# Create output graphs, saved as PDF file:
countries <- unique(output$country)
seasons <- unique(output$season)
fc_starts <- sort(unique(output$fc_start))
output$group <- paste(output$run, output$oev_base, output$oev_denom, sep='_')
output$oev_base <- factor(output$oev_base)
output$oev_denom <- factor(output$oev_denom)

for (season in seasons) {
  print(season)
  graphs <- list()
  temp <- output[output$season == season, ]
  
  num.graphs <- 1
  for (wk in fc_starts) {
    # sub <- temp[temp$fc_start == wk, ]
    sub <- temp[temp$fc_start == wk | (temp$result == 'train' & temp$week <= wk), ]
    
    if (length(sub$country) > 0) {
      
      g <- ggplot(sub, aes(x = time, y = Est, group = factor(group), colour = result)) + geom_line() +
        geom_point(data = sub, aes(x = time, y = observed), colour = 'black', cex = 0.9) +
        facet_wrap(~ country, ncol = 3, scale = 'free_y') + scale_color_brewer(palette = 'Set1') +
        labs(title = wk, x = 'Time', y = 'Syn.+', colour = '') + theme_bw()
      # g <- ggplot(sub, aes(x = time, y = Est, group = factor(group), col = oev_base)) + geom_line() +
      #   geom_point(data = sub, aes(x = time, y = observed), colour = 'black', cex = 0.9) +
      #   facet_grid(oev_denom ~ country, scale = 'free_y') + scale_color_brewer(palette = 'Set1') +
      #   labs(title = wk, x = 'Time', y = 'Syn.+', colour = '') + theme_bw() +
      #   geom_vline(xintercept = 276 + 7 * (wk - 40), lwd = 1.0)
      graphs[[num.graphs]] <- g
      num.graphs <- num.graphs + 1
      
    }
  }
  
  print('Graph list completed.')
  glist <- marrangeGrob(grobs = graphs, nrow = 1, ncol = 1)
  ggsave(paste(dir.save, 'output_', season, '_H3_network.pdf', sep = ''), glist, width = 25, height = 9, dpi = 600)
  print('Done.')
}


