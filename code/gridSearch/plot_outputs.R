
# Produce "output plots" similar to in Aim1 to see what is happening at each stage of the forecast

library(ggplot2); library(data.table); library(magrittr); library(plyr); library(plot3D); library(gmodels);
library(gtable); library(gridExtra); library(reshape2); library(stringr)

#ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
#need the edited ggsave function to generate PDF files properly

# Specify output folder:
dir.save <- 'code/gridSearch/plots/'

# Read in results:
output <- read.csv('results/A(H1)/indiv_mid/outputOP_PROC.csv')

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
# iliiso <- read.csv('data/WHO_data_05-09-19.csv')
# iliiso <- iliiso[, c(1:3, 5, 7:9, 12:15, 18, 20)]
# 
# scalings <- read.csv('data/scalings_frame_05-09-19.csv') # 1.3 for France in early seasons
# scalings <- scalings[c(1:2, 4, 6:8, 11:14, 17, 19), ]
# for (i in 2:13) {
#   if (names(iliiso)[i] == 'France') {
#     iliiso[1:286, i] <- iliiso[1:286, i] * 1.3
#     iliiso[287:495, i] <- iliiso[287:495, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   } else {
#     iliiso[, i] <- iliiso[, i] * scalings$gamma[scalings$country == names(iliiso)[i]]
#   }
#   
#   iliiso[, i][iliiso[, i] < 0] <- NA # replace negatives with NAs
# }

iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
names(iliiso) <- c('time', 'AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
iliiso <- melt(iliiso)
names(iliiso)[3] <- 'observed'

output <- merge(output, iliiso, by.x = c('country', 'time2'), by.y = c('variable', 'time'))

# # Choose oev_base:
# output.full <- output
# output <- output[output$oev_base == 1e4, ]

#### OUTPUT ####
# Create output graphs, saved as PDF file:
countries <- unique(output$country)
seasons <- unique(output$season)
fc_starts <- sort(unique(output$fc_start))
output$group <- paste(output$run, output$oev_base, sep='_')

for (season in seasons) {
  print(season)
  graphs <- list()
  temp <- output[output$season == season, ]
  
  num.graphs <- 1
  for (wk in fc_starts) {
    sub <- temp[temp$fc_start == wk, ]
    
    if (length(sub$country) > 0) {
      
      g <- ggplot(sub, aes(x = time, y = Est, group = factor(group), colour = result)) + geom_line() +
        geom_point(data = sub, aes(x = time, y = observed), colour = 'black', cex = 0.9) +
        facet_wrap(~ country, ncol = 3, scale = 'free_y') + scale_color_brewer(palette = 'Set1') +
        labs(title = wk, x = 'Time', y = 'Syn.+', colour = '') + theme_bw()
      graphs[[num.graphs]] <- g
      num.graphs <- num.graphs + 1
      
    }
  }
  
  print('Graph list completed.')
  glist <- marrangeGrob(grobs = graphs, nrow = 1, ncol = 1)
  ggsave(paste(dir.save, 'output_', season, '_H1_INDIV.pdf', sep = ''), glist, width = 18, height = 9, dpi = 600)
  print('Done.')
}


