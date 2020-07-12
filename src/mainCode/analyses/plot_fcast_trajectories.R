
### Produce plots to show fit/forecasts over time ###

library(reshape2); library(stringr); library(ggplot2); library(gridExtra)

# Specify (sub)type:
strain <- 'A(H1)'

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

if (model.type == 'network') {
  output.fits <- read.csv(paste0('results/fits/outputOP_', strain, '_fitsOnly.csv'))
} else if (model.type == 'isolated') {
  output.fits <- read.csv(paste0('results/fits/outputOP_', strain, '_fitsOnly_ISOLATED.csv'))
}

if (model.type == 'network') {
  output <- output[, c(1:10, 13, 17)]
  output.fits <- output.fits[, c(12:16, 1:5, 8)]
} else if (model.type == 'isolated') {
  output <- output[, c(1:10, 17, 25)]
  output.fits <- output.fits[, c(20:24, 1:4, 19, 11)]
}

# Format output.fits:
if (model.type == 'network') {
  countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
  output.fits$country <- countries[output.fits$country + 1]
  output.fits$country <- factor(output.fits$country)
  rm(countries)
}
output.fits$subtype <- strain
output.fits$fc_start <- 70

# check this:
summary(output$fc_start[output$result == 'train'])
output <- output[output$result == 'fcast', ]

# Combine:
output <- rbind(output, output.fits)
rm(output.fits)

# Add observations:
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
output$group <- paste(output$run, output$oev_base, output$oev_denom, output$result, sep='_')
output$oev_base <- factor(output$oev_base)
output$oev_denom <- factor(output$oev_denom)
output$result <- factor(output$result, levels = levels(output$result)[2:1])

for (season in seasons) {
  print(season)
  graphs <- list()
  temp <- output[output$season == season, ]
  
  # remove countries with no data at all for that season:
  to.remove <- c()
  for (country in countries) {
    if(!any(temp$result[temp$country == country] == 'fcast')) {
      to.remove <- c(to.remove, country)
    }
  }
  temp <- temp[!(temp$country %in% to.remove), ]
  rm(to.remove)
  
  num.graphs <- 1
  for (wk in fc_starts[fc_starts < 70]) {
    # sub <- temp[temp$fc_start == wk, ]
    # sub <- temp[temp$fc_start == wk | (temp$result == 'train' & temp$week <= wk), ]
    
    sub <- temp[temp$fc_start == wk | temp$result == 'train', ]
    sub$Est[sub$result == 'train' & sub$week > wk] <- NA
    sub$Est[is.na(sub$observed) & sub$Est == 0 & !is.na(sub$Est)] <- NA
    
    if (length(sub$country) > 0) {
      
      g <- ggplot(sub, aes(x = time, y = Est, group = factor(group), colour = result)) + geom_line() +
        geom_point(data = sub, aes(x = time, y = observed), colour = 'black', cex = 0.9) + theme_bw() +
        facet_wrap(~ country, ncol = 3, scale = 'free_y') + scale_color_brewer(palette = 'Set1', direction = -1) +
        labs(title = wk, x = 'Time', y = 'Syn.+', colour = '')
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
  ggsave(paste0(dir.save, 'output_', season, '_', strain, '_', model.type, '.pdf'), glist, width = 25, height = 9, dpi = 600)
  print('Done.')
}

rm(list = ls())
