
library(reshape2)

### AIR ###
### Format data: 2010 forward
a2 <- read.table('formatTravelData/rawData/avia_paocc.tsv', sep = '\t', header = T)
# Air passenger transport between reporting countries (avia_paocc)

# First column: unit, train, source, destination
a2[, 1] <- as.character(a2[, 1])
a2 <- cbind(a2, do.call('rbind', strsplit(a2[, 1], ',')))
a2 <- a2[, c(18:29, 35:46, 52:63, 69:80, 86:97, 103:114, 120:131, 422:425)]

a2 <- a2[a2$`1` == 'PAS', ]; a2$`1` <- NULL
a2 <- a2[a2$`2` %in% c('PAS_CRD', 'PAS_CRD_ARR', 'PAS_CRD_DEP'), ]

names(a2)[85:87] <- c('unit', 'source', 'dest')

a2 <- a2[!(a2$source %in% c('EU27', 'EU28')), ]; a2 <- a2[!(a2$dest %in% c('EU27', 'EU28')), ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# keep only countries that are represented in source AND dest
countries.to.keep <- intersect(levels(a2$source), levels(a2$dest))
a2 <- a2[a2$source %in% countries.to.keep & a2$dest %in% countries.to.keep, ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# What about travel within a country?
a2 <- a2[a2$source != a2$dest, ]

# of course, we also only need those countries for which we did forecasting, right?
countries <- c('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU',
               'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI',
               'ES', 'SE', 'TR', 'UK', 'IS')
a2 <- a2[a2$source %in% countries & a2$dest %in% countries, ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# Average over all years for all months
for (i in 1:84) {
  a2[, i] <- as.numeric(as.character(a2[, i]))
  a2[, i][a2[, i] == 0 & !is.na(a2[, i])] <- NA
}
a2$jan <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84)], na.rm = T)
a2$feb <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 1], na.rm = T)
a2$mar <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 2], na.rm = T)
a2$apr <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 3], na.rm = T)
a2$may <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 4], na.rm = T)
a2$jun <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 5], na.rm = T)
a2$jul <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 6], na.rm = T)
a2$aug <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 7], na.rm = T)
a2$sep <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 8], na.rm = T)
a2$oct <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 9], na.rm = T)
a2$nov <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 10], na.rm = T)
a2$dec <- rowMeans(a2[, c(12, 24, 36, 48, 60, 72, 84) - 11], na.rm = T)
a2 <- a2[, 85:99] # last 15 cols

a2 <- a2[a2$unit == 'PAS_CRD_DEP', ]; a2$unit <- NULL # averages
# I think I want to keep just departing, but it's really unclear what this all means

# Melt data frames so that there is a column for 'time' and a column for 'value'
a2 <- melt(a2, id.vars = c('source', 'dest'), variable.name = 'month')

# Remove NAs
a2 <- a2[!is.na(a2$value), ]; a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# One more round of choosing only countries in source and dest:
countries.to.keep <- intersect(levels(a2$source), levels(a2$dest))
a2 <- a2[a2$source %in% countries.to.keep & a2$dest %in% countries.to.keep, ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

a2 <- a2[a2$value > 0, ]

a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)
countries <- levels(a2$source)
names(a2)[3] <- 'month'

# Get associated names for each of these countries:
country.names <- c('Austria', 'Belgium', 'Bulgaria', 'Czech Republic', 'Germany', 'Denmark',
                   'Estonia', 'Greece', 'Spain', 'Finland', 'France', 'Croatia', 'Hungary',
                   'Ireland', 'Iceland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia',
                   'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Sweden',
                   'Slovenia', 'Slovakia', 'United Kingdom')
country.names <- as.data.frame(cbind(levels(a2$source), country.names))
names(country.names) <- c('abb', 'name')

# Calculate matrix of air travel between all countries
a.by.month <- vector('list', length(unique(a2$month)))
for (k in 1:length(unique(a2$month))) {
  month <- unique(a2$month)[k]
  a.rand <- matrix(0, nrow = length(countries), ncol = length(countries))
  rownames(a.rand)= colnames(a.rand) = countries
  
  for (i in 1:length(countries)) {
    count.source <- countries[i]
    for (j in 1:length(countries)) {
      count.dest <- countries[j]
      if (length(a2$value[a2$source == count.source & a2$dest == count.dest &
                          a2$month == month]) > 0) {
        a.rand[i, j] <- a2$value[a2$source == count.source & a2$dest == count.dest &
                                   a2$month == month]
      }
    }
  }
  
  a.by.month[[k]] <- a.rand
}

# At this point, these are still counts for the whole month!

# Fill in matrices assuming adjacency:
# (also go ahead and make daily)
for (i in 1:12) {
  a.temp <- a.by.month[[i]]
  a.temp <- a.temp * 12 / 365
  
  for (ix in 1:length(colnames(a.temp))) {
    for (jx in 1:length(colnames(a.temp))) {
      if (a.temp[ix, jx] == 0 & a.temp[jx, ix] != 0) {
        # print('!')
        a.temp[ix, jx] <- a.temp[jx, ix]
      }
    }
  }
  
  a.by.month[[i]] <- a.temp
}

# Make matrices symmetric:
for (i in 1:12) {
  a.temp <- a.by.month[[i]]
  
  a.temp.sym <- a.temp
  a.temp.sym.lower <- which(lower.tri(a.temp.sym), arr.ind = TRUE)
  avg.vals <- c()
  
  for (j in 1:dim(a.temp.sym.lower)[1]) {
    index <- a.temp.sym.lower[j, ]
    avg.vals <- c(avg.vals, (a.temp.sym[index[1], index[2]] + a.temp.sym[index[2], index[1]]) / 2)
  }
  
  a.temp.sym[a.temp.sym.lower] <- avg.vals
  a.temp.sym.upper <- cbind(a.temp.sym.lower[, 2], a.temp.sym.lower[, 1])
  a.temp.sym[a.temp.sym.upper] <- avg.vals
  
  # print(isSymmetric(a.temp.sym))
  
  save(a.temp.sym, file = paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  a.by.month[[i]] <- a.temp.sym
}



