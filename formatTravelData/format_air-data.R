
library(reshape2)

### AIR ###
### Format data: 2010 forward
a2 <- read.table('formatTravelData/rawData/avia_paocc.tsv', sep = '\t', header = T)
a3 <- read.table('formatTravelData/rawData/avia_paocc_1.tsv', sep = '\t', header = T)
# Air passenger transport between reporting countries (avia_paocc)
# Second (a3) downloaded on Jan 30, 2020

# First column: unit, train, source, destination
a2[, 1] <- as.character(a2[, 1])
a2 <- cbind(a2, do.call('rbind', strsplit(a2[, 1], ',')))
a2 <- a2[, c(18:29, 35:46, 52:63, 69:80, 86:97, 103:114, 120:131, 422:425)]

a3[, 1] <- as.character(a3[, 1])
a3 <- cbind(a3, do.call('rbind', strsplit(a3[, 1], ',')))
a3 <- a3[, c(292:303, 309:320, 326:337, 343:354, 360:371, 377:388, 394:405, 411:422, 456:459)] # only want monthly values (not quarterly; 2010-2017, to match commuting)

a2 <- a2[a2$`1` == 'PAS', ]; a2$`1` <- NULL
a2 <- a2[a2$`2` %in% c('PAS_CRD', 'PAS_CRD_ARR', 'PAS_CRD_DEP'), ]

a3$`2` <- NULL
unique(a3$`3`) # same as what a2 limited to above

names(a2)[85:87] <- c('unit', 'source', 'dest')
names(a3)[97:99] <- c('source', 'unit', 'dest')

a2 <- a2[!(a2$source %in% c('EU27', 'EU28')), ]; a2 <- a2[!(a2$dest %in% c('EU27', 'EU28')), ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# in a3, countries are written as names
levels(a3$source) <- c('AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU',
                       'IS', 'IE', 'IT', 'LV', 'LT', 'LU', 'ML', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI',
                       'ES', 'SE', 'CH', 'UK')
levels(a3$dest) <- c('AT', 'BE', 'HR', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# # keep only countries that are represented in source AND dest
# countries.to.keep <- intersect(levels(a2$source), levels(a2$dest))
# a2 <- a2[a2$source %in% countries.to.keep & a2$dest %in% countries.to.keep, ]
# a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

# of course, we also only need those countries for which we did forecasting, right?
# countries <- c('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU',
#                'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI',
#                'ES', 'SE', 'TR', 'UK', 'IS')
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
a2 <- a2[a2$source %in% countries & a2$dest %in% countries, ]
a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)

a3 <- a3[a3$source %in% countries & a3$dest %in% countries, ]
a3$source <- factor(a3$source); a3$dest <- factor(a3$dest)

# What about travel within a country?
a2 <- a2[a2$source != a2$dest, ]
a3 <- a3[a3$source != a3$dest, ]

# a3 values don't seem to be numeric? fix this:
for (i in 1:96) {
  a3[, i] <- as.numeric(as.character(gsub(' ', '', a3[, i])))
}

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

for (i in 1:96) {
  # a3[, i] <- as.numeric(as.character(a3[, i]))
  a3[, i][a3[, i] == 0 & !is.na(a3[, i])] <- NA
}
a3$jan <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85)], na.rm = TRUE)
a3$feb <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 1], na.rm = TRUE)
a3$mar <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 2], na.rm = TRUE)
a3$apr <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 3], na.rm = TRUE)
a3$may <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 4], na.rm = TRUE)
a3$jun <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 5], na.rm = TRUE)
a3$jul <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 6], na.rm = TRUE)
a3$aug <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 7], na.rm = TRUE)
a3$sep <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 8], na.rm = TRUE)
a3$oct <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 9], na.rm = TRUE)
a3$nov <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 10], na.rm = TRUE)
a3$dec <- rowMeans(a3[, c(1, 13, 25, 27, 49, 61, 73, 85) + 11], na.rm = TRUE)

a2 <- a2[a2$unit == 'PAS_CRD_DEP', ]; a2$unit <- NULL # averages
# I think I want to keep just departing, but it's really unclear what this all means
a3 <- a3[a3$unit == 'Passengers carried (departures)', ]; a3$unit <- NULL

# are a2 and a3 the same except the added year?
a1 <- merge(a2, a3, by = c('source', 'dest'))
# generally yes
rm(a1)

# Keep only monthly averages:
a2 <- a2[, 85:98] # last 15 cols
a3 <- a3[, 97:110]
# in general, values tend to be a little higher than before 2017 added, but that seems realistic

# Melt data frames so that there is a column for 'time' and a column for 'value'
a2 <- melt(a2, id.vars = c('source', 'dest'), variable.name = 'month')
a3 <- melt(a3, id.vars = c('source', 'dest'), variable.name = 'month')

# Remove NAs
a2 <- a2[!is.na(a2$value), ]; a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)
a3 <- a3[!is.na(a3$value), ]; a3$source <- factor(a3$source); a3$dest <- factor(a3$dest)

# # One more round of choosing only countries in source and dest:
# countries.to.keep <- intersect(levels(a2$source), levels(a2$dest))
# a2 <- a2[a2$source %in% countries.to.keep & a2$dest %in% countries.to.keep, ]
# a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)
# We have these down by now

a2 <- a2[a2$value > 0, ]
a3 <- a3[a3$value > 0, ]

a2$source <- factor(a2$source); a2$dest <- factor(a2$dest)
countries <- levels(a2$source)
# names(a2)[3] <- 'month'

a3$source <- factor(a3$source); a3$dest <- factor(a3$dest)

# # Get associated names for each of these countries:
# country.names <- c('Austria', 'Belgium', 'Bulgaria', 'Czech Republic', 'Germany', 'Denmark',
#                    'Estonia', 'Greece', 'Spain', 'Finland', 'France', 'Croatia', 'Hungary',
#                    'Ireland', 'Iceland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia',
#                    'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Sweden',
#                    'Slovenia', 'Slovakia', 'United Kingdom')
# country.names <- as.data.frame(cbind(levels(a2$source), country.names))
# names(country.names) <- c('abb', 'name')

# Calculate matrix of air travel between all countries
a.by.month <- vector('list', length(unique(a3$month)))
for (k in 1:length(unique(a3$month))) {
  month <- unique(a3$month)[k]
  a.rand <- matrix(0, nrow = length(countries), ncol = length(countries))
  rownames(a.rand)= colnames(a.rand) = countries
  
  for (i in 1:length(countries)) {
    count.source <- countries[i]
    for (j in 1:length(countries)) {
      count.dest <- countries[j]
      if (length(a3$value[a3$source == count.source & a3$dest == count.dest &
                          a3$month == month]) > 0) {
        a.rand[i, j] <- a3$value[a3$source == count.source & a3$dest == count.dest &
                                   a3$month == month]
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
  
  save(a.temp.sym, file = paste0('formatTravelData/formattedData/air_', i, '_01-31.RData'))
  a.by.month[[i]] <- a.temp.sym
}



