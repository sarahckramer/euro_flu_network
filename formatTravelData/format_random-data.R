
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

### TRAIN ###
t1 <- read.table('formatTravelData/rawData/rail_pa_intcmng.tsv', sep = '\t', header = T) # international railway travel from country of disembarkation to reporting country
t2 <- read.table('formatTravelData/rawData/rail_pa_intgong.tsv', sep = '\t', header = T) # international railway travel from reporting country to country of disembarkation

# First column: unit, train, source, destination
t1[, 1] <- as.character(t1[, 1]); t2[, 1] <- as.character(t2[, 1])
t1 <- cbind(t1, do.call('rbind', strsplit(t1[, 1], ',')))
t2 <- cbind(t2, do.call('rbind', strsplit(t2[, 1], ',')))
t1 <- t1[, c(2:16, 19:20)]; t2 <- t2[, c(2:16, 19:20)]

for (i in 1:15) {
  t1[, i][t1[, i] %in% c(': ', ': c', ': z')] <- NA
  t2[, i][t2[, i] %in% c(': ', ': c', ': z')] <- NA
  
  t1[, i] <- factor(t1[, i])
  t2[, i] <- factor(t2[, i])
  
  # print(levels(t1[, i]))
}
# 2007/2004 seem to have fewest NA values

names(t1)[16:17] <- c('source', 'dest'); names(t2)[16:17] <- c('source', 'dest')

t1 <- t1[!(t1$source %in% c('EU27', 'EU28', 'WORLD', 'UNK')), ]; t1 <- t1[t1$dest != 'EU27', ]
t1$source[t1$source == 'RS_ME'] <- 'RS'
t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)

t2 <- t2[!(t2$source %in% c('EU27', 'EU28', 'WORLD', 'UNK')), ]; t2 <- t2[!(t2$dest %in% c('EU27', 'EU28')), ]
t2$source[t2$source == 'RS_ME'] <- 'RS'
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

# might have to keep only countries that are represented in source AND dest
countries.to.keep <- intersect(levels(t1$source), levels(t1$dest))

t1 <- t1[t1$source %in% countries.to.keep & t1$dest %in% countries.to.keep, ] 
t2 <- t2[t2$source %in% countries.to.keep & t2$dest %in% countries.to.keep, ]

t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

# repeat is necessary:
countries.to.keep <- intersect(levels(t1$source), levels(t1$dest))

t1 <- t1[t1$source %in% countries.to.keep & t1$dest %in% countries.to.keep, ] 
t2 <- t2[t2$source %in% countries.to.keep & t2$dest %in% countries.to.keep, ]

t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

# What about travel within a country?
t1 <- t1[t1$source != t1$dest, ]; t2 <- t2[t2$source != t2$dest, ]

t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

# Melt data frames so that there is a column for 'time' and a column for 'value'
for (i in 1:15) {
  t1[, i] <- (as.character(t1[, i]))
  t2[, i] <- (as.character(t2[, i]))
}

t1 <- melt(t1, id.vars = c('source', 'dest'), variable.name = 'year')
t2 <- melt(t2, id.vars = c('source', 'dest'), variable.name = 'year')

# Remove NAs
t1 <- t1[!is.na(t1$value), ]; t2 <- t2[!is.na(t2$value), ]
t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

# # Remove years before 2010
# t1 <- t1[t1$year %in% c('X2016', 'X2015', 'X2014', 'X2013', 'X2012', 'X2011', 'X2010'), ]
# t2 <- t2[t2$year %in% c('X2016', 'X2015', 'X2014', 'X2013', 'X2012', 'X2011', 'X2010'), ]
# t1$year <- factor(t1$year); t2$year <- factor(t2$year)

# One more round of choosing only countries in source and dest:
countries.to.keep <- intersect(levels(t1$source), levels(t1$dest))

t1 <- t1[t1$source %in% countries.to.keep & t1$dest %in% countries.to.keep, ] 
t2 <- t2[t2$source %in% countries.to.keep & t2$dest %in% countries.to.keep, ]

t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

t1$year <- substr(t1$year, 2, 5); t1$year <- as.numeric(t1$year)
t2$year <- substr(t2$year, 2, 5); t2$year <- as.numeric(t2$year)

# what about data including "p" (provisional)?
# we could either remove the p and just use the data, or count these as NA
t1$value <- as.numeric(as.character(t1$value)) # 29 NAs
t2$value <- as.numeric(as.character(t2$value)) # 34 NAs
# since it's so few, probably okay to just remove them
t1 <- t1[!is.na(t1$value), ]; t2 <- t2[!is.na(t2$value), ]

# Some values in t1 appear inconsistent over the years:
for (year in unique(t1$year)) {
  for (s in levels(t1$source)) {
    for (d in levels(t1$source)[levels(t1$source) != s]) {
      
      temp1 <- t1[t1$source == s & t1$dest == d & t1$year == year, ]
      temp2 <- t2[t2$source == s & t2$dest == d & t2$year == year, ]
      
      if (length(temp1$source) > 0 & length(temp2$source) > 0) {
        if (temp1$value > 10 & temp2$value > 10) {
          if ((temp1$value > 4 * temp2$value) | (temp1$value < 0.25 * temp2$value)) {
            print(temp1)
            print(temp2)
            print('')
          }
        }
      }
      
    }
  }
}
rm(temp1); rm(temp2); rm(d); rm(s); rm(year)

t1 <- t1[!(t1$source == 'NL' & t1$dest == 'BE' & t1$year %in% c(2004, 2005)), ]
t1 <- t1[!(t1$source == 'LU' & t1$dest == 'BE' & t1$year %in% c(2004, 2005)), ]

# Also remove 0s:
t1 <- t1[t1$value != 0, ]; t2 <- t2[t2$value != 0, ]

# Final check of source/dest countries
t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

countries.to.keep <- intersect(levels(t1$source), levels(t1$dest))

t1 <- t1[t1$source %in% countries.to.keep & t1$dest %in% countries.to.keep, ] 
t2 <- t2[t2$source %in% countries.to.keep & t2$dest %in% countries.to.keep, ]

t1$source <- factor(t1$source); t1$dest <- factor(t1$dest)
t2$source <- factor(t2$source); t2$dest <- factor(t2$dest)

t1$report <- 'end'; t2$report <- 'start'

t <- rbind(t1, t2); t$report <- factor(t$report)

### DECISION 1 ###
# Cut off 2002 and 2003 - not much data
t <- t[!(t$year %in% c(2002, 2003)), ]

### DECISION 2 ###
# Take mean of two values when both exist, and use this
t$mean.val <- NULL
starts.or.ends <- c()
for (year in 2004:2016) {
  for (s in levels(t$source)) {
    for (d in levels(t$dest)) {
      temp <- t[t$year == year & t$source == s & t$dest == d,]
      l <- length(temp$report)
      
      if (l == 1) {
        t$mean.val[t$year == year & t$source == s & t$dest == d] <- temp$value
        starts.or.ends <- c(starts.or.ends, as.character(temp$report))
      } else if (l == 2) {
        t$mean.val[t$year == year & t$source == s & t$dest == d] <- mean(temp$value)
      } else if (l > 2) {
        print(temp)
      }
      
    }
  }
}
print(table(starts.or.ends)) # mostly ends (687:89) #(484:182)
# difference seems to mostly be the fact that now I removed 0's BEFORE averaging

t <- t[, c(1:3, 6)]
t <- unique(t)
names(t)[4] <- 'value'

# DECISION 3: Average routes over all years
# Unless there is some strange outlier
t <- t[t$value > 0, ]
t$source <- factor(t$source); t$dest <- factor(t$dest)

t$val.avg <- NA
for (i in levels(t$source)) {
  for (j in levels(t$dest)) {
    dat.temp <- t[t$source == i & t$dest == j, ]
    
    if (length(dat.temp$source) > 1) {
      p.list <- c()
      
      for (year in unique(dat.temp$year)) {
        p.list <- c(p.list, dat.temp$value[dat.temp$year == year])
      }
      
      t$val.avg[t$source == i & t$dest == j] <- mean(p.list)
      
    } else if (length(dat.temp$source == 1)) {
      t$val.avg[t$source == i & t$dest == j] <- dat.temp$value
    }
    
  }
}
# Sometimes there are noticeable increases/decreases over time, but within same order of magnitude
# Should I remove 0s? - I think I can; average when there are travelers

rm(dat.temp)
t$year <- NULL; t$value <- NULL
t <- unique(t)

# Start removing countries not in all data sources:
t <- t[!(t$source %in% c('TR', 'CH', 'MK')) & !(t$dest %in% c('TR', 'CH', 'MK')), ]
t$source <- factor(t$source); t$dest <- factor(t$dest)

# Calculate matrix of train travel between all countries:
countries <- levels(t$source)
t.rand <- matrix(0, nrow = length(countries), ncol = length(countries))
rownames(t.rand)= colnames(t.rand) = countries

for (i in 1:length(countries)) {
  count.source <- countries[i]
  for (j in 1:length(countries)) {
    count.dest <- countries[j]
    if (length(t$val.avg[t$source == count.source & t$dest == count.dest]) > 0) {
      t.rand[i, j] <- t$val.avg[t$source == count.source & t$dest == count.dest]
    }
  }
}

# Fill in missing values, assuming symmetry:
for (i in 1:length(colnames(t.rand))) {
  for (j in 1:length(colnames(t.rand))) {
    if (t.rand[i, j] == 0 & t.rand[j, i] != 0) {
      t.rand[i, j] <- t.rand[j, i]
    }
  }
}

# Make matrices symmetric:
t.rand.sym <- t.rand
t.rand.sym.lower <- which(lower.tri(t.rand.sym), arr.ind = TRUE)
avg.vals <- c()
for (j in 1:dim(t.rand.sym.lower)[1]) {
  index <- t.rand.sym.lower[j, ]
  avg.vals <- c(avg.vals, (t.rand.sym[index[1], index[2]] + t.rand.sym[index[2], index[1]]) / 2)
}

t.rand.sym[t.rand.sym.lower] <- avg.vals
t.rand.sym.upper <- cbind(t.rand.sym.lower[, 2], t.rand.sym.lower[, 1])
t.rand.sym[t.rand.sym.upper] <- avg.vals
print(isSymmetric(t.rand.sym))

# Convert to daily:
t.rand.sym <- t.rand.sym * 1000 / 365

# Save formatted train data:
t.rand <- t.rand.sym
save(t.rand, file = 'formatTravelData/formattedData/train_05-07.RData')






















