### Calculate the % of each country's population commuting to another country ###

# based on additional data source from Eurostat
c <- read.csv('../rawData/lfst_r_lfe2ecomm.tsv', sep = '\t', header = T)
# Employment and commuting by NUTS 2 regions (1 000) [lfst_r_lfe2ecomm]

c[, 1] <- as.character(c[, 1])
c <- cbind(c, do.call('rbind', strsplit(c[, 1], ',')))

c <- c[, c(2:15, 22:23)]
names(c)[15:16] <- c('to', 'country')

c <- c[c$to != 'NRP', ]; c$to <- factor(c$to)

countries <- c('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU',
               'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI',
               'ES', 'SE', 'CH', 'TR', 'UK', 'MK')
c <- c[c$country %in% countries, ]; c$country <- factor(c$country)

c <- c[order(c$country), ]
c <- c[c$country != 'TR', ] # no data on foreign commuters
c$country <- factor(c$country)

for (i in 1:14) {
  c[, i] <- as.numeric(as.character(c[, i]))
}
c <- c[, c(1:12, 15:16)] # no data for 2005; remove 2004 as well and start w/ 2006

# Create new data frame w/ yearly percentages of foreign commuters:
a <- NULL
a$country <- levels(c$country)
a <- as.data.frame(a)

names(c)[1:12] <- substr(names(c)[1:12], 2, 5)

res.mat <- matrix(nrow = length(levels(c$country)), ncol = 12) # percent of all commuters leaving country
for (j in 1:length(levels(c$country))) {
  country <- levels(c$country)[j]
  
  temp.for <- c[c$country == country & c$to == 'FOR', ]
  temp.in <- c[c$country == country & c$to == 'INR', ]
  temp.out <- c[c$country == country & c$to == 'OUTR', ]
  
  len.out <- length(temp.out$country)
  
  for (i in 1:12) {
    foreign <- temp.for[, i]; inner <- temp.in[, i]
    
    if (len.out == 1) {
      outer <- temp.out[, i]
    } else {
      outer <- 0
    }
    
    inner <- ifelse(is.na(inner), 0, inner)
    outer <- ifelse(is.na(outer), 0, outer)
    
    res.mat[j, i] <- foreign / (foreign + outer + inner) * 100
  }
}
# no more than 8% of any country's commuting is international

# Now average across all years?
c$Xmean <- sapply(1:length(c$country), function (ix) {
  mean(unlist(c[ix, 1:12]), na.rm = TRUE)
})

c <- c[, 13:15]
write.csv(c, file = '../formattedData/commuting_number-out_05-07.csv', row.names = FALSE)

res.mat <- as.data.frame(cbind(levels(c$country), res.mat))
write.csv(res.mat, file = '../formattedData/commuting_prop-foreign_05-07.csv', row.names = FALSE)
