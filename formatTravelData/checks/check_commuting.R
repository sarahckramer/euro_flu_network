
# If these are just a sample of the population, do they need to be scaled up?
# Check that commuters into LU matches the expected amount (~120,000 - 180,000) -
    # here 128,000-189,000, so good! (implying we don't need to scale at all?)
# Also check that number commuting matches % of population commuting
# Change over time simply due to changing sample sizes/populations?

# load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
# countries <- colnames(comm.by.year[[1]])
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

c1 <- read.csv('formatTravelData/rawData/ESTA45672_190215.csv')
c1 <- c1[, c(1:2, 4, 6)] # remove unnecessary columns
c1 <- c1[!is.na(c1$VALUE), ] # remove combinations where no commuting occurs
c1 <- c1[c1$COUNTRYW != 'Reporting country' & c1$COUNTRYW != 'No answer', ]
c1 <- c1[c1$COUNTRY %in% countries, ]; c1$COUNTRY <- factor(c1$COUNTRY)

c.out <- read.csv('formatTravelData/formattedData/commuting_number-out_byYear_012420.csv')
c.out <- c.out[c.out$to == 'FOR', ]; #c.out <- c.out[, 2:3]
c.out <- c.out[c.out$country %in% countries, ]; c.out$country <- factor(c.out$country)

# Now for each country and year, we want to see if the total number (in thousands) out matches the total number in the set I got
# If not, any "missing"-ness might be the number of people we need to distribute along other routes
# But if total is ever less than what we have, there's a problem
# I believe these come from the same data set, but I'm not sure exactly how these are processed or if "unreliable" data is kept
# Also there are years/countries that are missing, so that doesn't help

years <- 2017:2010
factors <- c()
for (ix in 1:length(years)) {
  dat.temp <- c1[c1$YEAR == years[ix], ]
  print(years[ix])
  
  for (jx in countries) {
    print(jx)
    # print(sum(dat.temp$VALUE[dat.temp$COUNTRY == jx]))
    # print(c.out[c.out$country == jx, ix])
    print(c.out[c.out$country == jx, ix] / sum(dat.temp$VALUE[dat.temp$COUNTRY == jx]))
    print(c.out[c.out$country == jx, ix] - sum(dat.temp$VALUE[dat.temp$COUNTRY == jx]))
    factors <- c(factors, c.out[c.out$country == jx, ix] / sum(dat.temp$VALUE[dat.temp$COUNTRY == jx]))
  }
  
  print('')
}









res.all <- NULL
for (i in 1:length(comm.by.year)) {
  # print(cbind(c.out, rowSums(comm.by.year[[i]])[-14] / 1000))
  res.temp <- NULL
  for (country in countries) {
    res.temp <- rbind(res.temp, c(c.out$Xmean[c.out$country == country], sum(c1$VALUE[c1$YEAR == unique(c1$YEAR)[i] & c1$COUNTRY == country])))
  }
  rats <- res.temp[, 1] / res.temp[, 2]
  print(summary(rats[rats != Inf]))
  print(cbind(countries, rats))
  print('')
  
  res.temp <- cbind(countries, res.temp, unique(c1$YEAR)[i])
  res.all <- rbind(res.all, res.temp)
}

c.out <- cbind(c.out, rowSums(comm.by.year[[6]])[-14] / 1000)
names(c.out) <- c('country', 'Xmean', 'rowSum')
c.out$factor <- c.out$Xmean / c.out$rowSum
c.out$factor[c.out$factor == Inf & !is.na(c.out$factor)] <- NA
summary(c.out$factor) # ranges from 0.8 (so slightly smaller) to almost 5 times higher in outbound commuter data
# but of course this will be different - doesn't include "other"
