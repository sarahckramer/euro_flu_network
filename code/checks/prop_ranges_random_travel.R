
# Look at what might be appropriate proportions of commuting volume to use in proportional random travel by comparing commuters to daily train traffic

# In Sen's work, the range was 0 - 0.05 (0-5%)

# List countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')

# Read in commuting data and limit to relevant countries:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
for (i in 1:length(comm.by.year)) {
  comm.by.year[[i]] <- comm.by.year[[i]][countries, countries]
}

# Read in formatted train data:
load('../forecastsE/train_04-30.RData')
t.rand1 <- t.rand
load('../forecastsE/train_04-16.RData')
t.rand2 <- t.rand.sym
load('../forecastsE/train_02-07.RData')
t.rand3 <- t.rand.sym
rm(t.rand, t.rand.sym, i)

# The one from 4-16 clearly doesn't contain enough countries? - AT is missing
# Whereas there are too many countries in 2-07 version - includes EL, LT, LV, and NO
# Most recent (4-30) is missing RO

# How do the three compare when they overlap?
rownames(t.rand2)
t.rand1.red <- t.rand1[rownames(t.rand2), rownames(t.rand2)]
t.rand3.red <- t.rand3[rownames(t.rand2), rownames(t.rand2)]

all.equal(t.rand2, t.rand1.red)
all.equal(t.rand2, t.rand3.red)
all.equal(t.rand1.red, t.rand3.red)
# Great, so all the same! Compile the ones we need:

t.rand <- t.rand3[countries[2:20], countries[2:20]] # AT has no train data
rm(t.rand1, t.rand1.red, t.rand2, t.rand3, t.rand3.red)

# Explore a bit...
load('formatTravelData/formattedData/air_1_05-07.RData')
a.temp.sym <- a.temp.sym[countries, countries]
plot(a.temp.sym[2:20, 2], t.rand[, 1], pch = 20, cex = 1.5)
plot(a.temp.sym[2:20, 18], t.rand[, 17], pch = 20, cex = 1.5)
# as far as I can tell by past notes, these are daily values

# Since commuting overall tends to be an order of magnitude higher, let's look at commuting / train, and see the range

# First, remove AT from commuting for now:
countries <- c('BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
for (i in 1:length(comm.by.year)) {
  comm.by.year[[i]] <- comm.by.year[[i]][countries, countries]
}

# Also need to remove 0 from t.rand, so that no Inf occurs:
# t.rand[t.rand == 0] <- NA
# Actually though, if we leave them as 0s, we can tell the difference between NaN (0/0) and Inf (+/0)

# Now loop through all countries (outgoing) and seasons to see range of values:
hist.all <- c()
ratio.by.count <- vector('list', length(countries))
for (i in 1:length(countries)) {
  ratio.mat <- matrix(NA, nrow = length(comm.by.year), ncol = length(countries))
  
  for (j in 1:length(comm.by.year)) {
    comm.temp <- comm.by.year[[j]]
    
    hist.all <- c(hist.all, comm.temp[i, ] / t.rand[i, ])
    ratio.mat[j, ] <- comm.temp[i, ] / t.rand[i, ]
    
  }
  
  colnames(ratio.mat) <- countries
  ratio.by.count[[i]] <- ratio.mat
}
# note that an NA means there is no train travel or commuting on this route; 0 means there is train travel but no commuting; Inf means commuting but no train travel

# Look at histogram of values overall:
hist(hist.all[hist.all > 0 & !is.na(hist.all)], breaks = 100)
hist(1 / hist.all[hist.all > 0 & !is.na(hist.all)], breaks = 100)
# So while most values are low, there are actually routes where there is more train travel than commuting
    # Also note that we have no info on car and other travel
# Sen used a very low value, maybe assuming that most inter-state travel was commuting; but can the same be said for international travel in Europe?
    # In which case, should higher values be allowed?
    # Or do we stick with the values Sen used, to ensure we stick to the same model for now?
    # Alternatively, we can just use the train data

# Look at quantiles in hist.all:
quantile(hist.all[hist.all > 0 & !is.na(hist.all) & hist.all != Inf], probs = seq(0, 1, by = 0.1))
quantile(1 / hist.all[hist.all > 0 & !is.na(hist.all) & hist.all != Inf], probs = seq(0, 1, by = 0.1)) # above 1 occurs somewhere above 90th; median is 0.0297; 70th 0.10, 80th ~ 0.20
quantile(1 / hist.all[hist.all > 0 & !is.na(hist.all) & hist.all != Inf], probs = c(0.75, 0.925)) # 75th is 0.146, 92.5 is about 1.0
# so like 0 - 0.15 ish?

# Explore a bit by country:
# DK to SE, LU to BE, LU to FR, UK to FR: more train than commuting; almost all Infs are to UK

# and what about when there is train but no commuting? are these train flows low?
mat.tnoc <- NULL
for (i in 1:length(countries)) {
  mat.tnoc <- rbind(mat.tnoc, colMeans(ratio.by.count[[i]]))
}
rownames(mat.tnoc) = colnames(mat.tnoc) = countries

b <- t.rand[which(mat.tnoc == 0, arr.ind = T)] # mostly quite low, although there's a 436.5 (DE-BE), a 868.4 (DE-CZ), a 977.9 (DE-IT), and a 2776.6 (BE-UK and UK-BE) here
b[b > 400]
# the one way ones probably aren't a problem - can use the larger of the two commuting flows (or the average? or average OR non-zero) to estimate random travel

# So can try Sen's 0-0.05, or do 0-0.15 to account for perhaps higher rates of random travel (relative to commuting) between European countries
# or just use symmetric train data - but missing AT!
















