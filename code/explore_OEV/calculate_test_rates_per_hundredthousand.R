
# Read in test numbers:
test.dat <- read.csv('data/testCounts_052719.csv')

# Reduce to countries of interest:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
test.dat <- test.dat[, c(1, count.indices + 1)]

# Read in population sizes:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Scale to tests per 100,000 population:
for (i in 2:13) {
  test.dat[, i] <- (test.dat[, i] / pop.size$pop[i - 1]) * 100000
}

# Save:
write.csv(test.dat, 'data/testRates_010820.csv', row.names = FALSE)

# Plot out and look at ranges by country:
for (i in 2:13) {
  plot(test.dat[, i], type = 'l', main = names(test.dat)[i])
}
# AT, FR: ~10-15
# ES: 4-8
# 1-5: BE, CZ, HU, IT, NL, PL, SK
# DE: 0.5
# LU: ~40