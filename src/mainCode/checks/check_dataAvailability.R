
# Sen only used states w/ at least 30,000 syndromic cases over the study period
    # Can also look at number of tests and set a threshold there?
    # Was this 30,000 cases, or 30,000 total visits (denominator)?

### List countries
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

### Read in influenza data
iliiso <- read.csv('data/WHO_data_05-09-19.csv') # in same order as "countries" vector
iliiso <- iliiso[, c(1, count.indices + 1)]

### Read in syndromic/virologic counts:
test.dat <- read.csv('data/testCounts_052719.csv')
syn.dat <- read.csv('data/synDatCounts_060519.csv')
pos.dat <- read.csv('data/posProp_060519.csv')

test.dat <- test.dat[, c(1, count.indices + 1)]
syn.dat <- syn.dat[, c(1, count.indices + 1)]
pos.dat <- pos.dat[, c(1, count.indices + 1)]

### Reduce data to the study period (2010-2018):
iliiso <- iliiso[79:495, ]
test.dat <- test.dat[79:495, ]
syn.dat <- syn.dat[79:495, ]
pos.dat <- pos.dat[79:495, ]

### Replace -1 with NA:
for (i in 2:21) {
  iliiso[, i][iliiso[, i] == -1] <- NA
  syn.dat[, i][syn.dat[, i] == -1] <- NA
  test.dat[, i][test.dat[, i] == -1] <- NA
  pos.dat[, i][pos.dat[, i] == -1] <- NA
}

### Get total number of syndromic cases over the study period:
syn.total <- c()
par(mfrow = c(5, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 2:21) {
  syn.total <- c(syn.total, sum(syn.dat[, i], na.rm = TRUE))
  plot(syn.dat[, i], pch = 20, type = 'b', cex = 0.5, main = names(syn.dat)[i], xlab = '', ylab = 'ILI/ARI')
}

### Get total number of tests performed over the study period:
test.total <- c()
par(mfrow = c(5, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 2:21) {
  test.total <- c(test.total, sum(test.dat[, i], na.rm = TRUE))
  plot(test.dat[, i], pch = 20, type = 'b', cex = 0.5, main = names(test.dat)[i], xlab = '', ylab = 'ILI/ARI')
}

### Look at values for each country - is there some obvious cutoff?
df <- as.data.frame(cbind(countries, syn.total, test.total))
df$syn.total <- as.numeric(as.character(df$syn.total))
df$test.total <- as.numeric(as.character(df$test.total))

plot(log(df$syn.total), df$test.total)
cor.test(df$syn.total, df$test.total, method = 'spearman') # not significant

hist(df$syn.total, breaks = 50) # most are very small, with a few over 1e6 (FR, DE, SK)
# above 30,000: AT, BE, HR, CZ, DK, FR, DE, HU, IT, PL, SK, SI, ES, UK
    # below: IE, LU, NL, PT, RO, SE
# log is almost uniformly distributed
# below 20,000: IE, PT, RO, SE
    # below 10,000: PT, RO, SE
    # lowest are PT (2112) and SE (3678)
# but no obvious cutoff

hist(df$test.total, breaks = 25) # concentrated near lower values
# high outliers (>300,000): FR, UK, SE
    # SE had one of the lowest syndromic counts, but one of the highest test counts
# below 30,000: BE, HR, CZ, HU, LU, RO, SK
# below 20,000: BE, CZ, HU, LU, RO, SK
# below 10,000: BE, CZ, LU

# the only country falling into the bottom levels of both is RO

### Look at these numbers normalized by population size?:
    # Doesn't mean small countries are suddenly collecting a ton of data, but at least gives us some idea of how representative the data may be
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

df$syn.total.prop <- df$syn.total / pop.size$pop * 100 # in percent of population
df$test.total.prop <- df$test.total / pop.size$pop * 100

hist(df$syn.total.prop, breaks = 25) # most are under 10%
df[df$syn.total.prop < 1, ] # BE, DK, IE, NL, PT, RO, ES, SE, UK
df[df$syn.total.prop < 0.1, ] # PT, RO, SE
# so lowest are actually the same as when we don't standardize

hist(df$test.total.prop, breaks = 20) # most less than 1
df[df$test.total.prop < 0.1, ] # BE, CZ, DE, RO
# almost the same, but add DE - DE has >30,000 tests, but this is a low percentage

### Look at noise as well?:
# Note that proportion of population sampled wasn't significantly associated with accuracy in individual models
    # Smoothness was associated with PI, but not PT
smoothness <- c()
par(mfrow = c(5, 4), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 2:21) {
  x <- iliiso[, i]
  smoothness <- c(smoothness, cor(x[-length(x)], x[-1], use = 'pairwise.complete.obs'))
  plot(iliiso[, i], pch = 20, type = 'b', cex = 0.5, main = names(iliiso)[i], xlab = '', ylab = 'Syn+')  
}

df$smooth <- smoothness

par(mfrow = c(1, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
hist(df$smooth, breaks = 20) # all above 0.8 except LU
df[df$smooth < 0.85, ] # LU, PT
df[df$smooth < 0.90, ] # HR, IE, LU, PT, RO, SK, SE
# really only LU counts as substantially worse than the others

smooth.syn = smooth.test = c()
for (i in 2:21) {
  x <- syn.dat[, i]
  smooth.syn <- c(smooth.syn, cor(x[-length(x)], x[-1], use = 'pairwise.complete.obs'))
  x <- test.dat[, i]
  smooth.test <- c(smooth.test, cor(x[-length(x)], x[-1], use = 'pairwise.complete.obs'))
}
df$smooth.syn <- smooth.syn; df$smooth.test <- smooth.test

hist(df$smooth.syn, breaks = 20) # same pattern as iliiso
hist(df$smooth.test, breaks = 20) # almost all above 0.90 (except CZ, HU, SK)







