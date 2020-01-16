
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# Read in scaled syndromic and pos for A(all):
syn.a <- read.csv('data/by_subtype/synDatCounts_A(all)_SCALED.csv')
pos.a <- read.csv('data/by_subtype/posprop_A(all).csv')
pos.a <- pos.a[, c(1, count.indices + 1)]

# Read in scaled syndromic and pos for H1:
syn.h1 <- read.csv('data/by_subtype/synDatCounts_A(H1)_SCALED.csv')
pos.h1 <- read.csv('data/by_subtype/posprop_A(H1).csv')
pos.h1 <- pos.h1[, c(1, count.indices + 1)]

# Read in scaled syndromic and pos for H1:
syn.h3 <- read.csv('data/by_subtype/synDatCounts_A(H3)_SCALED.csv')
pos.h3 <- read.csv('data/by_subtype/posprop_A(H3).csv')
pos.h3 <- pos.h3[, c(1, count.indices + 1)]

# Read in scaled syndromic and pos for H1:
syn.b <- read.csv('data/by_subtype/synDatCounts_B_SCALED.csv')
pos.b <- read.csv('data/by_subtype/posprop_B.csv')
pos.b <- pos.b[, c(1, count.indices + 1)]

# Also read in syndromic data and number of tests by country:
syn.dat <- read.csv('data/synDatCounts_060519.csv')
syn.dat <- syn.dat[, c(1, count.indices + 1)]
test.dat <- read.csv('data/testCounts_052719.csv')
test.dat <- test.dat[, c(1, count.indices + 1)]
test.rate <- read.csv('data/testRates_010820.csv')

# First, plot variabilitiy in raw synthetic and test data by country:
syn.dat <- melt(syn.dat); names(syn.dat) <- c('time', 'country', 'value')
test.dat <- melt(test.dat); names(test.dat) <- c('time', 'country', 'value')
test.rate <- melt(test.rate); names(test.rate) <- c('time', 'country', 'value')

syn.dat$value[syn.dat$value < 0] <- NA
test.dat$value[test.dat$value < 0] <- NA

p1 <- ggplot(data = syn.dat, aes(x = time, y = value, group = country)) + geom_line() +
  facet_wrap(~country, scales = 'free_y', ncol = 1) + theme_classic() + labs(y = 'Syndromic Data')
p1
p2 <- ggplot(data = test.dat, aes(x = time, y = value, group = country)) + geom_line() +
  facet_wrap(~country, scales = 'free_y', ncol = 1) + theme_classic() + labs(y = 'Test Counts (Raw)')
p2
p25 <- ggplot(data = test.rate, aes(x = time, y = value, group = country)) + geom_line() +
  facet_wrap(~country, scales = 'free_y', ncol = 1) + theme_classic() + labs(y = 'Test Counts (Scaled)')
p25

# Plot out scaled syndromic data by strain/country
syn.a <- melt(syn.a); names(syn.a) <- c('time', 'country', 'value')
syn.h1 <- melt(syn.h1); names(syn.h1) <- c('time', 'country', 'value')
syn.h3 <- melt(syn.h3); names(syn.h3) <- c('time', 'country', 'value')
syn.b <- melt(syn.b); names(syn.b) <- c('time', 'country', 'value')

syn.a$strain <- 'A(all)'; syn.h1$strain <- 'H1'; syn.h3$strain <- 'H3'; syn.b$strain <- 'B'

syn.scaled <- rbind(syn.a, syn.h1, syn.h3, syn.b)
syn.scaled$strain <- factor(syn.scaled$strain)
syn.scaled$strain <- factor(syn.scaled$strain, levels = levels(syn.scaled$strain)[c(1, 3:4, 2)])

p3 <- ggplot(data = syn.scaled, aes(x = time, y = value, group = strain, colour = strain)) + geom_line() + 
  facet_wrap(~country, scale = 'free_y', ncol = 1) + theme_classic() + labs(y = 'Syndromic (Scaled)')
p3

# Plot out posprop by strain/country
pos.a <- melt(pos.a); names(pos.a) <- c('time', 'country', 'value')
pos.h1 <- melt(pos.h1); names(pos.h1) <- c('time', 'country', 'value')
pos.h3 <- melt(pos.h3); names(pos.h3) <- c('time', 'country', 'value')
pos.b <- melt(pos.b); names(pos.b) <- c('time', 'country', 'value')

pos.a$strain <- 'A(all)'; pos.h1$strain <- 'H1'; pos.h3$strain <- 'H3'; pos.b$strain <- 'B'

pos.dat <- rbind(pos.a, pos.h1, pos.h3, pos.b)
pos.dat$strain <- factor(pos.dat$strain)
pos.dat$strain <- factor(pos.dat$strain, levels = levels(pos.dat$strain)[c(1, 3:4, 2)])

pos.dat$value[pos.dat$value < 0] <- NA

p4 <- ggplot(data = pos.dat, aes(x = time, y = value, group = strain, colour = strain)) + geom_line() + 
  facet_wrap(~country, scale = 'free_y', ncol = 1) + theme_classic() + labs(y = '% Positive')
p4

# Save:
pdf('results/explore_oev/plot_out_syn_test_prop_data.pdf', height = 10, width = 13)
print(p1)
print(p2)
print(p25)
print(p3)
print(p4)
dev.off()

pdf('results/explore_oev/plot_out_syn_test_prop_data_wA.pdf', height = 10, width = 13)
print(p1)
print(p2)
print(p25)
print(p3)
print(p4)
dev.off()

