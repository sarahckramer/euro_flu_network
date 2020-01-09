
# Set countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

# OEV function:
source('cluster/functions/calc_obsvars.R')

# H1:
prop.dat <- read.csv('data/by_subtype/posprop_A(H1).csv')
prop.dat <- prop.dat[, count.indices + 1]
prop.dat[prop.dat < 0] <- NA
matplot(prop.dat, pch = 20, type = 'b', lty = 1, col = viridis(12))

oev_base <- 0.8
oev_denom <- 5.1

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# 2.6 works; for 0.8: 5.1

# H3:
prop.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')
prop.dat <- prop.dat[, count.indices + 1]
prop.dat[prop.dat < 0] <- NA
matplot(prop.dat, pch = 20, type = 'b', lty = 1, col = viridis(12))

oev_base <- 0.8
oev_denom <- 3.1

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# here even 1.6 is good enough; for 0.8: 3.1

# B:
prop.dat <- read.csv('data/by_subtype/posprop_B.csv')
prop.dat <- prop.dat[, count.indices + 1]
prop.dat[prop.dat < 0] <- NA
matplot(prop.dat, pch = 20, type = 'b', lty = 1, col = viridis(12))

oev_base <- 0.8
oev_denom <- 5.1

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# 2.6 works; for 0.8: 5.1

# so if we use these, maybe starting denominator is like 10?


