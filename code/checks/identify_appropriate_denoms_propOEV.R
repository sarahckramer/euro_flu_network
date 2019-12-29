
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

oev_base <- 0.6
oev_denom <- 3

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# would need oev_denom 10 to get all less than 1, but this is due to a single point during the pandemic, which we could consider irrelevant here; otherwise 2.5 works

# H3:
prop.dat <- read.csv('data/by_subtype/posprop_A(H3).csv')
prop.dat <- prop.dat[, count.indices + 1]
prop.dat[prop.dat < 0] <- NA
matplot(prop.dat, pch = 20, type = 'b', lty = 1, col = viridis(12))

oev_base <- 0.5
oev_denom <- 1.5

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# here even 1.5 is good enough

# B:
prop.dat <- read.csv('data/by_subtype/posprop_B.csv')
prop.dat <- prop.dat[, count.indices + 1]
prop.dat[prop.dat < 0] <- NA
matplot(prop.dat, pch = 20, type = 'b', lty = 1, col = viridis(12))

oev_base <- 0.6
oev_denom <- 3

obs.vars <- calc_obsvars(obs = as.matrix(prop.dat), oev_base, oev_denom)
matplot(obs.vars, pch = 20, type = 'b', lty = 1, col = viridis(12))
which(obs.vars >= 1.0, arr.ind = T)

# 2.5 works


