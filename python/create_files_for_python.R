
# Want to save commuting/population; air data as txt files for python

# Countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get population sizes at country level:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in commuting data:
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
comm.by.year <- lapply(comm.by.year, function(ix) {
  ix[countries, countries]
})

# Get population counts by season:
N <- comm.by.year
N <- lapply(N, function(ix) {
  diag(ix) <- unlist(lapply(1:n, function(jx) {
    pop.size$pop[jx] - rowSums(ix)[jx]
  }))
  return(ix)
})

# N.check <- comm.by.year
# for (i in 1:length(N.check)) {
#   diag(N.check[[i]]) <- unlist(lapply(1:n, function(ix) {
#     pop.size$pop[ix] - rowSums(N.check[[i]])[ix]
#   }))
# }
# 
# all.equal(N, N.check)

# Write to txt files:
seasons = c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
for (i in 1:length(N)) {
  write.table(N[[i]], file = paste0('python/compartment_sizes/N', seasons[i], '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
  # write.table(mydata, "c:/mydata.txt", sep="\t") 
}

# Get air travel data:
air.dat <- list()
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  a.temp.sym <- a.temp.sym[countries, countries]
  air.dat[[i]] <- a.temp.sym
  rm(a.temp.sym)
}

# Write to files:
for (i in 1:12) {
  write.table(air.dat[[i]], file = paste0('python/air_travel/aRand', i, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
}

# Get start states/params:
D_low <- 2; L_low <- 1*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 8*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
S0_low <- 0.3; S0_up <- 0.9
I0_low <- 0; I0_up <- 0.00005

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
                     c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/parms0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/parms1.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/parms2.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/parms3.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/parms4.txt', sep = '\t', row.names = FALSE, col.names = FALSE)



















