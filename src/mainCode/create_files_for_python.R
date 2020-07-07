
## Want to save commuting/population; air; synthetic "outbreak" data as txt files for python_init ###

# Countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get population sizes at country level:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Read in commuting data:
seasons = c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')

load('src/formatTravelData/formattedData/comm_mat1.RData')
load('src/formatTravelData/formattedData/comm_mat2.RData')
load('src/formatTravelData/formattedData/comm_mat3.RData')
load('src/formatTravelData/formattedData/comm_mat4.RData')
load('src/formatTravelData/formattedData/comm_mat5.RData')

# For each of the five matrices, generate text files for use with python_init mainCode:
N <- comm.list1
for (i in 1:300) {
  N[[i]] <- lapply(N[[i]], function(ix) {
    diag(ix) <- unlist(lapply(1:n, function(jx) {
      pop.size$pop[jx] - rowSums(ix)[jx]
    }))
    return(ix)
  })
}
if (!dir.exists('data/python_init/compartment_sizes/')) {
  dir.create('data/python_init/')
  dir.create('data/python_init/compartment_sizes/')
}
for (ensmem in 1:length(N)) {
  N.temp <- N[[ensmem]]
  for (i in 1:length(N.temp)) {
    write.table(N.temp[[i]], file = paste0('data/python_init/compartment_sizes/N', seasons[i], '_', 1, '_', ensmem, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
  }
}
rm(comm.list1, comm.list2, comm.list3, comm.list4, comm.list5)

# For synthetic, just average over all eight seasons for the 5 runs:
load('src/formatTravelData/formattedData/comm_mat1.RData')
load('src/formatTravelData/formattedData/comm_mat2.RData')
load('src/formatTravelData/formattedData/comm_mat3.RData')
load('src/formatTravelData/formattedData/comm_mat4.RData')
load('src/formatTravelData/formattedData/comm_mat5.RData')

t.comm1 <- lapply(comm.list1, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
t.comm2 <- lapply(comm.list2, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
t.comm3 <- lapply(comm.list3, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
t.comm4 <- lapply(comm.list4, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
t.comm5 <- lapply(comm.list5, function(ix) {
  apply(simplify2array(ix), 1:2, mean)
})
rm(comm.list1, comm.list2, comm.list3, comm.list4, comm.list5)

# Again, do the following for all 5 matrices:
N <- t.comm5
for (i in 1:300) {
  diag(N[[i]]) <- unlist(lapply(1:n, function(ix) {
    pop.size$pop[ix] - rowSums(N[[i]])[ix]
  }))
}
if (!dir.exists('data/python_init/compartment_sizes_SYNTH/')) {
  # dir.create('data/python_init/')
  dir.create('data/python_init/compartment_sizes_SYNTH/')
}
for (ensmem in 1:length(N)) {
  write.table(N[[ensmem]], file = paste0('data/python_init/compartment_sizes_SYNTH/N_', 5, '_', ensmem, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
}

### Get air travel data:
air.dat <- list()
for (i in 1:12) {
  load(paste0('src/formatTravelData/formattedData/air_', i, '_01-31.RData'))
  a.temp.sym <- a.temp.sym[countries, countries]
  air.dat[[i]] <- a.temp.sym
  rm(a.temp.sym)
}

# Write to files:
if (!dir.exists('data/python_init/air_travel/')) {
  # dir.create('data/python_init/')
  dir.create('data/python_init/air_travel/')
}
for (i in 1:12) {
  write.table(air.dat[[i]], file = paste0('data/python_init/air_travel/aRand', i, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
}

### Get start states/params:
set.seed(1048993542)
library(tgp)

D_low <- 2; L_low <- 3*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
S0_low <- 0.4; S0_up <- 0.9
I0_low <- 0; I0_up <- 0.00005
n <- 12

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
                     c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))

if (!dir.exists('data/python_init/initial_parms/')) {
  dir.create('data/python_init/')
  dir.create('data/python_init/initial_parms/')
}

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms1.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms2.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms3.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms4.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

# for isolated:
theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up)

param.bound <- cbind(c(S0_low, I0_low, theta_low), c(S0_up, I0_up, theta_up))

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms0_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms1_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms2_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms3_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'data/python_init/initial_parms/parms4_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

### Write synthetic data
load('src/syntheticTests/syntheticData/for_synthetic_testing/synth_rates_toKeep_070220_wError_1e5_10.RData')
iliiso <- read.csv('data/WHO_data_A(H1)_SCALED.csv')

obs1 <- synth.outbreaks[[1]]; obs1 <- as.data.frame(obs1); obs1 <- cbind(1:52, obs1); names(obs1) <- c('time', countries)
obs2 <- synth.outbreaks[[2]]; obs2 <- as.data.frame(obs2); obs2 <- cbind(1:52, obs2); names(obs2) <- c('time', countries)
obs3 <- synth.outbreaks[[3]]; obs3 <- as.data.frame(obs3); obs3 <- cbind(1:52, obs3); names(obs3) <- c('time', countries)
obs4 <- synth.outbreaks[[4]]; obs4 <- as.data.frame(obs4); obs4 <- cbind(1:52, obs4); names(obs4) <- c('time', countries)
obs5 <- synth.outbreaks[[5]]; obs5 <- as.data.frame(obs5); obs5 <- cbind(1:52, obs5); names(obs5) <- c('time', countries)

if (!dir.exists('src/syntheticTests/syntheticData/for_python/')) {
  dir.create('src/syntheticTests/syntheticData/for_python/')
}
write.csv(obs1, file = 'src/syntheticTests/syntheticData/for_python/synth_wError_1.csv', row.names = FALSE)
write.csv(obs2, file = 'src/syntheticTests/syntheticData/for_python/synth_wError_2.csv', row.names = FALSE)
write.csv(obs3, file = 'src/syntheticTests/syntheticData/for_python/synth_wError_3.csv', row.names = FALSE)
write.csv(obs4, file = 'src/syntheticTests/syntheticData/for_python/synth_wError_4.csv', row.names = FALSE)
write.csv(obs5, file = 'src/syntheticTests/syntheticData/for_python/synth_wError_5.csv', row.names = FALSE)

rm(list = ls())

