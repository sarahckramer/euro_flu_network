
# Want to save commuting/population; air data as txt files for python

# Countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)
n <- length(countries)

# Get population sizes at country level:
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# # Read in commuting data:
# load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
# comm.by.year <- lapply(comm.by.year, function(ix) {
#   ix[countries, countries]
# })
# 
# # Calculate commuting for SEASON, not year:
# comm.by.seas <- vector('list', 8)
# for (i in 1:7) {
#   comm.by.seas[[i]] <- (comm.by.year[[i]] + comm.by.year[[i + 1]]) / 2
# }
# comm.by.seas[[8]] <- comm.by.year[[8]]
# # save(comm.by.seas, file = 'formatTravelData/formattedData/comm_mat_by_season_05-07_RELIABLE_ONLY.RData')
# 
# # Get population counts by season:
# N <- comm.by.seas
# N <- lapply(N, function(ix) {
#   diag(ix) <- unlist(lapply(1:n, function(jx) {
#     pop.size$pop[jx] - rowSums(ix)[jx]
#   }))
#   return(ix)
# })
# 
# # N.check <- comm.by.seas
# # for (i in 1:length(N.check)) {
# #   diag(N.check[[i]]) <- unlist(lapply(1:n, function(ix) {
# #     pop.size$pop[ix] - rowSums(N.check[[i]])[ix]
# #   }))
# # }
# # 
# # all.equal(N, N.check)
# 
# # Write to txt files:
# seasons = c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
# for (i in 1:length(N)) {
#   write.table(N[[i]], file = paste0('python/compartment_sizes/N', seasons[i], '_NEW.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
#   # write.table(mydata, "c:/mydata.txt", sep="\t") 
# }

# Get reliable+unreliable+... adjacent-only, or full:

seasons = c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')

load('formatTravelData/formattedData/comm_mat1_ADJ.RData')
load('formatTravelData/formattedData/comm_mat2_ADJ.RData')
load('formatTravelData/formattedData/comm_mat3_ADJ.RData')
load('formatTravelData/formattedData/comm_mat4_ADJ.RData')
load('formatTravelData/formattedData/comm_mat5_ADJ.RData')
N <- comm.list1
for (i in 1:300) {
  N[[i]] <- lapply(N[[i]], function(ix) {
    diag(ix) <- unlist(lapply(1:n, function(jx) {
      pop.size$pop[jx] - rowSums(ix)[jx]
    }))
    return(ix)
  })
}

for (ensmem in 1:length(N)) {
  N.temp <- N[[ensmem]]
  for (i in 1:length(N.temp)) {
    write.table(N.temp[[i]], file = paste0('python/compartment_sizes_NEW1/N', seasons[i], '_', 1, '_', ensmem, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
  }
}
rm(comm.list1, comm.list2, comm.list3, comm.list4, comm.list5)

# For synthetic, just average over all eight seasons for the 5 runs:
load('formatTravelData/formattedData/comm_mat1.RData')
load('formatTravelData/formattedData/comm_mat2.RData')
load('formatTravelData/formattedData/comm_mat3.RData')
load('formatTravelData/formattedData/comm_mat4.RData')
load('formatTravelData/formattedData/comm_mat5.RData')

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

N <- t.comm5
for (i in 1:300) {
  diag(N[[i]]) <- unlist(lapply(1:n, function(ix) {
    pop.size$pop[ix] - rowSums(N[[i]])[ix]
  }))
}
for (ensmem in 1:length(N)) {
  write.table(N[[ensmem]], file = paste0('python/compartment_sizes_SYNTH/N_', 5, '_', ensmem, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
}

# ### Get air travel data:
# air.dat <- list()
# for (i in 1:12) {
#   load(paste0('formatTravelData/formattedData/air_', i, '_01-31.RData'))
#   a.temp.sym <- a.temp.sym[countries, countries]
#   air.dat[[i]] <- a.temp.sym
#   rm(a.temp.sym)
# }
# 
# # Write to files:
# for (i in 1:12) {
#   write.table(air.dat[[i]], file = paste0('python/air_travel/aRand', i, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
# }

### Get start states/params:
set.seed(1048993542)

D_low <- 2; L_low <- 3*365; Rmx_low <- 2.0; Rdiff_low <- 0.2; airScale_low <- 0.75
D_up <- 7; L_up <- 10*365; Rmx_up <- 2.8; Rdiff_up <- 1.0; airScale_up <- 1.25
S0_low <- 0.4; S0_up <- 0.9
I0_low <- 0; I0_up <- 0.00005
n <- 12

theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low, airScale_low)
theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up, airScale_up)

param.bound <- cbind(c(rep(S0_low, n ** 2), rep(I0_low, n ** 2), theta_low),
                     c(rep(S0_up, n ** 2), rep(I0_up, n ** 2), theta_up))

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/SA/parms0_verylowI0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/SA/parms1_verylowI0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/SA/parms2_verylowI0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/SA/parms3_verylowI0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

parms <- t(lhs(300, param.bound))
write.table(parms, file = 'python/initial_parms/SA/parms4_verylowI0.txt', sep = '\t', row.names = FALSE, col.names = FALSE)
# 
# # for isolated:
# theta_low <- c(L_low, D_low, Rmx_low, Rdiff_low)
# theta_up <- c(L_up, D_up, Rmx_up, Rdiff_up)
# 
# param.bound <- cbind(c(S0_low, I0_low, theta_low), c(S0_up, I0_up, theta_up))
# 
# parms <- t(lhs(300, param.bound))
# write.table(parms, file = 'python/initial_parms/parms0_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)
# 
# parms <- t(lhs(300, param.bound))
# write.table(parms, file = 'python/initial_parms/parms1_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)
# 
# parms <- t(lhs(300, param.bound))
# write.table(parms, file = 'python/initial_parms/parms2_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)
# 
# parms <- t(lhs(300, param.bound))
# write.table(parms, file = 'python/initial_parms/parms3_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)
# 
# parms <- t(lhs(300, param.bound))
# write.table(parms, file = 'python/initial_parms/parms4_INDIV.txt', sep = '\t', row.names = FALSE, col.names = FALSE)

# ### Write synthetic data
# load('syntheticTests/syntheticData/synth_rates_toKeep_021020_wError_1e5_10.RData')
# iliiso <- read.csv('data/by_subtype/WHO_data_A(H1)_SCALED.csv')
# 
# obs1 <- synth.outbreaks[[1]]; obs1 <- as.data.frame(obs1); obs1 <- cbind(1:52, obs1); names(obs1) <- c('time', countries)
# obs2 <- synth.outbreaks[[2]]; obs2 <- as.data.frame(obs2); obs2 <- cbind(1:52, obs2); names(obs2) <- c('time', countries)
# obs3 <- synth.outbreaks[[3]]; obs3 <- as.data.frame(obs3); obs3 <- cbind(1:52, obs3); names(obs3) <- c('time', countries)
# obs4 <- synth.outbreaks[[4]]; obs4 <- as.data.frame(obs4); obs4 <- cbind(1:52, obs4); names(obs4) <- c('time', countries)
# obs5 <- synth.outbreaks[[5]]; obs5 <- as.data.frame(obs5); obs5 <- cbind(1:52, obs5); names(obs5) <- c('time', countries)
# 
# write.csv(obs1, file = 'syntheticTests/syntheticData/for_python/synth_wError_1.csv', row.names = FALSE)
# write.csv(obs2, file = 'syntheticTests/syntheticData/for_python/synth_wError_2.csv', row.names = FALSE)
# write.csv(obs3, file = 'syntheticTests/syntheticData/for_python/synth_wError_3.csv', row.names = FALSE)
# write.csv(obs4, file = 'syntheticTests/syntheticData/for_python/synth_wError_4.csv', row.names = FALSE)
# write.csv(obs5, file = 'syntheticTests/syntheticData/for_python/synth_wError_5.csv', row.names = FALSE)

load('syntheticTests/syntheticData/synth_rates_ISOL_error.RData')
# newI.keep <- t(newI.keep)
write.csv(newI.keep, file = 'syntheticTests/syntheticData/for_python/synth_wError_ISOLATED.csv', row.names = FALSE)



