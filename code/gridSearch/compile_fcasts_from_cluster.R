
# Identify missing:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18') # ADD '2018-19'
oevBase_list <- c(1e4, 1e5)
ntrnList <- 5:30

for (ix in 1:416) {
  season <- seasons[ceiling(ix / 52)]
  oev_base <- oevBase_list[ceiling((ix - 26) / 26) %% 2 + 1]
  ntrn <- ntrnList[ceiling(ix - 1) %% 26 + 1]
  
  if (!file.exists(paste0('results/raw/outputMet_', season, '_', oev_base, '_', ntrn, '_110719.csv'))) {
    print(ix)
  }
}
# 31, 57:58

#########################################################################################################################################################################
#########################################################################################################################################################################
#########################################################################################################################################################################

# Read in and compile all:
file.list <- list.files('results/raw/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('results/raw/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'results/outputMet_111219_INDIV.csv', row.names = FALSE)

file.list <- list.files('results/raw/', pattern = 'OP_')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/raw/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'results/outputOP_111219_INDIV.csv', row.names = FALSE)

file.list <- list.files('results/raw/', pattern = 'OPParams')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/raw/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'results/outputOPParams_110919_oldOEV_denom1.csv', row.names = FALSE)

file.list <- list.files('results/raw/', pattern = 'Dist')
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('results/raw/', file.list[[i]]))
}
dist <- NULL
for (i in 1:length(file.list)) {
  if (i %% 100 == 0) {
    print(i)
  }
  dist <- rbind(dist, dist.list[[i]])
}
rm(dist.list)
d.pt <- dist[dist$metric == 'pw', ]
d.ot <- dist[dist$metric == 'onset5', ]
write.csv(dist, file = 'results/outputDist_111219_INDIV.csv', row.names = FALSE)
write.csv(d.pt, file = 'results/outputDist_111219_INDIV_pt.csv', row.names = FALSE)
write.csv(d.ot, file = 'results/outputDist_111219_INDIV_ot.csv', row.names = FALSE)

file.list <- list.files('results/raw/', pattern = 'Ens_')
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('results/raw/', file.list[[i]]))
}
e <- NULL
for (i in 1:length(file.list)) {
  if (i %% 100 == 0) {
    print(i)
  }
  e <- rbind(e, ens.list[[i]])
}
rm(ens.list)
e.pi <- e[e$metric == 'pi', ]
e1 <- e[e$metric == '1week', ]
e2 <- e[e$metric == '2week', ]
e3 <- e[e$metric == '3week', ]
e4 <- e[e$metric == '4week', ]
write.csv(e.pi, file = 'results/outputEns_111219_INDIV_PI.csv', row.names = FALSE)
write.csv(e1, file = 'results/outputEns_111219_INDIV_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'results/outputEns_111219_INDIV_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'results/outputEns_111219_INDIV_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'results/outputEns_111219_INDIV_4wk.csv', row.names = FALSE)
write.csv(e, file = 'results/outputEns_111219_INDIV.csv', row.names = FALSE)




