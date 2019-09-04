
# Read in and compile all:
file.list <- list.files('code/gridSearch/outputs/fcasts_082819/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_082819/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'code/gridSearch/outputs/outputMet_090119.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_082819/', pattern = 'OP_')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_082819/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'code/gridSearch/outputs/outputOP_090119.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_082819/', pattern = 'OPParams')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_082819/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'code/gridSearch/outputs/outputOPParams_090119.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_082819/', pattern = 'Dist')
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_082819/', file.list[[i]]))
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
write.csv(dist, file = 'code/gridSearch/outputs/outputDist_090119.csv', row.names = FALSE)
write.csv(d.pt, file = 'code/gridSearch/outputs/outputDist_090119_pt.csv', row.names = FALSE)
write.csv(d.ot, file = 'code/gridSearch/outputs/outputDist_090119_ot.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_082819/', pattern = 'Ens_')
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_082819/', file.list[[i]]))
}
e = e.pi = e.1wk = e.2wk = e.3wk = e.4wk = NULL
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
write.csv(e.pi, file = 'code/gridSearch/outputs/outputEns_090119_PI.csv', row.names = FALSE)
write.csv(e1, file = 'code/gridSearch/outputs/outputEns_090119_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'code/gridSearch/outputs/outputEns_090119_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'code/gridSearch/outputs/outputEns_090119_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'code/gridSearch/outputs/outputEns_090119_4wk.csv', row.names = FALSE)
write.csv(e, file = 'code/gridSearch/outputs/outputEns_090119.csv', row.names = FALSE)

### Also for individual country forecasts:
file.list <- list.files('code/individualCountries/outputs/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/individualCountries/outputs/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'code/individualCountries/outputs/outputMet_082819.csv', row.names = FALSE)

file.list <- list.files('code/individualCountries/outputs/', pattern = 'OP')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/individualCountries/outputs/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'code/individualCountries/outputs/outputOP_082819.csv', row.names = FALSE)

file.list <- list.files('code/individualCountries/outputs/', pattern = 'Dist')
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('code/individualCountries/outputs/', file.list[[i]]))
}
dist <- NULL
for (i in 1:length(file.list)) {
  dist <- rbind(dist, dist.list[[i]])
}
rm(dist.list)
write.csv(dist, file = 'code/individualCountries/outputs/outputDist_082819.csv', row.names = FALSE)

d.pt <- dist[dist$metric == 'pw', ]
d.ot <- dist[dist$metric == 'onset5', ]

write.csv(d.pt, file = 'code/individualCountries/outputs/outputDist_082819_PT.csv', row.names = FALSE)
write.csv(d.ot, file = 'code/individualCountries/outputs/outputDist_082819_OT.csv', row.names = FALSE)

# write.csv(e.pi, file = 'code/gridSearch/outputs/outputEns_082819_PI.csv', row.names = FALSE)
# write.csv(e1, file = 'code/gridSearch/outputs/outputEns_082819_1wk.csv', row.names = FALSE)
# write.csv(e2, file = 'code/gridSearch/outputs/outputEns_082819_2wk.csv', row.names = FALSE)
# write.csv(e3, file = 'code/gridSearch/outputs/outputEns_082819_3wk.csv', row.names = FALSE)
# write.csv(e4, file = 'code/gridSearch/outputs/outputEns_082819_4wk.csv', row.names = FALSE)











