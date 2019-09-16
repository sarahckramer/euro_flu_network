
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

### "New" scalings ###
file.list.master <- list.files('results/', pattern = 'newScale')

file.list <- Filter(function(x) grepl('Met', x), file.list.master)
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'results/newScalings/outputMet_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OP_', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'results/newScalings/outputOP_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OPParams', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'results/newScalings/outputOPParams_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Dist', x), file.list.master)
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
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
write.csv(dist, file = 'results/newScalings/outputDist_090919.csv', row.names = FALSE)
write.csv(d.pt, file = 'results/newScalings/outputDist_090919_pt.csv', row.names = FALSE)
write.csv(d.ot, file = 'results/newScalings/outputDist_090919_ot.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Ens', x), file.list.master)
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
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
write.csv(e.pi, file = 'results/newScalings/outputEns_090919_PI.csv', row.names = FALSE)
write.csv(e1, file = 'results/newScalings/outputEns_090919_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'results/newScalings/outputEns_090919_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'results/newScalings/outputEns_090919_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'results/newScalings/outputEns_090919_4wk.csv', row.names = FALSE)
write.csv(e, file = 'results/newScalings/outputEns_090919.csv', row.names = FALSE)

### Proportional Random Travel ###
file.list.master <- list.files('results/', pattern = 'propRandTravel_')

file.list <- Filter(function(x) grepl('Met', x), file.list.master)
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'results/propRandTravel/outputMet_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OP_', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'results/propRandTravel/outputOP_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OPParams', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'results/propRandTravel/outputOPParams_090919.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Dist', x), file.list.master)
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
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
write.csv(dist, file = 'results/propRandTravel/outputDist_090919.csv', row.names = FALSE)
write.csv(d.pt, file = 'results/propRandTravel/outputDist_090919_pt.csv', row.names = FALSE)
write.csv(d.ot, file = 'results/propRandTravel/outputDist_090919_ot.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Ens', x), file.list.master)
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('results/', file.list[[i]]))
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
write.csv(e.pi, file = 'results/propRandTravel/outputEns_090919_PI.csv', row.names = FALSE)
write.csv(e1, file = 'results/propRandTravel/outputEns_090919_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'results/propRandTravel/outputEns_090919_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'results/propRandTravel/outputEns_090919_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'results/propRandTravel/outputEns_090919_4wk.csv', row.names = FALSE)
write.csv(e, file = 'results/propRandTravel/outputEns_090919.csv', row.names = FALSE)

### And reduce "original" files to oev_denom 10 and lambda 1.02:
file.list <- list.files(path = 'code/gridSearch/outputs/')
for (ix in 14:length(file.list)) {
  dat.temp <- read.csv(paste0('code/gridSearch/outputs/', file.list[[ix]]))
  
  print(table(dat.temp$oev_denom))
  print(table(dat.temp$lambda))
  print('')
  
  dat.temp <- dat.temp[dat.temp$oev_denom == 10 & dat.temp$lambda == 1.02, ]
  print(head(dat.temp))
  print('')
  
  write.csv(dat.temp, paste0('results/original/', file.list[[ix]]), row.names = FALSE)
}

file.list <- list.files(path = 'results/original/')
for (ix in 1:length(file.list)) {
  dat.temp <- read.csv(paste0('results/original/', file.list[[ix]]))
  dat.temp <- dat.temp[dat.temp$run %in% c(1, 2), ]
  write.csv(dat.temp, paste0('results/original/', file.list[[ix]]), row.names = FALSE)
}

### And for individual country runs:
file.list <- list.files(path = 'code/individualCountries/outputs/')
for (ix in 1:length(file.list)) {
  dat.temp <- read.csv(paste0('code/individualCountries/outputs/', file.list[[ix]]))
  
  print(table(dat.temp$oev_denom, dat.temp$lambda))
  dat.temp <- dat.temp[dat.temp$oev_denom == 10 & dat.temp$lambda == 1.02 & dat.temp$run %in% c(1, 2), ]
  print(head(dat.temp))
  print('')
  
  write.csv(dat.temp, paste0('results/indivCountries/', file.list[[ix]]), row.names = FALSE)
}

##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################

### Higher OEV-Base ###
file.list.master <- list.files('results/highOEVBase/')

file.list <- Filter(function(x) grepl('Met', x), file.list.master)
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('results/highOEVBase/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'results/highOEVBase/outputMet_091619.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OP_', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/highOEVBase/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'results/highOEVBase/outputOP_091619.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('OPParams', x), file.list.master)
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('results/highOEVBase/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'results/highOEVBase/outputOPParams_091619.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Dist', x), file.list.master)
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('results/highOEVBase/', file.list[[i]]))
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
write.csv(dist, file = 'results/highOEVBase/outputDist_091619.csv', row.names = FALSE)
write.csv(d.pt, file = 'results/highOEVBase/outputDist_091619_pt.csv', row.names = FALSE)
write.csv(d.ot, file = 'results/highOEVBase/outputDist_091619_ot.csv', row.names = FALSE)

file.list <- Filter(function(x) grepl('Ens', x), file.list.master)
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('results/highOEVBase/', file.list[[i]]))
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
write.csv(e.pi, file = 'results/highOEVBase/outputEns_091619_PI.csv', row.names = FALSE)
write.csv(e1, file = 'results/highOEVBase/outputEns_091619_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'results/highOEVBase/outputEns_091619_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'results/highOEVBase/outputEns_091619_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'results/highOEVBase/outputEns_091619_4wk.csv', row.names = FALSE)
write.csv(e, file = 'results/highOEVBase/outputEns_091619.csv', row.names = FALSE)















