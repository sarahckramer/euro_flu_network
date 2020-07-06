### Takes (sub)type-specific results files and compiles them ###

### NETWORK ###
# Read in all (7) files for each of 3 subtypes and combine:
res.list <- list()

file.list.h1 <- list.files(path = 'results/by_subtype/network_A(H1)/', pattern = '.csv')
file.list.h3 <- list.files(path = 'results/by_subtype/network_A(H3)/', pattern = '.csv')
file.list.b <- list.files(path = 'results/by_subtype/network_B/', pattern = '.csv')

for (i in 1:length(file.list.h1)) {
  f1 <- read.csv(paste0('results/by_subtype/network_A(H1)/', file.list.h1[i]))
  f2 <- read.csv(paste0('results/by_subtype/network_A(H3)/', file.list.h3[i]))
  f3 <- read.csv(paste0('results/by_subtype/network_B/', file.list.b[i]))
  
  f1$subtype = 'A(H1)'
  f2$subtype = 'A(H3)'
  f3$subtype = 'B'
  
  f <- rbind(f1, f2, f3)
  res.list[[i]] <- f
}
rm(f, f1, f2, f3, i)

# Write all combined files as csvs:
for (i in 1:length(res.list)) {
  if (i != 4) {
    write.csv(res.list[[i]], file = paste0('results/network/', file.list.h1[i]), row.names = FALSE)
  } else {
    write.csv(res.list[[i]], file = paste0('results/network/', 'outputOPParams.csv'), row.names = FALSE)
  }

}

# Clean up:
rm(list = ls())

### ISOLATED ###
# Read in all (5) files for each of 3 subtypes and combine:
res.list <- list()

file.list.h1 <- list.files(path = 'results/by_subtype/isolated_A(H1)/', pattern = '.csv')
file.list.h3 <- list.files(path = 'results/by_subtype/isolated_A(H3)/', pattern = '.csv')
file.list.b <- list.files(path = 'results/by_subtype/isolated_B/', pattern = '.csv')

for (i in 1:length(file.list.h1)) {
  f1 <- read.csv(paste0('results/by_subtype/isolated_A(H1)/', file.list.h1[i]))
  f2 <- read.csv(paste0('results/by_subtype/isolated_A(H3)/', file.list.h3[i]))
  f3 <- read.csv(paste0('results/by_subtype/isolated_B/', file.list.b[i]))
  
  f1$subtype = 'A(H1)'
  f2$subtype = 'A(H3)'
  f3$subtype = 'B'
  
  f <- rbind(f1, f2, f3)
  res.list[[i]] <- f
}
rm(f, f1, f2, f3, i)

# Write all combined files as csvs:
for (i in 1:length(res.list)) {
  write.csv(res.list[[i]], file = paste0('results/isolated/', file.list.h1[i]), row.names = FALSE)
}

# Clean up:
rm(list = ls())

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

### Dist and Ens ###

# Read in and join dist/network:
d1.h1 <- read.csv('results/by_subtype/network_A(H1)/extra/outputDist_A(H1).csv')
d1.h3 <- read.csv('results/by_subtype/network_A(H3)/extra/outputDist_A(H3).csv')
d1.b <- read.csv('results/by_subtype/network_B/extra/outputDist_B.csv')

d1.h1$subtype <- 'A(H1)'; d1.h3$subtype <- 'A(H3)'; d1.b$subtype <- 'B'

d1 <- rbind(d1.h1, d1.h3, d1.b)
rm(d1.h1, d1.h3, d1.b)
d1 <- d1[d1$metric %in% c('pw', 'onset5'), ]

# Read in and join dist/isolated:
d2.h1 <- read.csv('results/by_subtype/isolated_A(H1)/extra/outputDist_A(H1)_ISOLATED.csv')
d2.h3 <- read.csv('results/by_subtype/isolated_A(H3)/extra/outputDist_A(H3)_ISOLATED.csv')
d2.b <- read.csv('results/by_subtype/isolated_B/extra/outputDist_B_ISOLATED.csv')

d2.h1$subtype <- 'A(H1)'; d2.h3$subtype <- 'A(H3)'; d2.b$subtype <- 'B'

d2 <- rbind(d2.h1, d2.h3, d2.b)
rm(d2.h1, d2.h3, d2.b)
d2 <- d2[d2$metric %in% c('pw', 'onset5'), ]

# Remove network forecasts where there are no data and therefore no isolated forecasts:
set1 <- unique(d1[, c(1:2, 4, 6:7, 11)])
set2 <- unique(d2[, c(1:2, 5:7, 11)])

set1$group <- paste(set1$fc_start, set1$metric, set1$country, set1$season, set1$run, set1$subtype, sep = '_'); set1$group <- factor(set1$group)
set2$group <- paste(set2$fc_start, set2$metric, set2$country, set2$season, set2$run, set2$subtype, sep = '_'); set2$group <- factor(set2$group)

print(summary(levels(set2$group) %in% levels(set1$group))) # all true - good!
print(summary(levels(set1$group) %in% levels(set2$group)))
set1 <- set1[set1$group %in% levels(set2$group), ]
set1 <- set1[, 1:6]

d1 <- merge(d1, set1, by = c('fc_start', 'metric', 'country', 'season', 'run', 'subtype'))

set1 <- unique(d1[, 1:6])
set2 <- unique(d2[, c(1:2, 5:7, 11)])
print(dim(set1) == dim(set2))
rm(set1, set2)

# Reorder columns and save:
d1 <- d1[, c(4, 3, 1, 5, 9:11, 2, 7:8, 6)]
d2 <- d2[, c(6, 5, 1, 7:10, 2:4, 11)]

d1$model <- 'Network'; d2$model <- 'Isolated'
# d <- rbind(d1, d2); rm(d1, d2)

write.csv(d1, file = 'results/network/PROC_outputDist_pt_ot.csv', row.names = FALSE)
write.csv(d2, file = 'results/isolated/outputDist_pt_ot.csv', row.names = FALSE)
rm(d1, d2)

##################################################################################################################################

# Now do all the same for Ens:

# Read in and join network:
e1.h1 <- read.csv('results/by_subtype/network_A(H1)/extra/outputEns_A(H1).csv')
e1.h3 <- read.csv('results/by_subtype/network_A(H3)/extra/outputEns_A(H3).csv')
e1.b <- read.csv('results/by_subtype/network_B/extra/outputEns_B.csv')

e1.h1$subtype <- 'A(H1)'; e1.h3$subtype <- 'A(H3)'; e1.b$subtype <- 'B'

e1 <- rbind(e1.h1, e1.h3, e1.b)
rm(e1.h1, e1.h3, e1.b)
e1 <- e1[e1$metric == 'pi', ]
e1$metric <- factor(e1$metric)

# Read in and join isolated:
e2.h1 <- read.csv('results/by_subtype/isolated_A(H1)/extra/outputEns_A(H1)_ISOLATED.csv')
e2.h3 <- read.csv('results/by_subtype/isolated_A(H3)/extra/outputEns_A(H3)_ISOLATED.csv')
e2.b <- read.csv('results/by_subtype/isolated_B/extra/outputEns_B_ISOLATED.csv')

e2.h1$subtype <- 'A(H1)'; e2.h3$subtype <- 'A(H3)'; e2.b$subtype <- 'B'

e2 <- rbind(e2.h1, e2.h3, e2.b)
rm(e2.h1, e2.h3, e2.b)
e2 <- e2[e2$metric == 'pi', ]
e2$metric <- factor(e2$metric)

# Remove network forecasts where there are no data and therefore no isolated forecasts:
set1 <- unique(e1[, c(1:2, 304:305, 309)])
set2 <- unique(e2[, c(1, 303:305, 309)])

set1$group <- paste(set1$fc_start, set1$country, set1$season, set1$run, set1$subtype, sep = '_'); set1$group <- factor(set1$group)
set2$group <- paste(set2$fc_start, set2$country, set2$season, set2$run, set2$subtype, sep = '_'); set2$group <- factor(set2$group)

print(summary(levels(set2$group) %in% levels(set1$group))) # all true - good!
print(summary(levels(set1$group) %in% levels(set2$group)))
set1 <- set1[set1$group %in% levels(set2$group), ]
set1 <- set1[, 1:5]

e1 <- merge(e1, set1, by = c('fc_start', 'country', 'season', 'run', 'subtype'))

set1 <- unique(e1[, 1:5])
set2 <- unique(e2[, c(1, 303:305, 309)])
print(dim(set1) == dim(set2))
rm(set1, set2)

# Reorder columns and save:
e1 <- e1[, c(1:4, 307:309, 6:306, 5)]
e2 <- e2[, c(1, 303:308, 2:302, 309)]

e1$model <- 'Network'; e2$model <- 'Isolated'
# d <- rbind(e1, e2); rm(e1, e2)

write.csv(e1, file = 'results/network/PROC_outputEns_pi.csv', row.names = FALSE)
write.csv(e2, file = 'results/isolated/outputEns_pi.csv', row.names = FALSE)
rm(e1, e2)






