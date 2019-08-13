
# First get lists of possible oev/lambda values, to determine which were not successfully run:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
oevBase_list <- c(1e4, 1e5)
oevDenom_list <- c(1.0, 2.0, 5.0, 10.0, 20.0, 50.0)
lambdaList <- c(1.00, 1.01, 1.02, 1.03, 1.05)

# Identify missing files:
missing <- c()
for (i in 1:180) {
  season <- seasons[ceiling(i / 60)]
  oev_base <- oevBase_list[ceiling((i - 30) / 30) %% 2 + 1]
  oev_denom <- oevDenom_list[ceiling((i - 5) / 5) %% 6 + 1]
  lambda <- lambdaList[ceiling(i - 1) %% 5 + 1]
  
  if (!file.exists(paste0('code/gridSearch/outputs/fcasts_08092019/outputMet_', season, '_', oev_base, '_', oev_denom, '_', lambda, '_080519.csv'))) {
    missing <- c(missing, i)
  }
  
}
# should be 98

# 2   4   5   6   7   9  10  15  17  18  19  20  24  26  29  30  32  33  34  35  37  38  40  41  42  44  51  53  54  55  57  58  59  61  63  65  66  67  68  70  72  73  78
# 79  80  91  92  93  94  96  99 101 102 104 108 110 111 113 115 116 118 120 121 122 123 124 125 127 128 129 130 131 132 133 134 135 137 138 139 140 141 143 144 145 147 149
# 153 160 161 167 168 169 172 174 176 177 179 180

# 2, 4-7, 9-10, 15, 17-20, 24, 26, 29-30, 32-35, 37-38, 40-42, 44, 51, 53-55, 57-59, 61, 63, 65-68, 70, 72-73, 78-80, 91-94, 96, 99, 101-102, 104, 108, 110-111,
# 113, 115-116, 118, 120-125, 127-135, 137-141, 143-145, 147, 149
rm(lambda, lambdaList, oev_base, oevBase_list, oev_denom, oevDenom_list, season, seasons)

# Loop through all files and compile:
file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'code/gridSearch/outputs/outputMet_080519.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'OP_')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'code/gridSearch/outputs/outputOP_080519.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'OPParams')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'code/gridSearch/outputs/outputOPParams_080519.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Dist')
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
dist <- NULL
for (i in 1:length(file.list)) {
  dist <- rbind(dist, dist.list[[i]])
}
rm(dist.list)
write.csv(dist, file = 'code/gridSearch/outputs/outputDist_080519.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Ens_')
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
e <- NULL
for (i in 1:length(file.list)) {
  e <- rbind(e, ens.list[[i]])
}
rm(ens.list)
write.csv(e, file = 'code/gridSearch/outputs/outputEns_080519.csv', row.names = FALSE)

# Check if any patterns in missingness:
table(m$oev_base, m$oev_denom, m$lambda) # not that I can tell

### And again, now that each run split in 3:

# First make sure no value combos are missing 1-2 of the 3 files:
files.one <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = '080519.csv')
files.two <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = '080519_2')
files.three <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = '080519_3')
# all have exactly 430 files; want 490
rm(files.one, files.two, files.three)

# Check for missingness:
seasons <- c('2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18')
oevBase_list <- c(1e4, 1e5)
oevDenom_list <- c(1.0, 2.0, 5.0, 10.0, 20.0, 50.0)
lambdaList <- c(1.00, 1.01, 1.02, 1.03, 1.05)

missing <- c()
for (i in c(2, 4:7, 9:10, 15, 17:20, 24, 26, 29:30, 32:35, 37:38, 40:42, 44, 51, 53:55, 57:59, 61, 63, 65:68, 70, 72:73, 78:80, 91:94, 96, 99, 101:102, 104, 108,
            110:111, 113, 115:116, 118, 120:125, 127:135, 137:141, 143:145, 147, 149, 153, 160:161, 167:169, 172, 174, 176:177, 179:180)) {
  season <- seasons[ceiling(i / 60)]
  oev_base <- oevBase_list[ceiling((i - 30) / 30) %% 2 + 1]
  oev_denom <- oevDenom_list[ceiling((i - 5) / 5) %% 6 + 1]
  lambda <- lambdaList[ceiling(i - 1) %% 5 + 1]
  
  if (!file.exists(paste0('code/gridSearch/outputs/fcasts_08092019/outputMet_', season, '_', oev_base, '_', oev_denom, '_', lambda, '_080519.csv'))) {
    missing <- c(missing, i)
  }
  
}
# should be 12?
# 153 160 161 167 168 169 172 174 176 177 179 180
# 153, 160-161, 167-169, 172, 174, 176-177, 179-180

rm(lambda, lambdaList, oev_base, oevBase_list, oev_denom, oevDenom_list, season, seasons)

# Read in and compile all:
file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Met')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(met.list)
write.csv(m, file = 'code/gridSearch/outputs/outputMet_080519_2.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'OP_')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(op.list)
write.csv(o, file = 'code/gridSearch/outputs/outputOP_080519_2.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'OPParams')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
op <- NULL
for (i in 1:length(file.list)) {
  op <- rbind(op, op.list[[i]])
}
rm(op.list)
write.csv(op, file = 'code/gridSearch/outputs/outputOPParams_080519_2.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Dist')
dist.list <- list()
for (i in 1:length(file.list)) {
  dist.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
dist <- NULL
for (i in 1:length(file.list)) {
  dist <- rbind(dist, dist.list[[i]])
}
rm(dist.list)
write.csv(dist, file = 'code/gridSearch/outputs/outputDist_080519_2.csv', row.names = FALSE)

file.list <- list.files('code/gridSearch/outputs/fcasts_08092019/', pattern = 'Ens_')
ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/fcasts_08092019/', file.list[[i]]))
}
e <- NULL
for (i in 1:length(file.list)) {
  e <- rbind(e, ens.list[[i]])
}
rm(ens.list)
write.csv(e, file = 'code/gridSearch/outputs/outputEns_080519_2.csv', row.names = FALSE)

# Combine all compiled results files:
m <- read.csv('code/gridSearch/outputs/outputMet_080519.csv')
# m2 <- read.csv('code/gridSearch/outputs/outputMet_080519_2.csv')
m <- rbind(m, m2)
table(m$oev_base, m$oev_denom, m$lambda)
write.csv(m, file = 'code/gridSearch/outputs/outputMet_081219.csv', row.names = FALSE)

o <- read.csv('code/gridSearch/outputs/outputOP_080519.csv')
# o2 <- read.csv('code/gridSearch/outputs/outputOP_080519_2.csv')
o <- rbind(o, o2)
write.csv(o, file = 'code/gridSearch/outputs/outputOP_081219.csv', row.names = FALSE)

o <- read.csv('code/gridSearch/outputs/outputOPParams_080519.csv')
# o2 <- read.csv('code/gridSearch/outputs/outputOPParams_080519_2.csv')
o <- rbind(o, o2)
write.csv(o, file = 'code/gridSearch/outputs/outputOPParams_081219.csv', row.names = FALSE)

d <- read.csv('code/gridSearch/outputs/outputDist_080519.csv')
# d2 <- read.csv('code/gridSearch/outputs/outputDist_080519_2.csv')
d <- rbind(d, d2)
write.csv(d, file = 'code/gridSearch/outputs/outputDist_081219.csv', row.names = FALSE)

e <- read.csv('code/gridSearch/outputs/outputEns_080519.csv')
# e2 <- read.csv('code/gridSearch/outputs/outputEns_080519_2.csv')
e <- rbind(e, e2)
write.csv(e, file = 'code/gridSearch/outputs/outputEns_081219.csv', row.names = FALSE)

# Maybe split up dist and ens files since they're so large?
e.pi <- e[e$metric == 'pi', ]
e.ar <- e[e$metric == 'ar', ]
e1 <- e[e$metric == '1week', ]
e2 <- e[e$metric == '2week', ]
e3 <- e[e$metric == '3week', ]
e4 <- e[e$metric == '4week', ]

d.pt <- d[d$metric == 'pw', ]
d.ot <- d[d$metric == 'onset5', ]

write.csv(e.pi, file = 'code/gridSearch/outputs/outputEns_081219_PI.csv', row.names = FALSE)
write.csv(e1, file = 'code/gridSearch/outputs/outputEns_081219_1wk.csv', row.names = FALSE)
write.csv(e2, file = 'code/gridSearch/outputs/outputEns_081219_2wk.csv', row.names = FALSE)
write.csv(e3, file = 'code/gridSearch/outputs/outputEns_081219_3wk.csv', row.names = FALSE)
write.csv(e4, file = 'code/gridSearch/outputs/outputEns_081219_4wk.csv', row.names = FALSE)

write.csv(d.pt, file = 'code/gridSearch/outputs/outputDist_081219_PT.csv', row.names = FALSE)
write.csv(d.ot, file = 'code/gridSearch/outputs/outputDist_081219_OT.csv', row.names = FALSE)

# # Also need to make sure no repeats in OP and OPParams caused by fitting over same area twice:
# o <- read.csv('code/gridSearch/outputs/outputOP_081219.csv')
# op <- read.csv('code/gridSearch/outputs/outputOPParams_081219.csv')
# 
# o <- o[o$result == 'train', ]
# 
# dim(unique(o[, c(1:5, 8:10)]))
# dim(unique(op[, c(1:5, 8)]))
# # or just keep them all, as a record that slightly different training may have been done



























