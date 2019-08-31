
# Set directory:
setwd('/ifs/scratch/msph/ehs/sck2165/global_forecasting/core/code/individualCountries/outputs/')

# Read in all files:
file.list <- list.files(pattern = 'Ens')
print(file.list)

ens.list <- list()
for (i in 1:length(file.list)) {
  ens.list[[i]] <- read.csv(file.list[[i]])
}
print(length(ens.list)) # want 18
print(table(ens.list[[1]]$metric))

# Compile:
ens <- NULL
for (i in 1:length(file.list)) {
  ens <- rbind(ens, ens.list[[i]])
}
rm(ens.list)

# # Write full results:
# write.csv(ens, file = 'outputEns_082819.csv', row.names = FALSE)

# Separated out into metrics of interest:
e.pi <- ens[ens$metric == 'pi', ]
e.1wk <- ens[ens$metric == '1week', ]
e.2wk <- ens[ens$metric == '2week', ]
e.3wk <- ens[ens$metric == '3week', ]
e.4wk <- ens[ens$metric == '4week', ]

# Save:
write.csv(e.pi, file = 'outputEns_082819_PI.csv', row.names = FALSE)
write.csv(e.1wk, file = 'outputEns_082819_1wk.csv', row.names = FALSE)
write.csv(e.2wk, file = 'outputEns_082819_2wk.csv', row.names = FALSE)
write.csv(e.3wk, file = 'outputEns_082819_3wk.csv', row.names = FALSE)
write.csv(e.4wk, file = 'outputEns_082819_4wk.csv', row.names = FALSE)



