
# Load commuting data and determine which countries to remove:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData') # note: includes non-adjacent
to.remove <- which(((rowSums(comm.by.year[[1]]) == 0 | colSums(comm.by.year[[1]]) == 0) & rownames(comm.by.year[[1]]) != 'IS') | rownames(comm.by.year[[1]]) == 'RO')
# artificially added RO to to.remove - little data on inbound commuting
to.remove.NAMES <- names(to.remove)

for (i in 1:length(comm.by.year)) {
  comm.by.year[[i]] <- comm.by.year[[i]][-to.remove, -to.remove]
}
# 19 countries left

save(comm.by.year, file = 'formatTravelData/formattedData/comm_mat_by_year_05-07.RData')

# Load train data, reduce, and save:
load('formatTravelData/formattedData/train_05-07.RData')
t.rand <- t.rand[-which(rownames(t.rand) %in% to.remove.NAMES), -which(rownames(t.rand) %in% to.remove.NAMES)]

# Add IS back in:
t.rand <- rbind(t.rand[1:9, ], rep(0, 18), t.rand[10:18, ])
t.rand <- cbind(t.rand[, 1:9], rep(0, 19), t.rand[, 10:18])
rownames(t.rand) = colnames(t.rand) = rownames(comm.by.year[[1]])

save(t.rand, file = 'formatTravelData/formattedData/train_05-07.RData')

# Load air data, reduce, and save:
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  a.temp.sym <- a.temp.sym[-which(rownames(a.temp.sym) %in% to.remove.NAMES), -which(rownames(a.temp.sym) %in% to.remove.NAMES)]
  a.temp.sym <- a.temp.sym[2:20, 2:20] # remove AT
  save(a.temp.sym, file = paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
}







