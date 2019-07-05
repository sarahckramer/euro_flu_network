
# Initiate data frames:
m = o = oState = alps = NULL

# Loop through files:
num_out <- 5; num_runs <- 2
for (i in 1:24) {
  test.file <- paste0('syntheticTests/outputs/cluster/outputMet', i, '.csv')
  if (file.exists(test.file)) {
    m.temp <- read.csv(test.file)
    o.temp <- read.csv(paste0('syntheticTests/outputs/cluster/outputOP', i, '.csv'))
    load(paste0('syntheticTests/outputs/cluster/outputS', i, '.RData'))
    load(paste0('syntheticTests/outputs/cluster/outputI', i, '.RData'))
    load(paste0('syntheticTests/outputs/cluster/outputAlps', i, '.RData'))
    
    m <- rbind(m, m.temp); o <- rbind(o, o.temp)
    # print(unique(m.temp$oev_base))
    # print(unique(m.temp$oev_denom))
    # print(unique(m.temp$lambda))
    # print(''); print(''); print('')
    
    for (j in 1:length(outputsS[[1]])) {
      oState <- rbind(oState, cbind(ceiling(j / num_runs), (j - 1) %% num_runs + 1, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                    unique(m.temp$lambda), rep(1:43, 21), melt(outputsS[[1]][[j]]), melt(outputsI[[1]][[j]])))
      alps <- rbind(alps, cbind(ceiling(j / num_runs), (j - 1) %% num_runs + 1, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                unique(m.temp$lambda), melt(outputAlps[[j]])))
    }
    
  }
  
}; rm(m.temp, o.temp, outputsS, outputsI, test.file, i, j)

# Format oState:
oState <- oState[, c(1:8, 10)]
names(oState) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'week', 'country', 'S', 'newI')

# Format alps:
names(alps) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'week', 'alp')
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IS', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
alps$country <- countries[alps$country]

# Save compiled files:
write.csv(m, file = 'syntheticTests/outputs/cluster/outputMet.csv', row.names = FALSE)
write.csv(o, file = 'syntheticTests/outputs/cluster/outputOP.csv', row.names = FALSE)
write.csv(oState, file = 'syntheticTests/outputs/cluster/outputOPStates.csv', row.names = FALSE)
write.csv(alps, file = 'syntheticTests/outputs/cluster/outputAlps.csv', row.names = FALSE)

# Delete component files:
for (i in 1:24) {
  test.file <- paste0('syntheticTests/outputs/cluster/outputMet', i, '.csv')
  if (file.exists(test.file)) {
    
    file.remove(test.file)
    file.remove(paste0('syntheticTests/outputs/cluster/outputOP', i, '.csv'))
    file.remove(paste0('syntheticTests/outputs/cluster/outputS', i, '.RData'))
    file.remove(paste0('syntheticTests/outputs/cluster/outputI', i, '.RData'))
    file.remove(paste0('syntheticTests/outputs/cluster/outputAlps', i, '.RData'))
    
  }
  
}

rm(list=ls())







