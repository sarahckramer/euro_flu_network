
# Initiate data frames:
m = o = oState = alps = NULL

# Loop through files:
num_out <- 3; num_runs <- 2
for (i in 1:24) {
  test.file <- paste0('syntheticTests/outputs/cluster/071419/outputMet', i, '.csv')
  if (file.exists(test.file)) {
    m.temp <- read.csv(test.file)
    o.temp <- read.csv(paste0('syntheticTests/outputs/cluster/071419/outputOP', i, '.csv'))
    load(paste0('syntheticTests/outputs/cluster/071419/outputS', i, '.RData'))
    load(paste0('syntheticTests/outputs/cluster/071419/outputI', i, '.RData'))
    load(paste0('syntheticTests/outputs/cluster/071419/outputAlps', i, '.RData'))
    
    m <- rbind(m, m.temp); o <- rbind(o, o.temp)
    # print(unique(m.temp$oev_base))
    # print(unique(m.temp$oev_denom))
    # print(unique(m.temp$lambda))
    # print(''); print(''); print('')
    
    for (j in 1:length(outputsS[[1]])) {
      curr.out <- ceiling(j / num_runs)
      oState <- rbind(oState, cbind(curr.out, (j - 1) %% num_runs + 1, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                    unique(m.temp$lambda), rep(1:43, 20), melt(outputsS[[1]][[j]]), melt(outputsI[[1]][[j]])))
      alps <- rbind(alps, cbind(curr.out, (j - 1) %% num_runs + 1, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                unique(m.temp$lambda), melt(outputAlps[[j]])))
    }
    
  }
  
}; rm(m.temp, o.temp, outputsS, outputsI, test.file, i, j)

# Format oState:
oState <- oState[, c(1:8, 10)]
names(oState) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'week', 'country', 'S', 'newI')

# Format alps:
names(alps) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'week', 'alp')
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
alps$country <- countries[alps$country]

# Save compiled files:
write.csv(m, file = 'syntheticTests/outputs/cluster/071419/outputMet.csv', row.names = FALSE)
write.csv(o, file = 'syntheticTests/outputs/cluster/071419/outputOP.csv', row.names = FALSE)
write.csv(oState, file = 'syntheticTests/outputs/cluster/071419/outputOPStates.csv', row.names = FALSE)
write.csv(alps, file = 'syntheticTests/outputs/cluster/071419/outputAlps.csv', row.names = FALSE)

# Delete component files:
for (i in 1:24) {
  test.file <- paste0('syntheticTests/outputs/cluster/071419/outputMet', i, '.csv')
  if (file.exists(test.file)) {
    
    file.remove(test.file)
    file.remove(paste0('syntheticTests/outputs/cluster/071419/outputOP', i, '.csv'))
    file.remove(paste0('syntheticTests/outputs/cluster/071419/outputS', i, '.RData'))
    file.remove(paste0('syntheticTests/outputs/cluster/071419/outputI', i, '.RData'))
    file.remove(paste0('syntheticTests/outputs/cluster/071419/outputAlps', i, '.RData'))
    
  }
}

rm(list=ls())







