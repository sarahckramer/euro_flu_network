
# Initiate data frames:
m = o = oState = alps = NULL

# Loop through files:
for (i in 1:350) {
  test.file <- paste0('syntheticTests/outputs/cluster/072319/lowI0/outputMet', i, '_loop_S0range_I0narrow.csv')
  if (file.exists(test.file)) {
    
    # print(i)
    m.temp <- read.csv(test.file)
    o.temp <- read.csv(paste0('syntheticTests/outputs/cluster/072319/lowI0/outputOP', i, '_loop_S0range_I0narrow.csv'))
    load(paste0('syntheticTests/outputs/cluster/072319/lowI0/outputS', i, '_loop_S0range_I0narrow.RData'))
    load(paste0('syntheticTests/outputs/cluster/072319/lowI0/outputI', i, '_loop_S0range_I0narrow.RData'))
    load(paste0('syntheticTests/outputs/cluster/072319/lowI0/outputAlps', i, '_loop_S0range_I0narrow.RData'))

    m <- rbind(m, m.temp); o <- rbind(o, o.temp)
    
    for (j in 1:3) { # num_runs
      oState <- rbind(oState, cbind(unique(m.temp$outbreak), j, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                    unique(m.temp$lambda), rep(1:43, 20), melt(outputsS[[1]][[j]]), melt(outputsI[[1]][[j]])))
      alps <- rbind(alps, cbind(unique(m.temp$outbreak), j, unique(m.temp$oev_base), unique(m.temp$oev_denom),
                                unique(m.temp$lambda), melt(outputAlps[[j]])))
    }

  }
  
}; rm(m.temp, o.temp, outputsS, outputsI, outputAlps, test.file, i, j)
# 37:92, 94:128, 130:134, 136:157, 159:179, 181:187, 189:207, 209:242, 244:257, 259:299, 302:304, 306:324, 327:341, 343:361, 363:365, 367:368, 370:408, 410:435, 437, 439:443, 445:449, 451:488, 492:513,
# 515:621, 623, 625:672, 676:697, 701:864

# Format oState:
oState <- oState[, c(1:8, 10)]
names(oState) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'week', 'country', 'S', 'newI')

# Format alps:
names(alps) <- c('outbreak', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'week', 'alp')
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
alps$country <- countries[alps$country]

# # Limit to outbreaks/oev_base of interest:
# m <- m[m$outbreak %in% c(1, 6, 9, 13) & m$oev_base != 1e6, ]
# o <- o[o$outbreak %in% c(1, 6, 9, 13) & o$oev_base != 1e6, ]
# oState <- oState[oState$outbreak %in% c(1, 6, 9, 13) & oState$oev_base != 1e6, ]
# alps<- alps[alps$outbreak %in% c(1, 6, 9, 13) & alps$oev_base != 1e6, ]

# Save compiled files:
write.csv(m, file = 'syntheticTests/outputs/cluster/072319/outputMet_loop_reduceS0_I0narrow.csv', row.names = FALSE)
write.csv(o, file = 'syntheticTests/outputs/cluster/072319/outputOP_loop_reduceS0_I0narrow.csv', row.names = FALSE)
write.csv(oState, file = 'syntheticTests/outputs/cluster/072319/outputOPStates_loop_reduceS0_I0narrow.csv', row.names = FALSE)
write.csv(alps, file = 'syntheticTests/outputs/cluster/072319/outputAlps_loop_reduceS0_I0narrow.csv', row.names = FALSE)

# # Delete component files:
# for (i in 1:24) {
#   test.file <- paste0('syntheticTests/outputs/cluster/071419/outputMet', i, '.csv')
#   if (file.exists(test.file)) {
#     
#     file.remove(test.file)
#     file.remove(paste0('syntheticTests/outputs/cluster/071419/outputOP', i, '.csv'))
#     file.remove(paste0('syntheticTests/outputs/cluster/071419/outputS', i, '.RData'))
#     file.remove(paste0('syntheticTests/outputs/cluster/071419/outputI', i, '.RData'))
#     file.remove(paste0('syntheticTests/outputs/cluster/071419/outputAlps', i, '.RData'))
#     
#   }
# }

rm(list=ls())







