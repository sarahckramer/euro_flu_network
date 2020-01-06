
### METRICS ###
if (model.type == 'Network') {
  # m <- read.csv(list.files(pattern = 'Met_pro'))
  m <- m.store; rm(m.store)
  write.csv(m, file = 'outputMet_pro_FULL.csv', row.names = FALSE)
  
  m <- m[, c(1:52, 85:110)]
  write.csv(m, file = 'outputMet_pro.csv', row.names = FALSE)
  
} else if (model.type == 'Individual') {
  
  
  
}

# ### ENS ###
# e.pi <- read.csv('results/original/logScores_pi.csv')
# e.pi2 <- read.csv('results/indiv_new/logScores_pi.csv')
#
# e.pi2$scaling <- NULL
# names(e.pi2)[6] = names(e2)[6] <- 'metric'
# 
# write.csv(e.pi2, file = 'results/indiv_new/logScores_pi.csv', row.names = FALSE)





