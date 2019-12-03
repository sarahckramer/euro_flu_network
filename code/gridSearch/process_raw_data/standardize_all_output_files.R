
### METRICS ###
if (model.type == 'Network') {
  # m <- read.csv(list.files(pattern = 'Met_pro'))
  m <- m.store; rm(m.store)
  write.csv(m, file = 'outputMet_pro_FULL.csv', row.names = FALSE)
  
  m <- m[, c(1:52, 85:110)]
  write.csv(m, file = 'outputMet_pro.csv', row.names = FALSE)
  
} else if (model.type == 'Individual') {
  
  
  
}





# m <- read.csv('results/original/outputMet_111819_pro.csv')
# m2 <- read.csv('results/R0min_OEVold/outputMet_111819_INDIV_R0min-OEVold_pro.csv')
# 
# # Limit indiv. countries to countries in network model:
# m2 <- m2[m2$country %in% levels(m$country), ]
# m2$country <- factor(m2$country)
# 
# # And limit oev_denom/lambda:
# m2 <- m2[m2$oev_denom == 10 & m2$lambda == 1.02, ]
# 
# # And make sure the names are the same!
# which(!(names(m2) %in% names(m)))
# which(!(names(m) %in% names(m2)))
# 
# m2 <- m2[, c(2:3, 5:7, 4, 8, 1, 9:77)]
# names(m) == names(m2)
# 
# summary(names(m) == names(m2))
# 
# write.csv(m, file = 'results/PROCESS/outputMet_111819_pro.csv', row.names = FALSE)
# write.csv(m2, file = 'results/PROCESS/outputMet_111219_INDIV_pro.csv', row.names = FALSE)

# m <- read.csv('results/oldOEV_denom10/outputMet_110919_oldOEV_pro.csv')
# m <- m[, c(1:52, 85:109)]
# write.csv(m, file = 'results/oldOEV_denom10/outputMet_110919_oldOEV_pro.csv', row.names = FALSE)
#
# m <- read.csv('results/oldOEV_denom1/outputMet_110919_oldOEV_denom1_pro.csv')
# m <- m[, c(1:52, 85:109)]
# write.csv(m, file = 'results/oldOEV_denom1/outputMet_110919_oldOEV_denom1_pro.csv', row.names = FALSE)

# ### ENS ###
# e.pi <- read.csv('results/original/logScores_pi.csv')
# e.pi2 <- read.csv('results/indiv_new/logScores_pi.csv')
#
# e.pi2$scaling <- NULL
# names(e.pi2)[6] = names(e2)[6] <- 'metric'
# 
# write.csv(e.pi2, file = 'results/indiv_new/logScores_pi.csv', row.names = FALSE)





