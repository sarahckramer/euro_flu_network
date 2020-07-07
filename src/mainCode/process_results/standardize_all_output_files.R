### Ensure network and isolated results formatted in the same way ###

# Reorder metrics and output:
if (model.type == 'Network') {
  m <- m[, c(1, 33:37, 3, 2, 4:32, 38:56)]
  o <- o[, c(1, 13:16, 3:6, 2, 7:12)]
} else if (model.type == 'Individual') {
  m <- m.store[, c(32:37, 1, 31, 2:30, 38:56)]; rm(m.store)
  o <- o[, c(20:24, 1:4, 19, 5:18)]
}

# Save new results files:
# write.csv(m, file = 'outputMet_pro_PROC.csv', row.names = FALSE)
# write.csv(o, file = 'outputOP_PROC.csv', row.names = FALSE)

if (model.type == 'Network') {
  write.csv(m, file = paste0('../../by_subtype/network_', strain, '/outputMet_pro_PROC.csv'), row.names = FALSE)
  write.csv(o, file = paste0('../../by_subtype/network_', strain, '/outputOP_PROC.csv'), row.names = FALSE)
  for (i in 1:length(d)) {
    d.temp <- d[[i]]
    write.csv(d.temp, file = paste0('../../by_subtype/network_', strain, '/PROC_', logScore.files[i]), row.names = FALSE)
  }
} else if (model.type == 'Individual') {
  write.csv(m, file = paste0('../../by_subtype/isolated_', strain, '/outputMet_pro_PROC.csv'), row.names = FALSE)
  write.csv(o, file = paste0('../../by_subtype/isolated_', strain, '/outputOP_PROC.csv'), row.names = FALSE)
}







