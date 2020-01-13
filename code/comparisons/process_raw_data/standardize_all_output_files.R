
# Reorder metrics and output:
m <- m[, c(1, 33:37, 3, 2, 4:32, 38:56)]
o <- o[, c(1, 13:16, 3:6, 2, 7:12)]

# Save new results files:
write.csv(m, file = 'outputMet_pro_PROC.csv', row.names = FALSE)
write.csv(o, file = 'outputOP_PROC.csv', row.names = FALSE)
for (i in 1:length(d)) {
  d.temp <- d[[i]]
  write.csv(d.temp, file = paste0('PROC_', logScore.files[i]), row.names = FALSE)
}






