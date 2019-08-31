
### Function for basic "accuracy" plotting:
format_for_plot <- function(df, xvar, yvar) {
  mytable <- table(df[, xvar], df[, yvar])
  mytable <- prop.table(mytable, 1)[, 1:2]
  mytable <- melt(rowSums(mytable))
  mytable$lead <- as.numeric(rownames(mytable))
  res <- mytable
  
  num.fcasts.temp <- c()
  for (lead in res$lead) {
    num.fcasts.temp <- c(num.fcasts.temp, length(df[, 1][df[, xvar] == lead]))
  }
  res$len <- num.fcasts.temp
  
  return(res)
}



