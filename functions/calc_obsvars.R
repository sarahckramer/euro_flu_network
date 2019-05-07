calc_obsvars <- function(obs, oev_denom) {
  
  tmp <- matrix(0, nrow = nrow(obs), ncol = ncol(obs))
  for (i in 1:dim(obs)[2]) {
    for (j in 4:length(obs[, i])) {
      tmp[j, i] <- mean(obs[(j - 3):(j - 1), i], na.rm = TRUE)
    }
  }
  vars.temp <- (1e5 + (tmp ** 2) / 5) / oev_denom
  
  # check for NAs
  for (i in 1:dim(obs)[2]) {
    check.flag <- FALSE
    if (any(!is.na(obs[, i]))) {
      if (length(vars.temp[, i][is.na(vars.temp[, i])]) > 0) {
        check.flag <- TRUE
      }
      
      if (check.flag) {
        for (j in which(is.na(vars.temp[, i]))) {
          obs_before <- obs[j - 4, i]; k <- j
          while (is.na(obs_before)) {
            k <- k - 1
            obs_before <- obs[k, i]
          }
          
          obs_after <- obs[j, i]; l <- j
          while (is.na(obs_after)) {
            l <- l + 1
            obs_after <- obs[l, i]
          }
          
          art.mean <- mean(c(obs_before, obs_after))
          vars.temp[j, i] <- (1e5 + (art.mean ** 2) / 5) / oev_denom
        }
      }
    } else {
      vars.temp[, i] <- NA
    }
  }
  
  return(vars.temp)
}
