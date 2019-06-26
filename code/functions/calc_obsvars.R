calc_obsvars <- function(obs, oev_base, oev_denom, oev_denom_tmp, tmp_exp) {
  
  tmp <- matrix(0, nrow = nrow(obs), ncol = ncol(obs))
  for (i in 1:dim(obs)[2]) {
    for (j in 4:length(obs[, i])) {
      tmp[j, i] <- mean(obs[(j - 3):(j - 1), i], na.rm = TRUE)
    }
  }
  
  vars.temp <- tmp
  for (i in 1:dim(obs)[2]) {
    # vars.temp[, i] <- (pop.size$pop[i] + (tmp[, i] ** tmp_exp) / 5) / oev_denom
    vars.temp[, i] <- (oev_base + (tmp[, i] ** tmp_exp) / oev_denom_tmp) / oev_denom
  }
  
  # vars.temp <- (oev_base + (tmp ** tmp_exp) / 5) / oev_denom
  
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
          vars.temp[j, i] <- (oev_base + (art.mean ** tmp_exp) / oev_denom_tmp) / oev_denom
        }
      }
    } else {
      vars.temp[, i] <- NA
    }
  }
  
  return(vars.temp)
}

calc_obsvars_nTest <- function(obs, syn_dat, ntests, posprops, oev_base, oev_fact, oev_denom, tmp_exp) {
  
  # Look at average of previous 3 proportion positive values:
  tmp <- matrix(0, nrow = nrow(posprops), ncol = ncol(posprops))
  for (i in 1:dim(posprops)[2]) {
    for (j in 4:length(posprops[, i])) {
      tmp[j, i] <- mean(posprops[(j - 3):(j - 1), i], na.rm = TRUE)
    }
  }
  
  # FIRST: check for NAs here and deal with:
  for (i in 1:dim(tmp)[2]) {
    check.flag <- FALSE
    if (any(!is.na(obs[, i]))) {
      if (length(tmp[, i][is.na(tmp[, i])]) > 0) {
        check.flag <- TRUE
      }
      
      if (check.flag) {
        for (j in which(is.na(tmp[, i]))) {
          obs_before <- posprops[j - 4, i]; k <- j
          while (is.na(obs_before)) {
            k <- k - 1
            obs_before <- posprops[k, i]
          }
          
          obs_after <- posprops[j, i]; l <- j
          while (is.na(obs_after)) {
            l <- l + 1
            obs_after <- posprops[l, i]
          }
          
          art.mean <- mean(c(obs_before, obs_after))
          tmp[j, i] <- art.mean
        }
      }
    } else {
      tmp[, i] <- NA
    }
  }
  # So now the only NAs are for countries w/ no data at all for the season
  
  # Incorporate syndromic counts:
  vars.temp <- tmp
  for (i in 1:dim(posprops)[2]) {
    # vars.temp[, i] <- ((syn_dat[, i] / ntests[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp))) / oev_denom
    vars.temp[, i] <- ((syn_dat[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp)))
  }
  
  # Again, correct for NAs (using surrounding syn_dat counts):
  for (i in 1:dim(vars.temp)[2]) {
    check.flag <- FALSE
    if (any(!is.na(obs[, i]))) {
      if (length(vars.temp[, i][is.na(vars.temp[, i])]) > 0) {
        check.flag <- TRUE
      }
      
      if (check.flag) {
        for (j in which(is.na(vars.temp[, i]))) {
          obs_before <- syn_dat[j - 1, i]; k <- j
          while (is.na(obs_before)) {
            k <- k - 1
            obs_before <- syn_dat[k, i]
          }
          
          obs_after <- syn_dat[j + 1, i]; l <- j + 1
          while (is.na(obs_after)) {
            l <- l + 1
            obs_after <- syn_dat[l, i]
          }
          
          art.mean <- mean(c(obs_before, obs_after))
          vars.temp[j, i] <- (art.mean * (oev_base + oev_fact * (tmp[j, i] ** tmp_exp)))
        }
      }
    }
  }
  # again, restricted to just those with no data for this season
  
  # Finally, incorporate # of tests:
  for (i in 1:dim(posprops)[2]) {
    vars.temp[, i] <- ((1 / ntests[, i]) * vars.temp[, i]) / oev_denom
  }
  
  # Again, correct for NAs:
  # This time, use surrounding syn_dat/ntest data? Or just average/extrapolate outside vars.temp vals?
  # Although, if there really are no tests, we don't want to use this points, right?
  missing.now <- which(is.na(vars.temp), arr.ind = T)
  unique(missing.now[, 2])
  # if last string of NAs can leave, but otherwise set to 1?
  
  return(vars.temp)
}

