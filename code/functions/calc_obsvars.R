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

calc_obsvars_nTest <- function(syn_dat, ntests, posprops, oev_base, oev_fact, oev_denom, tmp_exp) {
  
  tmp <- matrix(0, nrow = nrow(posprops), ncol = ncol(posprops))
  for (i in 1:dim(posprops)[2]) {
    for (j in 4:length(posprops[, i])) {
      tmp[j, i] <- mean(posprops[(j - 3):(j - 1), i], na.rm = TRUE)
    }
  }
  vars.temp <- tmp
  for (i in 1:dim(posprops)[2]) {
    vars.temp[, i] <- ((syn_dat[, i] / ntests[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp))) / oev_denom
  }
  
  # check for NAs
  # but usually these are b/c ntests is NA...
  # could just use bounds of vars.temp on either side? That way, if obs. is 0, we can still use it?
  # but maybe that's not fair anyway, since these aren't true 0s - no tests!
  ntests.check <- ntests[which(is.na(vars.temp), arr.ind = T)]
  check.na <- which(is.na(vars.temp), arr.ind = T)[which(!is.na(ntests.check)), ]
  # these are the only NAs that aren't due to either ntests or syn_dat themselves being NA
  
  if (!is.null(dim(check.na)[1])) {
    check.na <- check.na[!is.na(syn_dat[check.na]), ]
    for (rownum in 1:dim(check.na)[1]) {
      rowcol <- check.na[rownum, ]
      j <- rowcol[1]; i <- rowcol[2]
      
      pos_before <- posprops[j - 4, i]; k <- j
      while (is.na(pos_before)) {
        k <- k - 1
        pos_before <- posprops[k, i]
      }
      pos_after <- posprops[j, i]; l <- j
      while (is.na(pos_after)) {
        l <- l + 1
        pos_after <- posprops[l, j]
      }
      
      art.mean <- mean(c(pos_before, pos_after))
      vars.temp[j, i] <- ((syn_dat[j, i] / ntests[j, i]) * (oev_base + oev_fact * (art.mean ** tmp_exp))) / oev_denom
    }
  } else {
    if (!is.null(check.na[1])) {
      j <- check.na[1]; i <- check.na[2]
      
      pos_before <- posprops[j - 4, i]; k <- j
      while (is.na(pos_before)) {
        k <- k - 1
        pos_before <- posprops[k, i]
      }
      pos_after <- posprops[j, i]; l <- j
      while (is.na(pos_after)) {
        l <- l + 1
        pos_after <- posprops[l, j]
      }
      
      art.mean <- mean(c(pos_before, pos_after))
      vars.temp[j, i] <- ((syn_dat[j, i] / ntests[j, i]) * (oev_base + oev_fact * (art.mean ** tmp_exp))) / oev_denom
    }
  }
  
  return(vars.temp)
}

