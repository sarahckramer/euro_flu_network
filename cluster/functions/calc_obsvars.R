calc_obsvars <- function(obs, oev_base, oev_denom) {#, oev_denom_tmp, tmp_exp) {
  
  tmp <- matrix(0, nrow = nrow(obs), ncol = ncol(obs))
  for (i in 1:dim(obs)[2]) {
    for (j in 3:length(obs[, i])) {
      tmp[j, i] <- mean(obs[(j - 2):(j), i], na.rm = TRUE)
    }
    tmp[2, i] <- mean(obs[1:2, i], na.rm = TRUE)
    
    if (!is.na(obs[1, i])) {
      tmp[1, i] <- obs[1, i]
    } else {
      tmp[1, i] <- NA
    }
  }
  
  vars.temp <- tmp
  for (i in 1:dim(obs)[2]) {
    # vars.temp[, i] <- (pop.size$pop[i] + (tmp[, i] ** tmp_exp) / 5) / oev_denom
    # vars.temp[, i] <- (oev_base + (tmp[, i] ** 2) / 5) / oev_denom
    vars.temp[, i] <- oev_base + (tmp[, i] ** 2) / oev_denom
  }
  
  # vars.temp <- (oev_base + (tmp ** tmp_exp) / 5) / oev_denom
  
  # # check for NAs
  # for (i in 1:dim(obs)[2]) {
  #   check.flag <- FALSE
  #   if (any(!is.na(obs[, i]))) {
  #     if (length(vars.temp[, i][is.na(vars.temp[, i])]) > 0) {
  #       check.flag <- TRUE
  #     }
  #     
  #     if (check.flag) {
  #       for (j in which(is.na(vars.temp[, i]))) {
  #         obs_before <- obs[j - 4, i]; k <- j
  #         while (is.na(obs_before)) {
  #           k <- k - 1
  #           obs_before <- obs[k, i]
  #         }
  #         
  #         obs_after <- obs[j, i]; l <- j
  #         while (is.na(obs_after)) {
  #           l <- l + 1
  #           obs_after <- obs[l, i]
  #         }
  #         
  #         art.mean <- mean(c(obs_before, obs_after))
  #         vars.temp[j, i] <- (oev_base + (art.mean ** tmp_exp) / oev_denom_tmp) / oev_denom
  #       }
  #     }
  #   } else {
  #     vars.temp[, i] <- NA
  #   }
  # }
  
  return(vars.temp)
}

calc_obsvars_nTest <- function(obs, syn_dat, ntests, posprops, oev_base, oev_denom, tmp_exp) {
  
  # Inputs need to be matrices:
  if (!is.matrix(obs)) {
    obs <- as.matrix(obs, ncol = 1)
  }
  if (!is.matrix(syn_dat)) {
    syn_dat <- as.matrix(syn_dat, ncol = 1)
  }
  if (!is.matrix(ntests)) {
    ntests <- as.matrix(ntests, ncol = 1)
  }
  if (!is.matrix(posprops)) {
    posprops <- as.matrix(posprops, ncol = 1)
  }
  
  # Look at average of current and previous 2 proportion positive values:
  tmp <- matrix(0, nrow = nrow(posprops), ncol = ncol(posprops))
  for (i in 1:dim(posprops)[2]) {
    for (j in 3:length(posprops[, i])) {
      tmp[j, i] <- mean(posprops[(j - 2):(j), i], na.rm = TRUE)
    }
    
    tmp[2, i] <- mean(posprops[1:2, i], na.rm = TRUE)
    if (!is.na(posprops[1, i])) {
      tmp[1, i] <- posprops[1, i]
    } else {
      tmp[1, i] <- NA # so that doesn't get overwritten w/ baseline
    }
  }
  
  # Also do this for syn_dat and ntests:
  tmp.syn <- matrix(0, nrow = nrow(syn_dat), ncol = ncol(syn.dat))
  for (i in 1:dim(syn_dat)[2]) {
    for (j in 3:length(syn_dat[, i])) {
      tmp.syn[j, i] <- mean(syn_dat[(j - 2):j, i], na.rm = TRUE)
    }
    tmp.syn[2, i] <- mean(syn_dat[1:2, i], na.rm = TRUE)
    if (!is.na(syn_dat[1, i])) {
      tmp.syn[1, i] <- syn_dat[1, i]
    } else {
      tmp.syn[1, i] <- NA # syn_dat[1, i]
    }
  }
  
  tmp.test <- matrix(0, nrow = nrow(ntests), ncol = ncol(syn.dat))
  for (i in 1:dim(ntests)[2]) {
    for (j in 3:length(ntests[, i])) {
      tmp.test[j, i] <- mean(ntests[(j - 2):j, i], na.rm = TRUE)
    }
    tmp.test[2, i] <- mean(ntests[1:2, i], na.rm = TRUE)
    if (!is.na(ntests[1, i])) {
      tmp.test[1, i] <- ntests[1, i]
    } else {
      tmp.test[1, i] <- NA
    }
  }
  
  # NAs here are either NAs in data, or else leading/lagging 0s (which were originally NAs)
  # For the leading/lagging 0s, we could do something about these NAs; however, is this honest? Is it even important, given that these are occuring outside the main outbreak?
  # Originally using average of boundary values for any missing values; but now they are only missing if data are also missing, so no point in "fixing" them, right?
  
  # Incorporate syndromic counts:
  vars.temp <- matrix(0, nrow = nrow(posprops), ncol = ncol(posprops))
  for (i in 1:dim(posprops)[2]) {
    vars.temp[, i] <- (tmp.syn[, i] ** 2 / tmp.test[, i]) * (oev_base + ((tmp[, i] ** tmp_exp) / oev_denom))
    # vars.temp[, i] <- ((syn_dat[, i] / ntests[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp))) / oev_denom
    # vars.temp[, i] <- ((syn_dat[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp)))
  }
  
  # na.now <- which(is.na(vars.temp), arr.ind = T)
  # na.rows.to.check <- which(obs[na.now] > 0 & !is.na(obs[na.now]))
  # na.now[na.rows.to.check, ] # these are data during the first 2 weeks, where we don't have a previous two weeks to average over
  # just set error in first 2 weeks to oev_base
  # vars.temp[1:2, ] <- oev_base
  
  # Any NAs gained here are going to be the leading/lagging NAs that we didn't change to 0 in posprops b/c can't divide by 0
  # So basically, anything that's NA here is also NA in the data themselves
  
  # if last string of NAs can leave, but otherwise set to 1?
  
  #print(max(tmp, na.rm = TRUE))
  #print(oev_base + ((tmp ** tmp_exp) / oev_denom))
  
  return(vars.temp)
}

