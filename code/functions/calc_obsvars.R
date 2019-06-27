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
  
  # Look at average of current and previous 2 proportion positive values:
  tmp <- matrix(0, nrow = nrow(posprops), ncol = ncol(posprops))
  for (i in 1:dim(posprops)[2]) {
    for (j in 3:length(posprops[, i])) {
      tmp[j, i] <- mean(posprops[(j - 2):(j), i], na.rm = TRUE)
    }
  }
  
  # NAs here are either NAs in data, or else leading/lagging 0s (which were originally NAs)
  # For the leading/lagging 0s, we could do something about these NAs; however, is this honest? Is it even important, given that these are occuring outside the main outbreak?
  # Originally using average of boundary values for any missing values; but now they are only missing if data are also missing, so no point in "fixing" them, right?
  
  # Incorporate syndromic counts:
  vars.temp <- tmp
  for (i in 1:dim(posprops)[2]) {
    vars.temp[, i] <- ((syn_dat[, i] / ntests[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp))) / oev_denom
    # vars.temp[, i] <- ((syn_dat[, i]) * (oev_base + oev_fact * (tmp[, i] ** tmp_exp)))
  }
  
  # Here NAs can be gained since using only syn_dat at time i, and not over last 3 weeks - change? But still only NAs where data also NA anyway!
  
  # # Finally, incorporate # of tests:
  # for (i in 1:dim(posprops)[2]) {
  #   vars.temp[, i] <- ((1 / ntests[, i]) * vars.temp[, i]) / oev_denom
  # }
  
  # Any NAs gained here are going to be the leading/lagging NAs that we didn't change to 0 here b/c can't divide by 0
  # So basically, anything that's NA here is also NA in the data themselves
  
  # # Again, correct for NAs:
  # # This time, use surrounding syn_dat/ntest data? Or just average/extrapolate outside vars.temp vals?
  # # Although, if there really are no tests, we don't want to use this points, right?
  # missing.now <- which(is.na(vars.temp), arr.ind = T)
  # unique(missing.now[, 2])
  # # if last string of NAs can leave, but otherwise set to 1?
  # # I imagine these new NAs are either where syn+ also NA, or else leading/lagging outbreaks where no tests
  #       # Can try to mitigate using surrounding 3 weeks, but late in season it seems fairly common to just stop testing
  #       # Most are leading/lagging, and we can't do anything about these; but mid-season ones could do as above and take average of surrounding values
  
  return(vars.temp)
}

