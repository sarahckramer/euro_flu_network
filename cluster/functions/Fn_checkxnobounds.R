## Function the check aphysicality of the variables/parameters
##  More information in 'Comparison of filtering methods for the modeling and retrospective forecasting of influenza epidemics' (PLoS Compute Biol)
##  by Wan Yang, Alicia Karspeck, and Jeffrey Shaman, 2014

Fn_checkxnobounds<-function(xnew, S.rows, I.rows, param.rows){
  n.ens=dim(xnew)[2]; # get the number of particles
  n.var=dim(xnew)[1]; # get the number of variables
  
  for (ii in S.rows) { # corrects if S > N
    ug <- max(xnew[ii, ])
    if (ug > N[ceiling(ii / n), (ii - 1) %% n + 1]) {
      for (jj in 1:n.ens) {
        if (xnew[ii, jj] > N[ceiling(ii / n), (ii - 1) %% n + 1]) {
          xnew[ii, jj] <- N[ceiling(ii / n), (ii - 1) %% n + 1] - 1
        }
      }
    }
  }
  
  for (ii in I.rows) { # corrects if I > N
    ug <- max(xnew[ii, ])
    if (ug > N[ceiling(ii / n) - n, (ii - 1) %% n + 1]) {
      for (jj in 1:n.ens) {
        if (xnew[ii, jj] > N[ceiling(ii / n) - n, (ii - 1) %% n + 1]) {
          xnew[ii, jj] <- N[ceiling(ii / n) - n, (ii - 1) %% n + 1]
        }
      }
    }
  }
  
  ug <- min(xnew) # Corrects if any state or parameter nudges negative # use this just for R0mx/R0diff!
  if (ug < 0) {
    for (jj in 1:n.ens) {
      for (ii in 1:n.var) {
        if (xnew[ii, jj] <= 0) {
          xnew[ii, jj] <- max(mean(xnew[ii, ], na.rm = TRUE), 1)
        }
      }
    }
  }
  
  # R0diff being set to 0 when negative is totally fine, I think
  # but R0mx shouldn't be < 1, according to above code
  # so let's fix that:
  
  ug <- min(xnew[param.rows[3], ]) # Corrects if R0mx < 1.0
  if (ug < 1.0) {
    for (jj in 1:n.ens) {
      if (xnew[param.rows[3], jj] < 1.0) {
        xnew[param.rows[3], jj] = max(median(xnew[param.rows[3], ]), 1.0)
      }
    }
  }
  
  ug <- min(xnew[param.rows[4], ]) # Corrects if R0diff < 0.01
  if (ug < 0.01) {
    for (jj in 1:n.ens) {
      if (xnew[param.rows[4], jj] < 0.01) {
        xnew[param.rows[4], jj] = max(median(xnew[param.rows[4], ]), 0.01)
      }
    }
  }
  
  ug <- min(xnew[param.rows[1], ]) # Corrects if L < 200 days
  if (ug < 200) {
    for (jj in 1:n.ens) {
      if (xnew[param.rows[1], jj] < 200) {
        xnew[param.rows[1], jj] <- max(median(xnew[param.rows[1], ]), 200)
      }
    }
  }
  
  ug <- min(xnew[param.rows[2], ]) # Corrects if D < 0.5 days
  if (ug <= 1) {
    for (jj in 1:n.ens) {
      if (xnew[param.rows[2], jj] < 0.5) {
        xnew[param.rows[2], jj] <- max(median(xnew[param.rows[2], ]), 0.5)
      }
    }
  }
  
  ug <- min(xnew[param.rows[3], ] - xnew[param.rows[4], ]) # Corrects if R0mx < R0diff; OLD: R0mx <= R0mn
  if (ug <= 0) {
    for (jj in 1:n.ens) {
      if (xnew[param.rows[3], jj] < xnew[param.rows[4], jj]) {
        xnew[param.rows[3], jj] <- xnew[param.rows[4], jj]# + 0.01
      }
    }
  }
  
  xnew
}

Fn_checkDA<-function(xnew,bound.low,bound.up){
  b.low=bound.low;
  b.up=bound.up;
  n.var=nrow(xnew); n.ens=ncol(xnew);
  for(vi in 1:n.var){
    #  Corrects if <b.low
    ug=min(xnew[vi,]);
    if (ug<b.low[vi]){  
      for (jj in 1:n.ens){
        if (xnew[vi,jj]<b.low[vi]){
          xnew[vi,jj]=b.low[vi];
        }
      }
    }
    ug=max(xnew[vi,]);
    if (ug>b.up[vi]){  
      for (jj in 1:n.ens){
        if (xnew[vi,jj]>b.up[vi]){
          xnew[vi,jj]=b.up[vi];
        }
      }
    }
  }
  xnew;
}