
### Issues are that some runs seem to dip below 0 for I/Inc, and that die-out between seasons is uncommon
### These issues don't seem contained to certain compartments, but lets check
### The R0mx/R0diff model may encourage early outbreaks and lack of die-out due to the not-super-low R0s in the off-season
    # We can try lower R0mx?
### May also want to look into LHS_wide S0s

####################################################################################################################################################################
####################################################################################################################################################################

# First let's read in our results and get the runs/params of interest:
load('syntheticTests/syntheticData/20yr_runs_cluster/resRates_20yr_last10.RData')
load('syntheticTests/syntheticData/init_parms_10000.RData')

# List of countries:
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

# Are we looking at averages, or at full data streams?
# Let's look at the mean annual cycle for now:
yr.breaks <- list(1:52, 53:104, 105:156, 157:208, 209:260, 261:312, 313:364, 365:416, 417:468, 469:520)
get_10yr_avg <- function(obs) {
  obs.avg <- matrix(0, nrow = nrow(obs), ncol = 52)
  for (yr in 1:10) {
    obs.avg <- obs.avg + obs[, yr.breaks[[yr]]]
  }
  obs.avg <- obs.avg / 10
  return(obs.avg)
}

# Get yearly averages:
res.avg1 <- lapply(run.list[[1]], get_10yr_avg)
res.avg2 <- lapply(run.list[[2]], get_10yr_avg)

run.list1 <- run.list[[1]]
run.list2 <- run.list[[2]]
rm(run.list)

res.list.comb <- vector('list', length(run.list1))
for (i in 1:10000) {
  res.list.comb[[i]] <- run.list1[[i]] + run.list2[[i]]
}
res.avg <- lapply(res.list.comb, get_10yr_avg)

# Limit to runs of interest:
to.explore <- c(9602, 9356, 9194, 8851, 8566, 7819, 7212, 7054, 6732, 5284, 5765, 5516, 4417, 4315, 4166, 3743, 3342, 3328, 2982, 2760, 2666, 2153, 2119, 1611, 961, 5,
                5605, 4682, 2130, 592, 6995, 2, 56, 131, 198, 402, 431, 535, 1179, 1537, 7736, 33, 299)
# look to see if reaching 0; 2 has negatives instead

red.list1 <- run.list1[to.explore]
red.list2 <- run.list2[to.explore]

# parms.red <- parms[, to.explore]

# Plot out our runs of interest, but subtype:
par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (run in to.explore) {
  matplot(t(run.list1[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  matplot(t(run.list2[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  # abline(v = seq(12, 523, by = 52), lwd = 1.5, col = 'red')
  # abline(v = seq(52, 523, by = 52), lwd = 1.5, col = 'red')
}
# for 0s, check: 2, 198
# for no die-out: 431/1179 (every year); 33/299 (every other, but synced)
# for comp (die-out - maybe): 4682, 6995, 2119, 7819, 9602 

####################################################################################################################################################################
####################################################################################################################################################################

# Now read in all functions and code needed to run these models!

### Read in libraries
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); #library(ggplot2); library(gridExtra); library(viridis); library(miceadds)

### Read in model functions
source('cluster/SIRS_network_multistrain.R')
source('cluster/functions/Util.R')
source('cluster/functions/synth_functions.R')

### Global variables:
dt <- 1 # time step for SIRS integration
tmstep <- 7 # data are weekly
wk_start <- 40

### Set parameters
num_ens <- 20 # (100 x 100, or 200 x 50)
tm_strt <- 1; tm_end <- 365 * 20 + 2; tm_step <- 1#; t <- 273 is first of October; adding 2 makes it the whole week - avoids NAs at the end
tm.range <- tm_strt:tm_end # should be length 7300 days, or 7300 / 365 = 20 years

### Parameters for the filters
discrete <- TRUE # run the model stochastically
print(dim(parms.red))

### Specify the countries in the model
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
count.indices <- c(1:2, 4, 6:8, 11:14, 17, 19)

### Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

### Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07_RELIABLE_ONLY.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

### Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))
# note: this now results in more home-home people than before, since there are fewer countries to commute to

### Read in humidity data
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])
for (i in 1:4) {
  AH <- rbind(AH, AH)
}
AH <- AH[1:7316, ] # 20 years + some for beta

### Get subtypes by season
Vtype <- read.csv('data/subtypes_seeding.csv')

####################################################################################################################################################################
####################################################################################################################################################################

# Choose runs:
parms.red <- parms[, c(2, 198, 431, 1179, 33, 299, 4682, 6995, 2119, 7819, 9602)]
# for 0s, check: 2, 198
# for no die-out: 431/1179 (every year); 33/299 (every other, but synced)
# for comp (die-out - maybe): 4682, 6995, 2119, 7819, 9602 
num_ens <- dim(parms.red)[2]
# parameter-wise, lack of die-out seems to be due to low L or low R0diff or high D; higher airScale?

### Run model!
init.states <- allocate_S0I0(parms.red, num_ens, n, N, s0.method = 'dist')

# res <- run_model(parms, init.states[[1]], init.states[[2]], AH, num_ens, n, N, tm.range, tmstep, tm_strt, tm_end, dt, pop.size, r0.mn = FALSE, multi = TRUE) # time: 50 ~ 30minutes
in.parms <- parms.red[(dim(parms.red)[1] - 4):(dim(parms.red)[1]), ]

beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
a <- -180
b <- log(in.parms[4, ])
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + (in.parms[3, ix] - in.parms[4, ix])) / in.parms[2, ix]
})
beta.full <- beta

D.temp <- in.parms[2, ]; L.temp <- in.parms[1, ]; airScale.temp <- in.parms[5, ]
S0.temp <- init.states[[1]]; I0.temp <- init.states[[2]]

m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS_multi(tm_strt = tm_strt, tm_end = tm_end, dt,
                         S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                         D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                         airScale = airScale.temp[ix], realdata = TRUE,
                         prohibAir = FALSE)
})
res.list <- format_model_results_multi(m, num_ens, tmstep, tm_strt, tm_end, n, pop.size)

# or, manually:
ix <- 4
S0 <- init.states[[1]][[ix]]; I0 <- init.states[[2]][[ix]]
D <- D.temp[ix]; L <- L.temp[ix]; airScale <- airScale.temp[ix]
beta <- beta.full[[ix]]

####################################################################################################################################################################
####################################################################################################################################################################

# Get model results:
res.rates1 <- res.list[[1]]; res.rates2 <- res.list[[2]]

# Plot to compare? (since synthetic)
par(mfrow = c(4, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (i in 1:20) {
  run <- to.explore[i]
  
  matplot(t(run.list1[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  matplot(t(run.list2[[run]]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  
  matplot(t(res.rates1[[i]][, 521:1043]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain1_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  matplot(t(res.rates2[[i]][, 521:1043]), type = 'b', pch = 20, cex = 0.6, col = viridis(12), main = paste0('Strain2_', run), xlab = 'Time', ylab = 'Inc.')
  abline(v = seq(0, 523, by = 52), lty = 2)
  
  # abline(v = seq(13, 523, by = 52), lwd = 1.5, col = 'red')
  # abline(v = seq(25, 523, by = 52), lwd = 1.5, col = 'red')
}
# for the ones that alternate seasons, which seasons they alternate on can change stochastically, even when same inputs are used
# The two strains themselves though often look similar - is there a reason for this? travel selected same for both?


# Check < 0
    # Only for strain 2; looks like I was subtracting instead of adding seed to newI
# Check why not die-ing out
# Play with R0mx, S0, dist/LHS
# Check which compartments hold on to cases
# Check that no interaction ever between strains


####################################################################################################################################################################
####################################################################################################################################################################










####################################################################################################################################################################
####################################################################################################################################################################


