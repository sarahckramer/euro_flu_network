
# Basically this, but add section to look at how relation to first outbreak timing changes with parameter values and model type

# Setup:
library("truncnorm"); library("tgp"); library("MASS"); library(reshape2); library(plyr); library(ggplot2); library(viridis)
source('code/SIRS_network.R')
dt <- 1 # time step for SIRS integration
tmstep <- 7 #data is weekly
wk_start <- 40

# Set parameter info:
tm_strt <- 273; tm_end <- 573; tm_step <- 1#; t <- 1 # 273 is first of October
tm.range <- tm_strt:tm_end

# Countries:
countries <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'FR', 'DE', 'HU', 'IE', 'IT',
               'LU', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'UK')
count.indices <- c(1:8, 10:21)

# Get collection of params that produce "full" outbreaks:
load('syntheticTests/syntheticData/params_07-14.RData')
load('syntheticTests/syntheticData/initStates_07-14.RData')

###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

### First: Run with AH-forcing AND travel ###

# Setup:
set.seed(10489436)

# Set population sizes and # of countries used
pop.size <- read.csv('data/popcounts_02-07.csv')
pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
pop.size <- pop.size[match(countries, pop.size$country), ]

# Load commuting data
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
t.comm <- t.comm[countries, countries]

# Set country populations
N <- t.comm; n <- length(countries) # w/ commuting
diag(N) <- unlist(lapply(1:n, function(ix) {
  pop.size$pop[ix] - rowSums(N)[ix]
}))

# Read in humidity data
ah <- read.csv('../GLDAS_data/ah_Europe_07142019.csv')
AH <- rbind(ah[, count.indices], ah[, count.indices])

# Set initial conditions and params:
parms <- rbind(init.states.SEL, t(select.parms))

# Repeat 6 times!:
count <- 1
while (count < 6) {
  parms <- cbind(parms, parms[, 1:24]); count <- count + 1
}

# Change R0mx and airScale where desired:
parms[44, 1:24] <- 2.5
parms[44, 25:48] <- 2.75
parms[44, 49:72] <- 3.0
parms[43, 73:96] <- 0.75
parms[43, 97:120] <- 1.00
parms[43, 121:144] <- 1.25

# Set initial S/I for all compartments:
num_ens <- dim(parms)[2]
S0.temp = I0.temp = vector('list', num_ens)
for (i in 1:num_ens) {
  S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
  
  diag(S0.temp[[i]]) <- parms[1:n, i]
  S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
    rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
  })
  S0.temp[[i]] <- t(S0.temp[[i]])
  S0.temp[[i]] <- S0.temp[[i]] * N
  
  diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
  I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
  I0.temp[[i]] <- I0.temp[[i]] * N
}

# Reduce parms to hold just the parameters now:
parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]

# Calculate betas
beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
AHpt <- AH[beta.range, ]
AHpt <- as.matrix(AHpt, length(AHpt), n)
b <- log(parms[4, ] - parms[5, ])
a <- -180
beta <- lapply(1:num_ens, function(ix) {
  (exp(a * AHpt + b[ix]) + parms[5, ix]) / parms[1, ix]
})

# Create vectors of initial parameters:
D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365; airScale.temp <- parms[3, ]

# Run!
m <- sapply(1:num_ens, function(ix) {
  propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
                   S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
                   D = D.temp[ix], L = L.temp[ix], beta[[ix]],
                   airScale = airScale.temp[ix], realdata = TRUE,
                   prohibAir = FALSE)
})

# Format results:
nt <- floor((length(tm_strt:tm_end) + 1) / 7)
newI <- lapply(1:(n ** 2), function(ix) {
  matrix(unlist(lapply(1:num_ens, function(jx) {
    m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
  })), nrow = num_ens, byrow = TRUE)
}) # each ensemble member has its own row

newI.c <- vector('list', n)
for (i in 1:n) {
  newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
}
newI.c.COUNT <- newI.c

for (i in 1:n) {
  newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
}

newI.ens <- vector('list', num_ens)
for (i in 1:num_ens) {
  newI.ens.temp <- NULL
  
  for (j in 1:n) {
    newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
  }
  
  newI.ens[[i]] <- t(newI.ens.temp)
}; rm(newI.ens.temp)


# Create data frame with results by outbreak, R0mx, airScale:
res.df <- NULL
for (i in 1:length(newI.ens)) {
  res.df <- rbind(res.df, cbind(melt(newI.ens[[i]]), i, as.vector(parms[4, i]), as.vector(parms[3, i])))
}
names(res.df) <- c('time', 'country', 'newI', 'outbreak', 'R0mx', 'airScale')
res.df$country <- countries[res.df$country]; res.df$country <- factor(res.df$country)

for (outbreak in 1:24) {
  res.df$outbreak[res.df$outbreak %in% (c(1, 25, 49, 73, 97, 121) + outbreak - 1)] <- outbreak
}
res.df$outbreak <- factor(res.df$outbreak)

res.df$R0mx[!(res.df$R0mx %in% c(2.5, 2.75, 3.0))] <- NA
res.df$airScale[!(res.df$airScale %in% c(0.75, 1.00, 1.25))] <- NA

# Plot full epidemics by outbreak and parameter values:
pdf('syntheticTests/sensitivity/outputs_plots/plot_full_outbreaks_by_param_vals.pdf', width = 10, height = 35)
p1 <- ggplot(data = res.df[is.na(res.df$airScale), ]) + geom_line(aes(x = time, y = newI, group = country, col = country)) +
  theme_classic() + labs(x = 'Time', y = 'Incidence', col = 'Country') +
  facet_grid(outbreak ~ R0mx) + scale_color_viridis(discrete = TRUE)
p2 <- ggplot(data = res.df[is.na(res.df$R0mx), ]) + geom_line(aes(x = time, y = newI, group = country, col = country)) +
  theme_classic() + labs(x = 'Time', y = 'Incidence', col = 'Country') +
  facet_grid(outbreak ~ airScale) + scale_color_viridis(discrete = TRUE)
print(p1); print(p2)
dev.off()
# Probably not super important b/c there's not much you can see from looking at entire trajectory like this; instead, we'll want certain metrics to explore
# This might also be something to ask for more guidance on - what exactly should I look for?

# For now, look for PT relative to first country to peak, and plot distribution of this value by country and by R0mx/airScale parameter - this gives us an idea of how the following change:
      # 1) Absolute value of timing
      # 2) Synchrony
      # 3) Geographic pattern of transmission (Can also search for specific instances of peak order changing)
res.df.pt.R0mx = res.df.pt.airScale = NULL
for (outbreak in levels(res.df$outbreak)) {
  for (country in levels(res.df$country)) {
    res.df.temp.R0 <- res.df[res.df$outbreak == outbreak & res.df$country == country & is.na(res.df$airScale), ]
    res.df.temp.airScale <- res.df[res.df$outbreak == outbreak & res.df$country == country & is.na(res.df$R0mx), ]
    
    for (r0 in unique(res.df.temp.R0$R0mx)) {
      res.df.temp <- res.df.temp.R0[res.df.temp.R0$R0mx == r0, ]
      res.df.pt.R0mx <- rbind(res.df.pt.R0mx, c(outbreak, country, r0, which.max(res.df.temp$newI)))
    }
    
    for (aS in unique(res.df.temp.airScale$airScale)) {
      res.df.temp <- res.df.temp.airScale[res.df.temp.airScale$airScale == aS, ]
      res.df.pt.airScale <- rbind(res.df.pt.airScale, c(outbreak, country, aS, which.max(res.df.temp$newI)))
    }
    
  }
}
res.df.pt.R0mx <- as.data.frame(res.df.pt.R0mx)
res.df.pt.airScale <- as.data.frame(res.df.pt.airScale)

names(res.df.pt.R0mx) <- c('outbreak', 'country', 'R0mx', 'pt')
names(res.df.pt.airScale) <- c('outbreak', 'country', 'airScale', 'pt')

res.df.pt.R0mx$pt <- as.numeric(as.character(res.df.pt.R0mx$pt))
res.df.pt.airScale$pt <- as.numeric(as.character(res.df.pt.airScale$pt))

pdf('syntheticTests/sensitivity/outputs_plots/plot_PT_by_param.pdf', width = 10, height = 8)
# Quick plot: Boxplot of PT range by country by R0mx/airScale (deals with #1 above):
p1 <- ggplot(data = res.df.pt.R0mx) + geom_boxplot(aes(x = country, y = pt), fill = 'steelblue2') +
  geom_hline(yintercept = mean(res.df.pt.R0mx$pt), lwd = 1.0, lty = 2, col = 'gray80') + 
  theme_classic() + labs(x = '', y = 'Peak Timing') + facet_wrap(~ R0mx, ncol = 1)
p2 <- ggplot(data = res.df.pt.airScale) + geom_boxplot(aes(x = country, y = pt), fill = 'steelblue2') +
  geom_hline(yintercept = mean(res.df.pt.airScale$pt), lwd = 1.0, lty = 2, col = 'gray80') + 
  theme_classic() + labs(x = '', y = 'Peak Timing') + facet_wrap(~ airScale, ncol = 1)
print(p1); print(p2)

# Calculate RELATIVE peak timing:
res.df.pt.R0mx$pt.rank = res.df.pt.airScale$pt.rank = NA
for (i in levels(res.df.pt.R0mx$outbreak)) {
  
  for (r0 in levels(res.df.pt.R0mx$R0mx)) {
    m.temp <- res.df.pt.R0mx[res.df.pt.R0mx$outbreak == i & res.df.pt.R0mx$R0mx == r0, ]
    
    m.temp$pt.rank[which(m.temp$pt == min(m.temp$pt, na.rm = T))] <- 0
    pt.base <- unique(m.temp$pt[m.temp$pt.rank == 0 & !is.na(m.temp$pt.rank)])
    m.temp$pt.rank[is.na(m.temp$pt.rank)] <- m.temp$pt[is.na(m.temp$pt.rank)] - pt.base
    
    res.df.pt.R0mx$pt.rank[res.df.pt.R0mx$outbreak == i & res.df.pt.R0mx$R0mx == r0] <- m.temp$pt.rank
  }
  
  for (aS in levels(res.df.pt.airScale$airScale)) {
    m.temp <- res.df.pt.airScale[res.df.pt.airScale$outbreak == i & res.df.pt.airScale$airScale == aS, ]
    
    m.temp$pt.rank[which(m.temp$pt == min(m.temp$pt, na.rm = T))] <- 0
    pt.base <- unique(m.temp$pt[m.temp$pt.rank == 0 & !is.na(m.temp$pt.rank)])
    m.temp$pt.rank[is.na(m.temp$pt.rank)] <- m.temp$pt[is.na(m.temp$pt.rank)] - pt.base
    
    res.df.pt.airScale$pt.rank[res.df.pt.airScale$outbreak == i & res.df.pt.airScale$airScale == aS] <- m.temp$pt.rank
  }
  
}

# Boxplot of relative PT by country by R0mx/airScale:
p1 <- ggplot(data = res.df.pt.R0mx) + geom_boxplot(aes(x = country, y = pt.rank), fill = 'steelblue2') +
  geom_hline(yintercept = mean(res.df.pt.R0mx$pt.rank), lwd = 1.0, lty = 2, col = 'gray80') + 
  theme_classic() + labs(x = '', y = 'Peak Timing (Relative)') + facet_wrap(~ R0mx, ncol = 1)
p2 <- ggplot(data = res.df.pt.airScale) + geom_boxplot(aes(x = country, y = pt.rank), fill = 'steelblue2') +
  geom_hline(yintercept = mean(res.df.pt.airScale$pt.rank), lwd = 1.0, lty = 2, col = 'gray80') + 
  theme_classic() + labs(x = '', y = 'Peak Timing (Relative)') + facet_wrap(~ airScale, ncol = 1)
print(p1); print(p2)

dev.off()

# # Any cases of peak order changing with R0mx/airScale change?:
# for (i in levels(res.df.pt.R0mx$outbreak)) {
#   
#   m.temp <- res.df.pt.R0mx[res.df.pt.R0mx$outbreak == i, ]
#   order.pt <- vector('list', 3)
#   for (j in 1:3) {
#     r0 <- levels(m.temp$R0mx)[j]
#     order.pt[[j]] <- m.temp[m.temp$R0mx == r0, ][order(m.temp$pt.rank[m.temp$R0mx == r0]), c('country', 'pt.rank')]
#   }
#   
#   
# }
# doesn't look to be much, and I'm not even sure how much of this we expect to occur if it's just air travel being stronger, not different







###################################################################################################################################################################################################
###################################################################################################################################################################################################
###################################################################################################################################################################################################

# Then the idea is to test PARAMETER SENSITIVITY (esp. airScale) in a model w/o AH forcing, as well
# (Although I'm not sure it's necessary to look at R0 sensitivity in a model w/o travel for AH vs. no AH)
# Playing around with this can be done once we have a set of "realistic" outbreaks that don't use AH forcing

# ### Same, but no AH-forcing ###
# 
# # Setup:
# set.seed(10489436)
# 
# # Set initial conditions and params:
# parms <- rbind(init.states.USE, t(parms.USE))
# 
# # Repeat 6 times!:
# count <- 1
# while (count < 7) {
#   parms <- cbind(parms, parms[, 1:3]); count <- count + 1
# }
# 
# # Change R0mx and airScale where desired:
# parms[46, 4:6] <- 1.4
# parms[46, 7:9] <- 1.6
# parms[46, 10:12] <- 2.0
# parms[45, 13:15] <- 0.75
# parms[45, 16:18] <- 1.00
# parms[45, 19:21] <- 1.25
# parms[46, 13:21] <- parms[46, 13:21] - 0.9
# 
# # Set initial S/I for all compartments:
# num_ens <- dim(parms)[2]
# S0.temp = I0.temp = vector('list', num_ens)
# for (i in 1:num_ens) {
#   S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
#   
#   diag(S0.temp[[i]]) <- parms[1:n, i]
#   S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
#     rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
#   })
#   S0.temp[[i]] <- t(S0.temp[[i]])
#   S0.temp[[i]] <- S0.temp[[i]] * N
#   
#   diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
#   I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
#   I0.temp[[i]] <- I0.temp[[i]] * N
# }
# 
# # Reduce parms to hold just the parameters now:
# parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
# 
# # Calculate betas
# beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
# beta <- lapply(1:num_ens, function(ix) {
#   matrix(parms[4, ix] / parms[1, ix], nrow = length(beta.range), ncol = n)
# })
# 
# # Create vectors of initial parameters:
# D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365; airScale.temp <- parms[3, ]
# 
# # Run!
# m <- sapply(1:num_ens, function(ix) {
#   propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
#                    S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
#                    D = D.temp[ix], L = L.temp[ix], beta[[ix]],
#                    airScale = airScale.temp[ix], realdata = TRUE,
#                    prohibAir = FALSE)
# })
# 
# # Format results:
# nt <- floor((length(tm_strt:tm_end) + 1) / 7)
# newI <- lapply(1:(n ** 2), function(ix) {
#   matrix(unlist(lapply(1:num_ens, function(jx) {
#     m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
#   })), nrow = num_ens, byrow = TRUE)
# }) # each ensemble member has its own row
# 
# newI.c <- vector('list', n)
# for (i in 1:n) {
#   newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
# }
# newI.c.COUNT <- newI.c
# 
# for (i in 1:n) {
#   newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
# }
# 
# newI.ens <- vector('list', num_ens)
# for (i in 1:num_ens) {
#   newI.ens.temp <- NULL
#   
#   for (j in 1:n) {
#     newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
#   }
#   
#   newI.ens[[i]] <- t(newI.ens.temp)
# }; rm(newI.ens.temp)
# 
# pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noAH.pdf', height = 10, width = 8)
# 
# par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# matplot(newI.ens[[13]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
# matplot(newI.ens[[16]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
# matplot(newI.ens[[19]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')
# 
# matplot(newI.ens[[14]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
# matplot(newI.ens[[17]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
# matplot(newI.ens[[20]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')
# 
# matplot(newI.ens[[15]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 0.75 (R0 = R0mx - 0.9)')
# matplot(newI.ens[[18]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.00')
# matplot(newI.ens[[21]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'airScale = 1.25')
# # so R0mx increases synchrony, but little impact of airScale (can see very slight changes in some countries, but not like increased synchrony in Sen's model)
# 
# dev.off()
# 
# ###################################################################################################################################################################################################
# ###################################################################################################################################################################################################
# ###################################################################################################################################################################################################
# 
# ### AH-forcing, but no travel ###
# 
# # Setup:
# set.seed(10489436)
# 
# # Set population sizes and # of countries used
# pop.size <- read.csv('data/popcounts_02-07.csv')
# pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
# pop.size <- pop.size[match(countries, pop.size$country), ]
# 
# # Load commuting data
# load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
# t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
# t.comm <- t.comm[countries, countries]
# t.comm[t.comm > 0] <- 0
# 
# # Set country populations
# N <- t.comm; n <- length(countries) # w/ commuting
# diag(N) <- unlist(lapply(1:n, function(ix) {
#   pop.size$pop[ix] - rowSums(N)[ix]
# }))
# 
# # Set initial conditions and params:
# parms <- rbind(init.states.USE, t(parms.USE))
# 
# # Repeat 4 times!:
# count <- 1
# while (count < 4) {
#   parms <- cbind(parms, parms[, 1:3]); count <- count + 1
# }
# 
# # Change R0mx and airScale where desired:
# parms[46, 4:6] <- 2.5
# parms[46, 7:9] <- 2.75
# parms[46, 10:12] <- 3.0
# # parms[45, 13:15] <- 0.75
# # parms[45, 16:18] <- 1.00
# # parms[45, 19:21] <- 1.25
# 
# # Set initial S/I for all compartments:
# num_ens <- dim(parms)[2]
# S0.temp = I0.temp = vector('list', num_ens)
# for (i in 1:num_ens) {
#   S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
#   
#   diag(S0.temp[[i]]) <- parms[1:n, i]
#   S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
#     rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
#   })
#   S0.temp[[i]] <- t(S0.temp[[i]])
#   S0.temp[[i]] <- S0.temp[[i]] * N
#   
#   diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
#   I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
#   I0.temp[[i]] <- I0.temp[[i]] * N
# }
# 
# # Reduce parms to hold just the parameters now:
# parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
# 
# # Calculate betas
# beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
# AHpt <- AH[beta.range, ]
# AHpt <- as.matrix(AHpt, length(AHpt), n)
# b <- log(parms[4, ] - parms[5, ])
# a <- -180
# beta <- lapply(1:num_ens, function(ix) {
#   (exp(a * AHpt + b[ix]) + parms[5, ix]) / parms[1, ix]
# })
# 
# # Create vectors of initial parameters:
# D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365
# 
# # Run!
# m <- sapply(1:num_ens, function(ix) {
#   propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
#                    S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
#                    D = D.temp[ix], L = L.temp[ix], beta[[ix]],
#                    airScale = 0, realdata = TRUE, prohibAir = TRUE)
# })
# 
# # Format results:
# nt <- floor((length(tm_strt:tm_end) + 1) / 7)
# newI <- lapply(1:(n ** 2), function(ix) {
#   matrix(unlist(lapply(1:num_ens, function(jx) {
#     m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
#   })), nrow = num_ens, byrow = TRUE)
# }) # each ensemble member has its own row
# 
# newI.c <- vector('list', n)
# for (i in 1:n) {
#   newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
# }
# newI.c.COUNT <- newI.c
# 
# for (i in 1:n) {
#   newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
# }
# 
# newI.ens <- vector('list', num_ens)
# for (i in 1:num_ens) {
#   newI.ens.temp <- NULL
#   
#   for (j in 1:n) {
#     newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
#   }
#   
#   newI.ens[[i]] <- t(newI.ens.temp)
# }; rm(newI.ens.temp)
# 
# 
# pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noTravel.pdf', height = 10, width = 8)
# 
# par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
# matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
# matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')
# 
# matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
# matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
# matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')
# 
# matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.5')
# matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 2.75')
# matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0mx = 3.0')
# # these certainly look a lot "ropier" than with commuting - less synchrony with later peaks; so commuting seems to play a role, but not air travel as much; that agrees with literature, though
# 
# dev.off()
# 
# ###################################################################################################################################################################################################
# ###################################################################################################################################################################################################
# ###################################################################################################################################################################################################
# 
# ### No AH-forcing, no travel ###
# # Here, dynamics should be due purely to start conditions
# 
# # Setup:
# set.seed(10489436)
# 
# # Set population sizes and # of countries used
# pop.size <- read.csv('data/popcounts_02-07.csv')
# pop.size <- pop.size[pop.size$country %in% countries, ]; pop.size$country <- factor(pop.size$country)
# pop.size <- pop.size[match(countries, pop.size$country), ]
# 
# # Load commuting data
# load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData')
# t.comm <- apply(simplify2array(comm.by.year), 1:2, mean); rm(comm.by.year)
# t.comm <- t.comm[countries, countries]
# t.comm[t.comm > 0] <- 0
# 
# # Set country populations
# N <- t.comm; n <- length(countries) # w/ commuting
# diag(N) <- unlist(lapply(1:n, function(ix) {
#   pop.size$pop[ix] - rowSums(N)[ix]
# }))
# 
# # Set initial conditions and params:
# parms <- rbind(init.states.USE, t(parms.USE))
# 
# # Repeat 4 times!:
# count <- 1
# while (count < 4) {
#   parms <- cbind(parms, parms[, 1:3]); count <- count + 1
# }
# 
# # Change R0mx and airScale where desired:
# parms[46, 4:6] <- 1.4
# parms[46, 7:9] <- 1.6
# parms[46, 10:12] <- 2.0
# # parms[45, 13:15] <- 0.75
# # parms[45, 16:18] <- 1.00
# # parms[45, 19:21] <- 1.25
# 
# # Set initial S/I for all compartments:
# num_ens <- dim(parms)[2]
# S0.temp = I0.temp = vector('list', num_ens)
# for (i in 1:num_ens) {
#   S0.temp[[i]] = I0.temp[[i]] = matrix(0, nrow = n, ncol = n)
#   
#   diag(S0.temp[[i]]) <- parms[1:n, i]
#   S0.temp[[i]][S0.temp[[i]] == 0] <- sapply(1:n, function(jx) {
#     rnorm(n - 1, mean = S0.temp[[i]][jx, jx], sd = 0.05)
#   })
#   S0.temp[[i]] <- t(S0.temp[[i]])
#   S0.temp[[i]] <- S0.temp[[i]] * N
#   
#   diag(I0.temp[[i]]) <- parms[(1:n) + n, i]
#   I0.temp[[i]] <- sweep(N / rowSums(N), 1, diag(I0.temp[[i]]), '*')
#   I0.temp[[i]] <- I0.temp[[i]] * N
# }
# 
# # Reduce parms to hold just the parameters now:
# parms <- parms[(dim(parms)[1] - 4):(dim(parms)[1]), ]
# 
# # Calculate betas
# beta.range <- tm.range[1]:(tail(tm.range, 1) + 2 * tmstep)
# beta <- lapply(1:num_ens, function(ix) {
#   matrix(parms[4, ix] / parms[1, ix], nrow = length(beta.range), ncol = n)
# })
# 
# # Create vectors of initial parameters:
# D.temp <- parms[1, ]; L.temp <- parms[2, ] * 365
# 
# # Run!
# m <- sapply(1:num_ens, function(ix) {
#   propagateToySIRS(tm_strt = tm_strt, tm_end = tm_end, dt,
#                    S0 = S0.temp[[ix]], I0 = I0.temp[[ix]], N,
#                    D = D.temp[ix], L = L.temp[ix], beta[[ix]],
#                    airScale = 0, realdata = TRUE, prohibAir = TRUE)
# })
# 
# # Format results:
# nt <- floor((length(tm_strt:tm_end) + 1) / 7)
# newI <- lapply(1:(n ** 2), function(ix) {
#   matrix(unlist(lapply(1:num_ens, function(jx) {
#     m[3, jx]$newI[tmstep * (1:nt) + 1, ix] - m[3, jx]$newI[tmstep * (0:(nt - 1)) + 1, ix]
#   })), nrow = num_ens, byrow = TRUE)
# }) # each ensemble member has its own row
# 
# newI.c <- vector('list', n)
# for (i in 1:n) {
#   newI.c[[i]] <- Reduce('+', newI[(i * n - n + 1):(i * n)])
# }
# newI.c.COUNT <- newI.c
# 
# for (i in 1:n) {
#   newI.c[[i]] <- (newI.c[[i]] / pop.size$pop[i]) * 100000
# }
# 
# newI.ens <- vector('list', num_ens)
# for (i in 1:num_ens) {
#   newI.ens.temp <- NULL
#   
#   for (j in 1:n) {
#     newI.ens.temp <- rbind(newI.ens.temp, newI.c[[j]][i, ])
#   }
#   
#   newI.ens[[i]] <- t(newI.ens.temp)
# }; rm(newI.ens.temp)
# 
# pdf('syntheticTests/sensitivity/outputs_plots/sens_to_models_and_params_red_noAH_noTravel.pdf', height = 10, width = 8)
# 
# par(mfrow = c(3, 1), cex = 1.0, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
# matplot(newI.ens[[4]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[7]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[10]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# matplot(newI.ens[[5]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[8]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[11]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# matplot(newI.ens[[6]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.4')
# matplot(newI.ens[[9]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 1.6')
# matplot(newI.ens[[12]], type = 'b', pch = 20, lty = 1, col = viridis(n), main = 'R0 = 2.0')
# 
# dev.off()


