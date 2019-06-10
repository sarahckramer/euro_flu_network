
# Read in all metrics files and merge:
file.list <- list.files('code/gridSearch/outputs/metrics/')
met.list <- list()
for (i in 1:length(file.list)) {
  met.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/metrics/', file.list[[i]]))
}

# Merge all files:
m <- NULL
for (i in 1:length(file.list)) {
  m <- rbind(m, met.list[[i]])
}
rm(file.list, met.list, i)

# Remove where obs are NA:
m <- m[!is.na(m$pt.obs), ]

# Look at: PT acc, PI acc, corr, rmse by all 5 variables and by season:






# And also assess this stuff by specific country:





# Read in all output files and merge:
file.list <- list.files('code/gridSearch/outputs/param_fits/')
op.list <- list()
for (i in 1:length(file.list)) {
  op.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/param_fits/', file.list[[i]]))
}

# Remove country incidence for now:
for (i in 1:length(file.list)) {
  op.list[[i]] <- op.list[[i]][, c(1:7, 29:33)]
}

# Add "time" variable:
for (i in 1:length(file.list)) {
  op.list[[i]]$time <- rep(1:40, 6)
}

# Merge all files:
o <- NULL
for (i in 1:length(file.list)) {
  o <- rbind(o, op.list[[i]])
}
rm(file.list, op.list, i)

# Change variables to factors:
for (i in 2:7) {
  o[, i] <- factor(o[, i])
}

# Look at inferred params by all 5 variables:
# Plot by values?:
# First by oev_denom:
p1 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = L,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = L,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + scale_y_continuous(limits = c(365, 3650)) + labs(x = 'Time') +
  facet_wrap(~ season)
grid.arrange(p1, p2, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = D,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + labs(x = 'Time') + facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = D,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + scale_y_continuous(limits = c(2, 7)) + labs(x = 'Time') +
  facet_wrap(~ season)
grid.arrange(p1, p2, ncol = 1)

p1 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = R0mx,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + scale_y_continuous(limits = c(1.3, 4.0)) + labs(x = 'Time') +
  facet_wrap(~ season)
p2 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = R0mn,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + scale_y_continuous(limits = c(0.8, 1.2)) + labs(x = 'Time') +
  facet_wrap(~ season)
p3 <- ggplot(data = o) + geom_line(aes(x = time,
                                       y = airScale,
                                       group = paste(run, oev_denom, oev_fact,
                                                     oev_base, tmp_exp, lambda),
                                       col = oev_denom)) +
  theme_classic() + scale_y_continuous(limits = c(0.01, 2)) + labs(x = 'Time') +
  facet_wrap(~ season)
grid.arrange(p1, p2, p3, ncol = 1)

# Runs of the same season w/ different values aren't necessarily agreeing on what the 
# "true" parameters are, but oev_denom = 1 seems best able to get realistic values, at
# least for now (although partially it's just that these actually ran)
# Seems to be greater agreement for 2011-12 than 2010-11 for now












