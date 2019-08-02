
# Read in all "alps" files and merge:
file.list <- list.files('code/gridSearch/outputs/obs/', pattern = 'Alps')
alp.list <- list()
for (i in 1:length(file.list)) {
  alp.list[[i]] <- read.csv(paste0('code/gridSearch/outputs/obs/', file.list[[i]]))
}

# Merge all files:
alps <- NULL
for (i in 1:length(file.list)) {
  alps <- rbind(alps, alp.list[[i]])
}
rm(file.list, alp.list, i)

# Add "groups" factor:
alps$group <- paste(alps$season, alps$run, alps$oev_base, alps$oev_denom, alps$lambda, sep = '_')
alps$group <- factor(alps$group)

# Look at alps by season/country to see where data tend to take over compared to model (i.e., when alp is LOW):
p5 <- ggplot(data = alps) + geom_line(aes(x = week, y = value, group = group, col = season)) +
  theme_classic() + facet_wrap(~ country)
print(p5)
# not really seeing any that are way worse than any others - all data become pretty narrow compared to the ensemble range at some point

alps$oev_base <- factor(alps$oev_base)
p6 <- ggplot(data = alps) + geom_line(aes(x = week, y = value, group = group, col = oev_base), alpha = 0.5) +
  theme_classic() + facet_wrap(~ country)
print(p6)
# way more "takeover" of data when oev_base is 1e4

alps$oev_denom <- factor(alps$oev_denom)
p7 <- ggplot(data = alps) + geom_line(aes(x = week, y = value, group = group, col = oev_denom), alpha = 0.5) +
  theme_classic() + facet_wrap(~ country)
print(p7)
# no impact

alps$lambda <- factor(alps$lambda)
p8 <- ggplot(data = alps) + geom_line(aes(x = week, y = value, group = group, col = lambda), alpha = 0.5) +
  theme_classic() + facet_wrap(~ country)
print(p8)
# actually doesn't look to be a clear pattern, even though we'd expect data to "take over" more when lambda is high



