
# Divergence tends to occur when predicted/observed lead weeks > 0
    # Look at ratio of OEV to ens.var. at lead weeks 1-4
    # (Okay to restrict to this narrow range - limits the amount of information that needs processing)

# Read in and compile files of OEV and ensemble variance (both scaled):
file.list <- list.files(path = 'results/original/obs-ens_Vars/')
oVar <- NULL
for (i in 1:length(file.list)) {
  var.temp <- read.csv(paste0('results/original/obs-ens_Vars/', file.list[i]))
  oVar <- rbind(oVar, var.temp)
}
rm(var.temp, file.list, i)

# Format data frame:
oVar <- oVar[, c(1:2, 4:7)]
names(oVar) <- c('season', 'run', 'country', 'week', 'obs_var', 'ens_var')

# Join with metrics file to get obs_pkwk:
m <- read.csv('results/original/fairComp/outputMet_110819_pro_PROC.csv')
m <- unique(m[, c(1:2, 9)])
oVar <- merge(oVar, m, by = c('season', 'country'))

# Calculate obs. lead weeks:
oVar$lead <- oVar$week - oVar$obs_pkwk

# First let's look at the ratios over time, by country:
oVar$ratio <- oVar$obs_var / oVar$ens_var
hist(oVar$ratio)
quantile(oVar$ratio) # 25: 0.566; 50: 1.06; 75: 6.9; 100: 19578140 (obviously too much)
quantile(oVar$ratio, probs = c(0.80, 0.85, 0.90, 0.95)) # 14, 33, 97, 735
# so the 90th percentile of ~100 could be promising; could also try with 10 (between 75 and 80th)

oVar$run <- factor(oVar$run)

pdf('code/gridSearch/plots/oev-ens_ratio_byObsLead.pdf', width = 12, height = 10)
ggplot(data = oVar[oVar$lead > -9 & oVar$lead < 5, ], aes(x = lead, y = ratio, col = run)) + geom_point() + geom_line() +
  facet_grid(country ~ season, scales = 'free_y') + scale_y_continuous(trans = 'log10')
# I still feel decent about using 5 as a cutoff, at least to try

# let's limit this to lead weeks 1-4 and see what's happening:
ggplot(data = oVar[oVar$lead > 0 & oVar$lead < 5, ], aes(x = lead, y = ratio, col = run)) + geom_point() + geom_line() + facet_grid(country ~ season) +
  scale_y_continuous(limits = c(1, 100), trans = 'log10')
# maybe 5 is a decent cutoff honestly - they're usually only around a ratio of 1 at week 1 post peak, but we see issues happening soon! (could also try 10)
dev.off()

# Which countries have pretty clear divergence in output plots?:
# DE, SK; PL, AT, NL, LU, CZ
# really tends to depend on outbreak patterns - fits fine if there is a consistent downward slope, but doesn't deal with upticks/unexpected activity well


