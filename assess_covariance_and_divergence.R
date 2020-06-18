
### Assess Cross-Ensemble Covariability ###
# Here we're just looking at fit, so whether forecasts predict onset or not is not relevant here (right?) -- QUESTION 1

# Plot:
    # Covariance against R0mx, R0diff, D, L, and airScale for all 12 countries
    # Covariance between each of 12 countries and: all S; only S "involving" country; only main S
    # Same for I and newI

# We'll have plots like this for several (sub)types and seasons and runs...

countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
# a <- read.csv('python/results/outputCrossEnsVar_A(H1).csv')
# a$country <- countries[a$country + 1]
b <- read.csv('python/results/outputCorrCoefs_A(H1).csv')
b$country <- countries[b$country + 1]

# a1 <- a[a$country == 'AT', ]
s.vals <- 3:146
i.vals <- 147:290
newi.vals <- 291:434

# plot(a1[, 435], pch = 20)

a <- a[a$week < 70, ]
b <- b[b$week < 70, ]

# ggplot(data = a, aes(x = week, y = X433, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'L')
# ggplot(data = a, aes(x = week, y = X434, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'D')
# ggplot(data = a, aes(x = week, y = X435, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'R0mx')
# ggplot(data = a, aes(x = week, y = X436, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'R0diff')
# ggplot(data = a, aes(x = week, y = X437, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'aScale')
# 
# ggplot(data = a, aes(x = week, y = X1, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country, scales = 'free') + labs(x = 'Week', y = 'L')
# # not really finding correlations with things that aren't newI??

pdf('results/plots/correlations_06-18.pdf', width = 18, height = 10)
ggplot(data = b, aes(x = week, y = X433, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'L') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = b, aes(x = week, y = X434, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'D') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = b, aes(x = week, y = X435, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'R0max') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = b, aes(x = week, y = X436, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'R0diff') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
ggplot(data = b, aes(x = week, y = X437, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Week', y = 'aScale') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))
dev.off()

# for parameters, can get means/medians, as well as sds or quantiles, by time for the 5 parameters?
# then for state variables... do same, for each country separately? Average over all S, I, newI? Or just those that "involve" the country? or just main compartments? main vs. commuter?

# ggplot(data = b, aes(x = week, y = X4, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
#   labs(x = 'Week', y = 'state') + scale_color_brewer(palette = 'Set2') + scale_y_continuous(limits = c(-1, 1))





#rm(list = ls())

### Assess Filter Divergence Over Time ###
# For each country, show by observed lead week (no "predicted" since we're looking at fits, not forecasts)
countries <- c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')
a <- read.csv('python/results/outputKalmanGain_A(H1).csv')
b <- read.csv('python/results/outputVarRatio_A(H1).csv')

# Set country names:
a$country <- countries[a$country + 1]; b$country <- countries[b$country + 1]

# Get observed lead weeks:
m <- read.csv('results/submission1/by_subtype/network_A(H1)/outputMet_pro_PROC.csv')
m <- unique(m[, c(1, 8:9)])
a <- merge(a, m, by = c('season', 'country'))
b <- merge(b, m, by = c('season', 'country'))
a$lead_week <- a$week - a$obs_pkwk
b$lead_week <- b$week - b$obs_pkwk

# Again, several (sub)types, seasons, and runs...
ggplot(data = a, aes(x = week, y = kg, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) + labs(x = 'Week', y = 'KG')
ggplot(data = b, aes(x = week, y = ratio, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) + labs(x = 'Week', y = 'OEV / Ens. Var.') +
  scale_y_continuous(trans = 'log10')

# By observed lead:
a1 <- a[a$lead_week >= -8 & a$lead_week <= 8, ]
b1 <- b[b$lead_week >= -8 & b$lead_week <= 8, ]

ggplot(data = a1, aes(x = lead_week, y = kg, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) + labs(x = 'Obs. Lead Week', y = 'Kalman Gain')
ggplot(data = b1, aes(x = lead_week, y = ratio, group = run)) + geom_point(size = 0.75) + theme_bw() + facet_grid(season ~ country) + labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.')

# ggplot(data = a1, aes(x = lead_week, y = kg, group = paste(country, run), col = country)) + geom_line() + theme_bw() + facet_wrap(~ season) + labs(x = 'Obs. Lead Week', y = 'Kalman Gain')

p1 <- ggplot(data = a1, aes(x = lead_week, y = kg, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Obs. Lead Week', y = 'Kalman Gain') + scale_x_continuous(breaks = -8:8) + scale_color_brewer(palette = 'Set2')
p2 <- ggplot(data = b1, aes(x = lead_week, y = ratio, group = paste(season, run), col = season)) + geom_line() + theme_bw() + facet_wrap(~ country) +
  labs(x = 'Obs. Lead Week', y = 'OEV / Ens. Var.') + scale_x_continuous(breaks = -8:8) + scale_color_brewer(palette = 'Set2') +
  scale_y_continuous(trans = 'log10')

# # Or outbreak only:
# a2 <- a[a$week < 72, ]; b2 <- b[b$week < 72, ] # end of week 19 / end of the season
# ggplot(data = a2, aes(x = week, y = kg, group = run)) + geom_line() + theme_bw() + facet_grid(season ~ country) + labs(x = 'Week', y = 'KG')
# # ggplot(data = b2, aes(x = week, y = ratio, group = run)) + geom_line() + theme_bw() + facet_grid(season ~ country) + labs(x = 'Week', y = 'OEV / Ens. Var.')

# grid.arrange(p1, p2, ncol = 1)

pdf('results/plots/divergence_06-18.pdf', width = 18, height = 10)
print(p1)
print(p2)
dev.off()








