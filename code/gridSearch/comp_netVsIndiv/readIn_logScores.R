
# Read in log scores from network model:
d <- read.csv('code/gridSearch/outputs/logScores_pt_ot.csv')
e.pi <- read.csv('code/gridSearch/outputs/logScores_pi.csv')
e <- read.csv('code/gridSearch/outputs/logScores_1-4wk.csv')

# Read in log scores from individual models:
d2 <- read.csv('code/individualCountries/outputs/logScores_pt_ot.csv')
e.pi2 <- read.csv('code/individualCountries/outputs/logScores_pi.csv')
e2 <- read.csv('code/individualCountries/outputs/logScores_1-4wk.csv')
names(e2)[8] <- 'metric'

# Add factor describing model:
d$model <- 'Network'; d2$model <- 'Individual'
e.pi$model <- 'Network'; e.pi2$model <- 'Individual'
e$model <- 'Network'; e2$model <- 'Individual'

# # Check that observed values are identical between the two!:
# d.check <- merge(d, d2, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'fc_start'))
# e.pi.check <- merge(e.pi, e.pi2, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'fc_start'))
# e.check <- merge(e, e2, by = c('season', 'run', 'oev_base', 'oev_denom', 'lambda', 'country', 'fc_start', 'metric'))
# 
# all.equal(d.check$obs_pkwk.x, d.check$obs_pkwk.y)
# all.equal(d.check$onsetObs5.x, d.check$onsetObs5.y)
# all.equal(d.check$FWeek_pkwk.x, d.check$FWeek_pkwk.y)
# all.equal(d.check$FWeek_onwk.x, d.check$FWeek_onwk.y)
# 
# all.equal(e.pi.check$FWeek_pkwk.x, e.pi.check$FWeek_pkwk.y)
# all.equal(e.pi.check$FWeek_onwk.x, e.pi.check$FWeek_onwk.y)
# all.equal(e.pi.check$obs_peak_int.x, e.pi.check$obs_peak_int.y)
# 
# all.equal(e.check$FWeek_pkwk.x, e.check$FWeek_pkwk.y)
# all.equal(e.check$obs_val.x, e.check$obs_val.y)
# 
# # differences found in obs_peak_int and obs_val
# e.check1 <- e.pi.check[round(e.pi.check$obs_peak_int.x, 1) != round(e.pi.check$obs_peak_int.y, 1), ]
# # point is, they're all off by small decimal values, and I don't think it's worth worrying about - scores are already calculated
# 
# e.check2 <- e.check[round(e.check$obs_val.x, 1) != round(e.check$obs_val.y, 1), ]
# rm(d.check, e.pi.check, e.check, e.check1, e.check2)
# # these are also very slight differences...

# Remove if no onset:
# Already done, except for 1-4 weeks, where might not be necessary?

# Compile:
d <- rbind(d, d2)
names(e.pi2)[8] <- 'metric'
e.pi <- rbind(e.pi, e.pi2)
e <- rbind(e, e2)
rm(d2, e.pi2, e2)

