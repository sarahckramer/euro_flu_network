
# Read in log scores from all 4 models:
d1 <- read.csv('results/original/logScores_pt_ot.csv')
e.pi1 <- read.csv('results/original/logScores_pi.csv')
e1 <- read.csv('results/original/logScores_1-4wk.csv')

d2 <- read.csv('results/newScalings/logScores_pt_ot.csv')
e.pi2 <- read.csv('results/newScalings/logScores_pi.csv')
e2 <- read.csv('results/newScalings/logScores_1-4wk.csv')

d3 <- read.csv('results/propRandTravel/logScores_pt_ot.csv')
e.pi3 <- read.csv('results/propRandTravel/logScores_pi.csv')
e3 <- read.csv('results/propRandTravel/logScores_1-4wk.csv')

d4 <- read.csv('results/indivCountries/logScores_pt_ot.csv')
e.pi4 <- read.csv('results/indivCountries/logScores_pi.csv')
e4 <- read.csv('results/indivCountries/logScores_1-4wk.csv')
names(e.pi4)[8] = names(e4)[8] = 'metric'

# Add factors describing model:
d1$model <- 'Original'; d2$model <- 'New Scale'; d3$model <- 'Random Travel'; d4$model <- 'Individual'
e.pi1$model <- 'Original'; e.pi2$model <- 'New Scale'; e.pi3$model <- 'Random Travel'; e.pi4$model <- 'Individual'
e1$model <- 'Original'; e2$model <- 'New Scale'; e3$model <- 'Random Travel'; e4$model <- 'Individual'

# Remove if no onset:
# Already done, except for 1-4 weeks, where might not be necessary?

# Remove oev_denom and lambda specifications in 1 and 4:
d1$oev_denom <- NULL; d1$lambda <- NULL
d4$oev_denom <- NULL; d4$lambda <- NULL

e.pi1$oev_denom <- NULL; e.pi1$lambda <- NULL
e.pi4$oev_denom <- NULL; e.pi4$lambda <- NULL

e1$oev_denom <- NULL; e1$lambda <- NULL
e4$oev_denom <- NULL; e4$lambda <- NULL

# Compile:
d <- rbind(d1, d2, d3, d4)
e.pi <- rbind(e.pi1, e.pi2, e.pi3, e.pi4)
e <- rbind(e1, e2, e3, e4)

# Make model factor:
d$model <- factor(d$model); d$model <- factor(d$model, levels = levels(d$model)[c(3, 2, 4, 1)])
e.pi$model <- factor(e.pi$model); e.pi$model <- factor(e.pi$model, levels = levels(e.pi$model)[c(3, 2, 4, 1)])
e$model <- factor(e$model); e$model <- factor(e$model, levels = levels(e$model)[c(3, 2, 4, 1)])

# Clean-up:
rm(d1, d2, d3, d4, e.pi1, e.pi2, e.pi3, e.pi4, e1, e2, e3, e4)

