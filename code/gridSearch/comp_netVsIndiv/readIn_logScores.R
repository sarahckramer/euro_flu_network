
# Read in log scores from network model:
d <- read.csv(paste0(model1, list.files(path = model1, pattern = '_pt_ot')))
e.pi <- read.csv(paste0(model1, list.files(path = model1, pattern = '_pi')))
e <- read.csv(paste0(model1, list.files(path = model1, pattern = '_1-4wk')))

d2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_pt_ot')))
e.pi2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_pi')))
e2 <- read.csv(paste0(model2, list.files(path = model2, pattern = '_1-4wk')))

# Add factor describing model:
d$model <- 'Network'; d2$model <- 'Individual'
e.pi$model <- 'Network'; e.pi2$model <- 'Individual'
e$model <- 'Network'; e2$model <- 'Individual'

# Remove if no onset:
# Already done, except for 1-4 weeks, where might not be necessary?

# Compile:
d <- rbind(d, d2)
e.pi <- rbind(e.pi, e.pi2)
e <- rbind(e, e2)
rm(d2, e.pi2, e2)

