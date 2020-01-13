
### Run to process files downloaded from cluster (network model only) ###
model.type <- 'Network'
# model.type <- 'Individual'

# Set strain:
strain <- 'A(H3)'

# 1. Process metrics file:
source('code/comparisons/process_raw_data/process_metrics_file.R')
m.store <- m

# 2. Calculate log scores:
if (model.type == 'Network') {
  source('../../../code/comparisons/process_raw_data/calculate_log_scores.R')
  
} else if (model.type == 'Individual') {
  source('../../../code/individualCountries/calculate_log_scores.R')
  write.csv(m.store, file = 'outputMet_pro.csv', row.names = FALSE)
}

# 3. Remove where there would be no forecasts in individual model:
if (model.type == 'Network') {
  source('../../../code/comparisons/process_raw_data/remove_fcasts_where_no_data.R')
} else if (model.type == 'Individual') {
  source('../../code/individualCountries/remove_fcasts_where_oevNew0NA.R')
}

# 4. Reorder columns as necessary:
if (model.type == 'Network') {
  source('../../../code/comparisons/process_raw_data/standardize_all_output_files.R')
}

# 5: Reset directory:
if (exists('model.type')) {
  setwd('../../..')
} else {
  setwd('../../')
}

# Clear environment:
rm(list = ls())
