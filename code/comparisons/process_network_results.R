
### Run to process files from python/downloaded from cluster ###
model.type <- 'Network'
# model.type <- 'Individual'

# Set strain:
strain <- 'A(H3)'

# 1. Process metrics file:
source('code/comparisons/process_raw_data/process_metrics_file.R')
m.store <- m

# 2. Calculate log scores:
source('../../../code/comparisons/process_raw_data/calculate_log_scores.R')

# 3. Remove where there would be no forecasts in individual model:
if (model.type == 'Network') {
  source('../../../code/comparisons/process_raw_data/remove_fcasts_where_no_data.R')
} else if (model.type == 'Individual') {
  o <- read.csv(list.files(pattern = 'OP_'))
}

# 4. Reorder columns as necessary:
source('../../../code/comparisons/process_raw_data/standardize_all_output_files.R')

# 5: Reset directory:
setwd('../../..')

# Clear environment:
rm(list = ls())
