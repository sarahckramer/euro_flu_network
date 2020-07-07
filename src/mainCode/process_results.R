
### Run to process files from python_init ###

# Specify model.type and (sub)type
model.type <- 'Network'
# model.type <- 'Individual'
strain <- 'B' # 'A(H1)', 'A(H3)', or 'B'

# 1. Process metrics file:
source('code/process_results/process_metrics_file.R')
m.store <- m

# 2. Calculate log scores:
source('../../../code/process_results/calculate_log_scores.R')

# 3. Remove where there would be no forecasts in individual model:
if (model.type == 'Network') {
  source('../../../code/process_results/remove_fcasts_where_no_data.R')
} else if (model.type == 'Individual') {
  o <- read.csv(list.files(pattern = 'OP_'))
}

# 4. Reorder columns as necessary:
source('../../../code/process_results/standardize_all_output_files.R')

# 5: Reset directory:
setwd('../../..')

# Clear environment:
rm(list = ls())
