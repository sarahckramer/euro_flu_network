
### Run to process files downloaded from cluster (network model only) ###
model.type <- 'Network'
# model.type <- 'Individual'

# 1. Read in and compile:
if (model.type == 'Network') {
  source('code/gridSearch/process_raw_data/compile_fcasts_from_cluster.R')
}

# 2. Process metrics file:
source('code/gridSearch/process_raw_data/process_metrics_file.R')
m.store <- m

# 3. Calculate log scores:
if (model.type == 'Network') {
  source('../../code/gridSearch/process_raw_data/calculate_log_scores.R')
  
} else if (model.type == 'Individual') {
  source('../../code/individualCountries/calculate_log_scores.R')
}

# 4. Reorder columns as necessary:
if (model.type == 'Network') {
  source('../../code/gridSearch/process_raw_data/standardize_all_output_files.R')
}

# 5. Remove where there would be no forecasts in individual model:
if (model.type == 'Network') {
  source('../../code/gridSearch/process_raw_data/remove_fcasts_where_no_data.R')
}
# nothing to do here for individual model

# 6: Reset directory:
setwd('../../')

# 7: Delete files in "raw":
file.list <- list.files(path = 'results/raw/')
for (file in file.list) {
  file.remove(paste0('results/raw/', file))
}
rm(file, file.list)

rm(list = ls())
