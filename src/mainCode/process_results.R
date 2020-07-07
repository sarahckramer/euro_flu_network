
### Run to process files from python_init ###

# Specify model.type and (sub)type
# model.type <- 'Network'
model.type <- 'Individual'
strain <- 'A(H1)' # 'A(H1)', 'A(H3)', or 'B'

# Create relevant directories:
if (!dir.exists('results/by_subtype/')) {
  dir.create('results/by_subtype/')
}
if (model.type == 'Network') {
  if (!dir.exists(paste0('results/by_subtype/network_', strain))) {
    dir.create(paste0('results/by_subtype/network_', strain))
  }
  if (!dir.exists(paste0('results/by_subtype/network_', strain, '/extra/'))) {
    dir.create(paste0('results/by_subtype/network_', strain, '/extra/'))
  }
} else if (model.type == 'Individual') {
  if (!dir.exists(paste0('results/by_subtype/isolated', strain))) {
    dir.create(paste0('results/by_subtype/isolated_', strain))
  }
  if (!dir.exists(paste0('results/by_subtype/isolated_', strain, '/extra/'))) {
    dir.create(paste0('results/by_subtype/isolated_', strain, '/extra/'))
  }
}

# 1. Process metrics file:
source('src/mainCode/process_results/process_metrics_file.R')
m.store <- m

# 2. Calculate log scores:
source('../../../src/mainCode/process_results/calculate_log_scores.R')

# 3. Remove where there would be no forecasts in individual model:
if (model.type == 'Network') {
  source('../../../src/mainCode/process_results/remove_fcasts_where_no_data.R')
} else if (model.type == 'Individual') {
  o <- read.csv(list.files(pattern = 'OP_'))
}

# 4. Reorder columns as necessary:
source('../../../src/mainCode/process_results/standardize_all_output_files.R')

# 5. Move "OPParams" file (network only):
if (model.type == 'Network') {
  op <- read.csv(file = list.files(pattern = 'OPParams'))
  write.csv(op, file = paste0('../../by_subtype/network_', strain, '/', list.files(pattern = 'OPParams')), row.names = FALSE)
  file.remove(list.files(pattern = 'OPParams'))
}

# 6. Move initial results to "extra" folder:
files.to.move <- list.files()
for (f in files.to.move) {
  f.temp <- read.csv(f)
  
  if (model.type == 'Network') {
    write.csv(f.temp, paste0('../../by_subtype/network_', strain, '/extra/', f), row.names = FALSE)
  } else if (model.type == 'Individual') {
    write.csv(f.temp, paste0('../../by_subtype/isolated_', strain, '/extra/', f), row.names = FALSE)
  }
  
  file.remove(f)
}

# 7. Reset directory:
setwd('../../..')

# 8. Clear environment:
rm(list = ls())
