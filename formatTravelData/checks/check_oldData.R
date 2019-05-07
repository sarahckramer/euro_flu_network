
# Check that data formatted based on files in new repository is the same as "old" data:

# Air:
for (i in 1:12) {
  load(paste0('formatTravelData/formattedData/air_', i, '_05-07.RData'))
  print(a.temp.sym['BE', ])
  load(paste0('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/air_', i, '_04-16.RData'))
  print(a.temp.sym['BE', 2:20])
  print('')
}

# Train:
load('formatTravelData/formattedData/train_05-07.RData')
print(t.rand['BE', ])
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/forecastsE/travel_data/train_04-30.RData')
print(t.rand['BE', 2:20])

# Commuting:
load('formatTravelData/formattedData/comm_mat_by_year_05-07.RData') # note: includes non-adjacent
load('/Users/sarahkramer/Desktop/Lab/spatial_transmission/flight_data/data/matrices/comm_mat_by_year_ADJ_04-16.RData')
print(comm.by.year[[1]]['BE', ]); print(comm.by.year.adj[[1]]['BE', 2:20])
print(comm.by.year[[1]]['IT', ]); print(comm.by.year.adj[[1]]['IT', 2:20])

# Somehow Romania has returned? Can probably remove, since not much data on outgoing commuters
# Otherwise, things look consistent




