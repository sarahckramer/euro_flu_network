
# Format all travel data:
source('formatTravelData/format_random-data.R')
source('formatTravelData/format_commuting-data.R')
source('formatTravelData/reduce_travel_data_to_routes_w_commuting.R')
source('formatTravelData/checks/percent_commuting_out.R')
rm(list=ls())
