
# Country names
europe <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'France', 'Germany', 'Hungary', 'Ireland', 'Italy', 'Luxembourg',
            'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom')

# Countries by data type
north.ili <- c('AT', 'BE', 'HR', 'CZ', 'DK', 'HU', 'IE', 'IT', 'NL', 'PL', 'PT', 'RO', 'SK', 'ES', 'SE')
north.ari <- c('LU', 'UK', 'DE', 'SI', 'FR')

# Read in results
m <- read.csv('code/gridSearch/outputs/outputMet_090119.csv')

m <- read.csv('code/individualCountries/outputs/outputMet_082819.csv')
names(m)[4] <- 'scaling'
m$scaling[m$country == 'FR' & m$season %in% c('2010-11', '2011-12', '2012-13', '2013-14')] <- 1.3
for (i in 37:44) {
  m[, i] <- m[, i] + 40 - 1
}

# Are peak intensities still the same for all base/denom/lambda at this point?:
m.check <- unique(m[, c(1, 8:9, 17)]) # yep!


# Re-code any magnitudes by scaling
m$obs_peak_int <- round(m$obs_peak_int/m$scaling, digits=4)
m$peak_intensity <- m$peak_intensity/m$scaling
m$intensity_err <- m$intensity_err/m$scaling
m$tot_attack <- m$tot_attack/m$scaling
m$totAttackObs <- m$totAttackObs/m$scaling
m$delta_AR <- m$delta_AR/m$scaling
m$obs_1week <- m$obs_1week/m$scaling
m$fcast_1week <- m$fcast_1week/m$scaling
# m$delta_1w <- m$delta_1w/m$scaling # these are already relative differences
m$obs_2week <- m$obs_2week/m$scaling
m$fcast_2week <- m$fcast_2week/m$scaling
# m$delta_2w <- m$delta_2w/m$scaling
m$obs_3week <- m$obs_3week/m$scaling
m$fcast_3week <- m$fcast_3week/m$scaling
# m$delta_3w <- m$delta_3w/m$scaling
m$obs_4week <- m$obs_4week/m$scaling
m$fcast_4week <- m$fcast_4week/m$scaling
# m$delta_4w <- m$delta_4w/m$scaling

# Calculate relevant metrics
m$FWeek_pkwk <- m$fc_start - m$obs_pkwk
m$FWeek_pkwk_bin <- cut(m$FWeek_pkwk, c(-Inf, -10, -7, -4, -1, 2, 5, 8, Inf))
m$abs_delta_pkwk_mean <- abs(m$delta_pkwk_mean)
m$abs_delta_peak_int <- abs(m$intensity_err) + m$obs_peak_int
m$abs_delta_AR <- abs(m$delta_AR) + m$totAttackObs
# m$abs_delta_1w <- abs(m$delta_1w) + m$obs_1week
# m$abs_delta_2w <- abs(m$delta_2w) + m$obs_2week
# m$abs_delta_3w <- abs(m$delta_3w) + m$obs_3week
# m$abs_delta_4w <- abs(m$delta_4w) + m$obs_4week

# Bin peak intensity by accuracy
# Binnings are going to leave out instances where:
# No peak was predicted
# No actual peak occured
# Both of the above
for(country in levels(m$country)) {
  peak.vals <- unique(m$obs_peak_int[m$country == country])
  peak.vals <- peak.vals[!is.na(peak.vals)]
  #print(peak.vals)
  for(peak.val in peak.vals){
    m$abs_delta_peak_int_bin[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)] <- cut(m$abs_delta_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)], c("1" = unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)]), "2" = (1.125*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)])), "3" = (1.25*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)])), "4" = (1.375*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)])), "5" = (1.5*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)])), "6" = (2*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)])), "7" = (10*unique(m$obs_peak_int[m$country == country & m$obs_peak_int==peak.val & !is.na(m$obs_peak_int)]))))
  }
}
m$abs_delta_peak_int_bin[is.na(m$abs_delta_peak_int_bin) & !is.na(m$obs_pkwk)] <- '7'
m$abs_delta_peak_int_bin <- factor(m$abs_delta_peak_int_bin)

# AR accuracy
m$abs_delta_AR_bin <- NULL
m$abs_delta_AR_bin[m$abs_delta_AR < 1.125 * m$totAttackObs] <- '1'
m$abs_delta_AR_bin[m$abs_delta_AR < 1.25 * m$totAttackObs & m$abs_delta_AR >= 1.125 * m$totAttackObs] <- '2'
m$abs_delta_AR_bin[m$abs_delta_AR < 1.375 * m$totAttackObs & m$abs_delta_AR >= 1.25 * m$totAttackObs] <- '3'
m$abs_delta_AR_bin[m$abs_delta_AR < 1.5 * m$totAttackObs & m$abs_delta_AR >= 1.375 * m$totAttackObs] <- '4'
m$abs_delta_AR_bin[m$abs_delta_AR >= 1.5 * m$totAttackObs] <- '5'
m$abs_delta_AR_bin <- factor(m$abs_delta_AR_bin)

# Calculate absolute value of onset timing error
m$FWeek_onwk <- m$fc_start - m$onsetObs5
m$delta_onset <- m$onset5 - m$onsetObs5
m$abs_delta_onset <- abs(m$delta_onset)

# Weeks from predicted lead signs are reversed from FWeek_pkwk - change:
m$leadpkwk_mean <- (m$leadpkwk_mean) * (-1)

# # Add additional metrics
m$region[m$country %in% c('AT', 'BE', 'HR', 'FR', 'DE', 'IT', 'LU', 'NL', 'PT', 'SI', 'ES')] <- 'Southwest Europe'
m$region[m$country %in% c('CZ', 'HU', 'PL', 'RO', 'SK')] <- 'Eastern Europe'
m$region[m$country %in% c('DK', 'IE', 'SE', 'UK')] <- 'Northern Europe'
m$region <- factor(m$region)

m$scaling.range <- cut(m$scaling, c(0, 0.5, 1, 2, 10, 20, 50, 100, 300, 500))

m$data.type[m$country %in% north.ili] <- 'ILI'
m$data.type[m$country %in% north.ari] <- 'ARI'
m$data.type[m$country == 'FR' & m$season %in% c('2014-15', '2015-16', '2016-17', '2017-18', '2018-19')] <- 'ILI'
m$data.type <- factor(m$data.type)

m$st_int_err <- m$intensity_err / m$obs_peak_int
m$abs_st_int_err <- abs(m$st_int_err)

m$accurate_pkwk[m$abs_delta_pkwk_mean %in% c(0,1)] <- 'yes'
m$accurate_pkwk[!(m$abs_delta_pkwk_mean %in% c(0,1))] <- 'no'
m$accurate_pkwk <- factor(m$accurate_pkwk)

m$accurate_int[m$abs_delta_peak_int_bin %in% c(1,2)] <- 'yes'
m$accurate_int[!(m$abs_delta_peak_int_bin %in% c(1,2))] <- 'no'
m$accurate_int <- factor(m$accurate_int)

m$accurate_on[m$abs_delta_onset %in% c(0,1)] <- 'yes'
m$accurate_on[!(m$abs_delta_onset %in% c(0,1))] <- 'no'
m$accurate_on <- factor(m$accurate_on)

# Write new metrics file
write.csv(m, file = 'code/gridSearch/outputs/outputMet_090119_pro.csv', row.names = F)
# write.csv(m, file = 'code/individualCountries/outputs/outputMet_082819_pro.csv', row.names = F)

# Clear environment
rm(list=ls())
