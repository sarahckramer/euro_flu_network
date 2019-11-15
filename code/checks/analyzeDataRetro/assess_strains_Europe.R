
# Read in and format virologic data
setwd('E://Lab/spatial_transmission/WHO_data/data_4-12-18/')
v1 <- read.csv('FluNetInteractiveReport1.csv', header = FALSE)
v2 <- read.csv('FluNetInteractiveReport2.csv', header = FALSE)

setwd('E://Lab/spatial_transmission/WHO_data/data_8-05-19_TEMP/')
v3 <- read.csv('FluNetInteractiveReport.csv', header = FALSE)

colnames(v1) <- as.character(unlist(v1[3, ]))
v1 <- v1[-c(1:3), ]
rownames(v1) <- as.numeric(rownames(v1)) - 3

colnames(v2) <- as.character(unlist(v2[3, ]))
v2 <- v2[-c(1:3), ]
rownames(v2) <- as.numeric(rownames(v2)) - 3

colnames(v3) <- as.character(unlist(v3[3, ]))
v3 <- v3[-c(1:3), ]
rownames(v3) <- as.numeric(rownames(v3)) - 3

v1 <- v1[, c(1, 4:5, 8:12, 14:15, 19:20)]; v2 <- v2[, c(1, 4:5, 8:12, 14:15, 19:20)]; v3 <- v3[, c(1, 4:5, 8:12, 14:15, 19:20)]
vir.dat <- rbind(v1, v2, v3)
rm(v1, v2, v3)

# Continue to format appropriately
for (i in 2:12) {
  vir.dat[, i] <- as.numeric(as.character(vir.dat[, i]))
}; rm(i)

# Change country names
vir.dat$Country <- as.character(vir.dat$Country)
vir.dat$Country[vir.dat$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'
vir.dat$Country[vir.dat$Country == 'Bolivia (Plurinational State of)'] <- 'Bolivia'

# Remove countries not used international model OR US
# countries <- c('Austria', 'Belgium', 'Bulgaria', 'Czech Republic', 'Germany', 'Denmark', 'Greece', 'Spain',
#                'France', 'Croatia', 'Hungary', 'Ireland', 'Italy', 'Lithuania', 'Luxembourg',
#                'Latvia', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Sweden',
#                'Slovenia', 'Slovakia', 'United Kingdom', 'United States of America')
countries <- c('Austria', 'Belgium', 'Czechia', 'Germany', 'Spain', 'France', 'Hungary',
               'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Slovakia')
vir.dat <- vir.dat[vir.dat$Country %in% countries,]
vir.dat$Country <- factor(vir.dat$Country)

# Determine denominators
vir.dat$SPEC_PROCESSED_NB[is.na(vir.dat$SPEC_PROCESSED_NB) & !is.na(vir.dat$SPEC_RECEIVED_NB)] <-
  vir.dat$SPEC_RECEIVED_NB[is.na(vir.dat$SPEC_PROCESSED_NB) & !is.na(vir.dat$SPEC_RECEIVED_NB)]
vir.dat$SPEC_RECEIVED_NB <- NULL

# Calculate proportion positive at each time point for each strain
vir.dat$SPEC_PROCESSED_NB[vir.dat$SPEC_PROCESSED_NB == 0 & !is.na(vir.dat$SPEC_PROCESSED_NB)] <- NA
for (i in 5:11) {
  vir.dat[, i] <- vir.dat[, i] / vir.dat$SPEC_PROCESSED_NB
}

# Remove any proportions above 1
for (i in 5:11) {
  vir.dat[, i][vir.dat[, i] > 1 & !is.na(vir.dat[, i])] <- NA
}

# Remove denominator column
vir.dat$SPEC_PROCESSED_NB <- NULL

# Want to see if:
#   Dominant strains actaully exist, as in the US - most of the time
#   Dominant strains are same in all countries - usually, but some B-dominant
#   Dominant strains change over the course of a season - yes, mostly between A and B

# ah.all <- rep(NA, 9786)
# for (i in 1:9786) {
#   if (!(is.na(vir.dat$AH1[i]) & is.na(vir.dat$AH1N12009[i]))) {
#     ah.all[i] <- sum(vir.dat$AH1[i], vir.dat$AH1N12009[i], na.rm = T)
#   }
# }
ah.all <- rep(NA, dim(vir.dat)[1])
for (i in 1:dim(vir.dat)[1]) {
  if (!(is.na(vir.dat$AH1[i]) & is.na(vir.dat$AH1N12009[i]))) {
    ah.all[i] <- sum(vir.dat$AH1[i], vir.dat$AH1N12009[i], na.rm = T)
  }
}
vir.dat$AH1 <- ah.all; vir.dat$AH1N12009 <- NULL
# types to look at are H1, H3, A not subtyped, and B
vir.dat <- vir.dat[, c(1:6, 8)]

# # Shorten country names
# levels(vir.dat$Country)[24:25] <- c('UK', 'US')

# Order countries by region
# geo.ord <- c(17, 21, 5, 23, 9, 14, 1, 13, 6, 4, 22, 15, 16, 11:12, 10, 20, 3, 8, 19, 18, 2, 7, 24)
# vir.dat$Country <- factor(vir.dat$Country, levels = levels(vir.dat$Country)[geo.ord])

geo.ord <- c(12, 4, 9, 2, 8, 5, 1, 3, 10, 7, 6, 11)
vir.dat$Country <- factor(vir.dat$Country, levels = levels(vir.dat$Country)[geo.ord])


# vir.dat <- vir.dat[vir.dat$Country %in% c('Austria', 'Belgium', 'Czech Republic', 'Germany', 'Spain', 'France', 'Hungary', 'Italy',
#                                           'Luxembourg', 'Netherlands', 'Poland', 'Slovakia'), ]




# Create separate data frames for each season?
vir.dat.1011 <- vir.dat[(vir.dat$Year == 2010 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2011 & vir.dat$Week < 20), ]
vir.dat.1112 <- vir.dat[(vir.dat$Year == 2011 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2012 & vir.dat$Week < 20), ]
vir.dat.1213 <- vir.dat[(vir.dat$Year == 2012 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2013 & vir.dat$Week < 20), ]
vir.dat.1314 <- vir.dat[(vir.dat$Year == 2013 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2014 & vir.dat$Week < 20), ]
vir.dat.1415 <- vir.dat[(vir.dat$Year == 2014 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2015 & vir.dat$Week < 20), ]
vir.dat.1516 <- vir.dat[(vir.dat$Year == 2015 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2016 & vir.dat$Week < 20), ]
vir.dat.1617 <- vir.dat[(vir.dat$Year == 2016 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2017 & vir.dat$Week < 20), ]
vir.dat.1718 <- vir.dat[(vir.dat$Year == 2017 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2018 & vir.dat$Week < 20), ]
vir.dat.1819 <- vir.dat[(vir.dat$Year == 2018 & vir.dat$Week >= 40) |
                          (vir.dat$Year == 2019 & vir.dat$Week < 20), ]
# missing 17-18!!

# Recalculate week numbers
vir.dat.1011$Week[vir.dat.1011$Week < 40] <- vir.dat.1011$Week[vir.dat.1011$Week < 40] + max(vir.dat.1011$Week)
vir.dat.1112$Week[vir.dat.1112$Week < 40] <- vir.dat.1112$Week[vir.dat.1112$Week < 40] + max(vir.dat.1112$Week)
vir.dat.1213$Week[vir.dat.1213$Week < 40] <- vir.dat.1213$Week[vir.dat.1213$Week < 40] + max(vir.dat.1213$Week)
vir.dat.1314$Week[vir.dat.1314$Week < 40] <- vir.dat.1314$Week[vir.dat.1314$Week < 40] + max(vir.dat.1314$Week)
vir.dat.1415$Week[vir.dat.1415$Week < 40] <- vir.dat.1415$Week[vir.dat.1415$Week < 40] + max(vir.dat.1415$Week)
vir.dat.1516$Week[vir.dat.1516$Week < 40] <- vir.dat.1516$Week[vir.dat.1516$Week < 40] + max(vir.dat.1516$Week)
vir.dat.1617$Week[vir.dat.1617$Week < 40] <- vir.dat.1617$Week[vir.dat.1617$Week < 40] + max(vir.dat.1617$Week)
vir.dat.1718$Week[vir.dat.1718$Week < 40] <- vir.dat.1718$Week[vir.dat.1718$Week < 40] + max(vir.dat.1718$Week)
vir.dat.1819$Week[vir.dat.1819$Week < 40] <- vir.dat.1819$Week[vir.dat.1819$Week < 40] + max(vir.dat.1819$Week)

vir.dat.1011$Year <- NULL; vir.dat.1112$Year <- NULL; vir.dat.1213$Year <- NULL
vir.dat.1314$Year <- NULL; vir.dat.1415$Year <- NULL; vir.dat.1516$Year <- NULL
vir.dat.1617$Year <- NULL; vir.dat.1718$Year <- NULL; vir.dat.1819$Year <- NULL

# Create list of data frames
seasons <- list(vir.dat.1011, vir.dat.1112, vir.dat.1213, vir.dat.1314,
                vir.dat.1415, vir.dat.1516, vir.dat.1617, vir.dat.1819)

pdf('/Users/sarahkramer/Desktop/Lab/spatial_transmission/WHO_data/type-subtype_Europe_111519.pdf',
    width = 8, height = 8)
# Melt and plot
for (i in 1:8) {
  if (i == 1) {
    season <- '2010-11'
  } else if (i == 2) {
    season <- '2011-12'
  } else if (i == 3) {
    season <- '2012-13'
  } else if (i == 4) {
    season <- '2013-14'
  } else if (i == 5) {
    season <- '2014-15'
  } else if (i == 6) {
    season <- '2015-16'
  } else if (i == 7) {
    season <- '2016-17'
  }
  
  vir.temp <- seasons[[i]]
  dat.temp <- melt(vir.temp, id.vars = c('Country', 'Week'))
  dat.temp <- dat.temp[!is.na(dat.temp$value), ]
  p1 <- ggplot(dat.temp, aes(x = Week, y = value, col = variable, group = variable)) +
    geom_line() + geom_point(size = 0.5) + facet_wrap(~ Country) +
    labs(x = 'Week', y = '% of Tests', col = 'Type/Subtype', title = season) +
    theme_bw() + scale_color_manual(values = c('#d73027', '#d95f02', '#e6ab02', '#1a9850'))
  print(p1)
}
dev.off()




















