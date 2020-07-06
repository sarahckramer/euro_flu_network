### Format raw virologic and syndromic data, and calculate (sub)type-specific syndromic+ data ###

########################################################################################################################################################################
# Read in syndromic data:
syn.dat <- read.csv('data/synDatCounts_060519.csv')

########################################################################################################################################################################
# Read in original virological data and format:

setwd('E://Lab/spatial_transmission/WHO_data/data_4-12-18/')
v1 <- read.csv('FluNetInteractiveReport1.csv', header = FALSE)
v2 <- read.csv('FluNetInteractiveReport2.csv', header = FALSE)

setwd('E://Lab/spatial_transmission/WHO_data/17-18/')
v3 <- read.csv('FluNetInteractiveReport.csv', header = FALSE)

setwd('E://Lab/spatial_transmission/WHO_data/data_8-05-19_TEMP/')
v4 <- read.csv('FluNetInteractiveReport.csv', header = FALSE)

setwd('../../EuropeanNetwork/')

colnames(v1) <- as.character(unlist(v1[3, ]))
v1 <- v1[-c(1:3), ]
rownames(v1) <- as.numeric(rownames(v1)) - 3

colnames(v2) <- as.character(unlist(v2[3, ]))
v2 <- v2[-c(1:3), ]
rownames(v2) <- as.numeric(rownames(v2)) - 3

colnames(v3) <- as.character(unlist(v3[3, ]))
v3 <- v3[-c(1:3), ]
rownames(v3) <- as.numeric(rownames(v3)) - 3

colnames(v4) <- as.character(unlist(v4[3, ]))
v4 <- v4[-c(1:3), ]
rownames(v4) <- as.numeric(rownames(v4)) - 3

v1 <- v1[, c(1, 4:5, 8:12, 14:15, 19:20)]; v2 <- v2[, c(1, 4:5, 8:12, 14:15, 19:20)]; v3 <- v3[, c(1, 4:5, 8:12, 14:15, 19:20)]; v4 <- v4[, c(1, 4:5, 8:12, 14:15, 19:20)]
vir.dat <- rbind(v1, v2, v3, v4)
rm(v1, v2, v3, v4)

# Continue to format appropriately
for (i in 2:12) {
  vir.dat[, i] <- as.numeric(as.character(vir.dat[, i]))
}; rm(i)

# Limit to countries in syn.dat:
vir.dat$Country <- as.character(vir.dat$Country)
vir.dat$Country[vir.dat$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'
vir.dat$Country <- factor(vir.dat$Country)
countries <- c('Austria', 'Belgium', 'Croatia', 'Czechia', 'Denmark', 'Germany', 'Spain', 'France', 'Hungary', 'Iceland', 'Ireland',
               'Italy', 'Luxembourg', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Sweden', 'United Kingdom')
vir.dat <- vir.dat[vir.dat$Country %in% countries,]
vir.dat$Country <- factor(vir.dat$Country)

# Determine denominators
vir.dat$SPEC_PROCESSED_NB[is.na(vir.dat$SPEC_PROCESSED_NB) & !is.na(vir.dat$SPEC_RECEIVED_NB)] <-
  vir.dat$SPEC_RECEIVED_NB[is.na(vir.dat$SPEC_PROCESSED_NB) & !is.na(vir.dat$SPEC_RECEIVED_NB)]
vir.dat$SPEC_RECEIVED_NB <- NULL

# Calculate proportion positive at each time point for each strain
vir.dat$SPEC_PROCESSED_NB[vir.dat$SPEC_PROCESSED_NB == 0 & !is.na(vir.dat$SPEC_PROCESSED_NB)] <- NA
vir.dat <- vir.dat[!is.na(vir.dat$SPEC_PROCESSED_NB), ]
for (i in 5:11) {
  vir.dat[, i] <- vir.dat[, i] / vir.dat$SPEC_PROCESSED_NB
}

# Remove any proportions above 1
for (i in 5:11) {
  vir.dat[, i][vir.dat[, i] > 1 & !is.na(vir.dat[, i])] <- NA
}

# Remove denominator column
vir.dat$SPEC_PROCESSED_NB <- NULL

# Combine H1N1 columns:
ah.all <- rep(NA, dim(vir.dat)[1])
for (i in 1:dim(vir.dat)[1]) {
  if (!(is.na(vir.dat$AH1[i]) & is.na(vir.dat$AH1N12009[i]))) {
    ah.all[i] <- sum(vir.dat$AH1[i], vir.dat$AH1N12009[i], na.rm = T)
  }
}
vir.dat$AH1 <- ah.all; vir.dat$AH1N12009 <- NULL

########################################################################################################################################################################
# Allocate non-subtyped A proportionally:

# For each country/year/week, what proportion of subtyped A are H1 vs. H3?
subtyped.A.all <- rep(NA, dim(vir.dat)[1])
for (i in 1:dim(vir.dat)[1]) {
  if (!(is.na(vir.dat$AH1[i]) & is.na(vir.dat$AH3[i]))) {
    subtyped.A.all[i] <- sum(vir.dat$AH1[i], vir.dat$AH3[i], na.rm = T)
  }
}

# If no subtyped A, label as NA
subtyped.A.all[subtyped.A.all == 0 & !is.na(subtyped.A.all)] <- NA

# Add this as a column
vir.dat$ASUBTYPED <- subtyped.A.all
all.equal(vir.dat$ANOTSUBTYPED+vir.dat$ASUBTYPED, vir.dat$INF_A) # different number of NAs, but checks out for actual numbers

# This will remove normally useable data for country/year/weeks where there are unsubtyped A, but no subtyped A - how often does this occur?
vir.dat.lost <- vir.dat[vir.dat$ANOTSUBTYPED > 0 & !is.na(vir.dat$ANOTSUBTYPED) & is.na(vir.dat$ASUBTYPED), ]
# 541 out of 9044 observations... will need to set H1 and H3 here to NA - don't want them to be read as zeros, since that's not really what's happening
# high counts in Denmark, Sweden, Poland, but some everywhere
# no strong pattern by year
# more likely to happen in weeks 16-20, so near end of season anyway
# how many within 40-15?: 248
# could also allocate evenly to potentially conserve data, but that doesn't seem to be a valid assumption
   # allocate based on proportions throughout whole season in that country?
rm(vir.dat.lost)

vir.dat$AH1[vir.dat$ANOTSUBTYPED > 0 & !is.na(vir.dat$ANOTSUBTYPED) & is.na(vir.dat$ASUBTYPED)] <- NA
vir.dat$AH3[vir.dat$ANOTSUBTYPED > 0 & !is.na(vir.dat$ANOTSUBTYPED) & is.na(vir.dat$ASUBTYPED)] <- NA

# For remaining, allocate the not subtyped As accordingly:
# Get proportion of A_subtyped that are each subtype
vir.dat$AH1.prop <- vir.dat$AH1 / vir.dat$ASUBTYPED
vir.dat$AH3.prop <- vir.dat$AH3 / vir.dat$ASUBTYPED
summary((vir.dat$AH1.prop + vir.dat$AH3.prop)) # good, these sum to 1

summary(vir.dat$ANOTSUBTYPED[is.na(vir.dat$AH1.prop) & !is.na(vir.dat$AH1)]) # so if the props are NA, either so are the AH1/AH3 columns, or else there are no non-subtyped to allocate

# Let's split the dfs for easier processing
vir.dat.nosplit <- vir.dat[is.na(vir.dat$ASUBTYPED), ]
vir.dat <- vir.dat[!is.na(vir.dat$ASUBTYPED), ]

# Also some where not.subtyped is 0 - so none to allocate and nothing to do
vir.dat.noneToAllocate <- vir.dat[vir.dat$ANOTSUBTYPED == 0 | is.na(vir.dat$ANOTSUBTYPED), ]
vir.dat <- vir.dat[vir.dat$ANOTSUBTYPED > 0 & !is.na(vir.dat$ANOTSUBTYPED), ]

# Still some NAs in H1.prop and H3.prop - how to handle?
vir.dat[is.na(vir.dat$AH1.prop), ]
vir.dat[is.na(vir.dat$AH3.prop), ]
# never are they both NA; just allocate all non-subtyped to non-NA subtype

# Go through and calculate how much non-subtyped goes to each subtype:
vir.dat$nonH1 <- vir.dat$ANOTSUBTYPED * vir.dat$AH1.prop
vir.dat$nonH3 <- vir.dat$ANOTSUBTYPED * vir.dat$AH3.prop
# same numbers of NAs as prop columns
all.equal(vir.dat$nonH1 + vir.dat$nonH3, vir.dat$ANOTSUBTYPED) # check

# Now add these in to the H1/H3 columns:
vir.dat$AH1 <- vir.dat$AH1 + vir.dat$nonH1
vir.dat$AH3 <- vir.dat$AH3 + vir.dat$nonH3
all.equal(vir.dat$AH1 + vir.dat$AH3, vir.dat$INF_A) # check

# Merge back together:
vir.dat <- vir.dat[, 1:12]
vir.dat <- rbind(vir.dat, vir.dat.noneToAllocate, vir.dat.nosplit)

# Reduce:
vir.dat <- vir.dat[, c(1:5, 7:9)]

# Some where ASUBTYPED is NA but H1/H3/not subtyped all 0 - these are genuine 0s, keep!

# Need to remove where sum is greater than 1 (ALL_INF is NA):
vir.dat$AH1[is.na(vir.dat$ALL_INF)] <- NA
vir.dat$AH3[is.na(vir.dat$ALL_INF)] <- NA
vir.dat$INF_A[is.na(vir.dat$ALL_INF)] <- NA
vir.dat$INF_B[is.na(vir.dat$ALL_INF)] <- NA

########################################################################################################################################################################
# Format for multiplying:
# A, B, H1N1, H3N2; all (as check)

library(reshape2)
library(stringr)

vir.dat$Week <- as.factor(vir.dat$Week)
levels(vir.dat$Week) <- str_pad(levels(vir.dat$Week), 2, pad = '0')

vir.dat$time <- paste(vir.dat$Year, vir.dat$Week, sep = '_')
vir.dat$time <- factor(vir.dat$time)

flip.vir.dat <- dcast(vir.dat, time ~ Country, fill = (-1), value.var = 'ALL_INF')
flip.vir.dat$time <- factor(flip.vir.dat$time)

flip.vir.dat.A <- dcast(vir.dat, time ~ Country, fill = (-1), value.var = 'INF_A')
flip.vir.dat.A$time <- factor(flip.vir.dat.A$time)

flip.vir.dat.AH1 <- dcast(vir.dat, time ~ Country, fill = (-1), value.var = 'AH1')
flip.vir.dat.AH1$time <- factor(flip.vir.dat.AH1$time)

flip.vir.dat.AH3 <- dcast(vir.dat, time ~ Country, fill = (-1), value.var = 'AH3')
flip.vir.dat.AH3$time <- factor(flip.vir.dat.AH3$time)

flip.vir.dat.B <- dcast(vir.dat, time ~ Country, fill = (-1), value.var = 'INF_B')
flip.vir.dat.B$time <- factor(flip.vir.dat.B$time)

# And restrict to same time period:
flip.list <- list(flip.vir.dat, flip.vir.dat.A, flip.vir.dat.AH1, flip.vir.dat.AH3, flip.vir.dat.B)
for (i in 1:5) {
  flip.list[[i]] <- flip.list[[i]][15:509, ]
  flip.list[[i]][, 2:22][flip.list[[i]][, 2:22] < 0] <- NA
}
flip.vir.dat <- flip.list[[1]]
flip.vir.dat.A <- flip.list[[2]]
flip.vir.dat.AH1 <- flip.list[[3]]
flip.vir.dat.AH3 <- flip.list[[4]]
flip.vir.dat.B <- flip.list[[5]]

# ########################################################################################################################################################################
# # Save positivity data for use in OEV calculations:
# for (i in 1:5) {
#   flip.list[[i]][, 2:22][is.na(flip.list[[i]][, 2:22])] <- -1
# }
# flip.vir.dat.A <- flip.list[[2]]
# flip.vir.dat.AH1 <- flip.list[[3]]
# flip.vir.dat.AH3 <- flip.list[[4]]
# flip.vir.dat.B <- flip.list[[5]]
# 
# # I think removing all the "low-quality" data is not important here b/c these will already be NAs in syn.dat?
#     # Oh, actually, no they're not
#     # I wonder if they were correctly removed for the forecasts with the full data?
# # But since these points are NA in the "data" (syndromic+), it doesn't matter if the OEV value has a value
# write.csv(flip.vir.dat.A, file = 'data/by_subtype/posprop_A(all).csv', row.names = FALSE)
# write.csv(flip.vir.dat.AH1, file = 'data/by_subtype/posprop_A(H1).csv', row.names = FALSE)
# write.csv(flip.vir.dat.AH3, file = 'data/by_subtype/posprop_A(H3).csv', row.names = FALSE)
# write.csv(flip.vir.dat.B, file = 'data/by_subtype/posprop_B.csv', row.names = FALSE)
# 
# # # Turn -1 back into NA for calculations:
# # for (i in 1:5) {
# #   flip.list[[i]][, 2:22][flip.list[[i]][, 2:22] < 0] <- NA
# # }
# # flip.vir.dat.A <- flip.list[[2]]
# # flip.vir.dat.AH1 <- flip.list[[3]]
# # flip.vir.dat.AH3 <- flip.list[[4]]
# # flip.vir.dat.B <- flip.list[[5]]
# # # or just restart and ignore this section...

########################################################################################################################################################################
# Multiply by syndromic data:
# A, B, H1N1, H3N2; all (as check)

# Syn.dat: replace -1 with NA:
syn.dat[, 2:22][syn.dat[, 2:22] == -1] <- NA

# First, check all:
flip.dat <- flip.vir.dat
for (i in 2:22) {
  flip.dat[, i] <- flip.dat[, i] * syn.dat[, i]
}

iliiso <- read.csv('data/WHO_data_05-09-19.csv')
iliiso[, 2:22][iliiso[, 2:22] < 0] <- NA
all.equal(iliiso, flip.dat, check.attributes = FALSE) # some minor differences where more NAs in iliiso - related to pandemic?
# AT, CZ, IT, PL, PT
plot(iliiso$Portugal, pch = 20)
points(flip.dat$Portugal, pch = 20, col = 'blue')
# AT: pandemic; CZ: outbreak that was too small and removed; IT: pandemic; PL: small outbreak; PT: pandemic

# Remove where pandemic was the difference (AT, IT, PT):
at.remove <- which(is.na(iliiso$Austria) & !is.na(flip.dat$Austria))
it.remove <- which(is.na(iliiso$Italy) & !is.na(flip.dat$Italy))
pt.remove <- which(is.na(iliiso$Portugal) & !is.na(flip.dat$Portugal))

syn.dat$Austria[at.remove] <- NA
syn.dat$Italy[it.remove] <- NA
syn.dat$Portugal[pt.remove] <- NA

# For others (CZ, PL), keep for now, but see if they need to be removed later
    # Argument for removal might simply be that, in "all strain" model, these were seen as a sign of unreliable/unusually sparce data collection that year

# Following that, let's actually remove them:
cz.remove <- which(is.na(iliiso$Czechia) & !is.na(flip.dat$Czechia)) # 2013-14
pl.remove <- which(is.na(iliiso$Poland) & !is.na(flip.dat$Poland)) # 2011-12

syn.dat$Czechia[cz.remove] <- NA
syn.dat$Poland[pl.remove] <- NA

flip.dat <- flip.vir.dat
for (i in 2:22) {
  flip.dat[, i] <- flip.dat[, i] * syn.dat[, i]
}
all.equal(iliiso, flip.dat, check.attributes = FALSE)

# Multiply for all types/subtypes:
for (i in 2:22) {
  flip.vir.dat.A[, i] <- flip.vir.dat.A[, i] * syn.dat[, i]
  flip.vir.dat.AH1[, i] <- flip.vir.dat.AH1[, i] * syn.dat[, i]
  flip.vir.dat.AH3[, i] <- flip.vir.dat.AH3[, i] * syn.dat[, i]
  flip.vir.dat.B[, i] <- flip.vir.dat.B[, i] * syn.dat[, i]
}

########################################################################################################################################################################
# Save:
# write.csv(flip.vir.dat.A, file = 'data/WHO_data_A(all).csv', row.names = FALSE)
# write.csv(flip.vir.dat.AH1, file = 'data/WHO_data_A(H1).csv', row.names = FALSE)
# write.csv(flip.vir.dat.AH3, file = 'data/WHO_data_A(H3).csv', row.names = FALSE)
# write.csv(flip.vir.dat.B, file = 'data/WHO_data_B.csv', row.names = FALSE)

########################################################################################################################################################################
# Plot/explore:
# First limit to the countries we're currently interested in:
flip.dat <- flip.dat[, c(1:3, 5, 7:9, 12:15, 18, 20)]
flip.vir.dat.A <- flip.vir.dat.A[, c(1:3, 5, 7:9, 12:15, 18, 20)]
flip.vir.dat.AH1 <- flip.vir.dat.AH1[, c(1:3, 5, 7:9, 12:15, 18, 20)]
flip.vir.dat.AH3 <- flip.vir.dat.AH3[, c(1:3, 5, 7:9, 12:15, 18, 20)]
flip.vir.dat.B <- flip.vir.dat.B[, c(1:3, 5, 7:9, 12:15, 18, 20)]

# And limit to seasonal influenza:
flip.dat <- flip.dat[79:495 ,]
flip.vir.dat.A <- flip.vir.dat.A[79:495, ]
flip.vir.dat.AH1 <- flip.vir.dat.AH1[79:495, ]
flip.vir.dat.AH3 <- flip.vir.dat.AH3[79:495, ]
flip.vir.dat.B <- flip.vir.dat.B[79:495, ]

flip.list <- list(flip.dat, flip.vir.dat.A, flip.vir.dat.AH1, flip.vir.dat.AH3, flip.vir.dat.B)
plot.names <- c('All', 'A (All)', 'A (H1N1)', 'A (H3N2)', 'B')

# pdf('code/checks/analyzeDataRetro/outputs/plot_syndromic+_bySubtype_NEW.pdf', width = 15, height = 16)
# for (i in 1:length(flip.list)) {
#   
#   par(mfrow = c(12, 1), cex = 0.8, mar = c(2.5, 2.5, 2, 1), mgp = c(1.5, 0.5, 0))
#   # par(mfrow = c(6, 2), cex = 0.8, mar = c(1.5, 1.5, 1.5, 1), mgp = c(1.5, 0.5, 0))
#   for (j in 2:13) {
#     plot(flip.list[[i]][, j], pch = 20, cex = 0.7, type = 'b', main = names(flip.list[[i]])[j], xlab = 'Time', xaxt = 'n', ylab = plot.names[i])
#     abline(v = c(1, 53, 105, 157, 209, 261, 314, 366, 418))
#     # axis(side = 1, at = c(14, 66, 118, 170, 222, 275, 327, 379), labels = 2011:2018)
#     axis(side = 1, at = c(1, 53, 105, 157, 209, 261, 314, 366), labels = c('10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18'))
#   }
#   
# }
# dev.off()










