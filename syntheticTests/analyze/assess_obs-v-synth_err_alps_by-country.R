
# Note: Any of this is really only important if we need to prevent "collapse" - so find our best combos of oev_base/oev_denom/lambda first, then see if any countries in particular are causing issues
# Can also use this code to determine which oev_base/oev_denom/lambda combos lead to collapse

# Read in "alps":
alps <- read.csv('syntheticTests/outputs/cluster/071519/outputAlps.csv')
# alps <- alps[alps$oev_base != 1e6, ]

# Plot alps over each outbreak by country:
alps$group <- paste(alps$outbreak, alps$run, alps$oev_base, alps$oev_denom, alps$lambda, alps$country, sep = '_'); alps$group <- factor(alps$group)
# p1 <- ggplot(data = alps) + geom_line(aes(x = week, y = alp, group = group)) +
#   theme_classic() + labs(x = 'Week', y = 'alp') +
#   facet_wrap(~ country)
# p1
# all drop very low b/c some combos of oev_base/oev_denom/lambda don't fit well - loop through these at least

# Loop through OEV/lambda params:
for (oev1 in unique(alps$oev_base)) {
  for (oev2 in unique(alps$oev_denom)) {
    for (lam in unique(alps$lambda)) {
      print(paste(oev1, oev2, lam, sep = '_'))
      a.temp <- alps[alps$oev_base == oev1 & alps$oev_denom == oev2 & alps$lambda == lam, ]
      a.temp$group <- factor(a.temp$group); a.temp$outbreak <- factor(a.temp$outbreak)
      
      if (length(a.temp$country) > 0) {
        p2 <- ggplot(data = a.temp) + geom_line(aes(x = week, y = alp, group = group, col = outbreak)) +
          geom_hline(yintercept = 0.5) + geom_hline(yintercept = 0.3) +
          theme_classic() +
          labs(x = 'Week', y = 'alp', col = 'Outbreak', title = paste(oev1, oev2, lam, sep = '_')) +
          facet_wrap(~ country) + scale_color_brewer(palette = 'Set2')
        print(p2)
        
        p3 <- ggplot(data = a.temp) + geom_line(aes(x = week, y = alp, group = group, col = country)) +
          theme_classic() +
          labs(x = 'Week', y = 'alp', col = 'Outbreak', title = paste(oev1, oev2, lam, sep = '_')) +
          facet_wrap(~ outbreak) + scale_color_viridis(discrete = TRUE)
        print(p3)
      }
      
    }
  }
}
# Mostly HR, HU, SE, SI; but also AT, FR, RO, SK, UK; PL?

# Combos to keep: 1e4/5/1-1.01, 1e4/10/1-1.01, all 1e5
# 1e4/20/1.05 seems to work for outbreak 13 but none of the others?



# Troublesome: CZ, DE, DK, FR, HU, IT, PL, RO, SE, SI, SK, UK
# Might be more interesting to see which drops first

# Loop through each outbreak/run and see: (1) If any drop below 0.5/0.2; (2) Which do so; (3) Order in which countries do so
for (oev1 in unique(alps$oev_base)) {
  for (oev2 in unique(alps$oev_denom)) {
    for (lam in unique(alps$lambda)) {
      print(paste(oev1, oev2, lam, sep = '_'))
      
      for (out in unique(alps$outbreak)) {
        for (run in unique(alps$run)) {
          print(paste(out, run, sep = '_'))
          a.temp <- alps[alps$oev_base == oev1 & alps$oev_denom == oev2 & alps$lambda == lam & alps$outbreak == out & alps$run == run & alps$week > 5, ]
          
          # if (any(a.temp$alp < 0.5)) {
          #   print('Less than 0.5:')
          #   a.temp.small <- a.temp[a.temp$alp < 0.5, ]
          #   print(as.vector(unique(a.temp.small$country)))
          #   print(length(unique(a.temp.small$country)))
          #   
          #   print(as.vector(a.temp.small$country[a.temp.small$week == min(a.temp.small$week)]))
          #   print(a.temp.small[, c('country', 'week')])
          # }
          
          if (any(a.temp$alp < 0.2)) {
            print('Less than 0.2:')
            a.temp.small <- a.temp[a.temp$alp < 0.2, ]
            print(as.vector(unique(a.temp.small$country)))
            print(length(unique(a.temp.small$country)))
            
            print(as.vector(a.temp.small$country[a.temp.small$week == min(a.temp.small$week)]))
            print(a.temp.small[, c('country', 'week')])
          }
          
          print('')
        }
      }
      print(''); print(''); print('')
    }
  }
}
# HR, DE, HU, PT, SI, RO, AT, LU, PL, DK, FR, SE, UK, SK, IT, CZ, IE, UK (seems like almost any country can drive it - not sure much of a pattern)
# Eventually look at where collapses are occurring, and see what the patterns look like there

# Looks like SE begins to dip first, but the RO/SI/UK follow and drop even lower (but this is an example from only one outbreak!)


















