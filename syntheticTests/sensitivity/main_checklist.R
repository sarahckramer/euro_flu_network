
### Determine main driving forces of model patterns
# [x] Generate synthetic outbreaks where all compartments have same proportion S0/I0
      # (Controls for differences in immunity driving patterns)
# [-] Run synthetic models with a range of all five (L, D, R0mx, R0mn, airScale) parameters
      # Look at sensitivity to each parameter
# [x] Generate synthetic outbreaks without humidity forcing (L, D, R0 (constant), airScale)
      # Allows us to determine whether humidity is the main driving force of the patterns we see
# [] Generate synthetic outbreaks with only air/only commuting/ no travel... ### Not urgent ###
      # [] W/ humidity forcing
      # [] W/o humidity forcing
# 


# # 1: S0/I0 var // AH-forced // Both travel
# # 2: S0/I0 same // AH-forced // Both travel (and grid sensitivity search)
# # 3: S0/I0 same // not forced // Both travel (and grid sensitivity search)
# # 3.5: S0/I0 var // not forced // Both travel (and grid sensitivity search)
# 
# # And if we want to assess role of travel:
# # 4: S0/I0 same // AH-forced // Air only
# # 5: S0/I0 same // not forced // Air only
# # 6: S0/I0 same // AH-forced // Commuting only
# # 7: S0/I0 same // not forced // Commuting only
# # 8: S0/I0 same // AH-forced // No travel
# # 9: S0/I0 same // not forced // No travel

# 1: S0/I0 var // AH-forcing // Both travel
# 2: AH-forcing // Both travel
# 3: S0/I0 var // Both travel

# 4: S0/I0 var // AH-forcing
    # 4.1: No travel; 4.2: Air only; 4.3: Commuting only
# 5: S0/I0 var
    # 5.1: No travel; 5.2: Air only; 5.3: Commuting only
# 6: AH-forcing
  # 6.1: No travel; 6.2: Air only; 6.3: Commuting only
# 7: Doesn't make sense to do travel alone with no AH-forcing/S0I0-var - then outbreak has almost same states/same parameters for all countries



