
# Read in parameter ranges and initial S/I:
load('syntheticTests/syntheticData/params_1000_redS1.Rdata')
load('syntheticTests/syntheticData/S0_1000_redS1.Rdata')
load('syntheticTests/syntheticData/I0_1000_redS1.Rdata')

parms.real <- parms.list[[3]]
parms.we <- parms.list[[4]]
parms.not <- parms.list[[5]]

s0.real <- s0.list[[3]]
s0.we <- s0.list[[4]]
s0.not <- s0.list[[5]]

i0.real <- i0.list[[3]]
i0.we <- i0.list[[4]]
i0.not <- i0.list[[5]]

# Reformat matrices into data frames:
rownames(parms.real) = rownames(parms.we) = rownames(parms.not) = c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')

parms.real <- melt(parms.real)
names(parms.real) <- c('param', 'index', 'value')
parms.real$group <- 'Realistic'

parms.we <- melt(parms.we)
names(parms.we) <- c('param', 'index', 'value')
parms.we$group <- 'West-to-East'

parms.not <- melt(parms.not)
names(parms.not) <- c('param', 'index', 'value')
parms.not$group <- 'Not Realistic'

parms.df <- rbind(parms.real, parms.we, parms.not)
parms.df$group <- factor(parms.df$group)

rownames(s0.real) = rownames(s0.we) = rownames(s0.not) = rownames(i0.real) = rownames(i0.we) = rownames(i0.not) =
  c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

s0.real <- melt(s0.real); names(s0.real) <- c('country', 'index', 'value'); s0.real$group <- 'Realistic'
s0.we <- melt(s0.we); names(s0.we) <- c('country', 'index', 'value'); s0.we$group <- 'West-to-East'
s0.not <- melt(s0.not); names(s0.not) <- c('country', 'index', 'value'); s0.not$group <- 'Not Realistic'

i0.real <- melt(i0.real); names(i0.real) <- c('country', 'index', 'value'); i0.real$group <- 'Realistic'
i0.we <- melt(i0.we); names(i0.we) <- c('country', 'index', 'value'); i0.we$group <- 'West-to-East'
i0.not <- melt(i0.not); names(i0.not) <- c('country', 'index', 'value'); i0.not$group <- 'Not Realistic'

s0.df <- rbind(s0.real, s0.we, s0.not)
s0.df$group <- factor(s0.df$group)

i0.df <- rbind(i0.real, i0.we, i0.not)
i0.df$group <- factor(i0.df$group)

pdf('syntheticTests/outputs/explore/param_explore_1D.pdf', width = 15, height = 12)
# Plot histograms of each parameter value for: onset/real; onset/real/w-e; neither:
p1 <- ggplot(data = parms.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_grid(group ~ param, scales = 'free') + theme_bw() + labs(x = 'Value', y = 'Density')
# print(p1)

# Plot histograms of S0/I0 by country for each group:
p2 <- ggplot(data = s0.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_grid(group ~ country) + theme_bw() + labs(x = 'S0', y = 'Density')
# print(p2)

p3 <- ggplot(data = i0.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_grid(group ~ country) + theme_bw() + labs(x = 'I0', y = 'Density')
# print(p3)

# Plot violin plots for all three groups:
p4 <- ggplot(data = parms.df, aes(x = group, y = value)) + geom_violin(fill = 'lightblue', draw_quantiles = c(0.25, 0.5, 0.75)) + theme_classic() + 
  facet_wrap(~ param, scales = 'free_y') + labs(x = 'Group', y = 'Value') + geom_jitter(height = 0, width = 0.1, pch = 20, col = 'darkblue', size = 2.0)
print(p4)

# Same, for S0/I0 by country:
p5 <- ggplot(data = s0.df, aes(x = group, y = value)) + geom_violin(fill = 'lightblue', draw_quantiles = c(0.25, 0.5, 0.75)) + theme_classic() +
  facet_wrap(~ country, scales = 'free_y') + labs(x = 'Group', y = 'S0') + geom_jitter(height = 0, width = 0.1, pch = 20, col = 'darkblue', size = 2.0)
print(p5)
p6 <- ggplot(data = i0.df, aes(x = group, y = value)) + geom_violin(fill = 'lightblue', draw_quantiles = c(0.25, 0.5, 0.75)) + theme_classic() +
  facet_wrap(~ country, scales = 'free_y') + labs(x = 'Group', y = 'I0') + geom_jitter(height = 0, width = 0.1, pch = 20, col = 'darkblue', size = 2.0)
print(p6)
dev.off()

# Assess partial rank correlations:
library(ppcor)

# realistic:
parms.real <- t(parms.list[[3]]); colnames(parms.real) <- c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')
m <- pcor(parms.real, method = 'spearman')
sig.corr <- which(m$p.value < 0.005 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.real)[sig.corr[, 1]], colnames(parms.real)[sig.corr[, 2]])
print(colnames(parms.real)[unique(sig.corr[, 1])])
# S0_mean, D, R0mx, R0diff all sig. related to each other

sig.corr <- which(m$p.value < 0.05 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.real)[sig.corr[, 1]], colnames(parms.real)[sig.corr[, 2]])
print(colnames(parms.real)[unique(sig.corr[, 1])])
# this adds S0_sd in - sig related to R0mx

# west-east only:
parms.we <- t(parms.list[[4]]); colnames(parms.we) <- c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')
m <- pcor(parms.we, method = 'spearman')
sig.corr <- which(m$p.value < 0.005 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.we)[sig.corr[, 1]], colnames(parms.we)[sig.corr[, 2]])
print(colnames(parms.we)[unique(sig.corr[, 1])])
# S0_mean, D, R0mx, R0diff all sig. related to each other

sig.corr <- which(m$p.value < 0.05 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.we)[sig.corr[, 1]], colnames(parms.we)[sig.corr[, 2]])
print(colnames(parms.we)[unique(sig.corr[, 1])])
# nothing added

# not realistic:
parms.not <- t(parms.list[[5]]); colnames(parms.not) <- c('S0_mean', 'S0_sd', 'L', 'D', 'R0mx', 'R0diff', 'airScale')
m <- pcor(parms.not, method = 'spearman')
sig.corr <- which(m$p.value < 0.005 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.not)[sig.corr[, 1]], colnames(parms.not)[sig.corr[, 2]])
print(colnames(parms.not)[unique(sig.corr[, 1])])
# R0mx/R0diff correlated with S0_mean/S0_sd, but no relationships between S0_mean/S0_sd or R0mx/R0diff

sig.corr <- which(m$p.value < 0.05 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.not)[sig.corr[, 1]], colnames(parms.not)[sig.corr[, 2]])
print(colnames(parms.not)[unique(sig.corr[, 1])])
# now all related to each other

# Plot out 2-dimensional correlations of parameters:
# Interested both in whether correlations exist, and in if these correlations differ by group
# for (group in levels(parms.df$group)) {
#   print(group)
#   par(mfrow = c(7, 3), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
#   df.temp <- parms.df[parms.df$group == group, ]
#   
#   for (ix in 1:6) {
#     for (jx in (ix + 1):7) {
#       
#       parm1 <- levels(parms.df$param)[ix]
#       parm2 <- levels(parms.df$param)[jx]
#       
#       plot(df.temp$value[df.temp$param == parm1], df.temp$value[df.temp$param == parm2], pch = 20, xlab = parm1, ylab = parm2, main = group)
#       print(paste(parm1, parm2))
#       print(cor.test(df.temp$value[df.temp$param == parm1], df.temp$value[df.temp$param == parm2], method = 'spearman'))
#       
#     }
#   }
#   
# }
# not: S0_mean/R0mx (neg); S0_mean/R0diff (pos); S0_sd/R0mx (pos) & S0_sd/R0diff (neg); L/R0diff (marginal; neg)

# realistic: S0_mean/S0_sd (pos); S0_mean/D (pos); S0_mean/R0mx (neg) & S0_mean/R0diff (pos); S0_sd/R0mx (neg); L/D & L/R0diff (marginal; neg);
# D/R0mx (pos); D/R0diff (marginal; neg); R0mx/R0diff (pos); R0mx/airScale (marginal; pos)

# w-e: S0_mean/D (marginal; pos); S0_mean/R0mx (neg); S0_sd/L (neg)
# note: very few runs, obviously

# same relationship between S0_mean and R0mx/R0diff for both not and realistic, but opposite relationship for S0_sd/R0mx; S0mean/R0mx also for w-e; S0_sd/L? (w-e only)

pdf('syntheticTests/outputs/explore/param_explore_2D.pdf', width = 15, height = 12)
# Repeat, w/o considering L or airScale - likely not super important; use Bonferroni

for (group in levels(parms.df$group)) {
  print(group)
  par(mfrow = c(5, 5), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
  df.temp <- parms.df[parms.df$group == group, ]
  
  for (parm1 in levels(parms.df$param)[c(1:2, 4:6)]) {
    for (parm2 in levels(parms.df$param)[c(1:2, 4:6)]) {
      
      plot(df.temp$value[df.temp$param == parm1], df.temp$value[df.temp$param == parm2], pch = 20, xlab = parm1, ylab = parm2, main = group)
      print(paste(parm1, parm2))
      print(cor.test(df.temp$value[df.temp$param == parm1], df.temp$value[df.temp$param == parm2], method = 'spearman')) # want < 0.05/10 = 0.005
      
    }
  }
  
}
# Not Realistic: S0_mean/R0mx (neg); S0_sd/R0mx (pos); S0_sd/R0diff (neg)
# Realistic: S0_mean/R0mx (neg); (D seems to be lowest (trend) when S0mean and R0mx are both low, which rarely happens)
# West-to-East: S0_mean/R0mx (neg)

# The only consistent and strong relationship is between S0_mean and R0mx, and it's consistently negative
# But also worth exploring the relationships between S0_sd and R0mx/R0diff, and how they change by group
# And of course see what D is up to when other variables are covarying

# Plot out 2- and 3-dimensional heatplots to explore covariability:
# For 2d: all combos of S0mean/D/R0mx/R0diff; also S0sd with R0mx/R0diff; for all groups; color is frequency
# For 3d:
# Maybe limit this to a few combination of interest - first, how to all params vary with S0mean/R0mx (only consistent and strong bivariate relationship)
# Explore how S0mean, D, R0mx, and R0diff change given the relationship between any 2 of them (12 plots?) (for all 3 groups)

parms.df <- dcast(parms.df, index + group ~ param)

calc_z_for_heatmap <- function(df, p1, p2, p3 = NA) {
  parm.levels1 <- seq(min(df[, p1]), max(df[, p1]), length.out = 11)
  parm.levels2 <- seq(min(df[, p2]), max(df[, p2]), length.out = 11)
  
  parms.heat <- vector('list', 3)
  for (i in 1:3) {
    group <- levels(df$group)[i]
    df.temp <- df[df$group == group, ]
    
    # parm.levels1 <- quantile(df.temp[, p1], seq(0, 1, by = 0.1))
    # parm.levels2 <- quantile(df.temp[, p2], seq(0, 1, by = 0.1))
    
    df.heat <- NULL
    for (ix in 1:10) {
      for (jx in 1:10) {
        df.temp2 <- df.temp[(df.temp[, p1] >= parm.levels1[ix] & df.temp[, p1] <= parm.levels1[ix + 1]) &
                                    (df.temp[, p2] >= parm.levels2[jx] & df.temp[, p2] <= parm.levels2[jx + 1]), ]
        
        if (is.na(p3)) { # 2D
          if (length(df.temp2[, p1]) > 0) {
            df.heat <- rbind(df.heat, c(ix, jx, length(df.temp2[, p1]) / dim(df.temp)[1]))
          } else {
            df.heat <- rbind(df.heat, c(ix, jx, 0))
          }
          
        } else { # 3D
          if (length(df.temp2[, p3]) > 0) {
            df.heat <- rbind(df.heat, c(ix, jx, mean(df.temp2[, p3])))
          } else {
            df.heat <- rbind(df.heat, c(ix, jx, NA))
          }
        }
        
        
        
      }
    }
    
    df.heat <- as.data.frame(df.heat)
    names(df.heat) <- c('x', 'y', 'z')
    parms.heat[[i]] <- df.heat
  }
  
  if (is.na(p3)) {
    plot1 <- ggplot(data = parms.heat[[1]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(0, max(c(parms.heat[[1]]$z, parms.heat[[2]]$z, parms.heat[[3]]$z)))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = 'Freq.', title = 'Not Realistic')
    plot2 <- ggplot(data = parms.heat[[2]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(0, max(c(parms.heat[[1]]$z, parms.heat[[2]]$z, parms.heat[[3]]$z)))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = 'Freq.', title = 'Realistic')
    plot3 <- ggplot(data = parms.heat[[3]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(0, max(c(parms.heat[[1]]$z, parms.heat[[2]]$z, parms.heat[[3]]$z)))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = 'Freq.', title = 'West-to-East')
  } else {
    plot1 <- ggplot(data = parms.heat[[1]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(min(df[, p3]), max(df[, p3]))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = p3, title = 'Not Realistic')
    plot2 <- ggplot(data = parms.heat[[2]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(min(df[, p3]), max(df[, p3]))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = p3, title = 'Realistic')
    plot3 <- ggplot(data = parms.heat[[3]]) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(min(df[, p3]), max(df[, p3]))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = p3, title = 'West-to-East')
  }
  
  return(list(plot1, plot2, plot3))
}

# 2D:
plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'D')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx')
plots3 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots3[[1]], plots1[[2]], plots2[[2]], plots3[[2]], plots1[[3]], plots2[[3]], plots3[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'D', 'R0mx')
plots2 <- calc_z_for_heatmap(parms.df, 'D', 'R0diff')
plots3 <- calc_z_for_heatmap(parms.df, 'R0mx', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots3[[1]], plots1[[2]], plots2[[2]], plots3[[2]], plots1[[3]], plots2[[3]], plots3[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'S0_sd')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_sd', 'R0mx')
plots3 <- calc_z_for_heatmap(parms.df, 'S0_sd', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots3[[1]], plots1[[2]], plots2[[2]], plots3[[2]], plots1[[3]], plots2[[3]], plots3[[3]])
dev.off()

# 3D:
pdf('syntheticTests/outputs/explore/param_explore_3D.pdf', width = 15, height = 12)
plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx', 'D')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'D', 'R0mx')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'D', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0diff', 'D')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0diff', 'R0mx')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'D', 'R0mx', 'S0_mean')
plots2 <- calc_z_for_heatmap(parms.df, 'D', 'R0mx', 'R0diff')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'D', 'R0diff', 'S0_mean')
plots2 <- calc_z_for_heatmap(parms.df, 'D', 'R0diff', 'R0mx')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'R0mx', 'R0diff', 'S0_mean')
plots2 <- calc_z_for_heatmap(parms.df, 'R0mx', 'R0diff', 'D')
grid.arrange(plots1[[1]], plots2[[1]], plots1[[2]], plots2[[2]], plots1[[3]], plots2[[3]])

plots1 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx', 'S0_sd')
plots2 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx', 'L')
plots3 <- calc_z_for_heatmap(parms.df, 'S0_mean', 'R0mx', 'airScale')
grid.arrange(plots1[[1]], plots2[[1]], plots3[[1]], plots1[[2]], plots2[[2]], plots3[[2]], plots1[[3]], plots2[[3]], plots3[[3]])
dev.off()

# For each country, partial rank correlation for S0 vs R0mx/R0diff/D/L/airScale
s0.df <- merge(s0.df, parms.df, by = c('index', 'group'))
i0.df <- merge(i0.df, parms.df, by = c('index', 'group'))

for (group in levels(s0.df$group)) {
  print(group)
  for (country in levels(s0.df$country)) {
    print(country)
    df.temp <- s0.df[s0.df$group == group & s0.df$country == country, 4:11]
    m <- pcor(df.temp, method = 'spearman')
    sig.corr <- which(m$p.value[1, 3:8] < 0.005)
    print(names(sig.corr))
    sig.corr <- which(m$p.value[1, 3:8] < 0.05)
    print(names(sig.corr))
    print('')
  }
  print('')
}
# Not realistic: BE/R0diff; DE/S0_sd; SK/R0mx(!)
# Realistic: CZ/R0mx(!), CZ/L; DE/D; PL/airScale; ES/D(!), ES/R0mx(!), ES/R0diff
# West-to-East: CZ/L, CZ/D, CZ/R0mx; FR/S0_sd; PL/airScale(!); SK/airScale(!), SK/R0mx, SK/D; ES/D, ES/R0mx

# group <- 'Realistic'; country <- 'ES'
# plot(s0.df$R0diff[s0.df$group == group & s0.df$country == country],
#      s0.df$value[s0.df$group == group & s0.df$country == country],
#      pch = 20, ylab = 'S0', xlab = 'Param', main = country)

# CZ: higher S0 when R0mx is lower; less strong, but higher S0 when L is higher
# PL: higher S0 when airScale is higher - ? (but very slight increase)
# SK: higher S0 when R0mx is lower (only strongly sig for not realistic, though - so it needs this regardless of realism - why correlated here?)(tendendy to leave out where R0mx low and S0 high?)
# SK: (w-e only) higher S0 when airScale is lower - otherwise can't efficiently reach?
# ES: higher S0 when D is higher, and when R0mx is lower

# Perhaps when R0mx is high, CZ must be lower to avoid instant outbreak?; for SK and ES relationship might just help prevent no-onset situations
# Can't puzzle out all of these, and some are probably unimportant; most of the relationship are fairly weak overall





