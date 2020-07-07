### Explore patterns in parameters that generated "realistic" synthetic outbreaks ###

# Read in parameter ranges and initial S/I:
load('src/syntheticTests/syntheticData/params_1000_070220.RData')
load('src/syntheticTests/syntheticData/S0_1000_070220.Rdata')
load('src/syntheticTests/syntheticData/I0_1000_070220.Rdata')

parms.real <- parms.list[[3]]
s0.real <- s0.list[[3]]
i0.real <- i0.list[[3]]

# Reformat matrices into data frames:
rownames(parms.real) <- c('L', 'D', 'R0mx', 'R0diff', 'airScale')

parms.real <- melt(parms.real)
names(parms.real) <- c('param', 'index', 'value')

rownames(s0.real) = rownames(i0.real) = c('AT', 'BE', 'CZ', 'FR', 'DE', 'HU', 'IT', 'LU', 'NL', 'PL', 'SK', 'ES')

s0.real <- melt(s0.real); names(s0.real) <- c('country', 'index', 'value')
i0.real <- melt(i0.real); names(i0.real) <- c('country', 'index', 'value')

parms.df <- as.data.frame(parms.real)
s0.df <- as.data.frame(s0.real)
i0.df <- as.data.frame(i0.real)

### 1D ###

pdf('src/syntheticTests/outputs/explore/param_explore_1D.pdf', width = 17, height = 12)

# Plot histograms of each parameter value for: onset/real; onset/real/w-e; neither:
p1 <- ggplot(data = parms.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_wrap(~ param, scales = 'free', ncol = 1) + theme_bw() + labs(x = 'Value', y = 'Density')
print(p1)

# Plot histograms of S0/I0 by country for each group:
p2 <- ggplot(data = s0.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_wrap(~ country) + theme_bw() + labs(x = 'S0', y = 'Density')
# print(p2)

p3 <- ggplot(data = i0.df) + geom_histogram(aes(x = value, y = ..ndensity..), fill = 'gray80', col = 'black', bins = 50) +
  facet_wrap(~ country) + theme_bw() + labs(x = 'I0', y = 'Density')
# print(p3)

# Violin plots:
p5 <- ggplot(data = s0.df, aes(x = country, y = value)) + geom_violin(fill = 'lightblue', draw_quantiles = c(0.25, 0.5, 0.75)) + theme_classic() +
  labs(x = 'Group', y = 'S0') + geom_jitter(height = 0, width = 0.1, pch = 20, col = 'darkblue', size = 2.0)
print(p5)
p6 <- ggplot(data = i0.df, aes(x = country, y = value)) + geom_violin(fill = 'lightblue', draw_quantiles = c(0.25, 0.5, 0.75)) + theme_classic() +
  labs(x = 'Group', y = 'I0') + geom_jitter(height = 0, width = 0.1, pch = 20, col = 'darkblue', size = 2.0)
print(p6)
dev.off()

# Assess partial rank correlations:
library(ppcor)

# realistic:
parms.real <- t(parms.list[[3]]); colnames(parms.real) <- c('L', 'D', 'R0mx', 'R0diff', 'airScale')
m <- pcor(parms.real, method = 'spearman')
sig.corr <- which(m$p.value < 0.005 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.real)[sig.corr[, 1]], colnames(parms.real)[sig.corr[, 2]])
print(colnames(parms.real)[unique(sig.corr[, 1])])
# D, R0mx, R0diff all sig. related to each other

sig.corr <- which(m$p.value < 0.05 & m$estimate < 1.0, arr.ind = TRUE)
m$estimate[sig.corr]
cbind(colnames(parms.real)[sig.corr[, 1]], colnames(parms.real)[sig.corr[, 2]])
print(colnames(parms.real)[unique(sig.corr[, 1])])
# same

pdf('src/syntheticTests/outputs/explore/param_explore_2D.pdf', width = 20, height = 12)

par(mfrow = c(5, 2), cex = 0.8, mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
for (ix in 1:4) {
  for (jx in (ix + 1):5) {
    parm1 <- levels(parms.df$param)[ix]
    parm2 <- levels(parms.df$param)[jx]
    plot(parms.df$value[parms.df$param == parm1], parms.df$value[parms.df$param == parm2], pch = 20, xlab = parm1, ylab = parm2)
    print(paste(parm1, parm2))
    print(cor.test(parms.df$value[parms.df$param == parm1], parms.df$value[parms.df$param == parm2], method = 'spearman'))
  }
}
# D/R0mx (pos); R0mx/R0diff (pos)

# Plot out 2- and 3-dimensional heatplots to explore covariability:
parms.df <- dcast(parms.df, index ~ param)

calc_z_for_heatmap <- function(df, p1, p2, p3 = NA) {
  parm.levels1 <- seq(min(df[, p1]), max(df[, p1]), length.out = 11)
  parm.levels2 <- seq(min(df[, p2]), max(df[, p2]), length.out = 11)
  
  df.heat <- NULL
  for (ix in 1:10) {
    for (jx in 1:10) {
      df.temp <- df[(df[, p1] >= parm.levels1[ix] & df[, p1] <= parm.levels1[ix + 1]) &
                      (df[, p2] >= parm.levels2[jx] & df[, p2] <= parm.levels2[jx + 1]), ]
      
      if (is.na(p3)) { # 2D
        if (length(df.temp[, p1]) > 0) {
          df.heat <- rbind(df.heat, c(ix, jx, length(df.temp[, p1]) / dim(df)[1]))
        } else {
          df.heat <- rbind(df.heat, c(ix, jx, 0))
        }
        
      } else { # 3D
        if (length(df.temp[, p3]) > 0) {
          df.heat <- rbind(df.heat, c(ix, jx, mean(df.temp[, p3])))
        } else {
          df.heat <- rbind(df.heat, c(ix, jx, NA))
        }
      }
      
    }
  }
  
  df.heat <- as.data.frame(df.heat)
  names(df.heat) <- c('x', 'y', 'z')
  
  
  if (is.na(p3)) {
    plot1 <- ggplot(data = df.heat) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(0, max(c(df.heat$z, df.heat$z, df.heat$z)))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = 'Freq.')
    
  } else {
    plot1 <- ggplot(data = df.heat) + geom_tile(aes(x = x, y = y, fill = z)) + scale_fill_viridis(na.value = 'white', limits = c(min(df[, p3]), max(df[, p3]))) +
      theme_classic() + scale_x_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) + scale_y_continuous(expand = c(0, 0), breaks = 1:10, labels = 1:10) +
      labs(x = p1, y = p2, fill = p3)
    
  }
  
  return(plot1)
}

# 2D:
plots1 <- calc_z_for_heatmap(parms.df, 'D', 'R0mx')
plots2 <- calc_z_for_heatmap(parms.df, 'D', 'R0diff')
plots3 <- calc_z_for_heatmap(parms.df, 'R0mx', 'R0diff')
grid.arrange(plots1, plots2, plots3, ncol = 2)

dev.off()

# 3D:
pdf('src/syntheticTests/outputs/explore/param_explore_3D.pdf', width = 21, height = 7)

plots1 <- calc_z_for_heatmap(parms.df, 'D', 'R0mx', 'R0diff')
plots2 <- calc_z_for_heatmap(parms.df, 'D', 'R0diff', 'R0mx')
plots3 <- calc_z_for_heatmap(parms.df, 'R0mx', 'R0diff', 'D')
grid.arrange(plots1, plots2, plots3, ncol = 3)

dev.off()

# For each country, partial rank correlation for S0 vs R0mx/R0diff/D/L/airScale
s0.df <- merge(s0.df, parms.df, by = 'index')
i0.df <- merge(i0.df, parms.df, by = 'index')

for (country in levels(s0.df$country)) {
  print(country)
  df.temp <- s0.df[s0.df$country == country, 3:8]
  m <- pcor(df.temp, method = 'spearman')
  sig.corr <- which(m$p.value[2:6, 1] < 0.005)
  # print(names(sig.corr))
  print(names(df.temp)[sig.corr])
  sig.corr <- which(m$p.value[2:6, 1] < 0.05)
  # print(names(sig.corr))
  print(names(df.temp)[sig.corr + 1])
  print('')
}

rm(list = ls())
