
pdf('code/gridSearch/plots/fcast_results_081419.pdf', width = 14, height = 9)

# Focus mainly on metrics file for now:
m <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')

# Limit to needed columns:
m <- m[, c(1:9, 12:13, 15, 17:19, 25:36, 39, 43, 47, 85:95, 97:101, 103:112, 115:117)]
m$leadonset5 <- m$fc_start - m$onset5

# Remove if no onset:
m <- m[!is.na(m$onsetObs5), ]

# Make relevant values factors:
m$oev_base <- factor(m$oev_base)
m$oev_denom <- factor(m$oev_denom)
m$lambda <- factor(m$lambda)
m$run <- factor(m$run)

# Plot overall PT, PI, and OT accuracy by predicted lead week:
m.temp1 <- m[m$leadpkwk_mean >= -8 & m$leadpkwk_mean < 5 & !is.na(m$onset5), ]
m.temp1$leadpkwk_mean <- factor(m.temp1$leadpkwk_mean)

# m[m$country == 'AT' & m$season == '2011-12' & m$fc_start == 45 & m$run == 2 & m$oev_base == 10000 & m$lambda == 1.05 & m$oev_denom == 5, ] # unique!

m.temp <- m.temp1
mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_pkwk_mean)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable
mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_peak_int_bin)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- merge(res, mytable, by='lead')

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadpkwk_mean == lead]))
}
# num.fcasts.temp <- c(num.fcasts.temp, num.fcasts.temp)
res$len <- num.fcasts.temp

names(res)[2:3] <- c('pt', 'pi')
res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value, colour = variable)) +
  geom_line() + geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (All Runs)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

m.temp <- m.temp1[m.temp1$oev_base == 1e4, ]
mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_pkwk_mean)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable
mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_peak_int_bin)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- merge(res, mytable, by='lead')

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadpkwk_mean == lead]))
}
# num.fcasts.temp <- c(num.fcasts.temp, num.fcasts.temp)
res$len <- num.fcasts.temp

names(res)[2:3] <- c('pt', 'pi')
res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value, colour = variable)) +
  geom_line() + geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OEV_base = 1e4)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      
      mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_pkwk_mean)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- mytable
      mytable <- table(m.temp$leadpkwk_mean, m.temp$abs_delta_peak_int_bin)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- merge(res, mytable, by='lead')
      
      num.fcasts.temp <- c()
      for (lead in res$lead) {
        num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadpkwk_mean == lead]))
      }
      # num.fcasts.temp <- c(num.fcasts.temp, num.fcasts.temp)
      res$len <- num.fcasts.temp
      
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
      
    }
  }
  
}

names(res.all)[2:3] <- c('pt', 'pi')
res <- melt(res.all, id.vars = c('lead', 'oev_base', 'oev_denom', 'lambda', 'len'))

res$group <- paste(res$oev_base, res$oev_denom, res$lambda, sep = '_'); res$group <- factor(res$group)

p1 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # maybe lambda > 1 better? but not really seeing patterns here
p3 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p3 # again, oev_base 1e4 definitely looks better, but maybe mostly for -8 through -6 - at -5 through -3, 1e5 seems better, then 1e4 seems to take over again
p4 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # 2-20 (so, intermediate values) seem to have a slight edge, but nothing major

# and PI:
p1 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # higher lambdas allow OEV_base 1e5 to almost catch up with 1e4
p3 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p3 # oev_base 1e4 definitely looks better; and very few fcasts at all for 1e5 when before week -4
p4 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # no major differences

# and OT:
m.temp1 <- m[m$leadonset5 >= -6 & m$leadonset5 < 6 & !is.na(m$onset5), ]
m.temp1$leadonset5 <- factor(m.temp1$leadonset5)

m.temp <- m.temp1[m.temp1$oev_base == 1e4, ]
mytable <- table(m.temp$leadonset5, m.temp$abs_delta_onset)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset5 == lead]))
}
res$len <- num.fcasts.temp

res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value)) + geom_line() + geom_point(aes(size = len)) +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      
      mytable <- table(m.temp$leadonset5, m.temp$abs_delta_onset)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- mytable
      
      num.fcasts.temp <- c()
      for (lead in res$lead) {
        num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$leadonset5 == lead]))
      }
      res$len <- num.fcasts.temp
      
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
    }
  }
  
}

# names(res.all)[1] <- c('ot')
res <- melt(res.all, id.vars = c('lead', 'oev_base', 'oev_denom', 'lambda', 'len'))

res$group <- paste(res$oev_base, res$oev_denom, res$lambda, sep = '_'); res$group <- factor(res$group)

p1 <- ggplot(data = res, aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # need lower, but no strong pattern
p3 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p3) # only 1e4 really gets most of the pre-onset ones
p4 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Predicted Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # no real pattern

# Plot overall PT, PI, OT, and AR accuracy by observed lead week:
m.temp1 <- m[m$FWeek_pkwk >= -8 & m$FWeek_pkwk < 5 & !is.na(m$onset5), ]
m.temp1$FWeek_pkwk <- factor(m.temp1$FWeek_pkwk)

m.temp <- m.temp1#[m.temp1$oev_base == 1e4, ]
mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_pkwk_mean)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable
mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_peak_int_bin)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- merge(res, mytable, by='lead')

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_pkwk == lead]))
}
res$len <- num.fcasts.temp

names(res)[2:3] <- c('pt', 'pi')
res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value, colour = variable)) +
  geom_line() + geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (All Runs)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

m.temp <- m.temp1[m.temp1$oev_base == 1e4, ]
mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_pkwk_mean)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable
mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_peak_int_bin)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- merge(res, mytable, by='lead')

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_pkwk == lead]))
}
res$len <- num.fcasts.temp

names(res)[2:3] <- c('pt', 'pi')
res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value, colour = variable)) +
  geom_line() + geom_point(aes(size = len)) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OEV_base = 1e4)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      
      mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_pkwk_mean)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- mytable
      mytable <- table(m.temp$FWeek_pkwk, m.temp$abs_delta_peak_int_bin)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- merge(res, mytable, by='lead')
      
      num.fcasts.temp <- c()
      for (lead in res$lead) {
        num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_pkwk == lead]))
      }
      # num.fcasts.temp <- c(num.fcasts.temp, num.fcasts.temp)
      res$len <- num.fcasts.temp
      
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
    }
  }
}

names(res.all)[2:3] <- c('pt', 'pi')
res <- melt(res.all, id.vars = c('lead', 'oev_base', 'oev_denom', 'lambda', 'len'))

res$group <- paste(res$oev_base, res$oev_denom, res$lambda, sep = '_'); res$group <- factor(res$group)

p1 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # lower look better
p3 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p3 # 1e4 clearly better before -3 or -4 or so, but 1e5 looks better for -3 through 0
p4 <- ggplot(data = res[res$variable == 'pt', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # no real pattern

# and PI:
p1 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # higher lambdas allow OEV_base 1e5 to almost catch up with 1e4
p3 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p3 # oev_base 1e4 definitely looks better; and very few fcasts at all for 1e5 when before week -4
p4 <- ggplot(data = res[res$variable == 'pi', ], aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (PI)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # no major differences

# and OT:
m.temp1 <- m[m$FWeek_onwk >= -6 & m$FWeek_onwk < 6 & !is.na(m$onset5), ]
m.temp1$FWeek_onwk <- factor(m.temp1$FWeek_onwk)

m.temp <- m.temp1[m.temp1$oev_base == 1e4, ]
mytable <- table(m.temp$FWeek_onwk, m.temp$abs_delta_onset)
mytable <- prop.table(mytable, 1)[, 1:2]
mytable <- melt(rowSums(mytable))
mytable$lead <- as.numeric(rownames(mytable))
res <- mytable

num.fcasts.temp <- c()
for (lead in res$lead) {
  num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk == lead]))
}
res$len <- num.fcasts.temp

res <- melt(res, id.vars = c('lead', 'len'))

p1 <- ggplot(data = res, aes(x = lead, y = value)) + geom_line() + geom_point(aes(size = len)) +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

res.all <- NULL
for (o1 in levels(m$oev_base)) {
  for (o2 in levels(m$oev_denom)) {
    for (l in levels(m$lambda)) {
      m.temp <- m.temp1[m.temp1$oev_base == o1 & m.temp1$oev_denom == o2 & m.temp1$lambda == l, ]
      
      mytable <- table(m.temp$FWeek_onwk, m.temp$abs_delta_onset)
      mytable <- prop.table(mytable, 1)[, 1:2]
      mytable <- melt(rowSums(mytable))
      mytable$lead <- as.numeric(rownames(mytable))
      res <- mytable
      
      num.fcasts.temp <- c()
      for (lead in res$lead) {
        num.fcasts.temp <- c(num.fcasts.temp, length(m.temp$country[m.temp$FWeek_onwk == lead]))
      }
      res$len <- num.fcasts.temp
      
      res$oev_base <- o1; res$oev_denom <- o2; res$lambda <- l
      res.all <- rbind(res.all, res)
    }
  }
  
}

# names(res.all)[1] <- c('ot')
res <- melt(res.all, id.vars = c('lead', 'oev_base', 'oev_denom', 'lambda', 'len'))

res$group <- paste(res$oev_base, res$oev_denom, res$lambda, sep = '_'); res$group <- factor(res$group)

p1 <- ggplot(data = res, aes(x = lead, y = value, group = group, colour = oev_denom, pch = oev_base)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
print(p1)

p2 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ lambda) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p2 # need lower to get anything for very early, but no strong pattern
p3 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_base) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p3 # only 1e4 really gets most of the pre-onset ones
p4 <- ggplot(data = res, aes(x = lead, y = value, group = group)) +
  geom_line() + geom_point(aes(size = len)) + facet_wrap(~ oev_denom) + #scale_color_brewer(palette = 'Set1') +
  theme_bw() + labs(x = 'Observed Lead Week', y = '% Accurate (OT)') + coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
p4 # no real pattern

# Assess log scores for: PT, PI, OT, 1-4 weeks ahead:
d <- read.csv('code/gridSearch/outputs/logScores_pt_ot.csv')
e.pi <- read.csv('code/gridSearch/outputs/logScores_pi.csv')
e <- read.csv('code/gridSearch/outputs/logScores_1-4wk.csv')

d$group <- paste(d$oev_base, d$oev_denom, d$lambda, d$season, d$country, d$run, d$fc_start, sep = '_'); d$group <- factor(d$group)
e$group <- paste(e$oev_base, e$oev_denom, e$lambda, e$season, e$country, e$run, e$fc_start, sep = '_'); e$group <- factor(e$group)
e.pi$group <- paste(e.pi$oev_base, e.pi$oev_denom, e.pi$lambda, e.pi$season, e.pi$country, e.pi$run, e.pi$fc_start, sep = '_'); e.pi$group <- factor(e.pi$group)

e.pi$oev_denom <- factor(e.pi$oev_denom)

e.temp <- e.pi[e.pi$leadpkwk_mean >= -8 & e.pi$leadpkwk_mean < 5 & !is.na(e.pi$leadonset5), ]
# p1 <- ggplot(data = e.temp) + geom_line(aes(x = leadpkwk_mean, y = score, group = group, col = oev_denom)) +
#   geom_point(aes(x = leadpkwk_mean, y = score, group = group, col = oev_denom)) +
#   facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
#   labs(x = 'Predicted Lead Week', y = 'log Score (PI)')# +
#   # scale_x_continuous(breaks = unique(res$lead), labels = unique(res$lead))
# print(p1)
e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = oev_denom)) +
  facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (PI)') + scale_x_continuous(breaks = -6:4)
print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = leadpkwk_mean), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Log Score (PI)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = leadpkwk_mean), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (PI)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(leadpkwk_mean ~ oev_base) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
print(p5)
# p6 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
#   facet_grid(leadpkwk_mean ~ lambda) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
# print(p6)
e.agg <- aggregate(score ~ leadpkwk_mean + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = lambda)) +
  geom_point(aes(x = leadpkwk_mean, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score (PI)') + scale_x_continuous(breaks = -6:4)
print(p6)

e.temp <- d[d$leadpkwk_mean >= -8 & d$leadpkwk_mean < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
e.temp$oev_denom <- factor(e.temp$oev_denom)
e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = oev_denom)) +
  facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (PT)') + scale_x_continuous(breaks = -6:4)
print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = leadpkwk_mean), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Log Score (PT)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = leadpkwk_mean), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (PT)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(leadpkwk_mean ~ oev_base) + theme_bw() + labs(x = 'Log Score (PT)', y = 'Density')
print(p5)
e.agg <- aggregate(score ~ leadpkwk_mean + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = lambda)) +
  geom_point(aes(x = leadpkwk_mean, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score (PT)') + scale_x_continuous(breaks = -6:4)
print(p6)

e.temp <- d[d$leadonset5 >= -6 & d$leadonset5 < 7 & !is.na(d$leadonset5) & d$metric == 'ot', ] # still subject to majority being NA!
e.temp$oev_denom <- factor(e.temp$oev_denom)
e.temp$group <- paste(e.temp$leadonset5, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadonset5, y = exp(score), group = group, fill = oev_denom)) +
  facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (OT)') + scale_x_continuous(breaks = -6:4)
print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadonset5, y = score, group = leadonset5), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Log Score (OT)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadonset5, y = exp(score), group = leadonset5), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Proportion Accurate (OT)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(leadonset5 ~ oev_base) + theme_bw() + labs(x = 'Log Score (OT)', y = 'Density')
print(p5)
e.agg <- aggregate(score ~ leadonset5 + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = leadonset5, y = score, col = lambda)) +
  geom_point(aes(x = leadonset5, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Predicted Lead Week', y = 'Median Log Score (OT)') + scale_x_continuous(breaks = -6:4)
print(p6)

for (wk in levels(e$metric)) {
  e.temp <- e[e$metric == wk & e$leadpkwk_mean >= -8 & e$leadpkwk_mean < 5, ]
  names(e.temp)[10] <- 'score'
  e.temp$oev_denom <- factor(e.temp$oev_denom)
  
  # e.temp$group <- paste(e.temp$leadpkwk_mean, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
  # p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = group, fill = oev_denom)) +
  #   facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
  #   labs(x = 'Predicted Lead Week', y = paste0('Proportion Accurate (', wk, ')')) + scale_x_continuous(breaks = -6:4)
  # print(p2)
  # p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = score, group = leadpkwk_mean), fill = 'gray90') +
  #   facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  #   labs(x = 'Predicted Lead Week', y = paste0('Log Score (', wk, ')')) + scale_x_continuous(breaks = -6:4)
  # print(p3)
  # p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = leadpkwk_mean, y = exp(score), group = leadpkwk_mean), fill = 'gray90') +
  #   facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  #   labs(x = 'Predicted Lead Week', y = paste0('Proportion Accurate (', wk, ')')) + scale_x_continuous(breaks = -6:4)
  # print(p4)
  p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
    facet_grid(leadpkwk_mean ~ oev_base) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
  print(p5)
  # p6 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  #   facet_grid(leadpkwk_mean ~ lambda) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
  # print(p6)
  e.agg <- aggregate(score ~ leadpkwk_mean + oev_base + lambda, data = e.temp, FUN = median)
  e.agg$lambda <- factor(e.agg$lambda)
  p6 <- ggplot(data = e.agg) + geom_line(aes(x = leadpkwk_mean, y = score, col = lambda)) +
    geom_point(aes(x = leadpkwk_mean, y = score, col = lambda)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Predicted Lead Week', y = paste0('Median Log Score (', wk, ')')) + scale_x_continuous(breaks = -6:4)
  print(p6)
}

# And now do all this by observed:
e.temp <- e.pi[e.pi$FWeek_pkwk >= -8 & e.pi$FWeek_pkwk < 5 & !is.na(e.pi$leadonset5), ]
e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
# p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = group, fill = oev_denom)) +
#   facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
#   labs(x = 'Observed Lead Week', y = 'Proportion Accurate (PI)') + scale_x_continuous(breaks = -6:4)
# print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = FWeek_pkwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score (PI)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = FWeek_pkwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Proportion Accurate (PI)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(FWeek_pkwk ~ oev_base) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
print(p5)
e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = lambda)) +
  geom_point(aes(x = FWeek_pkwk, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score (PI)') + scale_x_continuous(breaks = -6:4)
print(p6)

e.temp <- d[d$FWeek_pkwk >= -8 & d$FWeek_pkwk < 5 & d$metric == 'pt' & !is.na(d$leadonset5), ]
e.temp$oev_denom <- factor(e.temp$oev_denom)
e.temp$group <- paste(e.temp$FWeek_pkwk, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
# p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = group, fill = oev_denom)) +
#   facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
#   labs(x = 'Observed Lead Week', y = 'Proportion Accurate (PT)') + scale_x_continuous(breaks = -6:4)
# print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = score, group = FWeek_pkwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score (PT)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_pkwk, y = exp(score), group = FWeek_pkwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Proportion Accurate (PT)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(FWeek_pkwk ~ oev_base) + theme_bw() + labs(x = 'Log Score (PT)', y = 'Density')
print(p5)
e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = lambda)) +
  geom_point(aes(x = FWeek_pkwk, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score (PT)') + scale_x_continuous(breaks = -6:4)
print(p6)

e.temp <- d[d$FWeek_onwk >= -6 & d$FWeek_onwk < 7 & !is.na(d$FWeek_onwk) & d$metric == 'ot', ] # here I want to see distribution, even among those that don't predict an onset!
e.temp$oev_denom <- factor(e.temp$oev_denom)
e.temp$group <- paste(e.temp$FWeek_onwk, e.temp$oev_denom); e.temp$group <- factor(e.temp$group)
# p2 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_onwk, y = exp(score), group = group, fill = oev_denom)) +
#   facet_grid(lambda ~ oev_base) + theme_bw() + scale_color_brewer(palette = 'Set1') +
#   labs(x = 'Observed Lead Week', y = 'Proportion Accurate (OT)') + scale_x_continuous(breaks = -6:4)
# print(p2)
p3 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_onwk, y = score, group = FWeek_onwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Log Score (OT)') + scale_x_continuous(breaks = -6:4)
print(p3)
p4 <- ggplot(data = e.temp) + geom_boxplot(aes(x = FWeek_onwk, y = exp(score), group = FWeek_onwk), fill = 'gray90') +
  facet_grid(lambda ~ oev_base) + theme_bw() + #scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Proportion Accurate (OT)') + scale_x_continuous(breaks = -6:4)
print(p4)
p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
  facet_grid(FWeek_onwk ~ oev_base) + theme_bw() + labs(x = 'Log Score (OT)', y = 'Density')
print(p5)
e.agg <- aggregate(score ~ FWeek_onwk + oev_base + lambda, data = e.temp, FUN = median)
e.agg$lambda <- factor(e.agg$lambda)
p6 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_onwk, y = score, col = lambda)) +
  geom_point(aes(x = FWeek_onwk, y = score, col = lambda)) + facet_wrap(~oev_base) +
  theme_bw() + scale_color_brewer(palette = 'Set1') +
  labs(x = 'Observed Lead Week', y = 'Median Log Score (OT)') + scale_x_continuous(breaks = -6:4)
print(p6)

for (wk in levels(e$metric)) {
  e.temp <- e[e$metric == wk & e$FWeek_pkwk >= -8 & e$FWeek_pkwk < 5, ]
  names(e.temp)[10] <- 'score'
  e.temp$oev_denom <- factor(e.temp$oev_denom)
  
  p5 <- ggplot(data = e.temp) + geom_histogram(aes(x = score, y = ..density..), fill = 'gray90', col = 'black') +
    facet_grid(FWeek_pkwk ~ oev_base) + theme_bw() + labs(x = 'Log Score (PI)', y = 'Density')
  print(p5)
  
  e.agg <- aggregate(score ~ FWeek_pkwk + oev_base + lambda, data = e.temp, FUN = median)
  e.agg$lambda <- factor(e.agg$lambda)
  p6 <- ggplot(data = e.agg) + geom_line(aes(x = FWeek_pkwk, y = score, col = lambda)) +
    geom_point(aes(x = FWeek_pkwk, y = score, col = lambda)) + facet_wrap(~oev_base) +
    theme_bw() + scale_color_brewer(palette = 'Set1') +
    labs(x = 'Observed Lead Week', y = paste0('Median Log Score (', wk, ')')) + scale_x_continuous(breaks = -6:4)
  print(p6)
}

# Check inferred param. values at each time step as well (1e4 normally is a little unrealistic):
o <- read.csv('code/gridSearch/outputs/outputOPParams_081219.csv')
o$group <- paste(o$oev_base, o$oev_denom, o$lambda, o$season, o$run, o$fc_start, sep = '_')
o$group <- factor(o$group)
o$oev_base <- factor(o$oev_base)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = L, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'L') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = D, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'D') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mx, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'R0max') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = R0mn, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'R0min') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o) + geom_line(aes(x = week, y = airScale, group = group, col = oev_base)) +
  theme_classic() + labs(x = 'Week', y = 'airScale') + facet_grid(lambda ~ oev_denom) +
  scale_color_brewer(palette = 'Set1')
print(p1)

p1 <- ggplot(data = o[o$oev_base == 1e4, ]) + geom_line(aes(x = week, y = airScale, group = group)) +
  theme_classic() + labs(x = 'Week', y = 'airScale') + facet_grid(lambda ~ oev_denom, scales = 'free_y')
print(p1)

# Explore accuracy (PT, PI, OT) by country:
d <- d[!is.na(d$leadonset5), ]; e.pi <- e.pi[!is.na(e.pi$leadonset5), ]

d.pt.agg <- aggregate(score ~ country + oev_base + lambda + leadpkwk_mean, data = d[d$metric == 'pt', ], FUN = median)
d.pt.agg <- d.pt.agg[d.pt.agg$leadpkwk_mean >= -8 & d.pt.agg$leadpkwk_mean < 5, ]
d.pt.agg$group <- paste(d.pt.agg$oev_base, d.pt.agg$lambda, sep = '_'); d.pt.agg$group <- factor(d.pt.agg$group)

d.ot.agg <- aggregate(score ~ country + oev_base + lambda + leadonset5, data = d[d$metric == 'ot', ], FUN = median)
d.ot.agg <- d.ot.agg[d.ot.agg$leadonset5 >= -6 & d.ot.agg$leadonset5 < 7, ]
d.ot.agg$group <- paste(d.ot.agg$oev_base, d.ot.agg$lambda, sep = '_'); d.ot.agg$group <- factor(d.ot.agg$group)

e.pi.agg <- aggregate(score ~ country + oev_base + lambda + leadpkwk_mean, data = e.pi, FUN = median)
e.pi.agg <- e.pi.agg[e.pi.agg$leadpkwk_mean >= -8 & e.pi.agg$leadpkwk_mean < 5, ]
e.pi.agg$group <- paste(e.pi.agg$oev_base, e.pi.agg$lambda, sep = '_'); e.pi.agg$group <- factor(e.pi.agg$group)

p1 <- ggplot(data = d.pt.agg) + geom_line(aes(x = leadpkwk_mean, y = score, group = group, col = group)) +
  facet_wrap(~ country) + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (PT)')# +
# scale_color_viridis(discrete = TRUE)
print(p1)
p2 <- ggplot(data = d.pt.agg) + geom_line(aes(x = leadpkwk_mean, y = score, group = group, col = group)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (PT)')# +
# scale_color_viridis(discrete = TRUE)
print(p2)

p1 <- ggplot(data = e.pi.agg) + geom_line(aes(x = leadpkwk_mean, y = score, group = group, col = group)) +
  facet_wrap(~ country) + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (PI)')
print(p1)
p2 <- ggplot(data = e.pi.agg) + geom_line(aes(x = leadpkwk_mean, y = score, group = group, col = group)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (PI)')
print(p2)

p1 <- ggplot(data = d.ot.agg) + geom_line(aes(x = leadonset5, y = score, group = group, col = group)) +
  facet_wrap(~ country) + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (OT)')
print(p1)
p2 <- ggplot(data = d.ot.agg) + geom_line(aes(x = leadonset5, y = score, group = group, col = group)) +
  facet_wrap(~ country, scales = 'free_y') + theme_bw() + labs(x = 'Predicted Lead Week', y = 'Log Score (OT)')
print(p2)

# Look at MAEs:









# Check calibration (method 2):
# Start with PT:
library(Hmisc)
a.dist <- read.csv('code/gridSearch/outputs/outputDist_081219_PT.csv')

wtd.quantile.new <- function(x) {
  if (round(sum(x[, 2])) != 1) {
    print('ERROR 1')
  }
  
  if (any(round(x[, 2] * 300, 0) < 1)) {
    print('ERROR 2')
  }
  
  y <- wtd.quantile(x = x[, 1], weights = round(x[, 2] * 300, 0), probs = p)
  return(y)
}
p <- c(0.005, 0.025, 0.05, 0.1, 0.25, 0.375, 0.625, 0.75, 0.9, 0.95, 0.975, 0.995)

countries <- levels(a.dist$country) # note: different order than that in which forecasts run!
a <- read.csv('code/gridSearch/outputs/outputMet_081219_pro.csv')
a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk')]

a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_pkwk <- a.dist$bin - a.dist$obs_pkwk

a.pkwk <- a.dist[a.dist$leadpkwk_mean >= -8 & a.dist$leadpkwk_mean < 4 & !is.na(a.dist$leadpkwk_mean) & !is.na(a.dist$onset5) & !is.na(a.dist$onsetObs5), ]
# if it doesn't predict an onset, we're not interested in its predictions of peak timing
a.pkwk$leadpkwk_bin <- cut(a.pkwk$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

a.pkwk <- a.pkwk[a.pkwk$bin != -1, ]
a.pkwk <- a.pkwk[a.pkwk$value > 0, ] # don't need weeks with 0% confidence

a.pkwk$oev_base <- factor(a.pkwk$oev_base)
a.pkwk$oev_denom <- factor(a.pkwk$oev_denom)
a.pkwk$lambda <- factor(a.pkwk$lambda)
levels(a.pkwk$lambda)[1] <- '1.00'

dat.temp.all <- data.frame()
for (lead in levels(a.pkwk$leadpkwk_bin)) {
  print(lead)
  
  a.temp <- a.pkwk[a.pkwk$leadpkwk_bin == lead, ]# & a.pkwk$lambda == lam, ]
  c1 <- split(a.temp[, c('bin', 'value')], a.temp[, c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda')])
  c15 <- sapply(1:length(c1), function(ix) {
    unlist(c1[ix])
  })
  c1 <- c1[lapply(c15, length) > 0]
  c2 <- lapply(c1, wtd.quantile.new)
  c3 <- as.data.frame(unlist(c2))
  c35 <- matrix(unlist(strsplit(rownames(c3), split = '[.]')), nrow = length(c3$`unlist(c2)`), byrow = TRUE)
  c4 <- cbind(c3, c35)
  rownames(c4) <- NULL
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'), paste(c4[, 10], c4[, 11], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'quantile')
  c5 <- dcast(c4, country + season + run + fc_start + oev_base + oev_denom + lambda ~ quantile, value.var = 'value')
  # a.new <- merge(c5, unique(a.temp[, c(1:7, 11)]), by = c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda'))
  a.new <- merge(c5, unique(a.temp[, c(1:2, 11)]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    # for (o2 in unique(a.new$oev_denom)) {
      for (lam in unique(a.new$lambda)) {
        a.new.temp <- a.new[a.new$oev_base == o1 & a.new$oev_denom == o2 & a.new$lambda == lam, ]
        # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
        
        denom <- length(a.new.temp$country)
        
        if (denom > 0) {
          
          p25 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`37.5%` & a.new.temp$obs_pkwk <= a.new.temp$`62.5%`)
          p50 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`25.0%` & a.new.temp$obs_pkwk <= a.new.temp$`75.0%`)
          p80 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`10.0%` & a.new.temp$obs_pkwk <= a.new.temp$`90.0%`)
          p90 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 5.0%` & a.new.temp$obs_pkwk <= a.new.temp$`95.0%`)
          p95 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 2.5%` & a.new.temp$obs_pkwk <= a.new.temp$`97.5%`)
          p99 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 0.5%` & a.new.temp$obs_pkwk <= a.new.temp$`99.5%`)
          
          y <- c(p25 / denom, p50 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
          
          # also calculate RMSE for lead/oev/lambda combo:
          rmse <- sqrt(sum((y - c(25, 50, 80, 90, 95, 99)) ** 2) / 6)
          
          dat.temp <- as.data.frame(cbind(c(25, 50, 80, 90, 95, 99), y, rep(lead, 6), rep(rmse, 6), rep(denom, 6), rep(o1, 6), rep(o2, 6), rep(lam, 6)))
          dat.temp.all <- rbind(dat.temp.all, dat.temp)
          
        }
        
      }
    # }
  }
  
  # a.new.temp <- a.new
  # denom <- length(a.new.temp$country)
  # 
  # p25 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`37.5%` & a.new.temp$obs_pkwk <= a.new.temp$`62.5%`)
  # p50 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`25.0%` & a.new.temp$obs_pkwk <= a.new.temp$`75.0%`)
  # p80 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`10.0%` & a.new.temp$obs_pkwk <= a.new.temp$`90.0%`)
  # p90 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 5.0%` & a.new.temp$obs_pkwk <= a.new.temp$`95.0%`)
  # p95 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 2.5%` & a.new.temp$obs_pkwk <= a.new.temp$`97.5%`)
  # p99 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 0.5%` & a.new.temp$obs_pkwk <= a.new.temp$`99.5%`)
  # 
  # y <- c(p25 / denom, p50 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.pt.temp <- dat.temp.all
dat.pt.temp$metric <- 'Peak Timing'

names(dat.pt.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'oev_denom', 'lambda', 'metric')
dat.pt.temp$group <- paste(dat.pt.temp$lead, dat.pt.temp$oev_base, dat.pt.temp$oev_denom, dat.pt.temp$lambda, sep = '_'); dat.pt.temp$group <- factor(dat.pt.temp$group)

p1 <- ggplot(data = dat.pt.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray80') +
  geom_line() + geom_point(size = 4) +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:') + theme_bw() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p1
# again, not really an impact of oev_denom - but check for PI/OT before merging them all

# Now do OT:
a.dist <- read.csv('code/gridSearch/outputs/outputDist_081219_OT.csv')
a.dist <- merge(a.dist, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.dist$bin <- a.dist$bin + 40 - 1
a.dist$bin[a.dist$bin == 38] <- -1

a.dist$delta_onwk <- a.dist$bin - a.dist$onsetObs5
a.dist$leadonwk_mean <- a.dist$fc_start - a.dist$onset5

a.onwk <- a.dist[a.dist$leadonwk_mean >= -6 & a.dist$leadonwk_mean < 4 & !is.na(a.dist$leadonwk_mean) & !is.na(a.dist$onset5) & !is.na(a.dist$onsetObs5), ]
a.onwk$leadonwk_bin <- cut(a.onwk$leadonwk_mean, c(-7, -5, -3, -1, 1, 3))

a.onwk <- a.onwk[a.onwk$bin != -1, ] # can only consider the weighted quantiles among ensemble members that actually predict an onset
a.onwk <- a.onwk[a.onwk$value > 0, ] # don't need weeks with 0% confidence

a.onwk$oev_base <- factor(a.onwk$oev_base)
a.onwk$oev_denom <- factor(a.onwk$oev_denom)
a.onwk$lambda <- factor(a.onwk$lambda)
levels(a.onwk$lambda)[1] <- '1.00'

wtd.quantile.onset <- function(x) {
  # if (round(sum(x[, 2])) != 1) {
  #   print('ERROR 1')
  # }
  
  if (any(round(x[, 2] * 300, 0) < 1)) {
    print('ERROR 2')
  }
  
  tot.mem <- round(sum(x[, 2]) * 300, 0)
  
  y <- wtd.quantile(x = x[, 1], weights = round(x[, 2] * tot.mem, 0), probs = p) # does this actually make any difference?
  return(y)
}

dat.temp.all <- data.frame()
for (lead in levels(a.onwk$leadonwk_bin)) {
  print(lead)
  
  a.temp <- a.onwk[a.onwk$leadonwk_bin == lead, ]
  c1 <- split(a.temp[, c('bin', 'value')], a.temp[, c('country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda')])
  c15 <- sapply(1:length(c1), function(ix) {
    unlist(c1[ix])
  })
  c1 <- c1[lapply(c15, length) > 0]
  c2 <- lapply(c1, wtd.quantile.onset)
  c3 <- as.data.frame(unlist(c2))
  c35 <- matrix(unlist(strsplit(rownames(c3), split = '[.]')), nrow = length(c3$`unlist(c2)`), byrow = TRUE)
  c4 <- cbind(c3, c35)
  rownames(c4) <- NULL
  c4 <- cbind(c4[, 1:7], paste(c4[, 8], c4[, 9], sep = '.'), paste(c4[, 10], c4[, 11], sep = '.'))
  colnames(c4) <- c('value', 'country', 'season', 'run', 'fc_start', 'oev_base', 'oev_denom', 'lambda', 'quantile')
  c5 <- dcast(c4, country + season + run + fc_start + oev_base + oev_denom + lambda ~ quantile, value.var = 'value')
  a.new <- merge(c5, unique(a.temp[, c(1:2, 13)]), by = c('country', 'season'))
  rm(c1, c15, c2, c3, c35, c4, c5)
  
  # Now check how often observed values fall into certain credible intervals for each lead/oev/lambda combo:
  for (o1 in unique(a.new$oev_base)) {
    for (o2 in unique(a.new$oev_denom)) {
      for (lam in unique(a.new$lambda)) {
        a.new.temp <- a.new[a.new$oev_base == o1 & a.new$oev_denom == o2 & a.new$lambda == lam, ]
        # print(dim(a.new.temp)) # this leaves us with very few runs... (should be more once I run the other seasons!)
        
        denom <- length(a.new.temp$country)
        
        if (denom > 0) {
          
          p25 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`37.5%` & a.new.temp$onsetObs5 <= a.new.temp$`62.5%`)
          p50 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`25.0%` & a.new.temp$onsetObs5 <= a.new.temp$`75.0%`)
          p80 <- sum(a.new.temp$onsetObs5 >= a.new.temp$`10.0%` & a.new.temp$onsetObs5 <= a.new.temp$`90.0%`)
          p90 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 5.0%` & a.new.temp$onsetObs5 <= a.new.temp$`95.0%`)
          p95 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 2.5%` & a.new.temp$onsetObs5 <= a.new.temp$`97.5%`)
          p99 <- sum(a.new.temp$onsetObs5 >= a.new.temp$` 0.5%` & a.new.temp$onsetObs5 <= a.new.temp$`99.5%`)
          
          y <- c(p25 / denom, p50 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
          
          # also calculate RMSE for lead/oev/lambda combo:
          rmse <- sqrt(sum((y - c(25, 50, 80, 90, 95, 99)) ** 2) / 6)
          
          dat.temp <- as.data.frame(cbind(c(25, 50, 80, 90, 95, 99), y, rep(lead, 6), rep(rmse, 6), rep(denom, 6), rep(o1, 6), rep(o2, 6), rep(lam, 6)))
          dat.temp.all <- rbind(dat.temp.all, dat.temp)
          
        }
        
      }
    }
  }
  
  # a.new.temp <- a.new
  # denom <- length(a.new.temp$country)
  # 
  # p25 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`37.5%` & a.new.temp$obs_pkwk <= a.new.temp$`62.5%`)
  # p50 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`25.0%` & a.new.temp$obs_pkwk <= a.new.temp$`75.0%`)
  # p80 <- sum(a.new.temp$obs_pkwk >= a.new.temp$`10.0%` & a.new.temp$obs_pkwk <= a.new.temp$`90.0%`)
  # p90 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 5.0%` & a.new.temp$obs_pkwk <= a.new.temp$`95.0%`)
  # p95 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 2.5%` & a.new.temp$obs_pkwk <= a.new.temp$`97.5%`)
  # p99 <- sum(a.new.temp$obs_pkwk >= a.new.temp$` 0.5%` & a.new.temp$obs_pkwk <= a.new.temp$`99.5%`)
  # 
  # y <- c(p25 / denom, p50 / denom, p80 / denom, p90 / denom, p95 / denom, p99 / denom) * 100
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.ot.temp <- dat.temp.all
dat.ot.temp$metric <- 'Onset Timing'

names(dat.ot.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'oev_denom', 'lambda', 'metric')
dat.ot.temp$group <- paste(dat.ot.temp$lead, dat.ot.temp$oev_base, dat.ot.temp$oev_denom, dat.ot.temp$lambda, sep = '_'); dat.ot.temp$group <- factor(dat.ot.temp$group)
# dat.ot.temp$lead <- factor(dat.ot.temp$lead, levels = levels(dat.ot.temp$lead)[5:1])

p1 <- ggplot(data = dat.ot.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray50', size = 1.0) +
  geom_line() + geom_point(size = 4) +#geom_point(aes(size = len)) +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:') + theme_bw() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p1

# Finally, PI:
a.dist <- read.csv('code/gridSearch/outputs/outputEns_081219_PI.csv')
a.int <- a.dist

a.red <- a[, c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'scaling', 'fc_start', 'obs_peak_int', 'obs_pkwk', 'onset5', 'onsetObs5', 'leadpkwk_mean', 'FWeek_pkwk')]
a.int <- merge(a.int, a.red, by = c('country', 'season', 'run', 'oev_base', 'oev_denom', 'lambda', 'fc_start'))

a.int[, c(9:308)] <- a.int[, c(9:308)] / a.int[, 309]
a.int$scaling <- NULL

a.int <- a.int[a.int$leadpkwk_mean >= -8 & a.int$leadpkwk_mean < 4 & !is.na(a.int$leadpkwk_mean) & !is.na(a.int$onset5) & !is.na(a.int$onsetObs5),]
a.int$leadpkwk_bin <- cut(a.int$leadpkwk_mean, c(-9, -7, -5, -3, -1, 1, 3))

a.int$oev_base <- factor(a.int$oev_base)
a.int$oev_denom <- factor(a.int$oev_denom)
a.int$lambda <- factor(a.int$lambda)
levels(a.int$lambda)[1] <- '1.00'

dat.temp.all <- data.frame()
for (lead in levels(a.int$leadpkwk_bin)) {
  print(lead)
  
  a.temp <- a.int[a.int$leadpkwk_bin == lead, ]
  len <- length(a.temp$country)
  
  if (len > 0) {
    a.new <- sapply(1:length(a.temp$leadpkwk_mean), function(ix) {
      quantile(a.temp[ix, 9:308], probs = p)
    })
    a.new <- t(a.new); a.new <- cbind(a.temp, a.new); a.new <- a.new[, -c(9:308)]
    
    
    for (o1 in levels(a.temp$oev_base)) {
      for (o2 in levels(a.temp$oev_denom)) {
        for (lam in levels(a.temp$lambda)) {
          a.new.temp <- a.new[a.new$oev_base == o1 & a.new$oev_denom == o2 & a.new$lambda == lam, ]
          denom <- length(a.new.temp$country)
          
          if (denom > 0) {
            p25 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`37.5%` & a.new.temp$obs_peak_int <= a.new.temp$`62.5%`)
            p50 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`25%` & a.new.temp$obs_peak_int <= a.new.temp$`75%`)
            p80 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`10%` & a.new.temp$obs_peak_int <= a.new.temp$`90%`)
            p90 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`5%` & a.new.temp$obs_peak_int <= a.new.temp$`95%`)
            p95 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`2.5%` & a.new.temp$obs_peak_int <= a.new.temp$`97.5%`)
            p99 <- sum(a.new.temp$obs_peak_int >= a.new.temp$`0.5%` & a.new.temp$obs_peak_int <= a.new.temp$`99.5%`)
            
            y <- c(p25 / denom, p50 / denom, p80 / denom, p90 / denom,
                   p95 / denom, p99 / denom) * 100
            rmse <- sqrt(sum((c(p25 / denom, p50 / denom, p80 / denom, p90 / denom,
                                p95 / denom, p99 / denom) * 100 - c(25, 50, 80, 90, 95, 99)) ** 2) / 6)
            
            dat.temp <- as.data.frame(cbind(c(25, 50, 80, 90, 95, 99), y, rep(lead, 6), rep(rmse, 6), rep(denom, 6), rep(o1, 6), rep(o2, 6), rep(lam, 6)))
            dat.temp.all <- rbind(dat.temp.all, dat.temp)
          }
          
        }
      }
    }
    
  }
  
}

for (i in c(1:2, 4:5)) {
  dat.temp.all[, i] <- as.numeric(as.character(dat.temp.all[, i]))
}
dat.pi.temp <- dat.temp.all
dat.pi.temp$metric <- 'Peak Intensity'

names(dat.pi.temp) <- c('quantile', 'y', 'lead', 'rmse', 'len', 'oev_base', 'oev_denom', 'lambda', 'metric')
dat.pi.temp$group <- paste(dat.pi.temp$lead, dat.pi.temp$oev_base, dat.pi.temp$oev_denom, dat.pi.temp$lambda, sep = '_'); dat.pi.temp$group <- factor(dat.pi.temp$group)

p1 <- ggplot(data = dat.pi.temp, aes(x = quantile, y = y, color = lead, group = group)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray80') +
  geom_line() + geom_point(size = 4) +
  labs(x = 'Prediction Interval', y = '% of Obs Within Interval', colour = 'Pred. Lead:') + theme_bw() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = 'Set1') +
  # scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(oev_base ~ lambda)
p1
















dat.fit <- rbind(dat.pt.temp, dat.pi.temp, dat.ot.temp)




dat.fig <- rbind(dat.pt.temp, dat.pt.trop); dat.fig <- rbind(dat.fig, dat.pi.temp); dat.fig <- rbind(dat.fig, dat.pi.trop)
dat.fig$metric <- factor(dat.fig$metric); dat.fig$metric <- relevel(dat.fig$metric, ref = 'Peak Timing')
dat.fig$region <- factor(dat.fig$region)

p2 <- ggplot(data = dat.fig, aes(x = V1, y = y, colour = V3)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = 'gray80') +
  geom_line() + geom_point(size = 4) +
  labs(x = 'Prediction Interval', y = '% of Obs within PI', colour = 'Pred. Lead:') + theme_bw() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 12), axis.text = element_text(size = 10),
        strip.text = element_blank(), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12), strip.background = element_blank()) +
  # scale_color_brewer(palette = 'Set1') +
  scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b')) +#, '#a6d96a', '#1a9850')) +
  scale_x_continuous(limits = c(20, 100), breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  facet_grid(region ~ metric)
dat.text <- data.frame(label = c('A', 'B', 'C', 'D'), region = c('Temperate', 'Temperate', 'Tropics', 'Tropics'),
                       metric = c('Peak Timing', 'Peak Intensity', 'Peak Timing', 'Peak Intensity'))
p2 + geom_text(data = dat.text, mapping = aes(x = 23, y = 96, label = label), size = 8, color = 'black')









dev.off()






# ################
# ### FIGURE 3 ###
# ################
# # Timing:
# # Set up
# a$pkwk_var <- a$pkwk_sd ** 2
# c$pkwk_var <- c$pkwk_sd ** 2
# 
# a$peak_intensity_var <- a$peak_intensity_sd ** 2
# c$peak_intensity_var <- c$peak_intensity_sd ** 2
# 
# a.temp <- a[a$leadpkwk_mean >= -6 & a$leadpkwk_mean < 5 & !is.na(a$onset5) & !is.na(a$onsetObs5),]
# a.temp$leadpkwk_mean_bin <- cut(a.temp$leadpkwk_mean, c(-7, -5, -3, -1, 2, 4))
# a.temp$leadpkwk_mean_bin <- factor(a.temp$leadpkwk_mean_bin)
# 
# a.temp$pkwk_var_bin <- cut(a.temp$pkwk_var, quantile(a.temp$pkwk_var, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T))
# a.temp$pkwk_var_bin <- factor(a.temp$pkwk_var_bin)
# 
# c.temp <- c[c$leadpkwk_mean >= -6 & c$leadpkwk_mean < 5 & !is.na(c$leadpkwk_mean) & !is.na(c$onsetObs),]
# c.temp$leadpkwk_mean_bin <- cut(c.temp$leadpkwk_mean, c(-7, -5, -3, -1, 2, 4))
# c.temp$leadpkwk_mean_bin <- factor(c.temp$leadpkwk_mean_bin)
# 
# c.temp$pkwk_var_bin <- cut(c.temp$pkwk_var, quantile(c.temp$pkwk_var, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T))
# c.temp$pkwk_var_bin <- factor(c.temp$pkwk_var_bin)
# 
# # Temperate, w/ humidity
# x <- rep(quantile(a.temp$pkwk_var, probs = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95), na.rm=T), 5)
# y <- c()
# bins <- c()
# lens <- c()
# 
# for (bin in levels(a.temp$leadpkwk_mean_bin)) {
#   print(bin)
#   bins <- c(bins, rep(bin, 10))
#   m.temp <- a.temp[a.temp$leadpkwk_mean_bin == bin,]
#   
#   for (bin.var in levels(m.temp$pkwk_var_bin)) {
#     m.temp.2 <- m.temp[m.temp$pkwk_var_bin == bin.var & !is.na(m.temp$pkwk_var_bin),]
#     lens <- c(lens, length(m.temp.2$country))
#     if (length(m.temp.2$country) > 100) {
#       y <- c(y, length(m.temp.2$country[m.temp.2$accurate_pkwk == 'yes'])/
#                length(m.temp.2$country))
#     } else {
#       y <- c(y, NA)
#     }
#     
#   }
# }
# 
# cali.pt <- as.data.frame(cbind(as.vector(x), y, bins, lens))
# cali.pt$V5 <- 'temp_h'
# # Note that we're limiting calculation of percent accurate to where AT LEAST 100 forecasts
# 
# # Tropics
# x <- rep(quantile(c.temp$pkwk_var, probs = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95), na.rm=T), 5)
# y <- c()
# bins <- c()
# lens <- c()
# 
# for (bin in levels(c.temp$leadpkwk_mean_bin)) {
#   print(bin)
#   bins <- c(bins, rep(bin, 10))
#   m.temp <- c.temp[c.temp$leadpkwk_mean_bin == bin & !is.na(c.temp$leadpkwk_mean_bin),]
#   
#   for (bin.var in levels(m.temp$pkwk_var_bin)) {
#     m.temp.2 <- m.temp[m.temp$pkwk_var_bin == bin.var & !is.na(m.temp$pkwk_var_bin),]
#     lens <- c(lens, length(m.temp.2$country))
#     if (length(m.temp.2$country) > 10) {
#       y <- c(y, length(m.temp.2$country[m.temp.2$accurate_pkwk == 'yes'])/
#                length(m.temp.2$country))
#     } else {
#       y <- c(y, NA)
#     }
#     
#   }
# }
# 
# cali.pt <- rbind(cali.pt, as.data.frame(cbind(as.vector(x), y, bins, lens, rep('trop', 50))))
# 
# cali.pt$V1 <- as.numeric(as.character(cali.pt$V1))
# cali.pt$y <- as.numeric(as.character(cali.pt$y))
# cali.pt$lens <- as.numeric(as.character(cali.pt$lens))
# cali.pt$bins <- factor(cali.pt$bins, levels(cali.pt$bins)[c(4, 3, 2, 1, 5)])
# 
# # Intensity:
# # Set up
# a.temp$peak_intensity_var <- a.temp$peak_intensity_var / a.temp$scaling
# a.temp$peak_intensity_var <- a.temp$peak_intensity_var / a.temp$peak_intensity
# a.temp$peak_intensity_var_bin <- cut(a.temp$peak_intensity_var, quantile(a.temp$peak_intensity_var, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = T))
# a.temp$peak_intensity_var_bin <- factor(a.temp$peak_intensity_var_bin)
# 
# c.temp$peak_intensity_var <- c.temp$peak_intensity_var / c.temp$scaling
# c.temp$peak_intensity_var <- c.temp$peak_intensity_var / c.temp$peak_intensity
# c.temp$peak_intensity_var_bin <- cut(c.temp$peak_intensity_var, quantile(c.temp$peak_intensity_var, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = T))
# c.temp$peak_intensity_var_bin <- factor(c.temp$peak_intensity_var_bin)
# 
# # Temperate, w/ humidity
# x <- rep(quantile(a.temp$peak_intensity_var, probs = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95), na.rm = T), 5)
# y <- c()
# bins <- c()
# lens <- c()
# 
# for (bin in levels(a.temp$leadpkwk_mean_bin)) {
#   print(bin)
#   bins <- c(bins, rep(bin, 10))
#   m.temp <- a.temp[a.temp$leadpkwk_mean_bin == bin,]
#   
#   for (bin.var in levels(m.temp$peak_intensity_var_bin)) {
#     m.temp.2 <- m.temp[m.temp$peak_intensity_var_bin == bin.var & !is.na(m.temp$peak_intensity_var_bin),]
#     lens <- c(lens, length(m.temp.2$country))
#     if (length(m.temp.2$country) > 100) {
#       y <- c(y, length(m.temp.2$country[m.temp.2$accurate_int == 'yes'])/
#                length(m.temp.2$country))
#     } else {
#       y <- c(y, NA)
#     }
#     
#   }
# }
# 
# cali.pi <- as.data.frame(cbind(as.vector(x), y, bins, lens))
# cali.pi$V5 <- 'temp_h'
# # Note that we're limiting calculation of percent accurate to where AT LEAST 100 forecasts
# 
# cali.pi <- rbind(cali.pi, as.data.frame(cbind(as.vector(x), y, bins, lens, rep('trop', 50))))
# 
# cali.pi$V1 <- as.numeric(as.character(cali.pi$V1))
# cali.pi$y <- as.numeric(as.character(cali.pi$y))
# cali.pi$lens <- as.numeric(as.character(cali.pi$lens))
# cali.pi$bins <- factor(cali.pi$bins, levels(cali.pi$bins)[c(4, 3, 2, 1, 5)])
# 
# cali.pt$metric <- 'Peak Timing'; cali.pi$metric <- 'Peak Intensity'
# cali <- rbind(cali.pt, cali.pi); rm(cali.pt, cali.pi)
# cali$metric <- factor(cali$metric)
# cali$metric <- relevel(cali$metric, ref = 'Peak Timing')
# 
# breaks <- 1:10
# p1 <- ggplot(data = cali, aes(x = rep(1:10, 20), y = y, colour = bins)) + 
#   geom_line() + geom_point(aes(size = lens)) + theme_bw() +
#   theme(aspect.ratio = 1, legend.text = element_text(size = 12),
#         axis.text = element_text(size = 10), axis.title = element_text(size = 12),
#         legend.title = element_text(size = 12), strip.background = element_blank(),
#         strip.text = element_blank()) +
#   scale_size_continuous(breaks = c(100, 600), labels = c(100, 600), limits = c(0, 745), range = c(1, 6)) +
#   labs(x = 'Ensemble Variance (Quantile)', y = 'Proportion Accurate within\n1 Week/25%',
#        colour = 'Pred. Lead:', size = '# of Fcasts:') + coord_cartesian(ylim = c(0, 1)) +
#   scale_x_continuous(breaks = breaks) + facet_grid(V5 ~ metric) +
#   #scale_color_brewer(palette = 'RdYlGn') +
#   scale_color_manual(values = c('#d73027', '#fdae61', '#fee08b', '#a6d96a', '#1a9850')) +
#   guides(colour = guide_legend(order = 1,override.aes = list(size = 5,linetype = 'blank')),
#          size = guide_legend(order = 2))
# dat.text <- data.frame(label = c('A', 'B', 'C', 'D'), V5 = c('temp_h', 'temp_h', 'trop', 'trop'),
#                        metric = c('Peak Timing', 'Peak Intensity', 'Peak Timing', 'Peak Intensity'))
# p1 + geom_text(data = dat.text, mapping = aes(x = 9.8, y = 0.97, label = label), size = 8, color = 'black')






