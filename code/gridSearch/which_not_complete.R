
### Determine which indices/input combos weren't run, and:
# 1. Rerun?
# 2. Delete pdf files

# Get lists of possible values:
oevBase_list <- c(1e4, 1e5, 1e6)
oevFact_list <- c(0.1, 0.2, 0.5, 1.0)
oevDenom_list <- c(1.0, 5.0, 10.0, 20.0, 100.0)
tmpExpList <- c(1.0, 1.5, 2.0)
lambdaList <- c(1.00, 1.01, 1.03, 1.05)

# Set full task.index list:
task.index <- 1:100 # eventually through 720

# Calculate values for each index:
oev_base <- oevBase_list[ceiling(task.index / 240)]
oev_fact <- oevFact_list[ceiling((task.index - 60) / 60) %% 4 + 1]
oev_denom <- oevDenom_list[ceiling((task.index - 12) / 12) %% 5 + 1]
tmp_exp <- tmpExpList[ceiling((task.index - 4) / 4) %% 3 + 1]
lambda <- lambdaList[ceiling(task.index - 1) %% 4 + 1]

# Determine which are present in metrics files:
file.list <- list.files('code/gridSearch/outputs/metrics/')
missing <- c()
for (i in task.index) {
  if (!file.exists(paste('code/gridSearch/outputs/metrics/outputMet', oev_base[i], oev_fact[i],
                         oev_denom[i], tmp_exp[i], lambda[i], '060519.csv', sep = '_'))) {
    missing <- c(missing, i)
  }
}
# when failure occurs, it looks to be due to very low obs. error compared to ens. error
# failure also seems pretty consistent - might not happen on the first run, but repeats itself
# may fail on one run but not the other, but other non-fail runs are also bad fits

# Any pattern in values that ran or did not?:
df <- as.data.frame(cbind(task.index, oev_base, oev_fact, oev_denom, tmp_exp, lambda, rep('y', length(task.index))))
names(df)[c(1, 7)] <- c('index', 'run')
df$run <- as.character(df$run); df$run[missing] <- 'n'; df$run <- factor(df$run)

print(table(df$run, df$oev_base)) # for now, all 1e4
print(table(df$run, df$oev_fact)) # working more for 0.2, but may be confounding?
print(table(df$run, df$oev_denom)) # looks way more likely for "1" to work
print(table(df$run, df$tmp_exp)) # pretty similar
print(table(df$run, df$lambda)) # lower values look a bit better

a <- glm(run ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = df, family = 'binomial')
print(exp(a$coefficients))
print(exp(confint(a)))
# so far: higher oev_denom and lambda make less likely to work

# 2010-11 or 2011-12? Are there pdf files for both seasons?
for (i in missing) {
  print(file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                    oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_')))
}
# some only fail in 2011-12...
# but let's still just delete them all for now - can try running again, but seems like these
# might just not work

# Delete pdf files of "missing":
for (i in missing) {
  if (file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                        oev_denom[i], tmp_exp[i], lambda[i], '2010-11_060519.pdf', sep = '_'))) {
    file.remove(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                      oev_denom[i], tmp_exp[i], lambda[i], '2010-11_060519.pdf', sep = '_'))
  }
  if (file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                        oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_'))) {
    file.remove(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                      oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_'))
  }
}

# 4   7  12  13  15  16  17  19  20  22  24  25  27  28  29  30  31  32  33  34  35  36  37  38
# 39  40  42  43  44  45  46  47  48  50  51  52  53  54  55  56  57  58  59  60  75  76  77  79
# 80  81  82  83  84  86  87  88  89  90  91  93  94  95  96  97  98  99 100


















