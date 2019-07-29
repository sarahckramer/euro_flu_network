
# Read in TRUE parameter values:
load('syntheticTests/syntheticData/params_07-14.RData')
select.parms <- select.parms[to.keep, ]
select.parms$L <- select.parms$L * 365
select.parms <- as.data.frame(cbind(rep(c(1, 6, 9 , 13), 5), melt(select.parms)))
names(select.parms) <- c('outbreak', 'parameter', 'value')


load('syntheticTests/outputs/cluster/072319/res_loop_S0range_reprobe95.RData')
o <- res[[2]]




