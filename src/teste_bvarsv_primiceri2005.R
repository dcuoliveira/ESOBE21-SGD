rm(list=ls())
library("bvarsv")

set.seed(1)
data(usmacro)

# Run estimation 
fit = bvar.sv.tvp(Y = usmacro,
                  p = 2,
                  nburn = 5000,
                  nrep = 50000)

# SD of inflation residual 
# Get posterior draws 
sd_inf <- parameter.draws(fit, type = "vcv", row = 1, col = 1)

# SD of unemployment residual 
# Get posterior draws 
sd_une <- parameter.draws(fit, type = "vcv", row = 2, col = 2)

# SD of interest rate residual 
# Get posterior draws 
sd_tbi <- parameter.draws(fit, type = "vcv", row = 3, col = 3)



