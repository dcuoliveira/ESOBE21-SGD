rm(list=ls())
library('bvartools')
library('here')
source(here('src', 'utils.R'))

#### BVECM STRUCTURE ####
# Y is the matrix of dependent variables
# W is a matrix of potentially cointegrated regressors
# X is the matrix of non-cointegration regressors.
data("e6")
data = gen_vec(data = e6,
               p = 4, # lag order of the VAR form of the model
               r = 1, # cointegration rank of Pi
               const = "unrestricted",
               season = "unrestricted",
               iterations = 5000,
               burnin = 1000)

#### PRIOR ####
# non-informative prior
data = add_priors(data,
                  coint = list(v_i = 0, p_tau_i = 1),
                  coef = list(v_i = 0, v_i_det = 0),
                  sigma = list(df = 0, scale = .0001))

#### POSTERIORS ####
out_posteriors = posteriors_draws_koopetal2010(data=data)
package_posteriors = draw_posterior(data)

# Generate bvec object
bvec_est = bvec(y = data$data$Y,
                w = data$data$W,
                x = data$data$X[, 1:6],
                x_d = data$data$X[, -(1:6)],
                Pi = out_posteriors$draws_pi,
                Gamma = out_posteriors$draws_gamma[1:out_posteriors$k_nondet,],
                C = out_posteriors$draws_gamma[(out_posteriors$k_nondet + 1):nrow(out_posteriors$draws_gamma),],
                Sigma = out_posteriors$draws_sigma)
summary(bvec_est)

#### FORECAST ####
# Generate deterministc terms for function predict
new_d = data$data$X[3 + 1:10, c("const", "season.1", "season.2", "season.3")]

# The function bvec_to_bvar can be used to transform the VEC model into a VAR in levels:
bvar_form = bvec_to_bvar(bvec_est)

# Genrate forecasts
bvar_pred = predict(bvar_form, n.ahead = 10, new_d = new_d)

# Plot forecasts
plot(bvar_pred)







