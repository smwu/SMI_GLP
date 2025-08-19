# Power calculations

# Power and sample size calculations based on the unadjusted Cox regression model 
# (equivalent to log-rank test) for unequal groups, as presented in 
# Schoenfeld (1983). Similar calculations can also be done by using 
# https://sample-size.net/sample-size-survival-analysis/.
# Inputs:
#   alpha: Proportion specifying Type I error
#   beta: Proportion specifying Type II error, equal to 1 - power
#   q_treat: Proportion allocated to treatment
#   tau: Maximum anticipated follow-up time, in same units as `phi`, `mu_0`, or 
#     `med_0`
#   phi: Hazard ratio. Optional (see details). Default is `NULL`.
#   lambda_0: Hazard function in the control group. Optional (see details). 
#     Default is `NULL`.
#   mu_0: Mean survival time in the control group, in same units as `tau`.
#     Optional (see details). Default is `NULL`.
#   med_0: Median survival time in the control group, in same units as `tau`.
#     Optional (see details). Default is `NULL`.
#   lambda_1: Hazard function in the treatment group. Optional (see details). 
#     Default is `NULL`.
#   mu_1: Mean survival time in the treatment group, in same units as `tau`.
#     Optional (see details). Default is `NULL`.
#   med_1: Median survival time in the treatment group, in same units as `tau`.
#     Optional (see details). Default is `NULL`.
#   print_output: Boolean specifying whether output should be printed. Default 
#     is `TRUE`.
# Outputs: List containing the following:
#   d: Number of events needed
#   n: Sample size required 
#   n_treat: Sample size required for the treatment group
# Details:
#   One of `lambda_0`, `mu_0`, or `med_0` must be specified to characterize the 
#     baseline event rate in the control group. In addition, either `phi` or 
#     one of `lambda_1`, `mu_1`, or `med_1` must be specified to characterize 
#     the event rate in the treatment group. 
get_sample_size_onestep <- function(alpha = 0.05, beta = 0.1, q_treat = 0.5, 
                                    tau, phi = NULL, lambda_0 = NULL, 
                                    mu_0 = NULL, med_0 = NULL, mu_1 = NULL, 
                                    med_1 = NULL, lambda_1 = NULL, 
                                    print_output = TRUE) {
  
  ### Step 0: Calculate hazard ratio assuming exponential distribution
  if (is.null(lambda_0)) {
    if (is.null(mu_0)) {
      if (is.null(med_0)) {
        stop("Either lambda_0, mu_0, or med_0 must be specified.")
      } else {
        lambda_0 <- -log(0.5) / med_0
      }
    } else {
      lambda_0 <- 1 / mu_0 
    }
  }
  
  if (is.null(phi)) {
    if (is.null(lambda_1)) {
      if (is.null(mu_1)) {
        if (is.null(med_1)) {
          stop("Either phi, lambda_1, mu_1, or med_1 must be specified.")
        } else {
          lambda_1 <- -log(0.5) / med_1
        }
      } else {
        lambda_1 <- 1 / mu_1
      }
    } 
    phi <- lambda_1 / lambda_0
  }
  
  ### Step 1: Get number of events needed
  
  # Critical value z-score under the null
  z_crit <- qnorm(1 - (alpha / 2))
  # Critical value z-score under the alternative
  z_power <- qnorm(1 - beta)
  
  # Calculate number of events needed for adequate power
  # Uses Schoenfeld's formula
  numer <- (z_crit + z_power)^2
  denom <- q_treat * (1 - q_treat) * (log(phi)^2)
  
  # Round up for number of events needed
  d <- numer / denom
  
  if (print_output) {
    print(paste0("Number of events needed: ", ceiling(d)))
  }
  
  ### Step 2: Get sample size
  
  # Calculate the sample size needed for adequate power
  denom <- 1 - ((1 - q_treat) * exp(-tau * lambda_0)) - 
    (q_treat * exp(-tau * phi * lambda_0))
  
  # Round up for sample size
  n <- ceiling(d / denom)
  
  if (print_output) {
    print(paste0("Sample size needed: ", n))
  }
  
  ### Additional: Get sample size for treatment group
  n_treat <- ceiling(n * q_treat)
  if (print_output) {
    print(paste0("n_treat: ", n_treat))
  }
  
  return(list(d = d, n = n, n_treat = n_treat))
}


# Try the one-step method test results: YAY it works!
test_n <- get_sample_size_onestep(alpha = 0.05, beta = 0.1,  q_treat = 0.5, 
                                  tau = 33, med_0 = 9, med_1 = 14)

# Examine impact of q_treat
# As q_treat deviates from 0.5, total sample size goes up
# However, n_treat is lowest for smallest q_treat, and increases exponentially 
# as q_treat increases
q_treat_vec <- seq(0.05, 0.95, by = 0.05)
n_vec <- sapply(q_treat_vec, function(x) 
  get_sample_size_onestep(alpha = 0.05, beta = 0.1,  q_treat = x, tau = 33, 
                          med_0 = 9, med_1 = 14, print_output = FALSE)$n)
n_treat_vec <- sapply(q_treat_vec, function(x) 
  get_sample_size_onestep(alpha = 0.05, beta = 0.1,  q_treat = x, tau = 33, 
                          med_0 = 9, med_1 = 14, print_output = FALSE)$n_treat)
plot(q_treat_vec, n_vec, pch = 16, 
     xlab = "Proportion Allocated to Treatment",
     ylab = "Sample size Required",
     ylim = c(0, max(n_vec)))
points(q_treat_vec, n_treat_vec, pch = 2)

### Try with the numbers that we would use

# Type I error
alpha <- 0.05
# Type II error
beta <- 0.2

# Proportion allocated to treatment
q_treat <- 0.15

# Average anticipated follow-up time in years
tau <- 5

# Get sample size for MACE outcomes among very high risk 
# Abrahami et al. (2020) Clin Pharmacol Ther: 
# IR 22.1/1000 PYAR for sulfonylureas w/ CPRD 2009-2013
inc_0 <- 22.1 
# Convert incidence to hazard by dividing by PYAR (okay for rare events)
lambda_0 <- inc_0 / 1000
# Placeholder hazard ratios
phi <- 0.8
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = phi)
# 90% power
n <- get_sample_size_onestep(alpha = alpha, beta = 0.1, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = phi)


# Get sample size for psychiatric hospitalisation outcomes 
# Richards-Belle et al. (2025) plosOne:
# 1-year incidence ranged from 13.3% to 16.1% across treatment arms, estimated 
# from the cumulative incidence function accounting for death as a potential 
# competing risk.
# We will use an average 14.4% for the control arm incidence.
inc_0 <- 0.144
phi <- 0.8
# The hazard can be approximated by the incidence rate, assuming a constant 
# hazard (i.e., Exponential distribution) and a rare event rate
lambda_0 <- inc_0 
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = phi)
# Different hazard ratios
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = 0.5)
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = 1.25)

# Get sample size for incident SMI outcomes
# Prop GLP vs non for those with T2DM
q_treat <- 0.15  # Shapiro et al. (2025)
# Kirkbride et al. (2012) PLOSOne: 31.7 for psychotic disorders
# Lloyd et al., 2018 BJPsych: 1.7 for bipolar disorders
# Rait et al., 2009 BJPsych: Depr 14.0 per 1000 person-years at risk from 1996-2006
# Hardoon et al., 2013 PLOSOne: SMI 46.4 per 100000 PYAR from 2000-2010 age 16-65 
# using UK THIN data
inc_0 <- 46.4  + 1400
# Convert incidence per 100,000 person-years to hazard
lambda_0 <- inc_0 / 100000 
phi <- 0.8
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = phi)
# Different hazard ratios
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = 0.5)
n <- get_sample_size_onestep(alpha = alpha, beta = beta, q_treat = q_treat, 
                             tau = tau, lambda_0 = lambda_0, phi = 1.2)



get_CI_sampsize <- function(p, g, L, alpha) {
  n <- (qnorm(alpha/2) / L)^2 * p * (1-p) 
  n_geno <- n / (1 - g)
  return(ceiling(n_geno))
}


#================= OLD CODE ====================================================
# 
# # Inputs:
# #   alpha: Type I error
# #   beta: Type II error
# #   phi: Hazard ratio
# #   q_treat: Proportion allocated to treatment 
# get_num_events <- function(alpha, beta, phi, q_treat) {
#   # Critical value z-score under the null
#   z_crit <- qnorm(1 - (alpha / 2))
#   # Critical value z-score under the alternative
#   z_power <- qnorm(1 - beta)
#   
#   # Calculate number of events needed for adequate power
#   # Uses Schoenfeld's formula
#   numer <- (z_crit + z_power)^2
#   denom <- q_treat * (1 - q_treat) * (log(phi)^2)
#   
#   # Round up for number of events needed
#   d <- ceiling(numer / denom)
#   
#   print(paste0("Number of events needed: ", d))
#   
#   return(d)
# }
# 
# # Inputs:
# #   d: Number of events needed, outputted from `get_num_events`
# #   q_treat: Proportion allocated to treatment
# #   tau: Maximum anticipated follow-up time, in same units as hazard
# #   lambda_0: Hazard function in the control group
# #   phi: Hazard ratio
# get_sample_size <- function(d, q_treat, tau, lambda_0 = NULL, phi = NULL) {
#   
#   # Calculate the sample size needed for adequate power
#   denom <- 1 - ((1 - q_treat) * exp(-tau * lambda_0)) - 
#     (q_treat * exp(-tau * phi * lambda_0))
#   
#   # Round up for sample size
#   n <- ceiling(d / denom)
#   
#   print(paste0("Sample size needed: ", n))
#   
#   return(n)
# }
# 
# # Type I error
# alpha <- 0.05
# # Type II error
# beta <- 0.2
# 
# # Proportion allocated to treatment
# q_treat <- 0.5
# 
# # Calculate values for hazard ratio, phi
# # Mean survival time in months for control group
# mu_0 <- 12 
# # Mean survival time in months for treatment group
# mu_1 <- 24
# # Calculate control hazard and hazard ratio assuming exponential distribution
# lambda_0 <- 1 / mu_0 
# phi <- mu_0 / mu_1
# 
# # Follow-up time in months
# tau <- 24
# 
# d <- get_num_events(alpha = alpha, beta = beta, phi = phi, q_treat = q_treat)
# n <- get_sample_size(d = d, q_treat = q_treat, tau = tau, lambda_0 = lambda_0, 
#                      phi = phi)
# 
# # Test same results as lecture slides Example #2 on p247. Works but rounded up 1
# d <- get_num_events(alpha = 0.05, beta = 0.1, phi = 9/14, q_treat = 0.5)
# test_n <- get_sample_size(d = d, q_treat = 0.5, tau = 33, lambda_0 = 0.0770, 
#                           phi = 9/14)
# 
# 
