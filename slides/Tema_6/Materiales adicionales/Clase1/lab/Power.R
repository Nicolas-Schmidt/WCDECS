# Power Analysis


## ------ 1. Analytic solution ------ # 
power.t.test(
# sample size (leave blank!) n=,
# minimum detectable effect size
  delta = 5, sd = 20,
# alpha and power (1-kappa)
  sig.level = 0.05, power = 0.8,
# two-tailed vs. one-tailed test
  alternative = "two.sided"
)


## ------ 2. Simulation ------ # 

possible.ns <- seq(from=100, to=2000, by=50) # The sample sizespossible.ns <- seq(from=100, to=2000, by=50) # The sample sizes
powers <- rep(NA, length(possible.ns))       # Empty object to collect simulation estimates
alpha <- 0.05                                # Standard significance level
sims <- 500                                  # Number of simulations to conduct for each N


# Try the first vaues to understand the code below
# j=1
# i=1


#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                        # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)   # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=60, sd=20)              # control potential outcome
    tau <- 5                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    # head(cbind(Y0, Y1, tau, Z.sim, Y.sim))
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    # summary(fit.sim)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values (assuming 
                                                   # equal variance in treatement and 
                                                   # control groups)
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to 
                                                     #p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments) # store average success rate (power) for each N
}


# Plot
plot(possible.ns, powers, ylim=c(0,1), 
     main= expression(paste("Power Calculation Different Sample Size (", tau, " = 5, SD = 20)")),
     xlab = "Sample size - N")
abline(h=0.8, col="red")

