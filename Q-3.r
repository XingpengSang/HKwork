machine1 <- c(340, 341, 335, 344, 338, 337, 346, 345, 343, 344)
machine2 <- c(335, 341, 346, 342, 350, 338, 337, 339, 344, 339)

sigma1 <- 3.0
sigma2 <- 3.0

n1 <- length(machine1)
n2 <- length(machine2)
xbar1 <- mean(machine1)
xbar2 <- mean(machine2)

cat("Machine 1: n =", n1, ", mean =", round(xbar1, 2), "\n")
cat("Machine 2: n =", n2, ", mean =", round(xbar2, 2), "\n\n")

#Q3b
#Hypotheses
alpha <- 0.05
se <- sqrt(sigma1^2 / n1 + sigma2^2 / n2)
z_stat <- (xbar1 - xbar2) / se
cat("Standard error =", round(se, 4), "\n")
cat("Z statistic =", round(z_stat, 4), "\n\n")
#p-value
p_value <- 2 * (1 - pnorm(abs(z_stat)))
cat("p-value =", round(p_value, 4), "\n\n")
#Decision
cat("Significance level alpha =", alpha, "\n")
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: The two machines have different mean fill volumes.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: No sufficient evidence that the two machines differ in mean fill volume.\n")
}

#Q3c
#Difference in means
diff_means <- xbar1 - xbar2
#Standard error 
se <- sqrt(sigma1^2 / n1 + sigma2^2 / n2)
#95% CI 
z_crit <- qnorm(0.975)   # = 1.96
ci_lower <- diff_means - z_crit * se
ci_upper <- diff_means + z_crit * se
cat("Difference in means (mu1 - mu2) =", round(diff_means, 4), "\n")
cat("Standard error =", round(se, 4), "\n")
cat("95% CI = (", round(ci_lower, 4), ",", round(ci_upper, 4), ")\n")

#Q3e
#Given values
delta <- 3.0      # practically important difference (ml)
sigma <- 3.0      # common standard deviation (ml)
n1 <- 10
n2 <- 10
alpha <- 0.05
#Effect size
d <- delta / sigma   # = 1
#Noncentrality parameter 
ncp <- delta / (sigma * sqrt(2 / n1))
#Degrees of freedom
df_t <- n1 + n2 - 2   # = 18
#Critical t value (two-sided)
tcrit <- qt(1 - alpha/2, df_t)
#Power from noncentral t distribution
power <- 1 - pt(tcrit, df_t, ncp) + pt(-tcrit, df_t, ncp)
#Type II error
beta <- 1 - power
cat("ncp =", round(ncp, 4), "\n")
cat("df =", df_t, "\n")
cat("tcrit =", round(tcrit, 4), "\n")
cat("Power =", round(power, 4), "\n")
cat("Type II error (beta) =", round(beta, 4), "\n")
