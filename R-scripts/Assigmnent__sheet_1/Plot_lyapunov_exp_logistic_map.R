library(ggplot2)

# Compute Lyapunov exponent for the logistic map
lyapunov_logistic <- function(mu, x0 = 0.5, n = 1000, discard = 100) {
  x <- x0
  lyap_sum <- 0
  
  # Iterate the map, discarding initial transient
  for (i in 1:discard) {
    x <- mu * x * (1 - x)
  }
  
  # Compute Lyapunov sum
  for (i in 1:n) {
    x <- mu * x * (1 - x)
    lyap_sum <- lyap_sum + log(abs(mu * (1 - 2 * x)))
  }
  
  return(lyap_sum / n)
}

# Generate Lyapunov exponents for a range of r values
mu_values <- seq(2.5, 4, length.out = 500)
lyap_values <- sapply(mu_values, lyapunov_logistic)

# Plot Lyapunov exponent vs. r
df <- data.frame(mu = mu_values, lambda = lyap_values)

ggplot(df, aes(x = mu, y = lambda)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("mu") +
  ylab("Lyapunov Exponent") +
  ggtitle("Lyapunov Exponent for the logistic map") +
  theme_minimal()

