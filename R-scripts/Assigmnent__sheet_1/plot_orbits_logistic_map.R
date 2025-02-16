
# Load necessary library
library(ggplot2)

# Logistic map function
logistic_map <- function(mu, x0, n) {
  x_values <- numeric(n)
  x_values[1] <- x0
  for (i in 2:n) {
    x_values[i] <- mu * x_values[i-1] * (1 - x_values[i-1])
  }
  return(data.frame(n = 1:n, x = x_values, mu = mu, x0 = x0))
}

# Parameters
n <- 50  # Number of iterations
x0_values <- c(0.2, 0.5, 0.7)  # Different initial conditions
mu_values <- c(2.8, 3.2, 3.5, 3.9)  # Different growth rates

# Generate orbits for different values of r and x0
orbit_data <- data.frame()
for (mu in mu_values) {
  for (x0 in x0_values) {
    orbit_data <- rbind(orbit_data, logistic_map(mu, x0, n))
  }
}

# Plot orbits

orbit_data$x0_label <- (paste0("x[0]:", orbit_data$x0)) # Create facet_grid labels
orbit_data$mu_label <- (paste0("mu:", orbit_data$mu))

ggplot(orbit_data, aes(x = n, y = x, color = factor(mu))) +
  geom_line(size = 1) +
  facet_grid(x0_label~mu_label, scales = "free_y", labeller = "label_parsed") +
  xlab("Iteration (n)") +
  ylab("x_n") +
  ggtitle("Orbits of the logistic map for different values of mu and x0") +
  scale_color_discrete(name = "mu values") +
  theme_minimal()


png("Figures_videos/Assignment_sheet_1/orbits_logistic_map.png",
    width = 500*6, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(orbit_data, aes(x = n, y = x, color = factor(mu))) +
  geom_line(size = 1) +
  facet_grid(x0_label~mu_label, scales = "free_y", labeller = "label_parsed") +
  xlab("Iteration (n)") +
  ylab("x_n") +
  ggtitle("Orbits of the logistic map for different values of mu and x0") +
  scale_color_discrete(name = "mu values") +
  theme_minimal()

dev.off()
