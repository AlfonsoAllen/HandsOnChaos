
# Load necessary libraries
library(tidyverse)

# Define the tent map function
tent_map <- function(x, mu) {
  return(min(mu * x, mu * (1 - x)))
}

# Generate bifurcation data
generate_bifurcation <- function(mu_min, mu_max, mu_steps, x0 = 0.5, n_iter = 1000, n_keep = 300) {
  mu_values <- seq(mu_min, mu_max, length.out = mu_steps)
  bifurcation_data <- data.frame()
  
  for (mu in mu_values) {
    x <- numeric(n_iter)
    x[1] <- x0  # Initial condition
    
    # Iterate the tent equation
    for (i in 2:n_iter) {
      x[i] <- tent_map(x[i - 1], mu)
    }
    
    # Store only the last 'n_keep' points (to remove transients)
    temp_df <- data.frame(mu = rep(mu, n_keep), x = tail(x, n_keep))
    bifurcation_data <- rbind(bifurcation_data, temp_df)
  }
  
  return(bifurcation_data)
}

# Generate the bifurcation data
bif_data <- generate_bifurcation(mu_min = 0.5, mu_max = 1.5, mu_steps = 500)


png("Figures_videos/Assignment_sheet_1/bifurcation_tent_map.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

# Plot the bifurcation diagram
ggplot(bif_data, aes(x = mu, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation diagram of the tent map",
       x = "mu (parameter)",
       y = "Stable Values of x") +
  theme_minimal()

dev.off()


bif_data2 <- generate_bifurcation(mu_min = 1, mu_max = 1.2, mu_steps = 500)


png("Figures_videos/Assignment_sheet_1/bifurcation_tent_map.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

# Plot the bifurcation diagram
ggplot(bif_data2, aes(x = mu, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Detail of the bifurcation diagram of the tent map",
       x = "mu (parameter)",
       y = "Stable Values of x") +
  theme_minimal()

dev.off()
