
# Load necessary libraries
library(tidyverse)

# Define the logistic map function
logistic_map <- function(x, mu) {
  return(mu * x * (1 - x))
}

# Generate bifurcation data
generate_bifurcation <- function(mu_min, mu_max, mu_steps, x0 = 0.5, n_iter = 1000, n_keep = 300) {
  mu_values <- seq(mu_min, mu_max, length.out = mu_steps)
  bifurcation_data <- data.frame()
  
  for (mu in mu_values) {
    x <- numeric(n_iter)
    x[1] <- x0  # Initial condition
    
    # Iterate the logistic equation
    for (i in 2:n_iter) {
      x[i] <- logistic_map(x[i - 1], mu)
    }
    
    # Store only the last 'n_keep' points (to remove transients)
    temp_df <- data.frame(mu = rep(mu, n_keep), x = tail(x, n_keep))
    bifurcation_data <- rbind(bifurcation_data, temp_df)
  }
  
  return(bifurcation_data)
}

# Generate the bifurcation data
bif_data <- generate_bifurcation(mu_min = 2.5, mu_max = 4, mu_steps = 500)


png("Figures_videos/Assignment_sheet_1/bifurcation_logistic_map.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

# Plot the bifurcation diagram
ggplot(bif_data, aes(x = mu, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation Diagram of the Logistic Map",
       x = "mu (Growth Rate Parameter)",
       y = "Stable Values of x") +
  theme_minimal()

dev.off()


# Generate the bifurcation data to show to the 2-period and higher-period regions
bif_data2 <- generate_bifurcation(mu_min = 3, mu_max = 3.57, mu_steps = 500)


# Plot and save the bifurcation diagram

png("Figures_videos/Assignment_sheet_1/bifurcation_logistic_map_2period_detail.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(bif_data2, aes(x = mu, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation Diagram of the Logistic Map\nshowing the 2-period and higher-period regions",
       x = "mu (Growth Rate Parameter)",
       y = "Stable Values of x") +
  scale_x_log10() + 
  scale_y_log10() +
  theme_minimal()

dev.off()

# Generate the bifurcation data to show to the 4-period and higher-period regions
bif_data3 <- generate_bifurcation(mu_min = 3.449, mu_max = 3.57, mu_steps = 500)

# Plot and save the bifurcation diagram

png("Figures_videos/bifurcation_logistic_map_4period_detail.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(bif_data3, aes(x = mu, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation Diagram of the Logistic Map\nshowing to the 4-period and higher-period regions",
       x = "mu (Growth Rate Parameter)",
       y = "Stable Values of x") +
  scale_x_log10() + 
  scale_y_log10() +
  theme_minimal()

dev.off()
