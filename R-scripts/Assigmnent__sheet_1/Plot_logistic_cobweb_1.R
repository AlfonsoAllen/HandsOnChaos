# Load necessary libraries
library(tidyverse)


# Define the logistic function
logistic_map <- function(x, mu) {
  return(mu * x * (1 - x))
}

# Generate cobweb plot
cobweb_plot <- function(mu, x0 = 0.2, n_iter = 50) {
  x <- numeric(n_iter)
  x[1] <- x0  # Initial condition
  
  # Iterate the logistic equation
  for (i in 2:n_iter) {
    x[i] <- logistic_map(x[i - 1], mu)
  }
  
  # Generate y = f(x) line
  x_vals <- seq(0, 1, length.out = 100)
  y_vals <- logistic_map(x_vals, mu)
  
  # Create data frame for plotting
  cobweb_df <- data.frame(x = x[-length(x)], y = x[-1])
  
  # Plot
  ggplot() +
    geom_line(aes(x = x_vals, y = y_vals), color = "blue", linewidth = 1) +  # Logistic curve
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # y = x line
    geom_segment(data = cobweb_df, aes(x = x, xend = x, y = x, yend = y), color = "red") +  # Vertical lines
    geom_segment(data = cobweb_df, aes(x = x, xend = y, y = y, yend = y), color = "red") +  # Horizontal lines
    labs(title = paste("Cobweb Plot for Logistic Map (mu =", mu, ")"),
         x = "x_n",
         y = "x_(n+1)") +
    theme_minimal()
}

# Plot and save some cobweb plots

# Example: Plot for mu = 0.5

cobweb_plot(mu = 0.5, x0 = 0.2, n_iter = 1500)

# Code to save the above figure
png("Figures_videos/cobweb_plot_logistic_map_mu_0p5.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

cobweb_plot(mu = 0.5, x0 = 0.2, n_iter = 1500)

dev.off()

# Example: Plot for mu = 1.0
png("Figures_videos/cobweb_plot_logistic_map_mu_1p0.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

cobweb_plot(mu = 1.0, x0 = 0.5, n_iter = 1500)

dev.off()


# Example: Plot for mu = 1.5
png("Figures_videos/cobweb_plot_logistic_map_mu_1p5.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

cobweb_plot(mu = 1.5, x0 = 0.25, n_iter = 150)

dev.off()

# Example: Plot for mu = 3.2
png("Figures_videos/cobweb_plot_logistic_map_mu_3p2.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

cobweb_plot(mu = 3.2, x0 = 0.25, n_iter = 1500)

dev.off()

# Example: Plot for mu = 3.7
png("Figures_videos/cobweb_plot_logistic_map_mu_3p7.png",
width = 500*4, # The width of the plot in inches
height = 520*4, res=300)

cobweb_plot(mu = 3.7, x0 = 0.2, n_iter = 1500)

dev.off()
