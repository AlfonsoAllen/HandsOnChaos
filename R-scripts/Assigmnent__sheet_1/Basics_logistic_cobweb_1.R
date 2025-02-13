# Load necessary libraries
library(ggplot2)
library(av)

# Define the logistic function
logistic_map <- function(x, r) {
  return(r * x * (1 - x))
}

# Generate cobweb plot
cobweb_plot <- function(r, x0 = 0.2, n_iter = 50) {
  x <- numeric(n_iter)
  x[1] <- x0  # Initial condition
  
  # Iterate the logistic equation
  for (i in 2:n_iter) {
    x[i] <- logistic_map(x[i - 1], r)
  }
  
  # Generate y = f(x) line
  x_vals <- seq(0, 1, length.out = 100)
  y_vals <- logistic_map(x_vals, r)
  
  # Create data frame for plotting
  cobweb_df <- data.frame(x = x[-length(x)], y = x[-1])
  
  # Plot
  ggplot() +
    geom_line(aes(x = x_vals, y = y_vals), color = "blue", size = 1) +  # Logistic curve
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # y = x line
    geom_segment(data = cobweb_df, aes(x = x, xend = x, y = x, yend = y), color = "red") +  # Vertical lines
    geom_segment(data = cobweb_df, aes(x = x, xend = y, y = y, yend = y), color = "red") +  # Horizontal lines
    labs(title = paste("Cobweb Plot for Logistic Map (r =", r, ")"),
         x = "x_n",
         y = "x_(n+1)") +
    theme_minimal()
}

# Example: Plot for r = 0.5
cobweb_plot(r = 0.5, x0 = 0.2, n_iter = 1500)

# Example: Plot for r = 1.0
cobweb_plot(r = 1.0, x0 = 0.5, n_iter = 1500)
# Example: Plot for r = 1.0
cobweb_plot(r = 1.5, x0 = 0.25, n_iter = 150)

# Example: Plot for r = 1.0
cobweb_plot(r = 3.2, x0 = 0.25, n_iter = 1500)

# Example: Plot for r = 3.7
cobweb_plot(r = 3.7, x0 = 0.2, n_iter = 1500)
