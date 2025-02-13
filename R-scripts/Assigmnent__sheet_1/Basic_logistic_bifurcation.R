
# Load necessary libraries
library(ggplot2)

# Define the logistic map function
logistic_map <- function(x, r) {
  return(r * x * (1 - x))
}

# Generate bifurcation data
generate_bifurcation <- function(r_min, r_max, r_steps, x0 = 0.5, n_iter = 1000, n_keep = 300) {
  r_values <- seq(r_min, r_max, length.out = r_steps)
  bifurcation_data <- data.frame()
  
  for (r in r_values) {
    x <- numeric(n_iter)
    x[1] <- x0  # Initial condition
    
    # Iterate the logistic equation
    for (i in 2:n_iter) {
      x[i] <- logistic_map(x[i - 1], r)
    }
    
    # Store only the last 'n_keep' points (to remove transients)
    temp_df <- data.frame(r = rep(r, n_keep), x = tail(x, n_keep))
    bifurcation_data <- rbind(bifurcation_data, temp_df)
  }
  
  return(bifurcation_data)
}

# Generate the bifurcation data
bif_data <- generate_bifurcation(r_min = 2.5, r_max = 4, r_steps = 500)

# Plot the bifurcation diagram
ggplot(bif_data, aes(x = r, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation Diagram of the Logistic Map",
       x = "r (Growth Rate Parameter)",
       y = "Stable Values of x") +
  theme_minimal()


# Generate the bifurcation data
bif_data2 <- generate_bifurcation(r_min = 3, r_max = 3.57, r_steps = 500)

# Plot the bifurcation diagram
ggplot(bif_data2, aes(x = r, y = x)) +
  geom_point(color = "black", alpha = 0.2, size = 0.1) +
  labs(title = "Bifurcation Diagram of the Logistic Map",
       x = "r (Growth Rate Parameter)",
       y = "Stable Values of x") +
  scale_x_log10() + 
  scale_y_log10() +
  theme_minimal()
