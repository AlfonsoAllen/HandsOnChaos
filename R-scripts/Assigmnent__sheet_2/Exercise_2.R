# Load necessary libraries
library(deSolve)
library(ggplot2)

# Define the system dx/dt = 3x - x^3
dynamical_system <- function(t, state, parameters) {
  x <- state[1]
  dxdt <- 3*x - x^3  # Differential equation
  list(dxdt)
}

# ---- Step 1: Find Fixed Points ----
fixed_points <- polyroot(c(0,3,0,-1)) # Find the roots of the poly: f(x) = 3x - x^3

fixed_points

# ---- Step 2: Stability Analysis ----

f_prime <- function(x){3 - 3*x^2}  # Compute derivative f'(x)

stability_analysis <- function(fixed_points) {
  f_prime4fixed_points <- f_prime(fixed_points)
  stability <- ifelse(Re(f_prime4fixed_points) < 0, "Stable", "Unstable")  # Stability condition
  return(data.frame(Fixed_Point = fixed_points, Stability = stability))
}

stability_info <- stability_analysis(fixed_points)
stability_info

# ---- Step 3: Numerical Solution ----
# Define time range
time <- seq(0, 10, by = 0.01)  # From t = 0 to t = 10

# Initial condition: x(0) = 5
initial_state <- c(x = 5)

# Solve numerically
solution <- ode(y = initial_state, times = time, func = dynamical_system, parms = NULL)

# Convert to a data frame for plotting
df_solution <- as.data.frame(solution)

# ---- Step 4: Plot the Numerical Solution ----
ggplot(df_solution, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Solution of dx/dt = 3x - x^3", x = "Time (t)", y = "x(t)") +
  theme_minimal()