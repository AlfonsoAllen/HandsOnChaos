# Load required libraries
library(deSolve)
library(ggplot2)

# Define the differential equation dx/dt = x^2 - 1
dynamical_system <- function(t, state, parameters) {
  x <- state[1]
  dxdt <- x^2 - 1  # Differential equation
  list(dxdt)
}

# Define time range for the simulation
time <- seq(0, 5, by = 0.01)  # From t = 0 to t = 5, with small steps

# ---- CASE A: x(0) < -1 ----
initial_state_a <- c(x = -1.5)  # Initial condition x(0) < -1
solution_a <- ode(y = initial_state_a, times = time, func = dynamical_system, parms = NULL)

# Convert to a data frame for ggplot2
df_a <- as.data.frame(solution_a)

# Plot Case A
plot_a <- ggplot(df_a, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Case A: x(0) < -1", x = "Time (t)", y = "x(t)") +
  theme_minimal()