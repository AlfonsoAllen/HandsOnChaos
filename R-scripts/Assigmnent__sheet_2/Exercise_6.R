# Load necessary libraries
library(deSolve)
library(ggplot2)

# Define parameters
R <- 1      # Resistance in ohms
C <- 1      # Capacitance in farads
V <- 5      # Battery voltage
t_max <- 10 # Time duration

# Define the differential equation dQ/dt = arctanh(V/R - Q/RC)
dQ_dt <- function(t, Q, params) {
  dQdt <- atanh(V / R - Q / (R * C))  # Compute derivative
  list(dQdt)
}

# Initial condition (capacitor starts with no charge)
Q0 <- 0

# Time sequence
times <- seq(0, t_max, by = 0.1)

# Solve ODE
solution <- ode(y = Q0, times = times, func = dQ_dt, parms = NULL)

# Convert solution to dataframe
solution_df <- as.data.frame(solution)

# Plot the solution Q(t)
ggplot(solution_df, aes(x = time, y = Q)) +
  geom_line(color = "blue", size = 1) +
  xlab("Time (t)") +
  ylab("Charge (Q)") +
  ggtitle("Solution of dQ/dt = arctanh(V/R - Q/RC)") +
  theme_minimal()
