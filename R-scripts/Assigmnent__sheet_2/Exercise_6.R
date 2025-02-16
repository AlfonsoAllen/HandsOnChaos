# Load necessary libraries
library(deSolve)
library(ggplot2)

# Define parameters
R <- 1      # Resistance in ohms
C <- 1.5      # Capacitance in farads
V <- .5      # Battery voltage
t_max <- 10 # Time duration


#---- Step 1: Plot the x_dot vs x

# Define the function dx/dt = exp(x) - 10*cos(x)
dQ_dt <- function(Q, V, R, C) {
  return( atanh(V / R - Q / (R * C)))
}

# Generate x values
Q_values <- seq(-.5, 1.5, length.out = 300)  # Adjust range as needed
dQ_values <- dQ_dt(Q_values, V, R, C)

# Create a dataframe
data <- data.frame(Q = Q_values, dQ_dt = dQ_values)


# Plot dQ/dt vs. Q
ggplot(data, aes(x = Q, y = dQ_dt)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  labs(x="Q", y = "dQ/dt", color = NULL) +
  ggtitle("Phase portrait: dQ/dt vs. Q") +
  theme_bw()

# Save plot dx/dt vs. x
png("Figures_videos/Assignment_sheet_2/Example_6_nonlinear_resistor.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(data, aes(x = Q, y = dQ_dt)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  labs(x="Q", y = "dQ/dt", color = NULL) +
  ggtitle("Phase portrait: dQ/dt vs. Q") +
  theme_bw()

dev.off()




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
colnames(solution_df) <- c("time", "Q")


# Linear solution:

linear_solution_df <- solution_df
linear_solution_df$Q <- V*C*(1-exp(-R*C*linear_solution_df$time))

# Plot the solution Q(t)
ggplot(solution_df, aes(x = time, y = Q)) +
  geom_line(color = "blue", size = 1) +
  geom_line(data = linear_solution_df, aes(x = time, y = Q), color = "red", linetype = "dashed", size = 1) + # Add linear solution
  geom_hline(yintercept = V*C, color = "black", linetype = "dotted", size = 1) + # Add stationary value
  xlab("Time (t)") +
  ylab("Charge (Q)") +
  ggtitle("Solution of dQ/dt = arctanh(V/R - Q/RC) [blue line]") +
  theme_bw()

# Save plot dx/dt vs. x
png("Figures_videos/Assignment_sheet_2/Example_6_charge_nonlinear_resistor.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(solution_df, aes(x = time, y = Q)) +
  geom_line(color = "blue", size = 1) +
  geom_line(data = linear_solution_df, aes(x = time, y = Q), color = "red", linetype = "dashed", size = 1) + # Add linear solution
  geom_hline(yintercept = V*C, color = "black", linetype = "dotted", size = 1) + # Add stationary value
  xlab("Time (t)") +
  ylab("Charge (Q)") +
  ggtitle("Solution of dQ/dt = arctanh(V/R - Q/RC) [blue line]") +
  theme_bw()

dev.off()
