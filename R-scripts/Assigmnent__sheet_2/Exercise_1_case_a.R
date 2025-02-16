# Load required libraries
library(deSolve)
library(ggplot2)


#----
# Plot the x_dot vs x

# Define the function dx/dt = x^2 - 1
dx_dt <- function(x) {
  return(x^2 - 1)
}

# Generate x values
x_values <- seq(-2, 2, length.out = 100)  # Adjust range as needed
dx_values <- dx_dt(x_values)

# Create a dataframe
data <- data.frame(x = x_values, dx_dt = dx_values)

# Plot dx/dt vs. x
ggplot(data, aes(x = x, y = dx_dt)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  xlab("x") +
  ylab("dx/dt") +
  ggtitle("Phase portrait: dx/dt vs. x") +
  theme_bw()

# Save plot dx/dt vs. x
png("Figures_videos/Assignment_sheet_2/Example_1_phase_portrait.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(data, aes(x = x, y = dx_dt)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  xlab("x") +
  ylab("dx/dt") +
  ggtitle("Phase portrait: dx/dt vs. x") +
  theme_bw()

dev.off()

#----
# Find all the roots of the polynomial x^2 - 1

stationary_points <- polyroot(c(-1,0,1))
stationary_points

#----
# Solve the differential equation
# Define the differential equation dx/dt = x^2 - 1
dynamical_system <- function(t, state, parameters) {
  x <- state[1]
  dxdt <- dx_dt(x)  # Differential equation
  list(dxdt)
}

# Define time range for the simulation
time <- seq(0, 5, by = 0.01)  # From t = 0 to t = 5, with small steps

# ---- CASE A: x(0) < -1 ----
initial_state_a <- c(x = -1.5)  # Initial condition x(0) < -1
solution_a <- ode(y = initial_state_a, times = time, func = dynamical_system, parms = NULL, method = "ode45")

# Convert to a data frame for ggplot2
df_a <- as.data.frame(solution_a)

# Plot Case A
ggplot(df_a, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Case A: x(0) < -1", x = "Time (t)", y = "x(t)") +
  theme_bw()

# Save plot for case A
png("Figures_videos/Assignment_sheet_2/Example_1_case_a.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(df_a, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Case A: x(0) < -1", x = "Time (t)", y = "x(t)") +
  theme_bw()

dev.off()
