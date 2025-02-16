# Load necessary libraries
library(deSolve)
library(ggplot2)


# ---- Step 1: Plot the x_dot vs x

# Define the function dx/dt = 3x - x^3
dx_dt <- function(x) {
  return(3*x - x^3)
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
png("Figures_videos/Assignment_sheet_2/Example_2_phase_portrait.png",
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



# Define the system dx/dt = 3x - x^3
dynamical_system <- function(t, state, parameters) {
  x <- state[1]
  dxdt <- dx_dt(x)  # Differential equation
  list(dxdt)
}

# ---- Step 2: Find Fixed Points ----
fixed_points <- polyroot(c(0,3,0,-1)) # Find the roots of the poly: f(x) = 3x - x^3

fixed_points

# ---- Step 3: Stability Analysis ----

f_prime <- function(x){3 - 3*x^2}  # Compute derivative f'(x)

stability_analysis <- function(fixed_points) {
  f_prime4fixed_points <- f_prime(fixed_points)
  stability <- ifelse(Re(f_prime4fixed_points) < 0, "Stable", "Unstable")  # Stability condition
  return(data.frame(Fixed_Point = fixed_points, Stability = stability))
}

stability_info <- stability_analysis(fixed_points)
stability_info

# ---- Step 4: Numerical Solution ----
# Define time range
time <- seq(0, 10, by = 0.01)  # From t = 0 to t = 10

# Initial condition: x(0) = 5
initial_state <- c(x = 5)

# Solve numerically
solution <- ode(y = initial_state, times = time, func = dynamical_system, parms = NULL, method = "ode45")

# Convert to a data frame for plotting
df_solution <- as.data.frame(solution)

# ---- Step 5: Plot the Numerical Solution ----
ggplot(df_solution, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed", color = "red") + # Stable value 
  labs(title = "Solution of dx/dt = 3x - x^3", x = "Time (t)", y = "x(t)") +
  theme_bw()


# Save plot dx/dt vs. x
png("Figures_videos/Assignment_sheet_2/Example_2_solution_x0_5.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(df_solution, aes(x = time, y = x)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed", color = "red") + # Stable value 
  labs(title = "Solution of dx/dt = 3x - x^3", x = "Time (t)", y = "x(t)") +
  theme_bw()


dev.off()
