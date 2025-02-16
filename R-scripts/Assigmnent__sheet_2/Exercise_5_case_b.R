# Load required libraries
library(deSolve)
library(ggplot2)


#---- Step 1: Plot the x_dot vs x

# Define the function dx/dt = exp(x) - 10*cos(x)
dx_dt <- function(x) {
  return( exp(x) - 10 * cos(x))
}

# Generate x values
x_values <- seq(-3*pi, 2.5, length.out = 300)  # Adjust range as needed
dx_values <- dx_dt(x_values)

# Create a dataframe
data_aux1 <- data.frame(x = x_values, funct = dx_values, type = "dx_dt")
data_aux2 <- data.frame(x = x_values, funct = exp(x_values), type = "exp(x)")
data_aux3 <- data.frame(x = x_values, funct = - 10*cos(x_values), type = "- 10*cos(x)")

data <- bind_rows(data_aux1, data_aux2, data_aux3)

# Plot dx/dt vs. x
ggplot(data, aes(x = x, y = funct, color = as.factor(type))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  labs(x="x", y = "dx/dt", color = NULL) +
  ggtitle("Phase portrait: dx/dt vs. x") +
  theme_bw()

# Save plot dx/dt vs. x
png("Figures_videos/Assignment_sheet_2/Example_5_phase_portrait_case_b.png",
    width = 500*4, # The width of the plot in inches
    height = 520*4, res=300)

ggplot(data, aes(x = x, y = funct, color = as.factor(type))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Equilibrium points
  labs(x="x", y = "dx/dt", color = NULL) +
  ggtitle("Phase portrait: dx/dt vs. x") +
  theme_bw()

dev.off()

#---- Step 2: Find the roots of exp(x) - 10 * cos(x)

# Find a root in a given interval (e.g., near x = 0)
fixed_point_0 <- uniroot(dx_dt, interval = c(0, 2))$root
print(fixed_point_0)  # Should return approximately 0 (first root)

fixed_point_1 <- uniroot(dx_dt, interval = c(-2, 0))$root
print(fixed_point_1)  # Should return approximately pi (second root)

some_fixed_points <- c(fixed_point_0, fixed_point_1, -0.5*pi*c(2:10))

# ---- Step 3: Stability Analysis ----

f_prime <- function(x){exp(-x) + 10*sin(x)}  # Compute derivative f'(x)

stability_analysis <- function(fixed_points) {
  f_prime4fixed_points <- f_prime(fixed_points)
  stability <- ifelse(Re(f_prime4fixed_points) < 0, "Stable", "Unstable")  # Stability condition
  return(data.frame(Fixed_Point = fixed_points, Stability = stability))
}

stability_info <- stability_analysis(some_fixed_points)
stability_info
