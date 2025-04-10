
library(deSolve)
library(ggplot2)
library(plotly)  # Load plotly for 3D visualization

# Define the Lorenz system
lorenz_system <- function(t, state, parameters) {
  x <- state[1]
  y <- state[2]
  z <- state[3]
  sigma <- parameters["sigma"]
  rho <- parameters["rho"]
  beta <- parameters["beta"]
  
  dxdt <- sigma * (y - x)
  dydt <- x * (rho - z) - y
  dzdt <- x * y - beta * z
  
  list(c(dxdt, dydt, dzdt))
}

# The fixed points of the system depend on sigma, rho 
# and beta
# Lets create a function to estimate their coordinates
fixed_points_df <- function(sigma, rho, beta){
  
  A = rho-1
  B = sqrt(A*beta)
  
  fixed_df <- data.frame(x = c(0, B, -B),
                         y = c(0, B, -B),
                         z = c(0, A, A)
                         )
  return(fixed_df)
}
  
                               

# Lets solve the case sigma = 10, rho = 28, beta = 8/3
# when x = 1, y = 1, z = 1, from t = 0 to t = 100

# Definition of the values of the parameters

sigma_aux = 10
rho_aux = 28
beta_aux = 8/3

parameters <- c(sigma = sigma_aux,
                rho = rho_aux, 
                beta = beta_aux)

# Fixed points
fixed_p_df <- fixed_points_df(sigma_aux, 
                              rho_aux,
                              beta_aux)


# Definition of initial conditions (or state)
initial_state <- c(x = 1, y = 1, z = 1)

# Time spam
t_max <- 100 
times <- seq(0, t_max, by = 0.01)

# Solve the system
solution <- ode(y = initial_state, times = times, func = lorenz_system, parms = parameters, method = "ode45")

solution_df <- as.data.frame(solution)

# Plot the solution 
ggplot()+
  geom_path(data = solution_df, 
            aes(x = x, y = z), color = "blue")+
  geom_point(data = fixed_p_df, 
             aes(x = x, y = z),
             size = 3, color = "red")+
  theme_bw()

# 3D Plot using plotly
plot_ly(solution_df, x = ~x, y = ~y, z = ~z, 
        type = "scatter3d", 
        mode = "lines", 
        line = list(color = "blue"))


# If we want to plot two trajectories with very similar
# initial conditions:

# Definition of a new initial state
initial_state2 <- c(x = 1.01, y = 1, z = 1)

# Solve the system for initial_state2
solution2 <- ode(y = initial_state2, times = times, func = lorenz_system, parms = parameters, method = "ode45")

solution2_df <- as.data.frame(solution2)

plot_ly() %>%
  add_trace(data = solution_df,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "blue"),
            name = "Trajectory 1") %>%
  add_trace(data = solution2_df,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "red"),
            name = "Trajectory 2") 

# During the first time steps they overlap, but then
# they follow a different path


# We could also create a function to solve and plot 
# the system

run_lorenz <- function(sigma, rho, beta, x0, y0, z0, t_max, stationary = F) {
  parameters <- c(sigma = sigma, rho = rho, beta = beta)
  initial_state <- c(x = x0, y = y0, z = z0)
  times <- seq(0, t_max, by = 0.01)
  
  # Solve the system
  solution <- ode(y = initial_state, times = times, func = lorenz_system, parms = parameters, method = "ode45")
  
  solution_df <- as.data.frame(solution)
  
  
  # If stationary == TRUE, this chunk of code
  # plots only the trajectory from 0.5 * t_max to t_max
  
  if(stationary == T){
    
    solution_df <- solution_df %>% filter(time > 0.5*t_max)
  }
  
  # 3D Plot using plotly
  p3d <- plot_ly(solution_df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines", line = list(color = "blue")) %>%
    layout(title = paste("3D Lorenz Attractor: sigma =", sigma, ", rho =", rho, ", beta =", beta),
           scene = list(xaxis = list(title = "x"),
                        yaxis = list(title = "y"),
                        zaxis = list(title = "z")))
  
  print(p3d)
}


# Try with rho < 1 (non-chaotic behavior)
run_lorenz(sigma = 10, rho = 0.5, beta = 8/3, 
           x0 = 1, y0 = 1, z0 = 1, t_max = 100)

# Try with rho < 10 (non-chaotic behavior)
run_lorenz(sigma = 10, rho = 10, beta = 8/3, 
           x0 = 1, y0 = 1, z0 = 1, t_max = 100)

# Try with standard chaotic values
run_lorenz(sigma = 10, rho = 28, beta = 8/3, 
           x0 = 1, y0 = 1, z0 = 1, t_max = 100)

# Try with periodic behavior (rho > 99)
run_lorenz(sigma = 10, rho = 99.65, beta = 8/3, 
           x0 = 1, y0 = 1, z0 = 1, t_max = 100, 
           stationary = T)
run_lorenz(sigma = 10, rho = 350, beta = 8/3, 
           x0 = 1, y0 = 1, z0 = 1, t_max = 100, 
           stationary = T)
