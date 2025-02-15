# Load necessary libraries
library(ggplot2)
library(av)

# Define the logistic function
logistic_map <- function(x, mu) {
  return(mu * x * (1 - x))
}

# Define cobweb plot function
cobweb_plot <- function(mu, x0 = 0.2, n_iter = 100) {
  x <- numeric(n_iter)
  x[1] <- x0  # Initial condition
  
  for (i in 2:n_iter) {
    x[i] <- logistic_map(x[i - 1], mu)
  }
  
  # Generate y = f(x) line
  x_vals <- seq(0, 1, length.out = 100)
  y_vals <- logistic_map(x_vals, mu)
  
  # Data frame for cobweb lines
  cobweb_df <- data.frame(x = x[-length(x)], y = x[-1])
  
  # Create ggplot
  p <- ggplot() +
    geom_line(aes(x = x_vals, y = y_vals), color = "blue", size = 1) +  # Logistic curve
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # y = x line
    geom_segment(data = cobweb_df, aes(x = x, xend = x, y = x, yend = y), color = "red") +  # Vertical
    geom_segment(data = cobweb_df, aes(x = x, xend = y, y = y, yend = y), color = "red") +  # Horizontal
    labs(title = paste("Cobweb Plot (mu =", round(mu, 2), ")"),
         x = "x_n", y = "x_(n+1)") +
    theme_minimal()
  
  print(p)  # Important: Print the plot so av_capture_graphics() can record it
}

# Path and name of the video file
output_video <- "Figures_videos/video_cobweb_logistic_map.mp4"

# Vector with the values of r
mu_values <- seq(0, 4, by = 0.01)  # More gradual changes

# Record video
frame_count <- 0

av::av_capture_graphics({
  for (i in 1:length(mu_values)) {
    
    cobweb_plot(mu = mu_values[i], x0 = 0.2, n_iter = 500)
    
    frame_count <- frame_count + 1  
    cat("Frame", frame_count, "added\n")  
    Sys.sleep(0.05)  # Optional: Small delay to avoid excessive CPU usage
  }
}, width = 640, height = 480, framerate = 10, output = output_video)

cat("Video saved to", output_video, "\n")
