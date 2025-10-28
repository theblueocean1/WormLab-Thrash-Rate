# This Position visualizer takes the position data then finds the midpoint of all points using a median.
# Then, it extends the plot to "buffer" length in both x and y axis. Keeping the buffer a consistent
# number is crucial for visualization, as changing the buffer will distort the axis.
# This is very useful to visualize relative movement. Worms that move less will appear smaller since
# they use the same axis absolute size.

# This script takes the filtered (by valid BAMP tracks) position data to generate the plot.

# Load library
library(tidyverse)

# === Step 1: Load ONE selected file ===
file_path <- "../Modified WormLab Outputs/Position/2 Filtered (by Valid BAMP Tracks)/VG1038_1_1.csv_cleaned.csv"
df <- read.csv(file_path)

# === Step 2: Get worm columns ===
num_worms <- (ncol(df) - 2) / 2
worm_names <- paste0("Worm", 1:num_worms)

# === Step 3: Reshape to long format ===
long_df <- map_dfr(1:num_worms, function(i) {
  tibble(
    Frame = df$Frame,
    Time = df$`Time..s.`,
    Worm = worm_names[i],
    x = df[[2 * i + 1]],
    y = df[[2 * i + 2]]
  )
})

# === Step 4: Compute midpoint of all positions in this file ===
mid_x <- median(long_df$x, na.rm = TRUE)
mid_y <- median(long_df$y, na.rm = TRUE)

# === Step 5: Define axis buffer (tweak this!)
buffer <- 1500

x_limits <- c(mid_x - buffer, mid_x + buffer)
y_limits <- c(mid_y - buffer, mid_y + buffer)

# === Step 6: Plot using centered axis ===
plottitle <- "Worm Movement Paths (Centered View)"

ggplot(long_df, aes(x = x, y = y, color = Time)) +
  geom_path(alpha = 0.9) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  scale_color_gradientn(colors = c("darkgreen", "green"))