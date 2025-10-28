# This code is JUST for visualization! You can use both cleaned and filtered data. Can't use raw though

# --- Load libraries ---
library(ggplot2)
library(tidyr)
library(dplyr)

# --- User-configurable settings ---
input_file <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/2 Filtered/VG1038_2_7.csv_cleaned_filtered.csv"  # Or set the file path directly like: "path/to/file.csv"
plot_title <- "Mid-Point Bending Angle Over Time"
x_label <- "Time (s)"
y_label <- "Bending Angle (degrees)"
line_thickness <- 0.7
track_colors <- NULL  # Set to NULL for default, or c("red", "blue", ...) for custom colors
# --- Filtering thresholds ---
min_duration <- 5.0       # in seconds (i.e., how long the track must exist) <- RECC 5
min_range <- 60.0          # in degrees (min-to-max bending angle range) <- RECC 60

# --- Read and check the data ---
data <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

# Confirm minimum structure
if (ncol(data) < 3) {
  stop("The dataset must contain at least Frame, Time, and one Track column.")
}

# --- Reshape the data: long format ---
long_data <- data %>%
  pivot_longer(cols = 3:ncol(data), names_to = "Track", values_to = "BendingAngle") %>%
  mutate(BendingAngle = as.numeric(BendingAngle))

# Calculate per-track summaries
track_stats <- long_data %>%
  group_by(Track) %>%
  summarize(
    duration = max(`Time (s)`, na.rm = TRUE) - min(`Time (s)`, na.rm = TRUE),
    range = max(BendingAngle, na.rm = TRUE) - min(BendingAngle, na.rm = TRUE),
    .groups = "drop"
  )

# Filter out bad tracks
valid_tracks <- track_stats %>%
  filter(duration >= min_duration, range >= min_range) %>%
  pull(Track)

# Keep only the valid tracks in the data
filtered_data <- long_data %>%
  filter(Track %in% valid_tracks)

# --- Plot the data using ggplot2 --- This is for plotting where color is not the group
ggplot(filtered_data, aes(x = `Time (s)`, y = BendingAngle, color = Track)) +
  geom_line(linewidth = line_thickness, na.rm = TRUE) +
  labs(
    title = NULL,
    x = x_label,
    y = y_label,
  ) +
  coord_cartesian(ylim = c(-200, 200)) +
  theme_classic()

# --- Plot the data using ggplot2 --- This is for plotting where color is the group (track)
# ggplot(filtered_data, aes(x = `Time (s)`, y = BendingAngle, color = Track)) +
  #geom_line(linewidth = line_thickness, na.rm = TRUE) +
  #labs(
    #title = NULL,
    #x = x_label,
    #y = y_label,
  #) +
  #coord_cartesian(ylim = c(-200, 200)) +
  #theme_classic()