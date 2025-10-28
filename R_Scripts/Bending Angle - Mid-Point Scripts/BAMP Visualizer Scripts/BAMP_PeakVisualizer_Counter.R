# This script visualizes the peaks of the mid-point bending angle data, as well 
# as spit out a data frame that has a summary of the number of thrashing including
# other useful information.

# === Libraries ===
library(ggplot2)
library(tidyr)
library(dplyr)

# === Settings ===
input_file <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/3 Peaks/VG1049_3_1.csv_cleaned_filtered_peaks.csv"   # 🔁 Replace with your actual peaks file
plot_title <- "Local Maxima and Minima of Tracks"
x_label <- "Time (s)"
y_label <- "Bending Angle (°)"

# === Load Data ===
data <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

# Ensure numeric
data <- data %>%
  mutate(across(everything(), ~suppressWarnings(as.numeric(.))))

# === Reshape Data to Long Format ===
long_peaks <- data %>%
  pivot_longer(cols = -c(Frame, `Time (s)`),
               names_to = "Track_Type",
               values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(
    Track = sub("(_max|_min)$", "", Track_Type),
    Type = ifelse(grepl("_max$", Track_Type), "Max", "Min")
  )

# Plotting
ggplot(long_peaks, aes(x = `Time (s)`, y = Value, color = Track, shape = Type)) +
  geom_point(size = 2) +
  labs(
    title = plot_title,
    x = x_label,
    y = y_label,
    color = "Track",
    shape = "Peak Type"
  ) +
  theme_classic()

# Compute summary stats
Max_Count <- long_peaks %>% 
  filter(Type == "Max") %>% 
  count(Type)
Max_Count <- Max_Count[["n"]]

Min_Count <- long_peaks %>% 
  filter(Type == "Min") %>% 
  count(Type)
Min_Count <- Min_Count[["n"]]
Thrash_Count <- Max_Count + Min_Count

# Print data frame with summary stats
tribble(
  ~"Max Count", ~"Min Count", ~"Thrash Count",
  Max_Count, Min_Count, Thrash_Count
)

message("!!!If there is a big discrepancy between the Max Count and Min Count, check plot & make sure thrash count is correctly calculated!!!")
