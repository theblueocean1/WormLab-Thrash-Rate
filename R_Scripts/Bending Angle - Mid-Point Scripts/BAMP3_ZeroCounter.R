library(tidyverse)

input_folder <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/2 Filtered"
output_folder <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/3A Thrash Count from Zeros"

# Create output folder if not exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# === PROCESS FILES ===
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# Initialize empty results dataframe
results <- data.frame(Strain = character(),
                      Replicate = character(),
                      TimeID = character(),
                      Thrash_Count = numeric(),
                      stringsAsFactors = FALSE)

for (file in csv_files) {
  cat("Processing file:", basename(file), "\n")
  
  # Extract filename and remove suffix
  filename <- basename(file)
  filename_core <- sub("\\.csv.*$", "", filename)  # remove .csv and anything after
  parts <- strsplit(filename_core, "_")[[1]]
  
  # Extract metadata
  Strain <- parts[1]
  Replicate <- parts[2]
  TimeID <- parts[3]
  
  df <- read_csv(file)
  df <- df %>% 
    pivot_longer(cols = starts_with("Track"), names_to = "Track", values_to = "BendAng") %>% 
    filter(!is.na(BendAng)) %>% 
    mutate(sign = sign(BendAng)) %>% 
    mutate(sign_lag = lag(sign), BendAng_lag = lag(BendAng))
  
  zerodf <- df %>% 
    filter(abs(BendAng_lag - BendAng) > 10) %>% 
    filter(sign != sign_lag)
  
  numzeroes <- nrow(zerodf)
  
  thrash_count = numzeroes + 1
  
  # Append to results
  results <- rbind(results, data.frame(Strain = Strain,
                                       Replicate = Replicate,
                                       TimeID = TimeID,
                                       Thrash_Count = thrash_count,
                                       stringsAsFactors = FALSE))
  
  cat("Successfully processed file.")
  
}

# Save results
output_file <- file.path(output_folder, "peak_thrash_summary.csv")
write_csv(results, output_file)

cat("Summary saved to:", output_file, "\n")

# Possibility 1: Counting 0s in the data
# We cannot do this, as the data doesn't necessarily record 0 or values near 0.
# Possibility 2: Count peak using the peak algorithm (already in use)
# Problem: I don't understand what's going on there. Also pretty mid performance.
# Possibility 3: Count the number of 0-crossings to infer number of 0s in the data.
# The number of peaks is always going to be 1 + the number of crosses.