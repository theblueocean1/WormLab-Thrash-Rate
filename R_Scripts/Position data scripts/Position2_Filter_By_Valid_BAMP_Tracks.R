# This Position filter takes valid tracks from BAMP and only keeps those since BAMP is easier to de-noise.

library(tidyverse)

# Necessary folders
bamp_folder <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/2 Filtered"
position_folder <- "../Modified WormLab Outputs/Position/1 Cleaned"
output_folder <- "../Modified WormLab Outputs/Position/2 Filtered (by Valid BAMP Tracks)"

# === MAIN FUNCTION ===
filter_position_by_bamp <- function(position_folder, bamp_folder, output_folder) {
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  position_files <- list.files(position_folder, pattern = "_cleaned\\.csv$", full.names = TRUE)
  
  for (pos_file in position_files) {
    # === MATCHING BAMP FILE ===
    base_name <- basename(pos_file) %>%
      str_replace("_cleaned\\.csv$", "_cleaned_filtered.csv")
    bamp_file <- file.path(bamp_folder, base_name)
    
    if (!file.exists(bamp_file)) {
      message("No matching BAMP file for ", base_name, "; skipping.")
      next
    }
    
    # === READ FILES ===
    pos_df <- read.csv(pos_file)
    bamp_df <- read.csv(bamp_file)
    
    # === DETERMINE TRACKS TO KEEP ===
    bamp_cols <- names(bamp_df)
    track_cols <- bamp_cols[grepl("^Track\\d+$", bamp_cols)]
    track_nums <- as.integer(str_extract(track_cols, "\\d+"))
    
    # === BUILD FILTERED POSITION DF ===
    base_cols <- pos_df[, 1:2]  # Frame, Time (s)
    worm_cols <- map(track_nums, function(track_num) {
      x_col <- paste0("x", track_num)
      y_col <- paste0("y", track_num)
      
      if (!(x_col %in% names(pos_df)) || !(y_col %in% names(pos_df))) {
        return(NULL)  # skip if worm doesn't exist in Position file
      }
      
      pos_df[, c(x_col, y_col)]
    }) %>% discard(is.null) %>% bind_cols()
    
    filtered_df <- bind_cols(base_cols, worm_cols)
    
    # === WRITE OUTPUT ===
    output_path <- file.path(output_folder, basename(pos_file))
    write.csv(filtered_df, output_path, row.names = FALSE)
    message("Saved filtered file: ", output_path)
  }
}

# === RUN ===
filter_position_by_bamp(position_folder, bamp_folder, output_folder)