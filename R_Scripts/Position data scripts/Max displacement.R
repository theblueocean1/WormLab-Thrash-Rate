# Load libraries
library(tidyverse)
library(dplyr)
library(stringr)

# Folder path
folder_path <- "../Modified WormLab Outputs/Position/2 Filtered (by Valid BAMP Tracks)"

# Outputs path
output_folder <- "../Modified WormLab Outputs/Position/Max displacement.csv"
if (!dir.exists(dirname(output_folder))) {
  dir.create(dirname(output_folder), recursive = TRUE)
}

# Get all files
csv_files <- list.files(folder_path, full.names = TRUE)

# Set up the final tibble that will be generated into a csv file
final_df <- tibble(
  Strain = character(),
  Replicate = integer(),
  TimeID = integer(),
  `Max Displacement` = double()
)

# For every file in folder loop
for (file in csv_files) {
  STRAIN = str_extract(file, "[A-Z]{2,3}\\d{4}")
  REPLICATE = as.integer(str_extract(str_extract(file, "_\\d+_"), "\\d+"))
  TIMEID = as.integer(str_extract(str_extract(file, "\\d+\\."), "\\d+"))
  
  df <- read_csv(file)
  
  # Isolate the start row
  start_row <- df %>% filter(`Time..s.` == 0)
  # Remove all columns with NA
  start <- start_row %>% select(where(~!any(is.na(.))))
  # Drop the first two columns for both
  start <- start %>% subset(select = -c(Frame, Time..s.))
  # Convert this simplified data frame into a vector
  start_pos <- as.numeric(start)
  # Assign X_n Y_n to each component of vector so equation is easier to make
  start_x <- start_pos[1]
  start_y <- start_pos[2]
  
new_df <- df %>% 
    mutate(d = sqrt((start_x - df[[3]])^2 + (start_y - df[[4]])^2))

displacements <- vector("double", length = ncol(df))
for (i in seq_along(df)) {
  displacements[i] = sqrt((start_x - df[[i + 2]])^2 + (start_y - df[[i + 3]])^2)
}
  
  # Append all to the final data frame
  final_df <- add_row(
    final_df,
    Strain = STRAIN,
    Replicate = REPLICATE,
    TimeID = TIMEID,
    `Max Displacement` = max(new_df$d)
  )
}

# Make the csv file
write_csv(final_df, output_folder, na = "NA")