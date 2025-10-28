# Net displacement calculator. This script takes the input folder of the filtered (by valid BAMP track) position data to find the 
# net displacement of a worm. It returns a csv file with the net displacement data of all strain replicate time.

# Load libraries
library(tidyverse)
library(dplyr)
library(stringr)

# Folder path
folder_path <- "../Modified WormLab Outputs/Position/2 Filtered (by Valid BAMP Tracks)"

# Output folder path
output_folder <- "../Modified WormLab Outputs/Position/Net displacement.csv"
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
    `Net Displacement` = double()
)

# For every file in folder loop
for (file in csv_files) {
  STRAIN = str_extract(file, "[A-Z]{2,3}\\d{4}")
  REPLICATE = as.integer(str_extract(str_extract(file, "_\\d+_"), "\\d+"))
  TIMEID = as.integer(str_extract(str_extract(file, "\\d+\\."), "\\d+"))
  
  df <- read_csv(file)
  
  # Isolate the start and end row
  start_row <- df %>% filter(`Time..s.` == 0)
  end_row   <- df %>% filter(`Time..s.` == 60)
  
  # Remove all columns with NA
  start <- start_row %>% select(where(~!any(is.na(.))))
  end <- end_row %>% select(where(~!any(is.na(.))))
  
  # Drop the first two columns for both
  start <- start %>% subset(select = -c(Frame, Time..s.))
  end <- end %>% subset(select = -c(Frame, Time..s.))
  
  # Convert this simplified data frame into a vector
  start_pos <- as.numeric(start)
  end_pos <- as.numeric(end)
  
  # Assign X_n Y_n to each component of vector so equation is easier to make
  start_x <- start_pos[1]
  start_y <- start_pos[2]
  end_x <- end_pos[1]
  end_y <- end_pos[2]
  
  # Finally the distance equation
  d <- sqrt((start_x - end_x)^2 + (start_y - end_y)^2)
  
  # Append all to the final data frame
  final_df <- add_row(
    final_df,
    Strain = STRAIN,
    Replicate = REPLICATE,
    TimeID = TIMEID,
    `Net Displacement` = d
  )
}

# Make the csv file
write_csv(final_df, output_folder, na = "NA")