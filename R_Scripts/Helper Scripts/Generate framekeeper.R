# === USER INPUT ===
STRAINS <- c("XMN1408", "VG1038", "VG1049")       # Replace/add strains as needed
REPLICATES <- 1:4                        # Replace or adjust range as needed
TIMES <- 1:10    # Replace with actual number of timeIDs. For example, if you are tracking for 1 min every 10 min for 91 mins, input 1:9 instead of c(1).

# === Create All Combinations ===
df <- expand.grid(
  Strain = STRAINS,
  Replicate = REPLICATES,
  TimeID = TIMES
)

# === Add Empty START_FRAME and END_FRAME Columns ===
df$`Start Frame` <- NA_integer_
df$`End Frame` <- NA_integer_

# === Optional: Arrange for Readability ===
df <- df[order(df$Strain, df$Replicate, df$TimeID), ]

# === Write to CSV ===
write.csv(df, file = "Framekeeper/Test_MOR1_Framekeeper.csv", row.names = FALSE)

cat("CSV created: all_start_end_frames.csv\n")