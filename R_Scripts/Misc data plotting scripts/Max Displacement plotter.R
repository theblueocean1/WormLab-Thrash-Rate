# This code takes the "Max Displacement" csv file to visualize an averaged plot with appropriate statistics

# Import libraries
library(tidyverse)

# Load csv file
csv_file_path <- "../Modified WormLab Outputs/Position/Max displacement.csv"
df <- read_csv(csv_file_path)


# Filter out NAs
df_NA_filtered <- df %>% 
  filter(!is.na(`Max Displacement`))

# Group by Strain and TimeID, then summarise results
summary_df <- df_NA_filtered %>% 
  group_by(Strain, TimeID) %>% 
  summarise(`Avg Max Displacement` = mean(`Max Displacement`), SD = sd(`Max Displacement`), n = n(), SEM = SD / sqrt(n)) %>% 
  mutate(
    TimeMin = recode(TimeID, "1" = 0, "2" = 10, "3" = 20, "4" = 30, "5" = 40, "6" = 50, "7" = 60, "8" = 70, "9" = 80, "10" = 90),
    TimeMin = as.numeric(TimeMin)
  )

# ANOVA at time 10 min
summary_df_t10min <- summary_df %>% 
  filter(TimeMin == 10)
d_anova_model <- aov(`Avg Max Displacement` ~ Strain, data = summary_df_t10min)
d_anova_pval <- summary(d_anova_model)[[1]][["Pr(>F)"]][1]  # extract p-value

# === Define label mapping with expressions ===
strain_labels <- c(
  "XMN1408" = "wild-type",
  "VG1038" = expression(italic("dat-1")),
  "VG1049" = expression(italic("cat-2"))
)

# Plot
ggplot(summary_df, aes(x = TimeMin, y = `Avg Max Displacement`, color = Strain)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `Avg Max Displacement` - SEM,
                    ymax = `Avg Max Displacement` + SEM),
                width = 2, linewidth = 0.5) +
  # annotate("rect", xmin = 4, xmax = 16, ymin = -Inf, ymax = Inf,
  # alpha = 0.3, fill = "darkgray") +
  # annotate("text", x = 12.5, y = max(summary_df$`Avg Max Displacement`, na.rm = TRUE) * 0.18,
  # label = "ANOVA p = NA", size = 4, fontface = "italic") +
  # annotate("text", x = 12.5, y = max(summary_df$`Avg Max Displacement`, na.rm = TRUE) * 0.13,
  # label = "N = NA", size = 4) +
  scale_color_manual(values = c("XMN1408" = "red", "VG1038" = "limegreen", "VG1049" = "blue"), labels = strain_labels) +
  labs(
    x = "Time (min)",
    y = expression("Max Displacement Rate ("*min^{-1}*")"),
    title = NULL
  ) +
  theme_classic()
