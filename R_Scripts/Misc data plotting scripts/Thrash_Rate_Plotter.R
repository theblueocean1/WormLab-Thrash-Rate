library(tidyverse)

# === Data input ===
data_path <- "../Modified WormLab Outputs/Bending Angle - Mid-Point/4 Thrash Count/peak_thrash_summary.csv"
df <- read_csv(data_path)

# Data wrangling & summarise
df_1 <- df %>% 
  select(-Max_Count, -Min_Count) %>% 
  mutate(
    TimeMin = dplyr::recode(TimeID, "1" = 0, "2" = 10, "3" = 20, "4" = 30, "5" = 40, "6" = 50, "7" = 60, "8" = 70, "9" = 80, "10" = 90),
    TimeMin = as.numeric(TimeMin)
  )
df_summary <- df %>% 
  group_by(Strain, TimeID) %>% 
  select(-Max_Count, -Min_Count) %>% 
  summarise(
    `Avg Thrash Rate` = mean(Thrash_Count),
    SD = sd(Thrash_Count),
    n = n(),                                  # count worms in each group
    SEM = sd(Thrash_Count) / sqrt(n())
  ) %>% 
  mutate(
    TimeMin = dplyr::recode(TimeID, "1" = 0, "2" = 10, "3" = 20, "4" = 30, "5" = 40, "6" = 50, "7" = 60, "8" = 70, "9" = 80, "10" = 90),
    TimeMin = as.numeric(TimeMin)
  )

# === ANOVA at Time == 10 ===
df_t10 <- df_1 %>% filter(TimeMin == 10)

anova_model <- aov(`Thrash_Count` ~ Strain, data = df_t10)
anova_pval <- summary(anova_model)[[1]][["Pr(>F)"]][1]  # extract p-value

# Bonferroni not needed unless post-hoc multiple comparisons are done
p_label <- paste0("ANOVA p = ", signif(anova_pval, 3))

# === Define label mapping with expressions ===
strain_labels <- c(
  "XMN1408" = "wild-type (tgMOR)",
  "VG1038" = expression(italic("dat-1")),
  "VG1049" = expression(italic("cat-2"))
)

# === Plot ===
ggplot(df_summary, aes(x = TimeMin, y = `Avg Thrash Rate`, color = Strain)) +
  # annotate("rect", xmin = 4, xmax = 16, ymin = -Inf, ymax = Inf,
           # alpha = 0.3, fill = "darkgray") +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = `Avg Thrash Rate` - SEM, ymax = `Avg Thrash Rate` + SEM), width = 1) +
  # annotate("text", x = 13, y = 9, 
           # label = p_label, size = 4, hjust = 0.5) +
  # annotate("text", x = 13, y = 1, label = "N = 2", size = 4, hjust = 0.5) +
  scale_color_manual(values = c("XMN1408" = "red", "VG1038" = "limegreen", "VG1049" = "blue"), labels = strain_labels) +
  labs(
    title = NULL,
    y = "Thrashes/min",
    x = "Time (min)"
  ) +
  expand_limits(y = 0) +
  theme_classic()
