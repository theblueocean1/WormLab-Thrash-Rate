library(tidyverse)
library(ez)
library(modelr)
library(ggpubr)
library(car)
library(splines)
library(pracma)
library(lme4)
library(lmerTest)
library(emmeans)

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
df2 <- df_1 %>% 
  arrange(Strain, Replicate, TimeID)
df2g <- df2 %>% 
  group_by(Strain, TimeID) %>% 
  summarise(mtc = mean(Thrash_Count)) %>% 
  mutate(
    TimeMin = dplyr::recode(TimeID, "1" = 0, "2" = 10, "3" = 20, "4" = 30, "5" = 40, "6" = 50, "7" = 60, "8" = 70, "9" = 80, "10" = 90)) %>%
    mutate(TimeMin = as.numeric(TimeMin))

# Let's see what linear models are the best at describing what the hell I am
# seeing here.
lmod <- lm(Thrash_Count ~ Strain * TimeID, data = df2)
nsmod <- lm(Thrash_Count ~ Strain * ns(TimeID, df = 2), data = df2)
# df more than 2 just destroys the p-value upwards so it's not useful. Also
# we shouldn't be fitting more than 2 cubic curves anyways, it's unnecessary
# since we don't even know what the true df should be - 2 describes it fine.

# Interpretations
summary(lmod)
# VG1049 & XMN1408 thrash rate significantly different from VG1038
# Time significantly affects thrash count from baseline (0)
# All strains change with time not significantly compared to baseline
# i.e., all strains' rate of change of thrash rate is not different from each
# other.
summary(nsmod)
# VG1049 & XMN1408 thrash rate significantly different from VG1038
# Thrash rate changes with the first basis spline function significantly
# Thrash rate does not change with the second basis spline function significantly
# All rate of change of thrash rate is not significantly different

dflmod <- df2 %>% 
  add_predictions(lmod)
dfnsmod <- df2 %>% 
  add_predictions(nsmod)

# Just linear model
jlm <- ggplot(dflmod) +
  geom_point(aes(TimeMin, Thrash_Count, color = Strain)) +
  geom_point(aes(TimeMin, pred, color = Strain), size = 6) +
  ggtitle('"Linear" model')

# Linear model with TimeMin as nsplines with df
nsjlm <- ggplot(dfnsmod) +
  geom_point(aes(TimeMin, Thrash_Count, color = Strain)) +
  geom_point(aes(TimeMin, pred, color = Strain), size = 6) +
  ggtitle("Splines model (Time splined), df = 2")

# Just means
jmeans <- ggplot(df2) +
  geom_point(aes(TimeMin, Thrash_Count, color = Strain)) +
  geom_point(aes(TimeMin, mtc, color = Strain), size = 6, data = df2g) +
  ggtitle("Means")

# Look at this one for easy reference
ggarrange(jlm, nsjlm, jmeans, jlmm)

# =================== TWO-WAY REPEATED MEASURES ANOVA ===================
Anova(lmod, type = "III") # Anova() accounts for unbalanced n... aov() doesn't
Anova(nsmod, type = "III") # Type III accounts for unbalanced n.
summary(aov(lmod)) # Assuming a "balanced enough" n, we get good p-val
summary(aov(nsmod)) # Very similar p-vals

# ================= Linear Mixed-Effects STUFF FROM HERE =================
lmm <- lmer(Thrash_Count ~ Strain * TimeID + (1 | Replicate), data = df2)
anova(lmm)
Anova(lmm, type = "III") # Accounting for unbalanced n

dflmm <- df2 %>% 
  add_predictions(lmm)

jlmm <- ggplot(dflmm) +
  geom_point(aes(TimeMin, Thrash_Count, color = Strain)) +
  geom_point(aes(TimeMin, pred, color = Strain), size = 4) +
  ggtitle("Linear Mixed-Effects Model")

# Look at this one for easy reference
ggarrange(jlm, nsjlm, jmeans, jlmm)

# Kind of looks like overfitting, but LMM is widely used for repeated measurements
# on the same subject. To summarise: Both fixed and random effects are captured.
# In this case, the random effect is the individual worms (replicate)
# 1 in 1|Replicate means fit a random intercept for each individual worm.
# This assumes that all worms have different initial thrash rates. However,
# Strain and TimeID are fixed-effects. 
# LMM is tough because it usually depends on the researcher whether they want
# to use it or not - but I think it is pretty well-justified in this case because
# we are measuring each worm multiple times (classic). I also think it is necessary
# to account for individual worm baselines. It also gives the best results lol

# ================= AUC STUFF FROM HERE =================
AUCdf <- df_1 %>% 
  group_by(Strain, Replicate) %>% 
  summarise(AUC = trapz(TimeMin, Thrash_Count))

AUCmod <- lm(AUC ~ Strain, data = AUCdf)
Anova(AUCmod, type = "III") # Type III ANOVA test assuming linear differences
summary(aov(AUCmod)) # Normal ANOVA test assuming linear differences
# They give SAME p-values (0.000202).

# TukeyHSD for the AUC (assuming balanced n)
TukeyHSD(aov(AUCmod))
# Emmeans for the AUC (assuming unbalanced n)
emmeans(AUCmod, pairwise ~ Strain, adjust = "tukey")
# Again, both these give the same p-values so I'll just say that n is
# balanced enough.

# Just a summary stats table for AUC for those who wanna do stuff manually
AUCsum <- AUCdf %>% 
  group_by(Strain) %>% 
  summarise(
    n = n(),
    meanAUC = mean(AUC),
    sdAUC = sd(AUC),
    semAUC = sdAUC / n
  )
