# All mixed models analyses for our three dependent variables:
# 1. good boxes total
# 2. boxes total
# 3. good boxes percentage first 10 actions
#
# NB: this script contains the analysis about learning, so I look at the
# interaction between block number and AQ group.
#
# Here block is centered WITHIN SUBJECT.

### Clear workspace:
rm(list = ls())
set.seed(123)

# Import libraries:
library(readxl)
library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
library(sjPlot)
library(plotly)
library(emmeans)
library(datawizard)
library(olsrr)
library(moments)
library(car)
library(afex)
library(pander)
library(ggeffects)
library(ggpubr)
library(lmerTest)

# Use sum contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# Preparing the data
data <- read_excel("/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Adults/PulizieDiPrimavera/df_rc_same.xlsx")


# Subject ID
data$Subject <- as.factor(data$nickname)

# Numeric block
data$block <- as.numeric(data$block_number)
data$rich_NS <- as.factor(data$rich_NS)

# ICAR
# ICAR = 0 means that the score is missing, not that the true score is 0.
data$icar <- as.numeric(data$ICAR)
data$icar[data$icar == 0] <- NA


# median split AQ
# Work on AQ data to create a new column contaning AQ data splited by median split
# TRUE se AQ >= 21, FALSE se AQ < 21
data$AQ <- as.numeric(data$AQ_score)
data$AQmediansplit <- data$AQ >= 21
# AQ come fattore
data$AQmediansplit <- as.factor(data$AQmediansplit)

# rename the dv
data$goodboxes <- data$good_boxes_total
data$totalboxes <- data$boxes_total


data$Subject <- as.factor(data$nickname)
sub_IDs = unique(data$Subject)
n_sub = length(sub_IDs)


# To make block number centered , meaning not from 0 to 5 but from -2 to 2 (To favor the convergence!!)
# Calculate the mean of the block variable
block_mean <- mean(data$block)
# Subtract the mean from each value of block to center it
data$block <- data$block - block_mean

#center ICAR
ICAR_mean <- mean(data$icar, na.rm = TRUE)
data$icar <- data$icar - ICAR_mean

# ------------------------ GOOD BOXES ------------------------

# Check normality
hist(data$goodboxes, breaks = 50)
plot(density(data$goodboxes))
shapiro.test(data$goodboxes)

# Skewness
skewness(data$goodboxes)
agostino.test(data$goodboxes)

# Build the model
optimizer <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 1e6)
)

options(contrasts = c("contr.sum", "contr.poly"))
set.seed(123)

m000 <- glmer(
  goodboxes ~ block * AQmediansplit  + totalboxes + (1 + rich_NS | Subject),
  data = data,
  family = gaussian(link = identity),
  control = optimizer
)

# Selected model
sel_mod <- m000

summary(sel_mod)

# ---------------- SIMPLE SLOPES / DECOMPOSITION OF THE INTERACTION ----------------

AQ_slopes <- emtrends(
  sel_mod,
  specs = ~ AQmediansplit,
  var = "block"
)

AQ_slopes_test <- summary(
  AQ_slopes,
  infer = TRUE
)

AQ_slopes_test

AQ_slopes_difference <- pairs(AQ_slopes)

AQ_slopes_difference

# ---------------- POST-HOC COMPARISONS: HIGH vs LOW AQ WITHIN EACH BLOCK ----------------

AQ_block_emm <- emmeans(
  sel_mod,
  ~ AQmediansplit | block,
  at = list(block = sort(unique(data$block)))
)

AQ_block_contrasts <- pairs(
  AQ_block_emm,
  adjust = "holm"
)

AQ_block_contrasts_results <- summary(
  AQ_block_contrasts,
  infer = TRUE
)

AQ_block_contrasts_results

# ---------------- PLOT ----------------

# Create AQ group with clear labels
data$AQ_group <- ifelse(
  data$AQmediansplit == FALSE,
  "low AQ",
  "high AQ"
)

data$AQ_group <- factor(
  data$AQ_group,
  levels = c("low AQ", "high AQ")
)

# Original block scale
data$block_plot <- data$block + block_mean

# Model predictions
pred_df <- emmeans(
  sel_mod,
  ~ block * AQmediansplit,
  at = list(block = sort(unique(data$block)))
) |>
  as.data.frame()

pred_df$AQ_group <- ifelse(
  pred_df$AQ == "FALSE" | pred_df$AQ == FALSE,
  "low AQ",
  "high AQ"
)

pred_df$AQ_group <- factor(
  pred_df$AQ_group,
  levels = c("low AQ", "high AQ")
)

# Original block scale
pred_df$block_plot <- pred_df$block + block_mean

# Colours
cols <- c(
  "low AQ" = "blue",
  "high AQ" = "red"
)

# Plot
g <- ggplot() +
  geom_jitter(
    data = data,
    aes(
      x = block_plot,
      y = goodboxes,
      color = AQ_group
    ),
    width = 0.10,
    alpha = 0.35,
    size = 1.4
  ) +
  geom_line(
    data = pred_df,
    aes(
      x = block_plot,
      y = emmean,
      color = AQ_group,
      group = AQ_group
    ),
    linewidth = 1.4
  ) +
  geom_point(
    data = pred_df,
    aes(
      x = block_plot,
      y = emmean,
      color = AQ_group
    ),
    size = 3.5
  ) +
  scale_color_manual(
    values = cols,
    name = "AQ group"
  ) +
  scale_x_continuous(
    breaks = sort(unique(data$block_plot)),
    labels = paste0("block ", sort(unique(data$block_plot)))
  ) +
  labs(
    x = "block number",
    y = "good boxes rule change same trial"
  ) +
  theme_classic()

print(g)







# ------------------------ BOXES TOTAL ------------------------

# Check normality
hist(data$totalboxes, breaks = 50)
plot(density(data$totalboxes))
shapiro.test(data$totalboxes)

# Skewness
skewness(data$totalboxes)
agostino.test(data$totalboxes)

# Build the model
optimizer <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 1e6)
)

options(contrasts = c("contr.sum", "contr.poly"))
set.seed(123)

m000 <- glmer(
  totalboxes ~ block * AQ + (1 + rich_NS | Subject),
  data = data,
  family = gaussian(link = identity),
  control = optimizer
)

# Selected model
sel_mod <- m000

summary(sel_mod)

# ---------------- PLOT ----------------

# AQ group labels
data$AQ_group <- ifelse(
  data$AQ == FALSE,
  "low AQ",
  "high AQ"
)

data$AQ_group <- factor(
  data$AQ_group,
  levels = c("low AQ", "high AQ")
)

# Original block scale
data$block_plot <- data$block + block_mean

# Model predictions
pred_df <- emmeans(
  sel_mod,
  ~ block * AQ,
  at = list(block = sort(unique(data$block)))
) |>
  as.data.frame()

# AQ labels in predictions
pred_df$AQ_group <- ifelse(
  pred_df$AQ == "FALSE" | pred_df$AQ == FALSE,
  "low AQ",
  "high AQ"
)

pred_df$AQ_group <- factor(
  pred_df$AQ_group,
  levels = c("low AQ", "high AQ")
)

# Original block scale
pred_df$block_plot <- pred_df$block + block_mean

# Colours
cols <- c(
  "low AQ" = "blue",
  "high AQ" = "red"
)

# Plot
g <- ggplot() +
  geom_jitter(
    data = data,
    aes(
      x = block_plot,
      y = totalboxes,
      color = AQ_group
    ),
    width = 0.10,
    alpha = 0.35,
    size = 1.4
  ) +
  geom_line(
    data = pred_df,
    aes(
      x = block_plot,
      y = emmean,
      color = AQ_group,
      group = AQ_group
    ),
    linewidth = 1.4
  ) +
  geom_point(
    data = pred_df,
    aes(
      x = block_plot,
      y = emmean,
      color = AQ_group
    ),
    size = 3.5
  ) +
  scale_color_manual(
    values = cols,
    name = "AQ group"
  ) +
  scale_x_continuous(
    breaks = sort(unique(data$block_plot)),
    labels = paste0("block ", sort(unique(data$block_plot)))
  ) +
  labs(
    x = "block",
    y = "boxes total"
  ) +
  theme_classic()

print(g)

# Save plot
ggsave(
  filename = "/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Adults/Analisi_R/RuleChanges/boxestotal_rulechangesame_No1Block_AQ.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 300
)

