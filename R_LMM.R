# All mixed-model analyses for our two dependent variables: good boxes total and boxes total

# CLEAR WORKSPACE
rm(list = ls())
set.seed(123)


# IMPORT LIBRARIES
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

# PREPARE THE DATA
# Load dataset
data_all <- read_excel(
  "/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Adults/PulizieDiPrimavera/allresults_AQ_IQ.xlsx"
)


## Elimino Nisnig E Danmil
data <- data_all %>%
  filter(!nickname %in% c("Nisnig", "Danmil"))


# Building the full model
# Convert categorical and numerical variables of interest into factors:
data$trial <- as.numeric(data$trial_in_block_number) # as a factor to make this boolean variable as a factor having 2 levels, true indicates the first half while false indicates the second one
data$block <- as.numeric(data$block_number)

#to control for the differeces between NS and EW navigability
data$rich_NS <- as.factor(data$rich_NS)

# median split AQ
# Work on AQ data to create a new column contaning AQ data splited by median split
# TRUE se AQ >= 21, FALSE se AQ < 21
data$AQ <- as.numeric(data$AQ_score)
data$AQmediansplit <- data$AQ >= 21
# AQ come fattore
data$AQmediansplit <- as.factor(data$AQmediansplit)


# ICAR
# ICAR = 0 means that the score is missing, not that the true score is 0.
data$icar <- as.numeric(data$ICAR)
data$icar[data$icar == 0] <- NA


# rename the dv
data$goodboxes <- data$good_boxes_total
data$totalboxes <- data$boxes_total

data$Subject <- as.factor(data$nickname)
sub_IDs = unique(data$Subject)
n_sub = length(sub_IDs)

# To favor the convergence I center the variables
# HOW: First I calculate the mean of the variable (e.g. block) and then I subtract the mean from each value of block to center it

# center block
block_mean <- mean(data$block)
data$block <- data$block - block_mean

# center trial
trial_mean <- mean(data$trial, na.rm = TRUE)
data$trial <- data$trial - trial_mean

#center ICAR
ICAR_mean <- mean(data$icar, na.rm = TRUE)
data$icar <- data$icar - ICAR_mean


# Data including only subjects with a valid ICAR score
# this retain all observations for participants with a valid ICAR score 
# and remove all observations for participants with missing ICAR.
data_ICARsubjects <- data %>%
  filter(!is.na(icar))


#check
# Numero di soggetti unici nel dataset completo
n_subjects_data <- n_distinct(data$Subject)
cat("Numero di Subject unici in data:", n_subjects_data, "\n")

# Numero di soggetti unici nel dataset con ICAR valido
n_subjects_icar <- n_distinct(data_ICARsubjects$Subject)
cat("Numero di Subject unici in data_ICARsubjects:", n_subjects_icar, "\n")

# ------------------------ GOOD BOXES -------------------------------

# Check normality: Shapiro-Wilk's test
# if p > .05 we can assume normality
hist(data_ICARsubjects$goodboxes, breaks = 50)
plot(density(data_ICARsubjects$goodboxes)) # per vedere come è la distribuzione
shapiro.test(data_ICARsubjects$goodboxes)

# skewness
skewness(data_ICARsubjects$goodboxes) # coefficiente di asimmetria
agostino.test(data_ICARsubjects$goodboxes) # test di D'Agostino per il coefficiente di asimmetria

# Build the model:
# optimizer to allow the model to converge better
optimizer = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
options(contrasts = c("contr.sum", "contr.poly")) # use it always

# Since the distribution is negatively skewed, try different families
# (poisson, negative binomial, gaussian) and compare them

# POISSON
m000 = glmer(
  goodboxes ~ trial * block * AQmediansplit +
    totalboxes + icar
  + (1 + rich_NS | Subject),
  data = data_ICARsubjects,
  family = poisson(link = log),
  control = optimizer
)

# NEGATIVE BINOMIAL
m001 = glmer.nb(
  goodboxes ~ trial * block * AQmediansplit +
    totalboxes + icar
  + (1 + rich_NS | Subject),
  data = data_ICARsubjects,
  control = optimizer
)

# GAUSSIAN (totalboxes as a covariate)
m002 = glmer(
  goodboxes ~ trial * block * AQmediansplit +
    totalboxes + icar
  + (1 + rich_NS | Subject),
  data = data_ICARsubjects,
  family = gaussian(link = identity),
  control = optimizer
)

# Compare models: BIC
models = c("m000", "m001", "m002")
bics = c(BIC(m000), BIC(m001), BIC(m002))
bics
index = which.min(bics)
sprintf("Model with lowest BIC: %s (BIC = %.2f)", models[index], min(bics))

# Compare models: AIC
aics = c(AIC(m000), AIC(m001), AIC(m002))
aics
index = which.min(aics)
sprintf("Model with lowest AIC: %s (AIC = %.2f)", models[index], min(aics))

# Model selection --> in this case m002 wins both BIC and AIC, therefore use Gaussian

# ---------------
# Random effects selection

# with + (1|Subject)
set.seed(123)
m00 = glmer(
  goodboxes ~ trial * block * AQmediansplit  +
    totalboxes + icar + (1 + rich_NS | Subject),
  data = data_ICARsubjects,
  family = gaussian(link = identity),
  control = optimizer
)

# without + (1|Subject), using glm
set.seed(123)
m01 = glm(
  goodboxes ~ trial * block * AQmediansplit +
    totalboxes + icar,
  data = data_ICARsubjects,
  family = gaussian(link = identity)
)

models = c("m00", "m01")
bics = c(BIC(m00), BIC(m01))
bics
index = which.min(bics)
sprintf("Model with lowest BIC: %s (BIC = %.2f)", models[index], min(bics))
# m00 wins --> keep the random intercept + (1 + rich_NS | Subject)


# LRT
final_mod <- mixed(
  goodboxes ~ trial * block * AQmediansplit
    + totalboxes + icar +
    (1+ rich_NS| Subject),
  data = data_ICARsubjects,
  family = gaussian(link = identity),
  control = optimizer,
  method = "LRT"
)

# Selected model
sel_mod = final_mod
summary(sel_mod)



# LRT with all the subjects without icar as covariate
#final_mod <- mixed(
#  goodboxes ~ trial * block * AQmediansplit
#  + totalboxes + 
#    (1 + rich_NS| Subject),
#  data = data,
#  family = gaussian(link = identity),
#  control = optimizer,
#  method = "LRT"
#)

# Selected model
#sel_mod = final_mod
#summary(sel_mod)




# ---------------- POST HOC: SIMPLE SLOPES OF TRIAL ----------------
# Follow-up analysis of the significant trial × block × AQ interaction.
# This analysis:
# 1) estimates the simple slopes of trial (within-block learning rate)
# 2) separately for each AQ group (low vs high AQ)
# 3) at each level of block (i.e., across the six blocks)
# 4) compares these slopes between groups within each block,
#    applying Holm correction for multiple comparisons

library(emmeans)
library(dplyr)

# Recode AQ grouping variable for clearer output labels
data$AQmediansplit <- factor(
  data$AQmediansplit,
  levels = c(FALSE, TRUE),
  labels = c("low AQ", "high AQ")
)

# Estimate simple slopes of trial within each block and AQ group
slopes_trial <- emtrends(
  sel_mod,
  ~ AQmediansplit | block,   # slopes estimated per group within each block
  var = "trial",             # variable for which the slope is computed
  at = list(block = unique(data$block))  # evaluate slopes at each observed block
)

summary(slopes_trial)

# Pairwise comparisons of slopes between AQ groups within each block
# (i.e., tests whether learning rates differ between groups at each block)
# Holm correction controls for multiple comparisons
pairs(slopes_trial, adjust = "holm")
      


# ---------------- INTERCEPT CONTROL - Check for the differences in the STARTING POINT (TRIAL 0 of each block) ----------------
# Follow-up analysis to further interpret the significant trial × block × AQ interaction.
# While the previous analysis examined differences in learning rates (slopes),
# this analysis focuses on group differences at the beginning of each block.

# This analysis:
# 1) estimates the expected number of good boxes at the first trial of each block (trial = 0)
# 2) separately for each AQ group (low vs high AQ)
# 3) at each level of block (six blocks)
# 4) compares these starting values between AQ groups within each block,
#    applying Holm correction for multiple comparisons

emm_start <- emmeans(
  sel_mod,
  ~ AQmediansplit | block,
  at = list(
    trial = -4.5,
    block = sort(unique(data$block))
  )
)

# Display estimated starting values (with SE and confidence intervals)
summary(emm_start)

# Pairwise comparisons between AQ groups within each block
# (i.e., tests whether groups differ at the beginning of the block)
# Holm correction controls for multiple comparisons (as I was doing for the slopes)
pairs(emm_start, adjust = "holm")


# ---------------- PLOT ----------------

# Plotting variables for observed data
data_ICARsubjects$trial_plot <-
  data_ICARsubjects$trial + 1

data_ICARsubjects$block_plot <- factor(
  data_ICARsubjects$block_number + 1,
  levels = 1:6,
  labels = paste("block", 1:6)
)

# Gruppo AQ per i punti osservati
data_ICARsubjects$AQ_group <- factor(
  data_ICARsubjects$AQmediansplit,
  levels = c(FALSE, TRUE),
  labels = c("low AQ", "high AQ")
)

# Model predictions
pred_df <- emmeans(
  sel_mod,
  ~ AQmediansplit * trial | block,
  regrid = "response",
  at = list(
    trial = seq(
      min(data_ICARsubjects$trial, na.rm = TRUE),
      max(data_ICARsubjects$trial, na.rm = TRUE),
      length.out = 100
    ),
    block = sort(unique(data_ICARsubjects$block))
  )
) |>
  as.data.frame()

# Gruppo AQ per le predizioni
pred_df$AQ_group <- factor(
  pred_df$AQmediansplit,
  levels = c(FALSE, TRUE),
  labels = c("low AQ", "high AQ")
)

# Plotting variables for model predictions
pred_df$trial_plot <- pred_df$trial + 1

pred_df$block_plot <- factor(
  round(pred_df$block + block_mean) + 1,
  levels = 1:6,
  labels = paste("block", 1:6)
)

# Remove accidental NA facet rows
pred_df <- pred_df[!is.na(pred_df$block_plot), ]

# Colors
cols <- c(
  "low AQ" = "blue",
  "high AQ" = "red"
)

# Plot
g <- ggplot(
  pred_df,
  aes(
    x = trial_plot,
    y = emmean,
    color = AQ_group,
    fill = AQ_group,
    group = AQ_group
  )
) +
  geom_jitter(
    data = data_ICARsubjects,
    aes(
      x = trial_plot,
      y = goodboxes,
      color = AQ_group
    ),
    inherit.aes = FALSE,
    size = 0.6,
    width = 0.10,
    alpha = 0.40
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_ribbon(
    aes(
      ymin = emmean - SE,
      ymax = emmean + SE
    ),
    alpha = 0.20,
    color = NA
  ) +
  facet_wrap(
    ~ block_plot,
    nrow = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      min(pred_df$trial_plot),
      max(pred_df$trial_plot),
      length.out = 10
    ),
    labels = 1:10
  ) +
  scale_color_manual(
    values = cols,
    name = "AQ group"
  ) +
  scale_fill_manual(
    values = cols,
    guide = "none"
  ) +
  labs(
    x = "trial within block",
    y = "good boxes"
  ) +
  theme_classic()

print(g)
# ----------------------- BOXES TOTAL -------------------------------

#Check normality:  Shapiro-Wilk's test:
#if p > .05 we can assume the normality
hist(data$totalboxes, breaks = 50)
plot(density(data$totalboxes)) # per vedere come è la distribuzione
shapiro.test(data$totalboxes)
#skeweness
skewness(data$totalboxes) # coefficiente di asimmetria
agostino.test(data$totalboxes) # test di D'Agostino per il coefficiente di asimmetria

# Build the model:
# optimizer to allow the model to converge better
optimizer = glmerControl(optimizer= "bobyqa", optCtrl=list(maxfun=1e6))
# use optimizer1 when nothing fit ... avoid to use, use only if (super) needed
# optimizer1 = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6), calc.derivs = FALSE)
options(contrasts=c("contr.sum", "contr.poly")) # use it always


#since the distribution is NORMAL --> GAUSSIAN!

# GAUSSIAN
set.seed(123) # to fix the randomness, always use before fitting the model!
m000 = glmer(totalboxes ~ trial*block*AQmediansplit + (1 + rich_NS | Subject),
             data = data,family = gaussian(link = identity), control=optimizer)


# LRT
set.seed(123)
final_mod<-mixed(totalboxes ~ trial*block*AQmediansplit + (1 + rich_NS | Subject),
                 data = data,family = gaussian(link = identity), control=optimizer, method = "LRT")
LRT<-final_mod$anova_table
LRT

# Selected model:
sel_mod = final_mod
summary(sel_mod)

#post hoc
slopes <- emtrends(sel_mod, ~1, var = "block")
summary(slopes)

# slopes difference from zero
emm <- emtrends(sel_mod, ~1, var = "block")
summary(emm, infer = c(TRUE, TRUE))

#PLOT

data$block_plot <- data$block + block_mean

g <- ggplot(data, aes(x = block_plot, y = totalboxes)) +
  geom_jitter(
    color = "grey30",
    size = 0.4,
    width = 0.10,
    alpha = 0.35
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    linewidth = 1,
    color = "black"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 1.8,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 0:5,
    labels = 1:6
  ) +
  labs(
    x = "block",
    y = "total boxes"
  ) +
  theme_classic()

print(g)

ggsave(
  filename = "/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Analisi_R/Adaptation/boxestotal_adaptation.png",
  plot = g,
  width = 8,
  height = 5,
  dpi = 300
)