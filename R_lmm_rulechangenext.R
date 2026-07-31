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
data <- read_excel("/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Adults/PulizieDiPrimavera/df_rc_next.xlsx")


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

#Check normality:  Shapiro-Wilk's test:
#if p > .05 we can assume the normality
hist(data$goodboxes, breaks = 50)
plot(density(data$goodboxes)) # per vedere come è la distribuzione
shapiro.test(data$goodboxes)
#skeweness
skewness(data$goodboxes) # coefficiente di asimmetria
agostino.test(data$goodboxes) # test di D'Agostino per il coefficiente di asimmetria



# Build the model:
# optimizer to allow the model to converge better
optimizer = glmerControl(optimizer= "bobyqa", optCtrl=list(maxfun=1e6))
# use optimizer1 when nothing fit ... avoid to use, use only if (super) needed
# optimizer1 = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6), calc.derivs = FALSE)
options(contrasts=c("contr.sum", "contr.poly")) # use it always

# here we try GAUSSIAN
set.seed(123)
m000 = glmer(goodboxes ~ block*AQmediansplit + totalboxes + (1+ rich_NS|Subject),
             data = data,family = gaussian(link = identity), control=optimizer) 
# Selected model:
sel_mod = m000
summary(sel_mod)

# PLOT

# creo una variabile AQ con etichette chiare
data$AQ_group <- ifelse(data$AQmediansplit == FALSE, "low AQ", "high AQ")
data$AQ_group <- factor(data$AQ_group, levels = c("low AQ", "high AQ"))

# blocchi riportati alla scala originale
data$block_plot <- data$block + block_mean

# predizioni del modello
pred_df <- emmeans(
  sel_mod,
  ~ block * AQmediansplit,
  at = list(block = sort(unique(data$block)))
) |> 
  as.data.frame()

# stessa codifica del gruppo anche nelle predizioni
pred_df$AQ_group <- ifelse(pred_df$AQmediansplit == "FALSE" | pred_df$AQmediansplit == FALSE,
                           "low AQ", "high AQ")
pred_df$AQ_group <- factor(pred_df$AQ_group, levels = c("low AQ", "high AQ"))

# blocchi originali
pred_df$block_plot <- pred_df$block + block_mean

# colori
cols <- c("low AQ" = "blue", "high AQ" = "red")

# plot
g <- ggplot() +
  geom_jitter(
    data = data,
    aes(x = block_plot, y = goodboxes, color = AQ_group),
    width = 0.10,
    alpha = 0.35,
    size = 1.4
  ) +
  geom_line(
    data = pred_df,
    aes(x = block_plot, y = emmean, color = AQ_group, group = AQ_group),
    linewidth = 1.4
  ) +
  geom_point(
    data = pred_df,
    aes(x = block_plot, y = emmean, color = AQ_group),
    size = 3.5
  ) +
  scale_color_manual(values = cols, name = "AQ group") +
  scale_x_continuous(
    breaks = sort(unique(data$block_plot)),
    labels = paste0("block ", sort(unique(data$block_plot)))
  ) +
  labs(
    x = "block number",
    y = "good boxes rule change next trial"
  ) +
  theme_classic()

print(g)


# ------------------------ BOXES TOTAL ------------------------

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

# here we try GAUSSIAN
set.seed(123)
m000 = glmer(totalboxes ~ block*AQmediansplit + (1+ rich_NS|Subject),
             data = data,family = gaussian(link = identity), control=optimizer) 
# Selected model:
sel_mod = m000
summary(sel_mod)

# PLOT

# creo una variabile AQ con etichette chiare
data$AQ_group <- ifelse(data$AQmediansplit == FALSE, "low AQ", "high AQ")
data$AQ_group <- factor(data$AQ_group, levels = c("low AQ", "high AQ"))

# blocchi riportati alla scala originale
data$block_plot <- data$block + block_mean

# predizioni del modello
pred_df <- emmeans(
  sel_mod,
  ~ block * AQmediansplit,
  at = list(block = sort(unique(data$block)))
) |> 
  as.data.frame()

# stessa codifica del gruppo anche nelle predizioni
pred_df$AQ_group <- ifelse(pred_df$AQmediansplit == "FALSE" | pred_df$AQmediansplit == FALSE,
                           "low AQ", "high AQ")
pred_df$AQ_group <- factor(pred_df$AQ_group, levels = c("low AQ", "high AQ"))

# blocchi originali
pred_df$block_plot <- pred_df$block + block_mean

# colori
cols <- c("low AQ" = "blue", "high AQ" = "red")

# plot
g <- ggplot() +
  geom_jitter(
    data = data,
    aes(x = block_plot, y = totalboxes, color = AQ_group),
    width = 0.10,
    alpha = 0.35,
    size = 1.4
  ) +
  geom_line(
    data = pred_df,
    aes(x = block_plot, y = emmean, color = AQ_group, group = AQ_group),
    linewidth = 1.4
  ) +
  geom_point(
    data = pred_df,
    aes(x = block_plot, y = emmean, color = AQ_group),
    size = 3.5
  ) +
  scale_color_manual(values = cols, name = "AQ group") +
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

# save the plot
ggsave(
  filename = "/Users/valeriasimonelli/Desktop/PostDoc/Research/Foraging_Volatilitity/Adults/Analisi_R/RuleChanges/boxestotal_rulechangenext_No1Block_AQ.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 300
)
