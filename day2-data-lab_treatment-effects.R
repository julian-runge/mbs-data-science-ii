### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc", "Rmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")

# install packages in list
#lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("TBD")

# load cleaned and prepared datasets
data <- read.csv("day2-data.csv", header=TRUE, sep=",")
data$X <- NULL
data$unique_id <- as.factor(data$unique_id)

# pull descriptive stats
summarySE(data, "d1_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d7_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

# add winsorized revenue and repeat purchases for day 14
d <- .98
data$d14_rev_denoised <- data$d14_revenue
data$d14_rev_denoised[data$d14_revenue > quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))] <- quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))

data$d14_repeat_purchases <- data$d14_purchases-1
data$d14_repeat_purchases[data$d14_repeat_purchases<0] <- 0

summarySE(data, "d14_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d30_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d60_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

data$d60_rev_denoised <- data$d60_revenue
data$d60_rev_denoised[data$d60_revenue > quantile(filter(data,d60_revenue > 0)$d60_revenue, c(d))] <- quantile(filter(data,d60_revenue > 0)$d60_revenue, c(d))
summarySE(data, "d60_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)


### country and device tier
summarySE(data, "d30_offer_conversion", groupvars = c("country_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_offer_revenue", groupvars = c("country_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_conversion", groupvars = c("country_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_revenue", groupvars = c("country_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rev_denoised", groupvars = c("country_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d30_offer_conversion", groupvars = c("device_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_offer_revenue", groupvars = c("device_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_conversion", groupvars = c("device_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_revenue", groupvars = c("device_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rev_denoised", groupvars = c("device_tier"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

### visualize treatment effects

# using test_bucket
plotmeans(d30_revenue ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Gross Revenue",
          ylim=c(0.5,3.5),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

plotmeans(d30_rev_denoised ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Revenue (Denoised)",
          ylim=c(0.5,2.5),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

plotmeans(d60_rev_denoised ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 60 Revenue (Denoised)",
          ylim=c(2.3,3.8),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

# using the prettier action2
data$action2 <- as.factor(data$action2)
data$treatment_group <- ordered(data$action2,
                        levels = c("2.99 USD", "4.99 USD", "29.99 USD", "Control"))

plotmeans(d30_rev_denoised ~ interaction(treatment_group, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Revenue (Denoised)",
          ylim=c(1.2,2.2),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

### assess treatment effects using linear regression
# set the reference category for our independent variable / predictor
data$action2 <- relevel(data$action2, ref = "Control")

# estimate linear regression and show results
lm_treat = lm(d30_rev_denoised ~ action2, data = data)

summary(lm_treat) 

lm_treat2 = lm(d30_conversion ~ action2, data = data)

summary(lm_treat2) 

# look at associations between device memory and user spending
lm_ram = lm(d30_rev_denoised ~ device_ram, data = data)

summary(lm_ram)

data$device_ram_gb <- data$device_ram/1000
lm_ram_gb = lm(d30_rev_denoised ~ device_ram_gb, data = data)

summary(lm_ram_gb)

lm_ram_gb2 = lm(d30_revenue ~ device_ram_gb, data = data)

summary(lm_ram_gb2)

# look at interaction effects between treatments and device memory

lm_ram_treat2 = lm(d30_rev_denoised ~ device_ram_gb*action2, data = data)

summary(lm_ram_treat2)
