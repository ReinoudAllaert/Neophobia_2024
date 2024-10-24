## ----Dependencies, echo=TRUE-------------------------------------------------------------------------------------------------------
packages <- c('lmerTest', 'lme4', 'ggplot2', 'tidyverse', 'readxl', 'purrr', 'performance', 'emmeans', 'MASS', 'dplyr', 'tidyr', 'nlme')

# Check if each package is installed; if not, install it
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}



## ----Load data---------------------------------------------------------------------------------------------------------------------
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)
data <- read.csv("processed_data/neophobia_data.csv", row.names = 1)



## ----data summary, fig.width=10, fig.height=10, dpi=300----------------------------------------------------------------------------
# Quick look at structure
str(data)
summary(data)

# 13 birds were ID'd to be LBBG instead of HG, in the RR we stated  we would remove those from the analysis:

LBBG <- c("GY_RY", "BG_RR", "GR_GR", "GG_BR",
          "BG_BY", "GY_PP", "PR_BP", "PY_BY",
          "BP_BY", "BB_RR", "GG_BG", "BB_GY",
          "PR_RY")

data <- data %>% filter(!Bird_ID %in% LBBG)

# There are quite a bit of birds that never ate, let's explore those:
data_not_eat <- data %>% filter(Latency_to_Eat == 600)

# 13/29 not eating trials are from birds that did not eat twice or more 
sort(table(data_not_eat$Bird_ID), decreasing = TRUE)

data_not_eat <- data_not_eat %>%
  group_by(Object, Context) %>%
  summarise(Count = n())
data_not_eat$obj_cont <- paste0(data_not_eat$Object, "_", data_not_eat$Context)

# Almost all of those ocurrences are individual x novel
data_not_eat

# Given that these are actually the most interesting birds, we should keep them in the analysis. There are only 106 individual x novel trials in total. Excluding 29 of them  would mean losing 1/3 of the data.



## ----Add grp values----------------------------------------------------------------------------------------------------------------
most_frequent_group <- function(group_ids) {
  group_ids <- group_ids[!is.na(group_ids)]
  if (length(group_ids) == 0) return(NA)
  names(sort(table(group_ids), decreasing = TRUE))[1]
}

most_frequent_group_per_bird <- data %>%
  group_by(Bird_ID) %>%
  summarize(most_frequent_group = most_frequent_group(GroupID)) %>%
  ungroup()

data <- data %>%
  left_join(most_frequent_group_per_bird, by = "Bird_ID") %>%
  mutate(GroupID = ifelse(is.na(GroupID), most_frequent_group, GroupID)) %>%
  dplyr::select(-most_frequent_group)


## ----Set trial nbr-----------------------------------------------------------------------------------------------------------------
data$Trial <- data$Trial - 1



## ----Enter model 1, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------
enter_model <- lmer(Latency_to_enter ~ Object_contrast * Context_contrast + Trial + 
                (1 | NestID) + 
                (-1 + group_dummy | GroupID) + 
                (-1 +  ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(enter_model)
check_model(enter_model)



## ----Enter model 2, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------
enter_model2 <- lmer(Latency_to_enter ~ Object_contrast * Context_contrast + Trial + 
                (-1 +  ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(enter_model2)
check_model(enter_model2)


## ----Enter model 3, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------
enter_model3 <- lmer(log(Latency_to_enter) ~ Object_contrast + Context_contrast + Trial + 
                (-1 +  ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(enter_model3)
check_model(enter_model3)



## ----Enter model 4, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------

boxcox_transform <- boxcox(lm(Latency_to_enter ~ Object_contrast + Context_contrast + Trial, data = data))
best_lambda <- boxcox_transform$x[which.max(boxcox_transform$y)]
best_lambda
data$Latency_to_enter_trans <- (data$Latency_to_enter^best_lambda - 1) / best_lambda

enter_model4_boxcox <- lmer(Latency_to_enter_trans ~ Object_contrast + 
                              Context_contrast + Trial + 
                            (-1 + ind_dummy + group_dummy | Bird_ID), 
                            data = data)
summary(enter_model4_boxcox)
check_model(enter_model4_boxcox)


## ----Backtransform, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------
## First the fixed effects
bctran_enter <- make.tran("boxcox", best_lambda) 


# Compute marginal means for Context_contrast
emms_context <- emmeans(
  enter_model4_boxcox,
  specs = "Context_contrast",  # Marginal means for Context_contrast
  type = "response",
  tran = bctran_enter  # Back-transform using Box-Cox transformation
)
print(emms_context)

# Compute marginal means for Object_contrast
emms_object <- emmeans(
  enter_model4_boxcox,
  specs = "Object_contrast",  # Marginal means for Object_contrast
  type = "response",
  tran = bctran_enter  # Back-transform using Box-Cox transformation
)
print(emms_object)

# Compute marginal means for Trial
emms_trial_baseline <- emmeans(
  enter_model4_boxcox,
  specs = ~ Trial,  # Get marginal means for each trial
  at = list(Trial = 0:7),  # trial range (0 to 7)
  type = "response", 
  tran = bctran_enter  
) 
print(emms_trial_baseline) # Clear change in latency over time

# Compare each trial to Trial 0 (the baseline) 
contrast(emms_trial_baseline, "trt.vs.ctrl", ref = 1) 

#### Now look at the variance:

# variances from the model summary
random_effects_variances <- as.data.frame(VarCorr(enter_model4_boxcox))

# Extract variances for ind_dummy and group_dummy from the model output
var_ind_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "ind_dummy"]
var_group_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "group_dummy"]

# Standard deviations (square root of the variances)
sd_ind_dummy <- sqrt(var_ind_dummy)
sd_group_dummy <- sqrt(var_group_dummy)

# Simulate random effects for 100 individuals in both conditions
set.seed(0) 
simulated_ind_dummy <- rnorm(100, mean = 0, sd = sd_ind_dummy)
simulated_group_dummy <- rnorm(100, mean = 0, sd = sd_group_dummy)

# Back-transform the simulated random effects using the inverse Box-Cox transformation
lambda <- best_lambda 

# Function for backtranforming boxcox values
back_transform <- function(simulated_values, lambda) {
  if (lambda != 0) {
    return((simulated_values * lambda + 1)^(1 / lambda))
  } else {
    return(exp(simulated_values))
  }
}

# apply for both random effects
backtransformed_ind_dummy <- back_transform(simulated_ind_dummy, lambda)
backtransformed_group_dummy <- back_transform(simulated_group_dummy, lambda)

# Calculate the variance and standard deviation for the back-transformed random effects
variance_ind_dummy_backtransformed <- var(backtransformed_ind_dummy)
variance_group_dummy_backtransformed <- var(backtransformed_group_dummy)

sd_ind_dummy_backtransformed <- sd(backtransformed_ind_dummy)
sd_group_dummy_backtransformed <- sd(backtransformed_group_dummy)

# Print the variances and standard deviations
cat("Variance in seconds (individual):", variance_ind_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (individual):", sd_ind_dummy_backtransformed, "\n")
cat("Variance in seconds (group):", variance_group_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (group):", sd_group_dummy_backtransformed, "\n")


# Now likelihood ratio test to check whether this sign is different
# Fit model with and without both random effects

# Model 1: Full model with separate variances 
enter_model4_boxcox <- lmer(
  Latency_to_enter_trans ~ Object_contrast + Context_contrast + Trial +
  (-1 + ind_dummy + group_dummy | Bird_ID),
  data = data
)

# Model 2: Reduced model with a single random effect 
enter_model_reduced <- lmer(
  Latency_to_enter_trans ~ Object_contrast + Context_contrast + Trial +
  (1 | Bird_ID),  
  data = data
)

# compare the models using a likelihood ratio test (LRT)
anova(enter_model4_boxcox, enter_model_reduced)



## ----Eat model 1, fig.width=10, fig.height=10, dpi=300-----------------------------------------------------------------------------
# Given that the distribution of DV is similar to that of the enter model, we log transformed .
eat_model <- lmer(log(Latency_to_Eat) ~ Object_contrast * Context_contrast + Trial + 
                    (1 | NestID) + 
                   (- 1 + group_dummy | GroupID) + 
                   (- 1 + ind_dummy + group_dummy | Bird_ID),  
                 data = data)
summary(eat_model)
check_model(eat_model)


## ----Eat model 2, fig.width=10, fig.height=10, dpi=300-----------------------------------------------------------------------------

eat_model2 <- lmer(log(Latency_to_Eat) ~ Object_contrast * Context_contrast + Trial + 
                   (- 1 + ind_dummy + group_dummy | Bird_ID),  
                 data = data)
summary(eat_model2)
check_model(eat_model2)



## ----Eat model 3, fig.width=10, fig.height=10, dpi=300-----------------------------------------------------------------------------

boxcox_transform <- boxcox(lm(Latency_to_Eat ~ Object_contrast * Context_contrast + Trial, data = data))
best_lambda <- boxcox_transform$x[which.max(boxcox_transform$y)]
best_lambda
data$Latency_to_eat_trans <- (data$Latency_to_Eat^best_lambda - 1) / best_lambda

eat_model4_boxcox <- lmer(Latency_to_eat_trans ~ Object_contrast * 
                              Context_contrast + Trial + 
                            (-1 + ind_dummy + group_dummy | Bird_ID), 
                            data = data)

summary(eat_model4_boxcox)
check_model(eat_model4_boxcox)

# Should we jsut fit this model then? Seems like there is no point in trying to say something about the variance with a corr of 1 and singular fit?
latency_model_reduced <- lmer(Latency_to_eat_trans ~ Object_contrast * Context_contrast + Trial + 
                              (1 | Bird_ID), data = data)


## ----Backtransform2, fig.width=10, fig.height=10, dpi=300--------------------------------------------------------------------------
# Ficed effects
# Compute marginal means for the interaction between Object_contrast and Context_contrast, the only interesting fixed effect to look at for this model
emms_interaction_eat <- emmeans(
  eat_model4_boxcox,
  specs = ~ Object_contrast * Context_contrast,  # Interaction between object and context
  type = "response",                             # Back-transform to original scale
  tran = bctran_enter                            # Apply Box-Cox back-transformation
)
print(emms_interaction_eat)

# random effects

# Extract variances for ind_dummy and group_dummy from the model output
random_effects_variances <- as.data.frame(VarCorr(eat_model4_boxcox))
var_ind_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "ind_dummy"]
var_group_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "group_dummy"]

# Calculate the standard deviations (square root of the variances)
sd_ind_dummy <- sqrt(var_ind_dummy)
sd_group_dummy <- sqrt(var_group_dummy)

# Simulate random effects for 100 individuals in both conditions
set.seed(0)
simulated_ind_dummy <- rnorm(100, mean = 0, sd = sd_ind_dummy)
simulated_group_dummy <- rnorm(100, mean = 0, sd = sd_group_dummy)

# Back-transform the simulated random effects using exp() to reverse the log transformation
backtransformed_ind_dummy <- (simulated_ind_dummy * best_lambda + 1)^(1 / best_lambda)
backtransformed_group_dummy <- (simulated_group_dummy * best_lambda + 1)^(1 / best_lambda)

# Calculate the variance and standard deviation for the back-transformed random effects
variance_ind_dummy_backtransformed <- var(backtransformed_ind_dummy)
variance_group_dummy_backtransformed <- var(backtransformed_group_dummy)

sd_ind_dummy_backtransformed <- sd(backtransformed_ind_dummy)
sd_group_dummy_backtransformed <- sd(backtransformed_group_dummy)

# Print the variances and standard deviations for comparison
cat("Variance in seconds (individual):", variance_ind_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (individual):", sd_ind_dummy_backtransformed, "\n")
cat("Variance in seconds (group):", variance_group_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (group):", sd_group_dummy_backtransformed, "\n")

# Test difference in variance
# Full model: Separate random effects for individual and group conditions
eat_model4_boxcox <- lmer(Latency_to_eat_trans ~ Object_contrast * 
                              Context_contrast + Trial + 
                            (-1 + ind_dummy + group_dummy | Bird_ID), 
                            data = data)

# Reduced model: Single random effect for Bird_ID
eat_model4_reduced <- lmer(
  Latency_to_eat_trans ~ Object_contrast * Context_contrast + Trial +
  (1 | Bird_ID),
  data = data
)

# Compare the models using a likelihood ratio test (LRT)
anova(eat_model4_boxcox, eat_model4_reduced)


## ----ZOI model 2, fig.width=10, fig.height=10, dpi=300-----------------------------------------------------------------------------
zoi_model2 <- lmer(Zoi_duration ~ Object_contrast * Context_contrast + Trial + 
                (-1 +  ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(zoi_model2)
check_model(zoi_model2)


## ----ZOI model log, fig.width=10, fig.height=10, dpi=300---------------------------------------------------------------------------
## 
# Log transformation to better handle zeros
data$Log_Zoi_duration <- log(data$Zoi_duration + 1)

# Fit a linear mixed model on the transformed data
zoi_model_log <- lmer(
  Log_Zoi_duration ~ Object_contrast * Context_contrast + Trial + 
    (-1 + ind_dummy + group_dummy | Bird_ID), 
  data = data
)

# Summarize the model
summary(zoi_model_log)
check_model(zoi_model_log)


## ----backtrnafrom, fig.width=10, fig.height=10, dpi=300----------------------------------------------------------------------------
# Function for back-transforming the log-transformed values to seconds
# Back-transform the fixed effects
back_transform_log <- function(predicted_values) {
  # Apply exp() to reverse the log-transformation
  return(exp(predicted_values))
}

# Compute marginal means for the interaction between Object_contrast and Context_contrast
emms_interaction_zoi <- emmeans(
  zoi_model_log,  # The log-transformed model
  specs = ~ Object_contrast * Context_contrast,  # Interaction between object and context
  type = "response"  # Return values on the original (back-transformed) scale
)

# Apply the back-transform function to the emmeans results
predicted_zoi_interaction <- summary(emms_interaction_zoi)
predicted_zoi_interaction$backtransformed_response <- back_transform_log(predicted_zoi_interaction$emmean)

# Print the back-transformed marginal means
print(predicted_zoi_interaction)

# Random effects
# Extract variances for ind_dummy and group_dummy from the log-transformed model
random_effects_variances <- as.data.frame(VarCorr(zoi_model_log))
var_ind_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "ind_dummy"]
var_group_dummy <- random_effects_variances$vcov[random_effects_variances$grp == "Bird_ID" & random_effects_variances$var1 == "group_dummy"]

# Calculate the standard deviations (square root of the variances)
sd_ind_dummy <- sqrt(var_ind_dummy)
sd_group_dummy <- sqrt(var_group_dummy)

# Simulate random effects for 100 individuals in both conditions
set.seed(0)
simulated_ind_dummy <- rnorm(100, mean = 0, sd = sd_ind_dummy)
simulated_group_dummy <- rnorm(100, mean = 0, sd = sd_group_dummy)

# Back-transform the simulated random effects using exp() to reverse the log transformation
backtransformed_ind_dummy <- exp(simulated_ind_dummy)
backtransformed_group_dummy <- exp(simulated_group_dummy)

# Calculate the variance and standard deviation for the back-transformed random effects
variance_ind_dummy_backtransformed <- var(backtransformed_ind_dummy)
variance_group_dummy_backtransformed <- var(backtransformed_group_dummy)

sd_ind_dummy_backtransformed <- sd(backtransformed_ind_dummy)
sd_group_dummy_backtransformed <- sd(backtransformed_group_dummy)

# Print the variances and standard deviations for comparison
cat("Variance in seconds (individual):", variance_ind_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (individual):", sd_ind_dummy_backtransformed, "\n")
cat("Variance in seconds (group):", variance_group_dummy_backtransformed, "\n")
cat("Standard deviation in seconds (group):", sd_group_dummy_backtransformed, "\n")

# Test difference in variance
# Full model: Separate random effects for individual and group conditions
zoi_model_log_full <- lmer(
  Log_Zoi_duration ~ Object_contrast * Context_contrast + Trial +
  (-1 + ind_dummy + group_dummy | Bird_ID),
  data = data
)

# Reduced model: Single random effect for Bird_ID
zoi_model_log_reduced <- lmer(
  Log_Zoi_duration ~ Object_contrast * Context_contrast + Trial +
  (1 | Bird_ID),
  data = data
)

# Compare the models using a likelihood ratio test (LRT)
anova(zoi_model_log_full, zoi_model_log_reduced)



## ----multivariate model 1----------------------------------------------------------------------------------------------------------
# make data long:
data_long <- data %>%
  pivot_longer(
    cols = c(Latency_to_enter, Latency_to_Eat),
    names_to = "Behaviour_Type",
    values_to = "Latency"
  )
data_long <- data_long %>%
  mutate(
    eat_vs_leave_contrast = case_when(
      Behaviour_Type == "Latency_to_enter" ~ -0.5,
      Behaviour_Type == "Latency_to_Eat" ~ 0.5
    )
  )

# fir full model
latency_model <- lmer(Latency ~ eat_vs_leave_contrast * Object_contrast * Context_contrast + Trial + 
                    (1 | NestID) +
                    (- 1 + group_dummy | GroupID) + 
                    (- 1 + ind_dummy + group_dummy + eat_vs_leave_contrast | Bird_ID), 
                  data = data_long)

summary(latency_model)



## ----multivariate model 2, fig.width=10, fig.height=10, dpi=300--------------------------------------------------------------------

latency_model2 <- lmer(Latency ~ eat_vs_leave_contrast * Object_contrast * Context_contrast + Trial + 
                    (- 1 + ind_dummy + group_dummy + eat_vs_leave_contrast | Bird_ID), 
                  data = data_long)

summary(latency_model2)
check_model(latency_model2)



## ----multivariate model 3, fig.width=10, fig.height=10, dpi=300--------------------------------------------------------------------


boxcox_transform <- boxcox(lm(Latency ~ eat_vs_leave_contrast * Object_contrast * Context_contrast + Trial, data = data_long))
best_lambda <- boxcox_transform$x[which.max(boxcox_transform$y)]

data_long$Latency_trans <- (data_long$Latency^best_lambda - 1) / best_lambda



latency_model3_boxcox <- lmer(Latency_trans ~ eat_vs_leave_contrast * Object_contrast * Context_contrast + Trial + 
                    (- 1 + ind_dummy + group_dummy + eat_vs_leave_contrast | Bird_ID), 
                  data = data_long)

summary(latency_model3_boxcox)
check_model(latency_model3_boxcox)




## ----multivariate model 4, fig.width=10, fig.height=10, dpi=300--------------------------------------------------------------------
latency_model3_boxcox_reduced <- lmer(Latency_trans ~ eat_vs_leave_contrast * Object_contrast * Context_contrast +
                                       Trial + 
                                       (-1 + ind_dummy + group_dummy  | Bird_ID), 
                                     data = data_long)

# Summary of the updated model
summary(latency_model3_boxcox_reduced)

# Check the model diagnostics
check_model(latency_model3_boxcox_reduced)


## ----multivariate model 5, fig.width=10, fig.height=10, dpi=300--------------------------------------------------------------------
latency_model3_boxcox_reduced_2 <- lmer(
  Latency_trans ~ eat_vs_leave_contrast * Object_contrast + 
                  eat_vs_leave_contrast * Context_contrast + 
                  Object_contrast * Context_contrast + 
                  Trial + 
                  (-1 + ind_dummy + group_dummy | Bird_ID), 
  data = data_long
)

# Summary of the updated model
summary(latency_model3_boxcox_reduced_2)

# Check the model diagnostics
check_model(latency_model3_boxcox_reduced_2)


## ----Descriptives------------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif)
library(patchwork)

# Calculate summary stats for plotting (mean and standard error)
calculate_summary <- function(data, var) {
  data %>%
    group_by(Context, Object) %>%
    summarise(mean_value = mean({{var}}, na.rm = TRUE),
              se_value = sd({{var}}, na.rm = TRUE) / sqrt(n()),
              .groups = "drop")
}

# Summarise data for each variable
summary_latency_enter <- data %>%
  group_by(Context) %>%
  summarise(mean_value = mean(Latency_to_enter, na.rm = TRUE),
            se_value = sd(Latency_to_enter, na.rm = TRUE) / sqrt(n()))

summary_latency_eat <- calculate_summary(data, Latency_to_Eat)
summary_time_ZOI <- calculate_summary(data, Zoi_duration)

# Custom function to reorder factor levels for plots
reorder_levels <- function(data) {
  data <- data %>%
    mutate(Context_Object = interaction(Context, Object, sep = "-")) %>%
    mutate(Context_Object = factor(Context_Object, 
                                   levels = c("individual-control", "individual-novel", 
                                              "group-control", "group-novel")))
  return(data)
}

# Reorder levels for the interaction-based data
summary_latency_eat <- reorder_levels(summary_latency_eat)
summary_time_ZOI <- reorder_levels(summary_time_ZOI)

# Function to plot Latency to Enter (no Object, Context only)
plot_latency_enter <- ggplot(summary_latency_enter, aes(x = Context, y = mean_value, fill = Context)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6) + 
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                width = 0.2, position = position_dodge(0.6)) + 
  labs(x = "", y = "Mean Latency (s)", title = "Latency to Enter") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, , size = 12), 
        plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title = element_text(size = 12),
        legend.position = "none") + 
  scale_fill_manual(values = c("group" = "goldenrod", "individual" = "lightblue")) 

# Function to plot variables with interaction (Latency to Eat, ZOI Duration) and double brackets
plot_variable_with_double_brackets <- function(data, var_name, y_label) {
  ggplot(data, aes(x = Context_Object, y = mean_value, fill = Object)) + 
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) + 
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                  width = 0.2, position = position_dodge(0.6)) + 
    labs(x = "", y = y_label, title = var_name) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title = element_text(size = 12),
          legend.position = "none") + 
    scale_fill_manual(values = c("novel" = "goldenrod", "control" = "lightblue"))
}

# Plot Latency to Eat
plot_latency_eat <- plot_variable_with_double_brackets(summary_latency_eat, "Latency to Eat", "Mean Latency (s)")

# Plot Time Spent in ZOI
plot_time_ZOI <- plot_variable_with_double_brackets(summary_time_ZOI, "ZOI Duration", "Mean Time (s)")

# Combine plots using patchwork
combined_plot <- (plot_latency_enter | plot_latency_eat | plot_time_ZOI) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = "bottom")

# Print the combined plot
print(combined_plot)





## ----Plot of descriptives----------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Calculate summary stats for plotting (mean and standard error)
calculate_summary <- function(data, var) {
  data %>%
    group_by(Context, Object) %>%
    summarise(mean_value = mean({{var}}, na.rm = TRUE),
              se_value = sd({{var}}, na.rm = TRUE) / sqrt(n()),
              .groups = "drop")
}

# Summarise data for each variable
summary_latency_enter <- data %>%
  group_by(Context) %>%
  summarise(mean_value = mean(Latency_to_enter, na.rm = TRUE),
            se_value = sd(Latency_to_enter, na.rm = TRUE) / sqrt(n()))

summary_latency_eat <- calculate_summary(data, Latency_to_Eat)
summary_time_ZOI <- calculate_summary(data, Zoi_duration)

# Custom function to reorder factor levels for plots
reorder_levels <- function(data) {
  data <- data %>%
    mutate(Context_Object = interaction(Context, Object, sep = "-")) %>%
    mutate(Context_Object = factor(Context_Object, 
                                   levels = c("individual-control", "individual-novel", 
                                              "group-control", "group-novel")))
  return(data)
}

# Reorder levels for the interaction-based data
summary_latency_eat <- reorder_levels(summary_latency_eat)
summary_time_ZOI <- reorder_levels(summary_time_ZOI)

# Function to plot Latency to Enter (no Object, Context only)
plot_latency_enter <- ggplot(summary_latency_enter, aes(x = Context, y = mean_value, fill = Context)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6) + 
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                width = 0.2, position = position_dodge(0.6)) + 
  labs(x = "", y = "Mean latency (s)", title = "Latency to enter") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title = element_text(size = 12),
        legend.position = "none") + 
  scale_fill_manual(values = c("group" = "goldenrod", "individual" = "lightblue"))

# Function to plot variables with interaction (Latency to Eat, ZOI Duration)
plot_variable_without_signif <- function(data, var_name, y_label) {
  ggplot(data, aes(x = Context_Object, y = mean_value, fill = Object)) + 
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) + 
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                  width = 0.2, position = position_dodge(0.6)) + 
    labs(x = "", y = y_label, title = var_name) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title = element_text(size = 12),
          legend.position = "none") + 
    scale_fill_manual(values = c("novel" = "goldenrod", "control" = "lightblue"))
}

# Plot Latency to Eat
plot_latency_eat <- plot_variable_without_signif(summary_latency_eat, "Latency to eat", "Mean latency (s)")

# Plot Time Spent in ZOI
plot_time_ZOI <- plot_variable_without_signif(summary_time_ZOI, "ZOI duration", "Mean time (s)")

# Combine plots using patchwork
combined_plot <- (plot_latency_enter | plot_latency_eat | plot_time_ZOI) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = "bottom")

# Print the combined plot
print(combined_plot)





