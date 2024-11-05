# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif)
library(patchwork)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)
data <- read.csv("processed_data/neophobia_data.csv", row.names = 1)

data_delta <- data %>%
  group_by(Bird_ID, Context) %>%
  summarise(
    Latency_to_enter_delta = sum(Latency_to_enter[Object == "novel"]) - sum(Latency_to_enter[Object == "control"]),
    Latency_to_eat_delta = sum(Latency_to_Eat[Object == "novel"]) - sum(Latency_to_Eat[Object == "control"]),
    ZOI_duration_delta = sum(Zoi_duration[Object == "novel"]) - sum(Zoi_duration[Object == "control"])
  ) %>%
  ungroup()



# Calculate summary stats for plotting (mean and standard error)
calculate_summary <- function(data, var) {
  data %>%
    group_by(Context, Object) %>%
    summarise(mean_value = mean({{var}}, na.rm = TRUE),
              se_value = sd({{var}}, na.rm = TRUE) / sqrt(n()),
              .groups = "drop")
}

# Summarise data for each variable
summary_latency_enter <- calculate_summary(data, Latency_to_enter)

summary_latency_eat <- calculate_summary(data, Latency_to_Eat)
summary_time_ZOI <- calculate_summary(data, Zoi_duration)

# Custom function to reorder factor levels for plots
reorder_levels <- function(data) {
  data <- data %>%
    mutate(Context_Object = interaction(Context, Object, sep = "-")) %>%
    mutate(Context_Object = factor(Context_Object, 
                                   levels = c("individual-novel", "individual-control",
                                               "group-novel", "group-control")))
  return(data)
}

# Reorder levels for the interaction-based data
summary_latency_enter <- reorder_levels(summary_latency_enter)
summary_latency_eat <- reorder_levels(summary_latency_eat)
summary_time_ZOI <- reorder_levels(summary_time_ZOI)



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

plot_latency_enter <- plot_variable_with_double_brackets(summary_latency_enter, "Latency to Enter", "Mean Latency (s)")

# Plot Latency to Eat
plot_latency_eat <- plot_variable_with_double_brackets(summary_latency_eat, "Latency to Eat", "Mean Latency (s)")

# Plot Time Spent in ZOI
plot_time_ZOI <- plot_variable_with_double_brackets(summary_time_ZOI, "ZOI Duration", "Mean Time (s)")

# Combine plots using patchwork
combined_plot_1 <- (plot_latency_enter | plot_latency_eat | plot_time_ZOI) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = "bottom")

# Print the combined plot
print(combined_plot_1)


# Now make a plot of the difference sores
# Summarise data for mean and standard error calculations
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)



# Calculate summary stats (mean and standard error) for the unscaled data
data_summary <- data_delta %>%
  group_by(Context) %>%
  summarise(
    mean_enter = mean(Latency_to_enter_delta, na.rm = TRUE),
    se_enter = sd(Latency_to_enter_delta, na.rm = TRUE) / sqrt(n()),
    
    mean_eat = mean(Latency_to_eat_delta, na.rm = TRUE),
    se_eat = sd(Latency_to_eat_delta, na.rm = TRUE) / sqrt(n()),
    
    mean_zoi = -1*mean(ZOI_duration_delta, na.rm = TRUE),
    se_zoi = sd(ZOI_duration_delta, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Context = factor(Context, levels = c("individual", "group")))

# Set y-limits based on the data range to provide consistent scaling
max_y_limit <- max(abs(data_summary$mean_enter + data_summary$se_enter), 
                   abs(data_summary$mean_eat + data_summary$se_eat), 
                   abs(data_summary$mean_zoi + data_summary$se_zoi))

plot_delta_variable <- function(data, mean_var, se_var, title) {
  ggplot(data, aes(x = Context, y = .data[[mean_var]], group = 1)) +
    geom_line(color = "black") +
    geom_point(size = 3, color = "black") +
    geom_errorbar(aes(ymin = .data[[mean_var]] - .data[[se_var]], ymax = .data[[mean_var]] + .data[[se_var]]), 
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +  # Add dotted line at y = 0
    labs(title = title, x = NULL, y = NULL) +  # Remove individual y-axis labels
    ylim(-max_y_limit, max_y_limit) +  # Set symmetrical y-limits
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      legend.position = "none"
    )
}


# Create individual plots with the scaled summary data
plot_latency_enter <- plot_delta_variable(data_summary, "mean_enter", "se_enter", "")
plot_latency_eat <- plot_delta_variable(data_summary, "mean_eat", "se_eat", "")
plot_zoi_duration <- plot_delta_variable(data_summary, "mean_zoi", "se_zoi", "")

# Combine plots using patchwork with a common y-axis label
combined_plot_scaled <- (plot_latency_enter | plot_latency_eat | plot_zoi_duration) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = "bottom") &
  theme(plot.tag.position = "bottom") &
  labs(y = expression(atop("Neophobic Response", paste(plain("Less Neophobic") * phantom("   ") * plain("More Neophobic")))))






# Print the final combined plot
print(combined_plot_scaled)



final_combined_plot <- (combined_plot_1 / combined_plot_scaled) +  # Use "/" for vertical stacking, "|" for horizontal
  plot_layout(ncol = 1, heights = c(1, 1))

final_combined_plot
