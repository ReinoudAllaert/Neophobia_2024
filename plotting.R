# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif)
library(patchwork)
library(ggside)

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


data <- read.csv("processed_data/neophobia_data.csv", row.names = 1)

# Calculate delta values
data_delta <- data %>%
  group_by(Bird_ID, Context) %>%
  summarise(
    Latency_to_enter_delta = sum(Latency_to_enter[Object == "novel"]) - sum(Latency_to_enter[Object == "control"]),
    Latency_to_eat_delta = sum(Latency_to_Eat[Object == "novel"]) - sum(Latency_to_Eat[Object == "control"]),
    ZOI_duration_delta = sum(Zoi_duration[Object == "novel"]) - sum(Zoi_duration[Object == "control"])
  ) %>%
  ungroup()

# Prepare the data to ensure Bird_ID and Context are factors
plot_data <- data_delta %>%
  mutate(
    Bird_ID = as.factor(Bird_ID),
    Context = factor(Context, levels = c("individual", "group"))
  )

# Invert ZOI duration delta if necessary
plot_data$ZOI_duration_delta <- -1 * plot_data$ZOI_duration_delta

# Function to create individual line plots with symmetrical y-limits and a mean line
plot_delta_variable_as_line <- function(data, var, title) {
  max_val <- max(abs(data[[var]]), na.rm = TRUE)
  
  # Calculate mean for each context
  mean_data <- data %>%
    group_by(Context) %>%
    summarise(mean_value = mean(.data[[var]], na.rm = TRUE))
  
  ggplot(data, aes(x = Context, y = .data[[var]], group = Bird_ID)) +
    # Individual lines for each bird
    geom_line(color = "darkgrey", linetype = "dashed", size = 0.5) +
    # Mean line (preventing inheritance of Bird_ID)
    geom_line(
      data = mean_data,
      aes(x = Context, y = mean_value, group = 1),
      color = "black",
      size = 1,
      inherit.aes = FALSE
    ) +
    # Mean points (preventing inheritance of Bird_ID)
    geom_point(
      data = mean_data,
      aes(x = Context, y = mean_value, group = 1),
      color = "black",
      size = 3,
      inherit.aes = FALSE
    ) +
    labs(title = title, x = NULL, y = NULL) +
    ylim(-max_val, max_val) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      legend.position = "none"
    )
}

# Create individual line plots for each dependent variable with symmetrical y-limits
plot_latency_enter_line <- plot_delta_variable_as_line(
  plot_data,
  "Latency_to_enter_delta",
  ""
) + labs(x = "Latency to enter") # Remove title, keep consistent X-axis label

plot_latency_eat_line <- plot_delta_variable_as_line(
  plot_data,
  "Latency_to_eat_delta",
  ""
) + labs(x = "Latency to eat") # Remove title, keep consistent X-axis label

plot_zoi_duration_line <- plot_delta_variable_as_line(
  plot_data,
  "ZOI_duration_delta",
  ""
) + labs(x = "Inverted ZOI duration")

# Combine the individual line plots side by side
combined_line_plots <- (plot_latency_enter_line | plot_latency_eat_line | plot_zoi_duration_line) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') & # Add tags A, B, C
  theme(plot.tag.position = "topleft") & # Position tags at the top left
  labs(y = expression(atop("Neophobic response",
                           paste(plain("Less neophobic") * phantom("   ") * plain("More neophobic")))))



# Function to create boxplots in black and white with manually specified y-axis limits
plot_delta_variable_as_boxplot <- function(data, var, title, y_limits) {
  ggplot(data, aes(x = Context, y = .data[[var]])) +
    geom_boxplot(fill = "white", color = "black", outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
    labs(title = title, x = NULL, y = NULL) +
    ylim(y_limits[1], y_limits[2]) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      legend.position = "none"
    )
}

# Set y-axis limits for each boxplot subplot manually
y_limits_latency_enter <- c(-20, 20)
y_limits_latency_eat <- c(-1200, 1200)
y_limits_zoi_duration <- c(-600, 600)

# Create boxplots for each dependent variable with custom y-axis limits
plot_latency_enter_boxplot <- plot_delta_variable_as_boxplot(
  plot_data,
  "Latency_to_enter_delta",
  "Latency to enter",
  y_limits_latency_enter
)
plot_latency_eat_boxplot <- plot_delta_variable_as_boxplot(
  plot_data,
  "Latency_to_eat_delta",
  "Latency to eat",
  y_limits_latency_eat
)
plot_zoi_duration_boxplot <- plot_delta_variable_as_boxplot(
  plot_data,
  "ZOI_duration_delta",
  "Inverted ZOI duration",
  y_limits_zoi_duration
)

# Combine the boxplots side by side
combined_box_plots <- (plot_latency_enter_boxplot | plot_latency_eat_boxplot | plot_zoi_duration_boxplot) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = "bottom") &
  labs(y = expression(atop("Neophobic response",
                           paste(plain("Less neophobic") * phantom("   ") * plain("More neophobic")))))

# Combine the line plots on top and the boxplots on the bottom
final_combined_plot <- (combined_line_plots / combined_box_plots) +
  plot_layout(ncol = 1, heights = c(1, 1))

# Display the final combined plot
print(final_combined_plot)



