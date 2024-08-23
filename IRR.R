packages <- c("irr", "dplyr", "readxl", "stringr", "tidyr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Check IRR between coders

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)

#### data ####
#### load bird metadata + BORIS data #### 
chick_data <- read_excel("2024_chick_data.xlsx")
# birds that did not die
# create unique ID -> cage + colour
chick_data <- chick_data %>%
  filter(is.na(comments)) %>%
  mutate(bird_ID = paste0(enclosure, "_", substr(rr_grp, 2, 2), "_", neocol))

data <- read.csv("neophobia_data.csv", row.names=1)
# unprocessed IRR data
data_IRR <- read.csv("BORIS_IRR_SB.csv")


#### preprocess BORIS data ####

# same workflow as preprocessing, now for IRR data
# remove NA cols, remove .mp4
data_IRR <- data_IRR %>%
  select_if(~any(!is.na(.))) %>%
  mutate(Media.file = str_replace_all(Media.file, "\\.mp4$", ""))

# _ in colnames is easier
colnames(data_IRR) <- gsub("\\.", "_", colnames(data_IRR))

# extrac trial
data_IRR <- data_IRR %>%
  mutate(
    trial_ID = Observation_id)

# ID is sometimes XX_XX or X, function to extract right piece of information
extract_id <- function(parts) {
  if (length(parts) == 5) {
    return(parts[3])
  } else if (length(parts) == 6) {
    return(paste(parts[3], parts[4], sep = "_"))
  } else {
    return(NA)
  }
}

# split obs name in diff cols
data_IRR <- data_IRR %>%
  mutate(
    parts = str_split(Observation_id, "_"),
    enclosure = sapply(parts, function(x) x[1]),
    trial_day = sapply(parts, function(x) x[2]),
    ID = sapply(parts, extract_id),
    trial_type = ifelse(grepl("control", Observation_id), "control", "novel"),
    experimenter = sapply(parts, function(x) x[length(x)])
  ) %>%
  select(-parts)  # Remove the temporary 'parts' column


# filter rows where Behavior is "Trial start"
trial_start_data <- subset(data_IRR, Behavior == "Trial start")
## split data in group vs individual trials
individual_data <- data_IRR %>%
  filter(str_length(ID) == 5)

group_data <- data_IRR %>%
  filter(str_length(ID) != 5)

#### now check whether all data is present for group/ind trials ####
# INDIVIDUAL

# Set end times to exactly 10min
update_trial_times <- function(df) {
  df %>%
    group_by(Observation_id) %>%
    mutate(
      Test_arena_entry_time = ifelse(Behavior == "Test arena entry", Start__s_, NA),
      Trial_start_time = ifelse(Behavior == "Trial start", Start__s_, NA),
      Test_arena_entry_time = ifelse(all(is.na(Test_arena_entry_time)), NA, min(Test_arena_entry_time, na.rm = TRUE)),
      Trial_start_time = ifelse(all(is.na(Trial_start_time)), NA, min(Trial_start_time, na.rm = TRUE)),
      adjusted_time = ifelse(!is.na(Test_arena_entry_time), Test_arena_entry_time, Trial_start_time),
      Start__s_ = ifelse(Behavior == "Trial end" & !is.na(adjusted_time), adjusted_time + 600, Start__s_)
    ) %>%
    ungroup() %>%
    select(-adjusted_time) # Remove the temporary column
}

# Apply the function to individual trials
individual_data <- update_trial_times(individual_data)


# GROUP
# First split group trials in individual observations
# Go from group trials to values for ind birds (+ update start time)
split_subjects_and_update_start <- function(df) {
  df %>%
    group_by(Observation_id) %>%
    do({
      no_focal <- filter(., Subject == "No focal subject")
      focal <- filter(., Subject != "No focal subject")
      
      # Get unique subjects
      unique_subjects <- unique(focal$Subject)
      
      if (nrow(focal) > 0 & nrow(no_focal) > 0) {
        no_focal <- no_focal[rep(1:nrow(no_focal), each = length(unique_subjects)), ]
        no_focal$Subject <- rep(unique_subjects, times = nrow(no_focal) / length(unique_subjects))
      }
      
      combined <- bind_rows(no_focal, focal)
      
      combined <- combined %>%
        group_by(Observation_id, Subject) %>%
        mutate(
          Test_arena_entry_time = ifelse(Behavior == "Test arena entry", Start__s_, NA),
          Test_arena_entry_time = min(Test_arena_entry_time, na.rm = TRUE),
          Test_arena_entry_time = ifelse(is.infinite(Test_arena_entry_time), NA, Test_arena_entry_time),
          Start__s_ = ifelse(Behavior == "Trial end" & !is.na(Test_arena_entry_time), Test_arena_entry_time + 600, Start__s_)
        ) %>%
        ungroup()
      
      combined
    }) %>%
    distinct() %>%
    ungroup()
}
# Apply the function to the dataset
group_data <- split_subjects_and_update_start(group_data)

# streamline ID naming
group_data <- group_data %>%
  mutate(ID = gsub("^2A$", "A", ID),
         ID = gsub("^2B$", "B", ID))

# construct identifier for merging
group_data <- group_data %>% mutate(bird_ID = paste0(enclosure,"_", ID, "_", Subject))

group_data$bird_ID <- gsub("black", "bla", group_data$bird_ID)
group_data$bird_ID <- gsub("blue", "blu", group_data$bird_ID)

group_data <- group_data %>%
  left_join(select(chick_data, chick_id, bird_ID), by = "bird_ID")


#### Merge dfs ####

individual_data <- individual_data %>% mutate(chick_id = ID)

# find common columns
common_columns <- intersect(names(individual_data), names(group_data))

# select only common columns
individual_data_common <- individual_data %>% select(all_of(common_columns))
group_data_common <- group_data %>% select(all_of(common_columns))

# combine the data frames
combined_data <- rbind(individual_data_common, group_data_common) %>% 
  select(-Test_arena_entry_time) %>%
  mutate(trial_category = ifelse(Observation_id %in% unique(group_data$Observation_id), "group", "individual"))


#### Check data structure ####

# asserts to check:
##### Trial start present #####
trial_start_check <- combined_data %>%
  group_by(chick_id, trial_day) %>%
  summarize(has_trial_start = any(Behavior == "Trial start")) %>%
  ungroup() %>%
  filter(!has_trial_start)

# empty, OK
trial_start_check

##### Trial entry present #####
# except for RY_GG D1, RR_BY D1 and GG_PR D2 which did not participate
entry <- combined_data %>%
  group_by(chick_id, trial_day) %>%
  summarize(has_trial_entry = any(Behavior == "Test arena entry")) %>%
  ungroup() %>%
  filter(!has_trial_entry)

# Check OK
entry

##### Only eating in ZOI #####
results <- list()
# Get unique combinations of chick_id and trial_day
unique_combinations <- combined_data %>%
  select(chick_id, trial_day) %>%
  distinct()

# Iterate over each combination
for (i in seq_len(nrow(unique_combinations))) {
  chick_id <- unique_combinations$chick_id[i]
  trial_day <- unique_combinations$trial_day[i]
  
  # Subset data for the current combination
  subset_data <- combined_data %>%
    filter(chick_id == chick_id, trial_day == trial_day)
  
  # Extract eating times
  eating_times <- subset_data$Start__s_[subset_data$Behavior == "Eating"]
  
  # Extract ZOI intervals
  zoi_intervals <- subset_data %>%
    filter(Behavior == "Zone of Interest") %>%
    select(Start__s_, Stop__s_)
  
  # Check if eating times fall within any ZOI intervals
  eating_in_zoi <- sapply(eating_times, function(eating_time) {
    any(eating_time >= zoi_intervals$Start__s_ & eating_time <= zoi_intervals$Stop__s_)
  })
  
  # Store results for combinations where eating is not within ZOI
  if (length(eating_times) > 0 && !all(eating_in_zoi)) {
    results <- append(results, list(list(
      chick_id = chick_id,
      trial_day = trial_day,
      eating_times = eating_times,
      zoi_intervals = zoi_intervals
    )))
  }
}

# Empty, OK
results

# no need to check trial end, as manually set in function above

#### Data for models ####
# add latencies and time in ZOI
metrics_data <- combined_data %>%
  group_by(chick_id, trial_day, enclosure, trial_type, trial_category) %>%
  summarize(
    trial_start_time = min(Start__s_[Behavior == "Trial start"], na.rm = TRUE),
    entry_time = ifelse(any(Behavior == "Test arena entry"), min(Start__s_[Behavior == "Test arena entry"], na.rm = TRUE), NA),
    eating_time = ifelse(any(Behavior == "Eating"), min(Start__s_[Behavior == "Eating"], na.rm = TRUE), NA),
    zoi_duration = sum(Stop__s_[Behavior == "Zone of Interest"] - Start__s_[Behavior == "Zone of Interest"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    latency_to_enter = ifelse(is.na(entry_time), 600, entry_time - trial_start_time),
    latency_to_eat = ifelse(is.na(eating_time), 600, eating_time - entry_time),
    zoi_duration = replace_na(zoi_duration, 0)
  ) %>%
  select(chick_id, trial_day, enclosure, trial_type, trial_category, latency_to_enter, latency_to_eat, zoi_duration)


# add object
#this is the test schedule
schedule <- data.frame(
  Day_Cage = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7", "Day 8", "Day 9", "Day 10", "Day 11", "Day 12", "Day 13", "Day 14", "Day 15", "Day 16", "Day 17"),
  B2 = c("GC1 - 1", "IC1 - 1", "GT1 - 4", "IT1 - 3", "GC2 - 1", "IC2 - 1", "GT2 - 2", "IT2 - 5", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  B3 = c("IC1 - 2", "GT1 - 4", "IT1 - 1", "GC1 - 2", "IC2 - 2", "GT2 - 5", "IT2 - 3", "GC2 - 2", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  B4 = c(NA, NA, NA, "GT1 - 5", "IT1 - 1", "GC1 - 3", "IC1 - 3", "GT2 - 4", "IT2 - 2", "GC2 - 3", "IC2 - 3", NA, NA, NA, NA, NA, NA),
  B5 = c(NA, NA, NA, "IT1 - 1", "GC1 - 4", "IC1 - 4", "GT1 - 3", "IT2 - 2", "GC2 - 4", "IC2 - 4", "GT2 - 5", NA, NA, NA, NA, NA, NA),
  B6 = c(NA, NA, NA, NA, NA, NA, "GC1 - 1", "IC1 - 1", "GT1 - 2", "IT1 - 3", "GC2 - 1", "IC2 - 1", "GT2 - 4", "IT2 - 5", NA, NA, NA),
  B7 = c(NA, NA, NA, NA, NA, NA, "IC1 - 2", "GT1 - 3", "IT1 - 4", "GC1 - 2", "IC2 - 2", "GT2 - 5", "IT2 - 1", "GC2 - 2", NA, NA, NA),
  B8 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "GT1 - 2", "IT1 - 4", "GC1 - 3", "IC1 - 3", "GT2 - 1", "IT2 - 5", "GC2 - 3", "IC2 - 3"),
  B9 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "IT1 - 1", "GC1 - 4", "IC1 - 4", "GT1 - 5", "IT2 - 2", "GC2 - 4", "IC2 - 4", "GT2 - 3")
)

# format schedule for data extraction
data_long <- schedule %>%
  pivot_longer(cols = starts_with("B"), names_to = "enclosure", values_to = "Trial") %>%
  filter(!is.na(Trial) & Trial != "") %>%
  mutate(Object_Type = as.integer(sub(".* - ", "", Trial))) %>%
  group_by(enclosure) %>%
  mutate(trial_day = as.character(row_number())) %>%
  ungroup()

# merge the object type data with the metrics data
metrics_data <- metrics_data %>%
  left_join(data_long, by = c("enclosure", "trial_day")) %>%
  select(chick_id, trial_day, enclosure, trial_type, trial_category, latency_to_enter, latency_to_eat, zoi_duration, Object_Type)

# add the groupID and nestID
metrics_data <- metrics_data %>%
  left_join(select(chick_data, chick_id, rr_grp, egg_id), by = "chick_id") %>%
  mutate(
    GroupID = ifelse(trial_category == "individual", NA, rr_grp),
    NestID = substr(egg_id, 1, nchar(egg_id) - 1)
  ) %>%
  select(-rr_grp, -egg_id)

# rename columns to be consistent with RR
metrics_data <- metrics_data %>%
  rename(
    Latency_to_Eat = latency_to_eat,
    Object = trial_type,
    Trial = trial_day,
    Context = trial_category,
    Bird_ID = chick_id,
    Latency_to_enter = latency_to_enter,
    Zoi_duration = zoi_duration,
    Enclosure = enclosure
  ) %>%
  mutate(group_dummy = if_else(Context == "group", 1, 0),
         ind_dummy  = if_else(Context == "individual", 1, 0))

# Add contrast coding as described in RR
metrics_data <- metrics_data %>%
  mutate(
    Object_contrast = case_when(
      Object == "control" ~ -0.5,   
      Object == "novel" ~ 0.5    
    ),
    Context_contrast = case_when(
      Context == "group" ~ 0.5,
      TRUE ~ -0.5                          
    )
  )

metrics_data
write.csv(metrics_data, "neophobia_data_IRR.csv")


#################

filtered_metrics_data <- metrics_data %>%
  select(Bird_ID, Trial, Latency_to_enter, Latency_to_Eat, Zoi_duration)

filtered_data <- data %>%
  select(Bird_ID, Trial, Latency_to_enter, Latency_to_Eat, Zoi_duration)

# Merge datasets on Bird_ID and Trial
combined_data <- merge(filtered_metrics_data, filtered_data, by = c("Bird_ID", "Trial"), suffixes = c("_metrics", "_data"))

# Define the acceptable deviation thresholds
latency_threshold <- 1.5  # 1 second for latencies
zoi_threshold <- 5      # 5 seconds for Zoi_duration

# Calculate absolute differences and identify problematic observations
problematic_observations <- combined_data %>%
  mutate(
    Difference_Latency_to_enter = abs(Latency_to_enter_metrics - Latency_to_enter_data),
    Difference_Latency_to_eat = abs(Latency_to_Eat_metrics - Latency_to_Eat_data),
    Difference_Zoi_duration = abs(Zoi_duration_metrics - Zoi_duration_data)
  ) %>%
  filter(
    Difference_Latency_to_enter > latency_threshold |
      Difference_Latency_to_eat > latency_threshold |
      Difference_Zoi_duration > zoi_threshold
  ) %>%
  select(Bird_ID, Trial, Difference_Latency_to_enter, Difference_Latency_to_eat, Difference_Zoi_duration)

# Print the problematic observations
print(problematic_observations)



filtered_metrics_data <- metrics_data %>%
  select(Bird_ID, Trial, Latency_to_enter, Latency_to_Eat, Zoi_duration)

filtered_data <- data %>%
  select(Bird_ID, Trial, Latency_to_enter, Latency_to_Eat, Zoi_duration)

# Merge datasets on Bird_ID and Trial
combined_data <- merge(filtered_metrics_data, filtered_data, by = c("Bird_ID", "Trial"), suffixes = c("_metrics", "_data"))

# Calculate ICC for Latency_to_enter
icc_latency_to_enter <- irr::icc(combined_data[, c("Latency_to_enter_metrics", "Latency_to_enter_data")], model = "twoway", type = "consistency", unit = "single")

# Calculate ICC for Latency_to_Eat
icc_latency_to_eat <- irr::icc(combined_data[, c("Latency_to_Eat_metrics", "Latency_to_Eat_data")], model = "twoway", type = "consistency", unit = "single")

# Calculate ICC for Zoi_duration
icc_zoi_duration <- irr::icc(combined_data[, c("Zoi_duration_metrics", "Zoi_duration_data")], model = "twoway", type = "consistency", unit = "single")

# Display ICC results
print(icc_latency_to_enter)
print(icc_latency_to_eat)
print(icc_zoi_duration)

