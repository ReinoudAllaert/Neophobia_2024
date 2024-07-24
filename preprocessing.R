#### load/install packages ####

install.packages('dplyr')
install.packages('stringr')
install.packages('tidyr')
install.packages('here')
install.packages('lubridate')
install.packages('readxl')
install.packages('purrr')

library(dplyr)
library(stringr)
library(tidyr)
library(here)
library(lubridate)
library(readxl)
library(purrr)

#### load bird metadata + BORIS data #### 
chick_data <- read_excel("~/ECoBird Dropbox/Reinoud Allaert/Gull_2024/data/raw_data/2024_gull_data.xlsx", sheet = "chicks")
# birds that did not die
# create unique ID -> cage + colour
chick_data <- chick_data %>%
  filter(is.na(comments)) %>%
  mutate(bird_ID = paste0(enclosure, "_", substr(rr_grp, 2, 2), "_", neocol))

data_1 <- read.csv("BORIS_SK.csv")
data_2 <- read.csv("BORIS_RA.csv")
data <- rbind(data_1, data_2)

#### preprocess BORIS data ####

# Both experimenters coded the same day, remove those once
data <- data %>% filter(Observation.id != "B2_3_A_novel_SK")

# remove NA cols, remove .mp4
data <- data %>%
  select_if(~any(!is.na(.))) %>%
  mutate(Media.file = str_replace_all(Media.file, "\\.mp4$", ""))

# _ in colnames is easier
colnames(data) <- gsub("\\.", "_", colnames(data))

# extrac trial
data <- data %>%
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
data <- data %>%
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
trial_start_data <- subset(data, Behavior == "Trial start")
## split data in group vs individual trials
individual_data <- data %>%
  filter(str_length(ID) == 5)

group_data <- data %>%
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


# Check whether we have 4 + 4x trials per bird
trial_start_data <- subset(individual_data, Behavior == "Trial start")

# Count the number of rows per ID
count_per_ID <- table(trial_start_data$ID)

# Convert the table to a data frame for better readability
count_df <- as.data.frame(count_per_ID)
names(count_df) <- c("ID", "n")
count_df<- filter(count_df, n != 4)

# 0, so OK
nrow(count_df)

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

## Check whether we have 4 + 4x trials per bird
trial_start_data <- subset(group_data, Behavior == "Trial start")

# Count the number of rows per ID
count_per_ID <- table(trial_start_data$chick_id)

# Convert the table to a data frame for better readability
count_df <- as.data.frame(count_per_ID)
names(count_df) <- c("ID", "n")
count_df<- filter(count_df, n != 4)
# 0, SO OK
count_df

#### Merge dfs ####

individual_data <- individual_data %>% mutate(chick_id = ID)

# find common columns
common_columns <- intersect(names(individual_data), names(group_data))

# select only common columns
individual_data_common <- individual_data %>% select(all_of(common_columns))
group_data_common <- group_data %>% select(all_of(common_columns))

# combine the data frames
combined_data <- rbind(individual_data_common, group_data_common) %>% 
  select(-Test_arena_entry_time)

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


