packages <- c('lmerTest', 'lme4', 'ggplot2', 'tidyverse', 'readxl', 'purrr', 'performance', 'emmeans')

# Check if each package is installed; if not, install it
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)

data <- read.csv("processed_data/neophobia_data.csv", row.names=1)

##### just adding a line here 

# quick look at structure
str(data)
summary(data)


# not sure what groupID values should be during individual trials
# currently coded as NA

# if we want 0, just change NA to 0
#data$GroupID[is.na(data$GroupID)] <- "0"

# if we want the actual group, lookup groupID per bird
 most_frequent_group <- function(group_ids) {
  group_ids <- group_ids[!is.na(group_ids)]
  if (length(group_ids) == 0) return(NA)
  names(sort(table(group_ids), decreasing = TRUE))[1]
 }

# add to every bird
most_frequent_group_per_bird <- data %>%
  group_by(Bird_ID) %>%
  summarize(most_frequent_group = most_frequent_group(GroupID)) %>%
  ungroup()

data <- data %>%
  left_join(most_frequent_group_per_bird, by = "Bird_ID") %>%
  mutate(GroupID = ifelse(is.na(GroupID), most_frequent_group, GroupID)) %>%
  select(-most_frequent_group)

# basic model to try
model <- lmer(Latency_to_enter ~ Object_contrast * Context_contrast + Trial + 
                (1 | Bird_ID), 
              data = data)
summary(model)



# as described in RR
# random effect structure too complicated for amount of data?
enter_model <- lmer(Latency_to_enter ~ Object_contrast * Context_contrast + Trial + 
                (1 | NestID) + 
                (1 + group_dummy | GroupID) + 
                (1 + ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(enter_model)
check_model(enter_model)


eat_model <- lmer(Latency_to_Eat ~ Object_contrast * Context_contrast + Trial + 
                   (1 | NestID) + 
                   (1 + group_dummy | GroupID) + 
                   (1 + ind_dummy + group_dummy | Bird_ID), 
                 data = data)

summary(eat_model)
check_model(eat_model)

emm_eat <- emmeans(eat_model, ~ Object_contrast * Context_contrast)
pairs(emm_eat, adjust = "bonferroni")

zoi_model <- lmer(Zoi_duration ~ Object_contrast * Context_contrast + Trial + 
                    (1 | NestID) + 
                    (1 + group_dummy | GroupID) + 
                    (1 + ind_dummy + group_dummy | Bird_ID), 
                  data = data)

summary(zoi_model)
check_model(zoi_model)

# in RR we also mention analysing latencies combined

multivariate_model 



