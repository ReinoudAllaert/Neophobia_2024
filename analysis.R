#### load/install packages ####

install.packages(c('lmerTest', 'lme4', 'ggplot2', 'tidyverse',
                   'summarytools', 'readxl', 'purrr'))


library(lmerTest)
library(lme4)
library(ggplot2)
library(tidyverse)
library(summarytools)

data <- read.csv("neophobia_data.csv", row.names=1)


# quick look at structure
str(data)
summary(data)
dfSummary(data)


# not sure what groupID values should be during individual trials
# currently coded as NA

# if we want 0, just change NA to 0
data$GroupID[is.na(data$GroupID)] <- "0"

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

# this works
model <- lmer(Latency_to_enter ~ Object_contrast * Context_contrast + Trial + 
                (1 | Bird_ID), 
              data = data)
summary(model)



# as described in RR
# random effect structure too complicated for amount of data?
RR_model <- lmer(Latency_to_Eat ~ Object_contrast * Context_contrast + Trial + 
                (1 | NestID) + 
                (1 + group_dummy | GroupID) + 
                (1 + ind_dummy + group_dummy | Bird_ID), 
              data = data)

summary(RR_model)

