# Chapter 3 — Analysis Overview

# This is the analysis of Chapter 3 of the dissertation.
# It consists of three main tasks and two preliminary studies.
## The written English task, Path >> Manner
## The written English task, Manner >> Path
### Preliminary study with no gesture
### Preliminary study with no event
## Gesture study

# To make the analyses run, please make sure that the working directory is 
# set to the folder "Organized."

# Start--------------------------

# Load Required Libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(mgcv)
library(ggeffects)
library(readxl)
library(performance)
library(lmerTest)
library(emmeans)
library(brms)

# Set Seed for Reproducibility
set.seed(123)

## Experiment I: Written English Task, Path >> Manner ---------------------

data_writtenEnglish_i <- read.csv("WrittenEnglish_PathManner.csv")

# Data Inspection: Affirmative and Negative Polarities
View(data_writtenEnglish_i)

# Data Preparation
# 1 - Variable Selection and Description:
# The following variables are extracted for analysis:
  # Polarity: Sentence polarity (Affirmative, Negative)
  # Sentence: Modifier type in the sentence (P, M, C)
  # Event: Event type depicted in the GIF (P, M, C, 0)
  # Scenario: Scenario identifier (A–E), referring to the object shown in the GIF
  # Response_ID: Participant identifier
  # Rating: Participant response rating
data_writtenEnglish_i <- data_writtenEnglish_i %>%
  separate(Trial, into = c("Polarity", "SentenceType", "EventType", "Scenario"), sep = " ", remove = FALSE)

data_writtenEnglish_i <- data_writtenEnglish_i %>% mutate(
  Polarity = case_when(
    grepl("Aff", Polarity) ~ "Affirmative", 
    TRUE ~ "Negative"),
  SentenceType = case_when(
    grepl("M", SentenceType) ~ "Manner Modification",
    grepl("P", SentenceType) ~ "Path Modification",
    TRUE ~ "Conflated Modification"),
  EventType = case_when(
    grepl("M", EventType) ~ "Manner Event",
    grepl("P", EventType) ~ "Path Event",
    grepl("0", EventType) ~ "No Motion",
    TRUE ~ "Conflated Event"),
  Scenario = case_when(
    grepl("D", Scenario) ~ "Plank",
    grepl("E", Scenario) ~"Chair",
    grepl("B", Scenario) ~"Car",
    grepl("A", Scenario) ~ "Leaf", 
    TRUE ~ "Paper"), 
  Rating  = as.numeric(Rating)) 

# 2 - Factor Conversion of relevant categorical variables to ensure correct treatment in the analyses
data_writtenEnglish_i <- data_writtenEnglish_i %>%
  mutate(
    Response_ID = as.factor(Response_ID),
    SentenceType = factor(SentenceType, levels = c("Conflated Modification", "Manner Modification", "Path Modification")),
    EventType = factor(EventType, levels = c("Conflated Event", "Manner Event", "Path Event",  "No Motion")),
    Scenario = factor(Scenario, levels = c("Leaf", "Car", "Plank", "Chair", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
    Rating  = as.numeric(Rating)
    )

# 3 - Data Frame Conversion to ensure compatibility with functions and modeling procedures
data_writtenEnglish_i %>% as.data.frame()

# Data Inspection: Structure Check
str(data_writtenEnglish_i) # 150 participants
View(data_writtenEnglish_i)
# Data Quality Control: Detection of Inattentive Participants
# 1 - Compute the standard deviation (SD) of ratings for each participant
sd_by_participant <- data_writtenEnglish_i %>%
  group_by(Response_ID) %>%
  summarise(
    sd_rating = sd(Rating, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop")

# 2 - Determine the 5th percentile threshold for SD values
sd_threshold <- quantile(sd_by_participant$sd_rating, probs = 0.05, na.rm = TRUE)
print(sd_threshold) # 15.66882 

# 3 - Flag participants whose SD falls below the 5th percentile
sd_by_participant <- sd_by_participant %>%
  mutate(flag_low_variability = sd_rating < sd_threshold)

# 4 - Extract the subset of participants flagged for low variability
flagged_participants <- sd_by_participant %>%
  filter(flag_low_variability)

print(flagged_participants) # 8 participant to check

## List of the excluded participants for future reference
## 1 R_11cQ5fJSVqWA6nP     14.8  TRUE                
## 2 R_1PtxRc3IB2uCmcP      0    TRUE                
## 3 R_1rUCA3HOXxJwkef     12.5  TRUE                
## 4 R_3d3HYxD2V3TsTon      2.68 TRUE                
## 5 R_3sThvPSDN6PfDxI      9.80 TRUE                
## 6 R_5CQDs7NjJ5Gcacu      2.61 TRUE                
## 7 R_6EYUo4jqFe9A9km     10.9  TRUE                
## 8 R_7L2ZFH30GeZZJpD      9.40 TRUE

## Excluded participants based on the feedback comments
## 9 R_6AHzPTToSASboUX	feedback
## 10 R_55GfGQA10UMwSdj	feedback

# Data Cleaning: Participant Exclusion (a total of 10 participants)
flagged_ids <- sd_by_participant %>%
  filter(flag_low_variability) %>%
  pull(Response_ID)

data_writtenEnglish_i_clean <- data_writtenEnglish_i %>%
  filter(!Response_ID %in% flagged_ids)

ids_to_remove_feedback <- c("R_6AHzPTToSASboUX", "R_55GfGQA10UMwSdj")

data_writtenEnglish_i_clean <- data_writtenEnglish_i_clean %>%
  filter(!Response_ID %in% ids_to_remove_feedback) %>% droplevels()

# Data Cleaning Completed
View(data_writtenEnglish_i_clean)
str(data_writtenEnglish_i_clean) # 140 participants present

# Data Inspection: Post-Cleaning Overview

plot_writtenEnglish_i_All <- ggplot(data_writtenEnglish_i_clean, aes(x = EventType, y = Rating, fill = SentenceType, color = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.7, outliers = FALSE) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Event Type", y = "Rating") +
  theme_minimal(base_size = 24) +
  theme(
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

plot_writtenEnglish_i_All

data_writtenEnglish_i_clean_avg <- data_writtenEnglish_i_clean %>%
  group_by(SentenceType, EventType, Polarity) %>%
  summarise(
    MeanRating = mean(Rating, na.rm = TRUE),
    SDRating = sd(Rating, na.rm = TRUE),
    Count = n(),
    SE = SDRating / sqrt(Count),  # Standard Error Calculation
    .groups = "drop"
  )

print(data_writtenEnglish_i_clean_avg, n = 24)

ggplot(data_writtenEnglish_i_clean_avg, aes(x = EventType, y = MeanRating, fill = SentenceType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar chart
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE),
                width = 0.2, position = position_dodge(0.9)) +  # Error bars (Standard Error)
  facet_wrap(~ Polarity) +  # Facet by Polarity (Affirmative/Negative)
  theme_minimal(base_size = 14) +
  ylim(0,100) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")) +
  labs(title = "Mean Ratings of Gestures Across Events",
       x = "Polarity",
       y = "Mean Rating",
       fill = "Sentence Type")

# The distribution of the ratings does not approximate a normal distribution, 
# which might be due to the presence of several variables resulting in the ratings 
# on the extreme ends of the scale. Consequently, the data are reorganized prior 
# to further statistical analysis to get a better understanding.


### Data Subsetting----------------------

#### Affirmative Polarity-------------------

data_writtenEnglish_i_Aff <- data_writtenEnglish_i_clean %>%
  filter(
    Polarity == "Affirmative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_writtenEnglish_i_Aff)
str(data_writtenEnglish_i_Aff)

ggplot(data = data_writtenEnglish_i_Aff, aes(x = EventType, y = Rating, color = SentenceType, fill = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outliers = FALSE) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings in Affirmative Polarity Across Event Types",
    x = "Event Type", y = "Rating"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

## What about the data histogram ##
hist(data_writtenEnglish_i_Aff$Rating)

data_writtenEnglish_i_Aff <- data_writtenEnglish_i_Aff %>% mutate(
  SentenceType = factor(SentenceType, levels = c("Conflated Modification", "Manner Modification", "Path Modification")),
  EventType = factor(EventType, levels = c("Conflated Event", "Manner Event", "Path Event")),
)

str(data_writtenEnglish_i_Aff)

epsilon <- 1e-6
data_writtenEnglish_i_Aff$Rating_scaled <- (data_writtenEnglish_i_Aff$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_writtenEnglish_i_Aff$SentenceType) <- contr.treatment(3)
contrasts(data_writtenEnglish_i_Aff$EventType) <- contr.treatment(3)

model_writtenEnglish_i_Aff <- glmmTMB(
  Rating_scaled ~  SentenceType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_writtenEnglish_i_Aff)

summary(model_writtenEnglish_i_Aff)
contrasts(data_writtenEnglish_i_Aff$SentenceType)
# Get predicted values
pred_Aff <- ggpredict(model_writtenEnglish_i_Aff, terms = c("SentenceType", "EventType"))

# Plot
ggplot(pred_Aff, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  labs(
    x = "Sentence Type",
    y = "Predicted Rating (scaled)",
    color = "Event Type"
  ) +
  theme_minimal(base_size = 16)


# Compute summary stats
summary_data <- data_writtenEnglish_i_Aff %>%
  group_by(SentenceType, EventType, Polarity) %>%
  summarise(
    mean_rating = mean(Rating, na.rm = TRUE),
    sd = sd(Rating, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  )

# Plot
ggplot(summary_data %>% filter(Polarity == "Affirmative"),
       aes(x = EventType, y = mean_rating, color = SentenceType, group = SentenceType)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = mean_rating - ci, ymax = mean_rating + ci),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  labs(y = "Mean Rating", x = "Event Type") +
  theme_minimal()

# Get model predictions
emm_Aff <- emmeans(model_writtenEnglish_i_Aff, ~ SentenceType * EventType, type = "response")

# Convert to dataframe
emm_df <- as.data.frame(emm_Aff)
emm_df$response <- emm_df$response * 100
emm_df$lower.CL <- emm_df$asymp.LCL * 100
emm_df$upper.CL <- emm_df$asymp.UCL* 100
# Plot

ggplot(emm_df,
       aes(x = EventType, y = response, color = SentenceType, group = SentenceType)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  labs(y = "Predicted Rating", x = "Event Type") +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.position = "top") +
  theme_minimal(14)

#### Negative Polarity---------------------------

data_writtenEnglish_i_Neg <- data_writtenEnglish_i_clean %>%
  filter(
    Polarity == "Negative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_writtenEnglish_i_Neg)
str(data_writtenEnglish_i_Neg)

ggplot(data = data_writtenEnglish_i_Neg, aes(x = EventType, y = Rating, color = SentenceType, fill = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outliers = FALSE) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "",
    x = "Event Type ", y = "Rating"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

## What about the data histogram ##
hist(data_writtenEnglish_i_Neg$Rating)

data_writtenEnglish_i_Neg <- data_writtenEnglish_i_Neg %>% mutate(
  SentenceType = factor(SentenceType, levels = c("Conflated Modification", "Manner Modification", "Path Modification")),
  EventType = factor(EventType, levels = c("Conflated Event", "Manner Event", "Path Event")),
)

str(data_writtenEnglish_i_Neg)

epsilon <- 1e-6
data_writtenEnglish_i_Neg$Rating_scaled <- (data_writtenEnglish_i_Neg$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_writtenEnglish_i_Neg$SentenceType) <- contr.treatment(3)
contrasts(data_writtenEnglish_i_Neg$EventType) <- contr.treatment(3)

model_writtenEnglish_i_Neg <- glmmTMB(
  Rating_scaled ~  SentenceType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_writtenEnglish_i_Neg)
 
summary(model_writtenEnglish_i_Neg)

pred_Neg <- ggpredict(model_writtenEnglish_i_Neg, terms = c("EventType", "SentenceType"))

# Plot
ggplot(pred_Neg, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  labs(
    x = "Sentence Type",
    y = "Predicted Rating (scaled)",
    color = "Event Type"
  ) +
  theme_minimal(base_size = 16)

emm_Neg <- emmeans(model_writtenEnglish_i_Neg, ~ SentenceType * EventType, type = "response")

# Convert to dataframe
emm_df_Neg <- as.data.frame(emm_Neg)
emm_df_Neg$response <- emm_df_Neg$response * 100
emm_df_Neg$lower.CL <- emm_df_Neg$asymp.LCL * 100
emm_df_Neg$upper.CL <- emm_df_Neg$asymp.UCL* 100
# Plot

ggplot(emm_df_Neg,
       aes(x = EventType, y = response, color = SentenceType, group = SentenceType)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.4)) +
  labs(y = "Predicted Rating", x = "Event Type") +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.position = "top") +
  theme_minimal(14)

## Experiment II: Written English Task, Manner >> Path --------------------

data_writtenEnglish_ii <- read.csv("WrittenEnglish_MannerPath.csv")

# Data Inspection: Affirmative and Negative Polarities
View(data_writtenEnglish_ii)

# Data Preparation
# 1 - Variable Selection and Description:
# The following variables are extracted for analysis:
# Polarity: Sentence polarity (Affirmative, Negative)
# Sentence: Modifier type in the sentence (P, M, C)
# Event: Event type depicted in the GIF (P, M, C, 0)
# Scenario: Scenario identifier (A–E), referring to the object shown in the GIF
# Response_ID: Participant identifier
# Rating: Participant response rating
data_writtenEnglish_ii <- data_writtenEnglish_ii %>%
  separate(Trial, into = c("Polarity", "SentenceType", "EventType", "Scenario"), sep = " ", remove = FALSE)

data_writtenEnglish_ii <- data_writtenEnglish_ii %>% mutate(
  Polarity = case_when(
    grepl("Aff", Polarity) ~ "Affirmative", 
    TRUE ~ "Negative"),
  SentenceType = case_when(
    grepl("M", SentenceType) ~ "Manner Modification",
    grepl("P", SentenceType) ~ "Path Modification",
    TRUE ~ "Conflated Modification"),
  EventType = case_when(
    grepl("M", EventType) ~ "Manner Event",
    grepl("P", EventType) ~ "Path Event",
    grepl("0", EventType) ~ "No Motion",
    TRUE ~ "Conflated Event"),
  Scenario = case_when(
    grepl("D", Scenario) ~ "Plank",
    grepl("E", Scenario) ~"Chair",
    grepl("B", Scenario) ~"Car",
    grepl("A", Scenario) ~ "Leaf", 
    TRUE ~ "Paper"), 
  Rating  = as.numeric(Rating)) 

# 2 - Factor Conversion of relevant categorical variables to ensure correct treatment in the analyses
data_writtenEnglish_ii <- data_writtenEnglish_ii %>%
  mutate(
    Response_ID = as.factor(Response_ID),
    SentenceType = factor(SentenceType, levels = c("Conflated Modification", "Manner Modification", "Path Modification")),
    EventType = factor(EventType, levels = c("Conflated Event", "Manner Event", "Path Event",  "No Motion")),
    Scenario = factor(Scenario, levels = c("Leaf", "Car", "Plank", "Chair", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
    Rating  = as.numeric(Rating)
  )

# 3 - Data Frame Conversion to ensure compatibility with functions and modeling procedures
data_writtenEnglish_ii %>% as.data.frame()

# Data Inspection: Structure Check
str(data_writtenEnglish_ii) # 120 participants
View(data_writtenEnglish_ii)

# Data Quality Control: Detection of Inattentive Participants
# 1 - Compute the standard deviation (SD) of ratings for each participant
sd_by_participant_ii <- data_writtenEnglish_ii %>%
  group_by(Response_ID) %>%
  summarise(
    sd_rating = sd(Rating, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop")

# 2 - Determine the 5th percentile threshold for SD values
sd_threshold_ii <- quantile(sd_by_participant_ii$sd_rating, probs = 0.05, na.rm = TRUE)
print(sd_threshold_ii) # 15.57143

# 3 - Flag participants whose SD falls below the 5th percentile
sd_by_participant_ii <- sd_by_participant_ii %>%
  mutate(flag_low_variability_ii = sd_rating < sd_threshold_ii)

# 4 - Extract the subset of participants flagged for low variability
flagged_participants_ii <- sd_by_participant_ii %>%
  filter(flag_low_variability_ii)

print(flagged_participants_ii) # 6 participant to exclude due to their attentive behavior

## List of the excluded participants for future reference 
# 1 R_1EPV6B6NGW8CsfO      8.11       12 TRUE                
# 2 R_3D7rCzMUyegfU03     13.1        12 TRUE                
# 3 R_3gZM5beZQuZlLUw     11.9        12 TRUE                
# 4 R_55zPrqs4qSgjBhf     14.1        12 TRUE                
# 5 R_5TTfCPdqYISGozD      8.01       12 TRUE                
# 6 R_7kFF7aibEh7NaOI     14.2        12 TRUE 

# Data Cleaning: Participant Exclusion 
flagged_ids_ii <- sd_by_participant_ii %>%
  filter(flag_low_variability_ii) %>%
  pull(Response_ID)

data_writtenEnglish_ii_clean <- data_writtenEnglish_ii %>%
  filter(!Response_ID %in% flagged_ids_ii)

#ids_to_remove_feedback <- c("R_3g6c7RprqCv8ane", "R_3D03hDn4dPgGVFb", "R_5ewpTiL9emKTsJr")

data_writtenEnglish_ii_clean <- data_writtenEnglish_ii_clean %>%
  filter(!Response_ID %in% ids_to_remove_feedback) %>% droplevels()

# Data Cleaning Completed
str(data_writtenEnglish_ii_clean) #114 participants
View(data_writtenEnglish_ii_clean)

# Data Inspection: Post-Cleaning Overview
plot_writtenEnglish_ii_All <- ggplot(data_writtenEnglish_ii_clean, aes(x = EventType, y = Rating, color = SentenceType, fill = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings Across Polarity and Event Types",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

plot_writtenEnglish_ii_All

data_writtenEnglish_ii_clean_avg <- data_writtenEnglish_ii_clean %>%
  group_by(SentenceType, EventType, Polarity) %>%
  summarise(
    MeanRating = mean(Rating, na.rm = TRUE),
    SDRating = sd(Rating, na.rm = TRUE),
    Count = n(),
    SE = SDRating / sqrt(Count),  # Standard Error Calculation
    .groups = "drop"
  )

print(data_writtenEnglish_ii_clean_avg, n = 24)

ggplot(data_writtenEnglish_ii_clean_avg, aes(x = EventType, y = MeanRating, fill = SentenceType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar chart
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE),
                width = 0.2, position = position_dodge(0.9)) +  # Error bars (Standard Error)
  facet_wrap(~ Polarity) +  # Facet by Polarity (Affirmative/Negative)
  theme_minimal(base_size = 14) +
  ylim(0,100) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")) +
  labs(title = "Mean Ratings of Gestures Across Events",
       x = "Polarity",
       y = "Mean Rating",
       fill = "Sentence Type")

### Data Subsetting----------------------

#### Affirmative Polarity-------------------

data_writtenEnglish_ii_Aff <- data_writtenEnglish_ii_clean %>%
  filter(
    Polarity == "Affirmative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_writtenEnglish_ii_Aff)
str(data_writtenEnglish_ii_Aff)

ggplot(data = data_writtenEnglish_ii_Aff, aes(x = EventType, y = Rating, color = SentenceType, fill = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outliers = FALSE) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings in Affirmative Polarity Across Event Types",
    x = " ", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


# Average

data_writtenEnglish_ii_Aff_avg <- data_writtenEnglish_ii_Aff %>%
  group_by(SentenceType, EventType) %>%
  summarise(
    MeanRating = mean(Rating, na.rm = TRUE),
    SDRating = sd(Rating, na.rm = TRUE),
    Count = n(),
    SE = SDRating / sqrt(Count),  # Standard Error Calculation
    .groups = "drop"
  )

print(data_writtenEnglish_ii_Aff_avg, n = 24)

ggplot(data_writtenEnglish_ii_Aff_avg, aes(x = EventType, y = MeanRating, fill = SentenceType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar chart
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE),
                width = 0.2, position = position_dodge(0.9)) +  # Error bars (Standard Error)
  theme_minimal(base_size = 14) +
  ylim(0,100) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal", position = position_dodge(width = 0.9), width = 0.2, color="black") +
  theme(
    legend.position = "top",
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(face = "bold")) +
  labs(title = "Mean Ratings of Gestures Across Events",
       x = "Polarity",
       y = "Mean Rating",
       fill = "Sentence Type")

## What about the data histogram ##
hist(data_writtenEnglish_ii_Aff$Rating)

data_writtenEnglish_ii_Aff <- data_writtenEnglish_ii_Aff %>% mutate(
  SentenceType = factor(SentenceType, levels = c("Conflated Modification","Path Modification", "Manner Modification")),
  EventType = factor(EventType, levels = c("Conflated Event", "Path Event",  "Manner Event")),
)

str(data_writtenEnglish_ii_Aff)

epsilon <- 1e-6
data_writtenEnglish_ii_Aff$Rating_scaled <- (data_writtenEnglish_ii_Aff$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_writtenEnglish_ii_Aff$SentenceType) <- contr.treatment(3)
contrasts(data_writtenEnglish_ii_Aff$EventType) <- contr.treatment(3)

model_writtenEnglish_ii_Aff <- glmmTMB(
  Rating_scaled ~  SentenceType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_writtenEnglish_ii_Aff)

summary(model_writtenEnglish_ii_Aff)

emm <- emmeans(model_writtenEnglish_ii_Aff, ~ SentenceType | EventType, type = "response")
pairs(emm)

#### Negative Polarity---------------------------

data_writtenEnglish_ii_Neg <- data_writtenEnglish_ii_clean %>%
  filter(
    Polarity == "Negative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_writtenEnglish_ii_Neg)
str(data_writtenEnglish_ii_Neg)

ggplot(data = data_writtenEnglish_i_Neg, aes(x = EventType, y = Rating, color = SentenceType, fill = SentenceType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings in Affirmative Polarity Across Event Types",
    x = " ", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


## What about the data histogram ##
hist(data_writtenEnglish_ii_Neg$Rating)

data_writtenEnglish_ii_Neg <- data_writtenEnglish_ii_Neg %>% mutate(
  SentenceType = factor(SentenceType, levels = c("Conflated Modification", "Path Modification", "Manner Modification")),
  EventType = factor(EventType, levels = c( "Conflated Event", "Path Event", "Manner Event")),
)

epsilon <- 1e-6
data_writtenEnglish_ii_Neg$Rating_scaled <- (data_writtenEnglish_ii_Neg$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_writtenEnglish_ii_Neg$SentenceType) <- contr.treatment(3)
contrasts(data_writtenEnglish_ii_Neg$EventType) <- contr.treatment(3)

model_writtenEnglish_ii_Neg <- glmmTMB(
  Rating_scaled ~  SentenceType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_writtenEnglish_ii_Neg)

summary(model_writtenEnglish_ii_Neg)

emm <- emmeans(model_writtenEnglish_ii_Neg, ~ SentenceType | EventType, type = "response")
pairs(emm)


### Modifier Order Effect --------------------------------
## Create new data with the Conflated cases
data_writtenEnglish_Conf_i <- data_writtenEnglish_i_clean %>%
  filter(
    SentenceType %in% c("Conflated Modification"),
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")# keep motion event types only
  ) %>% droplevels()

data_writtenEnglish_Conf_ii <- data_writtenEnglish_ii_clean %>%
  filter(
    SentenceType %in% c("Conflated Modification"),
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")# keep motion event types only
  ) %>% droplevels()

data_writtenEnglish_Modifier <- bind_rows(data_writtenEnglish_Conf_i, data_writtenEnglish_Conf_ii)
View(data_writtenEnglish_Modifier)

data_writtenEnglish_Modifier$Order <- ifelse(grepl("P_M", data_writtenEnglish_Modifier$Order), "Experiment I 'in Path in Manner'", "Experiment II 'in Manner in Path'")

data_writtenEnglish_Modifier <- data_writtenEnglish_Modifier %>%
  mutate(
    Order = factor(Order, level = c("Experiment I 'in Path in Manner'", "Experiment II 'in Manner in Path'")),
    EventType = factor(EventType, level = c("Conflated Event","Path Event", "Manner Event"))
  )

str(data_writtenEnglish_Modifier)

#Average

data_writtenEnglish_Modifier_avg <- data_writtenEnglish_Modifier %>%
  group_by(Polarity, EventType, Order) %>%
  summarise(
    MeanRating = mean(Rating, na.rm = TRUE),
    SDRating = sd(Rating, na.rm = TRUE),
    Count = n(),
    SE = SDRating / sqrt(Count),  # Standard Error Calculation
    .groups = "drop"
  )

print(data_writtenEnglish_Modifier_avg, n = 24)

ggplot(data_writtenEnglish_Modifier_avg, aes(x = Polarity, y = MeanRating, fill = Order)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar chart
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE),
                width = 0.2, position = position_dodge(0.9)) +  # Error bars (Standard Error)
  theme_minimal(base_size = 14) +
  facet_wrap(~EventType) +
  ylim(0,100) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal", position = position_dodge(width = 0.9), width = 0.2, color="black") +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")) +
  labs(title = "Mean Ratings of Gestures Across Events",
       x = "Polarity",
       y = "Mean Rating",
       fill = "Sentence Type")
#

hist(data_writtenEnglish_Modifier$Rating)

ggplot(data = data_writtenEnglish_Modifier, aes(x = Polarity, y = Rating, color = EventType, fill = EventType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outliers = FALSE) +
  facet_wrap(~Order) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


# Creating the contrasts
contrasts(data_writtenEnglish_Modifier$EventType) <- contr.treatment(levels(data_writtenEnglish_Modifier$EventType))
contrasts(data_writtenEnglish_Modifier$Order) <- contr.treatment(levels(data_writtenEnglish_Modifier$Order))
contrasts(data_writtenEnglish_Modifier$Polarity) <- contr.treatment(levels(data_writtenEnglish_Modifier$Polarity))

# Fit a model
epsilon <- 1e-6
data_writtenEnglish_Modifier$Rating_scaled <- (data_writtenEnglish_Modifier$Rating / 100) * (1 - 2 * epsilon) + epsilon

model_writtenEnglish_Modifier <- glmmTMB(
  Rating_scaled ~  Order * EventType * Polarity + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_writtenEnglish_Modifier
)

summary(model_writtenEnglish_Modifier)

emm <- emmeans(model_writtenEnglish_Modifier, ~ Order | Polarity * EventType , type = "response")
pairs(emm)


plot(emm, comparisons = TRUE)

a <- brms::brm(Rating_scaled ~ Order * EventType * Polarity + (1 | Response_ID) + (1 | Scenario),
          data = data_writtenEnglish_Modifier)

plot(a)
## Experiment III: Co-Speech Gestures --------------------

# Load Data
data_gesture <- read.csv("Gesture_2025.csv")

# Inspect the Data
head(data_gesture)
summary(data_gesture)

# Data Preparation
data_gesture <- data_gesture %>%
  separate(Trial, into = c("Polarity", "GestureType", "EventType", "Scenario"), sep = " ", remove = FALSE) %>%
  mutate(
    Polarity = ifelse(grepl("Aff", Polarity), "Affirmative", "Negative"),
    GestureType = case_when(
      grepl("M", GestureType) ~ "Manner Gesture",
      grepl("P", GestureType) ~ "Path Gesture",
      TRUE ~ "Conflated Gesture"
    ),
    EventType = case_when(
      grepl("0", EventType) ~ "No Motion",
      grepl("M", EventType) ~ "Manner Event",
      grepl("P", EventType) ~ "Path Event",
      TRUE ~ "Conflated Event"
    ),
    Scenario = case_when(
      grepl("D", Scenario) ~ "Plank",
      grepl("E", Scenario) ~ "Chair",
      grepl("B", Scenario) ~ "Car",
      grepl("A", Scenario) ~ "Leaf",
      TRUE ~ "Paper"
    ),
    Rating = as.numeric(Rating),
    Response_ID = as.factor(Response_ID)
  )

# Data Quality Control: Detection of Inattentive Participants
# 1 - Compute the standard deviation (SD) of ratings for each participant
sd_by_participant_iii <- data_gesture %>%
  group_by(Response_ID) %>%
  summarise(
    sd_rating = sd(Rating, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop")

# 2 - Determine the 5th percentile threshold for SD values
sd_threshold_iii <- quantile(sd_by_participant_iii$sd_rating, probs = 0.05, na.rm = TRUE)
print(sd_threshold_iii) # 14.84392

# 3 - Flag participants whose SD falls below the 5th percentile
sd_by_participant_iii <- sd_by_participant_iii %>%
  mutate(flag_low_variability_iii = sd_rating < sd_threshold_iii)

# 4 - Extract the subset of participants flagged for low variability
flagged_participants_iii <- sd_by_participant_iii %>%
  filter(flag_low_variability_iii)

print(flagged_participants_iii) # 8 participant to exclude due to their attentive behavior

## List of the excluded participants for future reference 
# 1 R_1N4oeWm5P8v8kdH      6.95       12 TRUE                    
# 2 R_5mLqYYr32AXQwXb      3.81       12 TRUE                    
# 3 R_5n6I6YY1RfPIl9i      5.41       12 TRUE                    
# 4 R_66bFzWq66MUxHs9     12.8        12 TRUE                    
# 5 R_6nN1v6L0GujuD9D      7.23       12 TRUE                    
# 6 R_75JKlGqRwWVZLEZ     13.7        12 TRUE                    
# 7 R_7TB1FD5AA7ivMZj      5.93       12 TRUE                    
# 8 R_7yCcsQ5hA77VpbX     13.7        12 TRUE 

# Data Cleaning: Participant Exclusion (a total of 10 participants)
flagged_ids_iii <- sd_by_participant_iii %>%
  filter(flag_low_variability_iii) %>%
  pull(Response_ID)

data_gesture_clean <- data_gesture %>%
  filter(!Response_ID %in% flagged_ids_iii) %>% droplevels()


str(data_gesture_clean) #142 participants


# Convert to Factor Variables
data_gesture_clean <- data_gesture_clean %>%
  mutate(
    GestureType = factor(GestureType, levels = c("Conflated Gesture", "Manner Gesture", "Path Gesture")),
    EventType = factor(EventType, levels = c( "Conflated Event","Manner Event", "Path Event", "No Motion")),
    Scenario = factor(Scenario, levels = c("Leaf","Car", "Chair","Plank", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative"))
  )

# Check Data Structure
str(data_gesture_clean)
View(data_gesture_clean)


# Visualization: Boxplot of Ratings
ggplot(data_gesture_clean, aes(x = EventType, y = Rating, color = GestureType, fill = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings Across Polarity and Event Types",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

ggplot(data_gesture_clean, aes(x = Polarity, y = Rating, color = GestureType, fill = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  facet_wrap(~EventType) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings Across Polarity and Event Types",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

data_avg <- data_gesture_clean %>%
  group_by(GestureType, EventType, Polarity) %>%
  summarise(
    MeanRating = mean(Rating, na.rm = TRUE),
    SDRating = sd(Rating, na.rm = TRUE),
    Count = n(),
    SE = SDRating / sqrt(Count),  # Standard Error Calculation
    .groups = "drop"
  )

print(data_avg, n = 24)

ggplot(data_avg, aes(x = EventType, y = MeanRating, fill = GestureType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar chart
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE),
                width = 0.2, position = position_dodge(0.9)) +  # Error bars (Standard Error)
  facet_wrap(~ Polarity) +  # Facet by Polarity (Affirmative/Negative)
  theme_minimal(base_size = 14) +
  ylim(0,100) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")) +
  labs(title = "Mean Ratings of Gestures Across Events",
       x = "Polarity",
       y = "Mean Rating",
       fill = "Gesture Type")

# Histogram of Rating
hist(data_gesture_clean$Rating, main = "Histogram of Ratings", xlab = "Rating", col = "lightblue", border = "black")


# What happens if we focus on Motion vs No Motion
data_gesture_clean_No <- data_gesture_clean %>% filter(
  EventType == "No Motion") %>% droplevels()

data_gesture_clean_No <- data_gesture_clean_No %>%
  mutate(
    Response_ID = as.factor(Response_ID),
    GestureType = factor(GestureType, levels = c("Conflated Gesture", "Path Gesture", "Manner Gesture")),
    Scenario = factor(Scenario, levels = c("Leaf", "Car", "Plank", "Chair", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
    Rating  = as.numeric(Rating))

data_gesture_clean_No <- as.data.frame(data_gesture_clean_No) 

plot_data_gesture_clean_No <- ggplot(data = data_gesture_clean_No, aes(x = Polarity, y = Rating, fill = GestureType, color = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    #title = "Ratings for English Sentences Without Gesture",
    subtitle = "Comparison of ratings across polarity and gesture types",
    y = "Rating",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

plot_data_gesture_clean_No

contrasts(data_gesture_clean_No$Polarity) <- contr.sum(levels(data_gesture_clean_No$Polarity))
contrasts(data_gesture_clean_No$GestureType) <- contr.sum(levels(data_gesture_clean_No$GestureType))

# Fit a model
epsilon <- 1e-6
data_gesture_clean_No$Rating_scaled <- (data_gesture_clean_No$Rating / 100) * (1 - 2 * epsilon) + epsilon

model_gesture_clean_No <- glmmTMB(
  Rating_scaled ~  GestureType  * Polarity + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_gesture_clean_No
)

summary(model_gesture_clean_No)


### Data Subsetting----------------------

#### Affirmative Polarity-------------------

data_gesture_Aff <- data_gesture_clean %>%
  filter(
    Polarity == "Affirmative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_gesture_Aff)
str(data_gesture_Aff)

ggplot(data = data_gesture_Aff, aes(x = EventType, y = Rating, color = GestureType, fill = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings in Affirmative Polarity Across Event Types",
    x = " ", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

## What about the data histogram ##
hist(data_gesture_Aff$Rating)

data_gesture_Aff <- data_gesture_Aff %>% mutate(
  GestureType = factor(GestureType, levels = c("Path Gesture", "Manner Gesture", "Conflated Gesture")),
  EventType = factor(EventType, levels = c("Path Event", "Manner Event", "Conflated Event")),
)

str(data_gesture_Aff)

epsilon <- 1e-6
data_gesture_Aff$Rating_scaled <- (data_gesture_Aff$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_gesture_Aff$GestureType) <- contr.sum(3)
contrasts(data_gesture_Aff$EventType) <- contr.sum(3)

model_gesture_Aff <- glmmTMB(
  Rating_scaled ~  GestureType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_gesture_Aff)

summary(model_gesture_Aff)


emm <- emmeans(model_gesture_Aff, ~ GestureType | EventType, type = "response")
pairs(emm)

plot(ggpredict(model_gesture_Aff, terms = "EventType"))

library(DHARMa)
sim <- simulateResiduals(model_gesture_Aff)
plot(sim)

model_disp <- glmmTMB(
  Rating_scaled ~ GestureType * EventType +
    (1 | Response_ID) + (1 | Scenario),
  dispformula = ~ GestureType + EventType,
  family = beta_family(),
  data = data_gesture_Aff
)
anova(model_disp, model_gesture_Aff)
#### Negative Polarity---------------------------

data_gesture_Neg <- data_gesture_clean %>%
  filter(
    Polarity == "Negative",  # keep only affirmatives
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

View(data_gesture_Neg)
str(data_gesture_Neg)

ggplot(data = data_gesture_Neg, aes(x = EventType, y = Rating, color = GestureType, fill = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.25) +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings in Affirmative Polarity Across Event Types",
    x = " ", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

## What about the data histogram ##
hist(data_gesture_Neg$Rating)

data_gesture_Neg <- data_gesture_Neg %>% mutate(
  GestureType = factor(GestureType, levels = c("Path Gesture", "Manner Gesture", "Conflated Gesture")),
  EventType = factor(EventType, levels = c("Path Event", "Manner Event", "Conflated Event")),
)

epsilon <- 1e-6
data_gesture_Neg$Rating_scaled <- (data_gesture_Neg$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_gesture_Neg$GestureType) <- contr.sum(3)
contrasts(data_gesture_Neg$EventType) <- contr.sum(3)

model_gesture_neg <- glmmTMB(
  Rating_scaled ~  GestureType * EventType  + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_gesture_Neg)

summary(model_gesture_neg)

emm <- emmeans(model_gesture_neg, ~ GestureType | EventType, type = "response")
pairs(emm)

pred <- ggpredict(model_gesture_Aff,
                  terms = c("EventType","GestureType"))

### Gesture no distinction under negation

# Set Contrasts for Categorical Variables
contrasts(data_gesture_clean$Polarity) <- contr.sum(levels(data_gesture_clean$Polarity))
contrasts(data_gesture_clean$EventType) <- contr.sum(levels(data_gesture_clean$EventType))
contrasts(data_gesture_clean$GestureType) <- contr.sum(levels(data_gesture_clean$GestureType))

# Prepare Data for Beta Regression
epsilon <- 1e-6
data_gesture_clean$Rating_scaled <- (data_gesture_clean$Rating / 100) * (1 - 2 * epsilon) + epsilon

# Fit Beta Regression Model
model_gesture<- glmmTMB(
  Rating_scaled ~ Polarity * GestureType * EventType + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_gesture_clean
)
summary(model_gesture)

### Preliminary Studies ---------------------------
### No Gesture Data in English ----------------

data_Prelim_NoGesture <- read_csv("Prelim_Gesture_NoGesture.csv")

# Extract relevant columns

data_Prelim_NoGesture <- data_Prelim_NoGesture %>% 
  separate(Trial, into = c("Polarity", "EventType", "Scenario"), sep = " ", remove = FALSE) %>%
  mutate(
  Polarity = case_when(
    grepl("Aff", Trial) ~ "Affirmative", 
    TRUE ~ "Negative"),
  EventType = case_when(
    grepl("M", EventType) ~ "Manner Event",
    grepl("P", EventType) ~ "Path Event",
    grepl("0", EventType) ~ "No Motion",
    TRUE ~ "Conflated Event"),
  Scenario = case_when(
    grepl("D", Scenario) ~ "Plank",
    grepl("E", Scenario) ~"Chair",
    grepl("B", Scenario) ~"Car",
    grepl("A", Scenario) ~ "Leaf", 
    TRUE ~ "Paper"), 
  Rating  = as.numeric(Rating)) 

data_Prelim_NoGesture <- data_Prelim_NoGesture %>%
  mutate(
    Response_ID = as.factor(Response_ID),
    EventType = factor(EventType, levels = c("Conflated Event", "Manner Event","Path Event", "No Motion")),
    Scenario = factor(Scenario, levels = c("Leaf", "Car", "Plank", "Chair", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
    Rating  = as.numeric(Rating))

str(data_Prelim_NoGesture)
View(data_Prelim_NoGesture)

# Plots

plot_Prelim_NoGesture <- ggplot(data = data_Prelim_NoGesture, aes(x = Polarity, y = Rating, color = EventType, fill = EventType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    #title = "Ratings for English Sentences Without Gesture",
    subtitle = "Comparison of ratings across polarity and event types",
    x = "Polarity",
    y = "Rating",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

plot_Prelim_NoGesture

epsilon <- 1e-6
data_Prelim_NoGesture$Rating_scaled <- (data_Prelim_NoGesture$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_Prelim_NoGesture$Polarity) <- contr.sum(levels(data_Prelim_NoGesture$Polarity))
contrasts(data_Prelim_NoGesture$EventType) <- contr.sum(levels(data_Prelim_NoGesture$EventType))

model_Prelim_NoGesture <- glmmTMB(
  Rating_scaled ~ Polarity * EventType + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_Prelim_NoGesture)

summary(model_Prelim_NoGesture)

emm <- emmeans(model_Prelim_NoGesture, ~ EventType | Polarity, type = "response")
pairs(emm)

# No Motion not involved

data_Prelim_NoGesture_Mot <- data_Prelim_NoGesture %>%
  filter(
    EventType %in% c("Path Event", "Manner Event", "Conflated Event")  # keep motion event types only
  ) %>% droplevels()

epsilon <- 1e-6
data_Prelim_NoGesture_Mot$Rating_scaled <- (data_Prelim_NoGesture_Mot$Rating / 100) * (1 - 2 * epsilon) + epsilon

contrasts(data_Prelim_NoGesture_Mot$Polarity) <- contr.sum(levels(data_Prelim_NoGesture_Mot$Polarity))
contrasts(data_Prelim_NoGesture_Mot$EventType) <- contr.sum(levels(data_Prelim_NoGesture_Mot$EventType))

model_Prelim_NoGesture_Mot <- glmmTMB(
  Rating_scaled ~ Polarity * EventType + (1 | Response_ID) + (1 | Scenario),
  family = beta_family(link = "logit"),
  data = data_Prelim_NoGesture_Mot)

summary(model_Prelim_NoGesture_Mot)
### No difference between event types in affirmative cases. 
### Polarity has significant effect as expected.

### No Context Data in English --------------------

data_Prelim_NoContext <- read_csv("Prelim_Gesture_NoEvent.csv")

# View and inspect the daread_csv()# View and inspect the data
head(data_Prelim_NoContext$Trial)
summary(data_Prelim_NoContext)

# Data preparation

data_Prelim_NoContext <- data_Prelim_NoContext %>% 
  separate(Trial, into = c("Polarity", "GestureType", "Scenario"), sep = " ", remove = FALSE) %>%
  mutate(
    Polarity = case_when(
      grepl("Aff", Trial) ~ "Affirmative", 
      TRUE ~ "Negative"),
    GestureType = case_when(
      grepl("M", GestureType) ~ "Manner Gesture",
      grepl("P", GestureType) ~ "Path Gesture",
      grepl("0", GestureType) ~ "No Gesture",
      TRUE ~ "Conflated Gesture"),
    Scenario = case_when(
      grepl("D", Scenario) ~ "Plank",
      grepl("E", Scenario) ~"Chair",
      grepl("B", Scenario) ~"Car",
      grepl("A", Scenario) ~ "Leaf", 
      TRUE ~ "Paper"), 
    Rating  = as.numeric(Rating)) 

data_Prelim_NoContext <- data_Prelim_NoContext %>%
  mutate(
    Response_ID = as.factor(Response_ID),
    GestureType = factor(GestureType, levels = c("Conflated Gesture", "Manner Gesture", "Path Gesture", "No Gesture")),
    Scenario = factor(Scenario, levels = c("Leaf", "Car", "Plank", "Chair", "Paper")),
    Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
    Rating  = as.numeric(Rating))

data_Prelim_NoContext %>% as.data.frame()

# Check the structure
str(data_Prelim_NoContext)
View(data_Prelim_NoContext)

# Plots
plot_Prelim_NoContext <- ggplot(data = data_Prelim_NoContext, aes(x = Polarity, y = Rating, fill = GestureType, color = GestureType)) +
  geom_boxplot(notch = TRUE, alpha = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    #title = "Ratings for English Sentences Without Gesture",
    subtitle = "Comparison of ratings across polarity and gesture types",
    x = "Polarity",
    y = "Rating",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

plot_Prelim_NoContext