library(tidyverse)
library(googlesheets4)

pre_sheet <- "https://docs.google.com/spreadsheets/d/1R16PlsMiakKfRtoDW5f5tvpKH7R7vefR5WkGrqJlvk0/edit#gid=0"
pre_survey <- read_sheet(pre_sheet)

# Numeric encoding of levels
# 1 = I don't understand what this involves
# 2 = I would recognise this but don't really use it
# 3 = I've used a bit but am not comfortable with it
# 4 = Fairly comfortable in some aspects
# 5 = Comfortable most of the time
# 6 = Confident

names(pre_survey) <- names(pre_survey) %>%
  sub(pattern = "Please rate your level of comfort with, or understanding of, the following",
      replacement = "") %>%
  str_replace_all(pattern = "\\[|\\]", replacement = "") %>%
  trimws()

pre_survey <- pre_survey %>%
  rename("metadata" = "The existence or role of metadata in a sequencing project",
         "sequence_data_org_format" = "The organisation and format of sequence data (any)",
         "file_system_org" = "Organising a file system for a bioinformatics project",
         "cloud_comp" = "Cloud computing",
         "cloud_server_connection" = "Connecting to a cloud server such as AWS, Microsoft Azure or Google Cloud Platform",
         "basic_shell_commands" = "Using  a shell and commands such as ls, pwd, mkdir, cat",
         "file_system" = "Working directories, paths and avigating a file system from the command line",
         "file_permissions" = "Viewing and changing file permissions in the shell",
         "grep" = "The search tool grep",
         "nano" = "Using a text editor like nano to write shell scripts",
         "read_quality" = "Assessing read quality of sequence data",
         "trimming_filtering" = "Trimming and filtering sequence data",
         "variant_calling" = "Variant calling")

understanding_levels <- c("I don't understand what this involves",
               "I would recognise this but don't really use it",
               "I've used a bit but am not comfortable with it",
               "Fairly comfortable in some aspects",
               "Comfortable most of the time",
               "Confident")
pre_survey_long <- pre_survey %>%
  select(-Name) %>%
  pivot_longer(!ID, names_to = "topic", values_to = "rating")
pre_survey_long$rating <- factor(pre_survey_long$rating, levels = understanding_levels)

pre_survey_long$rating_numeric <- pre_survey_long$rating %>%
  str_replace_all("I don't understand what this involves", "1") %>%
  str_replace_all("I would recognise this but don't really use it", "2") %>%
  str_replace_all("I've used a bit but am not comfortable with it", "3") %>%
  str_replace_all("Fairly comfortable in some aspects", "4") %>%
  str_replace_all("Comfortable most of the time", "5") %>%
  str_replace_all("Confident", "6")
pre_survey_long$rating_numeric <- as.numeric(pre_survey_long$rating_numeric)

pre_survey_long_clean <- na.omit(pre_survey_long)

pre_survey_long_clean <- pre_survey_long_clean %>% 
  mutate(theme = case_when(
  topic == "metadata" ~ "project-org",
  topic == "file_system_org" ~ "project-org",
  topic == "sequence_data_org_format" ~ "project-org",
  topic == "cloud_comp" ~ "command-line",
  topic == "grep" ~ "command-line",
  topic == "nano" ~ "command-line",
  topic == "file_system" ~ "command-line",
  topic == "file_permissions" ~ "command-line",
  topic == "cloud_server_connection" ~ "command-line",
  topic == "basic_shell_commands" ~ "command-line",
  topic == "read_quality" ~ "genomics",
  topic == "trimming_filtering" ~ "genomics",
  topic == "variant_calling" ~ "genomics"
))

pre_survey_long_clean$topic <- factor(pre_survey_long_clean$topic, levels = pre_survey_long_clean[1:13,]$topic)

#get summary of mean rating for each topic
pre_survey_means <- pre_survey_long_clean %>% 
  group_by(topic, theme) %>% 
  summarise(
    rating_numeric = mean(rating_numeric))

#get summary of how many people chose each rating for each topic
topic_summary <- pre_survey_long_clean %>%
  group_by(topic) %>%
  count(rating)

# Plots -------------------------------------------------------------------

#bar chart with jittered points
ggplot(pre_survey_means, aes(x = topic, y = rating_numeric)) +
  geom_col() +
  ylim(0,6) +
  geom_jitter(data = pre_survey_long_clean, aes(x = topic, y = rating_numeric, col = topic)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#boxplot with jittered points
ggplot(pre_survey_long_clean, aes(x = topic, y = rating_numeric, fill = theme)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_jitter(alpha = 0.4)

#bar charts showing distribution of ratings per topic
ggplot(data = topic_summary, aes(x = rating, y = n)) +
  geom_col() +
  facet_wrap(vars(topic))
# Post-survey -------------------------------------------------------------

post_sheet <- "https://docs.google.com/spreadsheets/d/1dPBomyBW0K0njBK-ciGuk-IUvyrnDDX5a4kRG4rJtZ0/edit#gid=0"
post_survey <- read_sheet(post_sheet) %>%
  select(c(1:14))

names(post_survey) <- names(post_survey) %>%
  sub(pattern = "Please rate your level of comfort with, or understanding of, the following",
      replacement = "") %>%
  str_replace_all(pattern = "\\[|\\]", replacement = "") %>%
  trimws()

post_survey <- post_survey %>%
  rename("metadata" = "The existence or role of metadata in a sequencing project",
         "sequence_data_org_format" = "The organisation and format of sequence data (any)",
         "file_system_org" = "Organising a file system for a bioinformatics project",
         "cloud_comp" = "Cloud computing",
         "cloud_server_connection" = "Connecting to a cloud server such as AWS, Microsoft Azure or Google Cloud Platform",
         "basic_shell_commands" = "Using  a shell and commands such as ls, pwd, mkdir, cat",
         "file_system" = "Working directories, paths and avigating a file system from the command line",
         "file_permissions" = "Viewing and changing file permissions in the shell",
         "grep" = "The search tool grep",
         "nano" = "Using a text editor like nano to write shell scripts",
         "read_quality" = "Assessing read quality of sequence data",
         "trimming_filtering" = "Trimming and filtering sequence data",
         "variant_calling" = "Variant calling")

post_survey_long <- post_survey %>%
  pivot_longer(!Respondent, names_to = "topic", values_to = "rating")
post_survey_long$rating <- factor(post_survey_long$rating, levels = understanding_levels)

post_survey_long$rating_numeric <- post_survey_long$rating %>%
  str_replace_all("I don't understand what this involves", "1") %>%
  str_replace_all("I would recognise this but don't really use it", "2") %>%
  str_replace_all("I've used a bit but am not comfortable with it", "3") %>%
  str_replace_all("Fairly comfortable in some aspects", "4") %>%
  str_replace_all("Comfortable most of the time", "5") %>%
  str_replace_all("Confident", "6")
post_survey_long$rating_numeric <- as.numeric(post_survey_long$rating_numeric)

post_survey_long_clean <- na.omit(post_survey_long)

post_survey_long_clean <- post_survey_long_clean %>% 
  mutate(theme = case_when(
    topic == "metadata" ~ "project-org",
    topic == "file_system_org" ~ "project-org",
    topic == "sequence_data_org_format" ~ "project-org",
    topic == "cloud_comp" ~ "command-line",
    topic == "grep" ~ "command-line",
    topic == "nano" ~ "command-line",
    topic == "file_system" ~ "command-line",
    topic == "file_permissions" ~ "command-line",
    topic == "cloud_server_connection" ~ "command-line",
    topic == "basic_shell_commands" ~ "command-line",
    topic == "read_quality" ~ "genomics",
    topic == "trimming_filtering" ~ "genomics",
    topic == "variant_calling" ~ "genomics"
  ))

post_survey_long_clean$topic <- factor(post_survey_long_clean$topic, levels = post_survey_long_clean[1:13,]$topic)

post_survey_means <- post_survey_long_clean %>% 
  group_by(topic, theme) %>% 
  summarise(
    rating_numeric = mean(rating_numeric))


combined_survey_means <- merge(pre_survey_means, post_survey_means, by = c("topic", "theme")) %>%
  rename("pre" = "rating_numeric.x",
         "post" = "rating_numeric.y")

# Comparison plots --------------------------------------------------------

ggplot(pre_survey_means, aes(x = topic, y = rating_numeric)) +
  geom_point(colour = "red") +
  geom_point(data = post_survey_means, aes(x = topic, y = rating_numeric), colour = "blue") +
  ylim(0,6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

library(ggalt)

ggplot(combined_survey_means, aes(x = pre, xend = post, y = fct_rev(topic), colour = theme)) +
  geom_dumbbell(
    #colour = "#a3c4dc",
    #colour_xend = "#0e668b",
    size = 4,
    dot_guide = T,
    dot_guide_size = 0.15,
    dot_guide_colour = "grey60"
  ) +
  xlim(1,6) +
  theme_minimal() +
  xlab("Score") +
  ylab("Topic")

ggplot(combined_survey_means, aes(x = pre, xend = post, y = fct_rev(topic))) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 4,
    dot_guide = T,
    dot_guide_size = 0.15,
    dot_guide_colour = "grey60"
  ) +
  xlim(1,6) +
  theme_minimal() +
  xlab("Score") +
  ylab("Topic")

