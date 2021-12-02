library(tidyverse)
library(googlesheets4)



# Basic enrolment data ----------------------------------------------------

details_sheet <- "https://docs.google.com/spreadsheets/d/1DoHYKzDsDQdcGvnaW2c1-5q9GF0Qy-NQP3M8QLezMvI/edit#gid=0"
details <- read_sheet(details_sheet)

details <- details %>%
  rename("employer" = "Name of your institution or company where you work",
         "career_stage" = "Which best describes your career stage",
         "role" = "Which best describes your role. Please tick all that apply.",
         "os" = "What operating system do you have?",
         "admin_rights" = "Do you have administrator rights on your machine?")

details$employer <- as.factor(details$employer)
details$career_stage <- as.factor(details$career_stage)
details$role <- as.factor(details$role)
details$os <- as.factor(details$os)
details$admin_rights <- as.factor(details$admin_rights)

# employer bar chart
ggplot(count(details, vars = employer), aes(x=vars, y=n)) +
  geom_col()

# career stage bar chart
ggplot(count(details, vars = career_stage), aes(x=vars, y=n)) +
  geom_col()

# role bar chart
ggplot(count(details %>% separate_rows(role, sep = ", "), vars = role), aes(x=vars, y=n)) +
  geom_col()

# OS bar chart
ggplot(count(details, vars = os), aes(x=vars, y=n)) +
  geom_col()

# admin rights bar chart
ggplot(count(details, vars = admin_rights), aes(x=vars, y=n)) +
  geom_col()

# Diversity data ----------------------------------------------------------

div_sheet <- "https://docs.google.com/spreadsheets/d/1ayvKzyEdinIEAW0mWDhCmCl6sbbNYb1fX666Y7-zJCg/edit#gid=0"
div_results <- read_sheet(div_sheet)

div_results <- div_results %>%
  rename("gender" = "What is your gender?",
         "SAAB" = "Is the gender you identify with the same as your sex registered at birth?",
         "sexuality" = "Which of the following best describes your sexual orientation?",
         "age" = "What is your age?",
         "ethnicity" = "What is your ethnicity?",
         "disability" = "Do you consider yourself to have a disability?",
         "nationality" = "What is your legal nationality?"
         )

#convert all free text answers (nationality only) to lower case
div_results$nationality <- str_to_lower(div_results$nationality)

#replace NA (unanswered) with "Prefer not to respond"
div_results[is.na(div_results)] <- "Prefer not to respond"
div_results["disability"][div_results["disability"] == "Prefer not to say"] <- "Prefer not to respond"

div_results$gender <- as.factor(div_results$gender)
div_results$SAAB <- as.factor(div_results$SAAB)
div_results$sexuality <- as.factor(div_results$sexuality)
div_results$age <- as.factor(div_results$age)
div_results$ethnicity <- as.factor(div_results$ethnicity)
div_results$disability <- as.factor(div_results$disability)
div_results$nationality <- as.factor(div_results$nationality)

ggplot(count(div_results, vars = gender), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = SAAB), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = sexuality), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = age), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = ethnicity), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = disability), aes(x=vars, y=n)) +
  geom_col()

ggplot(count(div_results, vars = nationality), aes(x=vars, y=n)) +
  geom_col()
