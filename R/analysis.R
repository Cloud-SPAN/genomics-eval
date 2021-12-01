library(tidyverse)
library(googlesheets4)


# gs4_auth() run interactively
file <- "https://docs.google.com/spreadsheets/d/1303mVzZ2tduE9q3nRGzc0DRKldC3tuPqTNGekl_Lld0/edit?resourcekey#gid=2032352829"
precourse <- read_sheet(file)

topics <- precourse %>%
  select(starts_with("Please rate your level of comfort with, or understanding of, the following")) %>%
  data.frame(.,check.names = FALSE)
names(topics) <- names(topics) %>%
  sub(pattern = "Please rate your level of comfort with, or understanding of, the following \\[*",
      replacement = "") %>%
  sub(pattern = "\\]", replacement = "")
my_levels <- c("I don't understand what this involves",
               "I would recognise this but don't really use it",
               "I've used a bit but am not comfortable with it",
               "Fairly comfortable in some aspects",
               "Comfortable most of the time",
               "Confident")
project_org <- topics %>%
  select(1:3) %>%
  pivot_longer(names_to = "Topic",
               values_to = "Rating",
               cols = everything())
project_org$Rating <- factor(project_org$Rating, levels = my_levels)


command_line <- topics %>%
  select(4:10) %>%
  pivot_longer(names_to = "Topic",
               values_to = "Rating",
               cols = everything())
command_line$Rating <- factor(command_line$Rating, levels = my_levels)


genomics <- topics %>%
  select(11:13) %>%
  pivot_longer(names_to = "Topic",
               values_to = "Rating",
               cols = everything())
genomics$Rating <- factor(genomics$Rating, levels = my_levels)


project_org_plot <- project_org %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating)) +
  geom_bar(fill = "#9d5e78") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(name = "Number of people",
                     expand = c(0, 0),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
                     limits = c(0, 37)) +
  facet_wrap(.~ Topic, nrow = 5,  strip.position = "top",
             labeller = label_wrap_gen(width = 100)) +
  coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 12, lineheight = 1,
                                    margin = margin(b = 0)),
        strip.background = element_rect(fill = "#f6fafd", colour = "#f6fafd"),
        plot.background = element_rect(fill = "#f6fafd"),
        panel.background = element_rect(fill = "#f6fafd"))
ggsave("figures/project-org.png",
       plot = project_org_plot,
       width = 8,
       height = 5)



command_line_plot <- command_line %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating)) +
  geom_bar(fill = "#9d5e78") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(name = "Number of people",
                     expand = c(0, 0),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
                     limits = c(0, 37)) +
  facet_wrap(.~ Topic, nrow = 4,  strip.position = "top",
             labeller = label_wrap_gen(width = 60)) +
  coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 12, lineheight = 1,
                                    margin = margin(b = 0)),
        strip.background = element_rect(fill = "#f6fafd", colour = "#f6fafd"),
        plot.background = element_rect(fill = "#f6fafd"),
        panel.background = element_rect(fill = "#f6fafd"))

ggsave("figures/command-line.png",
       plot = command_line_plot,
       width = 14,
       height = 6)



genomics_plot <- genomics %>%
  filter(!is.na(Rating)) %>%
  ggplot(aes(x = Rating)) +
  geom_bar(fill = "#9d5e78") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(name = "Number of people",
                     expand = c(0, 0),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
                     limits = c(0, 37)) +
  facet_wrap(.~ Topic, nrow = 5,  strip.position = "top",
             labeller = label_wrap_gen(width = 100)) +
  coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 12, lineheight = 1,
                                    margin = margin(b = 0)),
        strip.background = element_rect(fill = "#f6fafd", colour = "#f6fafd"),
        plot.background = element_rect(fill = "#f6fafd"),
        panel.background = element_rect(fill = "#f6fafd"))

ggsave("figures/genomic.png",
       plot = genomics_plot,
       width = 8,
       height = 5)
