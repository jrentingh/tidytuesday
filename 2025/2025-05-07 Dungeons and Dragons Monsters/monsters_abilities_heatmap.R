# client - TidyTuesdays
# project - Dungeons and Dragons Monsters
# task - Create heatmap of average monster ability scores by monster type.
# date - 6/9/2025
# author - Jason Entingh


# load working directory
setwd("~/Jason/R Sandbox/TidyTuesdays/2025-05-07")

# load libraries
library(tidyverse)
library(shiny)


# load dataset
tuesdata <- tidytuesdayR::tt_load('2025-05-27')

# create heatmap of average ability score by monster type
monsters_abilities <- monsters |>
  select(type, str, dex, con, int, wis, cha) |>
  pivot_longer(
    cols = c("str", "dex", "con", "int", "wis", "cha"),
    names_to = "ability",
    values_to = "score"
  ) |>
  group_by(type, ability) |>
  mutate(
    avg_score = round(mean(score), 1),
    n = n()
  ) |>
  select(type, ability, avg_score, n)

monsters_abilities_heatmap <- monsters_abilities |>
  ggplot(aes(x = ability, y = factor(type, levels = sort(unique(type), decreasing = TRUE)), fill = avg_score)) +
  geom_tile() +
  geom_text(aes(label = avg_score), color = "white", size = 2.5) +
  scale_fill_gradient(low = "#c6dbef", high = "#08306b") +
  coord_fixed() +
  labs(
    title = "Average Ability Scores of Monsters in Dungeons and Dragons",
    subtitle = "By Monster Type",
    x = "Ability",
    y = "Monster Type",
    fill = "Average Ability Score"
    )
monsters_abilities_heatmap
ggsave(
  filename = "monsters_abilities_heatmap.png",
  path = "Viz",
  width = 7.6,
  height = 7.29
  )
