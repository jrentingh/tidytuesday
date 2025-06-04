# client - TidyTuesdays
# project - Dungeons and Dragons Monsters
# task - Exploratory data analysis of Dungeons and Dragons Monsters dataset.
# date - 6/3/2025
# author - Jason Entingh


# load working directory
setwd("~/Jason/R Sandbox/TidyTuesdays/2025-05-07")

# load libraries
library(tidyverse)
library(shiny)


# load dataset
tuesdata <- tidytuesdayR::tt_load('2025-05-27')

# assign dataset to variable
monsters <- tuesdata$monsters

### explore

# transform data
# assign values to size
monsters_size_vector <- c("Tiny", "Small", "Medium or Small", "Medium", "Large", "Huge", "Gargantuan")
monsters$size_nm <- as.numeric(factor(monsters$size, levels = monsters_size_vector))

monsters <- monsters |>
  relocate(size_nm, .after = size)


# explore relation between ac and cr
plot_ac_cr <- monsters |>
  ggplot(aes(ac, cr)) +
  geom_point() + 
  geom_smooth()
  
plot_ac_cr


# explore relation between size and cr
plot_size_cr <- monsters |>
  ggplot(aes(size_nm, cr)) +
  geom_point() + 
  geom_smooth()
plot_size_cr


# explore relation between hp and cr
plot_hp_cr <- monsters |>
  ggplot(aes(hp_number, cr)) +
  geom_point() +
  geom_smooth() +
  labs(title="As HP increases, so does CR", 
       subtitle="Data from 2024 Dungeons and Dragons Monsters List", 
       x="Hit Points (HP)",
       y="Challenge Rating (CR)")
plot_hp_cr
ggsave("scatter_hp_cr.png", path = "Viz")


# explore relation between number of resistances and immunities to cr
monsters$resistances_nm <- str_count(monsters$resistances, ",")+1
monsters <- monsters |>
  mutate(resistances_nm = replace_na(resistances_nm, 0)) |>
  relocate(resistances_nm, .after = resistances)

monsters$immunities_nm <- str_count(monsters$immunities,",")+1
monsters <- monsters |>
  mutate(immunities_nm = replace_na(immunities_nm,0)) |>
  relocate(immunities_nm, .after = immunities)

monsters <- monsters |>
  mutate(resimm = resistances_nm + immunities_nm) |>
  relocate(resimm, .after = immunities_nm)

plot_resimm_cr <- monsters |>
  ggplot(aes(resimm, cr)) +
  geom_point() +
  geom_smooth()
plot_resimm_cr


# explore relation between size and type
size_factor <- factor(monsters$size_nm)


plot_size_type <- monsters |>
  ggplot(aes(x=type, fill=size)) +
  geom_bar(position="fill") +
  scale_fill_brewer()
plot_size_type 


# explore relation between speed and type
avg_speed <- monsters |>
  group_by(type) |>
  summarize(mean_speed = mean(speed_base_number)) |>
  arrange(desc(mean_speed))

plot_speed_type <- avg_speed |>
  ggplot(aes(x=mean_speed, y= reorder(type, mean_speed))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(mean_speed, 1)),
            hjust = -0.5,
            size = 3.5) +
  labs(title = "Average Speed by Monster Type",
       x = "Average Speed",
       y= "Monster Type",
       subtitle="Data from 2024 Dungeons and Dragons Monsters List")
plot_speed_type
ggsave("col_speed_type.png", path = "Viz")


# explore relation between skill proficiencies and type

monsters_skills <- monsters |>
  separate_rows(skills, sep = ",\\s+") |>
  mutate(
    skill_name = str_extract(skills, "^[A-Za-z]+"),
    skill_modifier = as.numeric(str_extract(skills, "[+-]\\d+"))) |>
  mutate(
    skill_abbr = tolower(str_replace_all(skill_name, " ", "_")),
    skill_dummy = 1) |>
  pivot_wider(
    id_cols = c(name, type),
    names_from = skill_abbr,
    values_from = c(skill_dummy, skill_modifier),
    names_glue = "skill_{skill_abbr}{.value}",
    values_fill = list(skill_dummy = 0, skill_modifier = NA))

monsters_skills_collapsed <- monsters_skills |>
  group_by(type) |>
  summarize(across(starts_with("skill_"), mean, na.rm = TRUE))

plot_skills_type <- monsters_skills_collapsed |>
  ggplot(aes(x=skill_historyskill_dummy, y= reorder(type, skill_historyskill_dummy))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(skill_historyskill_dummy, 0.001)),
            hjust = -0.5,
            size = 3.5) +
  labs(title = "Average History Skill by Monster Type",
       x = "Average History Skill",
       y= "Monster Type",
       subtitle="Data from 2024 Dungeons and Dragons Monsters List")
plot_skills_type





#rshiny


monsters_skills <- monsters |>
  separate_rows(skills, sep = ",\\s+") |>
  mutate(
    skill_name = str_extract(skills, "^[A-Za-z]+"),
    skill_modifier = as.numeric(str_extract(skills, "[+-]\\d+")))

monsters_skills_collapsed <- monsters_skills |>
  group_by(type,skill_name) |>
  summarize(mean_modifier = mean(skill_modifier, na.rm = FALSE),
            .groups = "drop") |>
  mutate(skill_name = factor(skill_name, levels = sort(unique(skill_name))))


max_modifier <- max(monsters_skills_collapsed$mean_modifier, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Monster Skills by Type"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Choose Monster Type:",
                  choices = sort(unique(monsters_skills_collapsed$type)))
    ),
    mainPanel(
      plotOutput("skillPlot")
    )
  )
)

# --- Server ---
server <- function(input, output) {
  output$skillPlot <- renderPlot({
    # Filter data for selected monster type
    filtered_data <- monsters_skills_collapsed %>%
      filter(type == input$type) |>
      mutate(skill_name = factor(skill_name, levels = sort(unique(skill_name))))
    
    # Create plot
    ggplot(filtered_data, aes(x = mean_modifier, y = skill_name)) +
      geom_col(fill = "darkorange") +
      xlim(0, max_modifier) +
      theme_minimal() +
      labs(
        title = paste("Mean Skill Modifiers for", input$type),
        x = "Mean Modifier",
        y = "Skill"
      )
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
