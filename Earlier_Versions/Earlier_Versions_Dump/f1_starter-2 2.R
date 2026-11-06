# ============================================================
#  F1 project – starter script
#  Chart: "Where they started vs. where they finished"
#  Race: 2024 São Paulo Grand Prix (round 21), where
#  Verstappen won from 17th on the grid.
# ============================================================

# ---- 1. Packages --------------------------------------------
# install.packages(c("dplyr", "ggplot2", "stringr"))  # run once if needed
library(f1dataR)
library(dplyr)
library(ggplot2)
library(stringr)

# ---- 2. Load the data ---------------------------------------
# load_results(season, round) downloads one race's results
# from the Jolpica API and returns a table (tibble), one row per driver.
results <- load_results(season = 2024, round = 21)

# Always look at what you got before plotting:
names(results)   # the column names
View(results)    # opens the table in RStudio

# ---- 3. Clean it up -----------------------------------------
race <- results %>%
  mutate(
    grid     = as.numeric(grid),       # starting position
    position = as.numeric(position),   # finishing position
    grid     = if_else(grid == 0, 20, grid),  # 0 = started from pit lane
    gained   = grid - position,        # positive = places gained
    # "max_verstappen" -> "Max Verstappen"
    driver   = str_to_title(str_replace_all(driver_id, "_", " ")),
    # order drivers by finishing position for the chart
    driver   = reorder(driver, -position)
  )

# Team colours (2024), written by hand so they read well on white.
# Names must match the values in the constructor_id column.
team_colors <- c(
  red_bull     = "#3671C6", mclaren  = "#FF8000", ferrari = "#E8002D",
  mercedes     = "#00A19C", aston_martin = "#229971", alpine = "#FF87BC",
  williams     = "#1E5BC6", rb       = "#6692FF", sauber  = "#3BB143",
  haas         = "#7A7D80"
)
race$team_color <- team_colors[race$constructor_id]
# If a line comes out grey/missing, run unique(race$constructor_id)
# and check the name is in the list above.

# ---- 4. The chart -------------------------------------------
# Each driver gets an arrow from the grid slot to the finishing spot.
ggplot(race, aes(y = driver)) +
  geom_segment(aes(x = grid, xend = position, yend = driver),
               colour = race$team_color, linewidth = 1.2,
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_point(aes(x = grid), colour = "grey40", size = 2) +
  scale_x_reverse(breaks = 1:20) +   # P1 on the right, like a winner's podium
  theme_minimal(base_size = 12) +    # white background
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),   # fewer lines = less clutter
    plot.title         = element_text(face = "bold"),
    plot.background    = element_rect(fill = "white", colour = NA)
  ) +
  labs(
    title    = "Pole position isn't everything",
    subtitle = "São Paulo GP 2024 | grey dot = start, arrow tip = finish",
    x = "Position",
    y = NULL
  )

# ---- 5. Save it ---------------------------------------------
ggsave("sao_paulo_2024_grid_vs_finish.png", width = 9, height = 7, dpi = 300)
