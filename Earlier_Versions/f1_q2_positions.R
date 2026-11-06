# ============================================================
#  Q2 – How many positions did each driver gain/lose in a race?
#  EARLY VERSION (v1)
#  Chart 1: lap-by-lap position chart, in the style F1 TV uses
#  Chart 2: positions gained/lost (start vs finish)
#  No Python needed – uses Jolpica data only.
# ============================================================

library(f1dataR)
library(dplyr)
library(ggplot2)
library(stringr)

# ---- 1. Choose the race -------------------------------------
season <- 2024
rnd    <- 21     # São Paulo GP 2024. Use load_schedule(season) to find others.

# ---- 2. Load the data ---------------------------------------
results <- load_results(season, rnd)   # start (grid) + finish per driver
laps    <- load_laps(season, rnd)      # position of every driver on every lap

# ---- 3. Driver info: code, team colour, teammate ------------
team_colors <- c(
  red_bull = "#3671C6", mclaren = "#FF8000", ferrari = "#E8002D",
  mercedes = "#00A19C", aston_martin = "#229971", alpine = "#FF87BC",
  williams = "#1E5BC6", rb = "#6692FF", sauber = "#3BB143", haas = "#7A7D80"
)

info <- results %>%
  mutate(
    grid   = as.numeric(grid),
    finish = as.numeric(position),
    grid   = if_else(grid == 0, n(), grid),     # 0 = pit lane start -> last
    # "max_verstappen" -> "VER", "norris" -> "NOR"
    code   = toupper(str_sub(str_extract(driver_id, "[a-z]+$"), 1, 3)),
    finished = status == "Finished" | str_detect(status, "Lap")
  ) %>%
  group_by(constructor_id) %>%
  # F1 TV style: same colour for both teammates, 2nd one dashed
  mutate(teammate = if_else(row_number() == 1, "first", "second")) %>%
  ungroup()

# ---- 4. Build the lap-by-lap table --------------------------
# load_laps starts at lap 1, so we add "lap 0" = starting grid
start <- info %>% transmute(driver_id, lap = 0, position = grid)

positions <- laps %>%
  transmute(driver_id, lap = as.numeric(lap), position = as.numeric(position)) %>%
  bind_rows(start) %>%
  left_join(select(info, driver_id, code, constructor_id, teammate),
            by = "driver_id")

# label positions: at the start and at each driver's last lap
start_labels <- filter(positions, lap == 0)
end_labels   <- positions %>%
  group_by(driver_id) %>%
  slice_max(lap, n = 1) %>%
  ungroup()

# ---- 5. Chart 1: position chart (F1 TV style) ---------------
ggplot(positions, aes(lap, position, group = driver_id,
                      colour = constructor_id, linetype = teammate)) +
  geom_line(linewidth = 0.8) +
  geom_text(data = start_labels, aes(label = code),
            hjust = 1.3, size = 3, show.legend = FALSE) +
  geom_text(data = end_labels, aes(label = code),
            hjust = -0.3, size = 3, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:20) +              # P1 at the top
  scale_colour_manual(values = team_colors) +
  scale_linetype_manual(values = c(first = "solid", second = "22")) +
  coord_cartesian(clip = "off") +               # let labels sit outside
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.margin      = margin(10, 35, 10, 35)
  ) +
  labs(
    title    = "Race positions, lap by lap",
    subtitle = "São Paulo GP 2024 | lap 0 = starting grid | dashed = second driver of each team",
    x = "Lap", y = "Position"
  )

ggsave("q2_v1_position_chart.png", width = 12, height = 7, dpi = 300, bg = "white")

# ---- 6. Chart 2: positions gained / lost --------------------
gained <- info %>%
  mutate(
    gained = grid - finish,                 # positive = gained places
    code   = reorder(code, gained)
  )

ggplot(gained, aes(gained, code, fill = gained > 0, alpha = finished)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey30") +
  scale_fill_manual(values = c(`TRUE` = "#2E8B57", `FALSE` = "#C0392B"),
                    guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35),
                     labels = c(`TRUE` = "Finished", `FALSE` = "Did not finish"),
                     name = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(
    title    = "Positions gained and lost",
    subtitle = "São Paulo GP 2024 | starting grid vs. final result",
    x = "Positions gained (+) / lost (−)", y = NULL
  )

ggsave("q2_v1_gained_lost.png", width = 8, height = 7, dpi = 300, bg = "white")
