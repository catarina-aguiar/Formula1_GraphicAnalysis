# ============================================================
#  Q1 (simple version) – Sector times: Verstappen vs Hamilton
#  2026 Bahrain GP in Malaysia (Sepang), qualifying, fastest laps
#
#  DATA: OpenF1 API (https://openf1.org), "laps" table.
#  Sector times are OFFICIAL F1 timing, so no GPS alignment needed.
#
#  Chart 1: each driver's time in sector 1, 2 and 3
#  Chart 2: who was faster in each sector, and by how much
# ============================================================

# install.packages(c("jsonlite", "lubridate", "tidyr"))   # run once
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

base <- "https://api.openf1.org/v1/"

# ---- 1. Settings --------------------------------------------
#  OpenF1 uses car NUMBERS: 2026 VER = 3, HAM = 44
drivers <- tibble(
  driver = c("VER", "HAM"),
  number = c(3, 44)
)
driver_cols <- c(VER = "#3671C6", HAM = "#E8002D")
quali_day   <- "2026-10-03"
race_label  <- "Bahrain GP in Malaysia (Sepang) 2026 – qualifying"

# ---- 2. Find the qualifying session -------------------------
sessions <- fromJSON(paste0(base, "sessions?year=2026&session_name=Qualifying"))
session  <- sessions %>% filter(as.Date(ymd_hms(date_start)) == as.Date(quali_day))
session %>% select(session_key, country_name, circuit_short_name)   # check: Malaysia
sk <- session$session_key[1]

# ---- 3. Each driver's fastest lap ---------------------------
get_best_lap <- function(driver, number) {
  laps <- fromJSON(paste0(base, "laps?session_key=", sk, "&driver_number=", number))
  laps %>%
    filter(!is.na(lap_duration)) %>%
    slice_min(lap_duration, n = 1, with_ties = FALSE) %>%
    transmute(driver   = driver,
              lap      = lap_duration,
              Sector_1 = duration_sector_1,
              Sector_2 = duration_sector_2,
              Sector_3 = duration_sector_3)
}

best <- bind_rows(get_best_lap("VER", 3), get_best_lap("HAM", 44))
best
# CHECK: VER lap ~95.130, HAM ~95.428, and S1 + S2 + S3 ≈ lap time

write.csv(best, "q1_sepang2026_quali_sectors.csv", row.names = FALSE)

# Long format: one row per driver per sector
sectors <- best %>%
  pivot_longer(starts_with("Sector"), names_to = "sector", values_to = "time") %>%
  mutate(sector = sub("_", " ", sector))

# ---- 4. Chart 1: sector times -------------------------------
# Each sector gets its own panel with its own scale, so the small
# differences are visible. Dots (not bars), because bars would need
# to start at 0 s and the differences would disappear.
ggplot(sectors, aes(x = time, y = driver, colour = driver)) +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%.3f s", time)),
            vjust = -1.3, size = 3.8, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ sector, ncol = 1, scales = "free_x") +
  scale_colour_manual(values = driver_cols) +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  scale_x_continuous(expand = expansion(mult = 0.6)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text       = element_text(face = "bold", size = 12, hjust = 0),
    plot.title       = element_text(face = "bold")
  ) +
  labs(
    title    = "Sector times: VER vs HAM",
    subtitle = race_label,
    x = "Sector time (seconds)", y = NULL
  )

ggsave("q1_sectors_times.png", width = 8, height = 6, dpi = 300, bg = "white")

# ---- 5. Chart 2: who was faster in each sector --------------
# delta = HAM time - VER time
#   positive -> VER faster (bar goes right, blue)
#   negative -> HAM faster (bar goes left, red)
delta <- sectors %>%
  select(driver, sector, time) %>%
  pivot_wider(names_from = driver, values_from = time) %>%
  mutate(
    delta  = HAM - VER,
    faster = if_else(delta > 0, "VER", "HAM"),
    label  = sprintf("%s  -%.3f s", faster, abs(delta))
  )

delta                        # the numbers
sum(delta$delta)             # CHECK: ≈ 0.298 (the lap time gap)

lim <- max(abs(delta$delta)) * 1.6     # symmetric axis, room for labels

ggplot(delta, aes(x = delta, y = sector, fill = faster)) +
  geom_col(width = 0.55) +
  geom_vline(xintercept = 0, colour = "grey30") +
  geom_text(aes(label = label, colour = faster,
                hjust = if_else(delta > 0, -0.1, 1.1)),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = driver_cols) +
  scale_colour_manual(values = driver_cols) +
  scale_y_discrete(limits = rev) +       # Sector 1 at the top
  scale_x_continuous(limits = c(-lim, lim),
                     labels = function(x) sprintf("%.2f", abs(x))) +
  annotate("text", x = -lim, y = 3.6, label = "← HAM faster",
           hjust = 0, colour = driver_cols[["HAM"]], fontface = "bold") +
  annotate("text", x = lim, y = 3.6, label = "VER faster →",
           hjust = 1, colour = driver_cols[["VER"]], fontface = "bold") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.margin      = margin(20, 15, 10, 10)
  ) +
  labs(
    title    = "Where VER found his 0.298 s",
    subtitle = paste0(race_label, "\nTime gained by the faster driver in each sector"),
    x = "Time difference (seconds)", y = NULL
  )

ggsave("q1_sectors_delta.png", width = 9, height = 4.5, dpi = 300, bg = "white")
