# ============================================================
#  Q1 – Where on the track does one driver gain time over another?
#  EARLY VERSION (v1) – qualifying telemetry, simple charts
#  Chart 1: speed along the lap, both drivers
#  Chart 2: time gap between them along the lap
#  Chart 3: "track dominance" – who was faster in each mini-sector
#
#  NEEDS the Python/FastF1 setup working (the f1env environment).
#  Run from a fresh R session: Session > Restart R.
# ============================================================

reticulate::use_condaenv("f1env", required = TRUE)
library(f1dataR)
library(dplyr)
library(ggplot2)

dir.create("f1_cache", showWarnings = FALSE)
options(f1dataR.cache = file.path(getwd(), "f1_cache"))

# ---- 1. Choose the session and the two drivers --------------
#  <- change these to compare ANY two drivers at ANY race (2018+)
season <- 2026
race   <- 16             # round 16 = Bahrain GP in Malaysia (Sepang)
                         # check with: load_schedule(2026)
d1     <- "VER"          # 3-letter driver codes
d2     <- "HAM"
col1   <- "#3671C6"      # colour for d1 (Red Bull blue)
col2   <- "#E8002D"      # colour for d2 (Ferrari red)
race_label <- "Bahrain GP in Malaysia (Sepang)"   # used in chart titles

driver_cols <- setNames(c(col1, col2), c(d1, d2))

# ---- 2. Load each driver's fastest qualifying lap -----------
get_lap <- function(driver) {
  load_driver_telemetry(season, race, session = "Q",
                        driver = driver, laps = "fastest") %>%
    mutate(driver = driver)
}

tel <- bind_rows(get_lap(d1), get_lap(d2))
# key columns: distance (m from start of lap), speed (km/h),
#              time (s since start of lap), throttle, brake, n_gear

# ---- 3. Chart 1: speed along the lap ------------------------
ggplot(tel, aes(distance, speed, colour = driver)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = driver_cols) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title    = paste("Speed trace:", d1, "vs", d2),
    subtitle = paste(race_label, season, "| fastest qualifying lap"),
    x = "Distance along the lap (m)", y = "Speed (km/h)", colour = NULL
  )

ggsave("q1_v1_speed_trace.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 4. Chart 2: the gap along the lap ----------------------
# For every point of d1's lap, find how long d2 took to reach the
# same distance. Positive gap = d1 got there first (d1 is ahead).
t1 <- filter(tel, driver == d1)
t2 <- filter(tel, driver == d2)

gap <- tibble(
  distance = t1$distance,
  gap      = approx(t2$distance, t2$time, xout = t1$distance, ties = mean)$y - t1$time
)

ggplot(gap, aes(distance, gap)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 0, y = Inf, label = paste(d1, "ahead"),
           hjust = 0, vjust = 1.5, colour = col1, fontface = "bold") +
  annotate("text", x = 0, y = -Inf, label = paste(d2, "ahead"),
           hjust = 0, vjust = -0.8, colour = col2, fontface = "bold") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title    = paste("Gap between", d1, "and", d2, "along the lap"),
    subtitle = "Line going up = gaining time, going down = losing time",
    x = "Distance along the lap (m)", y = "Gap (seconds)"
  )

ggsave("q1_v1_gap.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 5. Chart 3: track dominance by mini-sector -------------
# Split the lap into equal pieces and see who was faster in each.
n_mini     <- 25
lap_length <- min(max(t1$distance), max(t2$distance))

mini <- tel %>%
  filter(distance <= lap_length) %>%
  mutate(minisector = cut(distance,
                          breaks = seq(0, lap_length, length.out = n_mini + 1),
                          labels = FALSE, include.lowest = TRUE)) %>%
  group_by(minisector, driver) %>%
  summarise(avg_speed = mean(speed), .groups = "drop") %>%
  group_by(minisector) %>%
  slice_max(avg_speed, n = 1, with_ties = FALSE) %>%   # the faster one wins
  ungroup()

count(mini, driver)   # how many mini-sectors each driver won

ggplot(mini, aes(minisector, 1, fill = driver)) +
  geom_tile(colour = "white", linewidth = 1) +
  scale_fill_manual(values = driver_cols) +
  scale_x_continuous(breaks = 1:n_mini) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(
    title    = "Track dominance: who was faster in each mini-sector",
    subtitle = paste(race_label, season, "qualifying | lap split into", n_mini, "equal parts"),
    x = "Mini-sector (start → finish)", fill = "Faster"
  )

ggsave("q1_v1_dominance.png", width = 11, height = 3, dpi = 300, bg = "white")
