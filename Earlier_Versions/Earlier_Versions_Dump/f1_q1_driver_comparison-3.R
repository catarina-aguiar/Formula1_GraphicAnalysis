# ============================================================
#  Q1 – Where on the track does one driver gain time over another?
#  EARLY VERSION (v1) – qualifying, Verstappen vs Hamilton
#  2026 Bahrain GP in Malaysia (Sepang)
#
#  DATA: OpenF1 API (https://openf1.org) – free, no Python needed.
#  (FastF1 can't load 2026 data: F1 put its timing feed behind a login.)
#
#  Chart 1: speed along the lap, both drivers
#  Chart 2: time gap between them along the lap
#  Chart 3: track dominance – who was faster in each mini-sector
#  Chart 4: the same track dominance, drawn on the circuit map
# ============================================================

# install.packages(c("jsonlite", "lubridate"))   # run once if needed
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)

base <- "https://api.openf1.org/v1/"

# Small helper: download one OpenF1 table, stop with a clear message if empty
get_openf1 <- function(query) {
  out <- fromJSON(paste0(base, query))
  if (length(out) == 0 || NROW(out) == 0) {
    stop("OpenF1 returned no data for: ", query)
  }
  out
}

# ---- 1. Choose the drivers ----------------------------------
#  OpenF1 uses car NUMBERS, not codes. 2026: VER = 3, HAM = 44
d1 <- "VER"; d1_num <- 3;  col1 <- "#3671C6"   # Red Bull blue
d2 <- "HAM"; d2_num <- 44; col2 <- "#E8002D"   # Ferrari red
quali_day  <- "2026-10-03"                     # date of qualifying
race_label <- "Bahrain GP in Malaysia (Sepang) 2026"

driver_cols <- setNames(c(col1, col2), c(d1, d2))

# ---- 2. Find the qualifying session -------------------------
sessions <- get_openf1("sessions?year=2026&session_name=Qualifying")
session  <- sessions %>% filter(as.Date(ymd_hms(date_start)) == as.Date(quali_day))
session %>% select(session_key, country_name, circuit_short_name, date_start)  # check!
sk <- session$session_key[1]

# ---- 3. Get each driver's fastest lap -----------------------
# laps -> find the fastest lap and when it started
# car_data -> speed every ~0.27 s; keep only the readings inside that lap
get_fastest_lap <- function(drv_num, code) {
  laps <- get_openf1(paste0("laps?session_key=", sk, "&driver_number=", drv_num))
  best <- laps %>% filter(!is.na(lap_duration)) %>% slice_min(lap_duration, n = 1)

  start <- ymd_hms(best$date_start)
  end   <- start + best$lap_duration

  car <- get_openf1(paste0("car_data?session_key=", sk, "&driver_number=", drv_num))

  car %>%
    mutate(date = ymd_hms(date)) %>%
    filter(date >= start, date <= end) %>%
    arrange(date) %>%
    mutate(
      time     = as.numeric(date - start, units = "secs"),   # s since lap start
      dt       = c(0, diff(time)),
      distance = cumsum(speed / 3.6 * dt),                   # m: speed x time
      rel_dist = distance / max(distance),                   # 0 = start, 1 = finish
      driver   = code,
      lap_start = start,
      lap_time = best$lap_duration,
      s1 = best$duration_sector_1, s2 = best$duration_sector_2,
      s3 = best$duration_sector_3
    )
}

tel <- bind_rows(get_fastest_lap(d1_num, d1), get_fastest_lap(d2_num, d2))

# Put both laps on the same distance scale (lap length = average of both)
lap_length <- tel %>% group_by(driver) %>% summarise(m = max(distance)) %>%
  pull(m) %>% mean()
tel <- tel %>% mutate(dist_m = rel_dist * lap_length)

# Official lap and sector times, to check against the charts
tel %>% distinct(driver, lap_time, s1, s2, s3)

# Save the data you used (you must submit dataset files!)
write.csv(tel, "q1_sepang2026_quali_VER_HAM_telemetry.csv", row.names = FALSE)

# ---- 4. Chart 1: speed along the lap ------------------------
ggplot(tel, aes(dist_m, speed, colour = driver)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = driver_cols) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title    = paste("Speed trace:", d1, "vs", d2),
    subtitle = paste(race_label, "| fastest qualifying lap"),
    x = "Distance along the lap (m)", y = "Speed (km/h)", colour = NULL
  )

ggsave("q1_v1_speed_trace.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 5. Chart 2: the gap along the lap ----------------------
# For every point of d1's lap, how long did d2 take to reach the same point?
# Positive gap = d1 got there first (d1 ahead).
t1 <- filter(tel, driver == d1)
t2 <- filter(tel, driver == d2)

gap <- tibble(
  dist_m = t1$dist_m,
  gap    = approx(t2$rel_dist, t2$time, xout = t1$rel_dist, ties = mean)$y - t1$time
)

ggplot(gap, aes(dist_m, gap)) +
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
    subtitle = "Line going up = VER gaining time, going down = HAM gaining time",
    x = "Distance along the lap (m)", y = "Gap (seconds)"
  )

ggsave("q1_v1_gap.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 6. Chart 3: track dominance by mini-sector -------------
n_mini <- 25

mini <- tel %>%
  mutate(minisector = cut(rel_dist, breaks = seq(0, 1, length.out = n_mini + 1),
                          labels = FALSE, include.lowest = TRUE)) %>%
  group_by(minisector, driver) %>%
  summarise(avg_speed = mean(speed), .groups = "drop") %>%
  group_by(minisector) %>%
  slice_max(avg_speed, n = 1, with_ties = FALSE) %>%   # faster one wins
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
    subtitle = paste(race_label, "qualifying | lap split into", n_mini, "equal parts"),
    x = "Mini-sector (start → finish)", fill = "Faster"
  )

ggsave("q1_v1_dominance.png", width = 11, height = 3, dpi = 300, bg = "white")

# ---- 7. Chart 4: track dominance ON THE TRACK MAP -----------
# OpenF1 "location" = x / y position of the car on track (~4 per second).
# We take d1's positions during the fastest lap to draw the circuit,
# give every point its place in the lap (0 -> 1), find its mini-sector,
# and colour it by whoever was faster there.
lap_start_d1 <- first(t1$lap_start)
lap_time_d1  <- first(t1$lap_time)

loc <- get_openf1(paste0("location?session_key=", sk, "&driver_number=", d1_num)) %>%
  mutate(date = ymd_hms(date)) %>%
  filter(date >= lap_start_d1, date <= lap_start_d1 + lap_time_d1) %>%
  arrange(date) %>%
  mutate(
    time     = as.numeric(date - lap_start_d1, units = "secs"),
    # where in the lap is this point? (match by time to d1's telemetry)
    rel_dist = approx(t1$time, t1$rel_dist, xout = time, rule = 2, ties = mean)$y,
    minisector = cut(rel_dist, breaks = seq(0, 1, length.out = n_mini + 1),
                     labels = FALSE, include.lowest = TRUE)
  ) %>%
  left_join(select(mini, minisector, faster = driver), by = "minisector")

# Save the track data too (for the submission)
write.csv(loc, "q1_sepang2026_quali_VER_track_xy.csv", row.names = FALSE)

# Numbers for the subtitle
wins     <- count(mini, driver)
n_d1     <- sum(wins$n[wins$driver == d1])
lap_diff <- abs(first(t2$lap_time) - first(t1$lap_time))

ggplot(loc, aes(x, y)) +
  # grey outline underneath = the track
  geom_path(colour = "grey85", linewidth = 7, lineend = "round") +
  # coloured line on top = who was faster in each mini-sector
  geom_path(aes(colour = faster, group = 1), linewidth = 3.5, lineend = "round") +
  # start / finish marker
  geom_point(data = slice(loc, 1), shape = 21, size = 4,
             fill = "white", colour = "black", stroke = 1.2) +
  geom_text(data = slice(loc, 1), label = "Start / Finish",
            vjust = -1.6, size = 3.5, fontface = "bold") +
  scale_colour_manual(values = driver_cols) +
  coord_fixed() +            # keep the real shape of the track
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "bottom",
    plot.margin     = margin(15, 15, 15, 15)
  ) +
  labs(
    title    = paste("Track dominance:", d1, "vs", d2),
    subtitle = paste0(race_label, " qualifying\n",
                      d1, " faster in ", n_d1, " of ", n_mini, " mini-sectors",
                      " | lap time gap: ", sprintf("%.3f", lap_diff), " s"),
    colour   = "Faster driver"
  )

ggsave("q1_v1_track_map.png", width = 8, height = 8, dpi = 300, bg = "white")
