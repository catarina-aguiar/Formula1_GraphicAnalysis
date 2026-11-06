# ============================================================
#  01_load_data.R
#  Downloads and prepares all the data for the poster.
#  Scripts 02 and 03 use the objects it creates (track, sectors,
#  positions, stints, ...). It also saves a copy as CSV in data/.
#
#  DATA SOURCES
#   - OpenF1 (https://openf1.org): qualifying laps + official
#     sector times, car telemetry (speed, gear), car position
#     (x/y), race tyre stints
#   - f1dataR (Jolpica API): race calendar, results, lap-by-lap
#     positions, pit stops
#
#  Change the SETTINGS block to make the poster for any two
#  drivers at any race from 2023 onwards (OpenF1 coverage).
# ============================================================

# install.packages(c("f1dataR", "jsonlite", "lubridate", "dplyr",
#                    "stringr", "tidyr"))                  # run once
library(f1dataR)
library(jsonlite)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

# ---- SETTINGS -----------------------------------------------
season    <- 2026
quali_day <- as.Date("2026-10-03")    # Saturday
race_day  <- as.Date("2026-10-04")    # Sunday
event     <- "Bahrain GP in Malaysia (Sepang) 2026"

# The two drivers. OpenF1 uses car numbers, Jolpica uses driver ids
# (see results$driver_id below if unsure).
drivers <- tibble(
  code      = c("VER", "HAM"),
  number    = c(3, 44),
  driver_id = c("max_verstappen", "hamilton"),
  colour    = c("#3671C6", "#E8002D")
)

# Tyre strategies: normally taken from OpenF1 automatically (set to NULL).
# For Sepang 2026 OpenF1's stint data is wrong (red-flagged start + restart
# behind the Safety Car), so we use the official strategies from the
# F1 and FIA race reports. lap_end = NA means "until the end of the race".
stints_override <- tribble(
  ~code, ~stint, ~lap_start, ~lap_end, ~compound,
  "VER", 1,  1,  9,  "INTERMEDIATE",
  "VER", 2, 10, 34,  "SOFT",
  "VER", 3, 35, 43,  "SOFT",
  "VER", 4, 44, NA,  "SOFT",
  "HAM", 1,  1, 31,  "SOFT",
  "HAM", 2, 32, 43,  "MEDIUM",
  "HAM", 3, 44, NA,  "SOFT"
)
# stints_override <- NULL     # <- use this line for any other race
# --------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
d1 <- drivers$code[1]; d2 <- drivers$code[2]
write.csv(mutate(drivers, season = season, event = event,
                 quali_day = quali_day, race_day = race_day),
          "data/settings.csv", row.names = FALSE)

base <- "https://api.openf1.org/v1/"
get_openf1 <- function(query) {
  out <- fromJSON(paste0(base, query))
  if (length(out) == 0 || NROW(out) == 0) stop("OpenF1 returned no data for: ", query)
  out
}
session_key <- function(name, day) {
  get_openf1(paste0("sessions?year=", season, "&session_name=", name)) %>%
    filter(as.Date(ymd_hms(date_start)) == day) %>%
    pull(session_key) %>% first()
}

# ============================================================
#  SATURDAY – QUALIFYING
# ============================================================
sk_q <- session_key("Qualifying", quali_day)

# ---- Fastest lap + official sector times --------------------
best <- bind_rows(lapply(seq_len(nrow(drivers)), function(i) {
  get_openf1(paste0("laps?session_key=", sk_q, "&driver_number=", drivers$number[i])) %>%
    filter(!is.na(lap_duration)) %>%
    slice_min(lap_duration, n = 1, with_ties = FALSE) %>%
    transmute(code = drivers$code[i], number = drivers$number[i],
              start = ymd_hms(date_start), lap = lap_duration,
              s1 = duration_sector_1, s2 = duration_sector_2, s3 = duration_sector_3)
}))
best    # CHECK: lap times match the official results?

# ---- Speed and gear during each fastest lap -----------------
# distance travelled = speed x time, added up reading by reading
get_car <- function(code) {
  b <- filter(best, code == !!code)
  get_openf1(paste0("car_data?session_key=", sk_q, "&driver_number=", b$number)) %>%
    mutate(date = ymd_hms(date)) %>%
    filter(date >= b$start, date <= b$start + b$lap) %>%
    arrange(date) %>%
    transmute(code = code,
              time = as.numeric(date - b$start, units = "secs"),
              speed, gear = n_gear, throttle, brake) %>%
    mutate(distance = cumsum(speed / 3.6 * c(0, diff(time))),
           rel = distance / max(distance))
}
tel <- bind_rows(get_car(d1), get_car(d2))
lap_m <- tel %>% group_by(code) %>% summarise(m = max(distance)) %>% pull(m) %>% mean()
tel <- tel %>% mutate(dist_m = rel * lap_m)
lap_m   # CHECK: roughly the circuit length in metres (Sepang ~5,540)

# ---- Line the two laps up at the corners --------------------
# Distance from speed x time drifts a little differently for each driver.
# Corners are in fixed places, so the slowest point of each big corner
# must be at the same distance: we stretch d2's distances to match d1's.
find_apexes <- function(d) {
  is_apex <- sapply(seq_len(nrow(d)), function(i) {
    near <- abs(d$dist_m - d$dist_m[i]) <= 300
    d$speed[i] == min(d$speed[near]) && d$speed[i] < 220
  })
  a <- d$dist_m[is_apex]
  a[c(TRUE, diff(a) > 300)]
}
apex_1 <- find_apexes(filter(tel, code == d1))
apex_2 <- find_apexes(filter(tel, code == d2))
anchors <- tibble(a1 = apex_1,
                  a2 = sapply(apex_1, function(a) apex_2[which.min(abs(apex_2 - a))])) %>%
  filter(abs(a1 - a2) < 300)
anchors  # CHECK: one row per big corner
tel <- tel %>%
  mutate(dist_m = if_else(code == d2,
                          approx(c(0, anchors$a2, lap_m), c(0, anchors$a1, lap_m),
                                 xout = dist_m, rule = 2)$y,
                          dist_m))

# ---- Official sectors: who was faster, where, how long ------
sec_time <- function(cd) unlist(best[best$code == cd, c("s1", "s2", "s3")])
boundary <- function(cd, t) {
  d <- filter(tel, code == cd)
  approx(d$time, d$dist_m, xout = t, ties = mean)$y
}
b1 <- mean(c(boundary(d1, best$s1[1]), boundary(d2, best$s1[2])))
b2 <- mean(c(boundary(d1, best$s1[1] + best$s2[1]), boundary(d2, best$s1[2] + best$s2[2])))

sectors <- tibble(
  sector  = 1:3,
  t_d1    = sec_time(d1),
  t_d2    = sec_time(d2),
  start_m = c(0, b1, b2),
  end_m   = c(b1, b2, lap_m)
) %>%
  mutate(delta    = t_d2 - t_d1,                 # > 0 means d1 faster
         faster   = if_else(delta > 0, d1, d2),
         length_m = end_m - start_m)
sectors   # CHECK: sum(sectors$delta) = lap time gap
sum(sectors$delta)

# ---- Track shape (x/y of d1's lap), with speed & gear of both
b_1 <- filter(best, code == d1)
loc <- get_openf1(paste0("location?session_key=", sk_q, "&driver_number=", b_1$number)) %>%
  mutate(date = ymd_hms(date)) %>%
  filter(date >= b_1$start, date <= b_1$start + b_1$lap, !(x == 0 & y == 0)) %>%
  arrange(date) %>%
  mutate(time = as.numeric(date - b_1$start, units = "secs"))

t1 <- filter(tel, code == d1); t2 <- filter(tel, code == d2)
at_dist <- function(d, col, dist, method = "linear")
  approx(d$dist_m, d[[col]], xout = dist, rule = 2, ties = mean, method = method)$y
smooth5 <- function(x) coalesce(as.numeric(stats::filter(x, rep(1 / 5, 5), sides = 2)), x)

track <- tibble(time = seq(0, max(loc$time), by = 0.05)) %>%      # smooth line
  mutate(x      = approx(loc$time, loc$x, xout = time)$y,
         y      = approx(loc$time, loc$y, xout = time)$y,
         dist_m = approx(t1$time, t1$dist_m, xout = time, rule = 2, ties = mean)$y,
         speed_1 = at_dist(t1, "speed", dist_m),
         speed_2 = at_dist(t2, "speed", dist_m),
         gear_1  = at_dist(t1, "gear", dist_m, "constant"),
         gear_2  = at_dist(t2, "gear", dist_m, "constant"),
         speed   = (speed_1 + speed_2) / 2,
         diff    = smooth5(speed_1 - speed_2),            # > 0 = d1 faster
         sector  = cut(dist_m, c(-Inf, b1, b2, Inf), labels = FALSE)) %>%
  filter(!is.na(x), !is.na(y))          # drop points before the first GPS reading

# ---- Save Saturday ------------------------------------------
write.csv(best,    "data/quali_fastest_laps.csv", row.names = FALSE)
write.csv(sectors, "data/quali_sectors.csv",      row.names = FALSE)
write.csv(tel,     "data/quali_telemetry.csv",    row.names = FALSE)
write.csv(track,   "data/quali_track.csv",        row.names = FALSE)

# ============================================================
#  SUNDAY – RACE
# ============================================================
schedule <- load_schedule(season)
rnd <- schedule %>% filter(as.Date(date) == race_day) %>% pull(round) %>% as.numeric()
rnd     # CHECK: round number (16 for Sepang 2026)

results <- load_results(season, rnd)
laps    <- load_laps(season, rnd)
pits    <- load_pitstops(season, rnd)

# "max_verstappen" -> "VER", "hamilton" -> "HAM"
code_of <- function(id) toupper(str_sub(str_extract(id, "[a-z]+$"), 1, 3))

race_results <- results %>%
  transmute(driver_id, code = code_of(driver_id),
            grid   = as.numeric(grid),
            grid   = if_else(grid == 0, n(), grid),      # pit-lane start -> last
            finish = as.numeric(position),
            status)
race_results %>% print(n = 25)   # CHECK: names and positions

# Race length = laps completed by the winner (official result).
# (The lap-by-lap data can contain extra laps, e.g. behind the Safety Car
#  before a restart, so we don't trust its highest lap number.)
n_laps <- results %>% filter(as.numeric(position) == 1) %>% pull(laps) %>% as.numeric()
n_laps     # CHECK: official race length (Sepang 2026: around 55-56)
max(as.numeric(laps$lap))   # highest lap in the lap-by-lap data, for comparison

positions <- laps %>%
  transmute(driver_id, lap = as.numeric(lap), position = as.numeric(position)) %>%
  filter(lap <= n_laps) %>%
  bind_rows(race_results %>% transmute(driver_id, lap = 0, position = grid)) %>%
  mutate(code = code_of(driver_id)) %>%
  arrange(driver_id, lap)

# ---- Tyre stints (OpenF1) -----------------------------------
sk_r <- session_key("Race", race_day)
stints <- bind_rows(lapply(seq_len(nrow(drivers)), function(i) {
  get_openf1(paste0("stints?session_key=", sk_r, "&driver_number=", drivers$number[i])) %>%
    transmute(code = drivers$code[i], stint = stint_number,
              lap_start = as.numeric(lap_start), lap_end = as.numeric(lap_end),
              compound = toupper(coalesce(compound, "UNKNOWN")))
})) %>%
  mutate(lap_end = coalesce(lap_end, n_laps)) %>%
  arrange(code, stint)

if (!is.null(stints_override)) {
  stints <- stints_override %>%
    mutate(lap_end = coalesce(as.numeric(lap_end), as.numeric(n_laps))) %>%
    arrange(code, stint)
}
stints   # CHECK: compounds and lap ranges make sense

# ---- Pit stops of the two drivers + tyre they switched to ---
pit_stops <- pits %>%
  transmute(driver_id, lap = as.numeric(lap)) %>%
  inner_join(select(drivers, driver_id, code), by = "driver_id") %>%
  left_join(select(positions, driver_id, lap, position), by = c("driver_id", "lap")) %>%
  rowwise() %>%
  mutate(new_compound = {
    s <- stints[stints$code == code & stints$lap_start > lap, ]
    if (nrow(s) == 0) "UNKNOWN" else s$compound[which.min(s$lap_start)]
  }) %>% ungroup()
pit_stops

# ---- Save Sunday --------------------------------------------
write.csv(race_results, "data/race_results.csv",   row.names = FALSE)
write.csv(positions,    "data/race_positions.csv", row.names = FALSE)
write.csv(stints,       "data/race_stints.csv",    row.names = FALSE)
write.csv(pit_stops,    "data/race_pit_stops.csv", row.names = FALSE)

message("Done! All data saved in the folder data/")
