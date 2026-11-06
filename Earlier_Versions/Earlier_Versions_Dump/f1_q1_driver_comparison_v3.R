# ============================================================
#  Q1 – Where on the track does one driver gain time over another?
#  VERSION 3 – qualifying, Verstappen vs Hamilton
#  2026 Bahrain GP in Malaysia (Sepang)
#
#  DATA: OpenF1 API (https://openf1.org) – free, no Python needed.
#  (FastF1 can't load 2026 data: F1 put its timing feed behind a login.)
#
#  What changed:
#  v2: laps lined up by POSITION ON TRACK (x/y), not by timestamps.
#  v3: (a) each GPS point is matched to the track by FOLLOWING the car
#      along the circuit, not just "closest point". At Sepang the main
#      and back straights run side by side, so "closest point" sometimes
#      jumped to the wrong straight and skipped ~0.3 s of VER's lap.
#      (b) each lap is scaled to its OFFICIAL lap time, so the totals
#      always add up to the real gap (0.298 s).
#      (c) labels are placed by checking free space: no label or
#      pointer line crosses the track.
#
#  Chart 1: speed along the lap, both drivers
#  Chart 2: time gap between them along the lap
#  Chart 3: track dominance strip (mini-sectors)
#  Chart 4: track dominance on the circuit map, with the time gained
#           along each stretch (zone) one driver won
# ============================================================

# install.packages(c("jsonlite", "lubridate"))   # run once
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)

base <- "https://api.openf1.org/v1/"

# Download one OpenF1 table; stop with a clear message if it's empty
get_openf1 <- function(query) {
  out <- fromJSON(paste0(base, query))
  if (length(out) == 0 || NROW(out) == 0) {
    stop("OpenF1 returned no data for: ", query)
  }
  out
}

# ---- 1. Settings --------------------------------------------
#  OpenF1 uses car NUMBERS, not codes. 2026: VER = 3, HAM = 44
d1 <- "VER"; d1_num <- 3;  col1 <- "#3671C6"   # Red Bull blue
d2 <- "HAM"; d2_num <- 44; col2 <- "#E8002D"   # Ferrari red
quali_day  <- "2026-10-03"
race_label <- "Bahrain GP in Malaysia (Sepang) 2026"
n_mini     <- 25                               # number of mini-sectors

driver_cols <- setNames(c(col1, col2), c(d1, d2))

# ---- 2. Find the qualifying session -------------------------
sessions <- get_openf1("sessions?year=2026&session_name=Qualifying")
session  <- sessions %>% filter(as.Date(ymd_hms(date_start)) == as.Date(quali_day))
session %>% select(session_key, country_name, circuit_short_name, date_start)  # check!
sk <- session$session_key[1]

# ---- 3. Load each driver's fastest lap ----------------------
# We take a window from 10 s before to 10 s after the lap, so the
# lap is fully inside it even if OpenF1's timestamps are a bit off.
load_driver <- function(drv_num, code) {
  laps <- get_openf1(paste0("laps?session_key=", sk, "&driver_number=", drv_num))
  best <- laps %>% filter(!is.na(lap_duration)) %>%
    slice_min(lap_duration, n = 1, with_ties = FALSE)

  start <- ymd_hms(best$date_start)
  from  <- start - 10
  to    <- start + best$lap_duration + 10

  car <- get_openf1(paste0("car_data?session_key=", sk, "&driver_number=", drv_num)) %>%
    mutate(date = ymd_hms(date)) %>%
    filter(date >= from, date <= to) %>% arrange(date)

  loc <- get_openf1(paste0("location?session_key=", sk, "&driver_number=", drv_num)) %>%
    mutate(date = ymd_hms(date)) %>%
    filter(date >= from, date <= to) %>% arrange(date) %>%
    filter(!(x == 0 & y == 0))                       # drop empty readings

  list(code = code, best = best, start = start, car = car, loc = loc)
}

D1 <- load_driver(d1_num, d1)
D2 <- load_driver(d2_num, d2)

# ---- 4. Reference track + position along it -----------------
# The circuit is drawn from VER's positions during his lap, then filled
# in every 2 m so positions can be measured precisely.
ref <- D1$loc %>%
  filter(date >= D1$start, date <= D1$start + D1$best$lap_duration) %>%
  select(x, y)
ref$s <- c(0, cumsum(sqrt(diff(ref$x)^2 + diff(ref$y)^2)))
# full lap length = the path + the small piece that closes the loop
L <- max(ref$s) + sqrt((ref$x[1] - last(ref$x))^2 + (ref$y[1] - last(ref$y))^2)

ref_d <- tibble(s = seq(0, max(ref$s), by = 2)) %>%
  mutate(x  = approx(ref$s, ref$x, xout = s)$y,
         y  = approx(ref$s, ref$y, xout = s)$y,
         id = row_number())

# How far along the track is each GPS point?
# We FOLLOW the car: start at the moment closest to the lap start
# (on the main straight), then for every next point only look a little
# ahead on the track (as far as the car could have driven since the
# last reading). This stops points jumping to the parallel straight.
track_pos_seq <- function(px, py, t) {
  N   <- nrow(ref_d)
  n   <- length(px)
  idx <- integer(n)
  i0  <- which.min(abs(t))
  idx[i0] <- which.min((ref_d$x - px[i0])^2 + (ref_d$y - py[i0])^2)

  next_idx <- function(i, prev, forward) {
    dt    <- abs(t[i] - t[prev])
    ahead <- ceiling((60 + 100 * dt) / 2)    # 100 m/s = 360 km/h, steps of 2 m
    offs  <- if (forward) (-8):ahead else (-ahead):8
    cand  <- ((idx[prev] - 1 + offs) %% N) + 1
    cand[which.min((ref_d$x[cand] - px[i])^2 + (ref_d$y[cand] - py[i])^2)]
  }
  if (i0 < n) for (i in (i0 + 1):n) idx[i] <- next_idx(i, i - 1, TRUE)
  if (i0 > 1) for (i in (i0 - 1):1) idx[i] <- next_idx(i, i + 1, FALSE)
  ref_d$s[idx]
}

# For one driver: position along the track at every moment, a function
# that says WHEN the driver reached any point of the lap, and the
# telemetry with track position added.
prep_driver <- function(D) {
  loc <- D$loc %>%
    mutate(time = as.numeric(date - D$start, units = "secs"))
  loc$s <- track_pos_seq(loc$x, loc$y, loc$time)
  # "unwrap": when the car crosses the line, keep counting (L, 2L, ...)
  jumps    <- (diff(loc$s) < -L / 2) - (diff(loc$s) > L / 2)
  loc$s_un <- cummax(loc$s + L * cumsum(c(0, jumps)))

  # which crossing of the line starts this lap?
  s_start <- approx(loc$time, loc$s_un, xout = 0, ties = mean)$y
  m       <- round(s_start / L)

  raw_time <- function(p) approx(loc$s_un, loc$time, xout = m * L + p, ties = mean)$y
  t0      <- raw_time(0)
  raw_lap <- raw_time(L) - t0                    # lap time measured from GPS

  # Scale so the lap takes exactly the OFFICIAL lap time (removes the
  # small errors from ~4 GPS readings per second)
  k <- D$best$lap_duration / raw_lap
  pos2time <- function(p) (raw_time(p) - t0) * k  # 0 at the line

  car <- D$car %>%
    mutate(time = as.numeric(date - D$start, units = "secs"),
           pos  = approx(loc$time, loc$s_un, xout = time, ties = mean)$y - m * L) %>%
    filter(pos >= 0, pos <= L) %>%
    mutate(time = (time - t0) * k, driver = D$code)

  list(code = D$code, best = D$best, pos2time = pos2time, car = car,
       raw_lap = raw_lap)
}

P1 <- prep_driver(D1)
P2 <- prep_driver(D2)

# CHECK: lap time measured from GPS (before scaling) vs official.
# Both should now be close (within ~0.1 s). If one is far off, the
# matching still went wrong - tell us!
check <- tibble(
  driver       = c(d1, d2),
  official     = c(P1$best$lap_duration, P2$best$lap_duration),
  measured_gps = c(P1$raw_lap, P2$raw_lap)
)
print(check, digits = 6)

# Telemetry with track position, saved for the submission
tel <- bind_rows(P1$car, P2$car)
write.csv(tel, "q1_sepang2026_quali_VER_HAM_telemetry.csv", row.names = FALSE)

# ---- 5. Chart 1: speed along the lap ------------------------
ggplot(tel, aes(pos, speed, colour = driver)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = driver_cols) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title    = paste("Speed trace:", d1, "vs", d2),
    subtitle = paste(race_label, "| fastest qualifying lap"),
    x = "Distance along the lap (m)", y = "Speed (km/h)", colour = NULL
  )

ggsave("q1_v3_speed_trace.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 6. Chart 2: the gap along the lap ----------------------
# At every point of the lap: how much later did d2 get there than d1?
# Positive = d1 ahead.
gap <- tibble(pos = seq(0, L, by = 10)) %>%
  mutate(gap = P2$pos2time(pos) - P1$pos2time(pos))

ggplot(gap, aes(pos, gap)) +
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
    subtitle = paste("Line going up =", d1, "gaining time, going down =", d2, "gaining time"),
    x = "Distance along the lap (m)", y = "Gap (seconds)"
  )

ggsave("q1_v3_gap.png", width = 11, height = 5, dpi = 300, bg = "white")

# ---- 7. Mini-sectors and zones ------------------------------
breaks <- seq(0, L, length.out = n_mini + 1)    # equal pieces of the lap

mini <- tibble(
  minisector = 1:n_mini,
  t_d1 = diff(P1$pos2time(breaks)),     # seconds d1 spent in this piece
  t_d2 = diff(P2$pos2time(breaks))      # seconds d2 spent in this piece
) %>%
  mutate(
    delta  = t_d2 - t_d1,                   # > 0 means d1 was quicker here
    driver = if_else(delta > 0, d1, d2),    # who won the mini-sector
    gained = abs(delta)                     # how much time the winner gained
  )

# ZONES: consecutive mini-sectors won by the same driver = one zone
mini <- mini %>%
  mutate(zone = cumsum(driver != lag(driver, default = first(driver))) + 1)
# if the lap starts and ends with the same driver, it's one zone
# across the start/finish line
wraps <- first(mini$driver) == last(mini$driver) && max(mini$zone) > 1
if (wraps) mini$zone[mini$zone == max(mini$zone)] <- 1

zones <- mini %>%
  group_by(zone, driver) %>%
  summarise(gained = sum(gained), minisectors = n(), .groups = "drop")

mini                                    # numbers per mini-sector
zones                                   # numbers per zone
totals <- mini %>% group_by(driver) %>% summarise(total_gained = sum(gained))
totals
# CHECK: must equal the official gap (0.298 s)
sum(mini$delta)

# ---- 8. Chart 3: dominance strip ----------------------------
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

ggsave("q1_v3_dominance.png", width = 11, height = 3, dpi = 300, bg = "white")

# ---- 9. Chart 4: dominance on the track map -----------------
track <- ref_d %>%
  mutate(minisector = cut(s, breaks = breaks, labels = FALSE, include.lowest = TRUE)) %>%
  left_join(select(mini, minisector, zone, faster = driver), by = "minisector")

track_closed <- bind_rows(track, slice(track, 1))   # join the loop up
write.csv(track, "q1_sepang2026_quali_track_xy.csv", row.names = FALSE)

# One label per zone, at the middle of the zone
labels <- track %>%
  mutate(order = if (wraps) if_else(zone == 1 & s > L / 2, s - L, s) else s) %>%
  group_by(zone, faster) %>%
  arrange(order, .by_group = TRUE) %>%
  slice(ceiling(n() / 2)) %>%
  ungroup() %>%
  left_join(select(zones, zone, gained), by = "zone") %>%
  mutate(label = sprintf("-%.3f", gained))

# Place each label in free space next to its stretch.
# For every label we try many spots around it (different directions and
# distances) and keep the CLOSEST spot where:
#   1. the label doesn't touch the track,
#   2. it doesn't overlap a label already placed,
#   3. its pointer line doesn't cross the track.
rng <- max(diff(range(track$x)), diff(range(track$y)))
lab_w <- 0.10  * rng      # label width  (in map units) - adjust if needed
lab_h <- 0.035 * rng      # label height
tw    <- 0.022 * rng      # half the width of the drawn track

angles <- seq(0, 2 * pi, length.out = 37)[-37]
dists  <- c(0.06, 0.08, 0.10, 0.13, 0.16, 0.20, 0.25) * rng

labels$lx <- NA_real_; labels$ly <- NA_real_
for (i in seq_len(nrow(labels))) {
  ax <- labels$x[i]; ay <- labels$y[i]
  best <- NULL; best_d <- Inf
  for (d in dists) {
    if (d > best_d) break
    for (a in angles) {
      cx <- ax + d * cos(a); cy <- ay + d * sin(a)
      # 1. not on the track
      if (any(abs(track$x - cx) < lab_w / 2 + tw &
              abs(track$y - cy) < lab_h / 2 + tw)) next
      # 2. not on another label
      done <- which(!is.na(labels$lx))
      if (any(abs(labels$lx[done] - cx) < lab_w * 1.05 &
              abs(labels$ly[done] - cy) < lab_h * 1.2)) next
      # 3. pointer line doesn't cross the track (ignore its starting point)
      tt <- seq(0, 1, length.out = 25)
      sx <- ax + tt * (cx - ax); sy <- ay + tt * (cy - ay)
      keep <- sqrt((sx - ax)^2 + (sy - ay)^2) > 2.5 * tw
      crosses <- any(sapply(which(keep), function(k)
        min((track$x - sx[k])^2 + (track$y - sy[k])^2) < (0.8 * tw)^2))
      if (crosses) next
      best <- c(cx, cy); best_d <- d
      break
    }
  }
  if (is.null(best)) best <- c(ax, ay + 0.08 * rng)   # fallback
  labels$lx[i] <- best[1]; labels$ly[i] <- best[2]
}

# Pointer lines: from the track to the edge of the label
labels <- labels %>%
  mutate(
    dx = lx - x, dy = ly - y,
    f  = pmin((lab_w / 2) / pmax(abs(dx), 1e-9), (lab_h / 2) / pmax(abs(dy), 1e-9)),
    ex = lx - f * dx, ey = ly - f * dy
  )

# Numbers for the subtitle
n_d1     <- sum(mini$driver == d1)
lap_diff <- abs(P2$best$lap_duration - P1$best$lap_duration)

ggplot(track_closed, aes(x, y)) +
  geom_path(colour = "grey85", linewidth = 7, lineend = "round") +
  geom_path(aes(colour = faster, group = 1), linewidth = 3.5, lineend = "round") +
  geom_point(data = slice(track, 1), shape = 21, size = 4,
             fill = "white", colour = "black", stroke = 1.2) +
  geom_segment(data = labels, aes(x = x, y = y, xend = ex, yend = ey,
                                   colour = faster),
               linewidth = 0.35, show.legend = FALSE) +
  geom_text(data = labels, aes(x = lx, y = ly, label = label, colour = faster),
            size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = driver_cols, breaks = c(d1, d2)) +  # no "NA"
  scale_x_continuous(expand = expansion(mult = 0.1)) +   # room for labels
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  coord_fixed() +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5),
    plot.caption    = element_text(hjust = 0.5, colour = "grey30"),
    legend.position = "bottom",
    plot.margin     = margin(15, 15, 15, 15)
  ) +
  labs(
    title    = paste("Track dominance:", d1, "vs", d2),
    subtitle = paste0(race_label, " qualifying\n",
                      d1, " faster in ", n_d1, " of ", n_mini, " mini-sectors",
                      " | lap time gap: ", sprintf("%.3f", lap_diff), " s"),
    caption  = paste("Numbers: time gained by the faster driver along each coloured stretch",
                     "\n(consecutive mini-sectors won by the same driver)"),
    colour   = "Faster driver"
  )

ggsave("q1_v3_track_map.png", width = 8, height = 8.5, dpi = 300, bg = "white")
