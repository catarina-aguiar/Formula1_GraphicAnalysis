# ============================================================
#  Q1 – Where does Verstappen beat Hamilton, and how?
#  2026 Bahrain GP in Malaysia (Sepang), qualifying, fastest laps
#
#  DATA: OpenF1 API (https://openf1.org)
#    - laps:     official lap and sector times
#    - car_data: speed during the lap (~4 readings per second)
#    - location: x / y position of the car (to draw the track)
#
#  Chart 1: track map, each sector coloured by the faster driver
#           (official sector times)
#  Chart 2: speed trace of both laps, shaded by the same sectors,
#           with the time gap (delta) underneath
#
#  v2: laps lined up at the corner apexes (fixes the left/right shift
#      of the speed lines), delta panel added, map labels moved.
# ============================================================

# install.packages(c("jsonlite", "lubridate", "tidyr", "patchwork"))   # run once
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)   # to stack the speed trace and the delta

base <- "https://api.openf1.org/v1/"
get_openf1 <- function(query) fromJSON(paste0(base, query))

# ---- 1. Settings --------------------------------------------
#  OpenF1 uses car NUMBERS: 2026 VER = 3, HAM = 44
d1 <- "VER"; d1_num <- 3
d2 <- "HAM"; d2_num <- 44
driver_cols <- c(VER = "#3671C6", HAM = "#E8002D")
quali_day   <- "2026-10-03"
race_label  <- "Bahrain GP in Malaysia (Sepang) 2026 – qualifying"

# ---- 2. Find the qualifying session -------------------------
sessions <- get_openf1("sessions?year=2026&session_name=Qualifying")
session  <- sessions %>% filter(as.Date(ymd_hms(date_start)) == as.Date(quali_day))
session %>% select(session_key, country_name, circuit_short_name)   # check: Malaysia
sk <- session$session_key[1]

# ---- 3. Fastest lap + official sector times -----------------
get_best_lap <- function(code, number) {
  get_openf1(paste0("laps?session_key=", sk, "&driver_number=", number)) %>%
    filter(!is.na(lap_duration)) %>%
    slice_min(lap_duration, n = 1, with_ties = FALSE) %>%
    transmute(driver = code, number = number,
              start  = ymd_hms(date_start),
              lap    = lap_duration,
              s1 = duration_sector_1, s2 = duration_sector_2, s3 = duration_sector_3)
}
best <- bind_rows(get_best_lap(d1, d1_num), get_best_lap(d2, d2_num))
best        # CHECK: VER ~95.130, HAM ~95.428

# Who was faster in each sector, and by how much (official times)
sectors <- tibble(
  sector = 1:3,
  t_d1   = unlist(best[best$driver == d1, c("s1", "s2", "s3")]),
  t_d2   = unlist(best[best$driver == d2, c("s1", "s2", "s3")])
) %>%
  mutate(
    delta  = t_d2 - t_d1,                       # > 0 means VER faster
    faster = if_else(delta > 0, d1, d2),
    label  = sprintf("Sector %d\n%s -%.3f s", sector, faster, abs(delta))
  )
sectors
sum(sectors$delta)   # CHECK: ≈ 0.298

write.csv(best,    "q1_sepang2026_quali_laps.csv",    row.names = FALSE)
write.csv(sectors, "q1_sepang2026_quali_sectors.csv", row.names = FALSE)

# ---- 4. Speed during each fastest lap -----------------------
# distance travelled = speed x time, added up reading by reading
get_speed <- function(code) {
  b <- filter(best, driver == code)
  get_openf1(paste0("car_data?session_key=", sk, "&driver_number=", b$number)) %>%
    mutate(date = ymd_hms(date)) %>%
    filter(date >= b$start, date <= b$start + b$lap) %>%
    arrange(date) %>%
    mutate(time     = as.numeric(date - b$start, units = "secs"),
           distance = cumsum(speed / 3.6 * c(0, diff(time))),
           rel      = distance / max(distance),    # 0 = start, 1 = finish
           driver   = code)
}
tel <- bind_rows(get_speed(d1), get_speed(d2))

# Same distance scale for both laps (average lap length, in metres)
lap_m <- tel %>% group_by(driver) %>% summarise(m = max(distance)) %>% pull(m) %>% mean()
tel   <- tel %>% mutate(dist_m = rel * lap_m)
lap_m          # CHECK: Sepang is ~5,540 m

# ---- 4b. Line up the two laps at the corners ----------------
# Distance = speed x time builds up small errors (only ~4 readings per
# second), different for each driver, so the speed lines drift apart.
# But corners are in fixed places: the SLOWEST point of each big corner
# must be at the same distance for both drivers. So we find those
# points and stretch HAM's distances so his corners line up with VER's.
find_apexes <- function(d) {
  # a point is an apex if it's the slowest within 300 m either side
  # and below 220 km/h (a real corner, not a small lift)
  is_apex <- sapply(seq_len(nrow(d)), function(i) {
    near <- abs(d$dist_m - d$dist_m[i]) <= 300
    d$speed[i] == min(d$speed[near]) && d$speed[i] < 220
  })
  a <- d$dist_m[is_apex]
  a[c(TRUE, diff(a) > 300)]          # one point per corner
}
apex_v <- find_apexes(filter(tel, driver == d1))
apex_h <- find_apexes(filter(tel, driver == d2))

anchors <- tibble(
  ver = apex_v,
  ham = sapply(apex_v, function(a) apex_h[which.min(abs(apex_h - a))])
) %>% filter(abs(ver - ham) < 300)   # only pairs that are the same corner
anchors        # CHECK: one row per big corner (around 7-8 at Sepang)

tel <- tel %>%
  mutate(dist_m = if_else(
    driver == d2,
    approx(c(0, anchors$ham, lap_m), c(0, anchors$ver, lap_m),
           xout = dist_m, rule = 2)$y,
    dist_m))

write.csv(tel, "q1_sepang2026_quali_speed.csv", row.names = FALSE)

# Where do the sectors start/end along the lap?
# = where each driver was when his official S1 / S1+S2 time was reached
# (averaged over both drivers)
boundary <- function(code, t) {
  d <- filter(tel, driver == code)
  approx(d$time, d$dist_m, xout = t, ties = mean)$y
}
b1 <- mean(c(boundary(d1, best$s1[1]), boundary(d2, best$s1[2])))
b2 <- mean(c(boundary(d1, best$s1[1] + best$s2[1]),
             boundary(d2, best$s1[2] + best$s2[2])))
sectors <- sectors %>% mutate(xmin = c(0, b1, b2), xmax = c(b1, b2, lap_m))

# ---- 5. Chart 1: track map by sector ------------------------
# Track drawn from VER's x/y positions during his lap; each point gets
# a sector from the official sector times.
b_d1 <- filter(best, driver == d1)
track <- get_openf1(paste0("location?session_key=", sk, "&driver_number=", d1_num)) %>%
  mutate(date = ymd_hms(date)) %>%
  filter(date >= b_d1$start, date <= b_d1$start + b_d1$lap, !(x == 0 & y == 0)) %>%
  arrange(date) %>%
  mutate(time   = as.numeric(date - b_d1$start, units = "secs"),
         sector = case_when(time < b_d1$s1 ~ 1,
                            time < b_d1$s1 + b_d1$s2 ~ 2,
                            TRUE ~ 3)) %>%
  left_join(select(sectors, sector, faster), by = "sector")

track_closed <- bind_rows(track, slice(track, 1))   # close the loop
write.csv(track, "q1_sepang2026_quali_track_xy.csv", row.names = FALSE)

# Sector borders (first point of sectors 2 and 3) and the start line
borders <- track %>% group_by(sector) %>% slice(1) %>% ungroup()

# Labels: middle of each sector, pushed outwards from the track centre
rng <- max(diff(range(track$x)), diff(range(track$y)))
cx  <- mean(range(track$x)); cy <- mean(range(track$y))
labels <- track %>%
  group_by(sector) %>% slice(ceiling(n() / 2)) %>% ungroup() %>%
  left_join(select(sectors, sector, label), by = "sector") %>%
  mutate(len = sqrt((x - cx)^2 + (y - cy)^2),
         lx  = x + (x - cx) / len * 0.06 * rng,
         ly  = y + (y - cy) / len * 0.06 * rng,
         # text on the left of the track extends to the left, and on the
         # right extends to the right, so it never covers the track
         hj  = if_else(lx < cx, 1, 0))

ggplot(track_closed, aes(x, y)) +
  geom_path(colour = "grey85", linewidth = 7, lineend = "round") +
  geom_path(aes(colour = faster, group = 1), linewidth = 3.5, lineend = "round") +
  geom_point(data = borders, shape = 21, size = 3.5,
             fill = "white", colour = "black", stroke = 1) +
  geom_text(data = labels, aes(lx, ly, label = label, colour = faster, hjust = hj),
            size = 4, fontface = "bold", lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = driver_cols) +
  scale_x_continuous(expand = expansion(mult = 0.3)) +   # room for labels
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  coord_fixed() +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin     = margin(15, 15, 15, 15)) +
  labs(title    = paste("Who was faster where:", d1, "vs", d2),
       subtitle = paste0(race_label, "\nLap time gap: ",
                         sprintf("%.3f", sum(sectors$delta)), " s | official sector times"),
       colour   = "Faster in sector")

ggsave("q1_sector_map.png", width = 8, height = 8.5, dpi = 300, bg = "white")

# ---- 6. Chart 2: speed trace + time delta -------------------
# Time gap along the lap: at every distance, how much later did HAM get
# there than VER? Positive = VER ahead. Ends at the lap time gap.
v <- filter(tel, driver == d1); h <- filter(tel, driver == d2)
delta <- tibble(dist_m = seq(0, lap_m, by = 5)) %>%
  mutate(gap = approx(h$dist_m, h$time, xout = dist_m, rule = 2, ties = mean)$y -
               approx(v$dist_m, v$time, xout = dist_m, rule = 2, ties = mean)$y)
tail(delta, 1)    # CHECK: ≈ 0.298

# Shared pieces for both panels: sector shading and borders
sector_bg <- list(
  geom_rect(data = sectors,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = faster),
            alpha = 0.08, inherit.aes = FALSE),
  geom_vline(xintercept = c(b1, b2), linetype = "dashed", colour = "grey50"),
  scale_fill_manual(values = driver_cols, guide = "none"),
  scale_x_continuous(limits = c(0, lap_m), expand = expansion(mult = 0.01))
)

p_speed <- ggplot() +
  sector_bg +
  geom_line(data = tel, aes(dist_m, speed, colour = driver), linewidth = 0.8) +
  geom_text(data = sectors,
            aes(x = (xmin + xmax) / 2, y = Inf, colour = faster,
                label = sub("\n", " · ", label)),
            vjust = 1.6, size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = driver_cols) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "top",
        panel.grid.minor = element_blank(),
        axis.text.x      = element_blank(),
        plot.title       = element_text(face = "bold")) +
  labs(title    = paste("Speed and time gap:", d1, "vs", d2),
       subtitle = paste(race_label, "| fastest laps"),
       x = NULL, y = "Speed (km/h)", colour = NULL)

p_delta <- ggplot(delta, aes(dist_m)) +
  sector_bg +
  geom_hline(yintercept = 0, colour = "grey40") +
  # blue area when VER is ahead, red area when HAM is ahead
  geom_ribbon(aes(ymin = 0, ymax = pmax(gap, 0)), fill = driver_cols[[d1]], alpha = 0.35) +
  geom_ribbon(aes(ymin = pmin(gap, 0), ymax = 0), fill = driver_cols[[d2]], alpha = 0.35) +
  geom_line(aes(y = gap), linewidth = 0.7) +
  annotate("text", x = 0, y = Inf, label = paste(d1, "ahead"), hjust = -0.05,
           vjust = 1.3, colour = driver_cols[[d1]], fontface = "bold", size = 3.5) +
  annotate("text", x = 0, y = -Inf, label = paste(d2, "ahead"), hjust = -0.05,
           vjust = -0.5, colour = driver_cols[[d2]], fontface = "bold", size = 3.5) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Distance along the lap (m)", y = "Gap (s)")

p_speed / p_delta + plot_layout(heights = c(3, 1.3))

ggsave("q1_speed_delta.png", width = 11, height = 7, dpi = 300, bg = "white")
