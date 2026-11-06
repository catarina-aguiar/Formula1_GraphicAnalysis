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
#  Chart 2: speed trace of both laps, shaded by the same sectors
# ============================================================

# install.packages(c("jsonlite", "lubridate", "tidyr"))   # run once
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

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
         lx  = x + (x - cx) / len * 0.13 * rng,
         ly  = y + (y - cy) / len * 0.13 * rng)

ggplot(track_closed, aes(x, y)) +
  geom_path(colour = "grey85", linewidth = 7, lineend = "round") +
  geom_path(aes(colour = faster, group = 1), linewidth = 3.5, lineend = "round") +
  geom_point(data = borders, shape = 21, size = 3.5,
             fill = "white", colour = "black", stroke = 1) +
  geom_text(data = labels, aes(lx, ly, label = label, colour = faster),
            size = 4, fontface = "bold", lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = driver_cols) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
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

# ---- 6. Chart 2: speed trace shaded by sector ---------------
ggplot() +
  # light background in the colour of the faster driver in each sector
  geom_rect(data = sectors,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = faster),
            alpha = 0.08) +
  geom_vline(xintercept = c(b1, b2), linetype = "dashed", colour = "grey50") +
  geom_line(data = tel, aes(dist_m, speed, colour = driver), linewidth = 0.8) +
  # sector labels at the top
  geom_text(data = sectors,
            aes(x = (xmin + xmax) / 2, y = Inf, colour = faster,
                label = sub("\n", " · ", label)),
            vjust = 1.6, size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = driver_cols) +
  scale_fill_manual(values = driver_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +   # room for labels
  theme_minimal(base_size = 12) +
  theme(legend.position  = "top",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold")) +
  labs(title    = paste("Speed trace:", d1, "vs", d2),
       subtitle = paste(race_label, "| fastest laps"),
       x = "Distance along the lap (m)", y = "Speed (km/h)", colour = NULL)

ggsave("q1_speed_trace_sectors.png", width = 11, height = 5, dpi = 300, bg = "white")
