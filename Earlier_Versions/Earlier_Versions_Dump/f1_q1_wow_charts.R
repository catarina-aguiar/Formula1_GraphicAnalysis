# ============================================================
#  Q1 – Three "wow" charts: Verstappen vs Hamilton
#  2026 Bahrain GP in Malaysia (Sepang), qualifying, fastest laps
#
#  RUN f1_q1_sectors_map_speed_v2.R FIRST (same R session).
#  This script reuses what it created: tel (speed, gear, aligned
#  distance), track (VER's x/y), sectors, labels, borders, b1, b2,
#  lap_m, best, driver_cols, d1, d2, race_label.
#
#  Chart A: the "breathing" track  (thickness = speed, colour = who's faster)
#  Chart B: the lap as a clock     (speed trace wrapped into a circle)
#  Chart C: gearbox fingerprint    (track coloured by gear, per driver)
# ============================================================

library(dplyr)
library(ggplot2)

v <- filter(tel, driver == d1)
h <- filter(tel, driver == d2)

# Smooth a noisy series a little (average of each point with its neighbours)
smooth5 <- function(x) as.numeric(stats::filter(x, rep(1 / 5, 5), sides = 2)) %>%
  coalesce(x)

# A denser version of the track (a point every 0.05 s of VER's lap) so the
# lines look smooth. Each point also gets its distance along the lap.
track_d <- tibble(time = seq(0, max(track$time), by = 0.05)) %>%
  mutate(x      = approx(track$time, track$x, xout = time)$y,
         y      = approx(track$time, track$y, xout = time)$y,
         dist_m = approx(v$time, v$dist_m, xout = time, rule = 2, ties = mean)$y)

# Speed and gear of BOTH drivers at the same point on track
at_dist <- function(d, col, method = "linear") {
  approx(d$dist_m, d[[col]], xout = track_d$dist_m, rule = 2,
         ties = mean, method = method)$y
}
track_d <- track_d %>%
  mutate(speed_v = at_dist(v, "speed"),
         speed_h = at_dist(h, "speed"),
         gear_v  = at_dist(v, "n_gear", "constant"),
         gear_h  = at_dist(h, "n_gear", "constant"),
         speed   = (speed_v + speed_h) / 2,          # average speed
         diff    = smooth5(speed_v - speed_h))       # > 0 = VER faster

track_d_closed <- bind_rows(track_d, slice(track_d, 1))   # close the loop
write.csv(track_d, "q1_sepang2026_quali_track_speed_gear.csv", row.names = FALSE)

# ============================================================
#  Chart A – The "breathing" track
#  thickness = average speed (thick on straights, thin in corners)
#  colour    = speed difference (blue = VER faster, red = HAM faster)
# ============================================================
lim <- max(abs(track_d$diff), na.rm = TRUE)

ggplot(track_d_closed, aes(x, y)) +
  geom_path(aes(linewidth = speed, colour = diff), lineend = "round") +
  geom_point(data = borders, shape = 21, size = 3,
             fill = "white", colour = "black", stroke = 1) +
  geom_text(data = labels, aes(lx, ly, label = label, hjust = hj),
            colour = "grey20", size = 3.8, fontface = "bold", lineheight = 0.9) +
  scale_linewidth(range = c(0.8, 7), name = "Speed (km/h)") +
  scale_colour_gradient2(low = driver_cols[[d2]], mid = "grey88", high = driver_cols[[d1]],
                         midpoint = 0, limits = c(-lim, lim),
                         name = paste0("Speed difference (km/h)\n← ", d2,
                                       " faster   ", d1, " faster →")) +
  scale_x_continuous(expand = expansion(mult = 0.3)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  coord_fixed() +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box      = "vertical",
        legend.title    = element_text(size = 10),
        plot.margin     = margin(15, 15, 15, 15)) +
  guides(colour = guide_colourbar(barwidth = 15, title.position = "top", order = 1),
         linewidth = guide_legend(title.position = "top", order = 2)) +
  labs(title    = paste("The breathing track:", d1, "vs", d2),
       subtitle = paste0(race_label,
                         "\nThickness = how fast  |  Colour = who is faster there"))

ggsave("q1_wow_breathing_track.png", width = 8, height = 9.5, dpi = 300, bg = "white")

# ============================================================
#  Chart B – The lap as a clock
#  Start line at 12 o'clock, lap goes clockwise.
#  Distance from the centre = speed. Corners = spikes inwards.
# ============================================================
rings <- tibble(speed = c(100, 200, 300))

ggplot() +
  # sector wedges, coloured by who was faster
  geom_rect(data = sectors,
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 340, fill = faster),
            alpha = 0.10) +
  # speed rings (100 / 200 / 300 km/h)
  geom_hline(data = rings, aes(yintercept = speed), colour = "grey80", linewidth = 0.3) +
  geom_text(data = rings, aes(x = lap_m * 0.985, y = speed, label = paste(speed, "km/h")),
            size = 2.6, colour = "grey50", hjust = 1) +
  geom_vline(xintercept = c(0, b1, b2), colour = "grey55", linetype = "dashed") +
  # the two laps
  geom_path(data = tel, aes(dist_m, speed, colour = driver), linewidth = 0.8) +
  # sector names outside the circle
  geom_text(data = sectors,
            aes(x = (xmin + xmax) / 2, y = 385, label = label, colour = faster),
            size = 3.6, fontface = "bold", lineheight = 0.9, show.legend = FALSE) +
  # lap times in the middle
  annotate("text", x = 0, y = -120, size = 4, fontface = "bold", lineheight = 1.1,
           label = paste0(d1, "  ", sprintf("%.3f", best$lap[best$driver == d1]), " s\n",
                          d2, "  ", sprintf("%.3f", best$lap[best$driver == d2]), " s")) +
  scale_colour_manual(values = driver_cols) +
  scale_fill_manual(values = driver_cols, guide = "none") +
  scale_x_continuous(limits = c(0, lap_m)) +
  scale_y_continuous(limits = c(-120, 400)) +      # the empty hole in the middle
  coord_polar(start = 0, direction = 1) +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5),
        legend.position = "bottom") +
  labs(title    = paste("One lap of Sepang as a clock:", d1, "vs", d2),
       subtitle = paste0(race_label,
                         "\nStart at 12 o'clock, clockwise | further out = faster"),
       colour = NULL)

ggsave("q1_wow_lap_clock.png", width = 8, height = 8.5, dpi = 300, bg = "white")

# ============================================================
#  Chart C – Gearbox fingerprint
#  The same track for each driver, coloured by the gear he used.
# ============================================================
gears <- bind_rows(
  track_d_closed %>% transmute(x, y, driver = d1, gear = gear_v),
  track_d_closed %>% transmute(x, y, driver = d2, gear = gear_h)
) %>%
  mutate(gear   = factor(round(gear), levels = 1:8),
         driver = factor(driver, levels = c(d1, d2)))

ggplot(gears, aes(x, y)) +
  geom_path(colour = "grey88", linewidth = 6, lineend = "round") +
  geom_path(aes(colour = gear, group = 1), linewidth = 3, lineend = "round") +
  facet_wrap(~ driver) +
  scale_colour_viridis_d(option = "plasma", end = 0.92, drop = FALSE, name = "Gear") +
  coord_fixed() +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5),
        strip.text      = element_text(face = "bold", size = 14,
                                       margin = margin(b = 8)),
        legend.position = "bottom",
        plot.margin     = margin(15, 15, 15, 15)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 4))) +
  labs(title    = "Gearbox fingerprint",
       subtitle = paste(race_label, "| gear used around the fastest lap"))

ggsave("q1_wow_gear_fingerprint.png", width = 11, height = 6.5, dpi = 300, bg = "white")
