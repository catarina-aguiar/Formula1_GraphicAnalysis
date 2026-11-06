# ============================================================
#  02_saturday.R  –  Qualifying charts (reads data/ only)
#
#  Chart 1: the "breathing" track
#           thickness = speed, colour = who was faster at that point,
#           plus the official sector results and lengths
#  Chart 2: gearbox fingerprint (gear used around the lap, per driver)
#
#  Tip: after the data is loaded, you can re-run just this script
#  to tweak the charts – nothing is downloaded again.
#
#  Uses the data loaded by 01_load_data.R (runs it if needed).
# ============================================================

library(dplyr)
library(ggplot2)

if (!exists("track")) source("01_load_data.R")    # load the data once
driver_cols <- setNames(drivers$colour, drivers$code)

# ---- Shared style (editorial look, used by every chart) -----
BG <- "#FBF8F2"; INK <- "#2A2420"; MUTED <- "#7A6F66"
theme_poster <- function(base_size = 12) {
  theme_void(base_size = base_size, base_family = "serif") +
    theme(plot.background = element_rect(fill = BG, colour = NA),
          plot.title      = element_text(face = "bold", colour = INK, size = base_size * 1.5),
          plot.subtitle   = element_text(colour = MUTED, margin = margin(t = 3, b = 8)),
          legend.position = "bottom",
          # legends start at the left edge, in line with title and subtitle
          legend.justification = "left",
          legend.location      = "plot",
          legend.box.just      = "top",
          plot.title.position  = "plot",
          legend.title    = element_text(colour = MUTED, size = base_size * 0.8, face = "bold"),
          legend.text     = element_text(colour = MUTED, size = base_size * 0.75),
          plot.margin     = margin(12, 12, 12, 12))
}

# ============================================================
#  Chart 1 – the breathing track
# ============================================================
# What should the COLOUR of the track show?
#   "difference" = gradient from d2 faster (red) to d1 faster (blue)
#   "speed"      = gradient of the actual speed (slow -> fast)
colour_by <- "difference"

diff_limit <- 15   # km/h: differences bigger than this get the strongest colour

track <- track %>%
  mutate(diff_cap = pmax(pmin(diff, diff_limit), -diff_limit),
         # line thickness from speed: thin in slow corners, thick on straights
         lw = 0.8 + 3.2 * (speed - min(speed)) / (max(speed) - min(speed)))

# the value the colour shows
track$colour_value <- if (colour_by == "difference") track$diff_cap else track$speed

if (colour_by == "difference") {
  colour_scale <- scale_colour_gradient2(
    low = driver_cols[[d2]], mid = "#E9E3DA", high = driver_cols[[d1]], midpoint = 0,
    limits = c(-diff_limit, diff_limit),
    breaks = seq(-diff_limit, diff_limit, 5),
    # sizes without minus signs; the two ends also say who was faster
    labels = function(b) case_when(
      b == -diff_limit ~ paste0(diff_limit, "+\n", d2, " faster"),
      b ==  diff_limit ~ paste0(diff_limit, "+\n", d1, " faster"),
      TRUE             ~ as.character(abs(b))),
    name = "Difference in speed at each point (km/h)")
} else {
  colour_scale <- scale_colour_viridis_c(option = "inferno", end = 0.95,
                                         name = "Speed (km/h)")
}
track_closed <- bind_rows(track, slice(track, 1))      # close the loop

# legend for the thickness: a few round speeds, converted to line widths
lw_of <- function(s) 0.8 + 3.2 * (s - min(track$speed)) / (max(track$speed) - min(track$speed))
speed_breaks <- c(100, 200, 300)
speed_breaks <- speed_breaks[speed_breaks > min(track$speed) & speed_breaks < max(track$speed)]

# Sector borders (white dots) and labels
borders <- track %>% group_by(sector) %>% slice(1) %>% ungroup()
rng <- max(diff(range(track$x)), diff(range(track$y)))
cx  <- mean(range(track$x)); cy <- mean(range(track$y))
sector_labels <- track %>%
  group_by(sector) %>% slice(ceiling(n() / 2)) %>% ungroup() %>%
  left_join(sectors, by = "sector") %>%
  mutate(label = sprintf("Sector %d · %.1f km\n%s -%.3f s",
                         sector, length_m / 1000, faster, abs(delta)))

# Put each label in empty space next to its sector: try spots in 24
# directions at growing distances, and keep the closest one where the
# label doesn't touch the track or another label.
xr <- diff(range(track$x)); yr <- diff(range(track$y))
lab_w <- 0.30 * xr        # approx. size of a label in map units
lab_h <- 0.10 * yr        # (make bigger if labels still touch the track)
clear <- 0.03 * rng       # extra space kept around the track
sector_labels$lx <- NA_real_; sector_labels$ly <- NA_real_
for (i in seq_len(nrow(sector_labels))) {
  for (d in c(0.10, 0.14, 0.18, 0.23, 0.28, 0.34) * rng) {
    for (a in seq(0, 2 * pi, length.out = 25)[-25]) {
      px <- sector_labels$x[i] + d * cos(a); py <- sector_labels$y[i] + d * sin(a)
      hits_track <- any(abs(track$x - px) < lab_w / 2 + clear &
                        abs(track$y - py) < lab_h / 2 + clear)
      done <- which(!is.na(sector_labels$lx))
      hits_label <- any(abs(sector_labels$lx[done] - px) < lab_w &
                        abs(sector_labels$ly[done] - py) < lab_h)
      if (!hits_track && !hits_label) {
        sector_labels$lx[i] <- px; sector_labels$ly[i] <- py; break
      }
    }
    if (!is.na(sector_labels$lx[i])) break
  }
}

# Manual fine-tuning if you want (in % of the track size, e.g. 0.05 = 5%)
label_nudge <- tibble(sector = 1:3, nudge_x = c(0, 0, 0), nudge_y = c(0, 0, 0))
sector_labels <- sector_labels %>%
  left_join(label_nudge, by = "sector") %>%
  mutate(lx = coalesce(lx, x) + nudge_x * rng,
         ly = coalesce(ly, y) + nudge_y * rng)

lap_gap <- sum(sectors$delta)

p_track <- ggplot(track_closed, aes(x, y)) +
  # light outline underneath
  geom_path(aes(linewidth = lw + 1.6), colour = "#E6DFD3", lineend = "round",
            show.legend = FALSE) +
  # coloured line: thickness = speed, colour = see colour_by above
  geom_path(aes(linewidth = lw, colour = colour_value, group = 1), lineend = "round") +
  geom_point(data = borders, shape = 21, size = 3, fill = "white", colour = INK, stroke = 1) +
  geom_text(data = sector_labels,
            aes(lx, ly, label = label, colour = NULL),
            colour = driver_cols[sector_labels$faster],
            family = "serif", fontface = "bold", size = 4, lineheight = 0.95) +
  scale_linewidth_identity(guide = "legend", name = "Speed (km/h)",
                           breaks = lw_of(speed_breaks), labels = speed_breaks) +
  colour_scale +
  scale_x_continuous(expand = expansion(mult = 0.32)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +    # room for labels
  coord_fixed() +
  guides(colour    = guide_colourbar(barwidth = unit(14, "lines"), title.position = "top",
                                     order = 1),
         # thickness legend goes inside the chart, bottom-right corner
         linewidth = guide_legend(title.position = "top", order = 2, position = "inside",
                                  override.aes = list(colour = "#8A8178"))) +
  theme_poster() +
  theme(legend.position.inside      = c(1, 0),     # right edge, bottom
        legend.justification.inside = c(1, 0),
        legend.background           = element_blank()) +
  labs(title    = "Where was each driver faster?",
       subtitle = paste0(event, " · ", d1, " ahead by ",
                         sprintf("%.3f", lap_gap), " s"))

p_track
ggsave("saturday_track.png", p_track, width = 9, height = 8, dpi = 300, bg = BG)

# ============================================================
#  Chart 2 – gearbox fingerprint
# ============================================================
gears <- bind_rows(
  track_closed %>% transmute(x, y, driver = d1, gear = gear_1),
  track_closed %>% transmute(x, y, driver = d2, gear = gear_2)
) %>%
  mutate(gear   = factor(round(gear), levels = 1:8),
         driver = factor(driver, levels = c(d1, d2)))

p_gears <- ggplot(gears, aes(x, y)) +
  geom_path(colour = "#E6DFD3", linewidth = 4, lineend = "round") +
  geom_path(aes(colour = gear, group = 1), linewidth = 2, lineend = "round") +
  facet_wrap(~ driver) +
  scale_colour_viridis_d(option = "plasma", end = 0.92, drop = FALSE, name = "Gear") +
  coord_fixed() +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 3))) +
  theme_poster() +
  theme(strip.text = element_text(face = "bold", size = 13, family = "serif",
                                  margin = margin(b = 4))) +
  labs(title = "How did they drive it?",
       subtitle = "Gear used around the lap")

p_gears
ggsave("saturday_gears.png", p_gears, width = 9, height = 4.5, dpi = 300, bg = BG)
