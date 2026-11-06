# ============================================================
#  02_saturday.R  –  Qualifying charts
# ============================================================

library(dplyr)
library(ggplot2)

if (!exists("FONT"))  source("00_theme.R")        # colours + font template
if (!exists("track")) source("01_load_data.R")    # load the data once
driver_cols <- setNames(drivers$colour, drivers$code)

# Shared style (colours and font come from 00_theme.R)
theme_poster <- function(base_size = 12) {
  theme_void(base_size = base_size, base_family = FONT) +
    theme(plot.background = element_rect(fill = BG, colour = NA),
          text            = element_text(colour = INK),
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

# -------------------------------
#  Chart 1 – the breathing track
# -------------------------------
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
    low = driver_cols[[d2]], mid = NEUTRAL, high = driver_cols[[d1]], midpoint = 0,
    limits = c(-diff_limit, diff_limit),
    breaks = seq(-diff_limit, diff_limit, 5),
    # sizes without minus signs; the two ends also say who was faster
    labels = function(b) case_when(
      b == -diff_limit ~ paste0(diff_limit, "+\n", d2, " faster"),
      b ==  diff_limit ~ paste0(diff_limit, "+\n", d1, " faster"),
      TRUE             ~ as.character(abs(b))),
    name = "Difference in speed\nat each point (km/h)")
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

# Put each label in empty space next to its sector

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

lap_gap <- sum(sectors$delta)

p_track <- ggplot(track_closed, aes(x, y))
# light outline underneath
p_track <- p_track + geom_path(aes(linewidth = lw + 1.6), colour = OUTLINE, lineend = "round",
          show.legend = FALSE)
# coloured line: thickness = speed, colour = see colour_by above
p_track <- p_track + geom_path(aes(linewidth = lw, colour = colour_value, group = 1), lineend = "round")
p_track <- p_track + geom_point(data = borders, shape = 21, size = 3, fill = "white", colour = BG, stroke = 1)
p_track <- p_track + geom_text(data = sector_labels,
                               aes(lx, ly, label = label, colour = NULL),
                               colour = driver_cols[sector_labels$faster],
                               family = FONT, fontface = "bold", size = 4.4, lineheight = 0.95)
p_track <- p_track + scale_linewidth_identity(guide = "legend", name = "Thickness = speed (km/h)",
                                              breaks = lw_of(speed_breaks), labels = speed_breaks)
p_track <- p_track + colour_scale
# extra room on the right for the Sector 2 label
p_track <- p_track + scale_x_continuous(expand = expansion(mult = c(0.04, 0.22)))
p_track <- p_track + scale_y_continuous(expand = expansion(mult = 0.12))   # room for labels
p_track <- p_track + coord_fixed(clip = "off")                             # labels may go past the edge
# both legends on the LEFT: thickness key at the top (order 1),
# colour bar lower down (order 2), with a gap between them
p_track <- p_track + guides(linewidth = guide_legend(title.position = "top", order = 1,
                                        direction = "horizontal", nrow = 1,
                                        override.aes = list(colour = MUTED)),
                                        colour    = guide_colourbar(direction = "vertical", barheight = unit(7, "lines"),
                                                                    title.position = "top", order = 2))
p_track <- p_track + theme_poster()
p_track <- p_track + theme(legend.position      = "left",
                            legend.justification = "center",
                            legend.box           = "vertical",
                            legend.box.just      = "left",
                            legend.spacing.y     = unit(40, "pt"),     # gap between the two legends
                            legend.background    = element_blank(),
                            legend.title         = element_text(hjust = 0, colour = MUTED, face = "bold",
                                                                size = 9.6, lineheight = 1.05,
                                                                margin = margin(b = 10)),
                            legend.box.spacing   = unit(4, "pt")) 
p_track <- p_track + labs(title    = "Where was each driver faster?", subtitle = paste0(d1, " ahead by ", sprintf("%.3f", lap_gap), " s"))

p_track
save_png(p_track, "saturday_track.png", width = 9, height = 8)

# -------------------------------
#  Chart 2 – gearbox fingerprint
# -------------------------------
gears <- bind_rows(
  track_closed %>% transmute(x, y, driver = d1, gear = gear_1),
  track_closed %>% transmute(x, y, driver = d2, gear = gear_2)
) %>%
  mutate(gear   = factor(round(gear), levels = 1:8),
         driver = factor(driver, levels = c(d1, d2)))

p_gears <- ggplot(gears, aes(x, y))
p_gears <- p_gears + geom_path(colour = OUTLINE, linewidth = 4, lineend = "round")
p_gears <- p_gears + geom_path(aes(colour = gear, group = 1), linewidth = 2, lineend = "round")
p_gears <- p_gears + facet_wrap(~ driver)
# begin = 0.2 so gear 1 isn't too dark to see on the black background
p_gears <- p_gears + scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 1, drop = FALSE, name = "Gear")
p_gears <- p_gears + coord_fixed()
# gear legend standing on the LEFT, so the two maps can be bigger
p_gears <- p_gears + guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 3)))
p_gears <- p_gears + theme_poster()
p_gears <- p_gears + theme(strip.text = element_text(face = "bold", size = 13, family = FONT, colour = INK,
                                margin = margin(b = 4)),
                                legend.position      = "left",
                                legend.justification = "center",
                                legend.key.spacing.y = unit(3, "pt"),
                                panel.spacing        = unit(10, "pt"))
p_gears <- p_gears + labs(title = "How did they drive it?", subtitle = "Gear used around the lap")

p_gears
save_png(p_gears, "saturday_gears.png", width = 9, height = 4.5)
