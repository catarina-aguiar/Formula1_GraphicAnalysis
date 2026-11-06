# ============================================================
#  03_sunday.R  –  Race charts (reads data/ only)
#
#  Chart 1: position on every lap – everyone grey, the two drivers
#           highlighted, pit stops coloured by the tyre fitted
#  Chart 2: "their races, as tyres" – each driver's strategy as a
#           tyre ring, places gained/lost in the centre
#
#  Uses the data loaded by 01_load_data.R (runs it if needed).
#  Tip: after the data is loaded, you can re-run just this script
#  to tweak the charts – nothing is downloaded again.
# ============================================================

library(dplyr)
library(ggplot2)

if (!exists("FONT"))      source("00_theme.R")        # colours + font template
if (!exists("positions")) source("01_load_data.R")    # load the data once
focus <- c(d1, d2)
driver_cols <- setNames(drivers$colour, drivers$code)
n_laps <- max(positions$lap)
n_drivers <- n_distinct(positions$driver_id)

# official Pirelli compound colours
tyre_cols <- c(SOFT = "#DA291C", MEDIUM = "#FFD12E", HARD = "#FFFFFF",
               INTERMEDIATE = "#43B02A", WET = "#0067AD", UNKNOWN = "#BDBDBD")

# ---- Shared style (colours and font come from 00_theme.R) ----
theme_poster <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = FONT) +
    theme(plot.background  = element_rect(fill = BG, colour = NA),
          text             = element_text(colour = INK),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.4),
          axis.text        = element_text(colour = MUTED),
          axis.title       = element_text(colour = MUTED, size = base_size * 0.8),
          plot.title       = element_text(face = "bold", colour = INK, size = base_size * 1.5),
          plot.subtitle    = element_text(colour = MUTED, margin = margin(t = 3, b = 8)),
          legend.position  = "bottom",
          legend.title     = element_text(colour = MUTED, size = base_size * 0.8, face = "bold"),
          legend.text      = element_text(colour = MUTED, size = base_size * 0.75),
          plot.margin      = margin(12, 12, 12, 12))
}

# ============================================================
#  Chart 1 – positions, lap by lap
# ============================================================
positions <- positions %>%
  mutate(group = if_else(code %in% focus, code, "other"))

# Driver codes at both ends of every line:
#   left  = starting grid order, right = where each line ends
#   (finishing position, or the lap where a driver retired)
start_labels <- positions %>% filter(lap == 0)
end_labels   <- positions %>%
  group_by(driver_id) %>% slice_max(lap, n = 1) %>% ungroup() %>%
  mutate(label = if_else(group != "other", paste0(code, "  P", position), code))

p_positions <- ggplot(positions, aes(lap, position, group = driver_id)) +
  geom_line(data = filter(positions, group == "other"),
            colour = OTHERS, linewidth = 0.45) +
  geom_line(data = filter(positions, group != "other"),
            aes(colour = group), linewidth = 1.5, lineend = "round") +
  # pit stops: white dot with a ring in team colour
  geom_point(data = pit_stops, aes(lap, position, colour = code),
             inherit.aes = FALSE, shape = 21, fill = "white", size = 3.2, stroke = 1.4) +
  # everyone else: small grey codes
  geom_text(data = filter(start_labels, group == "other"),
            aes(x = -1, label = code), colour = MUTED,
            hjust = 1, family = FONT, size = 2.7) +
  geom_text(data = filter(end_labels, group == "other"),
            aes(x = lap + 1, label = label), colour = MUTED,
            hjust = 0, family = FONT, size = 2.7) +
  # the two drivers: bold, team colours
  geom_text(data = filter(start_labels, group != "other"),
            aes(x = -1, label = code, colour = code),
            hjust = 1, family = FONT, fontface = "bold", size = 3.2) +
  geom_text(data = filter(end_labels, group != "other"),
            aes(x = lap + 1, label = label, colour = code),
            hjust = 0, family = FONT, fontface = "bold", size = 3.6) +
  # ---- story notes: write your own! (x = lap, y = position) ----
  # annotate("text", x = 8, y = 15, label = "lap 1: ...", family = FONT,
  #          fontface = "italic", size = 3.3, hjust = 0, colour = INK) +
  # annotate("segment", x = 7.8, xend = 3, y = 14.6, yend = 11, colour = INK, linewidth = 0.3) +
  scale_colour_manual(values = driver_cols, guide = "none") +
  scale_y_reverse(breaks = NULL) +        # no P1/P5 axis: the driver codes do the job
  scale_x_continuous(breaks = seq(0, n_laps, 10)) +
  coord_cartesian(xlim = c(-4, n_laps + 8), ylim = c(n_drivers + 0.5, 0.5), clip = "off") +
  theme_poster() +
  theme(legend.position = "none") +
  labs(title    = "Did qualifying matter?",
       subtitle = paste0(event, " · position on every lap (lap 0 = starting grid) · dots = pit stops"),
       x = "Lap", y = NULL)

p_positions
save_png(p_positions, "sunday_positions.png", width = 10, height = 6.5)

# ============================================================
#  Chart 2 – their races, as tyres
# ============================================================
# Each driver = a tyre seen from the side. The coloured stripe on the
# sidewall shows the compound used, going clockwise from the start
# (12 o'clock) to the end of the race. Gaps in the stripe = pit stops.
race_laps <- max(stints$lap_end)
gap <- 0.25                                  # small gap between stints (in laps)

tyres <- stints %>%
  mutate(code  = factor(code, levels = focus),
         start = lap_start - 1 + gap, end = lap_end - gap)

# one row per driver (used to draw the wheel behind the stripe)
centre <- tibble(code = factor(focus, levels = focus), laps = race_laps)


# the parts of the wheel, from the outside in (y = distance from centre)
RUBBER <- c(2.30, 3.50)   # black tyre
EDGE   <- c(3.50, 3.58)   # thin grey edge, so the black tyre shows on the dark background
STRIPE <- c(2.80, 3.05)   # compound stripe on the sidewall
RIM    <- c(1.35, 2.30)   # grey wheel rim
HUB    <- c(0.00, 1.35)   # light centre with the driver's name

p_tyres <- ggplot(tyres) +
  # black rubber with a thin grey edge
  geom_rect(data = centre, aes(xmin = 0, xmax = laps, ymin = EDGE[1], ymax = EDGE[2]),
            fill = "#5A5A6A", inherit.aes = FALSE) +
  geom_rect(data = centre, aes(xmin = 0, xmax = laps, ymin = RUBBER[1], ymax = RUBBER[2]),
            fill = "#0B0B0F", inherit.aes = FALSE) +
  # grey rim and light hub
  geom_rect(data = centre, aes(xmin = 0, xmax = laps, ymin = RIM[1], ymax = RIM[2]),
            fill = "#BFBFBF", inherit.aes = FALSE) +
  geom_rect(data = centre, aes(xmin = 0, xmax = laps, ymin = HUB[1], ymax = HUB[2]),
            fill = "#F4F1EC", inherit.aes = FALSE) +
  # driver's name in the middle, in team colour
  geom_text(data = centre, aes(x = 0, y = 0, label = code),
            colour = driver_cols[as.character(centre$code)],
            family = FONT, fontface = "bold", size = 7) +
  # compound stripe, one piece per stint
  geom_rect(aes(xmin = start, xmax = end, ymin = STRIPE[1], ymax = STRIPE[2], fill = compound)) +
  # start mark at 12 o'clock
  annotate("segment", x = 0, xend = 0, y = EDGE[2], yend = EDGE[2] + 0.25,
           colour = INK, linewidth = 0.6) +
  annotate("text", x = 0, y = EDGE[2] + 0.45, label = "START", family = FONT,
           size = 2.8, colour = MUTED, fontface = "bold") +
  facet_wrap(~ code) +
  scale_fill_manual(values = tyre_cols,
                    breaks = intersect(names(tyre_cols), unique(tyres$compound)),
                    labels = function(x) tools::toTitleCase(tolower(x)),
                    name = "Tyre") +
  scale_x_continuous(limits = c(0, race_laps)) +
  scale_y_continuous(limits = c(0, EDGE[2] + 0.55)) +
  coord_polar(theta = "x", start = 0, direction = 1) +   # 12 o'clock, clockwise
  guides(fill = guide_legend(override.aes = list(colour = BG, linewidth = 0.6))) +
  theme_void(base_family = FONT) +
  theme(plot.background = element_rect(fill = BG, colour = NA),
        strip.text      = element_blank(),     # names are in the middle of the tyres
        plot.title      = element_text(face = "bold", colour = INK, size = 16),
        plot.subtitle   = element_text(colour = MUTED, margin = margin(t = 3, b = 8)),
        legend.position = "right",               # beside the tyres, saves height
        legend.title    = element_text(colour = MUTED, face = "bold"),
        legend.text     = element_text(colour = MUTED),
        legend.key.spacing.y = unit(4, "pt"),
        plot.margin     = margin(12, 12, 12, 12)) +
  labs(title    = "Their races, as tyres",
       subtitle = "Coloured stripe = tyres used, clockwise from the start · gaps = pit stops")

p_tyres
save_png(p_tyres, "sunday_tyres.png", width = 8, height = 4.8)
