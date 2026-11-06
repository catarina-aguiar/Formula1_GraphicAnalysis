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

if (!exists("positions")) source("01_load_data.R")    # load the data once
focus <- c(d1, d2)
driver_cols <- setNames(drivers$colour, drivers$code)
n_laps <- max(positions$lap)
n_drivers <- n_distinct(positions$driver_id)

# official Pirelli compound colours
tyre_cols <- c(SOFT = "#DA291C", MEDIUM = "#FFD12E", HARD = "#FFFFFF",
               INTERMEDIATE = "#43B02A", WET = "#0067AD", UNKNOWN = "#BDBDBD")

# ---- Shared style -------------------------------------------
BG <- "#FBF8F2"; INK <- "#2A2420"; MUTED <- "#7A6F66"; GRID <- "#E3DBCD"
theme_poster <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "serif") +
    theme(plot.background  = element_rect(fill = BG, colour = NA),
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
            colour = "#D8D0C3", linewidth = 0.45) +
  geom_line(data = filter(positions, group != "other"),
            aes(colour = group), linewidth = 1.5, lineend = "round") +
  # pit stops: white dot with a ring in team colour
  geom_point(data = pit_stops, aes(lap, position, colour = code),
             inherit.aes = FALSE, shape = 21, fill = "white", size = 3.2, stroke = 1.4) +
  # everyone else: small grey codes
  geom_text(data = filter(start_labels, group == "other"),
            aes(x = -1, label = code), colour = MUTED,
            hjust = 1, family = "serif", size = 2.7) +
  geom_text(data = filter(end_labels, group == "other"),
            aes(x = lap + 1, label = label), colour = MUTED,
            hjust = 0, family = "serif", size = 2.7) +
  # the two drivers: bold, team colours
  geom_text(data = filter(start_labels, group != "other"),
            aes(x = -1, label = code, colour = code),
            hjust = 1, family = "serif", fontface = "bold", size = 3.2) +
  geom_text(data = filter(end_labels, group != "other"),
            aes(x = lap + 1, label = label, colour = code),
            hjust = 0, family = "serif", fontface = "bold", size = 3.6) +
  # ---- story notes: write your own! (x = lap, y = position) ----
  # annotate("text", x = 8, y = 15, label = "lap 1: ...", family = "serif",
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
ggsave("sunday_positions.png", p_positions, width = 10, height = 6.5, dpi = 300, bg = BG)

# ============================================================
#  Chart 2 – their races, as tyres
# ============================================================
# Each driver = a ring around a clock face: lap 0 at 12 o'clock,
# clockwise to the last lap; each stint coloured by its compound.
tyres <- stints %>%
  mutate(code  = factor(code, levels = focus),
         start = lap_start - 1, end = lap_end)

centre <- race_results %>%
  filter(code %in% focus) %>%
  mutate(code   = factor(code, levels = focus),
         gained = grid - finish,
         label  = if_else(gained == 0, "=", sprintf("%+d", gained)),
         col    = case_when(gained > 0 ~ "#2E8B57", gained < 0 ~ "#C0392B", TRUE ~ MUTED),
         strip  = paste0(code, "  ·  P", grid, " → P", finish))
strip_names <- setNames(centre$strip, as.character(centre$code))

p_tyres <- ggplot(tyres) +
  # outer ring in team colour
  geom_rect(data = centre, aes(xmin = 0, xmax = n_laps, ymin = 3.25, ymax = 3.55, fill = code),
            inherit.aes = FALSE) +
  # the tyre: one slice per stint
  geom_rect(aes(xmin = start, xmax = end, ymin = 2, ymax = 3.1, fill = compound),
            colour = INK, linewidth = 0.3) +
  # places gained / lost in the middle
  geom_text(data = centre, aes(x = 0, y = 0, label = label),
            colour = centre$col, family = "serif", fontface = "bold", size = 9) +
  facet_wrap(~ code, labeller = as_labeller(strip_names)) +
  scale_fill_manual(values = c(tyre_cols, driver_cols),
                    breaks = intersect(names(tyre_cols), unique(tyres$compound)),
                    name = "Tyre") +
  scale_x_continuous(limits = c(0, n_laps)) +
  scale_y_continuous(limits = c(0, 3.6)) +
  coord_polar(theta = "x", start = 0, direction = 1) +   # 12 o'clock, clockwise
  theme_void(base_family = "serif") +
  theme(plot.background = element_rect(fill = BG, colour = NA),
        strip.text      = element_text(face = "bold", size = 13, margin = margin(b = 6)),
        plot.title      = element_text(face = "bold", colour = INK, size = 16),
        plot.subtitle   = element_text(colour = MUTED, margin = margin(t = 3, b = 8)),
        legend.position = "bottom",
        legend.title    = element_text(colour = MUTED, face = "bold"),
        plot.margin     = margin(12, 12, 12, 12)) +
  labs(title    = "Their races, as tyres",
       subtitle = "Ring = tyres used, clockwise from the start · centre = places gained or lost")

p_tyres
ggsave("sunday_tyres.png", p_tyres, width = 8, height = 4.8, dpi = 300, bg = BG)
