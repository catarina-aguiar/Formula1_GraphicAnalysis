# ============================================================
#  04_poster.R  –  puts the four charts together into one image
#
#  Order: 00_theme.R -> 01_load_data.R -> 02_saturday.R -> 03_sunday.R
#  (this script runs whatever is missing automatically)
# ============================================================

# install.packages("patchwork")     # run once
library(dplyr)
library(ggplot2)
library(patchwork)

if (!exists("FONT"))        source("00_theme.R")
if (!exists("track"))       source("01_load_data.R")
if (!exists("p_track"))     source("02_saturday.R")
if (!exists("p_positions")) source("03_sunday.R")

# ---- Small red section headers ("kickers") ------------------
kicker <- function(text) {
  ggplot() +
    annotate("text", x = 0, y = 0.5, label = text, hjust = 0,
             family = FONT, fontface = "bold", colour = F1_RED, size = 6) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = BG, colour = NA),
          plot.margin     = margin(0, 12, 0, 12))      # same left edge as the charts
}

# ---- The two halves -----------------------------------------
saturday <- wrap_plots(kicker("SATURDAY  ·  QUALIFYING"), p_track, p_gears,
                       ncol = 1, heights = c(0.07, 1.25, 1.05))

# bottom rows are tall enough for the tyres / gear maps to fill their width
sunday   <- wrap_plots(kicker("SUNDAY  ·  RACE"), p_positions, p_tyres,
                       ncol = 1, heights = c(0.07, 1.25, 1.05))

# ---- Headline -----------------------------------------------
# The main question of the whole poster
main_question <- "Does the fastest lap on Saturday win the race on Sunday?"

headline <- plot_annotation(
  title    = main_question,
  subtitle = event,
  caption  = paste("Data: OpenF1 (qualifying telemetry, car positions, tyres)",
                   "and Jolpica via the f1dataR package (race results, positions, pit stops)"),
  theme = theme(
    plot.title      = element_text(family = FONT, face = "bold", colour = INK,
                                   size = 40, margin = margin(b = 4)),
    plot.subtitle   = element_text(family = FONT, colour = INK, size = 17,
                                   lineheight = 1.2, margin = margin(b = 14)),
    plot.caption    = element_text(family = FONT, colour = MUTED, size = 10),
    plot.background = element_rect(fill = BG, colour = NA),
    plot.margin     = margin(25, 25, 15, 25)
  )
)

# ---- Saturday | thin gap | Sunday ---------------------------
poster <- (saturday | plot_spacer() | sunday) +
  plot_layout(widths = c(1, 0.03, 1)) +
  headline &
  theme(plot.background = element_rect(fill = BG, colour = NA))

save_png(poster, "poster.png", width = 20, height = 12.5)
message("Poster saved as poster.png - open the file to check it (the RStudio preview is too small).")
