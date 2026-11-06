# ============================================================
#  04_poster.R  –  puts the four charts together into one image
#
#  Order: 00_theme.R -> 01_load_data.R -> 02_saturday.R -> 03_sunday.R
#  (this script runs whatever is missing automatically)
# ============================================================

# install.packages(c("patchwork", "jpeg", "png"))     # run once
library(dplyr)
library(ggplot2)
library(patchwork)

# ---- Background photo (optional) ----------------------------
# Put your own photo in this folder and write its file name here
# (.jpg or .png). Use NULL for the plain dark background.
background_photo <- "background.jpg"
photo_visibility <- 0.15     # 0 = invisible, 1 = full photo. Keep it low so
                             # the charts stay readable (0.10 - 0.20 works well)
has_photo <- !is.null(background_photo) && file.exists(background_photo)
if (!is.null(background_photo) && !has_photo)
  message("Background photo '", background_photo, "' not found - using the plain background.")
plot_bg <- if (has_photo) "transparent" else BG   # charts see-through if there's a photo

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
main_question <- MAIN_TITLE        # set in 00_theme.R

headline <- plot_annotation(
  title    = main_question,
  subtitle = event,
  caption  = paste0("Data: OpenF1 (qualifying telemetry, car positions, tyres) ",
                    "and Jolpica via the f1dataR package (race results, positions, pit stops)\n",
                    STUDENTS),
  theme = theme(
    plot.title      = element_text(family = FONT, face = "bold", colour = INK,
                                   size = 40, margin = margin(b = 4)),
    plot.subtitle   = element_text(family = FONT, colour = INK, size = 17,
                                   lineheight = 1.2, margin = margin(b = 14)),
    plot.caption    = element_text(family = FONT, colour = MUTED, size = 10, lineheight = 1.3),
    plot.background = element_rect(fill = plot_bg, colour = NA),
    plot.margin     = margin(25, 25, 15, 25)
  )
)

# ---- Saturday | thin gap | Sunday ---------------------------
poster <- (saturday | plot_spacer() | sunday) +
  plot_layout(widths = c(1, 0.03, 1)) +
  headline &
  theme(plot.background = element_rect(fill = plot_bg, colour = NA))

# ---- Save (photo underneath, darkened, then the poster on top) ----
save_poster <- function(poster, file, width = 20, height = 12.5, dpi = 300) {
  showtext_opts(dpi = dpi)
  png(file, width = width, height = height, units = "in", res = dpi, bg = BG)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = BG, col = NA))          # dark base
  if (has_photo) {
    img <- if (grepl("\\.png$", tolower(background_photo))) {
      png::readPNG(background_photo)
    } else {
      jpeg::readJPEG(background_photo)
    }
    # fill the whole poster with the photo (cropped, not stretched)
    img_ratio    <- dim(img)[2] / dim(img)[1]
    poster_ratio <- width / height
    w <- if (img_ratio > poster_ratio) img_ratio / poster_ratio else 1
    h <- if (img_ratio > poster_ratio) 1 else poster_ratio / img_ratio
    grid::grid.raster(img, width = grid::unit(w, "npc"), height = grid::unit(h, "npc"),
                      interpolate = TRUE)
    # dark layer on top of the photo, so only a little of it shows through
    grid::grid.rect(gp = grid::gpar(fill = BG, col = NA, alpha = 1 - photo_visibility))
  }
  print(poster, newpage = FALSE)                                  # the charts on top
  dev.off()
  showtext_opts(dpi = 96)
  invisible(file)
}

save_poster(poster, "poster.png")
message("Poster saved as poster.png - open the file to check it (the RStudio preview is too small).")
