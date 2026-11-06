# ============================================================
#  00_theme.R  –  shared template for every chart (F1 TV style)
#
#  All colours and the font are defined here ONCE, so the whole
#  poster changes in one place.
# ============================================================

# install.packages("showtext")     # run once
library(showtext)

# ---- Font ---------------------------------------------------
# Titillium Web: free font, very close to the F1 look (the official
# F1 font is not free). It is downloaded by code from Google Fonts,
# so it works on any computer without installing anything.
# If there is no internet, we fall back to the standard sans font.
FONT <- "titillium"
font_ok <- tryCatch({
  font_add_google("Titillium Web", FONT)
  TRUE
}, error = function(e) FALSE)
if (!font_ok) {
  FONT <- "sans"
  message("Titillium Web could not be downloaded - using the default font.")
}
showtext_auto()

# ---- Colours (F1 TV style: dark background) -----------------
BG      <- "#15151E"   # F1 near-black background
PANEL   <- "#1F1F2B"   # slightly lighter, for boxes
INK     <- "#FFFFFF"   # main text
MUTED   <- "#A3A3B0"   # secondary text, axis labels
GRID    <- "#2C2C3A"   # grid lines
OUTLINE <- "#34343F"   # light outline under the track lines
OTHERS  <- "#4A4A58"   # lines of all the other drivers
F1_RED  <- "#E10600"   # F1 brand red, for headlines and accents
NEUTRAL <- "#4E4E5C"   # middle of the speed-difference gradient

# ---- Saving PNGs with the right text size -------------------
# showtext needs to know the resolution when saving at 300 dpi,
# otherwise the text comes out tiny. Use save_png() instead of ggsave().
save_png <- function(plot, file, width, height, dpi = 300) {
  showtext_opts(dpi = dpi)
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = BG)
  showtext_opts(dpi = 96)                 # back to normal for the RStudio preview
  invisible(file)
}
showtext_opts(dpi = 96)

