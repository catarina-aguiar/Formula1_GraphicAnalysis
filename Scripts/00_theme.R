# ============================================================
#  00_theme.R  –  shared template for every chart (F1 TV style)
#
#  All colours and the font are defined here ONCE, so the whole
#  poster changes in one place.
# ============================================================

# install.packages("showtext")
library(showtext)

# Font 
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

# Title of the poster and the app
MAIN_TITLE <- "Comparing Drivers Performances During the Race Weekend"

# Authors (shown as a caption on the poster and in the app)
STUDENTS <- paste("Student numbers: 202205932 · 202602767 · 202107858 · 202006475                 On:", Sys.Date())

# Colours
BG      <- "#15151E"   # background
PANEL   <- "#1F1F2B"   # for boxes
INK     <- "#FFFFFF"   # main text
MUTED   <- "#A3A3B0"   # secondary text, axis labels
GRID    <- "#2C2C3A"   # grid lines
OUTLINE <- "#34343F"   # light outline under the track lines
OTHERS  <- "#4A4A58"   # lines of all the other drivers
F1_RED  <- "#E10600"   # for headlines and accents
NEUTRAL <- "#4E4E5C"   # middle of the speed-difference gradient

# Saving PNGs with the right text size
save_png <- function(plot, file, width, height, dpi = 300) {
  if (exists("APP_MODE") && isTRUE(APP_MODE)) return(invisible(NULL)) 
  showtext_opts(dpi = dpi)
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = BG)
  showtext_opts(dpi = 96)                 # back to normal for the RStudio preview
  invisible(file)
}
showtext_opts(dpi = 96)

message("Done!")

