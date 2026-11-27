# Formula1_GraphicAnalysis
## Final grade 20 (19,5)
Comparing Driver's Performance During the Race Weekend in Formula 1

COMPARING DRIVERS' PERFORMANCE DURING A RACE WEEKEND
Formula 1 Bahrain Grand Prix in Malaysia (Sepang) 2026 - Verstappen vs Hamilton

## FILES

00_theme.R       Shared style: colours, font (Titillium Web), title, caption
01_load_data.R   Downloads and prepares all the data (OpenF1 + Jolpica/f1dataR)
02_saturday.R    Qualifying charts: speed map of the track + gearbox maps
03_sunday.R      Race charts: positions lap by lap + tyre strategies
04_poster.R      Puts the four charts together and saves poster.png
05_app.R         Interactive Shiny app: same charts for any race and drivers

The scripts call each other by these exact names, so please keep them
together in one folder and don't rename them.


## HOW TO RUN

1. Open RStudio and set the working directory to this folder
   (Session > Set Working Directory > To Source File Location).

2. Install the packages (only the first time):

   install.packages(c("f1dataR", "jsonlite", "lubridate", "dplyr", "stringr",
                      "tidyr", "ggplot2", "patchwork", "showtext", "shiny",
                      "jpeg", "png"))

3. POSTER: open 04_poster.R and run the whole script.
   It runs 00 -> 01 -> 02 -> 03 automatically, then saves poster.png.
   Open the PNG file to see the poster (the RStudio preview is too small).

4. APP: open 05_app.R and run it.
   Choose a year, a race and two drivers, then press "Build".


## GOOD TO KNOW

- An internet connection is needed: the data is downloaded from the
  OpenF1 and Jolpica APIs, and the font from Google Fonts.
- OpenF1 blocks free access while an F1 session is live (error
  "401 Unauthorized"). If this happens, run the scripts again after
  the session ends.
- Loading a race takes about a minute (the car telemetry is downloaded).
- The app works for races from 2023 onwards (OpenF1 coverage).
- Restart R (Session > Restart R) between running the app and the poster.
- Optional: a photo called background.jpg in this folder is used as a
  darkened background for the poster.
- Two data corrections are made for Sepang 2026, explained in 01_load_data.R
  and in the report: extra laps at the start of the lap-by-lap data are
  removed, and the tyre strategies come from the official race reports.
