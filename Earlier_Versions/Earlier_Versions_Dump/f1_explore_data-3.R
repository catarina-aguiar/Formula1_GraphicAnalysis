# ============================================================
#  F1 project – exploring what each f1dataR function gives you
#  Run it section by section (Ctrl/Cmd + Enter).
#  For each table:  glimpse() = column names, types, first values
#                   View()    = the full table in RStudio
# ============================================================

# Use the Python environment "f1env" that has FastF1 installed.
# Must be the FIRST line after a fresh R start (Session > Restart R).
# One-time setup, if f1env doesn't exist yet:
#   reticulate::conda_create("f1env", python_version = "3.11")
#   reticulate::conda_install("f1env", "fastf1", pip = TRUE)
reticulate::use_condaenv("f1env", required = TRUE)

library(f1dataR)
library(dplyr)

get_fastf1_version()   # check: should print a version number, e.g. 3.x.x

# ---- 0. Setup (run once per session) ------------------------
# Save downloads to a folder on your computer instead of memory.
# Needed for the telemetry functions (part B), and it means
# you don't re-download the same race every time.
dir.create("f1_cache", showWarnings = FALSE)
options(f1dataR.cache = file.path(getwd(), "f1_cache"))
getOption("f1dataR.cache")   # check: should print a folder path

# The race we'll explore. Change these to look at any other race.
season <- 2025
rnd    <- 1            # round number (1 = Australian GP 2025)
driver <- "NOR"        # 3-letter driver code, for telemetry

# ============================================================
#  PART A – Jolpica data (fast, light, history back to 1950)
# ============================================================

# ---- A1. The calendar ---------------------------------------
schedule <- load_schedule(season)
glimpse(schedule)
# Look for: round number, race name, circuit, date.
# Use this to find the round number of any race you want.

# ---- A2. Race results ---------------------------------------
results <- load_results(season, rnd)
glimpse(results)
# One row per driver: finishing position, grid (start) position,
# points, laps completed, status (Finished / Accident / Engine...),
# fastest lap, top speed.

# ---- A3. Qualifying -----------------------------------------
quali <- load_quali(season, rnd)
glimpse(quali)
# One row per driver: qualifying position and Q1/Q2/Q3 times.

# ---- A4. Lap-by-lap positions -------------------------------
laps <- load_laps(season, rnd)
glimpse(laps)
# One row per driver PER LAP: position on that lap and lap time.
# -> This is the data for your "positions during the race" chart.

# ---- A5. Pit stops ------------------------------------------
pits <- load_pitstops(season, rnd)
glimpse(pits)
# One row per stop: driver, which lap, stop number, duration.

# ---- A6. Championship standings -----------------------------
drv_standings  <- load_standings(season, rnd, type = "driver")
team_standings <- load_standings(season, rnd, type = "constructor")
glimpse(drv_standings)
glimpse(team_standings)
# The championship table AFTER this round.

# ---- A7. Reference tables -----------------------------------
drivers  <- load_drivers(season)
circuits <- load_circuits(season)
glimpse(drivers)    # names, codes, nationality, date of birth
glimpse(circuits)   # circuit names, city, country, latitude/longitude

# ============================================================
#  PART B – FastF1 data (2018+, big, detailed)
#  The FIRST time you load a race it can take a minute or two.
#  If you get an error about Python or "fastf1":
#  Session > Restart R, then run the script from the very top
#  (use_condaenv must run before anything else touches Python).
# ============================================================

# ---- B1. Every lap, with sectors and tyres ------------------
session_laps <- load_session_laps(season, rnd, session = "R",
                                  add_weather = TRUE)
glimpse(session_laps)
# One row per driver per lap, with a LOT of columns:
# lap time, sector 1/2/3 times, speed traps, tyre compound,
# tyre age (tyre_life), stint, pit in/out, and weather
# (air & track temperature, rainfall...).

# ---- B2. Telemetry: the car itself --------------------------
tel <- load_driver_telemetry(season, rnd, session = "Q",
                             driver = driver, laps = "fastest")
glimpse(tel)
# Many rows for ONE lap (several readings per second):
# speed, rpm, gear (n_gear), throttle %, brake, DRS,
# distance along the lap, and x / y / z position on track.
# -> This is the data for the driver comparison and track maps.

# ---- B3. Circuit details ------------------------------------
circuit_info <- load_circuit_details(season, rnd)
str(circuit_info)
# A list with corner numbers + positions, marshal lights and
# marshal sectors. Useful for labelling corners on a track map.

# ---- Quick look at the telemetry as a track map -------------
# (a preview of the "circuit DNA" idea)
plot_fastest(season, rnd, session = "Q", driver = driver, color = "speed")
