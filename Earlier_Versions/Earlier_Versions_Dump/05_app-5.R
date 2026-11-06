# ============================================================
#  05_app.R  –  the dynamic version (Shiny app)
#
#  Pick a year, a race and two drivers, press "Build", and the
#  same four charts are made for that choice.
#
#  How to run: open this file in RStudio and press "Run App"
#  (or run the whole script). Needs internet. Works for races
#  from 2023 onwards (OpenF1 coverage).
# ============================================================

# install.packages("shiny")     # run once
library(shiny)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)

APP_MODE <- TRUE                 # tells the other scripts we're inside the app
source("00_theme.R")             # colours + font

base <- "https://api.openf1.org/v1/"

# ---- Helpers: what can be chosen ----------------------------
# All races of a year that already happened, with their Saturday
# qualifying and Sunday race dates
races_for_year <- function(year) {
  race  <- fromJSON(paste0(base, "sessions?year=", year, "&session_name=Race"))
  quali <- fromJSON(paste0(base, "sessions?year=", year, "&session_name=Qualifying"))
  meet  <- fromJSON(paste0(base, "meetings?year=", year))
  race %>%
    transmute(meeting_key, race_key = session_key,
              race_day = as.Date(ymd_hms(date_start))) %>%
    inner_join(quali %>% transmute(meeting_key, quali_day = as.Date(ymd_hms(date_start))),
               by = "meeting_key") %>%
    left_join(meet %>% transmute(meeting_key, meeting_name), by = "meeting_key") %>%
    filter(race_day < Sys.Date()) %>%
    arrange(race_day)
}

# The drivers who took part in a race (code, number, name, team colour)
drivers_for_race <- function(race_key) {
  fromJSON(paste0(base, "drivers?session_key=", race_key)) %>%
    transmute(code = name_acronym, number = driver_number,
              name = full_name, colour = paste0("#", team_colour)) %>%
    distinct(code, .keep_all = TRUE) %>%
    arrange(code)
}

# ---- Layout --------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(paste0("
    body { background:", BG, "; color:", INK, "; font-family: 'Titillium Web', sans-serif; }
    .well { background:", PANEL, "; border: none; color:", INK, "; }
    h1 { color:", F1_RED, "; font-weight: 700; }
    .control-label { color:", MUTED, "; }
    .btn-primary, .btn-primary:hover, .btn-primary:focus {
      background:", F1_RED, " !important; border: none !important;
      color: white !important; font-weight: 700; }
    .selectize-input, .selectize-dropdown { background:", PANEL, " !important;
                                            color:", INK, " !important; border-color:", GRID, "; }
    .selectize-input input { color:", INK, " !important; }
  ")))),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Titillium+Web:wght@400;700&display=swap"),

  # ---- top row: title on the left, controls on the right ----
  fluidRow(style = "margin-top: 18px;",
    column(5,
      h1(MAIN_TITLE,                                   # set in 00_theme.R
         style = paste0("color:", INK, "; font-size: 30px; margin-top: 0;")),
      textOutput("subtitle", container = function(...) tags$p(
        style = paste0("color:", MUTED, "; font-size: 18px; margin-top: 2px;"), ...))
    ),
    column(7,
      fluidRow(
        column(2, selectInput("year", "Year", choices = rev(2023:year(Sys.Date())),
                              selected = 2026)),
        column(4, selectInput("race", "Race", choices = NULL)),
        column(2, selectInput("d1", "Driver 1", choices = NULL)),
        column(2, selectInput("d2", "Driver 2", choices = NULL)),
        column(2, actionButton("build", "Build", class = "btn-primary",
                               width = "100%", style = "margin-top: 25px;"))
      ),
      tags$p(style = paste0("color:", MUTED, "; font-size: 12px; text-align: right;"),
             "Loading a new race takes about a minute: the car telemetry is downloaded",
             "from OpenF1 each time.")
    )
  ),

  # ---- the charts: Saturday left, Sunday right (like the poster) ----
  fluidRow(
    column(6,
      h3("SATURDAY · QUALIFYING", style = paste0("color:", F1_RED, "; font-weight: 700;")),
      plotOutput("p_track", height = "560px"),
      plotOutput("p_gears", height = "420px")
    ),
    column(6,
      h3("SUNDAY · RACE", style = paste0("color:", F1_RED, "; font-weight: 700;")),
      plotOutput("p_positions", height = "560px"),
      plotOutput("p_tyres", height = "420px")
    )
  ),

  # ---- caption: data sources + student numbers ----
  tags$footer(style = paste0("color:", MUTED, "; font-size: 12px; text-align: right;",
                             "margin: 20px 0 30px 0; line-height: 1.6;"),
    "Data: OpenF1 (qualifying telemetry, car positions, tyres) and Jolpica via the",
    "f1dataR package (race results, positions, pit stops)", tags$br(), STUDENTS)
)

# ---- What happens when you click ----------------------------
server <- function(input, output, session) {

  # ---- Screen resolution ------------------------------------
  # On high-resolution (Retina) screens Shiny draws the charts at 2x.
  # The font package must know this BEFORE any chart is drawn,
  # otherwise the text comes out half size. Set it as soon as the
  # app learns about the screen.
  observe({
    ratio <- session$clientData$pixelratio
    if (is.null(ratio)) ratio <- 1
    showtext_opts(dpi = 96 * ratio)
  })

  races <- reactive({
    tryCatch(races_for_year(input$year), error = function(e) NULL)
  })

  # fill the race list when the year changes
  observeEvent(races(), {
    r <- races()
    if (is.null(r) || nrow(r) == 0) {
      updateSelectInput(session, "race", choices = character(0))
      return()
    }
    choices <- setNames(r$race_key, r$meeting_name)
    # default: Sepang 2026 if it's in the list, otherwise the latest race
    sel <- r$race_key[r$race_day == as.Date("2026-10-04")]
    updateSelectInput(session, "race", choices = choices,
                      selected = if (length(sel)) sel else tail(r$race_key, 1))
  })

  race_drivers <- reactive({
    req(input$race)
    tryCatch(drivers_for_race(input$race), error = function(e) NULL)
  })

  # fill the two driver lists when the race changes
  observeEvent(race_drivers(), {
    d <- race_drivers(); req(d)
    choices <- setNames(d$code, paste0(d$code, " – ", d$name))
    updateSelectInput(session, "d1", choices = choices,
                      selected = if ("VER" %in% d$code) "VER" else d$code[1])
    updateSelectInput(session, "d2", choices = choices,
                      selected = if ("HAM" %in% d$code) "HAM" else d$code[2])
  })

  plots   <- reactiveVal(NULL)
  showing <- reactiveVal("Pick a race and two drivers, then press Build")

  # subtitle: the race currently on screen
  output$subtitle <- renderText(showing())

  observeEvent(input$build, {
    req(input$race, input$d1, input$d2)
    if (input$d1 == input$d2) {
      showNotification("Pick two different drivers.", type = "warning"); return()
    }
    r <- races() %>% filter(race_key == input$race)
    d <- race_drivers() %>% filter(code %in% c(input$d1, input$d2)) %>%
      arrange(match(code, c(input$d1, input$d2)))

    # Teammates have the same team colour. Give the second driver the
    # OPPOSITE colour on the colour wheel (complementary), so the
    # "who was faster" gradient still goes between two clearly different
    # colours (e.g. Red Bull blue -> orange, Ferrari red -> turquoise).
    if (toupper(d$colour[1]) == toupper(d$colour[2])) {
      hsv_col <- rgb2hsv(col2rgb(d$colour[1]))
      d$colour[2] <- hsv((hsv_col["h", 1] + 0.5) %% 1,
                         max(hsv_col["s", 1], 0.6),          # keep it vivid
                         max(hsv_col["v", 1], 0.8))          # and bright enough on black
    }

    withProgress(message = "Building the charts...", value = 0.1, {
      result <- tryCatch({
        # the settings that 01_load_data.R normally has at the top
        assign("season",    as.numeric(input$year), envir = globalenv())
        assign("quali_day", r$quali_day,            envir = globalenv())
        assign("race_day",  r$race_day,             envir = globalenv())
        assign("event",     paste(r$meeting_name, input$year), envir = globalenv())
        assign("drivers",   tibble(code = d$code, number = d$number, colour = d$colour),
               envir = globalenv())

        incProgress(0.2, detail = "downloading data (this is the slow part)")
        source("01_load_data.R", local = globalenv())
        incProgress(0.5, detail = "drawing Saturday")
        source("02_saturday.R", local = globalenv())
        incProgress(0.1, detail = "drawing Sunday")
        source("03_sunday.R", local = globalenv())

        list(track = p_track, gears = p_gears,
             positions = p_positions, tyres = p_tyres)
      }, error = function(e) {
        showNotification(paste("Could not build this one:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      })
    })
    if (!is.null(result)) {
      plots(result)
      showing(paste(r$meeting_name, input$year))   # e.g. "Bahrain Grand Prix 2026"
    }
  })

  # Draw a chart. The resolution for the font is set once at the start
  # (see "Screen resolution" at the top of the server).
  show_chart <- function(name) {
    renderPlot({ req(plots()); plots()[[name]] }, res = 96, bg = BG)
  }
  output$p_track     <- show_chart("track")
  output$p_gears     <- show_chart("gears")
  output$p_positions <- show_chart("positions")
  output$p_tyres     <- show_chart("tyres")
}

runApp(shinyApp(ui, server))
