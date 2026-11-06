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
    .btn-primary { background:", F1_RED, "; border: none; font-weight: 700; }
  ")))),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Titillium+Web:wght@400;700&display=swap"),

  titlePanel("Does the fastest lap on Saturday win the race on Sunday?"),

  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("year", "Year", choices = rev(2023:year(Sys.Date())), selected = 2026),
      selectInput("race", "Race", choices = NULL),
      selectInput("d1", "Driver 1", choices = NULL),
      selectInput("d2", "Driver 2", choices = NULL),
      actionButton("build", "Build", class = "btn-primary", width = "100%"),
      tags$p(style = paste0("color:", MUTED, "; margin-top: 12px; font-size: 12px;"),
             "Loading a new race takes about a minute: the car telemetry is downloaded",
             "from OpenF1 each time.")
    ),
    mainPanel(width = 9,
      h3("SATURDAY · QUALIFYING", style = paste0("color:", F1_RED, ";")),
      fluidRow(column(7, plotOutput("p_track", height = "520px")),
               column(5, plotOutput("p_gears", height = "520px"))),
      h3("SUNDAY · RACE", style = paste0("color:", F1_RED, ";")),
      fluidRow(column(7, plotOutput("p_positions", height = "520px")),
               column(5, plotOutput("p_tyres", height = "520px")))
    )
  )
)

# ---- What happens when you click ----------------------------
server <- function(input, output, session) {

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

  plots <- reactiveVal(NULL)

  observeEvent(input$build, {
    req(input$race, input$d1, input$d2)
    if (input$d1 == input$d2) {
      showNotification("Pick two different drivers.", type = "warning"); return()
    }
    r <- races() %>% filter(race_key == input$race)
    d <- race_drivers() %>% filter(code %in% c(input$d1, input$d2)) %>%
      arrange(match(code, c(input$d1, input$d2)))

    # teammates have the same team colour: make the second one lighter
    if (d$colour[1] == d$colour[2]) {
      d$colour[2] <- colorRampPalette(c(d$colour[2], "white"))(3)[2]
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
    if (!is.null(result)) plots(result)
  })

  output$p_track     <- renderPlot({ req(plots()); plots()$track },     res = 96, bg = BG)
  output$p_gears     <- renderPlot({ req(plots()); plots()$gears },     res = 96, bg = BG)
  output$p_positions <- renderPlot({ req(plots()); plots()$positions }, res = 96, bg = BG)
  output$p_tyres     <- renderPlot({ req(plots()); plots()$tyres },     res = 96, bg = BG)
}

runApp(shinyApp(ui, server))
