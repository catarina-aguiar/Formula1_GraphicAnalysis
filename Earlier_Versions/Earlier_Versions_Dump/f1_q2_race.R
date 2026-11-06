# ============================================================
#  Q2 – The race: positions lap by lap + tyre strategy
#  2026 Bahrain GP in Malaysia (Sepang), Sunday 4 October
#
#  DATA:
#   - f1dataR (Jolpica): grid, results, position on every lap,
#     pit stops  -> no Python needed
#   - OpenF1 "stints": tyre compound used in each stint
#
#  Chart 1: position chart – VER & HAM in colour, others grey,
#           pit stops marked
#  Chart 2: tyre strategy strip for VER & HAM (same lap axis)
#  Both stacked with patchwork -> the "Sunday" half of the poster
# ============================================================

# install.packages(c("f1dataR", "jsonlite", "lubridate", "patchwork"))  # once
library(f1dataR)
library(jsonlite)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ---- 1. Settings --------------------------------------------
season    <- 2026
race_day  <- as.Date("2026-10-04")
focus     <- tibble(code = c("VER", "HAM"),
                    driver_id = c("max_verstappen", "hamilton"),   # Jolpica ids
                    number = c(3, 44))                             # OpenF1 numbers
driver_cols <- c(VER = "#3671C6", HAM = "#E8002D")
# official Pirelli compound colours
tyre_cols <- c(SOFT = "#DA291C", MEDIUM = "#FFD12E", HARD = "#F0F0EC",
               INTERMEDIATE = "#43B02A", WET = "#0067AD", UNKNOWN = "#BDBDBD")
race_label <- "Bahrain GP in Malaysia (Sepang) 2026 – race"

# ---- 2. Find the round number from the calendar -------------
schedule <- load_schedule(season)
rnd <- schedule %>% filter(as.Date(date) == race_day) %>% pull(round) %>% as.numeric()
rnd     # CHECK: should be 16

# ---- 3. Load race data (Jolpica) ----------------------------
results <- load_results(season, rnd)
laps    <- load_laps(season, rnd)
pits    <- load_pitstops(season, rnd)

results %>% select(driver_id, grid, position, status) %>% print(n = 25)  # CHECK names

# driver_id -> 3-letter code (e.g. "max_verstappen" -> "VER")
code_of <- function(id) toupper(str_sub(str_extract(id, "[a-z]+$"), 1, 3))

grid <- results %>%
  transmute(driver_id, lap = 0,
            position = as.numeric(grid),
            position = if_else(position == 0, n(), position))   # pit lane start -> last

positions <- laps %>%
  transmute(driver_id, lap = as.numeric(lap), position = as.numeric(position)) %>%
  bind_rows(grid) %>%
  mutate(code = code_of(driver_id),
         group = if_else(code %in% focus$code, code, "other")) %>%
  arrange(driver_id, lap)

n_laps    <- max(positions$lap)
n_drivers <- n_distinct(positions$driver_id)

# pit stops of the two focus drivers, placed on their position line
pit_marks <- pits %>%
  transmute(driver_id, lap = as.numeric(lap)) %>%
  filter(driver_id %in% focus$driver_id) %>%
  left_join(positions, by = c("driver_id", "lap"))

# end-of-line labels: final position of VER and HAM
end_labels <- positions %>%
  filter(group != "other") %>%
  group_by(code) %>% slice_max(lap, n = 1) %>% ungroup() %>%
  mutate(label = paste0(code, "  P", position))

write.csv(positions, "q2_sepang2026_race_positions.csv", row.names = FALSE)

# ---- 4. Tyre stints (OpenF1) --------------------------------
base <- "https://api.openf1.org/v1/"
race_session <- fromJSON(paste0(base, "sessions?year=", season, "&session_name=Race")) %>%
  filter(as.Date(ymd_hms(date_start)) == race_day)
sk <- race_session$session_key[1]

stints <- bind_rows(lapply(seq_len(nrow(focus)), function(i) {
  fromJSON(paste0(base, "stints?session_key=", sk, "&driver_number=", focus$number[i])) %>%
    transmute(code = focus$code[i], stint = stint_number,
              lap_start, lap_end,
              compound = toupper(coalesce(compound, "UNKNOWN")))
})) %>%
  mutate(lap_end = if_else(is.na(lap_end), n_laps, as.numeric(lap_end)),
         code = factor(code, levels = rev(focus$code)))
stints     # CHECK: stints and compounds for VER and HAM

write.csv(stints, "q2_sepang2026_race_stints.csv", row.names = FALSE)

# ---- 5. Chart 1: positions ----------------------------------
x_scale <- scale_x_continuous(limits = c(0, n_laps + 6),        # room for end labels
                              breaks = seq(0, n_laps, 10),
                              expand = expansion(add = c(0.5, 0)))

p_pos <- ggplot(positions, aes(lap, position, group = driver_id)) +
  # everyone else: thin grey lines in the background
  geom_line(data = filter(positions, group == "other"),
            colour = "grey82", linewidth = 0.45) +
  # the two focus drivers on top
  geom_line(data = filter(positions, group != "other"),
            aes(colour = group), linewidth = 1.4) +
  geom_point(data = pit_marks, aes(colour = code), shape = 21, fill = "white",
             size = 2.6, stroke = 1.2) +
  geom_text(data = end_labels, aes(x = lap + 0.8, label = label, colour = code),
            hjust = 0, fontface = "bold", size = 3.8) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20), labels = function(x) paste0("P", x),
                  limits = c(n_drivers + 0.5, 0.5)) +
  x_scale +
  scale_colour_manual(values = driver_cols, guide = "none") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x        = element_blank(),
        plot.title         = element_text(face = "bold")) +
  labs(title    = "Sunday: did pole position matter?",
       subtitle = paste0(race_label, " | lap 0 = starting grid | ○ = pit stop"),
       x = NULL, y = NULL)

# ---- 6. Chart 2: tyre strategy ------------------------------
p_tyre <- ggplot(stints) +
  geom_rect(aes(xmin = lap_start - 1, xmax = lap_end,
                ymin = as.numeric(code) - 0.35, ymax = as.numeric(code) + 0.35,
                fill = compound),
            colour = "grey30", linewidth = 0.3) +
  scale_y_continuous(breaks = seq_along(levels(stints$code)),
                     labels = levels(stints$code)) +
  x_scale +
  scale_fill_manual(values = tyre_cols, name = "Tyre") +
  theme_minimal(base_size = 12) +
  theme(panel.grid   = element_blank(),
        axis.text.y  = element_text(face = "bold", size = 11,
                                    colour = driver_cols[levels(stints$code)]),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Lap", y = NULL)

# ---- 7. Stack them ------------------------------------------
p_race <- p_pos / p_tyre + plot_layout(heights = c(4, 1))
p_race

ggsave("q2_race_positions_tyres.png", p_race, width = 10, height = 7.5,
       dpi = 300, bg = "white")

# ---- 8. Key moments: look at these to write annotations -----
# (e.g. "HAM drops to P? on lap 1 on slicks", "VER pits on lap ?")
positions %>% filter(group != "other", lap <= 5) %>% arrange(code, lap) %>% print(n = 20)
pit_marks %>% select(code, lap, position)
