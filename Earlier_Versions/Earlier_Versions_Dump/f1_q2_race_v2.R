# ============================================================
#  Q2 – The race, version 2
#  2026 Bahrain GP in Malaysia (Sepang), Sunday 4 October
#
#  Chart 1 (top):    "tyre-striped race trace" – gap to the leader on
#                    every lap. VER & HAM drawn as a team-colour stripe
#                    with the tyre compound colour inside.
#  Chart 2 (bottom): position bump chart (smooth curves)
#
#  DATA: f1dataR (Jolpica) for laps, results, pit stops
#        OpenF1 "stints" for tyre compounds
# ============================================================

# install.packages(c("f1dataR", "jsonlite", "lubridate", "patchwork", "ggbump"))
library(f1dataR)
library(jsonlite)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggbump)      # smooth "bump" lines for the position chart

# ---- 1. Settings --------------------------------------------
season    <- 2026
race_day  <- as.Date("2026-10-04")
focus     <- tibble(code = c("VER", "HAM"),
                    driver_id = c("max_verstappen", "hamilton"),   # Jolpica ids
                    number = c(3, 44))                             # OpenF1 numbers
driver_cols <- c(VER = "#3671C6", HAM = "#E8002D")
tyre_cols   <- c(SOFT = "#DA291C", MEDIUM = "#FFD12E", HARD = "#F4F4F0",
                 INTERMEDIATE = "#43B02A", WET = "#0067AD", UNKNOWN = "#BDBDBD")
max_gap    <- 60          # seconds shown on the race trace (lapped cars go off-chart)
race_label <- "Bahrain GP in Malaysia (Sepang) 2026 – race"

# ---- 2. Round number + race data (Jolpica) ------------------
schedule <- load_schedule(season)
rnd <- schedule %>% filter(as.Date(date) == race_day) %>% pull(round) %>% as.numeric()
rnd     # CHECK: 16

results <- load_results(season, rnd)
laps    <- load_laps(season, rnd)
pits    <- load_pitstops(season, rnd)

code_of <- function(id) toupper(str_sub(str_extract(id, "[a-z]+$"), 1, 3))

# ---- 3. Gap to the leader on every lap ----------------------
# Add up each driver's lap times = the moment he crossed the line after
# each lap. The leader crossed first; everyone's gap = how many seconds
# later they crossed.
trace <- laps %>%
  transmute(driver_id, lap = as.numeric(lap), time_sec = as.numeric(time_sec)) %>%
  arrange(driver_id, lap) %>%
  group_by(driver_id) %>%
  mutate(total = cumsum(time_sec)) %>%
  group_by(lap) %>%
  mutate(gap = total - min(total)) %>%            # 0 = leader
  ungroup() %>%
  bind_rows(results %>% transmute(driver_id, lap = 0, gap = 0)) %>%   # all start at 0
  mutate(code  = code_of(driver_id),
         group = if_else(code %in% focus$code, code, "other")) %>%
  arrange(driver_id, lap)

n_laps <- max(trace$lap)
write.csv(trace, "q2_sepang2026_race_gaps.csv", row.names = FALSE)

# ---- 4. Tyre compound on every lap for VER & HAM (OpenF1) ---
base <- "https://api.openf1.org/v1/"
sk <- fromJSON(paste0(base, "sessions?year=", season, "&session_name=Race")) %>%
  filter(as.Date(ymd_hms(date_start)) == race_day) %>% pull(session_key) %>% first()

stints <- bind_rows(lapply(seq_len(nrow(focus)), function(i) {
  fromJSON(paste0(base, "stints?session_key=", sk, "&driver_number=", focus$number[i])) %>%
    transmute(code = focus$code[i],
              lap_start = as.numeric(lap_start),
              lap_end   = as.numeric(lap_end),
              compound  = toupper(coalesce(compound, "UNKNOWN")))
})) %>% mutate(lap_end = coalesce(lap_end, n_laps))
stints        # CHECK: stints and compounds make sense?
write.csv(stints, "q2_sepang2026_race_stints.csv", row.names = FALSE)

# which compound was each driver on, on each lap?
compound_on <- function(cd, lp) {
  lp <- max(lp, 1)                                 # lap 0 = start tyre
  s  <- filter(stints, code == cd, lap_start <= lp, lap_end >= lp)
  if (nrow(s) == 0) "UNKNOWN" else s$compound[1]
}
focus_trace <- trace %>%
  filter(group != "other") %>%
  rowwise() %>% mutate(compound = compound_on(code, lap)) %>% ungroup()

# pit stops of VER & HAM, placed on their trace line
pit_marks <- pits %>%
  transmute(driver_id, lap = as.numeric(lap)) %>%
  filter(driver_id %in% focus$driver_id) %>%
  left_join(trace, by = c("driver_id", "lap"))

end_trace <- focus_trace %>% group_by(code) %>% slice_max(lap, n = 1) %>% ungroup()

# ---- 5. Chart 1: tyre-striped race trace --------------------
x_scale <- scale_x_continuous(limits = c(0, n_laps + 7), breaks = seq(0, n_laps, 10),
                              expand = expansion(add = c(0.5, 0)))

p_trace <- ggplot() +
  # everyone else
  geom_line(data = filter(trace, group == "other"),
            aes(lap, gap, group = driver_id), colour = "grey84", linewidth = 0.4) +
  # outer stripe in team colour (one layer per driver, fixed colour)
  geom_path(data = filter(focus_trace, code == "VER"), aes(lap, gap),
            colour = driver_cols[["VER"]], linewidth = 3.4, lineend = "round") +
  geom_path(data = filter(focus_trace, code == "HAM"), aes(lap, gap),
            colour = driver_cols[["HAM"]], linewidth = 3.4, lineend = "round") +
  # inner stripe = tyre compound on that lap
  geom_path(data = focus_trace, aes(lap, gap, group = code, colour = compound),
            linewidth = 1.3, lineend = "round") +
  # pit stops
  geom_point(data = pit_marks, aes(lap, gap), shape = 21, fill = "white",
             colour = "grey15", size = 2.6, stroke = 1) +
  # driver labels at the end
  geom_text(data = end_trace, aes(lap + 0.8, gap, label = code),
            colour = driver_cols[end_trace$code], hjust = 0,
            fontface = "bold", size = 4) +
  scale_colour_manual(values = tyre_cols, name = "Tyre") +
  scale_y_reverse(labels = function(x) paste0("+", x, " s")) +
  coord_cartesian(ylim = c(max_gap, -2)) +        # leader at the top
  x_scale +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x        = element_blank(),
        legend.position    = "top",
        legend.justification = "left",
        plot.title         = element_text(face = "bold")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3))) +
  labs(title    = "Sunday: how the race unfolded",
       subtitle = paste0(race_label,
                         " | gap to the leader each lap · stripe = team, inside = tyre · ○ = pit stop"),
       x = NULL, y = "Gap to leader")

# ---- 6. Chart 2: positions as a bump chart ------------------
grid_pos <- results %>%
  transmute(driver_id, lap = 0, position = as.numeric(grid),
            position = if_else(position == 0, n(), position))
positions <- laps %>%
  transmute(driver_id, lap = as.numeric(lap), position = as.numeric(position)) %>%
  bind_rows(grid_pos) %>%
  mutate(code = code_of(driver_id),
         group = if_else(code %in% focus$code, code, "other")) %>%
  arrange(driver_id, lap)
n_drivers <- n_distinct(positions$driver_id)

end_pos <- positions %>% filter(group != "other") %>%
  group_by(code) %>% slice_max(lap, n = 1) %>% ungroup() %>%
  mutate(label = paste0(code, " P", position))

write.csv(positions, "q2_sepang2026_race_positions.csv", row.names = FALSE)

p_pos <- ggplot(positions, aes(lap, position, group = driver_id)) +
  geom_bump(data = filter(positions, group == "other"),
            colour = "grey84", linewidth = 0.4, smooth = 6) +
  geom_bump(data = filter(positions, group != "other"),
            aes(colour = group), linewidth = 1.4, smooth = 6) +
  geom_text(data = end_pos, aes(x = lap + 0.8, label = label, colour = code),
            hjust = 0, fontface = "bold", size = 3.8) +
  scale_colour_manual(values = driver_cols, guide = "none") +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20), labels = function(x) paste0("P", x)) +
  coord_cartesian(ylim = c(n_drivers + 0.5, 0.5)) +
  x_scale +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 11)) +
  labs(title = "Position, lap by lap", x = "Lap", y = NULL)

# ---- 7. Stack: trace on top, positions below ----------------
p_race <- p_trace / p_pos + plot_layout(heights = c(3, 2))
p_race

ggsave("q2_race_v2.png", p_race, width = 10, height = 9, dpi = 300, bg = "white")

# ---- 8. Key moments, to write your annotations --------------
trace %>% filter(group != "other", lap <= 5) %>%
  left_join(select(positions, driver_id, lap, position), by = c("driver_id", "lap")) %>%
  select(code, lap, gap, position) %>% arrange(code, lap) %>% print(n = 20)
pit_marks %>% select(code, lap, gap)
