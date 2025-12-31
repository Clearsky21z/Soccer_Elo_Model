## =========================================================
## 02-data_visualization.R
## Summaries + figures (cleaned data only)
## =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(here)
})

# -----------------------------
# Paths
# -----------------------------
clean_dir <- here::here("data", "clean")
fig_dir   <- here::here("data", "figures")

matches_path <- file.path(clean_dir, "matches_clean.rds")
coefs_path   <- file.path(clean_dir, "league_coefficients_long.rds")

if (!file.exists(matches_path)) {
  stop("Missing: ", matches_path, "\nRun script/01-data_cleaning.R first.")
}
if (!file.exists(coefs_path)) {
  stop("Missing: ", coefs_path, "\nRun script/01-data_cleaning.R first.")
}
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# -----------------------------
# Load cleaned data
# -----------------------------
matches <- readRDS(matches_path)
league_coefs_long <- readRDS(coefs_path)

# Pretty labels (capitalized countries)
league_labels <- c(
  england = "England",
  france  = "France",
  germany = "Germany",
  italy   = "Italy",
  spain   = "Spain"
)

# =========================================================
# Figure 1: Result proportions by league (p2)
# - NO missing in legend (drop non H/D/A before counting)
# - show H/D/A percentages on bars (1 decimal)
# =========================================================

matches_plot1 <- matches %>%
  mutate(
    League = tolower(str_squish(as.character(League))),
    League = factor(League, levels = names(league_labels))
  ) %>%
  filter(FTR %in% c("H", "D", "A")) %>%   # <- this guarantees no "Missing" category
  mutate(
    FTR_plot = factor(
      FTR,
      levels = c("H", "D", "A"),
      labels = c("Home win (H)", "Draw (D)", "Away win (A)")
    )
  )

outcome_by_league <- matches_plot1 %>%
  count(League, FTR_plot, name = "n") %>%
  group_by(League) %>%
  mutate(prop = n / sum(n)) %>%
  # ---- make 1-decimal labels that sum to 100.0 within each League ----
mutate(
  raw10   = prop * 1000,               # percent * 10  (tenths of a percent)
  floor10 = floor(raw10),
  frac10  = raw10 - floor10,
  rem10   = 1000L - sum(floor10),      # how many 0.1% units we still need
  add10   = ifelse(rank(-frac10, ties.method = "first") <= rem10, 1L, 0L),
  pct_adj = (floor10 + add10) / 10,
  pct_lab = sprintf("%.1f%%", pct_adj)
) %>%
  ungroup()


p2 <- ggplot(outcome_by_league, aes(x = League, y = prop, fill = FTR_plot)) +
  geom_col() +
  geom_text(
    aes(label = pct_lab),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_x_discrete(labels = league_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Result", na.translate = FALSE) +
  labs(
    x = "Country",
    y = "Percentage of matches"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "result_proportions_by_league.png"),
  plot     = p2,
  width    = 7,
  height   = 4.5,
  dpi      = 300
)

# =========================================================
# Figure 2: Goals distribution (p3)
# - show goals discretely: 0,1,2,3,4,5,5+
# - spell out FTHG/FTAG
# =========================================================

goals_long <- matches %>%
  select(FTHG, FTAG) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "side",
    values_to = "goals"
  ) %>%
  filter(!is.na(goals)) %>%
  mutate(
    side = factor(
      side,
      levels = c("FTHG", "FTAG"),
      labels = c(
        "Full-time home goals (FTHG)",
        "Full-time away goals (FTAG)"
      )
    ),
    goals_bin = case_when(
      goals >= 5 ~ "5+",
      TRUE       ~ as.character(as.integer(goals))
    ),
    goals_bin = factor(goals_bin, levels = c("0","1","2","3","4","5","5+"))
  )

goals_counts <- goals_long %>%
  count(side, goals_bin, name = "n")

p3 <- ggplot(goals_counts, aes(x = goals_bin, y = n, fill = side)) +
  geom_col(position = position_dodge(width = 0.9), colour = "white") +
  labs(
    x = "Goals scored",
    y = "Count of matches",
    fill = "Goal variable"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "goals_distribution_home_away.png"),
  plot     = p3,
  width    = 7,
  height   = 4.5,
  dpi      = 300
)

# =========================================================
# Figure 3: UEFA coefficients over time (p5)
# - x-axis should be a Date class
# =========================================================

coefs_plot <- league_coefs_long %>%
  mutate(
    League = tolower(as.character(League)),
    LeagueLabel = recode(League, !!!league_labels),
    # represent each season by July 1 of its start year (Date class)
    SeasonStartDate = as.Date(paste0(SeasonStartYear, "-07-01"))
  )

p5 <- ggplot(coefs_plot, aes(x = SeasonStartDate, y = coefficient, colour = LeagueLabel)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    x = "Season start date",
    y = "UEFA country coefficient",
    colour = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9)
  )


ggsave(
  filename = file.path(fig_dir, "uefa_coefficients_by_league.png"),
  plot     = p5,
  width    = 7,
  height   = 4.5,
  dpi      = 300
)

cat("Saved figures to: ", fig_dir, "\n")
