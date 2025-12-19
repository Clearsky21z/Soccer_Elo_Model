## =========================================================
## 01-data_cleaning.R
## Clean + merge league offsets; save cleaned datasets to data/clean/
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(here)
})

source(here::here("script", "00-data_loading.R"))

raw <- load_raw_data(
  data_dir  = here::here("data", "raw"),
  coef_path = here::here("data", "raw", "league_coefficients.csv")
)

matches_raw <- raw$matches_raw
league_coefs_wide <- raw$league_coefs_wide

# long-form league offsets
league_offsets <- league_coefs_wide %>%
  rename(Season = season) %>%
  pivot_longer(
    cols      = -Season,
    names_to  = "League",
    values_to = "league_offset"
  ) %>%
  mutate(
    Season = as.character(Season),
    League = tolower(League)
  )

# coefficient time series in long form
league_coefs_long <- league_coefs_wide %>%
  rename(Season = season) %>%
  pivot_longer(
    cols      = -Season,
    names_to  = "League",
    values_to = "coefficient"
  ) %>%
  mutate(
    Season          = as.character(Season),
    League          = tolower(League),
    SeasonStartYear = as.integer(substr(Season, 1, 4))
  )

# cleaned match table
matches_clean <- matches_raw %>%
  mutate(
    Season          = as.character(Season),
    League          = tolower(as.character(League)),
    SeasonStartYear = as.integer(substr(Season, 1, 4)),
    FTR             = factor(FTR, levels = c("H", "D", "A"))
  ) %>%
  left_join(league_offsets, by = c("Season", "League")) %>%
  mutate(
    league_offset = ifelse(is.na(league_offset), 0, league_offset)
  ) %>%
  filter(
    !is.na(Date),
    !is.na(HomeTeam),
    !is.na(AwayTeam),
    !is.na(FTHG),
    !is.na(FTAG),
    !is.na(FTR)
  ) %>%
  arrange(SeasonStartYear, Season, Date)

# output folder
clean_dir <- here::here("data", "clean")
if (!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE)

# save cleaned match data (RDS keeps types; CSV is handy for inspection)
saveRDS(matches_clean, file.path(clean_dir, "matches_clean.rds"))
readr::write_csv(matches_clean, file.path(clean_dir, "matches_clean.csv"))

# save coefficient time series
saveRDS(league_coefs_long, file.path(clean_dir, "league_coefficients_long.rds"))
readr::write_csv(league_coefs_long, file.path(clean_dir, "league_coefficients_long.csv"))

cat("Saved cleaned matches to: ", file.path(clean_dir, "matches_clean.rds"), "\n")
cat("Saved coefficient series to: ", file.path(clean_dir, "league_coefficients_long.rds"), "\n")
