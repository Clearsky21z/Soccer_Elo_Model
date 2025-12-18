## =========================================================
## 00-data_loading.R
## Read raw match files (data/raw/<league>/) + league coefficient file (data/)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(here)
})

# infer Season string like "2015/16" from filename "Germany_2015_16.csv"
parse_season_from_filename <- function(fname) {
  m <- stringr::str_match(fname, "([0-9]{4})_([0-9]{2})")
  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 2], "/", m[1, 3])
}

# load all Big-5 league CSVs under data/raw/<league>/
load_all_matches <- function(data_dir = here::here("data", "raw"),
                             leagues  = c("england", "spain", "italy", "germany", "france")) {
  
  if (!dir.exists(data_dir)) {
    stop("Raw data directory not found: ", data_dir)
  }
  
  all_files <- unlist(lapply(leagues, function(lg) {
    dir(file.path(data_dir, lg),
        pattern    = "\\.csv$",
        full.names = TRUE)
  }))
  
  if (length(all_files) == 0) {
    stop("No match CSV files found under: ", data_dir,
         "\nExpected structure like data/raw/england/*.csv")
  }
  
  matches_list <- lapply(all_files, function(f) {
    
    df <- readr::read_csv(f, show_col_types = FALSE)
    league <- tolower(basename(dirname(f)))   # "england", "germany", ...
    
    # add Season if missing
    if (!"Season" %in% names(df)) {
      df$Season <- parse_season_from_filename(basename(f))
    }
    
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
    if (!all(required_cols %in% names(df))) {
      stop("Missing required columns in file: ", f,
           "\nRequired: ", paste(required_cols, collapse = ", "))
    }
    
    df %>%
      mutate(
        League   = league,
        Season   = as.character(Season),
        Date     = lubridate::dmy(Date),
        HomeTeam = stringr::str_squish(as.character(HomeTeam)),
        AwayTeam = stringr::str_squish(as.character(AwayTeam)),
        FTR      = as.character(FTR)
      ) %>%
      select(Season, Date, League, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  })
  
  dplyr::bind_rows(matches_list)
}

load_league_coefficients <- function(coef_path = here::here("data", "league_coefficients.csv")) {
  if (!file.exists(coef_path)) {
    stop("League coefficient file not found: ", coef_path)
  }
  readr::read_csv(coef_path, show_col_types = FALSE)
}

load_raw_data <- function(data_dir  = here::here("data", "raw"),
                          coef_path = here::here("data", "raw", "league_coefficients.csv"),
                          leagues   = c("england", "spain", "italy", "germany", "france")) {
  
  matches_raw       <- load_all_matches(data_dir = data_dir, leagues = leagues)
  league_coefs_wide <- load_league_coefficients(coef_path = coef_path)
  
  list(
    matches_raw       = matches_raw,
    league_coefs_wide = league_coefs_wide
  )
}
