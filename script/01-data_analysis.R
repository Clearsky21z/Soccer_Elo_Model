library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

project_root <- getwd()

data_dir  <- file.path(project_root, "data")
coef_path <- file.path(data_dir,     "league_coefficients.csv")

cat("Project root: ", project_root, "\n")
cat("Data dir    : ", data_dir, "\n")
cat("Coef path   : ", coef_path, "\n")

## ---------------------------------------------------------
## 1. Helper: load all match data
## ---------------------------------------------------------

# infer Season string like "2015/16" from filename "Germany_2015_16.csv"
parse_season_from_filename <- function(fname) {
  m <- stringr::str_match(fname, "([0-9]{4})_([0-9]{2})")
  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 2], "/", m[1, 3])
}

# load all Big-5 league CSVs under data/<league>/
load_all_matches <- function(data_dir) {
  
  leagues <- c("england", "spain", "italy", "germany", "france")
  
  all_files <- unlist(lapply(leagues, function(lg) {
    dir(file.path(data_dir, lg),
        pattern = "\\.csv$",
        full.names = TRUE)
  }))
  
  matches_list <- lapply(all_files, function(f) {
    
    df <- read_csv(f, show_col_types = FALSE)
    league <- tolower(basename(dirname(f)))   # "england", "germany", ...
    
    # add Season if missing
    if (!"Season" %in% names(df)) {
      df$Season <- parse_season_from_filename(basename(f))
    }
    
    # football-data style columns
    stopifnot(all(c("Date", "HomeTeam", "AwayTeam",
                    "FTHG", "FTAG", "FTR") %in% names(df)))
    
    df %>%
      mutate(
        League = league,
        Season = as.character(Season),
        Date   = dmy(Date)            # convert to Date
      ) %>%
      # NOTE: we drop Time completely to avoid type headaches
      select(Season, Date, League,
             HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  })
  
  bind_rows(matches_list)
}

matches_raw <- load_all_matches(data_dir)

## ---------------------------------------------------------
## 2. League coefficients (UEFA averages) in long form
##    data/league_coefficients.csv columns:
##    season, england, spain, italy, germany, france
## ---------------------------------------------------------

league_coefs_wide <- read_csv(coef_path, show_col_types = FALSE)

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

## ---------------------------------------------------------
## 3. Prepare master matches table with offsets
## ---------------------------------------------------------

matches <- matches_raw %>%
  mutate(
    SeasonStartYear = as.integer(substr(Season, 1, 4))
  ) %>%
  left_join(league_offsets,
            by = c("Season", "League")) %>%
  arrange(SeasonStartYear, Season, Date)

## ---------------------------------------------------------
## 4. Global Elo function (multi-season, promos, offsets)
##    NEW: 'current' parameter -> extra weight for current season
## ---------------------------------------------------------

compute_elo_global <- function(matches,
                               K              = 40,
                               home_adv       = 100,
                               init_rating    = 1200,
                               lambda_shrink  = 0.85,
                               promo_penalty  = 50,
                               current        = 1) {
  
  n <- nrow(matches)
  
  matches$HomeElo_before <- NA_real_
  matches$AwayElo_before <- NA_real_
  matches$HomeElo_after  <- NA_real_
  matches$AwayElo_after  <- NA_real_
  matches$ExpHomeScore   <- NA_real_
  matches$ExpAwayScore   <- NA_real_
  
  # active-season ratings
  ratings <- numeric(0)
  names(ratings) <- character(0)
  
  # ratings parked between seasons
  carry_over_ratings <- numeric(0)
  names(carry_over_ratings) <- character(0)
  
  current_season <- matches$Season[1]
  latest_year    <- max(matches$SeasonStartYear, na.rm = TRUE)
  
  for (i in seq_len(n)) {
    
    season_i <- matches$Season[i]
    season_year_i <- matches$SeasonStartYear[i]
    
    ## ---- season break: shrink ratings between seasons ----
    if (i > 1 && season_i != current_season) {
      
      # shrink teams already in carry_over
      if (length(carry_over_ratings) > 0) {
        carry_over_ratings <- init_rating +
          lambda_shrink * (carry_over_ratings - init_rating)
      }
      
      # shrink teams that just finished last season
      if (length(ratings) > 0) {
        carry_from_prev <- init_rating +
          lambda_shrink * (ratings - init_rating)
        carry_over_ratings[names(carry_from_prev)] <- carry_from_prev
      }
      
      ratings <- numeric(0)
      current_season <- season_i
    }
    
    home <- matches$HomeTeam[i]
    away <- matches$AwayTeam[i]
    
    # ensure teams have ratings at this point
    if (!home %in% names(ratings)) {
      if (home %in% names(carry_over_ratings)) {
        ratings[home] <- carry_over_ratings[home]
      } else {
        ratings[home] <- init_rating - promo_penalty  # new / promoted
      }
    }
    
    if (!away %in% names(ratings)) {
      if (away %in% names(carry_over_ratings)) {
        ratings[away] <- carry_over_ratings[away]
      } else {
        ratings[away] <- init_rating - promo_penalty
      }
    }
    
    Rh <- ratings[home]
    Ra <- ratings[away]
    
    matches$HomeElo_before[i] <- Rh
    matches$AwayElo_before[i] <- Ra
    
    # league offset from coefficients (Avg); NA -> 0
    offset <- matches$league_offset[i]
    if (is.na(offset)) offset <- 0
    
    # expected scores with home advantage + league offset
    diff <- (Rh + home_adv + offset) - (Ra + offset)
    Eh   <- 1 / (1 + 10^(-diff / 400))
    Ea   <- 1 - Eh
    
    matches$ExpHomeScore[i] <- Eh
    matches$ExpAwayScore[i] <- Ea
    
    # actual scores
    res <- matches$FTR[i]
    Sh  <- ifelse(res == "H", 1,
                  ifelse(res == "D", 0.5, 0))
    Sa  <- 1 - Sh
    
    # effective K: boost for current season (latest_year)
    K_eff <- if (!is.na(season_year_i) && season_year_i == latest_year) {
      K * current
    } else {
      K
    }
    
    Rh_new <- Rh + K_eff * (Sh - Eh)
    Ra_new <- Ra + K_eff * (Sa - Ea)
    
    matches$HomeElo_after[i] <- Rh_new
    matches$AwayElo_after[i] <- Ra_new
    
    ratings[home] <- Rh_new
    ratings[away] <- Ra_new
  }
  
  matches
}

## ---------------------------------------------------------
## 5. Train/test split for tuning
## ---------------------------------------------------------

set.seed(123)
n <- nrow(matches)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

## ---------------------------------------------------------
## 6. Hyperparameter tuning with Brier + relevance
##    relevance_decay^(SeasonAge); plus 'current' (boost K in current season)
## ---------------------------------------------------------

evaluate_params <- function(K,
                            lambda_shrink,
                            promo_penalty,
                            relevance_decay,
                            current,
                            data,
                            train_idx,
                            home_adv = 100,
                            init_rating = 1200) {
  
  # compute relevance weights for this setting
  latest_year <- max(data$SeasonStartYear, na.rm = TRUE)
  
  data <- data %>%
    mutate(
      SeasonAge = latest_year - SeasonStartYear,
      relevance = relevance_decay ^ SeasonAge
    )
  
  # run global Elo on this data
  elo_df <- compute_elo_global(
    matches        = data,
    K              = K,
    home_adv       = home_adv,
    init_rating    = init_rating,
    lambda_shrink  = lambda_shrink,
    promo_penalty  = promo_penalty,
    current        = current
  )
  
  # actual home "score"
  elo_df <- elo_df %>%
    mutate(
      Sh = ifelse(FTR == "H", 1,
                  ifelse(FTR == "D", 0.5, 0))
    )
  
  train_dat <- elo_df[train_idx, ]
  test_dat  <- elo_df[-train_idx, ]
  
  w_train <- train_dat$relevance
  w_test  <- test_dat$relevance
  
  # relevance-weighted Brier
  brier_train <- sum(w_train * (train_dat$Sh - train_dat$ExpHomeScore)^2,
                     na.rm = TRUE) / sum(w_train, na.rm = TRUE)
  brier_test  <- sum(w_test  * (test_dat$Sh  - test_dat$ExpHomeScore)^2,
                     na.rm = TRUE) / sum(w_test, na.rm = TRUE)
  
  tibble::tibble(
    K               = K,
    lambda_shrink   = lambda_shrink,
    promo_penalty   = promo_penalty,
    relevance_decay = relevance_decay,
    current         = current,
    brier_train     = brier_train,
    brier_test      = brier_test
  )
}

## ---------------------------------------------------------
## 7. Smaller / harsher hyperparameter grid (with 'current')
## ---------------------------------------------------------

# K: narrower range, fewer values
K_grid          <- c(20, 25, 30)          # 3 values

# lambda_shrink: light vs medium vs no shrink
lambda_grid     <- c(0.7, 0.85, 1.0)      # 3 values

# promo_penalty: modest range
promo_grid      <- c(0, 50)               # 2 values

# relevance_decay: strong punishment for old seasons
relevance_grid  <- c(0.30, 0.40, 0.50)    # 3 values

# current: boost factor for current season K
current_grid    <- c(1.0, 1.5, 2.0)       # 3 values

param_grid <- expand.grid(
  K               = K_grid,
  lambda_shrink   = lambda_grid,
  promo_penalty   = promo_grid,
  relevance_decay = relevance_grid,
  current         = current_grid
)

nrow(param_grid)  # 3 * 3 * 2 * 3 * 3 = 162 combos

results_list <- lapply(
  seq_len(nrow(param_grid)),
  function(i) {
    row <- param_grid[i, ]
    evaluate_params(
      K               = row$K,
      lambda_shrink   = row$lambda_shrink,
      promo_penalty   = row$promo_penalty,
      relevance_decay = row$relevance_decay,
      current         = row$current,
      data            = matches,
      train_idx       = train_idx
    )
  }
)

results_df <- dplyr::bind_rows(results_list)
results_df

best_row <- results_df[which.min(results_df$brier_test), ]
best_row

best_K               <- best_row$K
best_lambda          <- best_row$lambda_shrink
best_promo_penalty   <- best_row$promo_penalty
best_relevance_decay <- best_row$relevance_decay
best_current         <- best_row$current

## ---------------------------------------------------------
## 8. Refit final model with best params
## ---------------------------------------------------------

# recompute relevance for the whole dataset using best_relevance_decay
latest_year <- max(matches$SeasonStartYear, na.rm = TRUE)

matches_with_rel <- matches %>%
  mutate(
    SeasonAge = latest_year - SeasonStartYear,
    relevance = best_relevance_decay ^ SeasonAge
  )

matches_elo <- compute_elo_global(
  matches        = matches_with_rel,
  K              = best_K,
  home_adv       = 100,          # still fixed for now
  init_rating    = 1200,
  lambda_shrink  = best_lambda,
  promo_penalty  = best_promo_penalty,
  current        = best_current
)

matches_elo <- matches_elo %>%
  mutate(
    Sh = ifelse(FTR == "H", 1,
                ifelse(FTR == "D", 0.5, 0))
  )

# overall (unweighted) Brier
brier_overall <- matches_elo %>%
  summarise(brier = mean((Sh - ExpHomeScore)^2, na.rm = TRUE)) %>%
  pull(brier)

brier_overall

final_ratings <- matches_elo %>%
  filter(!is.na(HomeTeam)) %>%
  group_by(HomeTeam) %>%
  summarise(
    Elo = max(HomeElo_after, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Elo))

final_ratings
