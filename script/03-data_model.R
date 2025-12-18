## =========================================================
## 03-global_elo_model.R
## Global multi-season Elo model (cleaned data only)
## =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
  library(tidyr)
})

# -----------------------------
# Load cleaned matches
# -----------------------------
matches_path <- here::here("data", "clean", "matches_clean.rds")
if (!file.exists(matches_path)) {
  stop("Missing: ", matches_path, "\nRun script/01-data_cleaning.R first.")
}

matches <- readRDS(matches_path) %>%
  arrange(SeasonStartYear, Season, Date)

# -----------------------------
# Elo engine
# -----------------------------
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
  
  ratings <- numeric(0)
  carry_over_ratings <- numeric(0)
  
  current_season <- matches$Season[1]
  latest_year <- max(matches$SeasonStartYear, na.rm = TRUE)
  
  for (i in seq_len(n)) {
    
    season_i <- matches$Season[i]
    season_year_i <- matches$SeasonStartYear[i]
    
    # ---- season break shrinkage ----
    if (i > 1 && season_i != current_season) {
      
      if (length(carry_over_ratings) > 0) {
        carry_over_ratings <- init_rating +
          lambda_shrink * (carry_over_ratings - init_rating)
      }
      
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
    
    if (!home %in% names(ratings)) {
      ratings[home] <- if (home %in% names(carry_over_ratings)) {
        carry_over_ratings[home]
      } else {
        init_rating - promo_penalty
      }
    }
    
    if (!away %in% names(ratings)) {
      ratings[away] <- if (away %in% names(carry_over_ratings)) {
        carry_over_ratings[away]
      } else {
        init_rating - promo_penalty
      }
    }
    
    Rh <- ratings[home]
    Ra <- ratings[away]
    
    matches$HomeElo_before[i] <- Rh
    matches$AwayElo_before[i] <- Ra
    
    offset <- matches$league_offset[i]
    if (is.na(offset)) offset <- 0
    
    diff <- (Rh + home_adv + offset) - (Ra + offset)
    Eh <- 1 / (1 + 10^(-diff / 400))
    Ea <- 1 - Eh
    
    matches$ExpHomeScore[i] <- Eh
    matches$ExpAwayScore[i] <- Ea
    
    res <- as.character(matches$FTR[i])
    Sh <- ifelse(res == "H", 1, ifelse(res == "D", 0.5, 0))
    Sa <- 1 - Sh
    
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

# -----------------------------
# Train/test split
# -----------------------------
set.seed(123)
n <- nrow(matches)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

# -----------------------------
# Evaluation (relevance-weighted Brier)
# -----------------------------
evaluate_params <- function(K, lambda_shrink, promo_penalty, relevance_decay, current,
                            data, train_idx, home_adv = 100, init_rating = 1200) {
  
  latest_year <- max(data$SeasonStartYear, na.rm = TRUE)
  
  data <- data %>%
    mutate(
      SeasonAge = latest_year - SeasonStartYear,
      relevance = relevance_decay ^ SeasonAge
    )
  
  elo_df <- compute_elo_global(
    matches       = data,
    K             = K,
    home_adv      = home_adv,
    init_rating   = init_rating,
    lambda_shrink = lambda_shrink,
    promo_penalty = promo_penalty,
    current       = current
  ) %>%
    mutate(Sh = ifelse(FTR == "H", 1, ifelse(FTR == "D", 0.5, 0)))
  
  train_dat <- elo_df[train_idx, ]
  test_dat  <- elo_df[-train_idx, ]
  
  w_train <- train_dat$relevance
  w_test  <- test_dat$relevance
  
  brier_train <- sum(w_train * (train_dat$Sh - train_dat$ExpHomeScore)^2, na.rm = TRUE) /
    sum(w_train, na.rm = TRUE)
  
  brier_test <- sum(w_test * (test_dat$Sh - test_dat$ExpHomeScore)^2, na.rm = TRUE) /
    sum(w_test, na.rm = TRUE)
  
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

brier_objective <- function(par, data, train_idx) {
  evaluate_params(
    K               = par[1],
    lambda_shrink   = par[2],
    promo_penalty   = par[3],
    relevance_decay = par[4],
    current         = par[5],
    data            = data,
    train_idx       = train_idx
  )$brier_test
}

# -----------------------------
# Optimisation
# -----------------------------
start_par <- c(K = 25, lambda_shrink = 0.85, promo_penalty = 50, relevance_decay = 0.40, current = 1.5)
lower_par <- c(K = 5,  lambda_shrink = 0.5,  promo_penalty = 0,  relevance_decay = 0.10, current = 1.0)
upper_par <- c(K = 80, lambda_shrink = 1.0,  promo_penalty = 200, relevance_decay = 0.80, current = 3.0)

opt_res <- optim(
  par    = start_par,
  fn     = brier_objective,
  method = "L-BFGS-B",
  lower  = lower_par,
  upper  = upper_par,
  data   = matches,
  train_idx = train_idx
)

best_K               <- opt_res$par["K"]
best_lambda          <- opt_res$par["lambda_shrink"]
best_promo_penalty   <- opt_res$par["promo_penalty"]
best_relevance_decay <- opt_res$par["relevance_decay"]
best_current         <- opt_res$par["current"]

# -----------------------------
# Final refit + outputs
# -----------------------------
latest_year <- max(matches$SeasonStartYear, na.rm = TRUE)

matches_with_rel <- matches %>%
  mutate(
    SeasonAge = latest_year - SeasonStartYear,
    relevance = best_relevance_decay ^ SeasonAge
  )

matches_elo <- compute_elo_global(
  matches       = matches_with_rel,
  K             = best_K,
  home_adv      = 100,
  init_rating   = 1200,
  lambda_shrink = best_lambda,
  promo_penalty = best_promo_penalty,
  current       = best_current
) %>%
  mutate(Sh = ifelse(FTR == "H", 1, ifelse(FTR == "D", 0.5, 0)))

brier_overall <- matches_elo %>%
  summarise(brier = mean((Sh - ExpHomeScore)^2, na.rm = TRUE)) %>%
  pull(brier)

final_ratings <- matches_elo %>%
  # Build a club-level table from both home+away appearances
  select(Date, SeasonStartYear, League,
         HomeTeam, AwayTeam, HomeElo_after, AwayElo_after) %>%
  pivot_longer(
    cols      = c(HomeTeam, AwayTeam),
    names_to  = "Side",
    values_to = "Club"
  ) %>%
  mutate(
    Elo_after = ifelse(Side == "HomeTeam", HomeElo_after, AwayElo_after)
  ) %>%
  filter(!is.na(Club), !is.na(Elo_after)) %>%
  group_by(Club) %>%
  summarise(
    Country = {
      # most recent league label for that club
      League[which.max(Date)]
    },
    Elo = Elo_after[which.max(Date)],
    .groups = "drop"
  ) %>%
  arrange(desc(Elo))

out_path <- here::here("final_ratings_global.csv")
write_csv(final_ratings, out_path)

