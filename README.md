# Soccer Elo Model

## Overview
This repository contains the data, scripts, figures, and paper outputs for a global **multi-season Elo rating model** of domestic league matches in Europe’s Big Five leagues (England, Spain, Italy, Germany, France). The dataset spans **2015/16 to the partial 2025/26 season** (cut off on **15 Dec 2025**).  

The project produces:
- match-outcome probability forecasts (evaluated using a Brier score), and  
- an end-of-sample **cross-league Elo ranking** of **163 clubs**.

## File Structure
The repository is organized as follows:

* **data/**

  * `raw/`: Raw input data files.

    * `england/`: Raw match CSVs by season (football-data.co.uk).
    * `spain/`: Raw match CSVs by season.
    * `italy/`: Raw match CSVs by season.
    * `germany/`: Raw match CSVs by season.
    * `france/`: Raw match CSVs by season.
    * `league_coefficients.csv`: UEFA country coefficients by season (manual export).
  * `clean/`: Cleaned and standardized datasets used for modelling and reporting.

    * `matches_clean.csv`: Cleaned unified match table (CSV).
    * `matches_clean.rds`: Cleaned unified match table (RDS).
    * `league_coefficients_long.csv`: Long-format UEFA coefficients (CSV).
    * `league_coefficients_long.rds`: Long-format UEFA coefficients (RDS).
  * `figures/`: Generated figures used in the paper (PNG).

* **script/**

  * `00-data_loading.R`: Loads raw match CSVs and creates a unified match table.
  * `01-data_cleaning.R`: Cleans matches, constructs UEFA-based offsets, and saves cleaned outputs.
  * `02-data_visualization.R`: Generates figures and saves them to `data/figures/`.
  * *(Other scripts may fit/tune the Elo model and export final outputs.)*

* **paper/**

  * `paper.qmd`: Main Quarto source file for the paper.
  * `references.bib`: Bibliography used by the paper.
  * `paper.pdf`: Rendered PDF output.
  * `paper.html`: Rendered HTML output.
  * `paper_files/`: Quarto supporting files for the HTML output.

* **project outputs (top-level)**

  * `final_ratings_global.csv`: Final club Elo table (used in paper tables).
  * `fdb_opta_ranking.csv`: FootballDatabase/Opta within-set ranks used for comparison.

* **project config**

  * `Soccer_Elo_Model.Rproj`: RStudio project file.


## License
