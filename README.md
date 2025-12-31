
# Soccer Elo Model (Big Five Leagues)

## Overview
This repository contains the data, scripts, figures, and paper outputs for a global **multi-season Elo rating model** of domestic league matches in Europe’s Big Five leagues (England, Spain, Italy, Germany, France). The dataset spans **2015/16 to the partial 2025/26 season** (cut off on **15 Dec 2025**).  

The project produces:
- match-outcome probability forecasts (evaluated using a Brier score), and  
- an end-of-sample **cross-league Elo ranking** of **163 clubs**.

## File Structure
```

.
├── data/
│   ├── raw/
│   │   ├── england/                 # raw match CSVs by season (football-data.co.uk)
│   │   ├── spain/                   # raw match CSVs by season
│   │   ├── italy/                   # raw match CSVs by season
│   │   ├── germany/                 # raw match CSVs by season
│   │   ├── france/                  # raw match CSVs by season
│   │   └── league_coefficients.csv  # UEFA country coefficients by season (manual export)
│   ├── clean/
│   │   ├── matches_clean.csv        # cleaned unified match table (CSV)
│   │   ├── matches_clean.rds        # cleaned unified match table (RDS)
│   │   ├── league_coefficients_long.csv # long-format coefficients (CSV)
│   │   └── league_coefficients_long.rds # long-format coefficients (RDS)
│   └── figures/                     # generated figures used in the paper (PNG)
│
├── script/
│   ├── 00-data_loading.R            # loads raw match CSVs and creates a unified table
│   ├── 01-data_cleaning.R           # cleans matches + builds/joins coefficient offsets
│   └── 02-data_visualization.R      # generates figures saved to data/figures/
│   # (other scripts here may fit/tune the Elo model and export results)
│
├── paper/
│   ├── paper.qmd                    # main Quarto paper source
│   ├── references.bib               # bibliography used by the paper
│   ├── paper.pdf                    # rendered PDF output
│   ├── paper.html                   # rendered HTML output
│   └── paper_files/                 # Quarto supporting files for HTML output
│
├── final_ratings_global.csv         # final club Elo table (used in paper tables)
├── fdb_opta_ranking.csv             # FootballDatabase/Opta within-set ranks for comparison
└── Soccer_Elo_Model.Rproj           # RStudio project file

```

## License
