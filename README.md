# Fantasy Dynasty

![License: MIT](https://img.shields.io/badge/License-MIT-yellow)

A repository for modeling fantasy football player value, simulating multi-year player careers,
valuing draft picks, projecting team outcomes, and powering a Shiny dashboard for tehe **Baylor Seniors Dynasty League**.

---

## Table of Contents
- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Methodology](#methodology)
  - [Scraping](#scraping)
  - [Player Value Added](#player-value-added)
  - [Player Total Value](#player-total-value)
  - [Draft Pick Value](#draft-pick-value)
  - [Future Standings](#future-standings)
- [Updating Workflow](#updating-workflow)
  - [Weekly Updates](#weekly-updates)
  - [Yearly Updates](#yearly-updates)
- [Roadmap](#roadmap)
- [References](#references)

# Overview

This project constructs a **value added** metric to measure fantasy players' value relative to their potential replacement,
models each player's future value using BART and career simulations, assigns values to draft picks, and projects
future team outcomes within the league.

The model and simulation outputs support:
- player value projections
- draft pick valuation
- trade and transaction grading
- future standing projections
- Shiny app visualizations

---

# Methodology
Each major component follows a consistent structure:
**Purpose → Data Inputs → Methods → Outputs → Files**

## Scraping
Collect and prepare all raw data used throughout the project.

### Data Inputs
- NFL box score data (nflfastR)
- Sleeper API data (rosters, drafts, trades, transactions)
- Weekly Projections (Fantasy Football Today)
- Keep Trade Cut player values

### Methods
- Web scraping using httr, rvest, jsonlite
- Cleaning and merging data sources
- Helper functions stored in `Scrape Support.R`

### Outputs
- Cleaned datasets of player performance, league transactions, weekly projections, and keep trade cut values.

### Files
- `Scrape.R`
- `Scrape Support.R`

## Player Value Added
Quantify the value a player contributes relative to a position-specific replacement.

### Data Inputs
- Weekly fantasy scores
- Weekly projections (for determining replacement player)

### Methods
- Compute replacement-level score each week based on projections  
- Value added = player fantasy points – replacement-level fantasy points
- Players only earn value if started

### Outputs
- Weekly value added for each player

### Files
- `Player Value Added.R`

## Player Total Value
Model and simulate total future value for each player.

### Data Inputs
- Age  
- Position  
- Preseason Keep Trade Cut value  
- Historical value added  

### Methods
- BART model for projecting next-season value added  
- Second BART to project future KTC value
- Year-by-year simulation of a 15-year career  
- GAM to model heteroskedastic residual variance  
- Discount future seasons at 5% annually

### Outputs
- Posterior samples of future value (with uncertainty quanification)
- Future Value is median of simulated output over 8-year horizon

### Files
- `Player Total Value.R`

## Draft Pick Value
Estimate the expected value of a rookie draft pick at each position.

### Files
- `Player Total Value.R`

### Data Inputs
- Rookies' realized total value  
- Drafts

### Methods
- Bayesian polynomial regression (Heteroskedastic Metropolis–Hastings)
- Posterior samples provide uncertainty around pick value  

### Outputs
- Expected value curves for picks
- Variance estimates and uncertainty quantification

### Files
- `Draft Pick Value.R`  
- `MCMC Samplers.R`

## Future Standings
Project team standings in future seasons to support pick valuation and strategy.

### Data Inputs
- Posterior samples from player future value models  
- Current rosters

### Methods
- Aggregate projected total value added for each team  
- Simulate every remaining week 5000 times
- Compute final standings in each simulation

### Outputs
- Distribution of projected standings for future years  
- Probabilities of finishing in specific ranks

### Files
- `Future Standings.R`

# Updating Workflow

## Weekly Updates
- Update weekly scores and player valuations  
- Run `Frequent Updates.R` in `workflows/`  
- Typical runtime: ~20 minutes

## Yearly Updates
- Retrain future value models with new season data  
- Refresh draft pick valuation
- See `Yearly Updates.R` for details

---

# Roadmap

### In Progress
- Add recent games in Team Rankings
- Schedule table - perhaps in matchups?

## Near Future
- Standings odds over time
- Draft pick odds over time
- Trade grades over time
- Model Fits

## Distant Future
- Zero-inflated model to predict value added (especially for rookie next year production)
- Incorporate time series technique to model a players' career.
- Store past seasons instead of computing them over and over lol
- Automate code updating
- Streamline code for broader application

# References

1. [NFL Box Score Data](https://www.nflfastr.com)
2. [Sleeper API](https://docs.sleeper.com)
3. [Keep Trade Cut](https://keeptradecut.com)
4. [Fantasy Football Today](https://www.fftoday.com/rankings)



