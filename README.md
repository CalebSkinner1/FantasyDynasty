# FantasyDynasty
This repository is designed to host a website for the Baylor Seniors Fantasy Football Dynasty League.

I create a **value added** metric to measure fantasy players' value relative to their potential replacement.
This metric is designed to be league specific - players only contribute (or lose) value added if started in the lineup.

From here, I predict each player's value added in future years using the player's age and
pre-season keep trade cut value. With this model, I am able to draw samples from future years and
simulate each player's career. These simulations form a player's **future value**.

With these values, I go on to assign values to draft picks, predict future fantasy team's future success, and
grade drafts, trades, transaction in my league. Below is a more thorough explanation of the various tasks.
They are listed roughly in order of creation. 

# Files

## Scraping

These two files perform most of the web scraping and data wrangling I employ to obtain the data in this project.
For convenience, I place functions in Scrape Support.R. First, I use nflfastR to obtain box score data for the players. Then, I use
Sleeper API's to acquire information about the league (ownership, trades, acquisitions, drafts, etc.). I scrape player projections from
Fantasy Football Today and player value from Keep Trade Cut.

## Player Value Added

This file computes the value added for each player during each week of the season.
Value added is computed as a difference in a players' fantasy value and the value of a replacement level player.
The replacement level players are determined based on their pre-match projections.
In this file, I compute the value of the average replacement level player during each week. Each starter's value added
is the difference in their fantasy score and the average replacement level players' fantasy points.
This way, players' value added is dependent on the roster and timing of their fantasy scores.

## Player Total Value

Here, I model a players' future value added and future keep trade cut value with Bayesian Additive Regression Trees (BART).
At this point, I use the pre-season trade cut value, age, and position to predict the season's total value added. Then,
I use the pre-season keep trade cut value, age, position, and season's total value added to predict the post-season keep trade cut value.
Next, I iterate this year-after-year modeling to predict the next fifteen years of a player's career. In the future, I hope to
improve this model by utilizing a players' entire career arc. This will help identify longer trends and more accurately
project a players' career.

One advantage of BART is the ability to generate posterior samples. This naturally affords a way to identify
uncertainty quantification of a players' career arc. This gives us an idea of the potential directions a player's
career could take. Players will often have very different levels of uncertainty in their future value projections. For example,
young unproven players have much more uncertainty than established veterans. However, BART models often struggle with
heteroskedasticity (where the variance of the outcome is different across different predictor values).
I adjust for this by modeling the residuals of the BART projections with a Generalized Additive Model
(GAM). This allows the model to estimate the variance of each player flexibly.

Overall, I define a players' future value as the sum of the median expected value added over the next eight seasons.
In line with popular economic and financial models, each subsequent year is discounted by 5%. A players' total value over a period 
is the sum of any realized value in that period and their future value.

## Draft Pick Value

In this script, I am interested in the value of a rookie draft pick. I look at previous draft picks and model the current total value
of the players selected by their pick number. This indicates the expected worth of a draft pick before a player has been
selected. This is profoundly useful for evaluating trades and planning the future of a team. I use a Bayesian polynomial regression model.
Initially, I wrote a standard Gibbs sampler (thanks to Sosa and Aristizabal (2021) for finding the full conditional distributions) in MCMC Samplers.R,
but this sampler assumed equal variance across all draft picks. Of course, top draft picks have a higher potential and higher variance,
so I need a way to account for this heteroskedasticity. I allow the variance to vary with the draft pick and modify the previous algorithm to
a Metropolis-Hastings step. The posterior samples give uncertainty quantification and demonstrate some future possibilities.

## Future Standings

Here, I model future results of the league. This is particularly helpful for draft pick valuation. Since draft picks are generally
available for trade for the next three years, I use the posterior samples from the projected future total value added to
project the total value added for each fantasy team in a year. I then rank these projections over the course of 5000 simulations
to find the distribution of the final standings. Of course, scoring the most points does not necessary win you the league. I add some extra
randomness to the standings to account for this reality.

## Scripts

This folder hosts the functions and objects needed for the Shiny app to run.
All of the tables and graphs in the shiny app can be edited here. The name of script file corresponds to the tab on the
Shiny app. Script Support.R holds all the necessary functions for all of the files.

# Updating

## Weekly Updates

Several files can be updated regularly to account for the latest scores and player valuations.
To download and process weekly updates, please see the Frequent Updates.R file in the Workflows Folder.
This document should help weekly updates be a seemless task. Overall, it takes a little more than one minute to update these files.

## Yearly Updates

Several files need to be updated at a yearly rate. For example, at the completion of each season, the future value model
can be updated with new data and the draft pick valuation can be updated. See the Yearly Updates.R for more details.

# To Do

## near future ideas
2. Standings odds over time
3. draft pick odds over time
4. trade odds over time
5. Model Fit

## once season starts
1. Some code will surely break
2. Compute future value as percentage of remaining season

## distant future ideas
1. Zero-inflated model to predict value added (especially for rookie next year production)
2. Incorporate time series technique to model a players' career.
3. Add pictures to each player
4. Store past seasons instead of computing them over and over lol
5. automate code updating

# References

1. [NFL Box Score Data](https://www.nflfastr.com)
2. [Sleeper API](https://docs.sleeper.com)
3. [Keep Trade Cut](https://keeptradecut.com)
4. [Fantasy Football Today](https://www.fftoday.com/rankings)



