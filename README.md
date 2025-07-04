# FantasyDynasty
This repository is designed to host a website for my Fantasy Football Dynasty League. In this repository,
I combine my interest in fantasy football with Bayesian Hierarchical Modeling, RShiny, intense data
wrangling, and creative modeling.

I create a *value added* metric, which finds the difference in each fantasy player's output
with the output of a replacement level player of their position.
Players only contribute (or lose) value added if started in the lineup.

From here, I predict each player's value added in future years using the player's age and
pre-season keep trade cut value. With this model, I am able to draw samples from future years and
simulate each player's career. These simulations form a player's *future value*.

With these values, I go on to assign values to draft picks, predict future fantasy team's future success, and
grade drafts, trades, transaction in my league. Below is a more thorough explanation of the various tasks.
They are listed roughly in order of creation. 

# Scraping

These two files perform most of the web scraping and data wrangling I employ to obtain the data in this project.
For convenience, I place functions in Scrape Support.R. First, I use nflfastR to obtain box score data for the players. Then, I use
Sleeper API's to acquire information about the league (ownership, trades, acquisitions, drafts, etc.). I scrape player projections from
Fantasy Football Today and player value from Keep Trade Cut.

# Player Value Added

This file computes the value added for each player during each week of the season.
Value added is computed as a difference in a players' fantasy value and the value of a replacement level player.
The replacement level players are determined based on their pre-match projections.
In this file, I compute the value of the average replacement level player during each week. Each starter's value added
is the difference in their fantasy score and the average replacement level players' fantasy points.
This way, players' value added is dependent on the roster and timing of their fantasy scores.

# Player Total Value

Here, I model a players' future value added and future keep trade cut value with Bayesian Additive Regression Trees (BART).
At this point, I use the pre-season trade cut value, age, and position to predict the season's total value added. Then,
I use the pre-season keep trade cut value, age, position, and season's total value added to predict the post-season keep trade cut value.
Next, I iterate this year-after-year modeling to predict the next fifteen years of a player's career. In the future, I hope to
improve this model by utilizing a players' entire career arc. This will help identify longer trends and more accurately
project a players' career.

One advantage of BART is the ability to generate posterior samples. This naturally affords a way to identify
uncertainty quantification of a players' career arc. This gives us an idea of the potential directions a player's
career could take. Overall, I define a players' future value as the sum of the median expected value added over the next eight seasons.
In line with popular economic and financial models, each subsequent year is discounted by 5%. A players' total value over a period 
is the sum of any realized value in that period and their future value.

# Draft Pick Value

In this script, I am interested in the value of a rookie draft pick. I look at previous draft picks and model the current total value
of the players selected by their pick number. This indicates the expected worth of a draft pick before a player has been
selected. This is profoundly useful for evaluating trades and planning the future of a team. I use a Bayesian polynomial regression model.
I write a standard Gibbs sampler in MCMC Samplers.R. Thanks to Sosa and Aristizabal (2021) for finding the full conditionals. I draw
posterior samples from the Bayesian polynomial regression to give uncertainty quantification and demonstrate some future possibilities.

# Future Standings

Here, I model future results of the league. This is particularly helpful for draft pick valuation. Since draft picks are generally
available for trade for the next three years, I use the posterior samples from the projected future total value added to
project the total value added for each fantasy team in a year. I then rank these projections over the course of 10000 simulations
to find the distribution of the final standings. Of course, scoring the most points does not necessary win you the league. In
the future, I hope to adjust these projections to account for this.

# Scripts

This folder hosts the functions and objects needed for the Shiny app to run.
All of the tables and graphs in the shiny app can be edited here. The name of script file corresponds to the tab on the
Shiny app. Script Support.R holds all the necessary functions for all of the files.

# References

1. [NFL Box Score Data](https://www.nflfastr.com)
2. [Sleeper API](https://docs.sleeper.com)
3. [Keep Trade Cut](https://keeptradecut.com)
4. [Fantasy Football Today](https://www.fftoday.com/rankings)

# weekly update
Several files can be updated regularly to account for the latest scores and player valuations.
To download and process weekly updates, please see the Frequent Updates.R file in the Workflows Folder.
This document should help weekly updates be a seemless task. Overall, it takes a little more than one minute to update these files.

# yearly update
Several files need to be updated at a yearly rate. For example, at the completion of each season, the future value model
can be updated with new data and the draft pick valuation can be updated. See the Yearly Updates.R for more details.

# future goals/updates - ideas
1. Zero-inflated model to predict value added (especially for rookie next year production)
2. Posterior predictive check for future value model
3. Adjust for heteroscedasticity in draft pick values
4. Account for variance of projections (widen intervals)
5. Incorporate time series technique to model a players' career.
6. check BART model
7. add pictures to each player
8. incorporate randomness into future standings projection

# To Do

## website
1. add remaining tabs

## once season starts
1. Some code will surely break
2. Compute future value as percentage of remaining season
3. Edit future standings to update as season progresses




