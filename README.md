# FantasyDynasty
This repository is designed to host a website for my Dynasty Fantasy Football League. In this repository,
I combine my interest in fantasy football with Bayesian Hierarchical Modeling, RShiny, intense data
wrangling, and creative modeling.

I create a *value added* metric, which finds the difference in each fantasy player's output
with the output of a replacement level player of their position.
Players only contribute (or lose) value added if started in the lineup.

From here, I employ a bayesian hierarchical polynomial regression model to predict each player's value added in future years.
To keep things simple, I limit predictors to age and player's keep trade cut value. With this model,
I am able to draw samples from future years and simulate each player's career. These simulations form
a player's *future value*.

With these values, I go on to assign values to draft picks, predict future fantasy team's future success, and
grade drafts, trades, transaction in my league. Below is a more thorough explanation of the various tasks.
They are listed roughly in order of creation. 

# Scraping

These two files host most of the web scraping and data wrangling I employ to obtain the data in this project.
For convenience, I place functions in Scrape Functions. First, I use nflfastR to obtain box score data for the players.


# Player Value Added

# Player Total Value

## Stan Files

# Draft Pick Value

# Individual Players

# Fantasy Teams

# References

1. [NFL Box Score Data](https://www.nflfastr.com)

# weekly update

# yearly update

# future goals
1. zero-inflated model to predict value added (especially for rookie next year production)
2. update bayesian model with correct hyperpriors (https://arxiv.org/pdf/2110.10565)
3. use bayesian model to predict rookie values (allow for variation when drawing samples)



# To Do

## content
1. fantasy teams
2. draft grades
3. trade grades
4. transaction grades

## website
1. shiny stuff

## housekeeping
1. readme lol
2. weekly update how to (for future self)
3. future goals




