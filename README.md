# FantasyDynasty
This repository is designed to host a website for my Fantasy Football Dynasty League. In this repository,
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

These two files perform most of the web scraping and data wrangling I employ to obtain the data in this project.
For convenience, I place functions in Scrape Support.R. First, I use nflfastR to obtain box score data for the players. Then, I use
Sleeper API's to acquire information about the league (ownership, trades, acquisitions, drafts, etc.). I scrape player projections from
Fantasy Football Today and player value from Keep Trade Cut.


# Player Value Added



# Player Total Value

## Stan Files

# Draft Pick Value

# Individual Players

# Fantasy Teams

# References

1. [NFL Box Score Data](https://www.nflfastr.com)
2. [Sleeper API](https://docs.sleeper.com)
3. [Keep Trade Cut](https://keeptradecut.com)
4. [Fantasy Football Today](https://www.fftoday.com/rankings)

# weekly update

# yearly update

# future goals/updates - ideas
1. Zero-inflated model to predict value added (especially for rookie next year production)
2. Posterior predictive check for future value model
4. Adjust for heteroscedasticity in draft pick values

# To Do

## content
DONE

## edit content
1. modeling DONE
2. scripts DONE
3. app


## website
1. shiny stuff

## housekeeping
1. readme lol
2. weekly update how to (for future self)
3. future goals
4. account for variance of projections (widen intervals)
5. improve efficiency of code
6. once season starts, some code will surely break
7. once season starts, compute future value as percentage of remaining season




