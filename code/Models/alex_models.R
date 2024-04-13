### File summary ###
# This file contains the construction of models that analyze the statistical difference among teams that
# are upset and teams that aren't over the course of 640 NCAA men's tournament games (2007-2019)

### Load data and libraries
library(tidyverse)
library(nnet)
library(hoopR)
library(janitor)
library(glmnet)
library(gam)
library(caret)

# Contains regular season average stats for the winners and losers of all matchups in the NCAA tournament (2007-2019)
df <- read_csv("./data/regular_season_stats_of_all_tournament_team_matchups_2007_2019.csv") %>%
  drop_na()

# Create variables with the difference in stats of winners and losers and the overall season stats
df <- df %>%
  mutate(victor_avg_points_season_plusminus = victor_avg_score - season_avg_score,
         loser_avg_points_season_plusminus = loser_avg_score - season_avg_score,
         victor_avg_opp_points_season_plusminus = victor_avg_opp_score - season_avg_opp_score,
         loser_avg_opp_points_season_plusminus = loser_avg_opp_score - season_avg_opp_score)

# Train-test-validation split
set.seed(17)
train_val_idx <- sample(nrow(df), 0.8*nrow(df))
train_val <- df[train_val_idx,]
test <- df[-train_val_idx,]
val_idx <- sample(nrow(train_val), 0.2*nrow(train_val))
val <- train_val[val_idx,]
train <- train_val[-val_idx,]
rm(train_val, train_val_idx, val_idx)

### Logistic Regression ###
# Outcome: Upset (1 if upset, 0 if not)

# Predictors of interest
preds <- c("upset",
           "victor_avg_points_season_plusminus",
           "loser_avg_points_season_plusminus",
           "victor_avg_opp_points_season_plusminus",
           "loser_avg_opp_points_season_plusminus")

# Logistic model of mean regular season points scored compared to the average of all tournament teams
model_logit <- glm(factor(upset) ~ ., data = train[preds], family = "binomial")
summary(model_logit)








