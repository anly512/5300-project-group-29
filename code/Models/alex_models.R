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
library(pROC)

# Contains regular season average stats for the winners and losers of all matchups in the NCAA tournament (2007-2019)
df <- read_csv("../../data/regular_season_stats_of_all_tournament_team_matchups_2007_2019.csv") %>%
  drop_na()

df <- df %>%
  mutate(upset_char = ifelse(upset == 0, "expected", "upset"))

# Create variables with the difference in stats of winners and losers and the overall season stats
#df <- df %>%
#  mutate(victor_avg_points_season_plusminus = victor_avg_score - season_avg_score,
#         loser_avg_points_season_plusminus = loser_avg_score - season_avg_score,
#         victor_avg_opp_points_season_plusminus = victor_avg_opp_score - season_avg_opp_score,
#         loser_avg_opp_points_season_plusminus = loser_avg_opp_score - season_avg_opp_score)

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

# Non predictors
nonpreds <- c("seed_t1",
              "seed_t2",
              "round",
              "season",
              "region_name",
              "name_t1",
              "name_t2",
              "victor",
              "loser",
              "score_t1",
              "score_t2",
              "victor_seed",
              "loser_seed",
              "region_number",
              "upset")

preds <- colnames(train)[!colnames(train) %in% nonpreds]
preds <- preds[!startsWith(preds, "season")]

# Logistic model of mean regular season points scored compared to the average of all tournament teams
# Logistic regression specs
specs <- trainControl(method = "cv",
                      number = 10,
                      savePredictions = "all",
                      classProbs = TRUE)

# Train model with cross validation
model_logit <- train(upset_char ~ .,
                     data = train[preds],
                     method = "glm",
                     family = "binomial",
                     trControl = specs)

rm(specs)

#model_logit <- glm(factor(upset) ~ ., data = train[preds], family = "binomial")
summary(model_logit)

# Predictions
logit_train_pred <- predict(model_logit, newdata = train[preds], type = "prob")
logit_val_pred <- predict(model_logit, newdata = val[preds], type = "prob")
logit_test_pred <- predict(model_logit, newdata = test[preds], type = "prob")
logit_train_pred$pred_class <- ifelse(logit_train_pred$upset > 0.5, 1, 0)
logit_test_pred$pred_class <- ifelse(logit_test_pred$upset > 0.5, 1, 0)
logit_val_pred$pred_class <- ifelse(logit_val_pred$upset > 0.5, 1, 0)

# ROC and AUC
logit_roc <- roc(ifelse(test$upset_char == "upset", 1, 0), logit_pred$pred_class, levels = c(0,1))
logit_auc <- auc(ifelse(test$upset_char == "upset", 1, 0), logit_pred$pred_class, levels = c(0,1))
plot.roc(logit_roc)

# Get accuracy
logit_train_acc <- mean(logit_train_pred$pred_class == train$upset)
logit_val_acc <- mean(logit_val_pred$pred_class == val$upset)
logit_test_acc <- mean(logit_test_pred$pred_class == test$upset)
print("Accuracies:")
print(paste("Training Accuracy",logit_train_acc))
print(paste("Validation Accuracy",logit_val_acc))
print(paste("Testing Accuracy",logit_test_acc))

# Nicer plot of ROC
roc_df <- coords(logit_roc, 'best', transpose = T)
roc_df <- data.frame(threshold = c(0, 0.5, 1),
                     specificity = nnet_roc$specificities,
                     sensitivity = nnet_roc$sensitivities)
roc_df
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add diagonal line for reference
  labs(title = "Logistic Regression ROC Curve", 
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       caption = "Data source: hoopR") +
  theme_bw()
ggsave("../../plots/Models/ROC_Curve_logit.png",
       height = 8,
       width = 16)

### Neural Networks ###
# Non predictors (these change since nnet wants a numeric output in the formula instead of character)
nonpreds <- c("seed_t1",
              "seed_t2",
              "round",
              "season",
              "region_name",
              "name_t1",
              "name_t2",
              "victor",
              "loser",
              "score_t1",
              "score_t2",
              "victor_seed",
              "loser_seed",
              "region_number",
              "upset_char")

preds <- colnames(train)[!colnames(train) %in% nonpreds]
preds <- preds[!startsWith(preds, "season")]

# Fit Neural Network
model_nnet <- nnet(upset ~ ., data = train[preds], size = 16, decay = 0.1)
#print("Optimal Weights:")
#print(model_nnet$wts)
# Predictions
nnet_train_pred <- data.frame(upset = predict(model_nnet, newdata = train[preds]))
nnet_val_pred <- data.frame(upset = predict(model_nnet, newdata = val[preds]))
nnet_test_pred <- data.frame(upset = predict(model_nnet, newdata = test[preds]))
nnet_train_pred$pred_class <- ifelse(nnet_train_pred$upset > 0.5, 1, 0)
nnet_test_pred$pred_class <- ifelse(nnet_test_pred$upset > 0.5, 1, 0)
nnet_val_pred$pred_class <- ifelse(nnet_val_pred$upset > 0.5, 1, 0)

# Get accuracies
nnet_train_acc <- mean(nnet_train_pred$pred_class == train$upset)
nnet_val_acc <- mean(nnet_val_pred$pred_class == val$upset)
nnet_test_acc <- mean(nnet_test_pred$pred_class == test$upset)
print("Accuracies:")
print(paste("Training Accuracy",nnet_train_acc))
print(paste("Validation Accuracy",nnet_val_acc))
print(paste("Testing Accuracy",nnet_test_acc))

# Calculate True Positives (TP) and False Positives (FP)
TP <- sum(nnet_test_pred == 1 & test$upset == 1)
FP <- sum(nnet_test_pred == 1 & test$upset == 0)

# ROC and AUC
nnet_roc <- roc(nnet_pred$pred_class, logit_pred$pred_class, levels = c(0,1))
nnet_auc <- auc(nnet_pred$pred_class, logit_pred$pred_class, levels = c(0,1))
plot.roc(nnet_roc)

# Nicer plot of ROC
roc_df <- coords(nnet_roc, 'best', transpose = T)
roc_df <- data.frame(threshold = c(0, 0.5, 1),
                     specificity = nnet_roc$specificities,
                     sensitivity = nnet_roc$sensitivities)
roc_df
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add diagonal line for reference
  labs(title = "Neural Network ROC Curve", 
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       caption = "Data source: hoopR") +
  theme_bw()
ggsave("../../plots/Models/ROC_Curve_nnet.png",
       height = 8,
       width = 16)
