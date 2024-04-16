# Contains regular season average stats for the winners and losers of all matchups in the NCAA tournament (2007-2019)
df <- read_csv("../../data/regular_season_stats_of_all_tournament_team_matchups_2007_2019.csv") %>%
  drop_na()

df <- df %>%
  mutate(upset_char = ifelse(upset == 0, "expected", "upset"))

# Load required libraries
library(caret)
library(e1071)  # For SVM
library(ROCR)   # For ROC analysis
library(ggplot2)

# Define predictors and outcome variable
nonpreds <- c("seed_t1", "seed_t2", "round", "season", "region_name", "name_t1", "name_t2", "victor", "loser", "score_t1", "score_t2", "victor_seed", "loser_seed", "region_number", "upset")
preds <- colnames(train)[!colnames(train) %in% nonpreds]
preds <- preds[!startsWith(preds, "season")]

# Define the tuning grid with the range of tuning hyperparameters
tuneGrid <- expand.grid(C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))

# Train SVM model with cross-validation
ctrl <- trainControl(method = "cv",
                     number = 10,
                     savePredictions = "all",
                     classProbs = TRUE)
model_svm <- train(upset_char ~ .,
                   data = train[preds],
                   method = "svmLinear",
                   tuneGrid = tuneGrid,
                   trControl = ctrl)

# Summary of the model
summary(model_svm)

# Predictions
svm_train_pred <- predict(model_svm, newdata = train[preds], type = "prob")
svm_val_pred <- predict(model_svm, newdata = val[preds], type = "prob")
svm_test_pred <- predict(model_svm, newdata = test[preds], type = "prob")
svm_train_pred$pred_class <- ifelse(svm_train_pred$upset > 0.5, 1, 0)
svm_val_pred$pred_class <- ifelse(svm_val_pred$upset > 0.5, 1, 0)
svm_test_pred$pred_class <- ifelse(svm_test_pred$upset > 0.5, 1, 0)

# ROC and AUC
svm_roc <- roc(ifelse(test$upset_char == "upset", 1, 0), svm_test_pred$pred_class)
svm_auc <- auc(ifelse(test$upset_char == "upset", 1, 0), svm_test_pred$pred_class)
plot(svm_roc)

# Get accuracy
svm_train_acc <- mean(svm_train_pred$pred_class == train$upset)
svm_val_acc <- mean(svm_val_pred$pred_class == val$upset)
svm_test_acc <- mean(svm_test_pred$pred_class == test$upset)
print("Accuracies:")
print(paste("Training Accuracy", svm_train_acc))
print(paste("Validation Accuracy", svm_val_acc))
print(paste("Testing Accuracy", svm_test_acc))

# Calculate precision
svm_train_precision <- sum(svm_train_pred$pred_class == 1 & train$upset == 1) / sum(svm_train_pred$pred_class == 1)
svm_val_precision <- sum(svm_val_pred$pred_class == 1 & val$upset == 1) / sum(svm_val_pred$pred_class == 1)
svm_test_precision <- sum(svm_test_pred$pred_class == 1 & test$upset == 1) / sum(svm_test_pred$pred_class == 1)

print("Precision:")
print(paste("Training Precision:", svm_train_precision))
print(paste("Validation Precision:", svm_val_precision))
print(paste("Testing Precision:", svm_test_precision))

# Nicer plot of ROC

# Extract coordinates from ROC object
roc_df <- coords(svm_roc, 'best', transpose = TRUE)

# Create a data frame for ROC curve
roc_df <- data.frame(
  threshold = c(0, 0.5, 1),
  specificity = svm_roc$specificities,
  sensitivity = svm_roc$sensitivities
)

# Plot ROC curve
roc_plot <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add diagonal line for reference
  labs(
    title = "SVM ROC Curve",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    caption = "Data source: hoopR"
  )+
  theme_bw()
ggsave("../../plots/Models/ROC_Curve_SVM.png",
       height = 8,
       width = 16)
