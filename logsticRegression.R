library(foreign)
library(ROSE)
library(pROC)
library(caret)
library(ggplot2)
library(haven)

data <- read_dta("C:/Users/shiva/Downloads/combined.dta")

predictors <- names(data)[names(data) != "CP"]
X <- as.matrix(data[, predictors])
y <- data$CP

base_model <- glm(CP ~ ., family = binomial(link = "logit"), data = data)

probs <- predict(base_model, type = "response")

thresholds <- seq(0.3, 0.7, by = 0.01)
threshold_results <- data.frame(
  threshold = thresholds,
  accuracy = numeric(length(thresholds)),
  sensitivity = numeric(length(thresholds)),
  specificity = numeric(length(thresholds)),
  balanced_accuracy = numeric(length(thresholds))
)

for (i in 1:length(thresholds)) {
  pred_classes <- ifelse(probs > thresholds[i], 1, 0)
  cm <- confusionMatrix(factor(pred_classes), factor(data$CP), positive = "1")
  
  threshold_results$accuracy[i] <- cm$overall["Accuracy"]
  threshold_results$sensitivity[i] <- cm$byClass["Sensitivity"]
  threshold_results$specificity[i] <- cm$byClass["Specificity"]
  threshold_results$balanced_accuracy[i] <- (cm$byClass["Sensitivity"] + cm$byClass["Specificity"]) / 2
}

optimal_idx <- which.max(threshold_results$balanced_accuracy)
optimal_threshold <- threshold_results$threshold[optimal_idx]
print(paste("Optimal threshold:", optimal_threshold))

optimal_pred <- ifelse(probs > optimal_threshold, 1, 0)
optimal_cm <- confusionMatrix(factor(optimal_pred), factor(data$CP), positive = "1")
print("Confusion Matrix with Optimal Threshold:")
print(optimal_cm$table)
print(paste("Accuracy with optimal threshold:", round(optimal_cm$overall["Accuracy"], 4)))
print(paste("Sensitivity with optimal threshold:", round(optimal_cm$byClass["Sensitivity"], 4)))
print(paste("Specificity with optimal threshold:", round(optimal_cm$byClass["Specificity"], 4)))

roc_obj <- roc(data$CP, probs)
auc_base <- auc(roc_obj)
print(paste("AUC for model:", round(auc_base, 4)))


roc_data_original <- data.frame(
  Specificity = 1 - roc_obj$specificities,
  Sensitivity = roc_obj$sensitivities,
  Model = "Original Model"
)





final_metrics <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Balanced Accuracy", "AUC"),
  Original_Model = c(
    round(optimal_cm$overall["Accuracy"], 4),
    round(optimal_cm$byClass["Sensitivity"], 4),
    round(optimal_cm$byClass["Specificity"], 4),
    round((optimal_cm$byClass["Sensitivity"] + optimal_cm$byClass["Specificity"])/2, 4),
    round(auc_base, 4)
  )
)

print("Comparison of Model Performance:")
print(final_metrics)