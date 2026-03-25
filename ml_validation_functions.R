# ML Validation Functions for Omics Dashboard
# Comprehensive model validation, metrics calculation, and visualization
# Author: GitHub Copilot
# Date: 2025-10-26

# ==============================================================================
# PERFORMANCE METRICS CALCULATION
# ==============================================================================

#' Calculate comprehensive binary classification metrics
#' @param predictions Predicted class labels or probabilities
#' @param actual Actual class labels
#' @param positive_class Name of the positive class
#' @return Data frame with metrics
calculate_binary_metrics <- function(predictions, actual, positive_class = NULL) {
  require(caret)
  
  # Convert to factors with same levels
  actual <- as.factor(actual)
  if (is.numeric(predictions)) {
    # Probabilities provided - use threshold
    predictions <- ifelse(predictions >= 0.5, levels(actual)[2], levels(actual)[1])
  }
  predictions <- factor(predictions, levels = levels(actual))
  
  # Create confusion matrix
  cm <- confusionMatrix(predictions, actual, positive = positive_class)
  
  # Extract metrics
  metrics <- data.frame(
    Metric = c("Accuracy", "Sensitivity (Recall)", "Specificity", 
               "Precision (PPV)", "NPV", "F1-Score", "Balanced Accuracy",
               "Kappa", "Detection Rate", "Detection Prevalence"),
    Value = c(
      cm$overall["Accuracy"],
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Pos Pred Value"],
      cm$byClass["Neg Pred Value"],
      cm$byClass["F1"],
      cm$byClass["Balanced Accuracy"],
      cm$overall["Kappa"],
      cm$byClass["Detection Rate"],
      cm$byClass["Detection Prevalence"]
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(
    metrics = metrics,
    confusion_matrix = cm$table,
    full_cm = cm
  ))
}

#' Calculate multi-class classification metrics
#' @param predictions Predicted class labels
#' @param actual Actual class labels
#' @return List with overall and per-class metrics
calculate_multiclass_metrics <- function(predictions, actual) {
  require(caret)
  
  # Convert to factors
  actual <- as.factor(actual)
  predictions <- factor(predictions, levels = levels(actual))
  
  # Overall confusion matrix
  cm <- confusionMatrix(predictions, actual)
  
  # Per-class metrics
  class_metrics <- data.frame(
    Class = rownames(cm$byClass),
    Sensitivity = cm$byClass[, "Sensitivity"],
    Specificity = cm$byClass[, "Specificity"],
    Precision = cm$byClass[, "Pos Pred Value"],
    F1 = cm$byClass[, "F1"],
    Balanced_Accuracy = cm$byClass[, "Balanced Accuracy"],
    stringsAsFactors = FALSE
  )
  
  # Overall metrics
  overall_metrics <- data.frame(
    Metric = c("Overall Accuracy", "Kappa", "Mean Sensitivity", 
               "Mean Specificity", "Mean Precision", "Mean F1"),
    Value = c(
      cm$overall["Accuracy"],
      cm$overall["Kappa"],
      mean(class_metrics$Sensitivity, na.rm = TRUE),
      mean(class_metrics$Specificity, na.rm = TRUE),
      mean(class_metrics$Precision, na.rm = TRUE),
      mean(class_metrics$F1, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(
    overall_metrics = overall_metrics,
    class_metrics = class_metrics,
    confusion_matrix = cm$table,
    full_cm = cm
  ))
}

#' Calculate regression metrics
#' @param predictions Predicted values
#' @param actual Actual values
#' @return Data frame with regression metrics
calculate_regression_metrics <- function(predictions, actual) {
  residuals <- actual - predictions
  
  # Calculate metrics
  mae <- mean(abs(residuals))
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  r2 <- 1 - (sum(residuals^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (length(actual) - 1) / (length(actual) - length(predictions) - 1))
  mape <- mean(abs(residuals / actual)) * 100
  
  metrics <- data.frame(
    Metric = c("MAE", "MSE", "RMSE", "R-squared", "Adj R-squared", "MAPE (%)"),
    Value = c(mae, mse, rmse, r2, adj_r2, mape),
    stringsAsFactors = FALSE
  )
  
  return(list(
    metrics = metrics,
    residuals = residuals,
    predictions = predictions,
    actual = actual
  ))
}

# ==============================================================================
# ROC AND AUC CALCULATION
# ==============================================================================

#' Calculate ROC curve and AUC with confidence intervals
#' @param predictions Predicted probabilities
#' @param actual Actual labels (binary)
#' @param n_bootstrap Number of bootstrap samples for CI
#' @return List with ROC data and AUC with CI
calculate_roc_auc <- function(predictions, actual, n_bootstrap = 1000) {
  require(pROC)
  
  # Calculate ROC
  roc_obj <- roc(actual, predictions, levels = rev(levels(as.factor(actual))))
  
  # Calculate AUC with CI
  auc_ci <- ci.auc(roc_obj, conf.level = 0.95, method = "bootstrap", boot.n = n_bootstrap)
  
  # Create data frame for plotting
  roc_data <- data.frame(
    Specificity = roc_obj$specificities,
    Sensitivity = roc_obj$sensitivities,
    Threshold = roc_obj$thresholds
  )
  
  return(list(
    roc_data = roc_data,
    auc = as.numeric(roc_obj$auc),
    auc_ci_lower = auc_ci[1],
    auc_ci_upper = auc_ci[3],
    roc_object = roc_obj
  ))
}

#' Calculate precision-recall curve
#' @param predictions Predicted probabilities
#' @param actual Actual labels (binary)
#' @return Data frame with precision-recall curve
calculate_pr_curve <- function(predictions, actual) {
  require(PRROC)
  
  # Convert to numeric (1 for positive, 0 for negative)
  actual_numeric <- as.numeric(actual) - 1
  
  # Calculate PR curve
  pr <- pr.curve(scores.class0 = predictions, weights.class0 = actual_numeric, curve = TRUE)
  
  pr_data <- data.frame(
    Recall = pr$curve[, 1],
    Precision = pr$curve[, 2],
    Threshold = pr$curve[, 3]
  )
  
  return(list(
    pr_data = pr_data,
    auc_pr = pr$auc.integral
  ))
}

# ==============================================================================
# VISUALIZATION FUNCTIONS
# ==============================================================================

#' Plot confusion matrix as heatmap
#' @param cm Confusion matrix
#' @param title Plot title
#' @return ggplot object
plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  require(ggplot2)
  require(reshape2)
  
  # Convert to data frame
  cm_df <- melt(cm)
  colnames(cm_df) <- c("Actual", "Predicted", "Count")
  
  # Calculate percentages
  cm_df$Percentage <- cm_df$Count / sum(cm_df$Count) * 100
  
  # Create plot
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", Count, Percentage)), 
              size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#e8f4f8", high = "#2980b9", name = "Count") +
    labs(title = title, x = "Predicted Class", y = "Actual Class") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11, face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_equal()
}

#' Plot ROC curve
#' @param roc_data Data frame with Specificity and Sensitivity
#' @param auc AUC value
#' @param auc_ci_lower Lower bound of AUC CI
#' @param auc_ci_upper Upper bound of AUC CI
#' @param title Plot title
#' @return ggplot object
plot_roc_curve <- function(roc_data, auc, auc_ci_lower, auc_ci_upper, title = "ROC Curve") {
  require(ggplot2)
  
  ggplot(roc_data, aes(x = 1 - Specificity, y = Sensitivity)) +
    geom_line(color = "#e74c3c", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    annotate("text", x = 0.7, y = 0.3, 
             label = sprintf("AUC = %.3f\n95%% CI: [%.3f, %.3f]", 
                             auc, auc_ci_lower, auc_ci_upper),
             size = 5, fontface = "bold", color = "#e74c3c") +
    labs(title = title, x = "False Positive Rate (1 - Specificity)", 
         y = "True Positive Rate (Sensitivity)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid = element_line(color = "gray90"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    ) +
    coord_equal() +
    xlim(0, 1) + ylim(0, 1)
}

#' Plot train vs test performance comparison
#' @param train_metrics Training metrics data frame
#' @param test_metrics Test metrics data frame
#' @return ggplot object
plot_train_test_comparison <- function(train_metrics, test_metrics) {
  require(ggplot2)
  require(dplyr)
  
  # Combine metrics
  comparison_df <- bind_rows(
    train_metrics %>% mutate(Set = "Training"),
    test_metrics %>% mutate(Set = "Testing")
  )
  
  # Filter to key metrics
  key_metrics <- c("Accuracy", "Sensitivity (Recall)", "Specificity", 
                   "Precision (PPV)", "F1-Score", "Balanced Accuracy")
  comparison_df <- comparison_df %>% filter(Metric %in% key_metrics)
  
  ggplot(comparison_df, aes(x = Metric, y = Value, fill = Set)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("Training" = "#3498db", "Testing" = "#e74c3c")) +
    labs(title = "Training vs Testing Performance",
         x = "Performance Metric", y = "Value") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 11),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, 1)
}

# ==============================================================================
# MODEL SAVING AND LOADING
# ==============================================================================

#' Save ML model with metadata
#' @param model Model object
#' @param model_name Model name
#' @param model_description Description
#' @param feature_names Feature names used
#' @param method ML method used
#' @param performance_metrics Performance metrics
#' @param file_path Path to save the model
#' @return TRUE if successful
save_ml_model <- function(model, model_name, model_description = "", 
                          feature_names = NULL, method = NULL, 
                          performance_metrics = NULL, file_path = NULL) {
  
  if (is.null(file_path)) {
    file_path <- paste0(model_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  }
  
  # Create model package
  model_package <- list(
    model = model,
    metadata = list(
      name = model_name,
      description = model_description,
      method = method,
      feature_names = feature_names,
      n_features = length(feature_names),
      created_date = Sys.time(),
      r_version = R.version.string,
      performance_metrics = performance_metrics
    )
  )
  
  # Save model
  saveRDS(model_package, file = file_path)
  
  cat("Model saved to:", file_path, "\n")
  return(TRUE)
}

#' Load ML model with metadata
#' @param file_path Path to model file
#' @return Model package with model and metadata
load_ml_model <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Model file not found: ", file_path)
  }
  
  model_package <- readRDS(file_path)
  
  cat("Model loaded successfully!\n")
  cat("Model name:", model_package$metadata$name, "\n")
  cat("Method:", model_package$metadata$method, "\n")
  cat("Features:", model_package$metadata$n_features, "\n")
  cat("Created:", format(model_package$metadata$created_date, "%Y-%m-%d %H:%M:%S"), "\n")
  
  return(model_package)
}
