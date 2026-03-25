# Enhanced Stability Selection for Random Forest Feature Selection
# Implements multiple stability methods specifically designed for RF importance

# 1. Enhanced Bootstrap Stability for Random Forest
perform_enhanced_bootstrap_stability_rf <- function(X, y, n_iterations = 100, 
                                                   importance_threshold = 0.01,
                                                   rf_params = list()) {
  
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required for RF stability analysis")
  }
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  feature_names <- colnames(X)
  
  # Validate parameters
  if (n_iterations < 10) {
    warning("n_iterations should be at least 10 for stable results")
    n_iterations <- max(10, n_iterations)
  }
  
  # Calculate minimum samples for bootstrap
  min_class_size <- if (is.factor(y)) min(table(y)) else n_samples
  max_samples <- floor(0.8 * n_samples)
  bootstrap_size <- min(max_samples, max(min_class_size * 2, floor(0.632 * n_samples)))
  
  cat("=== Enhanced Bootstrap Stability for Random Forest ===\n")
  cat("Iterations:", n_iterations, "\n")
  cat("Bootstrap size:", bootstrap_size, "/", n_samples, "\n")
  cat("Importance threshold:", importance_threshold, "\n")
  
  # Storage for results
  feature_selections <- matrix(FALSE, nrow = n_iterations, ncol = n_features)
  colnames(feature_selections) <- feature_names
  importance_matrix <- matrix(0, nrow = n_iterations, ncol = n_features)
  colnames(importance_matrix) <- feature_names
  performance_scores <- numeric(n_iterations)
  
  # Default RF parameters
  default_params <- list(
    ntree = 500,
    mtry = if (is.factor(y)) floor(sqrt(n_features)) else max(floor(n_features/3), 1),
    nodesize = if (is.factor(y)) 1 else 5,
    importance = TRUE
  )
  
  # Merge with user parameters
  rf_params <- modifyList(default_params, rf_params)
  
  for (i in 1:n_iterations) {
    tryCatch({
      # Performance-weighted bootstrap sampling
      if (i == 1) {
        # First iteration: standard bootstrap
        boot_indices <- sample(n_samples, size = bootstrap_size, replace = TRUE)
      } else {
        # Weight sampling based on previous performance
        weights <- exp(performance_scores[1:(i-1)] - max(performance_scores[1:(i-1)]))
        weights <- weights / sum(weights)
        
        # Create bootstrap sample with performance weighting
        boot_indices <- sample(n_samples, size = bootstrap_size, replace = TRUE, 
                              prob = rep(weights, length.out = n_samples))
      }
      
      X_boot <- X[boot_indices, , drop = FALSE]
      y_boot <- y[boot_indices]
      
      # Train Random Forest
      rf_args <- c(list(x = X_boot, y = y_boot), rf_params)
      rf_model <- do.call(randomForest::randomForest, rf_args)
      
      # Calculate out-of-bag performance for weighting
      if (is.factor(y)) {
        # Classification: use OOB error rate (convert to accuracy)
        oob_error <- rf_model$err.rate[rf_model$ntree, "OOB"]
        performance_scores[i] <- max(0, 1 - oob_error)
      } else {
        # Regression: use OOB R-squared
        oob_mse <- rf_model$mse[rf_model$ntree]
        total_var <- var(y_boot)
        performance_scores[i] <- max(0, 1 - oob_mse / total_var)
      }
      
      # Extract importance scores
      importance_scores <- randomForest::importance(rf_model, scale = TRUE)
      
      # Handle different importance matrix structures
      if (is.factor(y)) {
        # Classification: prefer MeanDecreaseAccuracy if available
        if ("MeanDecreaseAccuracy" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "MeanDecreaseAccuracy"]
        } else if ("MeanDecreaseGini" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "MeanDecreaseGini"]
        } else {
          imp_values <- importance_scores[, ncol(importance_scores)]
        }
      } else {
        # Regression: prefer %IncMSE if available
        if ("%IncMSE" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "%IncMSE"]
        } else {
          imp_values <- importance_scores[, 1]
        }
      }
      
      # Store importance scores
      importance_matrix[i, ] <- imp_values
      
      # Normalize importance scores to [0,1] range
      if (max(imp_values) > min(imp_values)) {
        normalized_imp <- (imp_values - min(imp_values)) / (max(imp_values) - min(imp_values))
      } else {
        normalized_imp <- rep(0.5, length(imp_values))
      }
      
      # Select features above threshold
      selected_features <- normalized_imp > importance_threshold
      feature_selections[i, ] <- selected_features
      
      if (i %% 10 == 0) {
        cat("Completed", i, "iterations\n")
      }
      
    }, error = function(e) {
      cat("Warning: Bootstrap iteration", i, "failed:", e$message, "\n")
      # Fill with zeros for failed iteration
      feature_selections[i, ] <- FALSE
      importance_matrix[i, ] <- 0
      performance_scores[i] <- 0
    })
  }
  
  # Calculate stability metrics
  selection_frequencies <- colMeans(feature_selections)
  mean_importance <- colMeans(importance_matrix)
  importance_stability <- apply(importance_matrix, 2, function(x) {
    if (sd(x) == 0) return(1)
    1 / (1 + sd(x) / (abs(mean(x)) + 1e-8))
  })
  
  # Create comprehensive results
  results <- data.frame(
    Feature = feature_names,
    Selection_Frequency = selection_frequencies,
    Mean_Importance = mean_importance,
    Importance_Stability = importance_stability,
    Stability_Score = selection_frequencies * importance_stability,
    stringsAsFactors = FALSE
  )
  
  # Sort by stability score
  results <- results[order(-results$Stability_Score), ]
  
  # Add metadata
  attr(results, "method") <- "Enhanced Bootstrap RF"
  attr(results, "n_iterations") <- n_iterations
  attr(results, "importance_threshold") <- importance_threshold
  attr(results, "mean_performance") <- mean(performance_scores)
  attr(results, "selection_matrix") <- feature_selections
  attr(results, "importance_matrix") <- importance_matrix
  
  cat("Bootstrap stability analysis completed\n")
  cat("Mean OOB performance:", round(mean(performance_scores), 4), "\n")
  
  return(results)
}

# 2. Subsampling Stability for Random Forest
perform_subsampling_stability_rf <- function(X, y, n_iterations = 100, 
                                           subsample_ratio = 0.7,
                                           importance_threshold = 0.01,
                                           rf_params = list()) {
  
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required for RF stability analysis")
  }
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  feature_names <- colnames(X)
  
  # Calculate subsample size
  subsample_size <- floor(subsample_ratio * n_samples)
  min_class_size <- if (is.factor(y)) min(table(y)) else n_samples
  
  # Ensure minimum representation of all classes
  if (is.factor(y)) {
    min_subsample_size <- length(levels(y)) * 2
    subsample_size <- max(subsample_size, min_subsample_size)
  }
  
  cat("=== Subsampling Stability for Random Forest ===\n")
  cat("Iterations:", n_iterations, "\n")
  cat("Subsample size:", subsample_size, "/", n_samples, "\n")
  cat("Subsample ratio:", round(subsample_size/n_samples, 3), "\n")
  
  # Storage for results
  feature_selections <- matrix(FALSE, nrow = n_iterations, ncol = n_features)
  colnames(feature_selections) <- feature_names
  importance_matrix <- matrix(0, nrow = n_iterations, ncol = n_features)
  colnames(importance_matrix) <- feature_names
  
  # Default RF parameters
  default_params <- list(
    ntree = 500,
    mtry = if (is.factor(y)) floor(sqrt(n_features)) else max(floor(n_features/3), 1),
    nodesize = if (is.factor(y)) 1 else 5,
    importance = TRUE
  )
  
  rf_params <- modifyList(default_params, rf_params)
  
  for (i in 1:n_iterations) {
    tryCatch({
      # Stratified subsampling
      if (is.factor(y)) {
        # Stratified sampling for classification
        subsample_indices <- c()
        for (level in levels(y)) {
          level_indices <- which(y == level)
          level_size <- floor(length(level_indices) * subsample_ratio)
          level_size <- max(level_size, 1)  # At least 1 sample per class
          selected_indices <- sample(level_indices, size = level_size, replace = FALSE)
          subsample_indices <- c(subsample_indices, selected_indices)
        }
      } else {
        # Random sampling for regression
        subsample_indices <- sample(n_samples, size = subsample_size, replace = FALSE)
      }
      
      X_sub <- X[subsample_indices, , drop = FALSE]
      y_sub <- y[subsample_indices]
      
      # Train Random Forest
      rf_args <- c(list(x = X_sub, y = y_sub), rf_params)
      rf_model <- do.call(randomForest::randomForest, rf_args)
      
      # Extract importance scores
      importance_scores <- randomForest::importance(rf_model, scale = TRUE)
      
      # Handle different importance matrix structures
      if (is.factor(y)) {
        if ("MeanDecreaseAccuracy" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "MeanDecreaseAccuracy"]
        } else if ("MeanDecreaseGini" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "MeanDecreaseGini"]
        } else {
          imp_values <- importance_scores[, ncol(importance_scores)]
        }
      } else {
        if ("%IncMSE" %in% colnames(importance_scores)) {
          imp_values <- importance_scores[, "%IncMSE"]
        } else {
          imp_values <- importance_scores[, 1]
        }
      }
      
      # Store importance scores
      importance_matrix[i, ] <- imp_values
      
      # Normalize and select features
      if (max(imp_values) > min(imp_values)) {
        normalized_imp <- (imp_values - min(imp_values)) / (max(imp_values) - min(imp_values))
      } else {
        normalized_imp <- rep(0.5, length(imp_values))
      }
      
      selected_features <- normalized_imp > importance_threshold
      feature_selections[i, ] <- selected_features
      
      if (i %% 10 == 0) {
        cat("Completed", i, "iterations\n")
      }
      
    }, error = function(e) {
      cat("Warning: Subsampling iteration", i, "failed:", e$message, "\n")
      feature_selections[i, ] <- FALSE
      importance_matrix[i, ] <- 0
    })
  }
  
  # Calculate stability metrics
  selection_frequencies <- colMeans(feature_selections)
  mean_importance <- colMeans(importance_matrix)
  importance_stability <- apply(importance_matrix, 2, function(x) {
    if (sd(x) == 0) return(1)
    1 / (1 + sd(x) / (abs(mean(x)) + 1e-8))
  })
  
  results <- data.frame(
    Feature = feature_names,
    Selection_Frequency = selection_frequencies,
    Mean_Importance = mean_importance,
    Importance_Stability = importance_stability,
    Stability_Score = selection_frequencies * importance_stability,
    stringsAsFactors = FALSE
  )
  
  results <- results[order(-results$Stability_Score), ]
  
  attr(results, "method") <- "Subsampling RF"
  attr(results, "n_iterations") <- n_iterations
  attr(results, "subsample_ratio") <- subsample_ratio
  attr(results, "selection_matrix") <- feature_selections
  attr(results, "importance_matrix") <- importance_matrix
  
  cat("Subsampling stability analysis completed\n")
  
  return(results)
}

# 3. Cross-Validation Stability for Random Forest
perform_cv_stability_rf <- function(X, y, n_folds = 10, n_repeats = 10,
                                   importance_threshold = 0.01,
                                   rf_params = list()) {
  
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required for RF stability analysis")
  }
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  feature_names <- colnames(X)
  
  # Validate parameters
  n_folds <- min(n_folds, n_samples)
  if (is.factor(y)) {
    min_class_size <- min(table(y))
    n_folds <- min(n_folds, min_class_size)
  }
  
  total_iterations <- n_folds * n_repeats
  
  cat("=== Cross-Validation Stability for Random Forest ===\n")
  cat("Folds:", n_folds, "Repeats:", n_repeats, "Total iterations:", total_iterations, "\n")
  
  # Storage for results
  feature_selections <- matrix(FALSE, nrow = total_iterations, ncol = n_features)
  colnames(feature_selections) <- feature_names
  importance_matrix <- matrix(0, nrow = total_iterations, ncol = n_features)
  colnames(importance_matrix) <- feature_names
  
  # Default RF parameters
  default_params <- list(
    ntree = 500,
    mtry = if (is.factor(y)) floor(sqrt(n_features)) else max(floor(n_features/3), 1),
    nodesize = if (is.factor(y)) 1 else 5,
    importance = TRUE
  )
  
  rf_params <- modifyList(default_params, rf_params)
  
  iteration <- 1
  
  for (repeat_i in 1:n_repeats) {
    # Create fold indices
    if (is.factor(y)) {
      # Stratified CV for classification
      fold_indices <- createStratifiedFolds(y, k = n_folds)
    } else {
      # Random CV for regression
      fold_indices <- sample(rep(1:n_folds, length.out = n_samples))
    }
    
    for (fold in 1:n_folds) {
      tryCatch({
        # Create training set (exclude current fold)
        train_indices <- which(fold_indices != fold)
        X_train <- X[train_indices, , drop = FALSE]
        y_train <- y[train_indices]
        
        # Train Random Forest
        rf_args <- c(list(x = X_train, y = y_train), rf_params)
        rf_model <- do.call(randomForest::randomForest, rf_args)
        
        # Extract importance scores
        importance_scores <- randomForest::importance(rf_model, scale = TRUE)
        
        # Handle different importance matrix structures
        if (is.factor(y)) {
          if ("MeanDecreaseAccuracy" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "MeanDecreaseAccuracy"]
          } else if ("MeanDecreaseGini" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "MeanDecreaseGini"]
          } else {
            imp_values <- importance_scores[, ncol(importance_scores)]
          }
        } else {
          if ("%IncMSE" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "%IncMSE"]
          } else {
            imp_values <- importance_scores[, 1]
          }
        }
        
        # Store importance scores
        importance_matrix[iteration, ] <- imp_values
        
        # Normalize and select features
        if (max(imp_values) > min(imp_values)) {
          normalized_imp <- (imp_values - min(imp_values)) / (max(imp_values) - min(imp_values))
        } else {
          normalized_imp <- rep(0.5, length(imp_values))
        }
        
        selected_features <- normalized_imp > importance_threshold
        feature_selections[iteration, ] <- selected_features
        
        iteration <- iteration + 1
        
        if (iteration %% 10 == 0) {
          cat("Completed", iteration, "iterations\n")
        }
        
      }, error = function(e) {
        cat("Warning: CV iteration", iteration, "failed:", e$message, "\n")
        feature_selections[iteration, ] <- FALSE
        importance_matrix[iteration, ] <- 0
        iteration <<- iteration + 1
      })
    }
  }
  
  # Calculate stability metrics
  selection_frequencies <- colMeans(feature_selections)
  mean_importance <- colMeans(importance_matrix)
  importance_stability <- apply(importance_matrix, 2, function(x) {
    if (sd(x) == 0) return(1)
    1 / (1 + sd(x) / (abs(mean(x)) + 1e-8))
  })
  
  results <- data.frame(
    Feature = feature_names,
    Selection_Frequency = selection_frequencies,
    Mean_Importance = mean_importance,
    Importance_Stability = importance_stability,
    Stability_Score = selection_frequencies * importance_stability,
    stringsAsFactors = FALSE
  )
  
  results <- results[order(-results$Stability_Score), ]
  
  attr(results, "method") <- "Cross-Validation RF"
  attr(results, "n_folds") <- n_folds
  attr(results, "n_repeats") <- n_repeats
  attr(results, "selection_matrix") <- feature_selections
  attr(results, "importance_matrix") <- importance_matrix
  
  cat("Cross-validation stability analysis completed\n")
  
  return(results)
}

# 4. Noise Injection Stability for Random Forest
perform_noise_stability_rf <- function(X, y, n_iterations = 100,
                                      noise_levels = c(0.01, 0.05, 0.1),
                                      importance_threshold = 0.01,
                                      rf_params = list()) {
  
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required for RF stability analysis")
  }
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  feature_names <- colnames(X)
  
  total_iterations <- n_iterations * length(noise_levels)
  
  cat("=== Noise Injection Stability for Random Forest ===\n")
  cat("Iterations per noise level:", n_iterations, "\n")
  cat("Noise levels:", paste(noise_levels, collapse = ", "), "\n")
  cat("Total iterations:", total_iterations, "\n")
  
  # Storage for results
  feature_selections <- matrix(FALSE, nrow = total_iterations, ncol = n_features)
  colnames(feature_selections) <- feature_names
  importance_matrix <- matrix(0, nrow = total_iterations, ncol = n_features)
  colnames(importance_matrix) <- feature_names
  
  # Calculate feature standard deviations for noise scaling
  feature_sds <- apply(X, 2, sd)
  
  # Default RF parameters
  default_params <- list(
    ntree = 500,
    mtry = if (is.factor(y)) floor(sqrt(n_features)) else max(floor(n_features/3), 1),
    nodesize = if (is.factor(y)) 1 else 5,
    importance = TRUE
  )
  
  rf_params <- modifyList(default_params, rf_params)
  
  iteration <- 1
  
  for (noise_level in noise_levels) {
    cat("Testing noise level:", noise_level, "\n")
    
    for (i in 1:n_iterations) {
      tryCatch({
        # Add Gaussian noise to features
        X_noisy <- X
        for (j in 1:n_features) {
          noise <- rnorm(n_samples, mean = 0, sd = noise_level * feature_sds[j])
          X_noisy[, j] <- X[, j] + noise
        }
        
        # Train Random Forest on noisy data
        rf_args <- c(list(x = X_noisy, y = y), rf_params)
        rf_model <- do.call(randomForest::randomForest, rf_args)
        
        # Extract importance scores
        importance_scores <- randomForest::importance(rf_model, scale = TRUE)
        
        # Handle different importance matrix structures
        if (is.factor(y)) {
          if ("MeanDecreaseAccuracy" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "MeanDecreaseAccuracy"]
          } else if ("MeanDecreaseGini" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "MeanDecreaseGini"]
          } else {
            imp_values <- importance_scores[, ncol(importance_scores)]
          }
        } else {
          if ("%IncMSE" %in% colnames(importance_scores)) {
            imp_values <- importance_scores[, "%IncMSE"]
          } else {
            imp_values <- importance_scores[, 1]
          }
        }
        
        # Store importance scores
        importance_matrix[iteration, ] <- imp_values
        
        # Normalize and select features
        if (max(imp_values) > min(imp_values)) {
          normalized_imp <- (imp_values - min(imp_values)) / (max(imp_values) - min(imp_values))
        } else {
          normalized_imp <- rep(0.5, length(imp_values))
        }
        
        selected_features <- normalized_imp > importance_threshold
        feature_selections[iteration, ] <- selected_features
        
        iteration <- iteration + 1
        
      }, error = function(e) {
        cat("Warning: Noise iteration", iteration, "failed:", e$message, "\n")
        feature_selections[iteration, ] <- FALSE
        importance_matrix[iteration, ] <- 0
        iteration <<- iteration + 1
      })
    }
  }
  
  # Calculate stability metrics
  selection_frequencies <- colMeans(feature_selections)
  mean_importance <- colMeans(importance_matrix)
  importance_stability <- apply(importance_matrix, 2, function(x) {
    if (sd(x) == 0) return(1)
    1 / (1 + sd(x) / (abs(mean(x)) + 1e-8))
  })
  
  results <- data.frame(
    Feature = feature_names,
    Selection_Frequency = selection_frequencies,
    Mean_Importance = mean_importance,
    Importance_Stability = importance_stability,
    Stability_Score = selection_frequencies * importance_stability,
    stringsAsFactors = FALSE
  )
  
  results <- results[order(-results$Stability_Score), ]
  
  attr(results, "method") <- "Noise Injection RF"
  attr(results, "n_iterations") <- n_iterations
  attr(results, "noise_levels") <- noise_levels
  attr(results, "selection_matrix") <- feature_selections
  attr(results, "importance_matrix") <- importance_matrix
  
  cat("Noise injection stability analysis completed\n")
  
  return(results)
}

# Helper function for stratified CV folds
createStratifiedFolds <- function(y, k) {
  if (!is.factor(y)) {
    stop("Stratified folds require factor response variable")
  }
  
  n <- length(y)
  folds <- rep(0, n)
  
  for (level in levels(y)) {
    level_indices <- which(y == level)
    level_folds <- sample(rep(1:k, length.out = length(level_indices)))
    folds[level_indices] <- level_folds
  }
  
  return(folds)
}

# Main wrapper function for RF stability analysis
perform_rf_stability_analysis <- function(X, y, method = "bootstrap", 
                                         n_iterations = 100, rf_params = list(), ...) {
  
  cat("=== Random Forest Stability Analysis ===\n")
  cat("Method:", method, "\n")
  cat("Features:", ncol(X), "Samples:", nrow(X), "\n")
  
  switch(method,
    "bootstrap" = perform_enhanced_bootstrap_stability_rf(X, y, n_iterations, rf_params = rf_params, ...),
    "subsampling" = perform_subsampling_stability_rf(X, y, n_iterations, rf_params = rf_params, ...),
    "cv" = perform_cv_stability_rf(X, y, rf_params = rf_params, ...),
    "noise" = perform_noise_stability_rf(X, y, n_iterations, rf_params = rf_params, ...),
    stop("Unknown RF stability method: ", method)
  )
}
