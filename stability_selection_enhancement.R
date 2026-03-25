# Enhanced Stability Selection for LASSO/Elastic Net
# This function implements multiple stability methods

# Load required packages
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(glmnet)
library(dplyr)

# 1. Stability Selection (Meinshausen & Bühlmann, 2010)
perform_stability_selection <- function(X, y, alpha = 1, family = "gaussian", 
                                      n_bootstrap = 100, pi_thr = 0.6, 
                                      subsample_size = 0.5, lambda_grid = NULL,
                                      feature_names = NULL) {
  
  # Set up parameters
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Use provided feature names or create defaults
  if (is.null(feature_names)) {
    feature_names <- colnames(X)
    if (is.null(feature_names)) {
      feature_names <- paste0("Feature_", 1:n_features)
    }
  } else {
    # Ensure feature_names length matches number of features
    if (length(feature_names) != n_features) {
      warning("Feature names length doesn't match number of features. Using default names.")
      feature_names <- paste0("Feature_", 1:n_features)
    }
  }
  
  # Create lambda grid if not provided
  if (is.null(lambda_grid)) {
    # Fit initial model to get reasonable lambda range
    initial_fit <- glmnet(X, y, family = family, alpha = alpha)
    lambda_grid <- exp(seq(log(max(initial_fit$lambda)), 
                          log(min(initial_fit$lambda)), 
                          length.out = 50))
  }
  
  # Initialize selection matrix: bootstrap x feature x lambda
  selection_tensor <- array(0, dim = c(n_bootstrap, n_features, length(lambda_grid)))
  dimnames(selection_tensor) <- list(
    paste0("Bootstrap_", 1:n_bootstrap),
    feature_names,
    paste0("Lambda_", round(lambda_grid, 6))
  )
  
  # Bootstrap sampling and model fitting
  for (b in 1:n_bootstrap) {
    # Subsample without replacement
    subsample_indices <- sample(1:n_samples, 
                               size = floor(n_samples * subsample_size), 
                               replace = FALSE)
    
    X_sub <- X[subsample_indices, , drop = FALSE]
    y_sub <- y[subsample_indices]
    
    # Check for sufficient variation
    if (is.factor(y_sub) && length(unique(y_sub)) < 2) next
    if (is.numeric(y_sub) && var(y_sub) == 0) next
    
    # Fit LASSO/Elastic Net for the lambda grid
    tryCatch({
      fit_sub <- glmnet(X_sub, y_sub, family = family, alpha = alpha, 
                       lambda = lambda_grid, standardize = TRUE)
      
      # Extract coefficient matrix
      coef_matrix <- as.matrix(coef(fit_sub))
      
      # Remove intercept
      if (nrow(coef_matrix) > n_features) {
        coef_matrix <- coef_matrix[-1, , drop = FALSE]
      }
      
      # Record selections (non-zero coefficients)
      for (l in 1:length(lambda_grid)) {
        selected_features <- abs(coef_matrix[, l]) > 1e-8
        selection_tensor[b, selected_features, l] <- 1
      }
      
    }, error = function(e) {
      cat("Bootstrap", b, "failed:", e$message, "\n")
    })
    
    if (b %% 20 == 0) cat("Completed", b, "bootstrap samples\n")
  }
  
  # Calculate selection frequencies for each lambda
  selection_frequencies <- apply(selection_tensor, c(2, 3), mean, na.rm = TRUE)
  
  # Find stable features (above threshold across lambda path)
  stable_features <- list()
  for (l in 1:length(lambda_grid)) {
    stable_idx <- selection_frequencies[, l] >= pi_thr
    stable_features[[l]] <- list(
      lambda = lambda_grid[l],
      features = feature_names[stable_idx],
      frequencies = selection_frequencies[stable_idx, l]
    )
  }
  
  # Find the most parsimonious stable set
  stable_counts <- sapply(stable_features, function(x) length(x$features))
  if (any(stable_counts > 0)) {
    # Choose lambda that gives smallest stable set with at least one feature
    valid_lambdas <- which(stable_counts > 0)
    optimal_idx <- valid_lambdas[which.min(stable_counts[valid_lambdas])]
    
    final_stable_set <- stable_features[[optimal_idx]]
  } else {
    # No stable features found, return empty set
    final_stable_set <- list(
      lambda = lambda_grid[1],
      features = character(0),
      frequencies = numeric(0)
    )
  }
  
  return(list(
    stable_features = final_stable_set,
    selection_frequencies = selection_frequencies,
    lambda_grid = lambda_grid,
    all_stable_sets = stable_features,
    parameters = list(
      pi_thr = pi_thr,
      n_bootstrap = n_bootstrap,
      subsample_size = subsample_size,
      alpha = alpha,
      family = family
    )
  ))
}

# 2. Randomized LASSO (Wang et al., 2011)
perform_randomized_lasso <- function(X, y, alpha = 1, family = "gaussian",
                                   n_bootstrap = 100, lambda_selection = "1se",
                                   scaling_factor = 0.9, feature_names = NULL) {
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Use provided feature names or create defaults
  if (is.null(feature_names)) {
    feature_names <- colnames(X)
    if (is.null(feature_names)) {
      feature_names <- paste0("Feature_", 1:n_features)
    }
  } else {
    # Ensure feature_names length matches number of features
    if (length(feature_names) != n_features) {
      warning("Feature names length doesn't match number of features. Using default names.")
      feature_names <- paste0("Feature_", 1:n_features)
    }
  }
  
  # Matrix to store selection results
  selection_matrix <- matrix(0, nrow = n_bootstrap, ncol = n_features)
  colnames(selection_matrix) <- feature_names
  
  for (b in 1:n_bootstrap) {
    # Bootstrap sample
    boot_indices <- sample(1:n_samples, size = n_samples, replace = TRUE)
    X_boot <- X[boot_indices, , drop = FALSE]
    y_boot <- y[boot_indices]
    
    # Random scaling of features (key innovation of randomized LASSO)
    random_weights <- runif(n_features, min = scaling_factor, max = 1)
    X_scaled <- sweep(X_boot, 2, random_weights, "*")
    
    # Check for variation and minimum sample requirements
    if (is.factor(y_boot) && length(unique(y_boot)) < 2) next
    if (is.numeric(y_boot) && var(y_boot) == 0) next
    
    # Check if we have enough samples for cross-validation
    if (length(y_boot) < 6) next
    
    # For classification, ensure we have at least 2 samples per class for CV
    if (family == "binomial") {
      class_counts <- table(y_boot)
      if (any(class_counts < 2)) next
    }
    
    # Fit CV LASSO
    tryCatch({
      # Calculate appropriate number of folds
      max_folds <- if (family == "binomial") {
        min(10, floor(min(table(y_boot)) * 2))
      } else {
        min(10, length(y_boot))
      }
      
      nfolds_use <- max(3, min(max_folds, 10))
      
      # Additional safety check
      if (nfolds_use < 3 || length(y_boot) < nfolds_use) next
      
      cv_fit <- cv.glmnet(X_scaled, y_boot, family = family, alpha = alpha, 
                         standardize = TRUE, nfolds = nfolds_use)
      
      # Select lambda
      lambda_use <- if (lambda_selection == "1se") cv_fit$lambda.1se else cv_fit$lambda.min
      
      # Get coefficients
      coefs <- coef(cv_fit, s = lambda_use)
      
      # Adjust coefficients for random scaling
      if (length(coefs) > 1) {
        feature_coefs <- as.numeric(coefs[-1])  # Remove intercept
        adjusted_coefs <- feature_coefs / random_weights
        
        # Record selections
        selected_idx <- abs(adjusted_coefs) > 1e-8
        selection_matrix[b, selected_idx] <- 1
      }
      
    }, error = function(e) {
      cat("Randomized LASSO bootstrap", b, "failed:", e$message, "\n")
    })
  }
  
  # Calculate selection frequencies
  selection_frequencies <- colMeans(selection_matrix)
  
  # Sort by frequency
  freq_order <- order(selection_frequencies, decreasing = TRUE)
  
  results <- data.frame(
    Feature = feature_names[freq_order],
    SelectionFrequency = selection_frequencies[freq_order],
    stringsAsFactors = FALSE
  )
  
  return(list(
    selection_frequencies = results,
    selection_matrix = selection_matrix,
    parameters = list(
      n_bootstrap = n_bootstrap,
      scaling_factor = scaling_factor,
      lambda_selection = lambda_selection,
      alpha = alpha,
      family = family
    )
  ))
}

# 3. Enhanced Bootstrap with Multiple Performance Metrics
perform_enhanced_bootstrap_stability <- function(X, y, alpha = 1, family = "gaussian",
                                               n_bootstrap = 100, lambda_selection = "1se",
                                               performance_metric = "auto", feature_names = NULL) {
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Use provided feature names or create defaults
  if (is.null(feature_names)) {
    feature_names <- colnames(X)
    if (is.null(feature_names)) {
      feature_names <- paste0("Feature_", 1:n_features)
    }
  } else {
    # Ensure feature_names length matches number of features
    if (length(feature_names) != n_features) {
      warning("Feature names length doesn't match number of features. Using default names.")
      feature_names <- paste0("Feature_", 1:n_features)
    }
  }
  
  # Determine performance metric
  if (performance_metric == "auto") {
    performance_metric <- if (family == "binomial") "auc" else if (family == "gaussian") "mse" else "deviance"
  }
  
  # Storage for results
  selection_matrix <- matrix(0, nrow = n_bootstrap, ncol = n_features)
  colnames(selection_matrix) <- feature_names
  
  performance_scores <- numeric(n_bootstrap)
  lambda_values <- numeric(n_bootstrap)
  
  for (b in 1:n_bootstrap) {
    # Bootstrap sample with out-of-bag for performance evaluation
    boot_indices <- sample(1:n_samples, size = n_samples, replace = TRUE)
    oob_indices <- setdiff(1:n_samples, unique(boot_indices))
    
    X_boot <- X[boot_indices, , drop = FALSE]
    y_boot <- y[boot_indices]
    
    if (length(oob_indices) > 0) {
      X_oob <- X[oob_indices, , drop = FALSE]
      y_oob <- y[oob_indices]
    } else {
      # Fallback if no OOB samples
      X_oob <- X_boot
      y_oob <- y_boot
    }
    
    # Check for variation and minimum sample requirements
    if (is.factor(y_boot) && length(unique(y_boot)) < 2) next
    if (is.numeric(y_boot) && var(y_boot) == 0) next
    
    # Check if we have enough samples for cross-validation
    # Need at least 6 samples for 3-fold CV (minimum required by glmnet)
    if (length(y_boot) < 6) next
    
    # For classification, ensure we have at least 2 samples per class for CV
    if (family == "binomial") {
      class_counts <- table(y_boot)
      if (any(class_counts < 2)) next
    }
    
    tryCatch({
      # Calculate appropriate number of folds
      # Ensure at least 3 folds, but not more than the number of samples
      # For classification, ensure each fold has at least 1 sample per class
      max_folds <- if (family == "binomial") {
        min(10, floor(min(table(y_boot)) * 2))  # Conservative: 2 folds per minimum class
      } else {
        min(10, length(y_boot))
      }
      
      nfolds_use <- max(3, min(max_folds, 10))
      
      # Additional safety check
      if (nfolds_use < 3 || length(y_boot) < nfolds_use) next
      
      # Fit CV LASSO
      cv_fit <- cv.glmnet(X_boot, y_boot, family = family, alpha = alpha,
                         standardize = TRUE, type.measure = performance_metric,
                         nfolds = nfolds_use)
      
      # Select lambda
      lambda_use <- if (lambda_selection == "1se") cv_fit$lambda.1se else cv_fit$lambda.min
      lambda_values[b] <- lambda_use
      
      # Evaluate performance on OOB data
      if (length(oob_indices) > 0 && nrow(X_oob) > 1) {
        oob_pred <- predict(cv_fit, newx = X_oob, s = lambda_use, type = "response")
        
        if (family == "binomial") {
          # AUC for binary classification
          if (requireNamespace("pROC", quietly = TRUE)) {
            perf_score <- as.numeric(pROC::auc(y_oob, as.numeric(oob_pred)))
          } else {
            # Fallback: classification accuracy
            pred_class <- ifelse(oob_pred > 0.5, 1, 0)
            perf_score <- mean(pred_class == as.numeric(y_oob) - 1)
          }
        } else if (family == "gaussian") {
          # MSE for regression
          perf_score <- mean((y_oob - oob_pred)^2)
        } else {
          # Deviance for other families
          perf_score <- cv_fit$cvm[cv_fit$lambda == lambda_use]
        }
        
        performance_scores[b] <- perf_score
      }
      
      # Get coefficients and record selections
      coefs <- coef(cv_fit, s = lambda_use)
      if (length(coefs) > 1) {
        feature_coefs <- as.numeric(coefs[-1])  # Remove intercept
        selected_idx <- abs(feature_coefs) > 1e-8
        selection_matrix[b, selected_idx] <- 1
      }
      
    }, error = function(e) {
      cat("Enhanced bootstrap", b, "failed:", e$message, "\n")
    })
  }
  
  # Calculate metrics
  selection_frequencies <- colMeans(selection_matrix)
  
  # Weight by performance (higher weight for better performance)
  valid_perfs <- !is.na(performance_scores) & performance_scores != 0
  if (sum(valid_perfs) > 0) {
    if (family == "binomial") {
      # For AUC, higher is better
      weights <- performance_scores / sum(performance_scores[valid_perfs])
    } else {
      # For MSE/deviance, lower is better
      weights <- (1/performance_scores) / sum(1/performance_scores[valid_perfs])
    }
    weights[!valid_perfs] <- 0
    
    # Weighted selection frequencies
    weighted_frequencies <- apply(selection_matrix * weights, 2, sum) / sum(weights)
  } else {
    weighted_frequencies <- selection_frequencies
  }
  
  # Compile results
  results <- data.frame(
    Feature = feature_names,
    SelectionFrequency = selection_frequencies,
    WeightedFrequency = weighted_frequencies,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(WeightedFrequency))
  
  return(list(
    selection_frequencies = results,
    performance_scores = performance_scores[valid_perfs],
    lambda_distribution = lambda_values[lambda_values > 0],
    selection_matrix = selection_matrix,
    parameters = list(
      n_bootstrap = n_bootstrap,
      lambda_selection = lambda_selection,
      performance_metric = performance_metric,
      alpha = alpha,
      family = family
    )
  ))
}

# 4. Stability Path Analysis
analyze_stability_path <- function(X, y, alpha = 1, family = "gaussian",
                                 lambda_grid = NULL, n_bootstrap = 50, feature_names = NULL) {
  
  if (is.null(lambda_grid)) {
    # Create comprehensive lambda grid
    initial_fit <- glmnet(X, y, family = family, alpha = alpha)
    lambda_grid <- exp(seq(log(max(initial_fit$lambda)), 
                          log(min(initial_fit$lambda)), 
                          length.out = 100))
  }
  
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Use provided feature names or create defaults
  if (is.null(feature_names)) {
    feature_names <- colnames(X)
    if (is.null(feature_names)) {
      feature_names <- paste0("Feature_", 1:n_features)
    }
  } else {
    # Ensure feature_names length matches number of features
    if (length(feature_names) != n_features) {
      warning("Feature names length doesn't match number of features. Using default names.")
      feature_names <- paste0("Feature_", 1:n_features)
    }
  }
  
  # 3D array: bootstrap x feature x lambda
  stability_tensor <- array(0, dim = c(n_bootstrap, n_features, length(lambda_grid)))
  dimnames(stability_tensor) <- list(
    paste0("Boot_", 1:n_bootstrap),
    feature_names,
    paste0("Lambda_", 1:length(lambda_grid))
  )
  
  for (b in 1:n_bootstrap) {
    boot_indices <- sample(1:n_samples, size = n_samples, replace = TRUE)
    X_boot <- X[boot_indices, , drop = FALSE]
    y_boot <- y[boot_indices]
    
    if (is.factor(y_boot) && length(unique(y_boot)) < 2) next
    if (is.numeric(y_boot) && var(y_boot) == 0) next
    
    tryCatch({
      fit_boot <- glmnet(X_boot, y_boot, family = family, alpha = alpha,
                        lambda = lambda_grid, standardize = TRUE)
      
      coef_matrix <- as.matrix(coef(fit_boot))
      if (nrow(coef_matrix) > n_features) {
        coef_matrix <- coef_matrix[-1, , drop = FALSE]  # Remove intercept
      }
      
      # Record selections across lambda path
      selections <- abs(coef_matrix) > 1e-8
      stability_tensor[b, , ] <- selections
      
    }, error = function(e) {
      cat("Stability path bootstrap", b, "failed:", e$message, "\n")
    })
  }
  
  # Calculate stability metrics across lambda path
  selection_frequencies <- apply(stability_tensor, c(2, 3), mean, na.rm = TRUE)
  
  # Find optimal lambda based on stability-performance tradeoff
  stability_scores <- apply(selection_frequencies, 2, function(freqs) {
    # Penalize both instability and too many features
    n_selected <- sum(freqs > 0.5)  # Features selected in >50% of bootstraps
    instability <- sum(freqs * (1 - freqs))  # Variance in selection
    score <- n_selected + instability
    return(score)
  })
  
  optimal_lambda_idx <- which.min(stability_scores)
  optimal_lambda <- lambda_grid[optimal_lambda_idx]
  
  return(list(
    selection_frequencies = selection_frequencies,
    lambda_grid = lambda_grid,
    optimal_lambda = optimal_lambda,
    optimal_features = feature_names[selection_frequencies[, optimal_lambda_idx] > 0.5],
    stability_scores = stability_scores,
    parameters = list(
      n_bootstrap = n_bootstrap,
      alpha = alpha,
      family = family
    )
  ))
}
