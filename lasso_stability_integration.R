# Integration code for enhanced stability in LASSO/Elastic Net
# Add this to your existing Omics_app.R file

# Load required packages for stability analysis
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Enhanced stability analysis function for LASSO/Elastic Net
enhanced_lasso_stability_analysis <- function(X, y, family_type, alpha, 
                                            stability_method = "bootstrap",
                                            n_bootstrap = 100, 
                                            stability_threshold = 0.6,
                                            feature_names = NULL) {
  
  # Source the stability functions
  source("stability_selection_enhancement.R")
  
  # Get feature names - use provided names or extract from data
  if (is.null(feature_names)) {
    feature_names <- colnames(X)
    if (is.null(feature_names)) {
      feature_names <- paste0("Feature_", 1:ncol(X))
    }
  }
  
  # Ensure feature names length matches data
  if (length(feature_names) != ncol(X)) {
    warning("Feature names length doesn't match data dimensions. Using default names.")
    feature_names <- paste0("Feature_", 1:ncol(X))
  }
  
  results <- list()
  
  # Method 1: Traditional Bootstrap (existing implementation enhanced)
  if (stability_method %in% c("bootstrap", "all")) {
    cat("Performing enhanced bootstrap stability analysis...\n")
    bootstrap_results <- perform_enhanced_bootstrap_stability(
      X = X, y = y, alpha = alpha, family = family_type,
      n_bootstrap = n_bootstrap, lambda_selection = "1se",
      feature_names = feature_names
    )
    results$bootstrap <- bootstrap_results
  }
  
  # Method 2: Stability Selection (Meinshausen & Bühlmann)
  if (stability_method %in% c("stability_selection", "all")) {
    cat("Performing stability selection analysis...\n")
    stability_results <- perform_stability_selection(
      X = X, y = y, alpha = alpha, family = family_type,
      n_bootstrap = n_bootstrap, pi_thr = stability_threshold,
      feature_names = feature_names
    )
    results$stability_selection <- stability_results
  }
  
  # Method 3: Randomized LASSO
  if (stability_method %in% c("randomized_lasso", "all")) {
    cat("Performing randomized LASSO analysis...\n")
    randomized_results <- perform_randomized_lasso(
      X = X, y = y, alpha = alpha, family = family_type,
      n_bootstrap = n_bootstrap, feature_names = feature_names
    )
    results$randomized_lasso <- randomized_results
  }
  
  # Method 4: Stability Path Analysis
  if (stability_method %in% c("stability_path", "all")) {
    cat("Performing stability path analysis...\n")
    path_results <- analyze_stability_path(
      X = X, y = y, alpha = alpha, family = family_type,
      n_bootstrap = min(50, n_bootstrap), feature_names = feature_names  # Reduced for computational efficiency
    )
    results$stability_path <- path_results
  }
  
  return(results)
}

# Enhanced plotting functions for stability analysis
plot_stability_comparison <- function(stability_results, top_n = 20) {
  
  plots <- list()
  
  # Helper function to clean feature names for display
  clean_feature_names <- function(features, max_length = 40) {
    # Truncate long feature names for better display
    ifelse(nchar(features) > max_length, 
           paste0(substr(features, 1, max_length-3), "..."), 
           features)
  }
  
  # Bootstrap stability plot
  if ("bootstrap" %in% names(stability_results)) {
    bootstrap_data <- stability_results$bootstrap$selection_frequencies %>%
      arrange(desc(WeightedFrequency)) %>%
      slice_head(n = top_n) %>%
      mutate(Feature_Display = clean_feature_names(Feature))
    
    plots$bootstrap <- ggplot(bootstrap_data, aes(x = reorder(Feature_Display, WeightedFrequency), 
                                                 y = WeightedFrequency)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = "Enhanced Bootstrap Stability",
           subtitle = "Performance-weighted selection frequencies",
           x = "Features", y = "Weighted Selection Frequency") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11))
  }
  
  # Stability selection plot
  if ("stability_selection" %in% names(stability_results)) {
    stable_features <- stability_results$stability_selection$stable_features
    if (length(stable_features$features) > 0) {
      stable_data <- data.frame(
        Feature = stable_features$features,
        Frequency = stable_features$frequencies
      ) %>%
        arrange(desc(Frequency)) %>%
        slice_head(n = top_n) %>%
        mutate(Feature_Display = clean_feature_names(Feature))
      
      plots$stability_selection <- ggplot(stable_data, aes(x = reorder(Feature_Display, Frequency), 
                                                          y = Frequency)) +
        geom_col(fill = "#e74c3c", alpha = 0.8) +
        geom_hline(yintercept = stability_results$stability_selection$parameters$pi_thr, 
                   linetype = "dashed", color = "red") +
        coord_flip() +
        labs(title = "Stability Selection (Meinshausen & Bühlmann)",
             subtitle = paste("Threshold:", stability_results$stability_selection$parameters$pi_thr),
             x = "Features", y = "Selection Frequency") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8),
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 11))
    }
  }
  
  # Randomized LASSO plot
  if ("randomized_lasso" %in% names(stability_results)) {
    randomized_data <- stability_results$randomized_lasso$selection_frequencies %>%
      arrange(desc(SelectionFrequency)) %>%
      slice_head(n = top_n) %>%
      mutate(Feature_Display = clean_feature_names(Feature))
    
    plots$randomized_lasso <- ggplot(randomized_data, aes(x = reorder(Feature_Display, SelectionFrequency), 
                                                         y = SelectionFrequency)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = "Randomized LASSO Stability",
           subtitle = "With random feature scaling",
           x = "Features", y = "Selection Frequency") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11))
  }
  
  # Stability path plot
  if ("stability_path" %in% names(stability_results)) {
    path_data <- stability_results$stability_path
    optimal_idx <- which(path_data$lambda_grid == path_data$optimal_lambda)
    
    # Create heatmap of selection frequencies across lambda path
    freq_matrix <- path_data$selection_frequencies
    
    # Select top features for visualization and clean names
    top_features_idx <- order(rowMeans(freq_matrix), decreasing = TRUE)[1:min(top_n, nrow(freq_matrix))]
    freq_subset <- freq_matrix[top_features_idx, ]
    
    # Clean feature names for display
    cleaned_feature_names <- clean_feature_names(rownames(freq_subset), max_length = 30)
    rownames(freq_subset) <- cleaned_feature_names
    
    # Convert to long format for ggplot
    freq_long <- expand.grid(Feature = rownames(freq_subset), 
                           Lambda_idx = 1:ncol(freq_subset)) %>%
      mutate(Frequency = as.vector(freq_subset),
             Lambda = path_data$lambda_grid[Lambda_idx])
    
    plots$stability_path <- ggplot(freq_long, aes(x = Lambda_idx, y = Feature, fill = Frequency)) +
      geom_tile() +
      geom_vline(xintercept = optimal_idx, color = "red", linetype = "dashed", size = 1) +
      scale_fill_gradient2(low = "white", mid = "yellow", high = "red", 
                          midpoint = 0.5, name = "Selection\nFrequency") +
      labs(title = "Stability Path Analysis",
           subtitle = paste("Optimal λ at position", optimal_idx, "(red line)"),
           x = "Lambda Index (left = high λ, right = low λ)", 
           y = "Features") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11))
  }
  
  return(plots)
}

# Consensus feature selection across methods
get_consensus_features <- function(stability_results, consensus_threshold = 0.5, 
                                 method_weights = NULL) {
  
  all_features <- list()
  
  # Extract features from each method with robust error handling
  tryCatch({
    if ("bootstrap" %in% names(stability_results) && 
        !is.null(stability_results$bootstrap) &&
        !is.null(stability_results$bootstrap$selection_frequencies) &&
        nrow(stability_results$bootstrap$selection_frequencies) > 0) {
      
      bootstrap_features <- stability_results$bootstrap$selection_frequencies %>%
        filter(WeightedFrequency >= consensus_threshold) %>%
        select(Feature, Score = WeightedFrequency)
      
      if (nrow(bootstrap_features) > 0) {
        all_features$bootstrap <- bootstrap_features
      }
    }
  }, error = function(e) {
    cat("Warning: Error extracting bootstrap features:", e$message, "\n")
  })
  
  tryCatch({
    if ("stability_selection" %in% names(stability_results) && 
        !is.null(stability_results$stability_selection) &&
        !is.null(stability_results$stability_selection$stable_features)) {
      
      stable_features <- stability_results$stability_selection$stable_features
      if (length(stable_features$features) > 0) {
        stability_features <- data.frame(
          Feature = stable_features$features,
          Score = stable_features$frequencies,
          stringsAsFactors = FALSE
        )
        all_features$stability_selection <- stability_features
      }
    }
  }, error = function(e) {
    cat("Warning: Error extracting stability selection features:", e$message, "\n")
  })
  
  tryCatch({
    if ("randomized_lasso" %in% names(stability_results) && 
        !is.null(stability_results$randomized_lasso) &&
        !is.null(stability_results$randomized_lasso$selection_frequencies) &&
        nrow(stability_results$randomized_lasso$selection_frequencies) > 0) {
      
      randomized_features <- stability_results$randomized_lasso$selection_frequencies %>%
        filter(SelectionFrequency >= consensus_threshold) %>%
        select(Feature, Score = SelectionFrequency)
      
      if (nrow(randomized_features) > 0) {
        all_features$randomized_lasso <- randomized_features
      }
    }
  }, error = function(e) {
    cat("Warning: Error extracting randomized LASSO features:", e$message, "\n")
  })
  
  tryCatch({
    if ("stability_path" %in% names(stability_results) && 
        !is.null(stability_results$stability_path) &&
        !is.null(stability_results$stability_path$optimal_features) &&
        length(stability_results$stability_path$optimal_features) > 0) {
      
      path_features <- data.frame(
        Feature = stability_results$stability_path$optimal_features,
        Score = 1.0,  # Binary selection from stability path
        stringsAsFactors = FALSE
      )
      
      if (nrow(path_features) > 0) {
        all_features$stability_path <- path_features
      }
    }
  }, error = function(e) {
    cat("Warning: Error extracting stability path features:", e$message, "\n")
  })
  
  # Check if we have any features at all
  if (length(all_features) == 0) {
    cat("Warning: No stable features found by any method.\n")
    return(data.frame(
      Feature = character(0), 
      ConsensusScore = numeric(0),
      MethodsSupporting = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Combine all features and calculate consensus score
  all_feature_names <- unique(unlist(lapply(all_features, function(x) {
    if (is.data.frame(x) && nrow(x) > 0) {
      return(x$Feature)
    } else {
      return(character(0))
    }
  })))
  
  # Check if we have any feature names
  if (length(all_feature_names) == 0) {
    cat("Warning: No feature names found after combining methods.\n")
    return(data.frame(
      Feature = character(0), 
      ConsensusScore = numeric(0),
      MethodsSupporting = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Default equal weights if not specified
  if (is.null(method_weights)) {
    method_weights <- setNames(rep(1/length(all_features), length(all_features)), 
                              names(all_features))
  }
  
  consensus_scores <- sapply(all_feature_names, function(feature) {
    method_scores <- sapply(names(all_features), function(method) {
      method_data <- all_features[[method]]
      if (is.data.frame(method_data) && nrow(method_data) > 0 && feature %in% method_data$Feature) {
        score_value <- method_data$Score[method_data$Feature == feature]
        return(score_value * method_weights[method])
      } else {
        return(0)
      }
    })
    sum(method_scores, na.rm = TRUE)
  })
  
  # Create consensus results
  consensus_results <- data.frame(
    Feature = all_feature_names,
    ConsensusScore = consensus_scores,
    MethodsSupporting = sapply(all_feature_names, function(feature) {
      supporting_methods <- names(all_features)[sapply(all_features, function(x) {
        if (is.data.frame(x) && nrow(x) > 0) {
          return(feature %in% x$Feature)
        } else {
          return(FALSE)
        }
      })]
      paste(supporting_methods, collapse = ", ")
    }),
    stringsAsFactors = FALSE
  )
  
  # Sort by consensus score
  consensus_results <- consensus_results %>%
    arrange(desc(ConsensusScore))
  
  return(consensus_results)
}
