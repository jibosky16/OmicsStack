# FAST STARTUP OPTIMIZATION FOR SHINY SERVER
# ==============================================================================
# Minimal startup configuration - load only essential packages immediately
# Heavy packages and databases will be loaded on-demand

# Set basic options
options(encoding = "UTF-8")
options(askYesNo = function(...) TRUE)

# Set locale safely (works on both Windows and Linux)
tryCatch({
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
}, error = function(e) {
  # If that fails, try C.UTF-8 (common on Linux servers)
  tryCatch({
    Sys.setlocale("LC_ALL", "C.UTF-8")
  }, error = function(e2) {
    # Fall back to default locale
    message("Using default locale")
  })
})

# Configure CRAN mirror (REQUIRED for shinyapps.io deployment)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Configure repositories minimally
if (requireNamespace("BiocManager", quietly = TRUE)) {
  options(BiocManager.check_repositories = FALSE)
}

# ESSENTIAL PACKAGES ONLY (for fast startup)
essential_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinycssloaders", "shinyWidgets",
  "DT", "dplyr", "ggplot2", "plotly", "colourpicker", "bslib", "shinyBS",
  "rhandsontable", "stringr", "tidyr", "digest", "DBI", "RSQLite",
  "readxl", "openxlsx"
)

# Load only essential packages at startup
for (pkg in essential_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}

# LAZY LOADING SYSTEM - Load heavy packages on-demand
lazy_load_packages <- function(package_list, bioc = FALSE) {
  for (pkg in package_list) {
    if (!paste0("package:", pkg) %in% search()) {
      tryCatch({
        if (bioc && requireNamespace("BiocManager", quietly = TRUE)) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            BiocManager::install(pkg, update = FALSE, ask = FALSE)
          }
        } else if (!bioc) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            install.packages(pkg, quiet = TRUE)
          }
        }
        library(pkg, character.only = TRUE)
      }, error = function(e) {
        message(paste("Failed to load package:", pkg, "- Error:", e$message))
      })
    }
  }
}

# Define package groups for lazy loading
visualization_packages <- c(
  "ggprism", "heatmaply", "pheatmap", "ComplexHeatmap", "circlize", "factoextra",
  "ggrepel", "ggsignif", "ggbeeswarm", "VennDiagram", "RColorBrewer", "gridExtra", "grid",
  "UpSetR", "reshape2", "ggVennDiagram", "ggupset", "patchwork", "svglite", "viridisLite", "corrplot"
)

statistics_packages <- c(
  "limma", "DESeq2", "edgeR", "ROTS", "sva", "preprocessCore", "vsn", "pROC",
  "multcomp", "agricolae", "PMCMRplus", "plyr", "PRROC"
)

ml_packages <- c(
  "randomForest", "randomForestExplainer", "glmnet", "mixOmics", "missMDA"
)

network_packages <- c(
  "WGCNA", "flashClust", "dynamicTreeCut"
)

enrichment_packages <- c(
  "clusterProfiler", "enrichplot", "DOSE", "ReactomePA", "gprofiler2", "enrichR",
  "GO.db", "GOSemSim", "topGO", "AnnotationDbi", "pathview", "msigdbr"
)

utility_packages <- c(
  "httr", "httr2", "jsonlite", "htmlwidgets", "visNetwork", "igraph", "ggraph",
  "png", "markdown", "future", "promises", "BiocParallel", "pwrss", "biodb", "biodbChebi", "hmdbQuery"
)

# Annotation database packages (loaded on-demand when specific organism is selected)
# annotation_packages <- c(
#   "org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db", "org.Dr.eg.db",
#   "org.At.tair.db", "org.Dm.eg.db", "org.Ce.eg.db", "org.Sc.sgd.db", "org.EcK12.eg.db"
# )

# Global flag to track loaded packages
loaded_package_groups <- reactiveVal(character(0))

# Function to load package group on-demand
load_package_group <- function(group_name) {
  current_loaded <- loaded_package_groups()
  if (!group_name %in% current_loaded) {
    switch(group_name,
           "visualization" = lazy_load_packages(visualization_packages, bioc = TRUE),
           "statistics" = lazy_load_packages(statistics_packages, bioc = TRUE),
           "ml" = lazy_load_packages(ml_packages, bioc = FALSE),
           "network" = lazy_load_packages(network_packages, bioc = TRUE),
           "enrichment" = lazy_load_packages(enrichment_packages, bioc = TRUE),
           "utility" = lazy_load_packages(utility_packages, bioc = TRUE)
    )
    loaded_package_groups(c(current_loaded, group_name))
    message(paste("Loaded package group:", group_name))
  }
}

# ============================================================================
# GLOBAL VERBOSE FLAG HANDLING
# ============================================================================

if (!exists(".omics_verbose_flag", envir = .GlobalEnv)) {
  assign(".omics_verbose_flag", isTRUE(getOption("omics.verbose", FALSE)), envir = .GlobalEnv)
}

set_omics_verbose <- function(value) {
  assign(".omics_verbose_flag", isTRUE(value), envir = .GlobalEnv)
}

is_omics_verbose <- function() {
  # Legacy support: honor a logical `verbose` defined by caller/test harness
  legacy_flag <- get0("verbose", ifnotfound = NULL, inherits = TRUE)
  if (is.logical(legacy_flag) && length(legacy_flag) == 1 && !is.na(legacy_flag)) {
    return(isTRUE(legacy_flag))
  }
  stored_flag <- get0(".omics_verbose_flag", envir = .GlobalEnv, ifnotfound = FALSE)
  if (is.logical(stored_flag) && length(stored_flag) == 1) {
    return(isTRUE(stored_flag))
  }
  isTRUE(getOption("omics.verbose", FALSE))
}

# Function to load annotation database on-demand
load_annotation_database <- function(organism) {
  # Map organism names to package names
  organism_map <- list(
    hsapiens = "org.Hs.eg.db",
    mmusculus = "org.Mm.eg.db",
    rnorvegicus = "org.Rn.eg.db",
    drerio = "org.Dr.eg.db",
    athaliana = "org.At.tair.db",
    dmelanogaster = "org.Dm.eg.db",
    celegans = "org.Ce.eg.db",
    scerevisiae = "org.Sc.sgd.db",
    ecoli = "org.EcK12.eg.db"
  )
  
  pkg_name <- organism_map[[tolower(organism)]]
  
  if (is.null(pkg_name)) {
    warning(paste("Unknown organism:", organism))
    return(NULL)
  }
  
  tryCatch({
    # Check if package is already loaded
    if (paste0("package:", pkg_name) %in% search()) {
      message(paste("Using already loaded annotation database:", pkg_name))
      return(get(pkg_name))
    }
    
    # Install if needed
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      message(paste("Installing annotation database:", pkg_name))
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", quiet = TRUE)
      }
      BiocManager::install(pkg_name, update = FALSE, ask = FALSE)
    }
    
    # Load the package
    library(pkg_name, character.only = TRUE, quietly = TRUE)
    message(paste("Loaded annotation database:", pkg_name))
    
    return(get(pkg_name))
  }, error = function(e) {
    warning(paste("Failed to load annotation database for", organism, ":", e$message))
    return(NULL)
  })
}

# Wrapper function for easier access (used throughout the app)
get_annotation_database <- function(organism) {
  return(load_annotation_database(organism))
}

# REMOVE HEAVY PRE-LOADING - Only load annotation databases when needed
# Skip all the heavy annotation database pre-loading for fast startup

# Initialize ID conversion cache
if (!exists(".id_conversion_cache", envir = .GlobalEnv)) {
  assign(".id_conversion_cache", new.env(), envir = .GlobalEnv)
}

# ============================================================================
# ID CONVERSION CACHE FUNCTIONS
# ============================================================================

# Function to create a cache key for ID conversion
create_id_cache_key <- function(gene_ids, fromType, toType, organism) {
  # Create a unique key based on sorted gene IDs and conversion parameters
  sorted_ids <- sort(gene_ids)
  key_data <- paste(c(sorted_ids, fromType, toType, organism), collapse = "|")
  return(digest::digest(key_data, algo = "md5"))
}

# Cached version of clusterProfiler::bitr function
cached_bitr <- function(geneID, fromType, toType, OrgDb, drop = TRUE, trigger_update = NULL) {
  # Check if ID conversion cache exists
  if (!exists(".id_conversion_cache", envir = .GlobalEnv)) {
    assign(".id_conversion_cache", new.env(), envir = .GlobalEnv)
  }
  
  # Create cache key
  organism <- deparse(substitute(OrgDb))
  cache_key <- create_id_cache_key(geneID, fromType, toType, organism)
  
  # Check if result is already cached
  if (exists(cache_key, envir = .GlobalEnv$.id_conversion_cache)) {
    message("Using cached ID conversion result")
    return(get(cache_key, envir = .GlobalEnv$.id_conversion_cache))
  }
  
  # Perform ID conversion
  message("Performing ID conversion (will be cached for future use)")
  
  # Handle different OrgDb input types
  if (is.character(substitute(OrgDb))) {
    # If OrgDb is passed as a string, get the actual object
    orgdb_obj <- get(organism)
  } else {
    # If OrgDb is passed as an object
    orgdb_obj <- OrgDb
  }
  
  result <- tryCatch({
    clusterProfiler::bitr(geneID = geneID,
                          fromType = fromType,
                          toType = toType,
                          OrgDb = orgdb_obj,
                          drop = drop)
  }, error = function(e) {
    message(paste("ID conversion failed:", e$message))
    return(NULL)
  })
  
  # Cache the result if successful
  if (!is.null(result) && nrow(result) > 0) {
    assign(cache_key, result, envir = .GlobalEnv$.id_conversion_cache)
    message(paste("Cached ID conversion for", length(geneID), "genes"))
    
    # Trigger reactive update if callback provided
    if (!is.null(trigger_update)) {
      tryCatch(trigger_update(), error = function(e) {
        # Silently handle trigger errors
      })
    }
  }
  
  return(result)
}

# Function to clear ID conversion cache
clear_id_conversion_cache <- function(trigger_update = NULL) {
  if (exists(".id_conversion_cache", envir = .GlobalEnv)) {
    rm(list = ls(envir = .GlobalEnv$.id_conversion_cache), envir = .GlobalEnv$.id_conversion_cache)
    message("ID conversion cache cleared")
    
    # Trigger reactive update if callback provided
    if (!is.null(trigger_update)) {
      tryCatch(trigger_update(), error = function(e) {
        # Silently handle trigger errors
      })
    }
  }
}

# Function to get ID conversion cache statistics
get_id_cache_stats <- function() {
  if (!exists(".id_conversion_cache", envir = .GlobalEnv)) {
    return(list(entries = 0, size_mb = 0))
  }
  
  cache_entries <- length(ls(envir = .GlobalEnv$.id_conversion_cache))
  
  # Estimate cache size
  cache_size_bytes <- tryCatch({
    object.size(.GlobalEnv$.id_conversion_cache)
  }, error = function(e) {
    0
  })
  
  cache_size_mb <- round(as.numeric(cache_size_bytes) / 1024^2, 2)
  
  return(list(entries = cache_entries, size_mb = cache_size_mb))
}

# ============================================================================
# END ID CONVERSION CACHE FUNCTIONS
# ============================================================================

# CLASS IMBALANCE HANDLING FUNCTIONS
# ============================================================================

# Function to detect class imbalance in target variable
detect_class_imbalance <- function(y, threshold = 0.1) {
  if (!is.factor(y)) y <- as.factor(y)
  
  class_counts <- table(y)
  class_props <- prop.table(class_counts)
  
  # Calculate imbalance ratio (minority class / majority class)
  min_prop <- min(class_props)
  max_prop <- max(class_props)
  imbalance_ratio <- min_prop / max_prop
  
  # Severe imbalance if any class has less than threshold proportion
  severe_imbalance <- min_prop < threshold
  
  list(
    is_imbalanced = severe_imbalance,
    class_counts = class_counts,
    class_proportions = class_props,
    imbalance_ratio = imbalance_ratio,
    minority_class = names(class_counts)[which.min(class_counts)],
    majority_class = names(class_counts)[which.max(class_counts)]
  )
}

# Function to calculate sample weights for glmnet
calculate_sample_weights <- function(y, method = "balanced", custom_weights = NULL) {
  if (!is.factor(y)) y <- as.factor(y)
  
  class_counts <- table(y)
  n_samples <- length(y)
  n_classes <- length(class_counts)
  
  if (method == "balanced") {
    # sklearn-style balanced weights: n_samples / (n_classes * class_count)
    class_weights <- n_samples / (n_classes * class_counts)
    sample_weights <- class_weights[as.character(y)]
  } else if (method == "inverse") {
    # Simple inverse frequency
    class_weights <- 1 / class_counts
    sample_weights <- class_weights[as.character(y)]
  } else if (method == "custom" && !is.null(custom_weights)) {
    # Custom weights provided by user
    if (length(custom_weights) == n_classes) {
      names(custom_weights) <- names(class_counts)
      sample_weights <- custom_weights[as.character(y)]
    } else {
      stop("Custom weights must have same length as number of classes")
    }
  } else {
    # No weighting
    sample_weights <- rep(1, length(y))
  }
  
  return(as.numeric(sample_weights))
}

# Function to calculate class weights for Random Forest
calculate_class_weights <- function(y, method = "balanced", custom_weights = NULL) {
  if (!is.factor(y)) y <- as.factor(y)
  
  class_counts <- table(y)
  n_samples <- length(y)
  n_classes <- length(class_counts)
  
  if (method == "balanced") {
    # Balanced weights: inversely proportional to class frequency
    class_weights <- n_samples / (n_classes * class_counts)
    names(class_weights) <- names(class_counts)
  } else if (method == "custom" && !is.null(custom_weights)) {
    # Custom weights provided by user
    if (length(custom_weights) == n_classes) {
      names(custom_weights) <- names(class_counts)
      class_weights <- custom_weights
    } else {
      stop("Custom weights must have same length as number of classes")
    }
  } else {
    # Equal weights (no adjustment)
    class_weights <- rep(1, n_classes)
    names(class_weights) <- names(class_counts)
  }
  
  return(class_weights)
}

# Function to calculate balanced sampsize for Random Forest
calculate_balanced_sampsize <- function(y, min_samples_per_class = NULL) {
  if (!is.factor(y)) y <- as.factor(y)
  
  class_counts <- table(y)
  
  if (is.null(min_samples_per_class)) {
    # Use size of smallest class
    min_size <- min(class_counts)
  } else {
    # Use user-specified minimum, but not more than smallest class
    min_size <- min(min_samples_per_class, min(class_counts))
  }
  
  # Create sampsize vector
  sampsize <- rep(min_size, length(class_counts))
  names(sampsize) <- names(class_counts)
  
  return(sampsize)
}

# END CLASS IMBALANCE HANDLING FUNCTIONS
# ============================================================================

# ============================================================================
# METABOLOMICS ID MAPPING FUNCTIONS
# ============================================================================

# Helper function to check if an ID value is valid (not NA, not "NA" string, not empty)
is_valid_id <- function(x) {
  tryCatch({
    if (is.null(x)) return(FALSE)
    if (length(x) == 0) return(FALSE)
    if (length(x) > 1) x <- x[1]  # Take first element if vector
    if (is.na(x)) return(FALSE)
    x <- as.character(x)
    if (x == "") return(FALSE)
    if (toupper(trimws(x)) == "NA") return(FALSE)
    return(TRUE)
  }, error = function(e) FALSE)
}

# Helper function to convert metabolite IDs to clickable links
make_metabolite_id_link <- function(id, id_type) {
  if (!is_valid_id(id)) return("")
  
  id <- as.character(id)
  base_url <- switch(toupper(id_type),
    "HMDB" = "https://hmdb.ca/metabolites/",
    "HMDB_ID" = "https://hmdb.ca/metabolites/",
    "KEGG" = "https://www.genome.jp/entry/",
    "KEGG_ID" = "https://www.genome.jp/entry/",
    "PUBCHEM" = "https://pubchem.ncbi.nlm.nih.gov/compound/",
    "PUBCHEM_CID" = "https://pubchem.ncbi.nlm.nih.gov/compound/",
    "CHEBI" = "https://www.ebi.ac.uk/chebi/searchId.do?chebiId=",
    "CHEBI_ID" = "https://www.ebi.ac.uk/chebi/searchId.do?chebiId=",
    "LIPIDMAPS" = "https://www.lipidmaps.org/databases/lmsd/",
    "LIPIDMAPS_ID" = "https://www.lipidmaps.org/databases/lmsd/",
    ""
  )
  
  if (base_url == "") return(id)
  
  paste0('<a href="', base_url, id, '" target="_blank">', id, '</a>')
}

# Helper function to add clickable links to all ID columns in a dataframe
add_metabolite_links_to_dataframe <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  id_columns <- c("HMDB_ID", "KEGG_ID", "PubChem_CID", "ChEBI_ID", "LipidMaps_ID")
  
  for (col in id_columns) {
    if (col %in% names(df)) {
      id_type <- sub("_ID|_CID", "", col)
      df[[col]] <- sapply(df[[col]], function(x) make_metabolite_id_link(x, id_type))
    }
  }
  
  return(df)
}

# Helper function to create collapsible content with "Show more/Show less"
create_collapsible_content <- function(text, max_chars = 200, id_prefix = "collapse") {
  if (is.na(text) || text == "") return("")
  
  text <- as.character(text)
  unique_id <- paste0(id_prefix, "_", sample(10000:99999, 1))
  
  if (nchar(text) <= max_chars) {
    return(text)
  }
  
  preview <- substr(text, 1, max_chars)
  full_text <- text
  
  # Create HTML with collapsible functionality
  html_content <- sprintf(
    '<div id="container_%s" style="word-wrap: break-word; overflow-wrap: break-word;">
      <span id="preview_%s" style="display: block;">%s...</span>
      <span id="full_%s" style="display: none; word-wrap: break-word; overflow-wrap: break-word;">%s</span>
      <a href="javascript:void(0);" onclick="toggleCollapse(\'%s\')" id="toggle_%s" style="color: #0066cc; cursor: pointer; font-weight: 500;">
        Show more
      </a>
    </div>',
    unique_id, unique_id, preview, unique_id, full_text, unique_id, unique_id
  )
  
  return(html_content)
}


# Load local metabolite database from SQLite
load_metabolite_database <- function() {
  tryCatch({
    if (file.exists("metabolites.db")) {
      con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
      
      # Get available columns
      available_cols <- DBI::dbListFields(con, "metabolites")
      cat("Available columns in metabolites table:", paste(available_cols, collapse = ", "), "\n")
      
      # Define desired columns (only include those that exist)
      desired_cols <- c("name", "iupac_name", "traditional_iupac", 
                       "kegg_id", "pubchem_id", "pubchem_cid",
                       "hmdb_id", "chebi_id", "lipidmaps_id")
      
      # Filter to only existing columns
      needed_cols <- desired_cols[desired_cols %in% available_cols]
      cat("Will load columns:", paste(needed_cols, collapse = ", "), "\n")
      
      if (length(needed_cols) == 0) {
        stop("No usable columns found in metabolites table")
      }
      
      query <- sprintf("SELECT %s FROM metabolites", paste(needed_cols, collapse = ", "))
      cat("Loading metabolite database from SQLite (this may take a moment for large databases)...\n")
      metab_list <- DBI::dbGetQuery(con, query) %>%
        mutate(across(any_of(c("name", "iupac_name", "traditional_iupac")), 
                     ~ tolower(trimws(as.character(.)))))
      DBI::dbDisconnect(con)
      
      # Skip expensive normalization during startup - do it lazily during searches
      # This makes database loading much faster for server startup
      cat("Loaded local metabolite database with", nrow(metab_list), "entries from SQLite (normalization skipped for faster startup)\n")
      return(metab_list)
    } else {
      stop("Metabolite database not found.")
    }
  }, error = function(e) {
    message("Error loading local metabolite database: ", e$message, ". Using online lookups only.")
    return(NULL)
  })
}

# Database connection - will be created on-demand for each query
# Note: SQLite handles concurrent reads efficiently, so no need to keep persistent connection
# biodb HMDB connection is disabled due to compatibility issues with Shiny reactive contexts

# Create performance indexes on metabolites table
create_metabolite_indexes <- function() {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Create indexes on name fields for faster searches
    tryCatch(DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_metabolites_name ON metabolites(name)"), error = function(e) NULL)
    tryCatch(DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_metabolites_iupac ON metabolites(iupac_name)"), error = function(e) NULL)
    tryCatch(DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_metabolites_traditional ON metabolites(traditional_iupac)"), error = function(e) NULL)
    
    # Optimize SQLite for better performance
    DBI::dbExecute(con, "PRAGMA synchronous = NORMAL")
    DBI::dbExecute(con, "PRAGMA cache_size = 10000")
    DBI::dbExecute(con, "PRAGMA temp_store = MEMORY")
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error creating metabolite indexes: ", e$message)
    return(FALSE)
  })
}

# Initialize cache table
initialize_cache_table <- function() {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Create cache table if it doesn't exist
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS cached_mappings (
        id INTEGER PRIMARY KEY,
        original_name TEXT,
        clean_name TEXT UNIQUE,
        normalized_name TEXT,
        kegg_id TEXT,
        kegg_status TEXT,
        pubchem_cid TEXT,
        pubchem_status TEXT,
        hmdb_id TEXT,
        chebi_id TEXT,
        chebi_status TEXT,
        lipidmaps_id TEXT,
        lipidmaps_status TEXT,
        databases_searched TEXT,
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Create indexes for fast lookups
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cache_clean_name ON cached_mappings(clean_name)")
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cache_normalized_name ON cached_mappings(normalized_name)")
    }, error = function(e) {
      # Indexes might already exist, that's OK
    })
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error initializing cache table: ", e$message)
    return(FALSE)
  })
}

# Load cached mappings from SQLite
load_cached_mappings <- function() {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(NULL)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Check if table exists
    tables <- DBI::dbListTables(con)
    if (!"cached_mappings" %in% tables) {
      DBI::dbDisconnect(con)
      return(NULL)
    }
    
    cached_data <- DBI::dbGetQuery(con, "SELECT * FROM cached_mappings")
    DBI::dbDisconnect(con)
    
    if (nrow(cached_data) > 0) {
      message("Loaded ", nrow(cached_data), " cached metabolite mappings from SQLite")
      return(cached_data)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    message("Error loading cached mappings: ", e$message)
    return(NULL)
  })
}

# Save all cached mappings to SQLite (batch operation)
save_all_cached_mappings <- function(cached_mappings_df) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.null(cached_mappings_df) || nrow(cached_mappings_df) == 0) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Ensure columns align with table schema and types are character
    required_cols <- c(
      "original_name","clean_name","normalized_name","kegg_id","kegg_status",
      "pubchem_cid","pubchem_status","hmdb_id","chebi_id",
      "chebi_status","lipidmaps_id","lipidmaps_status","databases_searched"
    )
    # Add missing columns with empty strings
    for (col in required_cols) {
      if (!col %in% names(cached_mappings_df)) cached_mappings_df[[col]] <- ""
    }
    # Drop unsupported columns (like id, last_updated) to avoid datatype mismatch
    cached_mappings_df <- cached_mappings_df[, required_cols]
    
    # Coerce to character to match TEXT columns
    for (col in names(cached_mappings_df)) {
      cached_mappings_df[[col]] <- as.character(cached_mappings_df[[col]])
      cached_mappings_df[[col]][is.na(cached_mappings_df[[col]])] <- ""
    }
    
    # Remove duplicates by clean_name (keep first occurrence)
    cached_mappings_df <- cached_mappings_df[!duplicated(cached_mappings_df$clean_name), ]
    
    # Replace entire cache table with new data
    DBI::dbExecute(con, "DELETE FROM cached_mappings")
    DBI::dbAppendTable(con, "cached_mappings", cached_mappings_df)
    
    DBI::dbDisconnect(con)
    message("Saved ", nrow(cached_mappings_df), " mappings to SQLite cache")
    return(TRUE)
  }, error = function(e) {
    message("Error saving cached mappings: ", e$message)
    return(FALSE)
  })
}

# Validate ID formats per database to prevent invalid data in cache
validate_id_format <- function(id, database) {
  if (!is_valid_id(id)) return(FALSE)
  
  id_str <- as.character(id)
  
  switch(database,
    "kegg" = grepl("^C\\d{5}$", id_str),           # Format: C00001-C99999
    "pubchem" = grepl("^\\d+$", id_str),           # Format: numeric
    "hmdb" = grepl("^HMDB\\d{7}$", id_str),        # Format: HMDB0000001-HMDB9999999
    "chebi" = grepl("^(CHEBI:)?\\d+$", id_str),    # Format: CHEBI:12345 or just 12345
    "lipidmaps" = grepl("^LM\\w{2}\\d{8}$", id_str), # Format: LMFA00000001
    TRUE  # If database unknown, accept if valid
  )
}

# OPTIMIZATION #6: Smart metabolomics data detection
# Only pre-populate cache if we're actually analyzing metabolomics data
# This now accepts user-specified column information from the group mapping modal
detect_metabolomics_data <- function(feature_col_name = NULL, id_col_name = NULL, data_columns = NULL) {
  # SIMPLIFIED: Use explicit user input instead of pattern matching
  # If user has already configured columns, use that signal
  
  # Signal 1: User explicitly selected metabolomics-specific ID columns
  if (!is.null(id_col_name)) {
    id_col_lower <- tolower(id_col_name)
    metabolomics_id_keywords <- c("kegg", "chebi", "hmdb", "pubchem", "lipidmaps", 
                                   "metabolite_id", "compound_id", "mz", "m/z")
    
    if (any(sapply(metabolomics_id_keywords, function(kw) grepl(kw, id_col_lower)))) {
      return(TRUE)
    }
  }
  
  # Signal 2: Feature column name indicates metabolomics
  if (!is.null(feature_col_name)) {
    feature_col_lower <- tolower(feature_col_name)
    metabolomics_feature_keywords <- c("metabolite", "compound", "lipid", "analyte")
    
    if (any(sapply(metabolomics_feature_keywords, function(kw) grepl(kw, feature_col_lower)))) {
      return(TRUE)
    }
  }
  
  # Signal 3: Check data_columns list for metabolomics signatures
  if (!is.null(data_columns)) {
    data_columns_lower <- tolower(data_columns)
    metabolomics_keywords <- c("metabolite", "compound", "mass", "mz", "m/z", 
                               "retention", "retention_time", "rt", "pubchem", 
                               "kegg", "hmdb", "chebi", "inchi", "smiles", "lipid")
    
    # If any column has metabolomics keyword, likely metabolomics data
    if (any(sapply(metabolomics_keywords, function(kw) {
      any(grepl(kw, data_columns_lower))
    }))) {
      return(TRUE)
    }
  }
  
  # Default: Not detected as metabolomics
  return(FALSE)
}

# Pre-warm cache with common metabolites on app startup
# OPTIMIZATION #6: Only pre-warm if metabolomics data is detected
initialize_default_cache <- function(skip_preload = FALSE) {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Check if cache already has entries
    count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM cached_mappings")$n
    
    DBI::dbDisconnect(con)
    
    # If cache already populated, skip
    if (count > 0) {
      return(TRUE)
    }
    
    # OPTIMIZATION #6: Skip pre-warming if requested (e.g., for non-metabolomics analyses)
    if (skip_preload) {
      if (is_omics_verbose()) message("[CACHE] Skipping metabolite cache pre-warming (non-metabolomics workflow)")
      return(FALSE)
    }
    
    # Common metabolites to pre-warm
    common_metabolites <- c(
      # Energy metabolism
      "glucose", "pyruvate", "acetyl-coa", "atp", "adp", "amp",
      # Amino acids
      "alanine", "arginine", "asparagine", "aspartate", "cysteine",
      "glutamate", "glutamine", "glycine", "histidine", "isoleucine",
      "leucine", "lysine", "methionine", "phenylalanine", "proline",
      "serine", "threonine", "tryptophan", "tyrosine", "valine",
      # Nucleotides
      "adenine", "guanine", "cytosine", "uracil", "thymine",
      "adenosine", "guanosine", "cytidine", "uridine",
      # Fatty acids
      "palmitate", "stearate", "oleate", "linoleate",
      # Other common metabolites
      "succinate", "fumarate", "malate", "citrate", "lactate",
      "ethanol", "acetone", "urea", "creatinine"
    )
    
    message("Pre-warming metabolite ID cache (one-time setup)...")
    
    # Resolve these metabolites
    resolve_ids(common_metabolites, 
               databases = c("kegg", "pubchem", "hmdb"),
               max_workers = 2)
    
    message("Cache pre-warming complete!")
    return(TRUE)
  }, error = function(e) {
    message("Cache pre-warming failed: ", e$message)
    return(FALSE)
  })
}

# Function to clean metabolite names
# Enhanced metabolite name normalization for better matching
normalize_metabolite_name <- function(name, keep_hyphens = FALSE) {
  # More aggressive normalization to catch common variants
  if (is.na(name) || name == "") return(NA)

  name <- as.character(name)

  # Expand Greek letters
  name <- gsub("α|alpha", "alpha", name, ignore.case = TRUE)
  name <- gsub("β|beta", "beta", name, ignore.case = TRUE)
  name <- gsub("γ|gamma", "gamma", name, ignore.case = TRUE)
  name <- gsub("δ|delta", "delta", name, ignore.case = TRUE)

  # Normalize stereochemistry prefixes
  name <- gsub("^[Dd]-", "", name)  # Remove D- prefix
  name <- gsub("^[Ll]-", "", name)  # Remove L- prefix
  name <- gsub("^[Ss]-", "", name)  # Remove S- prefix
  name <- gsub("^[Rr]-", "", name)  # Remove R- prefix

  # Remove common parenthetical stereochemistry
  name <- gsub("\\(\\+\\)", "", name)
  name <- gsub("\\(\\-\\)", "", name)
  name <- gsub("\\([Dd]\\)", "", name)
  name <- gsub("\\([Ll]\\)", "", name)

  # Remove numbers and dashes at start
  name <- gsub("^[0-9]+[\\s:-]*", "", name)
  name <- gsub("^[A-Z][0-9]+[\\s:-]*", "", name)

  # Lowercase and trim
  name <- tolower(trimws(name))

  # Remove special characters but optionally keep hyphens and spaces
  if (keep_hyphens) {
    name <- gsub("[^a-z0-9\\s-]", " ", name)
  } else {
    name <- gsub("[^a-z0-9\\s]", " ", name)
  }

  # Collapse multiple spaces
  name <- gsub("\\s+", " ", name)

  trimws(name)
}

clean_molecule_name <- function(name) {
  if (is.na(name) || name == "") return(NA)
  
  # Convert to character if not already
  name <- as.character(name)
  
  # Expand Greek letters
  name <- gsub("β", "beta", name)
  name <- gsub("α", "alpha", name)
  
  # Remove leading parenthetical prefixes like (D)- or (+)-
  name <- sub("^\\(([^)]*)\\)-+", "", name)
  name <- sub("^\\(([^)]*)\\)-+", "", name)
  
  # Trim whitespace
  name <- str_trim(name)
  
  return(name)
}

# ============================================================================
# PERSISTENT DATABASE CONNECTION POOL FOR METABOLOMICS
# ============================================================================

# Global persistent connection for metabolomics database (reused across all queries)
.metabolomics_connection <- NULL

# Get or create persistent connection to metabolomics database
get_metabolomics_connection <- function() {
  if (is.null(.metabolomics_connection)) {
    if (file.exists("metabolites.db")) {
      .metabolomics_connection <<- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
      if (is_omics_verbose()) message("[CONNECTION] Created persistent connection to metabolites.db")
    } else {
      return(NULL)
    }
  }
  return(.metabolomics_connection)
}

# Close persistent connection
close_metabolomics_connection <- function() {
  if (!is.null(.metabolomics_connection)) {
    DBI::dbDisconnect(.metabolomics_connection)
    .metabolomics_connection <<- NULL
    if (is_omics_verbose()) message("[CONNECTION] Closed persistent connection to metabolites.db")
  }
}

# Test if connection is still valid
is_metabolomics_connection_valid <- function() {
  if (is.null(.metabolomics_connection)) return(FALSE)
  tryCatch({
    DBI::dbGetQuery(.metabolomics_connection, "SELECT 1")
    return(TRUE)
  }, error = function(e) return(FALSE))
}

# ============================================================================
# BATCH METABOLITE SEARCH WITH SQL IN CLAUSES
# ============================================================================

# OPTIMIZATION #9: Batch search using SQL IN clauses and persistent connection
# Returns a single data frame with one row per metabolite (in input order)
batch_search_metabolites <- function(metabolite_names, databases = c("kegg", "pubchem", "hmdb", "chebi", "lipidmaps")) {
  
  if (length(metabolite_names) == 0) {
    return(data.frame(
      original_name = character(),
      clean_name = character(),
      normalized_name = character(),
      kegg_id = character(),
      pubchem_cid = character(),
      hmdb_id = character(),
      chebi_id = character(),
      lipidmaps_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get or create persistent connection
  con <- get_metabolomics_connection()
  if (is.null(con)) {
    if (is_omics_verbose()) message("[BATCH] No metabolites database found, returning empty results")
    return(data.frame(
      original_name = metabolite_names,
      clean_name = NA_character_,
      normalized_name = NA_character_,
      kegg_id = NA_character_,
      pubchem_cid = NA_character_,
      hmdb_id = NA_character_,
      chebi_id = NA_character_,
      lipidmaps_id = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  
  # Prepare search terms for all metabolites
  cleaned_names <- sapply(metabolite_names, clean_molecule_name)
  normalized_names <- sapply(metabolite_names, normalize_metabolite_name)  # without hyphens
  normalized_names_hyphen <- sapply(metabolite_names, function(x) normalize_metabolite_name(x, keep_hyphens = TRUE))  # with hyphens
  base_names <- sapply(metabolite_names, function(x) stringr::str_to_lower(stringr::str_trim(as.character(x))))
  
  # Initialize results data frame with all metabolites (will fill in IDs as we find them)
  results_df <- data.frame(
    original_name = metabolite_names,
    clean_name = cleaned_names,
    normalized_name = normalized_names,
    kegg_id = NA_character_,
    pubchem_cid = NA_character_,
    hmdb_id = NA_character_,
    chebi_id = NA_character_,
    lipidmaps_id = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # STEP 1: Check cache table for all metabolites at once (single query for all)
  # Also track which databases need retry due to temporary_failure status
  cache_retry_needed <- list()  # metabolite_index -> list of databases to retry
  
  tryCatch({
    if (is_omics_verbose()) message(sprintf("[BATCH-CACHE] Checking cache for %d metabolites", length(metabolite_names)))
    
    # Escape single quotes for SQL
    cleaned_escaped <- paste0("'", gsub("'", "''", cleaned_names), "'", collapse = ",")
    normalized_escaped <- paste0("'", gsub("'", "''", normalized_names), "'", collapse = ",")
    normalized_hyphen_escaped <- paste0("'", gsub("'", "''", normalized_names_hyphen), "'", collapse = ",")
    original_escaped <- paste0("'", gsub("'", "''", metabolite_names), "'", collapse = ",")
    
    # Search cache using all name variants (original, cleaned, normalized w/o hyphens, normalized w/ hyphens)
    cache_query <- sprintf("
      SELECT clean_name, normalized_name, original_name, kegg_id, kegg_status, pubchem_cid, pubchem_status, 
             hmdb_id, chebi_id, chebi_status, lipidmaps_id, lipidmaps_status
      FROM cached_mappings 
      WHERE clean_name IN (%s) OR normalized_name IN (%s) OR normalized_name IN (%s) OR original_name IN (%s)
    ", cleaned_escaped, normalized_escaped, normalized_hyphen_escaped, original_escaped)
    
    cached_results <- tryCatch(DBI::dbGetQuery(con, cache_query), error = function(e) NULL)
    
    if (!is.null(cached_results) && nrow(cached_results) > 0) {
      # Match cached results back to original metabolites using all name variants
      for (i in seq_along(metabolite_names)) {
        cached_row <- cached_results[
          (cached_results$original_name == metabolite_names[i]) |
          (cached_results$clean_name == cleaned_names[i]) | 
          (cached_results$normalized_name == normalized_names[i]) |
          (cached_results$normalized_name == normalized_names_hyphen[i]), 
        ]
        
        if (nrow(cached_row) > 0) {
          cached_row <- cached_row[1, ]  # Take first match
          cache_retry_needed[[i]] <- character(0)
          
          # For each database, check status and decide whether to use cached value or retry
          # kegg
          if ("kegg" %in% databases) {
            if (!is.null(cached_row$kegg_status) && cached_row$kegg_status == "temporary_failure") {
              cache_retry_needed[[i]] <- c(cache_retry_needed[[i]], "kegg")
            } else if (is_valid_id(cached_row$kegg_id)) {
              results_df$kegg_id[i] <- as.character(cached_row$kegg_id)
            }
          }
          
          # pubchem
          if ("pubchem" %in% databases) {
            if (!is.null(cached_row$pubchem_status) && cached_row$pubchem_status == "temporary_failure") {
              cache_retry_needed[[i]] <- c(cache_retry_needed[[i]], "pubchem")
            } else if (is_valid_id(cached_row$pubchem_cid)) {
              results_df$pubchem_cid[i] <- as.character(cached_row$pubchem_cid)
            }
          }
          
          # hmdb (no status check, always use if valid)
          if ("hmdb" %in% databases && is_valid_id(cached_row$hmdb_id)) {
            results_df$hmdb_id[i] <- as.character(cached_row$hmdb_id)
          }
          
          # chebi
          if ("chebi" %in% databases) {
            if (!is.null(cached_row$chebi_status) && cached_row$chebi_status == "temporary_failure") {
              cache_retry_needed[[i]] <- c(cache_retry_needed[[i]], "chebi")
            } else if (is_valid_id(cached_row$chebi_id)) {
              results_df$chebi_id[i] <- as.character(cached_row$chebi_id)
            }
          }
          
          # lipidmaps
          if ("lipidmaps" %in% databases) {
            if (!is.null(cached_row$lipidmaps_status) && cached_row$lipidmaps_status == "temporary_failure") {
              cache_retry_needed[[i]] <- c(cache_retry_needed[[i]], "lipidmaps")
            } else if (is_valid_id(cached_row$lipidmaps_id)) {
              results_df$lipidmaps_id[i] <- as.character(cached_row$lipidmaps_id)
            }
          }
        }
      }
    }
  }, error = function(e) {
    if (is_omics_verbose()) message(sprintf("[BATCH-CACHE] Cache query error: %s", e$message))
  })
  
  # STEP 2: Find uncached metabolites that still need database search
  uncached_indices <- which(
    is.na(results_df$kegg_id) & is.na(results_df$pubchem_cid) & 
    is.na(results_df$hmdb_id) & is.na(results_df$chebi_id) & is.na(results_df$lipidmaps_id)
  )
  
  # STEP 2b: Find metabolites with partial cache (have some IDs but missing HMDB)
  # HMDB is ONLY in the main metabolites table, not from API, so we need to query for it separately
  hmdb_missing_indices <- which(
    "hmdb" %in% databases & 
    is.na(results_df$hmdb_id) &
    !(1:length(metabolite_names) %in% uncached_indices)  # Exclude fully uncached (will be searched anyway)
  )
  
  if (length(hmdb_missing_indices) > 0 && is_omics_verbose()) {
    message(sprintf("[BATCH-DB] %d metabolites have cached entries but missing HMDB, will query main table", length(hmdb_missing_indices)))
  }
  
  # Combine indices that need main database query (fully uncached + HMDB missing)
  indices_needing_db_query <- unique(c(uncached_indices, hmdb_missing_indices))
  
  if (length(indices_needing_db_query) > 0) {
    # Prepare name variants for all indices needing query
    query_names <- metabolite_names[indices_needing_db_query]
    query_cleaned <- cleaned_names[indices_needing_db_query]
    query_normalized <- normalized_names[indices_needing_db_query]
    query_normalized_hyphen <- normalized_names_hyphen[indices_needing_db_query]
    query_base <- base_names[indices_needing_db_query]
    
    if (is_omics_verbose()) {
      message(sprintf("[BATCH-DB] Querying database for %d metabolites (including HMDB-missing)", length(indices_needing_db_query)))
    }
    
    # Query database for metabolites needing lookup
    # Use exact matching only (like backup code) - no LIKE queries
    tryCatch({
      # Split metabolites into chunks to avoid SQLite expression tree depth limit (max 1000)
      # We'll process them in batches of 500 metabolites max
      chunk_size <- 400  # Conservative to avoid hitting the 1000 depth limit
      num_chunks <- ceiling(length(indices_needing_db_query) / chunk_size)
      
      db_results_all <- NULL
      
      for (chunk_num in 1:num_chunks) {
        start_idx <- (chunk_num - 1) * chunk_size + 1
        end_idx <- min(chunk_num * chunk_size, length(indices_needing_db_query))
        chunk_indices <- start_idx:end_idx
        
        chunk_names <- query_names[chunk_indices]
        chunk_cleaned <- query_cleaned[chunk_indices]
        chunk_normalized <- query_normalized[chunk_indices]
        chunk_normalized_hyphen <- query_normalized_hyphen[chunk_indices]
        
        # Build exact match clauses for this chunk
        exact_clauses <- sapply(seq_along(chunk_names), function(i) {
          orig_escaped <- gsub("'", "''", chunk_names[i])
          clean_escaped <- gsub("'", "''", chunk_cleaned[i])
          norm_escaped <- gsub("'", "''", chunk_normalized[i])
          norm_hyphen_escaped <- gsub("'", "''", chunk_normalized_hyphen[i])
          
          sprintf("(LOWER(name) = LOWER('%s') OR LOWER(iupac_name) = LOWER('%s') OR LOWER(traditional_iupac) = LOWER('%s') OR 
                    LOWER(name) = LOWER('%s') OR LOWER(iupac_name) = LOWER('%s') OR LOWER(traditional_iupac) = LOWER('%s') OR
                    LOWER(name) = LOWER('%s') OR LOWER(iupac_name) = LOWER('%s') OR LOWER(traditional_iupac) = LOWER('%s') OR
                    LOWER(name) = LOWER('%s') OR LOWER(iupac_name) = LOWER('%s') OR LOWER(traditional_iupac) = LOWER('%s'))", 
                  orig_escaped, orig_escaped, orig_escaped,
                  clean_escaped, clean_escaped, clean_escaped,
                  norm_escaped, norm_escaped, norm_escaped,
                  norm_hyphen_escaped, norm_hyphen_escaped, norm_hyphen_escaped)
        })
        
        exact_where <- paste(exact_clauses, collapse = " OR ")
        
        # Query for exact matches in this chunk
        db_query <- sprintf("
          SELECT kegg_id, pubchem_id, hmdb_id, chebi_id,
                 name, iupac_name, traditional_iupac
          FROM metabolites
          WHERE %s
        ", exact_where)
        
        if (is_omics_verbose() && chunk_num == 1) {
          message(sprintf("[BATCH-DB] Querying database with exact matches for %d metabolites (in %d chunks of ~%d)", 
                         length(indices_needing_db_query), num_chunks, chunk_size))
        }
        
        db_chunk_results <- DBI::dbGetQuery(con, db_query)
        
        if (!is.null(db_chunk_results) && nrow(db_chunk_results) > 0) {
          db_results_all <- rbind(db_results_all, db_chunk_results)
        }
      }
      
      if (!is.null(db_results_all) && nrow(db_results_all) > 0) {
        # Match each metabolite to its database result
        # The SQL returned all matches; now we need to assign the correct row to each metabolite
        for (idx in seq_along(indices_needing_db_query)) {
          orig_idx <- indices_needing_db_query[idx]
          
          # Get the name variants for this metabolite
          orig_name <- tolower(query_names[idx])
          clean_name <- tolower(query_cleaned[idx])
          norm_name <- tolower(query_normalized[idx])
          norm_hyphen <- tolower(query_normalized_hyphen[idx])
          
          # Find matching row in db_results_all by checking all name columns
          match_idx <- which(
            tolower(db_results_all$name) == orig_name |
            tolower(db_results_all$iupac_name) == orig_name |
            tolower(db_results_all$traditional_iupac) == orig_name |
            tolower(db_results_all$name) == clean_name |
            tolower(db_results_all$iupac_name) == clean_name |
            tolower(db_results_all$traditional_iupac) == clean_name |
            tolower(db_results_all$name) == norm_name |
            tolower(db_results_all$iupac_name) == norm_name |
            tolower(db_results_all$traditional_iupac) == norm_name |
            tolower(db_results_all$name) == norm_hyphen |
            tolower(db_results_all$iupac_name) == norm_hyphen |
            tolower(db_results_all$traditional_iupac) == norm_hyphen
          )
          
          # Extract IDs from matched row
          # For HMDB-missing cases, only update HMDB (other IDs already from cache)
          if (length(match_idx) > 0) {
            row <- db_results_all[match_idx[1], ]
            # Only update if current result is NA (don't overwrite cached values)
            if (is.na(results_df$kegg_id[orig_idx]) && is_valid_id(row$kegg_id)) results_df$kegg_id[orig_idx] <- as.character(row$kegg_id)
            if (is.na(results_df$pubchem_cid[orig_idx]) && is_valid_id(row$pubchem_id)) results_df$pubchem_cid[orig_idx] <- as.character(row$pubchem_id)
            if (is.na(results_df$hmdb_id[orig_idx]) && is_valid_id(row$hmdb_id)) results_df$hmdb_id[orig_idx] <- as.character(row$hmdb_id)
            if (is.na(results_df$chebi_id[orig_idx]) && is_valid_id(row$chebi_id)) results_df$chebi_id[orig_idx] <- as.character(row$chebi_id)
          }
        }
      }
    }, error = function(e) {
      if (is_omics_verbose()) message(sprintf("[BATCH-DB] Database query error: %s", e$message))
    })
    
    # STEP 3: Call APIs for missing IDs after main database search
    # This includes both newly searched metabolites and retry cases from cache
    # IMPORTANT: Check for truly missing IDs using is_valid_id() to catch NA, "NA", empty strings
    # NOTE: LipidMaps is ONLY available via API (not in main metabolites table)
    api_calls_needed <- list()  # metabolite_index -> list(databases_to_call, statuses)
    
    for (idx in uncached_indices) {
      missing_dbs <- character(0)
      
      # Check which requested databases are still missing IDs
      # Use is_valid_id() to properly detect NA, "NA", empty strings, etc.
      if ("kegg" %in% databases && !is_valid_id(results_df$kegg_id[idx])) missing_dbs <- c(missing_dbs, "kegg")
      if ("pubchem" %in% databases && !is_valid_id(results_df$pubchem_cid[idx])) missing_dbs <- c(missing_dbs, "pubchem")
      if ("chebi" %in% databases && !is_valid_id(results_df$chebi_id[idx])) missing_dbs <- c(missing_dbs, "chebi")
      
      # LipidMaps is ALWAYS missing from main DB (no column in metabolites table)
      # So always call API if requested
      if ("lipidmaps" %in% databases) missing_dbs <- c(missing_dbs, "lipidmaps")
      
      if (length(missing_dbs) > 0) {
        api_calls_needed[[as.character(idx)]] <- list(databases = missing_dbs, statuses = list())
      }
    }
    
    # Also add retry cases from cache (temporary_failure status)
    for (i in seq_along(cache_retry_needed)) {
      if (length(cache_retry_needed[[i]]) > 0) {
        idx_str <- as.character(i)
        if (is.null(api_calls_needed[[idx_str]])) {
          api_calls_needed[[idx_str]] <- list(databases = character(0), statuses = list())
        }
        api_calls_needed[[idx_str]]$databases <- unique(c(api_calls_needed[[idx_str]]$databases, cache_retry_needed[[i]]))
      }
    }
    
    # Execute API calls for missing IDs
    if (length(api_calls_needed) > 0) {
      if (is_omics_verbose()) {
        total_calls <- sum(sapply(api_calls_needed, function(x) length(x$databases)))
        message(sprintf("[BATCH-API] Making %d API calls for %d metabolites", total_calls, length(api_calls_needed)))
      }
      
      # Define local API functions within batch context
      retry_api_call_batch <- function(api_function, compound_name, max_retries = 1) {
        for (attempt in 1:(max_retries + 1)) {
          result <- tryCatch({
            api_function(compound_name)
          }, error = function(e) {
            "TEMP_ERROR"
          })
          
          if (is.character(result) && result == "NOT_FOUND") {
            return(list(id = NA, status = "not_found"))
          } else if (is.character(result) && result == "TEMP_ERROR") {
            if (attempt <= max_retries) {
              Sys.sleep(0.5)
              next
            } else {
              return(list(id = NA, status = "temporary_failure"))
            }
          } else if (!is.na(result)) {
            return(list(id = result, status = "found"))
          }
        }
        return(list(id = NA, status = "temporary_failure"))
      }
      
      local_get_kegg_id_api_batch <- function(compound_name) {
        tryCatch({
          if (is.na(compound_name) || compound_name == "") return(NA)
          Sys.sleep(0.2)
          result <- KEGGREST::keggFind("compound", compound_name)
          if (length(result) > 0) {
            kegg_id <- names(result)[1]
            if (grepl("^cpd:", kegg_id)) kegg_id <- sub("^cpd:", "", kegg_id)
            return(kegg_id)
          } else {
            return("NOT_FOUND")
          }
        }, error = function(e) stop("TEMP_ERROR"))
      }
      
      local_get_pubchem_cid_api_batch <- function(compound_name) {
        tryCatch({
          if (is.na(compound_name) || compound_name == "") return(NA)
          Sys.sleep(0.15)
          
          # First, try using PubChemR if available
          if (requireNamespace("PubChemR", quietly = TRUE)) {
            tryCatch({
              result <- PubChemR::get_cid(compound_name)
              if (!is.null(result) && length(result) > 0) {
                return(as.character(result[1]))
              }
            }, error = function(e) NULL)
          }
          
          # Fallback: use httr to query PubChem REST API
          if (requireNamespace("httr", quietly = TRUE)) {
            Sys.sleep(0.15)  # Rate limiting
            
            # Try compound name lookup
            url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
                          URLencode(as.character(compound_name), reserved = TRUE),
                          "/cids/json")
            
            response <- httr::GET(url, httr::timeout(10))
            
            if (httr::status_code(response) == 200) {
              content_data <- httr::content(response, "parsed")
              if (!is.null(content_data$IdentifierList$CID) && 
                  length(content_data$IdentifierList$CID) > 0) {
                return(as.character(content_data$IdentifierList$CID[1]))
              }
            }
          }
          
          return("NOT_FOUND")
        }, error = function(e) stop("TEMP_ERROR"))
      }
      
      local_get_chebi_id_api_batch <- function(compound_name) {
        tryCatch({
          if (is.na(compound_name) || compound_name == "") return(NA)
          Sys.sleep(0.3)
          if (requireNamespace("webchem", quietly = TRUE)) {
            result <- webchem::get_chebiid(compound_name)
            if (!is.null(result) && nrow(result) > 0 && !is.na(result$chebiid[1])) {
              chebi_id <- as.character(result$chebiid[1])
              if (!grepl("^CHEBI:", chebi_id)) chebi_id <- paste0("CHEBI:", chebi_id)
              return(chebi_id)
            }
          }
          return("NOT_FOUND")
        }, error = function(e) stop("TEMP_ERROR"))
      }
      
      local_get_lipidmaps_id_api_batch <- function(compound_name) {
        # LipidMaps API not implemented - return NOT_FOUND
        return("NOT_FOUND")
      }
      
      # Call APIs for each metabolite
      # Try all name variants (original, cleaned, normalized, normalized-hyphen) to maximize chance of finding match
      for (idx_str in names(api_calls_needed)) {
        idx <- as.integer(idx_str)
        dbs_to_call <- api_calls_needed[[idx_str]]$databases
        
        # Prepare all name variants to try
        original_name <- metabolite_names[idx]
        cleaned_name <- cleaned_names[idx]
        normalized_name <- normalized_names[idx]
        normalized_name_hyphen <- normalized_names_hyphen[idx]
        
        for (db in dbs_to_call) {
          api_func <- switch(db,
            "kegg" = local_get_kegg_id_api_batch,
            "pubchem" = local_get_pubchem_cid_api_batch,
            "chebi" = local_get_chebi_id_api_batch,
            "lipidmaps" = local_get_lipidmaps_id_api_batch,
            NULL
          )
          
          if (!is.null(api_func)) {
            # OPTIMIZATION: Use higher retry count (3) for temporary_failure retries vs fresh searches (1)
            # This gives failed APIs more attempts to succeed on retry
            is_retry_from_cache <- !is.null(cache_retry_needed[[as.character(idx)]]) && db %in% cache_retry_needed[[as.character(idx)]]
            max_retries_for_db <- if (is_retry_from_cache) 10 else 1
            
            # Try name variants in order: original -> cleaned -> normalized-hyphen -> normalized
            name_variants <- c(original_name, cleaned_name, normalized_name_hyphen, normalized_name)
            result <- NULL
            
            for (variant in name_variants) {
              if (is.na(variant) || variant == "") next
              
              result <- retry_api_call_batch(api_func, variant, max_retries = max_retries_for_db)
              
              # If found, we have a result - stop trying variants
              # If temporary_failure, skip to next variant (don't retry same variant again)
              if (result$status == "found") {
                break
              } else if (result$status == "temporary_failure") {
                # Continue to next variant - don't retry same malformed name
                next
              }
              # If not_found, continue to next variant
            }
            
            # Store result and status from last attempt
            if (!is.null(result)) {
              api_calls_needed[[idx_str]]$statuses[[db]] <- result$status
              
              if (!is.na(result$id)) {
                col_name <- switch(db,
                  "kegg" = "kegg_id",
                  "pubchem" = "pubchem_cid",
                  "chebi" = "chebi_id",
                  "lipidmaps" = "lipidmaps_id"
                )
                results_df[[col_name]][idx] <- result$id
              }
            }
          }
        }
      }
    }
    
    # STEP 4: Cache ALL results (not just uncached ones) for future fast lookups
    # This ensures we update cache entries even if they were partially cached before
    tryCatch({
      message(sprintf("[BATCH-CACHE] Starting cache save for %d metabolites...", length(metabolite_names)))
      
      # Load existing cached_mappings
      cached_mappings <- load_cached_mappings()
      if (is.null(cached_mappings)) {
        cached_mappings <- data.frame(
          original_name = character(),
          clean_name = character(),
          normalized_name = character(),
          kegg_id = character(),
          kegg_status = character(),
          pubchem_cid = character(),
          pubchem_status = character(),
          hmdb_id = character(),
          chebi_id = character(),
          chebi_status = character(),
          lipidmaps_id = character(),
          lipidmaps_status = character(),
          databases_searched = character(),
          stringsAsFactors = FALSE
        )
      }
      
      # Column order MUST match database schema:
      # id, original_name, clean_name, kegg_id, kegg_status, pubchem_cid, pubchem_status,
      # hmdb_id, chebi_id, chebi_status, lipidmaps_id, lipidmaps_status, 
      # databases_searched, last_updated, normalized_name
      new_cache_entries <- data.frame(
        id = integer(),
        original_name = character(),
        clean_name = character(),
        kegg_id = character(),
        kegg_status = character(),
        pubchem_cid = character(),
        pubchem_status = character(),
        hmdb_id = character(),
        chebi_id = character(),
        chebi_status = character(),
        lipidmaps_id = character(),
        lipidmaps_status = character(),
        databases_searched = character(),
        last_updated = character(),
        normalized_name = character(),
        stringsAsFactors = FALSE
      )
      updated_count <- 0
      
      # Cache ALL metabolites that were searched
      # We cache even if IDs are not found to track "not_found" status and avoid repeated searches
      message(sprintf("[BATCH-CACHE] Processing %d metabolites for caching...", length(metabolite_names)))
      for (idx in seq_along(metabolite_names)) {
        idx_str <- as.character(idx)
        
        # Check if already in cache (match by clean_name, normalized_name, OR original_name)
        existing_idx <- which(
          cached_mappings$clean_name == results_df$clean_name[idx] |
          cached_mappings$normalized_name == results_df$normalized_name[idx] |
          cached_mappings$original_name == results_df$original_name[idx]
        )
          
        # Get API statuses if available
        api_statuses <- if (!is.null(api_calls_needed[[idx_str]])) api_calls_needed[[idx_str]]$statuses else list()
        
        # Determine status for each database
        kegg_status <- ""
        if ("kegg" %in% databases) {
          if (!is.null(api_statuses$kegg)) {
            kegg_status <- api_statuses$kegg
          } else if (is_valid_id(results_df$kegg_id[idx])) {
            kegg_status <- "found"
          } else if ("kegg" %in% databases) {
            kegg_status <- ""  # Not searched yet
          }
        }
        
        pubchem_status <- ""
        if ("pubchem" %in% databases) {
              if (!is.null(api_statuses$pubchem)) {
                pubchem_status <- api_statuses$pubchem
              } else if (is_valid_id(results_df$pubchem_cid[idx])) {
                pubchem_status <- "found"
              } else if ("pubchem" %in% databases) {
                pubchem_status <- ""
              }
            }
            
            chebi_status <- ""
            if ("chebi" %in% databases) {
              if (!is.null(api_statuses$chebi)) {
                chebi_status <- api_statuses$chebi
              } else if (is_valid_id(results_df$chebi_id[idx])) {
                chebi_status <- "found"
              } else if ("chebi" %in% databases) {
                chebi_status <- ""
              }
            }
        
        lipidmaps_status <- ""
        if ("lipidmaps" %in% databases) {
          if (!is.null(api_statuses$lipidmaps)) {
            lipidmaps_status <- api_statuses$lipidmaps
          } else if (is_valid_id(results_df$lipidmaps_id[idx])) {
            lipidmaps_status <- "found"
          } else if ("lipidmaps" %in% databases) {
            lipidmaps_status <- ""
          }
        }
        
        # Column order MUST match database schema:
        # id, original_name, clean_name, kegg_id, kegg_status, pubchem_cid, pubchem_status,
        # hmdb_id, chebi_id, chebi_status, lipidmaps_id, lipidmaps_status, 
        # databases_searched, last_updated, normalized_name
        new_entry <- data.frame(
          id = NA_integer_,  # Will be auto-generated by SQLite
          original_name = results_df$original_name[idx],
          clean_name = results_df$clean_name[idx],
          kegg_id = ifelse(is_valid_id(results_df$kegg_id[idx]), results_df$kegg_id[idx], ""),
          kegg_status = kegg_status,
          pubchem_cid = ifelse(is_valid_id(results_df$pubchem_cid[idx]), results_df$pubchem_cid[idx], ""),
          pubchem_status = pubchem_status,
          hmdb_id = ifelse(is_valid_id(results_df$hmdb_id[idx]), results_df$hmdb_id[idx], ""),
          chebi_id = ifelse(is_valid_id(results_df$chebi_id[idx]), results_df$chebi_id[idx], ""),
          chebi_status = chebi_status,
          lipidmaps_id = ifelse(is_valid_id(results_df$lipidmaps_id[idx]), results_df$lipidmaps_id[idx], ""),
          lipidmaps_status = lipidmaps_status,
          databases_searched = paste(sort(databases), collapse = ","),
          last_updated = as.character(Sys.time()),
          normalized_name = results_df$normalized_name[idx],
          stringsAsFactors = FALSE
        )
        
        if (length(existing_idx) > 0) {
          # Update existing entry
          cached_mappings[existing_idx[1], ] <- new_entry
          updated_count <- updated_count + 1
        } else {
          # Add new entry
          new_cache_entries <- rbind(new_cache_entries, new_entry)
        }
      }
      
      # Save cache if we have new entries OR updated existing ones
      if (nrow(new_cache_entries) > 0 || updated_count > 0) {
        if (nrow(new_cache_entries) > 0) {
          cached_mappings <- rbind(cached_mappings, new_cache_entries)
        }
        save_all_cached_mappings(cached_mappings)
        if (is_omics_verbose()) {
          message(sprintf("[BATCH-CACHE] Saved %d new and updated %d existing metabolites in cache", 
                         nrow(new_cache_entries), updated_count))
        }
      } else {
        if (is_omics_verbose()) {
          message(sprintf("[BATCH-CACHE] Nothing to save: new_entries=%d, updated=%d", 
                         nrow(new_cache_entries), updated_count))
        }
      }
        
    }, error = function(e) {
      message(sprintf("[BATCH-CACHE] Error saving to cache: %s", e$message))
    })
  }
  
  # Validate ID formats before returning
  for (col in c("kegg_id", "pubchem_cid", "hmdb_id", "chebi_id", "lipidmaps_id")) {
    db_name <- sub("_id|_cid", "", col)
    results_df[[col]] <- sapply(results_df[[col]], function(id) {
      if (!is.na(id) && !validate_id_format(id, db_name)) NA_character_ else id
    })
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[BATCH] Returning %d metabolites with IDs", nrow(results_df)))
  }
  
  return(results_df)
}

# ============================================================================
# FUZZY/SIMILARITY SEARCH FOR UNMATCHED METABOLITES
# ============================================================================

#' Fuzzy search for metabolites that weren't found by exact match
#' Uses SQL LIKE queries and string similarity to find potential matches
#' @param unmatched_names Character vector of metabolite names not found by exact search
#' @param max_suggestions Maximum number of suggestions per metabolite (default 5)
#' @param min_similarity Minimum string similarity score (0-1) to include suggestion (default 0.6)
#' @return Data frame with columns: original_name, suggested_name, similarity_score, kegg_id, pubchem_id, hmdb_id, chebi_id
fuzzy_search_metabolites <- function(unmatched_names, max_suggestions = 5, min_similarity = 0.6) {
  
  if (length(unmatched_names) == 0) {
    return(data.frame(
      original_name = character(),
      suggested_name = character(),
      similarity_score = numeric(),
      match_type = character(),
      kegg_id = character(),
      pubchem_id = character(),
      hmdb_id = character(),
      chebi_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  con <- get_metabolomics_connection()
  if (is.null(con)) {
    message("[FUZZY] No database connection available")
    return(NULL)
  }
  
  results <- data.frame(
    original_name = character(),
    suggested_name = character(),
    similarity_score = numeric(),
    match_type = character(),
    kegg_id = character(),
    pubchem_id = character(),
    hmdb_id = character(),
    chebi_id = character(),
    stringsAsFactors = FALSE
  )
  
  # String similarity function using Levenshtein distance
  string_similarity <- function(s1, s2) {
    if (is.na(s1) || is.na(s2) || s1 == "" || s2 == "") return(0)
    s1 <- tolower(s1)
    s2 <- tolower(s2)
    
    # Use adist (base R) for edit distance
    max_len <- max(nchar(s1), nchar(s2))
    if (max_len == 0) return(1)
    
    edit_dist <- utils::adist(s1, s2)[1,1]
    similarity <- 1 - (edit_dist / max_len)
    return(similarity)
  }
  
  # Check for common typo patterns
  detect_typo_type <- function(original, suggested) {
    orig_lower <- tolower(original)
    sugg_lower <- tolower(suggested)
    
    # Single character difference (typo)
    if (abs(nchar(orig_lower) - nchar(sugg_lower)) <= 1) {
      edit_dist <- utils::adist(orig_lower, sugg_lower)[1,1]
      if (edit_dist == 1) return("single_char_typo")
      if (edit_dist == 2) return("double_char_typo")
    }
    
    # Transposition (swapped letters)
    if (nchar(orig_lower) == nchar(sugg_lower)) {
      chars_orig <- strsplit(orig_lower, "")[[1]]
      chars_sugg <- strsplit(sugg_lower, "")[[1]]
      diffs <- which(chars_orig != chars_sugg)
      if (length(diffs) == 2 && diff(diffs) == 1) {
        if (chars_orig[diffs[1]] == chars_sugg[diffs[2]] && 
            chars_orig[diffs[2]] == chars_sugg[diffs[1]]) {
          return("transposition")
        }
      }
    }
    
    # Missing/extra hyphen
    orig_no_hyphen <- gsub("-", "", orig_lower)
    sugg_no_hyphen <- gsub("-", "", sugg_lower)
    if (orig_no_hyphen == sugg_no_hyphen) return("hyphen_variant")
    
    # Missing/extra space
    orig_no_space <- gsub(" ", "", orig_lower)
    sugg_no_space <- gsub(" ", "", sugg_lower)
    if (orig_no_space == sugg_no_space) return("space_variant")
    
    # Prefix match (e.g., "L-" vs no prefix)
    if (grepl("^[DLdl]-", orig_lower) || grepl("^[DLdl]-", sugg_lower)) {
      orig_no_prefix <- gsub("^[DLdl]-", "", orig_lower)
      sugg_no_prefix <- gsub("^[DLdl]-", "", sugg_lower)
      if (orig_no_prefix == sugg_no_prefix) return("stereoisomer_prefix")
    }
    
    return("similar_name")
  }
  
  for (name in unmatched_names) {
    if (is.na(name) || name == "") next
    
    # Clean and prepare search terms
    clean_name <- tolower(trimws(name))
    normalized <- gsub("[^a-zA-Z0-9]", "", clean_name)
    
    # Strategy 1: LIKE search with wildcards for partial matches
    # Handle common typo patterns: missing letters, extra letters, wrong letters
    like_patterns <- c()
    
    # Remove vowels for consonant skeleton matching (catches many typos)
    consonant_pattern <- gsub("[aeiou]", "%", clean_name)
    like_patterns <- c(like_patterns, consonant_pattern)
    
    # First few and last few characters
    if (nchar(clean_name) >= 4) {
      prefix <- substr(clean_name, 1, 4)
      suffix <- substr(clean_name, nchar(clean_name) - 3, nchar(clean_name))
      like_patterns <- c(like_patterns, paste0(prefix, "%"), paste0("%", suffix))
    }
    
    # Main word without common prefixes
    main_word <- gsub("^[dlsDLS][-]?", "", clean_name)
    if (nchar(main_word) >= 3 && main_word != clean_name) {
      like_patterns <- c(like_patterns, paste0("%", main_word, "%"))
    }
    
    # Build LIKE query
    like_clauses <- sapply(unique(like_patterns), function(p) {
      p_escaped <- gsub("'", "''", p)
      sprintf("LOWER(name) LIKE '%s' OR LOWER(iupac_name) LIKE '%s' OR LOWER(traditional_iupac) LIKE '%s'",
              p_escaped, p_escaped, p_escaped)
    })
    
    like_where <- paste(like_clauses, collapse = " OR ")
    
    tryCatch({
      query <- sprintf("
        SELECT DISTINCT name, iupac_name, traditional_iupac, kegg_id, pubchem_id, hmdb_id, chebi_id
        FROM metabolites
        WHERE %s
        LIMIT 50
      ", like_where)
      
      candidates <- DBI::dbGetQuery(con, query)
      
      if (!is.null(candidates) && nrow(candidates) > 0) {
        # Calculate similarity scores for each candidate
        candidate_scores <- lapply(seq_len(nrow(candidates)), function(i) {
          row <- candidates[i, ]
          
          # Check similarity against all name columns
          name_sim <- string_similarity(name, row$name)
          iupac_sim <- string_similarity(name, row$iupac_name)
          trad_sim <- string_similarity(name, row$traditional_iupac)
          
          best_sim <- max(name_sim, iupac_sim, trad_sim, na.rm = TRUE)
          
          # Determine which name matched best
          if (name_sim == best_sim && !is.na(row$name)) {
            best_name <- row$name
          } else if (iupac_sim == best_sim && !is.na(row$iupac_name)) {
            best_name <- row$iupac_name
          } else {
            best_name <- row$traditional_iupac
          }
          
          list(
            suggested_name = best_name,
            similarity_score = best_sim,
            match_type = detect_typo_type(name, best_name),
            kegg_id = row$kegg_id,
            pubchem_id = row$pubchem_id,
            hmdb_id = row$hmdb_id,
            chebi_id = row$chebi_id
          )
        })
        
        # Filter by minimum similarity and sort by score
        filtered_candidates <- candidate_scores[sapply(candidate_scores, function(x) x$similarity_score >= min_similarity)]
        
        if (length(filtered_candidates) > 0) {
          # Sort by similarity score descending
          scores <- sapply(filtered_candidates, function(x) x$similarity_score)
          filtered_candidates <- filtered_candidates[order(scores, decreasing = TRUE)]
          
          # Take top N suggestions
          top_candidates <- head(filtered_candidates, max_suggestions)
          
          for (cand in top_candidates) {
            results <- rbind(results, data.frame(
              original_name = name,
              suggested_name = cand$suggested_name,
              similarity_score = round(cand$similarity_score, 3),
              match_type = cand$match_type,
              kegg_id = ifelse(is.na(cand$kegg_id) || cand$kegg_id == "", "", cand$kegg_id),
              pubchem_id = ifelse(is.na(cand$pubchem_id) || cand$pubchem_id == "", "", cand$pubchem_id),
              hmdb_id = ifelse(is.na(cand$hmdb_id) || cand$hmdb_id == "", "", cand$hmdb_id),
              chebi_id = ifelse(is.na(cand$chebi_id) || cand$chebi_id == "", "", cand$chebi_id),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }, error = function(e) {
      if (is_omics_verbose()) message(sprintf("[FUZZY] Error searching for '%s': %s", name, e$message))
    })
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[FUZZY] Found %d suggestions for %d unmatched metabolites", 
                   nrow(results), length(unmatched_names)))
  }
  
  return(results)
}

# Metabolite worker function - now uses batch queries for local DB, then API fallback
metabolite_worker <- function(db_row, databases) {
  
  # metabolite_worker now receives a pre-populated data frame row from batch_search_metabolites
  # Its job is ONLY to fill in missing IDs via external API calls
  
  if (!is.data.frame(db_row) || nrow(db_row) != 1) {
    stop("metabolite_worker: db_row must be a data frame with exactly 1 row")
  }
  
  compound_name <- db_row$original_name[1]
  
  if (is_omics_verbose()) {
    message(sprintf("[WORKER] Processing '%s' (looking for missing IDs from APIs)", compound_name))
  }
  
  # Define local helper functions to avoid capturing global environment
  local_clean_molecule_name <- function(name) {
    if (is.na(name) || name == "") return(NA)
    name <- as.character(name)
    name <- stringr::str_remove(name, "^\\s*\\d+\\s*[-:]\\s*")
    name <- stringr::str_remove(name, "^\\s*\\d+\\s*\\.")
    name <- stringr::str_remove(name, "^\\s*[A-Z]\\d*\\s*[-:]\\s*")
    name <- stringr::str_trim(name)
    name <- tolower(name)
    name <- stringr::str_replace_all(name, "[^a-zA-Z0-9\\s]", " ")
    name <- stringr::str_replace_all(name, "\\s+", " ")
    name <- stringr::str_trim(name)
    return(name)
  }
  
  # Enhanced retry function that distinguishes between permanent and temporary failures
  retry_api_call <- function(api_function, compound_name, max_retries = 2, timeout_seconds = 15) {
    for (attempt in 1:(max_retries + 1)) {
      result <- tryCatch({
        old_timeout <- getOption("timeout")
        options(timeout = timeout_seconds)
        on.exit(options(timeout = old_timeout), add = TRUE)
        api_function(compound_name)
      }, error = function(e) {
        if (grepl("timeout|elapsed time", conditionMessage(e), ignore.case = TRUE)) {
          if (is_omics_verbose()) message(sprintf("[WORKER-API] Timeout on attempt %d/%d", attempt, max_retries + 1))
        }
        "TEMP_ERROR"
      })
      
      if (is.character(result) && result == "NOT_FOUND") {
        return(NA)
      } else if (is.character(result) && result == "TEMP_ERROR") {
        if (attempt <= max_retries) {
          backoff_seconds <- attempt * 0.5
          Sys.sleep(backoff_seconds)
          next
        } else {
          return(NA)
        }
      } else if (!is.na(result)) {
        return(result)
      }
    }
    return(NA)
  }
  
  # External API calls - only for missing IDs not found in local database
  local_get_kegg_id_api <- function(compound_name) {
    tryCatch({
      if (is.na(compound_name) || compound_name == "") return(NA)
      Sys.sleep(0.2)
      result <- KEGGREST::keggFind("compound", compound_name)
      if (length(result) > 0) {
        kegg_id <- names(result)[1]
        if (grepl("^cpd:", kegg_id)) kegg_id <- sub("^cpd:", "", kegg_id)
        return(kegg_id)
      } else {
        return("NOT_FOUND")
      }
    }, error = function(e) stop("TEMP_ERROR"))
  }
  
  local_get_pubchem_cid_api <- function(compound_name) {
    tryCatch({
      if (is.na(compound_name) || compound_name == "") return(NA)
      
      # First, try using PubChemR if available
      if (requireNamespace("PubChemR", quietly = TRUE)) {
        tryCatch({
          result <- PubChemR::get_cid(compound_name)
          if (!is.null(result) && length(result) > 0) {
            return(as.character(result[1]))
          }
        }, error = function(e) NULL)
      }
      
      # Fallback: use httr to query PubChem REST API
      if (requireNamespace("httr", quietly = TRUE)) {
        Sys.sleep(0.15)  # Rate limiting
        
        # Try compound name lookup
        url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
                      URLencode(as.character(compound_name), reserved = TRUE),
                      "/cids/json")
        
        response <- httr::GET(url, httr::timeout(10))
        
        if (httr::status_code(response) == 200) {
          content_data <- httr::content(response, "parsed")
          if (!is.null(content_data$IdentifierList$CID) && 
              length(content_data$IdentifierList$CID) > 0) {
            return(as.character(content_data$IdentifierList$CID[1]))
          }
        }
      }
      
      return("NOT_FOUND")
    }, error = function(e) {
      if (is_omics_verbose()) {
        message("[PubChem] Error querying '", compound_name, "': ", e$message)
      }
      stop("TEMP_ERROR")
    })
  }
  
  local_get_chebi_id_api <- function(compound_name) {
    tryCatch({
      if (is.na(compound_name) || compound_name == "") return(NA)
      Sys.sleep(0.3)
      if (requireNamespace("webchem", quietly = TRUE)) {
        result <- webchem::get_chebiid(compound_name)
        if (!is.null(result) && nrow(result) > 0 && !is.na(result$chebiid[1])) {
          chebi_id <- as.character(result$chebiid[1])
          if (!grepl("^CHEBI:", chebi_id)) chebi_id <- paste0("CHEBI:", chebi_id)
          return(chebi_id)
        } else {
          return("NOT_FOUND")
        }
      } else {
        return("NOT_FOUND")
      }
    }, error = function(e) stop("TEMP_ERROR"))
  }
  
  # Call APIs ONLY for requested databases where ID is still missing
  if (is_omics_verbose()) {
    message(sprintf("[WORKER] Checking for missing IDs in '%s'", compound_name))
  }
  
  # Collect APIs to call for missing IDs
  apis_to_call <- list()
  if ("kegg" %in% databases && is.na(db_row$kegg_id[1])) {
    apis_to_call$kegg <- local_get_kegg_id_api
  }
  if ("pubchem" %in% databases && is.na(db_row$pubchem_cid[1])) {
    apis_to_call$pubchem <- local_get_pubchem_cid_api
  }
  if ("chebi" %in% databases && is.na(db_row$chebi_id[1])) {
    apis_to_call$chebi <- local_get_chebi_id_api
  }
  
  # If APIs to call, try them
  if (length(apis_to_call) > 0) {
    if (is_omics_verbose()) {
      message(sprintf("[WORKER] Calling %d APIs for '%s'", length(apis_to_call), compound_name))
    }
    
    # Try parallel API calls if multiple needed
    if (length(apis_to_call) > 1) {
      tryCatch({
        old_plan <- future::plan()
        on.exit(future::plan(old_plan), add = TRUE)
        future::plan(future::multisession, workers = min(length(apis_to_call), 2))
        
        api_results <- future.apply::future_lapply(
          names(apis_to_call),
          function(db_name) {
            api_func <- apis_to_call[[db_name]]
            retry_api_call(api_func, compound_name, max_retries = 2)
          },
          future.seed = TRUE
        )
        
        if (!is.null(api_results) && length(api_results) == length(apis_to_call)) {
          for (i in seq_along(apis_to_call)) {
            db_name <- names(apis_to_call)[i]
            result <- api_results[[i]]
            if (!is.na(result) && result != "NOT_FOUND") {
              col_name <- if (db_name == "pubchem") "pubchem_cid" else if (db_name == "chebi") "chebi_id" else paste0(db_name, "_id")
              db_row[[col_name]][1] <<- result
            }
          }
        }
      }, error = function(e) {
        if (is_omics_verbose()) message(sprintf("[WORKER] Parallel API failed, trying sequential: %s", e$message))
        for (db_name in names(apis_to_call)) {
          api_func <- apis_to_call[[db_name]]
          result <- retry_api_call(api_func, compound_name, max_retries = 2)
          if (!is.na(result) && result != "NOT_FOUND") {
            col_name <- if (db_name == "pubchem") "pubchem_cid" else if (db_name == "chebi") "chebi_id" else paste0(db_name, "_id")
            db_row[[col_name]][1] <<- result
          }
        }
      })
    } else if (length(apis_to_call) == 1) {
      db_name <- names(apis_to_call)[1]
      api_func <- apis_to_call[[db_name]]
      result <- retry_api_call(api_func, compound_name, max_retries = 2)
      if (!is.na(result) && result != "NOT_FOUND") {
        col_name <- if (db_name == "pubchem") "pubchem_cid" else if (db_name == "chebi") "chebi_id" else paste0(db_name, "_id")
        db_row[[col_name]][1] <- result
      }
    }
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[WORKER] Returning for '%s': kegg=%s, pubchem=%s, hmdb=%s, chebi=%s",
                   compound_name,
                   ifelse(is.na(db_row$kegg_id[1]), "NA", db_row$kegg_id[1]),
                   ifelse(is.na(db_row$pubchem_cid[1]), "NA", db_row$pubchem_cid[1]),
                   ifelse(is.na(db_row$hmdb_id[1]), "NA", db_row$hmdb_id[1]),
                   ifelse(is.na(db_row$chebi_id[1]), "NA", db_row$chebi_id[1])))
  }
  
  return(db_row)
}

# Main function to resolve metabolite IDs using batch queries
resolve_ids <- function(metabolite_names, databases = c("kegg", "pubchem", "hmdb", "chebi", "lipidmaps"), 
                       max_workers = 4, progress_callback = NULL, in_parallel_context = FALSE) {
  
  if (length(metabolite_names) == 0) {
    return(data.frame(
      original_name = character(),
      kegg_id = character(),
      pubchem_cid = character(),
      hmdb_id = character(),
      chebi_id = character(),
      lipidmaps_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Initialize progress
  if (!is.null(progress_callback)) {
    progress_callback(0, paste("Starting ID mapping for", length(metabolite_names), "metabolites..."))
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[RESOLVE] Starting batch search for %d metabolites (persistent connection)", length(metabolite_names)))
  }
  
  # OPTIMIZATION #9: Use single persistent connection and batch SQL queries
  # Get all local database results at once using batch queries
  db_results <- batch_search_metabolites(metabolite_names, databases)
  
  if (!is.null(progress_callback)) {
    progress_callback(0.5, "Local database search complete, checking APIs for missing IDs...")
  }
  
  # OPTIMIZATION #10: Use metabolite_worker only for API calls on rows with missing IDs
  # This avoids redundant database queries and reuses batch results
  if (length(metabolite_names) > 0) {
    for (i in seq_along(metabolite_names)) {
      if (!is.null(progress_callback) && i %% 5 == 0) {
        progress_callback(0.5 + (i / length(metabolite_names)) * 0.5, 
                         paste("Processing API requests for metabolite", i, "of", length(metabolite_names)))
      }
      
      # metabolite_worker now receives data frame row and only calls APIs for missing IDs
      db_results[i, ] <- metabolite_worker(db_results[i, ], databases)
    }
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(1, "ID mapping completed!")
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[RESOLVE] Complete: returned %d metabolites", nrow(db_results)))
  }
  
  # Return only the standard columns (drop clean_name, normalized_name which are for caching)
  return(db_results[, c("original_name", "kegg_id", "pubchem_cid", "hmdb_id", "chebi_id", "lipidmaps_id")])
}

# Main function to resolve metabolite IDs
old_resolve_ids_deprecated <- function(metabolite_names, databases = c("kegg", "pubchem", "hmdb", "chebi", "lipidmaps"), 
                       max_workers = 4, progress_callback = NULL, in_parallel_context = FALSE) {
  
  if (length(metabolite_names) == 0) {
    return(data.frame(
      original_name = character(),
      kegg_id = character(),
      pubchem_cid = character(),
      hmdb_id = character(),
      chebi_id = character(),
      lipidmaps_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Initialize progress
  if (!is.null(progress_callback)) {
    progress_callback(0, paste("Starting ID mapping for", length(metabolite_names), "metabolites..."))
  }
  
  # No need to load database into memory - queries will be on-demand
  if (is_omics_verbose()) message("Using on-demand database queries (no server startup delay)")
  
  # Process each metabolite and collect results
  results <- list()
  
  for (i in seq_along(metabolite_names)) {
    name <- metabolite_names[i]
    
    # Update progress (every 5 or for single metabolites)
    if (!is.null(progress_callback) && (i %% 5 == 0 || length(metabolite_names) == 1)) {
      progress_callback(i / length(metabolite_names), 
                       paste("Processing metabolite", i, "of", length(metabolite_names)))
    }
    
    if (is_omics_verbose()) message(sprintf("[RESOLVE] Processing '%s' (%d/%d)", name, i, length(metabolite_names)))
    
    # Call metabolite_worker for this single metabolite
    worker_result <- metabolite_worker(name, databases)
    
    # metabolite_worker now returns a data frame row
    results[[i]] <- worker_result
  }
  
  # Combine all results into single data frame
  if (length(results) == 0) {
    result_df <- data.frame(
      original_name = character(),
      kegg_id = character(),
      pubchem_cid = character(),
      hmdb_id = character(),
      chebi_id = character(),
      lipidmaps_id = character(),
      stringsAsFactors = FALSE
    )
  } else {
    # Combine data frames from all metabolites
    result_df <- do.call(rbind, results)
    
    # Ensure all expected columns exist
    expected_cols <- c("original_name", "kegg_id", "pubchem_cid", "hmdb_id", "chebi_id", "lipidmaps_id")
    for (col in expected_cols) {
      if (!col %in% names(result_df)) {
        result_df[[col]] <- NA_character_
      }
    }
    
    # Reorder columns to standard order
    result_df <- result_df[, expected_cols]
    
    # Reset row names
    rownames(result_df) <- NULL
  }
  
  # Final progress update
  if (!is.null(progress_callback)) {
    progress_callback(1, "ID mapping completed!")
  }
  
  if (is_omics_verbose()) {
    message(sprintf("[DEBUG] resolve_ids returning: %d rows, columns: %s", nrow(result_df), paste(names(result_df), collapse=", ")))
    if (nrow(result_df) > 0) {
      message(sprintf("[DEBUG]   Row 1: original_name='%s', kegg_id='%s', pubchem_cid='%s', hmdb_id='%s'", 
                     result_df$original_name[1], result_df$kegg_id[1], result_df$pubchem_cid[1], result_df$hmdb_id[1]))
    }
  }
  
  return(result_df)
}

# ============================================================================
# END METABOLOMICS ID MAPPING FUNCTIONS
# ============================================================================

# ============================================================================
# METABOLOMICS CACHING SYSTEM
# ============================================================================

# Flag to track if metabolomics cache tables have been initialized
.metabolomics_cache_initialized <- FALSE

# Lazy initialization of metabolomics cache tables (only when first needed)
initialize_metabolomics_cache_tables <- function() {
  # Skip if already initialized
  if (.metabolomics_cache_initialized) {
    return(TRUE)
  }
  
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Create classification cache table
    existing_tables <- DBI::dbListTables(con)
    
    if ("cached_classifications" %in% existing_tables) {
      # Table exists, check if it needs migration
      table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(cached_classifications)")
      has_status <- "status" %in% table_info$name
      
      if (!has_status) {
        # Migrate old table to new schema
        tryCatch({
          message("[CACHE] Migrating cached_classifications table to new schema with status tracking...")
          DBI::dbExecute(con, "ALTER TABLE cached_classifications ADD COLUMN status TEXT DEFAULT 'found'")
          DBI::dbExecute(con, "ALTER TABLE cached_classifications ADD COLUMN error_message TEXT")
          message("[CACHE] Migration successful")
        }, error = function(e) {
          message("[CACHE] Warning: Could not migrate classifications table: ", e$message)
        })
      }
    } else {
      # Create new table with status tracking
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS cached_classifications (
          metabolite_name TEXT PRIMARY KEY,
          name_normalized TEXT,
          kingdom TEXT,
          super_class TEXT,
          class TEXT,
          sub_class TEXT,
          direct_parent TEXT,
          source TEXT DEFAULT 'pattern',
          status TEXT DEFAULT 'found',
          error_message TEXT,
          last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
    }
    
    # Create index for fast lookups
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_class_name ON cached_classifications(name_normalized)")
    }, error = function(e) {})
    
    # Clear expired entries (older than 180 days) on initialization
    tryCatch({
      DBI::dbExecute(con, "
        DELETE FROM cached_classifications 
        WHERE datetime(last_updated) <= datetime('now', '-180 days')
      ")
    }, error = function(e) {})
    
    # Create KEGG pathway cache table
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS cached_kegg_pathways (
        kegg_id TEXT,
        pathway_id TEXT,
        pathway_name TEXT,
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (kegg_id, pathway_id)
      )
    ")
    
    # Create index on kegg_id
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pathway_kegg ON cached_kegg_pathways(kegg_id)")
    }, error = function(e) {})
    
    # Create KEGG module cache table (parallel to pathway cache)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS cached_kegg_modules (
        kegg_id TEXT,
        module_id TEXT,
        module_name TEXT,
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (kegg_id, module_id)
      )
    ")
    
    # Create index on kegg_id for modules
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_module_kegg ON cached_kegg_modules(kegg_id)")
    }, error = function(e) {})
    
    # Create HMDB classification cache table with status tracking (180-day expiration)
    existing_tables <- DBI::dbListTables(con)
    
    if ("cached_hmdb_classifications" %in% existing_tables) {
      # Table exists, check if it needs migration
      table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(cached_hmdb_classifications)")
      has_status <- "status" %in% table_info$name
      
      if (!has_status) {
        # Migrate old table to new schema
        tryCatch({
          message("[CACHE] Migrating cached_hmdb_classifications table to new schema with status tracking...")
          DBI::dbExecute(con, "ALTER TABLE cached_hmdb_classifications ADD COLUMN status TEXT DEFAULT 'found'")
          DBI::dbExecute(con, "ALTER TABLE cached_hmdb_classifications ADD COLUMN error_message TEXT")
          message("[CACHE] Migration successful")
        }, error = function(e) {
          message("[CACHE] Warning: Could not migrate classifications table: ", e$message)
        })
      }
    } else {
      # Create new table with status tracking
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS cached_hmdb_classifications (
          hmdb_id TEXT PRIMARY KEY,
          kingdom TEXT,
          super_class TEXT,
          class TEXT,
          sub_class TEXT,
          direct_parent TEXT,
          description TEXT,
          status TEXT DEFAULT 'found',
          error_message TEXT,
          last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
    }
    
    # Create index for fast lookups
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_hmdb_class ON cached_hmdb_classifications(hmdb_id)")
    }, error = function(e) {})
    
    # Clear expired entries (older than 180 days) on initialization
    tryCatch({
      DBI::dbExecute(con, "
        DELETE FROM cached_hmdb_classifications 
        WHERE datetime(last_updated) <= datetime('now', '-180 days')
      ")
    }, error = function(e) {})
    
    # Create HMDB associations cache table with status tracking (180-day expiration)
    # First, check if table exists and needs migration
    existing_tables <- DBI::dbListTables(con)
    
    if ("cached_hmdb_associations" %in% existing_tables) {
      # Table exists, check if it has status column
      table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(cached_hmdb_associations)")
      has_status <- "status" %in% table_info$name
      
      if (!has_status) {
        # Migrate old table to new schema
        tryCatch({
          message("[CACHE] Migrating cached_hmdb_associations table to new schema with status tracking...")
          DBI::dbExecute(con, "ALTER TABLE cached_hmdb_associations ADD COLUMN status TEXT DEFAULT 'found'")
          DBI::dbExecute(con, "ALTER TABLE cached_hmdb_associations ADD COLUMN error_message TEXT")
          message("[CACHE] Migration successful")
        }, error = function(e) {
          message("[CACHE] Warning: Could not migrate table: ", e$message)
        })
      }
    } else {
      # Create new table
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS cached_hmdb_associations (
          hmdb_id TEXT PRIMARY KEY,
          metabolite_name TEXT,
          diseases TEXT,
          biospecimens TEXT,
          tissues TEXT,
          genes TEXT,
          proteins TEXT,
          pmids TEXT,
          status TEXT DEFAULT 'found',
          error_message TEXT,
          last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
    }
    
    # Create index on hmdb_id for fast lookups
    tryCatch({
      DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_assoc_hmdb ON cached_hmdb_associations(hmdb_id)")
    }, error = function(e) {})
    
    # Clear expired entries (older than 180 days) on initialization
    tryCatch({
      DBI::dbExecute(con, "
        DELETE FROM cached_hmdb_associations 
        WHERE datetime(last_updated) <= datetime('now', '-180 days')
      ")
    }, error = function(e) {})
    
    # Mark as initialized
    assign(".metabolomics_cache_initialized", TRUE, envir = .GlobalEnv)
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# In-memory cache for session-specific computed results
.metabolomics_session_cache <- new.env(parent = emptyenv())

# Get/set session cache
get_session_cache <- function(key) {
  if (exists(key, envir = .metabolomics_session_cache)) {
    return(get(key, envir = .metabolomics_session_cache))
  }
  return(NULL)
}

set_session_cache <- function(key, value) {
  assign(key, value, envir = .metabolomics_session_cache)
}

clear_session_cache <- function(pattern = NULL) {
  if (is.null(pattern)) {
    rm(list = ls(envir = .metabolomics_session_cache), envir = .metabolomics_session_cache)
  } else {
    keys <- ls(envir = .metabolomics_session_cache)
    to_remove <- keys[grepl(pattern, keys)]
    if (length(to_remove) > 0) {
      rm(list = to_remove, envir = .metabolomics_session_cache)
    }
  }
}

# ============================================================================
# CACHED CLASSIFICATION FUNCTIONS
# ============================================================================

# Load cached classifications from SQLite
load_cached_classifications <- function(metabolite_names, include_failures = FALSE) {
  tryCatch({
    if (!file.exists("metabolites.db") || length(metabolite_names) == 0) {
      return(NULL)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Check if table exists
    tables <- DBI::dbListTables(con)
    if (!"cached_classifications" %in% tables) {
      DBI::dbDisconnect(con)
      return(NULL)
    }
    
    # Normalize names for lookup
    normalized_names <- tolower(trimws(metabolite_names))
    
    # 180-day cache expiration
    expiration_days <- 180
    
    # By default, only return successful lookups (status = 'found')
    # If include_failures = TRUE, also return temporary_failure entries for retry
    status_filter <- if (include_failures) {
      "AND (status = 'found' OR status = 'temporary_failure')"
    } else {
      "AND status = 'found'"
    }
    
    # Query cached classifications with expiration check using proper SQL
    # Build query with individual OR conditions to avoid parameter issues
    if (length(normalized_names) > 0) {
      # Escape single quotes in names
      escaped_names <- gsub("'", "''", normalized_names)
      or_conditions <- paste0("'", escaped_names, "'", collapse = " OR name_normalized = ")
      query <- sprintf(
        "SELECT * FROM cached_classifications WHERE (name_normalized = %s) AND datetime(last_updated) > datetime('now', '-%d days') %s",
        or_conditions, expiration_days, status_filter
      )
      
      cached_data <- DBI::dbGetQuery(con, query)
    } else {
      cached_data <- data.frame()
    }
    
    DBI::dbDisconnect(con)
    
    if (nrow(cached_data) > 0) {
      return(cached_data)
    }
    return(NULL)
  }, error = function(e) {
    message("Error loading cached classifications: ", e$message)
    return(NULL)
  })
}

# Save classifications to SQLite cache
save_classifications_to_cache <- function(classification_df, status = "found", error_message = NULL) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.null(classification_df) || nrow(classification_df) == 0) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Ensure error_message is properly set
    error_msg <- if (is.null(error_message) || length(error_message) == 0) "" else as.character(error_message[1])
    
    # Prepare data for insertion
    cache_data <- data.frame(
      metabolite_name = classification_df$metabolite_name,
      name_normalized = sapply(classification_df$metabolite_name, function(n) tolower(trimws(n))),
      kingdom = classification_df$kingdom,
      super_class = classification_df$super_class,
      class = classification_df$class,
      sub_class = classification_df$sub_class,
      direct_parent = classification_df$direct_parent,
      source = ifelse("source" %in% names(classification_df), classification_df$source, "pattern"),
      status = status,
      error_message = error_msg,
      stringsAsFactors = FALSE
    )
    
    # Upsert (INSERT OR REPLACE)
    for (i in seq_len(nrow(cache_data))) {
      tryCatch({
        DBI::dbExecute(con, "
          INSERT OR REPLACE INTO cached_classifications 
          (metabolite_name, name_normalized, kingdom, super_class, class, sub_class, direct_parent, source, status, error_message)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ", params = as.list(cache_data[i, 1:10]))
      }, error = function(e) {})
    }
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error saving classifications to cache: ", e$message)
    return(FALSE)
  })
}

# Mark classification lookup as failed
mark_classification_failure <- function(metabolite_name, error_type = "temporary_failure", error_message = NULL) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.na(metabolite_name) || metabolite_name == "") {
      return(FALSE)
    }
    
    initialize_metabolomics_cache_tables()
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    error_msg <- if (is.null(error_message) || length(error_message) == 0) "" else as.character(error_message[1])
    
    # Insert or replace with failure status
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_classifications 
      (metabolite_name, name_normalized, status, error_message)
      VALUES (?, ?, ?, ?)
    ", params = list(
      metabolite_name, tolower(trimws(metabolite_name)), error_type, error_msg
    ))
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error marking classification failure: ", e$message)
    return(FALSE)
  })
}

# Cached version of classify_metabolites
classify_metabolites_cached <- function(metabolite_names, hmdb_ids = NULL, use_online = FALSE, progress_callback = NULL) {
  # Lazy initialize cache tables on first use
  initialize_metabolomics_cache_tables()
  
  n_metabolites <- length(metabolite_names)
  
  if (n_metabolites == 0) {
    return(data.frame(
      metabolite_name = character(),
      kingdom = character(),
      super_class = character(),
      class = character(),
      sub_class = character(),
      direct_parent = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Check session cache first (fastest)
  cache_key <- digest::digest(paste(sort(metabolite_names), collapse = "|"))
  session_cached <- get_session_cache(paste0("classification_", cache_key))
  if (!is.null(session_cached)) {
    return(session_cached)
  }
  
  # Check SQLite cache
  cached_results <- load_cached_classifications(metabolite_names)
  
  # Determine which metabolites need classification
  if (!is.null(cached_results)) {
    cached_names <- tolower(trimws(cached_results$metabolite_name))
    uncached_names <- metabolite_names[!tolower(trimws(metabolite_names)) %in% cached_names]
    uncached_hmdb <- if (!is.null(hmdb_ids)) hmdb_ids[!tolower(trimws(metabolite_names)) %in% cached_names] else NULL
  } else {
    uncached_names <- metabolite_names
    uncached_hmdb <- hmdb_ids
  }
  
  # Classify uncached metabolites
  new_results <- NULL
  if (length(uncached_names) > 0) {
    new_results <- classify_metabolites(uncached_names, uncached_hmdb, use_online, progress_callback)
    
    # Save new results to SQLite cache
    if (!is.null(new_results) && nrow(new_results) > 0) {
      save_classifications_to_cache(new_results)
    }
  }
  
  # Combine cached and new results
  if (!is.null(cached_results) && !is.null(new_results)) {
    # Align columns
    cached_results <- cached_results[, c("metabolite_name", "kingdom", "super_class", "class", "sub_class", "direct_parent")]
    if (!"hmdb_id" %in% names(cached_results)) cached_results$hmdb_id <- ""
    if (!"hmdb_id" %in% names(new_results)) new_results$hmdb_id <- ""
    result_df <- rbind(cached_results, new_results[, names(cached_results)])
  } else if (!is.null(cached_results)) {
    result_df <- cached_results[, c("metabolite_name", "kingdom", "super_class", "class", "sub_class", "direct_parent")]
    if (!"hmdb_id" %in% names(result_df)) result_df$hmdb_id <- ""
  } else {
    result_df <- new_results
  }
  
  # Reorder to match input order
  if (!is.null(result_df) && nrow(result_df) > 0) {
    result_df <- result_df[match(tolower(trimws(metabolite_names)), tolower(trimws(result_df$metabolite_name))), ]
    rownames(result_df) <- NULL
  }
  
  # Store in session cache
  set_session_cache(paste0("classification_", cache_key), result_df)
  
  return(result_df)
}

# ============================================================================
# CACHED KEGG PATHWAY FUNCTIONS
# ============================================================================

# Load cached KEGG pathways from SQLite
load_cached_kegg_pathways <- function(kegg_ids) {
  tryCatch({
    if (!file.exists("metabolites.db") || length(kegg_ids) == 0) {
      return(NULL)
    }
    
    valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
    if (length(valid_ids) == 0) return(NULL)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    tables <- DBI::dbListTables(con)
    if (!"cached_kegg_pathways" %in% tables) {
      DBI::dbDisconnect(con)
      return(NULL)
    }
    
    # Clean IDs
    clean_ids <- gsub("^cpd:", "", valid_ids)
    
    # Build query with individual OR conditions to avoid parameter issues
    escaped_ids <- gsub("'", "''", clean_ids)
    or_conditions <- paste0("'", escaped_ids, "'", collapse = " OR kegg_id = ")
    query <- sprintf("SELECT * FROM cached_kegg_pathways WHERE kegg_id = %s", or_conditions)
    cached_data <- DBI::dbGetQuery(con, query)
    
    DBI::dbDisconnect(con)
    
    if (nrow(cached_data) > 0) {
      return(cached_data)
    }
    return(NULL)
  }, error = function(e) {
    message("Error loading cached KEGG pathways: ", e$message)
    return(NULL)
  })
}

# Save KEGG pathways to SQLite cache
save_kegg_pathways_to_cache <- function(pathway_df) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.null(pathway_df) || nrow(pathway_df) == 0) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    for (i in seq_len(nrow(pathway_df))) {
      tryCatch({
        DBI::dbExecute(con, "
          INSERT OR REPLACE INTO cached_kegg_pathways (kegg_id, pathway_id, pathway_name)
          VALUES (?, ?, ?)
        ", params = list(pathway_df$kegg_id[i], pathway_df$pathway_id[i], pathway_df$pathway_name[i]))
      }, error = function(e) {})
    }
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error saving KEGG pathways to cache: ", e$message)
    return(FALSE)
  })
}

# Mark KEGG ID as having no pathways (negative caching)
save_kegg_no_pathways <- function(kegg_id) {
  tryCatch({
    if (!file.exists("metabolites.db")) return(FALSE)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_kegg_pathways (kegg_id, pathway_id, pathway_name)
      VALUES (?, 'NONE', 'No pathways found')
    ", params = list(gsub("^cpd:", "", kegg_id)))
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# ============================================================================
# KEGG MODULE CACHING FUNCTIONS (PARALLEL TO PATHWAY CACHING)
# ============================================================================

#' Load KEGG modules from SQLite cache
load_cached_kegg_modules <- function(kegg_ids) {
  tryCatch({
    if (!file.exists("metabolites.db") || length(kegg_ids) == 0) {
      return(NULL)
    }
    
    valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
    if (length(valid_ids) == 0) return(NULL)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    tables <- DBI::dbListTables(con)
    if (!"cached_kegg_modules" %in% tables) {
      DBI::dbDisconnect(con)
      return(NULL)
    }
    
    # Clean IDs
    clean_ids <- gsub("^cpd:", "", valid_ids)
    
    # Build query with individual OR conditions to avoid parameter issues
    escaped_ids <- gsub("'", "''", clean_ids)
    or_conditions <- paste0("'", escaped_ids, "'", collapse = " OR kegg_id = ")
    query <- sprintf("SELECT * FROM cached_kegg_modules WHERE kegg_id = %s", or_conditions)
    cached_data <- DBI::dbGetQuery(con, query)
    
    DBI::dbDisconnect(con)
    
    if (nrow(cached_data) > 0) {
      return(cached_data)
    }
    return(NULL)
  }, error = function(e) {
    message("Error loading cached KEGG modules: ", e$message)
    return(NULL)
  })
}

#' Save KEGG modules to SQLite cache
save_kegg_modules_to_cache <- function(module_df) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.null(module_df) || nrow(module_df) == 0) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    for (i in seq_len(nrow(module_df))) {
      tryCatch({
        DBI::dbExecute(con, "
          INSERT OR REPLACE INTO cached_kegg_modules (kegg_id, module_id, module_name)
          VALUES (?, ?, ?)
        ", params = list(module_df$kegg_id[i], module_df$module_id[i], module_df$module_name[i]))
      }, error = function(e) {})
    }
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error saving KEGG modules to cache: ", e$message)
    return(FALSE)
  })
}

#' Mark KEGG ID as having no modules (negative caching)
save_kegg_no_modules <- function(kegg_id) {
  tryCatch({
    if (!file.exists("metabolites.db")) return(FALSE)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_kegg_modules (kegg_id, module_id, module_name)
      VALUES (?, 'NONE', 'No modules found')
    ", params = list(gsub("^cpd:", "", kegg_id)))
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Cached version of fetch_kegg_pathways
fetch_kegg_pathways_cached <- function(kegg_id) {
  if (is.na(kegg_id) || kegg_id == "") {
    return(NULL)
  }
  
  clean_id <- gsub("^cpd:", "", kegg_id)
  
  # Check SQLite cache first
  cached <- load_cached_kegg_pathways(clean_id)
  if (!is.null(cached) && nrow(cached) > 0) {
    # Check if it's a "no pathways" marker
    if (nrow(cached) == 1 && cached$pathway_id[1] == "NONE") {
      return(NULL)
    }
    return(cached[, c("kegg_id", "pathway_id", "pathway_name")])
  }
  
  # Fetch from API
  result <- fetch_kegg_pathways(kegg_id)
  
  # Cache the result
  if (!is.null(result) && nrow(result) > 0) {
    save_kegg_pathways_to_cache(result)
  } else {
    # Negative cache - mark as no pathways found
    save_kegg_no_pathways(kegg_id)
  }
  
  return(result)
}

# Cached version of fetch_metabolite_pathways
fetch_metabolite_pathways_cached <- function(kegg_ids, progress_callback = NULL) {
  # Lazy initialize cache tables on first use
  initialize_metabolomics_cache_tables()
  
  if (length(kegg_ids) == 0 || all(is.na(kegg_ids))) {
    return(data.frame(
      kegg_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
  if (length(valid_ids) == 0) {
    return(data.frame(kegg_id = character(), pathway_id = character(), pathway_name = character(), stringsAsFactors = FALSE))
  }
  
  # Check session cache
  cache_key <- digest::digest(paste(sort(valid_ids), collapse = "|"))
  session_cached <- get_session_cache(paste0("pathways_", cache_key))
  if (!is.null(session_cached)) {
    return(session_cached)
  }
  
  # Check SQLite cache for all IDs at once
  cached_pathways <- load_cached_kegg_pathways(valid_ids)
  
  # Determine which IDs need API calls
  if (!is.null(cached_pathways)) {
    cached_ids <- unique(cached_pathways$kegg_id)
    uncached_ids <- valid_ids[!gsub("^cpd:", "", valid_ids) %in% cached_ids]
  } else {
    uncached_ids <- valid_ids
  }
  
  # Fetch uncached pathways from API
  new_pathways <- list()
  if (length(uncached_ids) > 0) {
    for (i in seq_along(uncached_ids)) {
      if (!is.null(progress_callback) && i %% 5 == 0) {
        progress_callback(i / length(uncached_ids), paste("Fetching pathways", i, "of", length(uncached_ids)))
      }
      
      pathways <- fetch_kegg_pathways_cached(uncached_ids[i])
      if (!is.null(pathways) && nrow(pathways) > 0) {
        new_pathways[[length(new_pathways) + 1]] <- pathways
      }
      
      # Rate limiting for KEGG API
      Sys.sleep(0.1)
    }
  }
  
  # Combine results
  all_pathways <- list()
  if (!is.null(cached_pathways) && nrow(cached_pathways) > 0) {
    # Filter out "NONE" markers
    real_cached <- cached_pathways[cached_pathways$pathway_id != "NONE", ]
    if (nrow(real_cached) > 0) {
      all_pathways[[1]] <- real_cached[, c("kegg_id", "pathway_id", "pathway_name")]
    }
  }
  if (length(new_pathways) > 0) {
    all_pathways <- c(all_pathways, new_pathways)
  }
  
  if (length(all_pathways) == 0) {
    result <- data.frame(kegg_id = character(), pathway_id = character(), pathway_name = character(), stringsAsFactors = FALSE)
  } else {
    result <- do.call(rbind, all_pathways)
  }
  
  # Store in session cache
  set_session_cache(paste0("pathways_", cache_key), result)
  
  return(result)
}

# ============================================================================
# FETCH METABOLITE ASSOCIATIONS FROM HMDB (WITH 30-DAY CACHING)
# ============================================================================

# Helper functions for association caching
load_cached_associations <- function(hmdb_ids, include_failures = FALSE) {
  tryCatch({
    if (!file.exists("metabolites.db") || length(hmdb_ids) == 0) {
      return(NULL)
    }
    
    # Ensure cache tables exist
    initialize_metabolomics_cache_tables()
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    tables <- DBI::dbListTables(con)
    if (!"cached_hmdb_associations" %in% tables) {
      DBI::dbDisconnect(con)
      return(NULL)
    }
    
    # 180-day cache expiration
    expiration_days <- 180
    
    # Escape IDs for SQL
    escaped_ids <- gsub("'", "''", hmdb_ids)
    id_list <- paste0("'", escaped_ids, "'", collapse = ", ")
    
    # Query cached associations that are not expired
    # By default, only return successful lookups (status = 'found')
    # If include_failures = TRUE, also return temporary_failure entries for retry
    status_filter <- if (include_failures) {
      "AND (status = 'found' OR status = 'temporary_failure')"
    } else {
      "AND status = 'found'"
    }
    
    query <- sprintf("
      SELECT * FROM cached_hmdb_associations 
      WHERE hmdb_id IN (%s)
      AND datetime(last_updated) > datetime('now', '-%d days')
      %s
    ", id_list, expiration_days, status_filter)
    
    cached_data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    
    if (nrow(cached_data) > 0) {
      return(cached_data)
    }
    return(NULL)
  }, error = function(e) {
    message("Error loading cached associations: ", e$message)
    return(NULL)
  })
}

save_cached_association <- function(hmdb_id, metabolite_name, diseases, biospecimens, tissues, genes, proteins, pmids, status = "found", error_message = NULL) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.na(hmdb_id) || hmdb_id == "") {
      return(FALSE)
    }
    
    # Ensure cache tables exist
    initialize_metabolomics_cache_tables()
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Ensure error_message is properly set (not NA or list)
    error_msg <- if (is.null(error_message) || length(error_message) == 0) "" else as.character(error_message[1])
    
    # Insert or replace the cached association with status
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_hmdb_associations 
      (hmdb_id, metabolite_name, diseases, biospecimens, tissues, genes, proteins, pmids, status, error_message, last_updated)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
      hmdb_id, metabolite_name, diseases, biospecimens, tissues, genes, proteins, pmids, status, error_msg
    ))
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error saving cached association: ", e$message)
    return(FALSE)
  })
}

# Function to mark association lookups as failed (for retry logic)
mark_association_failure <- function(hmdb_id, metabolite_name = NULL, error_type = "temporary_failure", error_message = NULL) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.na(hmdb_id) || hmdb_id == "") {
      return(FALSE)
    }
    
    initialize_metabolomics_cache_tables()
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    # Insert or replace with failure status
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_hmdb_associations 
      (hmdb_id, metabolite_name, status, error_message, last_updated)
      VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
      hmdb_id, metabolite_name %||% hmdb_id, error_type, error_message
    ))
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error marking association failure: ", e$message)
    return(FALSE)
  })
}

# Function to clear all association cache (useful for troubleshooting)
clear_association_cache <- function(clear_all = FALSE) {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(TRUE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    
    if (clear_all) {
      DBI::dbExecute(con, "DELETE FROM cached_hmdb_associations")
      message("Cleared all cached associations")
    } else {
      # Only clear failed attempts
      DBI::dbExecute(con, "DELETE FROM cached_hmdb_associations WHERE status != 'found'")
      message("Cleared failed association lookup attempts")
    }
    
    DBI::dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    message("Error clearing association cache: ", e$message)
    return(FALSE)
  })
}

# Function to fetch comprehensive associations for metabolites from HMDB
fetch_metabolite_associations_cached <- function(hmdb_ids, metabolite_names = NULL, progress_callback = NULL) {
  if (length(hmdb_ids) == 0 || all(is.na(hmdb_ids) | hmdb_ids == "")) {
    return(data.frame(
      Metabolite = character(),
      HMDB_ID = character(),
      Diseases = character(),
      Biospecimens = character(),
      Tissues = character(),
      Genes = character(),
      Proteins = character(),
      PMIDs = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # If metabolite names not provided, use HMDB IDs
  if (is.null(metabolite_names)) {
    metabolite_names <- hmdb_ids
  }
  
  # Ensure same length
  if (length(metabolite_names) != length(hmdb_ids)) {
    metabolite_names <- rep(metabolite_names, length.out = length(hmdb_ids))
  }
  
  valid_indices <- which(!is.na(hmdb_ids) & hmdb_ids != "")
  
  if (length(valid_indices) == 0) {
    return(data.frame(
      Metabolite = character(),
      HMDB_ID = character(),
      Diseases = character(),
      Biospecimens = character(),
      Tissues = character(),
      Genes = character(),
      Proteins = character(),
      PMIDs = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get valid IDs for cache lookup
  valid_hmdb_ids <- hmdb_ids[valid_indices]
  valid_metab_names <- metabolite_names[valid_indices]
  
  # Check cache first (180-day expiration)
  cached_data <- load_cached_associations(valid_hmdb_ids)
  cached_ids <- if (!is.null(cached_data)) cached_data$hmdb_id else character(0)
  
  message(sprintf("[ASSOCIATIONS] %d/%d IDs found in cache, %d need fetching from HMDB",
                 length(cached_ids), length(valid_hmdb_ids), length(valid_hmdb_ids) - length(cached_ids)))
  
  results <- vector("list", length(valid_indices))
  
  for (i in seq_along(valid_indices)) {
    idx <- valid_indices[i]
    hmdb_id <- hmdb_ids[idx]
    metab_name <- metabolite_names[idx]
    
    # Check if this ID is in cache
    if (hmdb_id %in% cached_ids) {
      cache_row <- cached_data[cached_data$hmdb_id == hmdb_id, ][1, ]
      results[[i]] <- data.frame(
        Metabolite = metab_name,
        HMDB_ID = hmdb_id,
        Diseases = tryCatch(as.character(cache_row$diseases[1]), error = function(e) ""),
        Biospecimens = tryCatch(as.character(cache_row$biospecimens[1]), error = function(e) ""),
        Tissues = tryCatch(as.character(cache_row$tissues[1]), error = function(e) ""),
        Genes = tryCatch(as.character(cache_row$genes[1]), error = function(e) ""),
        Proteins = tryCatch(as.character(cache_row$proteins[1]), error = function(e) ""),
        PMIDs = tryCatch(as.character(cache_row$pmids[1]), error = function(e) ""),
        stringsAsFactors = FALSE
      )
      next
    }
    
    if (!is.null(progress_callback) && i %% 5 == 0) {
      progress_callback(i / length(valid_indices), 
                       paste("Fetching associations", i, "of", length(valid_indices)))
    }
    
    # Fetch from HMDB using hmdbQuery
    associations <- tryCatch({
      if (requireNamespace("hmdbQuery", quietly = TRUE)) {
        # Use HmdbEntry() to query HMDB
        entry <- hmdbQuery::HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/", id = hmdb_id)
        
        if (!is.null(entry)) {
          # Extract diseases using diseases() accessor
          diseases_str <- ""
          pmids_str <- ""
          diseases_data <- tryCatch(hmdbQuery::diseases(entry), error = function(e) NULL)
          
          if (!is.null(diseases_data)) {
            # diseases() can return either a DataFrame or a matrix-like list
            disease_names <- c()
            all_pmids <- c()
            
            # Check if it's a DataFrame (has column names and is a data.frame)
            if ((is.data.frame(diseases_data) || methods::is(diseases_data, "DataFrame")) && 
                nrow(diseases_data) > 0 && "disease" %in% names(diseases_data)) {
              # DataFrame structure with disease column
              disease_names <- diseases_data$disease
              disease_names <- disease_names[!is.na(disease_names)]
              diseases_str <- paste(unique(disease_names), collapse = "; ")
              
              # Extract PMIDs from pmids column if it exists
              if ("pmids" %in% names(diseases_data)) {
                for (j in seq_len(nrow(diseases_data))) {
                  pmids_item <- diseases_data$pmids[[j]]
                  if (!is.null(pmids_item) && length(pmids_item) > 0) {
                    pmids_vec <- unlist(pmids_item)
                    pmids_vec <- pmids_vec[!is.na(pmids_vec) & pmids_vec != ""]
                    all_pmids <- c(all_pmids, pmids_vec)
                  }
                }
              }
            } else if (is.list(diseases_data) && length(diseases_data) > 0) {
              # Matrix-like list structure from HmdbEntry: each element is a disease list
              for (disease_item in diseases_data) {
                if (is.list(disease_item)) {
                  # disease_item structure: list with reference_text and pubmed_id elements
                  # Extract disease name from first reference_text if available
                  if ("reference_text" %in% names(disease_item)) {
                    # This is a reference-style list, need to find disease differently
                    # The disease name should be in the parent level
                    disease_names <- c(disease_names, NA)  # placeholder
                  } else {
                    # Simple disease string
                    disease_names <- c(disease_names, as.character(disease_item[[1]]))
                  }
                  
                  # Extract pubmed_ids from nested structure
                  if ("pubmed_id" %in% names(disease_item)) {
                    pmid <- disease_item$pubmed_id
                    if (!is.null(pmid) && pmid != "" && !is.na(pmid)) {
                      all_pmids <- c(all_pmids, pmid)
                    }
                  }
                } else if (is.character(disease_item)) {
                  disease_names <- c(disease_names, disease_item)
                }
              }
            }
            
            # Clean up disease names
            if (length(disease_names) > 0) {
              disease_names <- disease_names[!is.na(disease_names)]
              diseases_str <- paste(unique(disease_names), collapse = "; ")
            }
            
            # Format PMIDs as links
            if (length(all_pmids) > 0) {
              all_pmids <- all_pmids[!is.na(all_pmids) & all_pmids != ""]
              if (length(all_pmids) > 0) {
                pmid_links <- sapply(unique(all_pmids), function(pmid) {
                  paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid, '" target="_blank">', pmid, '</a>')
                })
                pmids_str <- paste(pmid_links, collapse = "; ")
              }
            }
          }
          
          # Extract biospecimens using biospecimens() accessor
          biospec_str <- ""
          biospec_data <- tryCatch(hmdbQuery::biospecimens(entry), error = function(e) NULL)
          if (!is.null(biospec_data) && length(biospec_data) > 0) {
            biospec_data <- biospec_data[!is.na(biospec_data)]
            biospec_str <- paste(unique(biospec_data), collapse = "; ")
          }
          
          # Extract tissues using tissues() accessor
          tissue_str <- ""
          tissue_data <- tryCatch(hmdbQuery::tissues(entry), error = function(e) NULL)
          if (!is.null(tissue_data) && length(tissue_data) > 0) {
            tissue_data <- tissue_data[!is.na(tissue_data)]
            tissue_str <- paste(unique(tissue_data), collapse = "; ")
          }
          
          # Extract genes and proteins using store() accessor
          genes_str <- ""
          proteins_str <- ""
          store_data <- tryCatch(hmdbQuery::store(entry), error = function(e) NULL)
          if (!is.null(store_data) && !is.null(store_data$protein_assoc)) {
            # protein_assoc is a matrix-like structure with rows "name" and "gene_name"
            protein_assoc <- store_data$protein_assoc
            
            # Extract gene names
            if ("gene_name" %in% rownames(protein_assoc)) {
              gene_names <- unlist(protein_assoc["gene_name", ])
              gene_names <- gene_names[!is.na(gene_names) & gene_names != ""]
              genes_str <- paste(unique(gene_names), collapse = "; ")
            }
            
            # Extract protein names
            if ("name" %in% rownames(protein_assoc)) {
              protein_names <- unlist(protein_assoc["name", ])
              protein_names <- protein_names[!is.na(protein_names) & protein_names != ""]
              proteins_str <- paste(unique(protein_names), collapse = "; ")
            }
          }
          
          list(
            diseases = diseases_str,
            biospecimens = biospec_str,
            tissues = tissue_str,
            genes = genes_str,
            proteins = proteins_str,
            pmids = pmids_str
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
    }, error = function(e) {
      message("Error fetching associations for ", hmdb_id, ": ", e$message)
      # Mark as temporary failure so we can retry later
      mark_association_failure(hmdb_id, metab_name, "temporary_failure", e$message)
      NULL
    })
    # Build result row
    results[[i]] <- data.frame(
      Metabolite = metab_name,
      HMDB_ID = hmdb_id,
      Diseases = if (!is.null(associations)) associations$diseases else "",
      Biospecimens = if (!is.null(associations)) associations$biospecimens else "",
      Tissues = if (!is.null(associations)) associations$tissues else "",
      Genes = if (!is.null(associations)) associations$genes else "",
      Proteins = if (!is.null(associations)) associations$proteins else "",
      PMIDs = if (!is.null(associations)) associations$pmids else "",
      stringsAsFactors = FALSE
    )
    
    # Save to cache for future use (only if we fetched from API, not from cache)
    if (!is.null(associations)) {
      save_cached_association(
        hmdb_id = hmdb_id,
        metabolite_name = metab_name,
        diseases = associations$diseases,
        biospecimens = associations$biospecimens,
        tissues = associations$tissues,
        genes = associations$genes,
        proteins = associations$proteins,
        pmids = associations$pmids,
        status = "found"
      )
    }
    
    # Rate limiting
    Sys.sleep(0.15)
  }
  
  result_df <- do.call(rbind, results)
  return(result_df)
}

# ============================================================================
# CACHED HMDB CLASSIFICATION FUNCTIONS
# ============================================================================

# Load cached HMDB classification
load_cached_hmdb_classification <- function(hmdb_id, include_failures = FALSE) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.na(hmdb_id) || hmdb_id == "") {
      return(NULL)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    tables <- DBI::dbListTables(con)
    if (!"cached_hmdb_classifications" %in% tables) {
      return(NULL)
    }
    
    # Build query with status filtering (skip temporary_failure unless include_failures=TRUE)
    status_filter <- if (include_failures) "" else "AND (status = 'found' OR status IS NULL)"
    
    query <- sprintf(
      "SELECT * FROM cached_hmdb_classifications WHERE (hmdb_id = ?) AND datetime(last_updated) > datetime('now', '-180 days') %s",
      status_filter
    )
    
    cached <- DBI::dbGetQuery(con, query, params = list(hmdb_id))
    
    if (nrow(cached) > 0) {
      return(list(
        kingdom = cached$kingdom[1],
        super_class = cached$super_class[1],
        class = cached$class[1],
        sub_class = cached$sub_class[1],
        direct_parent = cached$direct_parent[1],
        description = cached$description[1],
        status = cached$status[1],
        error_message = cached$error_message[1]
      ))
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# Save HMDB classification to cache with status tracking
save_hmdb_classification_to_cache <- function(hmdb_id, classification, status = "found", error_message = NULL) {
  tryCatch({
    if (!file.exists("metabolites.db") || is.null(classification)) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Handle NULL error_message
    error_msg <- if (is.null(error_message)) "" else as.character(error_message)[1]
    
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_hmdb_classifications 
      (hmdb_id, kingdom, super_class, class, sub_class, direct_parent, description, status, error_message)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      hmdb_id,
      tryCatch(as.character(classification$kingdom), error = function(e) "Unknown"),
      tryCatch(as.character(classification$super_class), error = function(e) "Unknown"),
      tryCatch(as.character(classification$class), error = function(e) "Unknown"),
      tryCatch(as.character(classification$sub_class), error = function(e) "Unknown"),
      tryCatch(as.character(classification$direct_parent), error = function(e) "Unknown"),
      tryCatch(as.character(classification$description), error = function(e) NA),
      status,
      error_msg
    ))
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Mark HMDB classification lookup as failed (for retry logic)
mark_hmdb_classification_failure <- function(hmdb_id, error_message) {
  tryCatch({
    if (!file.exists("metabolites.db")) {
      return(FALSE)
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    tables <- DBI::dbListTables(con)
    if (!"cached_hmdb_classifications" %in% tables) {
      return(FALSE)
    }
    
    error_msg <- if (is.null(error_message)) "" else as.character(error_message)[1]
    
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO cached_hmdb_classifications 
      (hmdb_id, kingdom, super_class, class, sub_class, direct_parent, description, status, error_message)
      VALUES (?, 'Unknown', 'Unknown', 'Unknown', 'Unknown', 'Unknown', NULL, 'temporary_failure', ?)
    ", params = list(hmdb_id, error_msg))
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Cached version of fetch_hmdb_classification
fetch_hmdb_classification_cached <- function(hmdb_id) {
  # Lazy initialize cache tables on first use
  initialize_metabolomics_cache_tables()
  
  if (is.na(hmdb_id) || hmdb_id == "" || !grepl("^HMDB", hmdb_id)) {
    return(NULL)
  }
  
  # Check cache first (skip temporary failures)
  cached <- load_cached_hmdb_classification(hmdb_id, include_failures = FALSE)
  if (!is.null(cached) && (is.na(cached$status) || cached$status != "temporary_failure")) {
    return(cached[names(cached) %in% c("kingdom", "super_class", "class", "sub_class", "direct_parent", "description")])
  }
  
  # Fetch from API
  result <- tryCatch({
    fetch_hmdb_classification(hmdb_id)
  }, error = function(e) {
    NULL
  })
  
  # Cache the result
  if (!is.null(result)) {
    save_hmdb_classification_to_cache(hmdb_id, result, status = "found", error_message = NULL)
  } else {
    # Record failure for retry logic
    mark_hmdb_classification_failure(hmdb_id, "API call failed or no data returned")
  }
  
  return(result)
}

# ============================================================================
# CACHED SUMMARY FUNCTIONS
# ============================================================================

# Cached version of summarize_metabolite_classification
summarize_metabolite_classification_cached <- function(classification_df, level = "super_class") {
  if (is.null(classification_df) || nrow(classification_df) == 0) {
    return(NULL)
  }
  
  # Create cache key from data hash and level
  cache_key <- paste0("summary_", level, "_", digest::digest(classification_df))
  
  # Check session cache
  cached <- get_session_cache(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }
  
  # Compute summary
  result <- summarize_metabolite_classification(classification_df, level)
  
  # Cache result
  if (!is.null(result)) {
    set_session_cache(cache_key, result)
  }
  
  return(result)
}

# Cached version of summarize_pathway_enrichment
summarize_pathway_enrichment_cached <- function(pathway_df) {
  if (is.null(pathway_df) || nrow(pathway_df) == 0) {
    return(NULL)
  }
  
  # Create cache key
  cache_key <- paste0("pathway_summary_", digest::digest(pathway_df))
  
  # Check session cache
  cached <- get_session_cache(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }
  
  # Compute summary
  result <- summarize_pathway_enrichment(pathway_df)
  
  # Cache result
  if (!is.null(result)) {
    set_session_cache(cache_key, result)
  }
  
  return(result)
}

# ============================================================================
# CACHE MANAGEMENT UTILITIES
# ============================================================================

# Get metabolomics cache statistics
get_metabolomics_cache_stats <- function() {
  stats <- list(
    classification_cache = 0,
    pathway_cache = 0,
    hmdb_cache = 0,
    session_cache_items = length(ls(envir = .metabolomics_session_cache))
  )
  
  tryCatch({
    if (file.exists("metabolites.db")) {
      con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
      
      tables <- DBI::dbListTables(con)
      
      if ("cached_classifications" %in% tables) {
        stats$classification_cache <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM cached_classifications")$n
      }
      if ("cached_kegg_pathways" %in% tables) {
        stats$pathway_cache <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT kegg_id) as n FROM cached_kegg_pathways")$n
      }
      if ("cached_hmdb_classifications" %in% tables) {
        stats$hmdb_cache <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM cached_hmdb_classifications")$n
      }
      
      DBI::dbDisconnect(con)
    }
  }, error = function(e) {})
  
  return(stats)
}

# Clear all metabolomics caches
clear_metabolomics_caches <- function(sqlite = TRUE, session = TRUE) {
  if (session) {
    clear_session_cache()
    message("✓ Session cache cleared")
  }
  
  if (sqlite && file.exists("metabolites.db")) {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), "metabolites.db")
      DBI::dbExecute(con, "DELETE FROM cached_classifications")
      DBI::dbExecute(con, "DELETE FROM cached_kegg_pathways")
      DBI::dbExecute(con, "DELETE FROM cached_hmdb_classifications")
      DBI::dbDisconnect(con)
      message("✓ SQLite metabolomics caches cleared")
    }, error = function(e) {
      message("Error clearing SQLite caches: ", e$message)
    })
  }
}

# ============================================================================
# END METABOLOMICS CACHING SYSTEM
# ============================================================================
# Note: Cache tables are initialized lazily on first use, not at app startup

# ============================================================================
# METABOLOMICS CLASSIFICATION AND ANALYSIS FUNCTIONS
# ============================================================================

# Chemical taxonomy levels (ClassyFire hierarchy)
METABOLITE_TAXONOMY_LEVELS <- c("kingdom", "super_class", "class", "sub_class", "direct_parent")

# Default metabolite classification database (built-in for offline use)
# This provides chemical taxonomy based on common metabolite categories
get_default_metabolite_taxonomy <- function() {
  list(
    # Lipids and lipid-like molecules
    "Lipids and lipid-like molecules" = list(
      kingdom = "Organic compounds",
      super_class = "Lipids and lipid-like molecules",
      classes = c("Fatty Acyls", "Glycerolipids", "Glycerophospholipids", "Sphingolipids", 
                  "Sterol Lipids", "Prenol lipids", "Saccharolipids", "Polyketides")
    ),
    # Organic acids
    "Organic acids and derivatives" = list(
      kingdom = "Organic compounds",
      super_class = "Organic acids and derivatives",
      classes = c("Carboxylic acids and derivatives", "Hydroxy acids and derivatives",
                  "Keto acids and derivatives", "Organic sulfuric acids and derivatives")
    ),
    # Amino acids
    "Amino acids" = list(
      kingdom = "Organic compounds",
      super_class = "Organic acids and derivatives",
      class = "Carboxylic acids and derivatives",
      sub_class = "Amino acids, peptides, and analogues"
    ),
    # Nucleosides
    "Nucleosides, nucleotides, and analogues" = list(
      kingdom = "Organic compounds",
      super_class = "Nucleosides, nucleotides, and analogues",
      classes = c("Purine nucleosides", "Pyrimidine nucleosides", "Purine nucleotides", "Pyrimidine nucleotides")
    ),
    # Carbohydrates
    "Organic oxygen compounds" = list(
      kingdom = "Organic compounds",
      super_class = "Organic oxygen compounds",
      classes = c("Organooxygen compounds", "Carbohydrates and carbohydrate conjugates")
    ),
    # Benzenoids
    "Benzenoids" = list(
      kingdom = "Organic compounds",
      super_class = "Benzenoids",
      classes = c("Benzene and substituted derivatives", "Phenols", "Naphthalenes")
    ),
    # Organoheterocyclics
    "Organoheterocyclic compounds" = list(
      kingdom = "Organic compounds",
      super_class = "Organoheterocyclic compounds",
      classes = c("Indoles and derivatives", "Pyridines and derivatives", "Imidazoles", "Pyrroles")
    )
  )
}

# Color palette for metabolite super classes
get_metabolite_superclass_colors <- function() {
  c(
    "Lipids and lipid-like molecules" = "#E74C3C",
    "Organic acids and derivatives" = "#3498DB",
    "Organoheterocyclic compounds" = "#2ECC71",
    "Benzenoids" = "#9B59B6",
    "Organic nitrogen compounds" = "#F39C12",
    "Organic oxygen compounds" = "#1ABC9C",
    "Phenylpropanoids and polyketides" = "#E67E22",
    "Nucleosides, nucleotides, and analogues" = "#34495E",
    "Alkaloids and derivatives" = "#95A5A6",
    "Lignans, neolignans and related compounds" = "#D35400",
    "Organohalogen compounds" = "#7F8C8D",
    "Organosulfur compounds" = "#C0392B",
    "Homogeneous non-metal compounds" = "#16A085",
    "Hydrocarbon derivatives" = "#8E44AD",
    "Unknown" = "#BDC3C7"
  )
}

# Function to classify metabolites based on name patterns
classify_metabolite_by_name <- function(metabolite_name) {
  if (is.na(metabolite_name) || metabolite_name == "") {
    return(list(
      kingdom = "Unknown",
      super_class = "Unknown",
      class = "Unknown",
      sub_class = "Unknown",
      direct_parent = "Unknown"
    ))
  }
  
  name_lower <- tolower(metabolite_name)
  
  # Pattern-based classification
  classification <- list(
    kingdom = "Organic compounds",
    super_class = "Unknown",
    class = "Unknown",
    sub_class = "Unknown",
    direct_parent = "Unknown"
  )
  
  # Lipids
  if (grepl("phosphatidyl|phospho|pc\\(|pe\\(|ps\\(|pi\\(|pg\\(|pa\\(|cardiolipin|lyso", name_lower)) {
    classification$super_class <- "Lipids and lipid-like molecules"
    classification$class <- "Glycerophospholipids"
    if (grepl("lyso", name_lower)) classification$sub_class <- "Lysophospholipids"
    else if (grepl("pc\\(|phosphatidylcholine", name_lower)) classification$sub_class <- "Phosphatidylcholines"
    else if (grepl("pe\\(|phosphatidylethanolamine", name_lower)) classification$sub_class <- "Phosphatidylethanolamines"
    else if (grepl("ps\\(|phosphatidylserine", name_lower)) classification$sub_class <- "Phosphatidylserines"
  } else if (grepl("sphingo|ceramide|cer\\(|sm\\(|ganglioside", name_lower)) {
    classification$super_class <- "Lipids and lipid-like molecules"
    classification$class <- "Sphingolipids"
    if (grepl("ceramide|cer\\(", name_lower)) classification$sub_class <- "Ceramides"
    else if (grepl("sphingomyelin|sm\\(", name_lower)) classification$sub_class <- "Sphingomyelins"
  } else if (grepl("sterol|cholesterol|bile|cholic", name_lower)) {
    classification$super_class <- "Lipids and lipid-like molecules"
    classification$class <- "Sterol Lipids"
    if (grepl("cholesterol", name_lower)) classification$sub_class <- "Cholesterol and derivatives"
    else if (grepl("bile|cholic", name_lower)) classification$sub_class <- "Bile acids and derivatives"
  } else if (grepl("fatty acid|carnitine|acylcarnitine|coa|acyl-coa", name_lower)) {
    classification$super_class <- "Lipids and lipid-like molecules"
    classification$class <- "Fatty Acyls"
    if (grepl("carnitine", name_lower)) classification$sub_class <- "Fatty acid esters"
    else classification$sub_class <- "Fatty acids and conjugates"
  } else if (grepl("triglyceride|tg\\(|dg\\(|mg\\(|diacylglycerol|monoacylglycerol", name_lower)) {
    classification$super_class <- "Lipids and lipid-like molecules"
    classification$class <- "Glycerolipids"
    if (grepl("triglyceride|tg\\(", name_lower)) classification$sub_class <- "Triradylglycerols"
    else if (grepl("dg\\(|diacylglycerol", name_lower)) classification$sub_class <- "Diradylglycerols"
    else classification$sub_class <- "Monoradylglycerols"
  }
  # Amino acids
  else if (grepl("alanine|arginine|asparagine|aspartate|aspartic|cysteine|glutamate|glutamic|glutamine|glycine|histidine|isoleucine|leucine|lysine|methionine|phenylalanine|proline|serine|threonine|tryptophan|tyrosine|valine", name_lower)) {
    classification$super_class <- "Organic acids and derivatives"
    classification$class <- "Carboxylic acids and derivatives"
    classification$sub_class <- "Amino acids, peptides, and analogues"
    classification$direct_parent <- "Alpha amino acids"
  }
  # Nucleosides and nucleotides
  else if (grepl("adenosine|guanosine|cytidine|uridine|thymidine|adenine|guanine|cytosine|uracil|thymine|atp|adp|amp|gtp|gdp|gmp|ctp|cdp|cmp|utp|udp|ump|nad|nadh|nadp|nadph|fad|fadh", name_lower)) {
    classification$super_class <- "Nucleosides, nucleotides, and analogues"
    if (grepl("adenosine|guanosine|adenine|guanine", name_lower)) classification$class <- "Purine nucleosides"
    else if (grepl("cytidine|uridine|thymidine|cytosine|uracil|thymine", name_lower)) classification$class <- "Pyrimidine nucleosides"
    if (grepl("tp$|diphosphate|triphosphate", name_lower)) classification$sub_class <- "Nucleoside phosphates"
  }
  # Carbohydrates
  else if (grepl("glucose|fructose|galactose|mannose|ribose|sucrose|maltose|lactose|glycogen|starch|hexose|pentose|sugar", name_lower)) {
    classification$super_class <- "Organic oxygen compounds"
    classification$class <- "Organooxygen compounds"
    classification$sub_class <- "Carbohydrates and carbohydrate conjugates"
    if (grepl("glucose|fructose|galactose|mannose", name_lower)) classification$direct_parent <- "Hexoses"
    else if (grepl("ribose", name_lower)) classification$direct_parent <- "Pentoses"
  }
  # Organic acids
  else if (grepl("citrate|citric|succinate|succinic|fumarate|fumaric|malate|malic|lactate|lactic|pyruvate|pyruvic|oxaloacetate|alpha-ketoglutarate|acetate|acetic|propionate|butyrate", name_lower)) {
    classification$super_class <- "Organic acids and derivatives"
    classification$class <- "Carboxylic acids and derivatives"
    classification$sub_class <- "Dicarboxylic acids and derivatives"
  }
  # Benzenoids / Phenolics
  else if (grepl("phenol|benzoic|benzyl|catechol|quinone|flavonoid|polyphenol|anthocyanin", name_lower)) {
    classification$super_class <- "Benzenoids"
    classification$class <- "Benzene and substituted derivatives"
    if (grepl("phenol", name_lower)) classification$sub_class <- "Phenols"
    else if (grepl("flavonoid", name_lower)) classification$sub_class <- "Flavonoids"
  }
  # Indoles and related
  else if (grepl("indole|tryptamine|serotonin|melatonin", name_lower)) {
    classification$super_class <- "Organoheterocyclic compounds"
    classification$class <- "Indoles and derivatives"
  }
  # Vitamins (various classes)
  else if (grepl("vitamin|retinol|retinal|retinoic|tocopherol|ascorbic|thiamine|riboflavin|niacin|pantothenic|pyridoxine|biotin|folate|folic|cobalamin", name_lower)) {
    if (grepl("retinol|retinal|retinoic|vitamin a", name_lower)) {
      classification$super_class <- "Lipids and lipid-like molecules"
      classification$class <- "Prenol lipids"
      classification$sub_class <- "Retinoids"
    } else if (grepl("tocopherol|vitamin e", name_lower)) {
      classification$super_class <- "Lipids and lipid-like molecules"
      classification$class <- "Prenol lipids"
      classification$sub_class <- "Vitamin E"
    } else if (grepl("ascorbic|vitamin c", name_lower)) {
      classification$super_class <- "Organic oxygen compounds"
      classification$class <- "Organooxygen compounds"
    } else {
      classification$super_class <- "Organoheterocyclic compounds"
      classification$class <- "Vitamins and cofactors"
    }
  }
  
  return(classification)
}

# Function to fetch classification from HMDB (if available)
fetch_hmdb_classification <- function(hmdb_id) {
  if (is.na(hmdb_id) || hmdb_id == "" || !grepl("^HMDB", hmdb_id)) {
    return(NULL)
  }
  
  tryCatch({
    # Try to use hmdbQuery package if available
    if (requireNamespace("hmdbQuery", quietly = TRUE)) {
      # Query HMDB for metabolite info using HmdbEntry
      entry <- hmdbQuery::HmdbEntry(prefix = "http://www.hmdb.ca/metabolites/", id = hmdb_id)
      
      if (!is.null(entry)) {
        # Extract taxonomy information from the store slot
        # The taxonomy data is stored in entry@store$taxonomy as a list
        store <- entry@store
        
        if (!is.null(store) && !is.null(store$taxonomy)) {
          taxonomy <- store$taxonomy
          
          return(list(
            kingdom = if (!is.null(taxonomy$kingdom)) taxonomy$kingdom else "Unknown",
            super_class = if (!is.null(taxonomy$super_class)) taxonomy$super_class else "Unknown",
            class = if (!is.null(taxonomy$class)) taxonomy$class else "Unknown",
            sub_class = if (!is.null(taxonomy$sub_class)) taxonomy$sub_class else "Unknown",
            direct_parent = if (!is.null(taxonomy$direct_parent)) taxonomy$direct_parent else "Unknown",
            description = if (!is.null(store$description)) store$description else NA,
            pathways = if (!is.null(store$biological_properties$pathways)) store$biological_properties$pathways else list()
          ))
        }
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Error fetching HMDB classification for ", hmdb_id, ": ", e$message)
    return(NULL)
  })
}

# Main function to classify metabolites
classify_metabolites <- function(metabolite_names, hmdb_ids = NULL, use_online = FALSE, progress_callback = NULL) {
  n_metabolites <- length(metabolite_names)
  
  if (n_metabolites == 0) {
    return(data.frame(
      metabolite_name = character(),
      kingdom = character(),
      super_class = character(),
      class = character(),
      sub_class = character(),
      direct_parent = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Use batched HMDB classification if IDs are available
  if (!is.null(hmdb_ids) && length(hmdb_ids) > 0 && any(!is.na(hmdb_ids) & hmdb_ids != "")) {
    classifications_batch <- classify_metabolites_batch_hmdb(metabolite_names, hmdb_ids, progress_callback)
    return(classifications_batch)
  }
  
  # Fallback to sequential classification for metabolites without HMDB IDs
  results <- vector("list", n_metabolites)
  
  for (i in seq_len(n_metabolites)) {
    if (!is.null(progress_callback) && i %% 10 == 0) {
      progress_callback(i / n_metabolites, paste("Classifying metabolite", i, "of", n_metabolites))
    }
    
    metabolite_name <- metabolite_names[i]
    hmdb_id <- if (!is.null(hmdb_ids) && length(hmdb_ids) >= i) hmdb_ids[i] else NA
    
    # Use HMDB lookup exclusively if ID available
    classification <- NULL
    if (!is.na(hmdb_id) && hmdb_id != "") {
      classification <- fetch_hmdb_classification_cached(hmdb_id)
    }
    
    # If HMDB lookup failed or no ID, create empty/unknown record
    if (is.null(classification)) {
      classification <- list(
        kingdom = "Unknown",
        super_class = "Unknown",
        class = "Unknown",
        sub_class = "Unknown",
        direct_parent = "Unknown"
      )
    }
    
    results[[i]] <- data.frame(
      metabolite_name = metabolite_name,
      hmdb_id = ifelse(is.na(hmdb_id), "", hmdb_id),
      kingdom = classification$kingdom,
      super_class = classification$super_class,
      class = classification$class,
      sub_class = classification$sub_class,
      direct_parent = classification$direct_parent,
      stringsAsFactors = FALSE
    )
  }
  
  result_df <- do.call(rbind, results)
  return(result_df)
}

# Batch classification using HMDB IDs (optimized for multiple metabolites)
# Fetches HMDB entries sequentially but efficiently with caching
classify_metabolites_batch_hmdb <- function(metabolite_names, hmdb_ids, progress_callback = NULL) {
  n_metabolites <- length(metabolite_names)
  
  # Validate inputs
  if (n_metabolites == 0) {
    return(data.frame(
      metabolite_name = character(),
      hmdb_id = character(),
      kingdom = character(),
      super_class = character(),
      class = character(),
      sub_class = character(),
      direct_parent = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Filter to valid HMDB IDs
  valid_indices <- which(!is.na(hmdb_ids) & hmdb_ids != "" & grepl("^HMDB", hmdb_ids))
  
  if (length(valid_indices) == 0) {
    # No valid HMDB IDs - return unknown for all
    return(data.frame(
      metabolite_name = metabolite_names,
      hmdb_id = ifelse(!is.na(hmdb_ids), hmdb_ids, ""),
      kingdom = rep("Unknown", n_metabolites),
      super_class = rep("Unknown", n_metabolites),
      class = rep("Unknown", n_metabolites),
      sub_class = rep("Unknown", n_metabolites),
      direct_parent = rep("Unknown", n_metabolites),
      stringsAsFactors = FALSE
    ))
  }
  
  # Initialize result structure
  results_df <- data.frame(
    metabolite_name = metabolite_names,
    hmdb_id = ifelse(!is.na(hmdb_ids), hmdb_ids, ""),
    kingdom = rep("Unknown", n_metabolites),
    super_class = rep("Unknown", n_metabolites),
    class = rep("Unknown", n_metabolites),
    sub_class = rep("Unknown", n_metabolites),
    direct_parent = rep("Unknown", n_metabolites),
    stringsAsFactors = FALSE
  )
  
  # Fetch HMDB entries sequentially (hmdbQuery doesn't support batch queries)
  tryCatch({
    if (requireNamespace("hmdbQuery", quietly = TRUE)) {
      message(sprintf("[DEBUG-CLASSIFY-BATCH] Fetching classifications for %d metabolites from HMDB (sequential)", length(valid_indices)))
      
      cache_hits <- 0
      cache_misses <- 0
      
      for (idx in seq_along(valid_indices)) {
        result_idx <- valid_indices[idx]
        hmdb_id <- hmdb_ids[result_idx]
        
        if (!is.null(progress_callback) && idx %% 5 == 0) {
          progress_callback(idx / length(valid_indices), 
                          paste("Classifying metabolite", idx, "of", length(valid_indices)))
        }
        
        # Check cache first
        cached <- load_cached_hmdb_classification(hmdb_id, include_failures = FALSE)
        if (!is.null(cached) && (is.na(cached$status) || cached$status != "temporary_failure")) {
          # Use cached result
          results_df$kingdom[result_idx] <- cached$kingdom %||% "Unknown"
          results_df$super_class[result_idx] <- cached$super_class %||% "Unknown"
          results_df$class[result_idx] <- cached$class %||% "Unknown"
          results_df$sub_class[result_idx] <- cached$sub_class %||% "Unknown"
          results_df$direct_parent[result_idx] <- cached$direct_parent %||% "Unknown"
          
          if (idx <= 3) {
            message(sprintf("[DEBUG-CLASSIFY-BATCH] Entry %d (%s) - CACHE HIT - Kingdom: %s, SuperClass: %s", 
                           idx, hmdb_id, cached$kingdom %||% "NULL", cached$super_class %||% "NULL"))
          }
          cache_hits <- cache_hits + 1
          next
        }
        
        cache_misses <- cache_misses + 1
        
        # Fetch single entry from API
        entry <- tryCatch({
          hmdbQuery::HmdbEntry(
            prefix = "http://www.hmdb.ca/metabolites/",
            id = hmdb_id
          )
        }, error = function(e) {
          if (idx <= 3) message("[DEBUG-CLASSIFY-BATCH] Error fetching ", hmdb_id, ": ", e$message)
          NULL
        })
        
        if (!is.null(entry) && !is.null(entry@store)) {
          store <- entry@store
          
          # Extract taxonomy if available
          if (!is.null(store$taxonomy)) {
            taxonomy <- store$taxonomy
            results_df$kingdom[result_idx] <- taxonomy$kingdom %||% "Unknown"
            results_df$super_class[result_idx] <- taxonomy$super_class %||% "Unknown"
            results_df$class[result_idx] <- taxonomy$class %||% "Unknown"
            results_df$sub_class[result_idx] <- taxonomy$sub_class %||% "Unknown"
            results_df$direct_parent[result_idx] <- taxonomy$direct_parent %||% "Unknown"
            
            # Cache the successful result
            classification_result <- list(
              kingdom = taxonomy$kingdom %||% "Unknown",
              super_class = taxonomy$super_class %||% "Unknown",
              class = taxonomy$class %||% "Unknown",
              sub_class = taxonomy$sub_class %||% "Unknown",
              direct_parent = taxonomy$direct_parent %||% "Unknown",
              description = store$description %||% NA
            )
            save_hmdb_classification_to_cache(hmdb_id, classification_result, status = "found")
            
            if (idx <= 3) {
              message(sprintf("[DEBUG-CLASSIFY-BATCH] Entry %d (%s) - API FETCH - Kingdom: %s, SuperClass: %s", 
                             idx, hmdb_id, taxonomy$kingdom %||% "NULL", taxonomy$super_class %||% "NULL"))
            }
          } else {
            if (idx <= 3) message("[DEBUG-CLASSIFY-BATCH] Entry ", idx, " (", hmdb_id, ") - No taxonomy in store")
            mark_hmdb_classification_failure(hmdb_id, "No taxonomy data in HMDB entry")
          }
        } else {
          if (idx <= 3) message("[DEBUG-CLASSIFY-BATCH] Entry ", idx, " (", hmdb_id, ") - Entry or store is NULL")
          mark_hmdb_classification_failure(hmdb_id, "HMDB entry or store is NULL")
        }
      }
      
      message(sprintf("[DEBUG-CLASSIFY-BATCH] Cache results - Hits: %d, Misses: %d", cache_hits, cache_misses))
    }
  }, error = function(e) {
    message("[DEBUG-CLASSIFY-BATCH] Error: ", e$message)
  })
  
  message("[DEBUG-CLASSIFY-BATCH] Returning dataframe with columns: ", paste(names(results_df), collapse=", "))
  if (nrow(results_df) > 0) {
    message("[DEBUG-CLASSIFY-BATCH] Sample row 1: ", paste(sapply(results_df[1,], as.character), collapse=" | "))
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(1.0, "Classification complete")
  }
  
  return(results_df)
}

# Function to summarize metabolite classification
summarize_metabolite_classification <- function(classification_df, level = "super_class") {
  if (is.null(classification_df) || nrow(classification_df) == 0) {
    return(NULL)
  }
  
  # Ensure the level column exists
  if (!level %in% names(classification_df)) {
    message("Level '", level, "' not found in classification data")
    return(NULL)
  }
  
  # Count metabolites by classification level
  summary_df <- classification_df %>%
    dplyr::group_by(!!sym(level)) %>%
    dplyr::summarise(
      count = n(),
      percentage = round(n() / nrow(classification_df) * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(count))
  
  # Add color assignments
  colors <- get_metabolite_superclass_colors()
  summary_df$color <- sapply(summary_df[[level]], function(x) {
    if (x %in% names(colors)) colors[x] else "#BDC3C7"
  })
  
  return(summary_df)
}

# Function to get pathway information from KEGG
fetch_kegg_pathways <- function(kegg_id) {
  if (is.na(kegg_id) || kegg_id == "") {
    return(NULL)
  }
  
  tryCatch({
    if (requireNamespace("KEGGREST", quietly = TRUE)) {
      # Clean KEGG ID (remove cpd: prefix if present)
      clean_id <- gsub("^cpd:", "", kegg_id)
      
      # Get compound info including pathways
      compound_info <- KEGGREST::keggGet(paste0("cpd:", clean_id))
      
      if (length(compound_info) > 0 && !is.null(compound_info[[1]]$PATHWAY)) {
        pathways <- compound_info[[1]]$PATHWAY
        return(data.frame(
          kegg_id = clean_id,
          pathway_id = names(pathways),
          pathway_name = unname(pathways),
          stringsAsFactors = FALSE
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Error fetching KEGG pathways for ", kegg_id, ": ", e$message)
    return(NULL)
  })
}

#' Fetch KEGG modules for a single compound
#' @param kegg_id Character, KEGG compound ID (e.g., "C00001")
#' @return Data frame with columns: kegg_id, module_id, module_name
fetch_kegg_modules <- function(kegg_id) {
  if (is.na(kegg_id) || kegg_id == "") {
    return(NULL)
  }
  
  tryCatch({
    # Clean KEGG ID (remove cpd: prefix if present)
    clean_id <- gsub("^cpd:", "", kegg_id)
    
    # Use KEGG REST API /link/module/{compound_id} to get modules linked to this compound
    # This is the proper REST API call according to KEGG API manual
    url <- paste0("https://rest.kegg.jp/link/module/", clean_id)
    
    response <- tryCatch({
      httr::GET(url, httr::timeout(10))
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(response) && httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      # Parse tab-separated response
      # Response format is: cpd:{compound_id}\tmd:{module_id}
      if (nchar(trimws(content)) > 0) {
        lines <- strsplit(content, "\n")[[1]]
        lines <- lines[nchar(trimws(lines)) > 0]
        
        if (length(lines) > 0) {
          # Parse each line to extract module_id
          # Response format is: cpd:{compound_id}\tmd:{module_id}
          # So we need x[2] and strip the "md:" prefix
          module_ids <- sapply(strsplit(lines, "\t"), function(x) {
            if (length(x) >= 2) gsub("^md:", "", x[2]) else NA
          })
          module_ids <- module_ids[!is.na(module_ids)]
          
          if (length(module_ids) == 0) {
            return(NULL)
          }
          
          # Get module names from KEGG REST API directly
          unique_modules <- unique(module_ids)
          module_names <- character(length(unique_modules))
          names(module_names) <- unique_modules
          
          # Initialize all names with module IDs as fallback
          for (m in unique_modules) {
            module_names[m] <- m
          }
          
          # Try to fetch module names from KEGG REST API in batch
          tryCatch({
            # Create URL for batch API call
            module_query <- paste(unique_modules, collapse = "+")
            url <- paste0("https://rest.kegg.jp/get/", module_query)
            
            response <- tryCatch({
              httr::GET(url, httr::timeout(15))
            }, error = function(e) NULL)
            
            if (!is.null(response) && httr::status_code(response) == 200) {
              content <- httr::content(response, as = "text", encoding = "UTF-8")
              lines <- strsplit(content, "\n")[[1]]
              
              # Parse KEGG response line by line
              current_module <- NULL
              for (line in lines) {
                if (startsWith(trimws(line), "ENTRY")) {
                  # Extract module ID from "ENTRY       M00001            Module"
                  parts <- strsplit(trimws(line), "\\s+")[[1]]
                  if (length(parts) >= 2) {
                    current_module <- parts[2]
                  }
                } else if (startsWith(trimws(line), "NAME") && !is.null(current_module)) {
                  # Extract name from "NAME        Module description line 1"
                  # NAME is followed by whitespace(s), then the description
                  name_text <- sub("^NAME\\s+", "", line)
                  name_text <- trimws(name_text)
                  if (nchar(name_text) > 0 && current_module %in% unique_modules) {
                    module_names[current_module] <- name_text
                  }
                }
              }
            }
          }, error = function(e) {
            # Silent error handling - use fallback module IDs
            if (is_omics_verbose()) message(sprintf("[MODULE] Could not fetch module names from KEGG REST API: %s", e$message))
          })
          
          # Create result dataframe with module names
          result <- data.frame(
            kegg_id = clean_id,
            module_id = unique_modules,
            module_name = as.character(module_names[unique_modules]),
            stringsAsFactors = FALSE
          )
          return(result)
        }
      }
    } else {
      status <- if (!is.null(response)) httr::status_code(response) else "NULL"
      if (is_omics_verbose()) message(sprintf("[MODULE] Failed to fetch modules for %s (status: %s)", clean_id, status))
    }
    return(NULL)
  }, error = function(e) {
    message("Error fetching KEGG modules for ", kegg_id, ": ", e$message)
    return(NULL)
  })
}

#' Cached version of fetch_kegg_modules
fetch_kegg_modules_cached <- function(kegg_id) {
  if (is.na(kegg_id) || kegg_id == "") {
    return(NULL)
  }
  
  clean_id <- gsub("^cpd:", "", kegg_id)
  
  # Check SQLite cache first
  cached <- load_cached_kegg_modules(clean_id)
  if (!is.null(cached) && nrow(cached) > 0) {
    # Check if it's a "no modules" marker
    if (nrow(cached) == 1 && cached$module_id[1] == "NONE") {
      return(NULL)
    }
    return(cached[, c("kegg_id", "module_id", "module_name")])
  }
  
  # Fetch from API
  result <- fetch_kegg_modules(kegg_id)
  
  # Cache the result
  if (!is.null(result) && nrow(result) > 0) {
    save_kegg_modules_to_cache(result)
  } else {
    # Negative cache - mark as no modules found
    save_kegg_no_modules(kegg_id)
  }
  
  return(result)
}


# Function to get all pathways for multiple metabolites
fetch_metabolite_pathways <- function(kegg_ids, progress_callback = NULL) {
  if (length(kegg_ids) == 0 || all(is.na(kegg_ids))) {
    return(data.frame(
      kegg_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  all_pathways <- list()
  valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
  
  for (i in seq_along(valid_ids)) {
    if (!is.null(progress_callback) && i %% 5 == 0) {
      progress_callback(i / length(valid_ids), paste("Fetching pathways", i, "of", length(valid_ids)))
    }
    
    pathways <- fetch_kegg_pathways(valid_ids[i])
    if (!is.null(pathways) && nrow(pathways) > 0) {
      all_pathways[[length(all_pathways) + 1]] <- pathways
    }
    
    # Add small delay to avoid overwhelming KEGG API
    Sys.sleep(0.1)
  }
  
  if (length(all_pathways) == 0) {
    return(data.frame(
      kegg_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  return(do.call(rbind, all_pathways))
}

# Function to summarize pathway enrichment
summarize_pathway_enrichment <- function(pathway_df) {
  if (is.null(pathway_df) || nrow(pathway_df) == 0) {
    return(NULL)
  }
  
  pathway_df %>%
    dplyr::group_by(pathway_id, pathway_name) %>%
    dplyr::summarise(
      metabolite_count = n(),
      metabolites = paste(unique(kegg_id), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(metabolite_count))
}

# ============================================================================
# PATHWAY ENRICHMENT ANALYSIS WITH STATISTICS
# ============================================================================

#' Get pathway sizes from KEGG (compound-only counts)
#' Uses /link/cpd/{pathway_id} REST API to count ONLY compounds in each pathway
#' @param pathway_ids Character vector of KEGG pathway IDs (e.g., "map00010")
#' @return Named vector with pathway_id -> pathway_size (compound count)
get_kegg_pathway_sizes <- function(pathway_ids) {
  if (length(pathway_ids) == 0) return(NULL)
  
  # Check session cache first
  cache_key <- digest::digest(paste(sort(unique(pathway_ids)), collapse = "|"))
  cached <- get_session_cache(paste0("pathway_sizes_", cache_key))
  if (!is.null(cached)) {
    message(sprintf("[PATHWAY] Using cached pathway sizes for %d pathways", length(cached)))
    return(cached)
  }
  
  pathway_sizes <- setNames(rep(NA_integer_, length(unique(pathway_ids))), unique(pathway_ids))
  unique_ids <- unique(pathway_ids)
  
  message(sprintf("[PATHWAY] Fetching compound counts from KEGG for %d pathways using /link/cpd/...", length(unique_ids)))
  
  # Fetch compound counts using KEGG REST API /link/cpd/{pathway_id}
  # This returns ONLY compounds, excluding genes, enzymes, reactions
  for (pid in unique_ids) {
    tryCatch({
      # Construct REST API URL: https://rest.kegg.jp/link/cpd/{pathway_id}
      url <- paste0("https://rest.kegg.jp/link/cpd/", pid)
      
      response <- tryCatch({
        httr::GET(url, httr::timeout(10))
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(response) && httr::status_code(response) == 200) {
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        
        # Parse tab-separated response: each line is "pathway_id\tcompound_id"
        # Count the number of lines (excluding empty ones)
        if (nchar(trimws(content)) > 0) {
          lines <- strsplit(content, "\n")[[1]]
          lines <- lines[nchar(trimws(lines)) > 0]
          compound_count <- length(lines)
          
          if (compound_count > 0) {
            pathway_sizes[pid] <- compound_count
            message(sprintf("[PATHWAY] %s: %d compounds", pid, compound_count))
          } else {
            message(sprintf("[PATHWAY] %s has no compounds in KEGG", pid))
          }
        } else {
          message(sprintf("[PATHWAY] %s has no compounds in KEGG", pid))
        }
      } else {
        # Log non-200 status or null response
        status <- if (!is.null(response)) httr::status_code(response) else "NULL"
        message(sprintf("[PATHWAY] Failed to fetch compounds for %s (status: %s)", pid, status))
      }
      
      Sys.sleep(0.15)  # Rate limiting for KEGG API
    }, error = function(e) {
      message(sprintf("[PATHWAY] Error getting compound count for %s: %s", pid, e$message))
    })
  }
  
  # Cache the results
  set_session_cache(paste0("pathway_sizes_", cache_key), pathway_sizes)
  
  # Log summary
  valid_count <- sum(!is.na(pathway_sizes))
  message(sprintf("[PATHWAY] Successfully retrieved compound counts for %d/%d pathways", 
                 valid_count, length(unique_ids)))
  
  return(pathway_sizes)
}

#' Calculate pathway enrichment statistics using hypergeometric test
#' 
#' @param pathway_df Data frame with columns: kegg_id, pathway_id, pathway_name
#' @param total_measured Total number of metabolites measured in the experiment
#' @param background_size Unique compounds mapped to KEGG pathways (from KEGG REST API)
#' @param p_adjust_method Method for p-value adjustment ("BH", "bonferroni", "holm", etc.)
#' @return Data frame with enrichment statistics
calculate_pathway_enrichment <- function(pathway_df, 
                                         total_measured = NULL,
                                         background_size = 6610,  # Unique compounds mapped to KEGG pathways (from KEGG REST API 2025)
                                         p_adjust_method = "BH") {
  
  if (is.null(pathway_df) || nrow(pathway_df) == 0) {
    return(NULL)
  }
  
  # Get unique metabolites that were mapped to any pathway
  unique_metabolites <- unique(pathway_df$kegg_id)
  n_mapped <- length(unique_metabolites)
  
  # If total_measured not provided, use mapped count as minimum
  if (is.null(total_measured) || total_measured < n_mapped) {
    total_measured <- n_mapped
  }
  
  # Summarize pathways
  pathway_summary <- pathway_df %>%
    dplyr::group_by(pathway_id, pathway_name) %>%
    dplyr::summarise(
      hits = dplyr::n_distinct(kegg_id),  # k: number of hits in pathway
      metabolites = paste(unique(kegg_id), collapse = ", "),
      .groups = "drop"
    )
  
  # Get pathway sizes (total metabolites in each pathway) - ALWAYS fetch from KEGG
  message(sprintf("[PATHWAY] Fetching pathway sizes from KEGG for %d pathways...", nrow(pathway_summary)))
  pathway_sizes <- get_kegg_pathway_sizes(pathway_summary$pathway_id)
  
  # Calculate enrichment statistics for each pathway
  enrichment_results <- lapply(seq_len(nrow(pathway_summary)), function(i) {
    row <- pathway_summary[i, ]
    pid <- row$pathway_id
    
    k <- row$hits                                    # Hits in pathway (from our list)
    M <- pathway_sizes[pid]                          # Total metabolites in pathway (from KEGG)
    
    # Skip pathways where we couldn't get the size from KEGG
    if (is.na(M)) {
      message(sprintf("[PATHWAY] Skipping %s - could not fetch pathway size from KEGG", pid))
      return(NULL)
    }
    
    N <- background_size                             # Total metabolites in database
    n <- total_measured                              # Total metabolites we measured
    
    # Enrichment ratio = (k/n) / (M/N) = (k*N) / (n*M)
    # Also known as fold enrichment
    expected <- (n * M) / N
    enrichment_ratio <- if (expected > 0) k / expected else NA
    
    # Hypergeometric test (Fisher's exact test equivalent)
    # P(X >= k) where X ~ Hypergeometric(N, M, n)
    # phyper(k-1, M, N-M, n, lower.tail = FALSE) gives P(X >= k)
    p_value <- tryCatch({
      phyper(k - 1, M, N - M, n, lower.tail = FALSE)
    }, error = function(e) {
      NA_real_
    })
    
    list(
      pathway_id = pid,
      pathway_name = row$pathway_name,
      hits = k,
      pathway_size = M,
      expected = round(expected, 2),
      enrichment_ratio = round(enrichment_ratio, 2),
      p_value = p_value,
      metabolites = row$metabolites
    )
  })
  
  # Remove NULL entries (pathways where we couldn't get size from KEGG)
  enrichment_results <- enrichment_results[!sapply(enrichment_results, is.null)]
  
  if (length(enrichment_results) == 0) {
    message("[PATHWAY] No pathways with valid sizes from KEGG")
    return(NULL)
  }
  
  # Combine results
  results_df <- do.call(rbind, lapply(enrichment_results, as.data.frame, stringsAsFactors = FALSE))
  
  # Calculate adjusted p-values
  results_df$p_adjusted <- p.adjust(results_df$p_value, method = p_adjust_method)
  
  # Add significance indicator
  results_df$significant <- results_df$p_adjusted < 0.05
  
  # Format p-values for display
  results_df$p_value_display <- sapply(results_df$p_value, function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return(sprintf("%.2e", p))
    return(sprintf("%.4f", p))
  })
  
  results_df$p_adjusted_display <- sapply(results_df$p_adjusted, function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return(sprintf("%.2e", p))
    return(sprintf("%.4f", p))
  })
  
  # Sort by p-value
  results_df <- results_df[order(results_df$p_value), ]
  
  # Add rank
  results_df$rank <- seq_len(nrow(results_df))
  
  # Reorder columns for better display
  col_order <- c("rank", "pathway_id", "pathway_name", "hits", "pathway_size", 
                 "expected", "enrichment_ratio", "p_value", "p_value_display",
                 "p_adjusted", "p_adjusted_display", "significant", "metabolites")
  results_df <- results_df[, col_order[col_order %in% names(results_df)]]
  
  # Add metadata as attribute
  attr(results_df, "analysis_params") <- list(
    total_measured = total_measured,
    total_mapped = n_mapped,
    background_size = background_size,
    n_pathways_tested = nrow(results_df),
    p_adjust_method = p_adjust_method,
    timestamp = Sys.time()
  )
  
  return(results_df)
}

#' Cached version of pathway enrichment calculation
calculate_pathway_enrichment_cached <- function(pathway_df, 
                                                 total_measured = NULL,
                                                 background_size = 6610,  # Unique compounds mapped to KEGG pathways (from KEGG REST API 2025)
                                                 p_adjust_method = "BH") {
  
  if (is.null(pathway_df) || nrow(pathway_df) == 0) return(NULL)
  
  # Create cache key
  cache_key <- digest::digest(list(
    pathway_df = pathway_df,
    total_measured = total_measured,
    background_size = background_size,
    p_adjust_method = p_adjust_method
  ))
  
  cached <- get_session_cache(paste0("pathway_enrichment_", cache_key))
  if (!is.null(cached)) return(cached)
  
  result <- calculate_pathway_enrichment(
    pathway_df = pathway_df,
    total_measured = total_measured,
    background_size = background_size,
    p_adjust_method = p_adjust_method
  )
  
  set_session_cache(paste0("pathway_enrichment_", cache_key), result)
  return(result)
}

# ============================================================================
# KEGG MODULE ENRICHMENT ANALYSIS (PARALLEL TO PATHWAY ENRICHMENT)
# ============================================================================

#' Fetch KEGG modules for given compounds
#' Fetches all KEGG modules associated with the input metabolite IDs
#' @param kegg_ids Character vector of KEGG compound IDs (e.g., "C00001")
#' @param progress_callback Optional callback function for progress reporting
#' @return Data frame with columns: kegg_id, module_id, module_name
fetch_metabolite_modules <- function(kegg_ids, progress_callback = NULL) {
  if (length(kegg_ids) == 0 || all(is.na(kegg_ids))) {
    return(data.frame(
      kegg_id = character(),
      module_id = character(),
      module_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  all_modules <- list()
  valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
  
  for (i in seq_along(valid_ids)) {
    if (!is.null(progress_callback) && i %% 5 == 0) {
      progress_callback(i / length(valid_ids), paste("Fetching modules", i, "of", length(valid_ids)))
    }
    
    modules <- fetch_kegg_modules(valid_ids[i])
    if (!is.null(modules) && nrow(modules) > 0) {
      all_modules[[length(all_modules) + 1]] <- modules
    }
    
    # Add small delay to avoid overwhelming KEGG API
    Sys.sleep(0.1)
  }
  
  if (length(all_modules) == 0) {
    return(data.frame(
      kegg_id = character(),
      module_id = character(),
      module_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  return(do.call(rbind, all_modules))
}

#' Cached version of fetch_metabolite_modules
fetch_metabolite_modules_cached <- function(kegg_ids, progress_callback = NULL) {
  initialize_metabolomics_cache_tables()
  
  if (length(kegg_ids) == 0 || all(is.na(kegg_ids))) {
    return(data.frame(
      kegg_id = character(),
      module_id = character(),
      module_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  valid_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
  if (length(valid_ids) == 0) {
    return(data.frame(kegg_id = character(), module_id = character(), module_name = character(), stringsAsFactors = FALSE))
  }
  
  # Check session cache
  cache_key <- digest::digest(paste(sort(valid_ids), collapse = "|"))
  session_cached <- get_session_cache(paste0("modules_", cache_key))
  if (!is.null(session_cached)) {
    return(session_cached)
  }
  
  # Check SQLite cache for all IDs at once
  cached_modules <- load_cached_kegg_modules(valid_ids)
  
  # Determine which IDs need API calls
  if (!is.null(cached_modules)) {
    cached_ids <- unique(cached_modules$kegg_id)
    uncached_ids <- valid_ids[!gsub("^cpd:", "", valid_ids) %in% cached_ids]
  } else {
    uncached_ids <- valid_ids
  }
  
  # Fetch uncached modules from API
  new_modules <- list()
  if (length(uncached_ids) > 0) {
    for (i in seq_along(uncached_ids)) {
      if (!is.null(progress_callback) && i %% 5 == 0) {
        progress_callback(i / length(uncached_ids), paste("Fetching modules", i, "of", length(uncached_ids)))
      }
      
      modules <- fetch_kegg_modules_cached(uncached_ids[i])
      if (!is.null(modules) && nrow(modules) > 0) {
        new_modules[[length(new_modules) + 1]] <- modules
      }
      
      # Rate limiting for KEGG API
      Sys.sleep(0.1)
    }
  }
  
  # Combine results
  all_modules <- list()
  if (!is.null(cached_modules) && nrow(cached_modules) > 0) {
    # Filter out "NONE" markers
    real_cached <- cached_modules[cached_modules$module_id != "NONE", ]
    if (nrow(real_cached) > 0) {
      all_modules[[1]] <- real_cached[, c("kegg_id", "module_id", "module_name")]
    }
  }
  if (length(new_modules) > 0) {
    all_modules <- c(all_modules, new_modules)
  }
  
  if (length(all_modules) == 0) {
    result <- data.frame(kegg_id = character(), module_id = character(), module_name = character(), stringsAsFactors = FALSE)
  } else {
    result <- do.call(rbind, all_modules)
    rownames(result) <- NULL
  }
  
  # Cache in session
  set_session_cache(paste0("modules_", cache_key), result)
  
  return(result)
}

#' Get module sizes from KEGG (compound-only counts)
#' Uses /link/cpd/{module_id} REST API to count ONLY compounds in each module
#' @param module_ids Character vector of KEGG module IDs (e.g., "M00001")
#' @return Named vector with module_id -> module_size (compound count)
get_kegg_module_sizes <- function(module_ids) {
  if (length(module_ids) == 0) return(NULL)
  
  # Check session cache first
  cache_key <- digest::digest(paste(sort(unique(module_ids)), collapse = "|"))
  cached <- get_session_cache(paste0("module_sizes_", cache_key))
  if (!is.null(cached)) {
    message(sprintf("[MODULE] Using cached module sizes for %d modules", length(cached)))
    return(cached)
  }
  
  module_sizes <- setNames(rep(NA_integer_, length(unique(module_ids))), unique(module_ids))
  unique_ids <- unique(module_ids)
  
  message(sprintf("[MODULE] Fetching compound counts from KEGG for %d modules using /link/cpd/...", length(unique_ids)))
  
  # Fetch compound counts using KEGG REST API /link/cpd/{module_id}
  # This returns ONLY compounds, excluding genes, enzymes, reactions
  for (mid in unique_ids) {
    tryCatch({
      # Construct REST API URL: https://rest.kegg.jp/link/cpd/{module_id}
      url <- paste0("https://rest.kegg.jp/link/cpd/", mid)
      
      response <- tryCatch({
        httr::GET(url, httr::timeout(10))
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(response) && httr::status_code(response) == 200) {
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        
        # Parse tab-separated response: each line is "module_id\tcompound_id"
        # Count the number of lines (excluding empty ones)
        if (nchar(trimws(content)) > 0) {
          lines <- strsplit(content, "\n")[[1]]
          lines <- lines[nchar(trimws(lines)) > 0]
          compound_count <- length(lines)
          
          if (compound_count > 0) {
            module_sizes[mid] <- compound_count
            message(sprintf("[MODULE] %s: %d compounds", mid, compound_count))
          } else {
            message(sprintf("[MODULE] %s has no compounds in KEGG", mid))
          }
        } else {
          message(sprintf("[MODULE] %s has no compounds in KEGG", mid))
        }
      } else {
        # Log non-200 status or null response
        status <- if (!is.null(response)) httr::status_code(response) else "NULL"
        message(sprintf("[MODULE] Failed to fetch compounds for %s (status: %s)", mid, status))
      }
      
      Sys.sleep(0.15)  # Rate limiting for KEGG API
    }, error = function(e) {
      message(sprintf("[MODULE] Error getting compound count for %s: %s", mid, e$message))
    })
  }
  
  # Cache the results
  set_session_cache(paste0("module_sizes_", cache_key), module_sizes)
  
  # Log summary
  valid_count <- sum(!is.na(module_sizes))
  message(sprintf("[MODULE] Successfully retrieved compound counts for %d/%d modules", 
                 valid_count, length(unique_ids)))
  
  return(module_sizes)
}

#' Calculate module enrichment statistics using hypergeometric test
#' 
#' @param module_df Data frame with columns: kegg_id, module_id, module_name
#' @param total_measured Total number of metabolites measured in the experiment
#' @param background_size Unique compounds mapped to KEGG modules (from KEGG REST API)
#' @param p_adjust_method Method for p-value adjustment ("BH", "bonferroni", "holm", etc.)
#' @return Data frame with enrichment statistics
calculate_module_enrichment <- function(module_df, 
                                        total_measured = NULL,
                                        background_size = 6610,  # Unique compounds mapped to KEGG modules (from KEGG REST API 2025)
                                        p_adjust_method = "BH") {
  
  if (is.null(module_df) || nrow(module_df) == 0) {
    return(NULL)
  }
  
  # Get unique metabolites that were mapped to any module
  unique_metabolites <- unique(module_df$kegg_id)
  n_mapped <- length(unique_metabolites)
  
  # If total_measured not provided, use mapped count as minimum
  if (is.null(total_measured) || total_measured < n_mapped) {
    total_measured <- n_mapped
  }
  
  # Summarize modules
  module_summary <- module_df %>%
    dplyr::group_by(module_id, module_name) %>%
    dplyr::summarise(
      hits = dplyr::n_distinct(kegg_id),  # k: number of hits in module
      metabolites = paste(unique(kegg_id), collapse = ", "),
      .groups = "drop"
    )
  
  # Get module sizes (total metabolites in each module) - ALWAYS fetch from KEGG
  message(sprintf("[MODULE] Fetching module sizes from KEGG for %d modules...", nrow(module_summary)))
  module_sizes <- get_kegg_module_sizes(module_summary$module_id)
  
  # Calculate enrichment statistics for each module
  enrichment_results <- lapply(seq_len(nrow(module_summary)), function(i) {
    row <- module_summary[i, ]
    mid <- row$module_id
    
    k <- row$hits                                    # Hits in module (from our list)
    M <- module_sizes[mid]                          # Total metabolites in module (from KEGG)
    
    # Skip modules where we couldn't get the size from KEGG
    if (is.na(M)) {
      message(sprintf("[MODULE] Skipping %s - could not fetch module size from KEGG", mid))
      return(NULL)
    }
    
    N <- background_size                             # Total metabolites in database
    n <- total_measured                              # Total metabolites we measured
    
    # Enrichment ratio = (k/n) / (M/N) = (k*N) / (n*M)
    # Also known as fold enrichment
    expected <- (n * M) / N
    enrichment_ratio <- if (expected > 0) k / expected else NA
    
    # Hypergeometric test (Fisher's exact test equivalent)
    # P(X >= k) where X ~ Hypergeometric(N, M, n)
    # phyper(k-1, M, N-M, n, lower.tail = FALSE) gives P(X >= k)
    p_value <- tryCatch({
      phyper(k - 1, M, N - M, n, lower.tail = FALSE)
    }, error = function(e) {
      NA_real_
    })
    
    list(
      module_id = mid,
      module_name = row$module_name,
      hits = k,
      module_size = M,
      expected = round(expected, 2),
      enrichment_ratio = round(enrichment_ratio, 2),
      p_value = p_value,
      metabolites = row$metabolites
    )
  })
  
  # Remove NULL entries (modules where we couldn't get size from KEGG)
  enrichment_results <- enrichment_results[!sapply(enrichment_results, is.null)]
  
  if (length(enrichment_results) == 0) {
    message("[MODULE] No modules with valid sizes from KEGG")
    return(NULL)
  }
  
  # Combine results
  results_df <- do.call(rbind, lapply(enrichment_results, as.data.frame, stringsAsFactors = FALSE))
  
  # Calculate adjusted p-values
  results_df$p_adjusted <- p.adjust(results_df$p_value, method = p_adjust_method)
  
  # Add significance indicator
  results_df$significant <- results_df$p_adjusted < 0.05
  
  # Format p-values for display
  results_df$p_value_display <- sapply(results_df$p_value, function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return(sprintf("%.2e", p))
    return(sprintf("%.4f", p))
  })
  
  results_df$p_adjusted_display <- sapply(results_df$p_adjusted, function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return(sprintf("%.2e", p))
    return(sprintf("%.4f", p))
  })
  
  # Sort by p-value
  results_df <- results_df[order(results_df$p_value), ]
  
  # Add rank
  results_df$rank <- seq_len(nrow(results_df))
  
  # Reorder columns for better display
  col_order <- c("rank", "module_id", "module_name", "hits", "module_size", 
                 "expected", "enrichment_ratio", "p_value", "p_value_display",
                 "p_adjusted", "p_adjusted_display", "significant", "metabolites")
  results_df <- results_df[, col_order[col_order %in% names(results_df)]]
  
  # Add metadata as attribute
  attr(results_df, "analysis_params") <- list(
    total_measured = total_measured,
    total_mapped = n_mapped,
    background_size = background_size,
    n_modules_tested = nrow(results_df),
    p_adjust_method = p_adjust_method,
    timestamp = Sys.time()
  )
  
  return(results_df)
}

#' Cached version of module enrichment calculation
calculate_module_enrichment_cached <- function(module_df, 
                                                total_measured = NULL,
                                                background_size = 6610,  # Unique compounds mapped to KEGG modules (from KEGG REST API 2025)
                                                p_adjust_method = "BH") {
  
  if (is.null(module_df) || nrow(module_df) == 0) return(NULL)
  
  # Create cache key
  cache_key <- digest::digest(list(
    module_df = module_df,
    total_measured = total_measured,
    background_size = background_size,
    p_adjust_method = p_adjust_method
  ))
  
  cached <- get_session_cache(paste0("module_enrichment_", cache_key))
  if (!is.null(cached)) return(cached)
  
  result <- calculate_module_enrichment(
    module_df = module_df,
    total_measured = total_measured,
    background_size = background_size,
    p_adjust_method = p_adjust_method
  )
  
  set_session_cache(paste0("module_enrichment_", cache_key), result)
  return(result)
}

# ============================================================================
# END METABOLOMICS CLASSIFICATION AND ANALYSIS FUNCTIONS
# ============================================================================

options(shiny.maxRequestSize = 100*1024^2)  # 100 MB upload size limit

# Create the www directory
dir.create("www", showWarnings = FALSE)

