#!/usr/bin/env Rscript

# Build-time package audit for OmicsStack.
# 1) Ensure all expected packages are installed.
# 2) Scan all R files and ensure all non-base/recommended packages used in code
#    are both listed as expected and installed.

args <- commandArgs(trailingOnly = TRUE)
code_dir <- "."

for (arg in args) {
  if (startsWith(arg, "--code-dir=")) {
    code_dir <- sub("^--code-dir=", "", arg)
  }
}

if (!dir.exists(code_dir)) {
  stop(sprintf("Code directory not found: %s", code_dir))
}

expected_pkgs <- sort(unique(c(
  # Core app and UI
  "shiny", "shinydashboard", "shinyjs", "shinycssloaders", "shinyWidgets", "shinyBS", "bslib",
  # Data wrangling and I/O
  "dplyr", "tidyr", "stringr", "readxl", "openxlsx", "writexl", "DT", "rhandsontable", "digest",
  # Visualization
  "ggplot2", "plotly", "ggprism", "ggrepel", "ggsignif", "ggVennDiagram", "ggupset", "patchwork",
  "reshape2", "gridExtra", "factoextra", "RColorBrewer", "viridis", "scales", "colourpicker",
  "VennDiagram", "UpSetR", "heatmaply", "pheatmap", "htmlwidgets", "visNetwork", "igraph", "ggraph",
  "ggbeeswarm", "ggridges", "cowplot", "fmsb", "svglite", "Cairo", "corrplot", "viridisLite",
  # Bioconductor analysis and enrichment
  "ComplexHeatmap", "circlize", "DESeq2", "edgeR", "limma", "sva", "preprocessCore", "vsn", "ROTS",
  "AnnotationDbi", "GO.db", "GOSemSim", "topGO", "clusterProfiler", "enrichplot", "DOSE", "ReactomePA",
  "msigdbr", "pathview", "SummarizedExperiment",
  # Annotation DBs
  "org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db", "org.Dr.eg.db", "org.At.tair.db", "org.Dm.eg.db",
  "org.Ce.eg.db", "org.Sc.sgd.db", "org.EcK12.eg.db",
  # ML and stats
  "randomForest", "randomForestExplainer", "glmnet", "mixOmics", "missMDA", "plyr", "multcomp",
  "agricolae", "PMCMRplus", "caret", "e1071", "Boruta", "ranger", "gbm", "xgboost", "iml",
  "MLmetrics", "rstatix", "car", "emmeans", "dunn.test", "mice", "foreach",
  "doParallel", "WGCNA", "dynamicTreeCut", "flashClust", "RSpectra", "pROC", "PRROC", "pwrss",
  "moments", "umap", "Rtsne",
  # Multi-omics and metabolomics
  "MOFA2", "MOFAdata", "impute", "Rgraphviz", "KEGGgraph", "KEGGREST", "hmdbQuery", "rhdf5",
  "biodb", "biodbChebi", "PubChemR", "webchem",
  # Utilities and infra
  "httr", "httr2", "jsonlite", "markdown", "png", "future", "future.apply", "promises", "later",
  "cli", "rlang", "xfun", "zip", "DBI", "RSQLite", "reticulate", "gprofiler2", "enrichR",
  "BiocParallel", "BiocManager", "data.table", "htmltools"
)))

scan_r_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")

  # Match pkg::symbol (avoid plain pkg:: with no symbol).
  ns <- regmatches(
    text,
    gregexpr("([A-Za-z][A-Za-z0-9.]*)::([A-Za-z][A-Za-z0-9._]*)", text, perl = TRUE)
  )[[1]]
  ns_pkgs <- character(0)
  if (length(ns) > 0 && ns[1] != "") {
    ns_pkgs <- sub("::.*$", "", ns)
  }

  # Match library(pkg) / require(pkg) calls.
  imp <- regmatches(
    text,
    gregexpr("(?:library|require)\\s*\\(\\s*['\"]?([A-Za-z][A-Za-z0-9.]*)", text, perl = TRUE)
  )[[1]]
  imp_pkgs <- character(0)
  if (length(imp) > 0 && imp[1] != "") {
    imp_pkgs <- sub("^(?:library|require)\\s*\\(\\s*['\"]?", "", imp, perl = TRUE)
  }

  unique(c(ns_pkgs, imp_pkgs))
}

r_files <- list.files(code_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
if (length(r_files) == 0) {
  stop(sprintf("No R files found in: %s", code_dir))
}

detected_pkgs <- sort(unique(unlist(lapply(r_files, scan_r_file), use.names = FALSE)))

# Filter out obvious non-package tokens from CSS pseudo selectors or common symbols
# that can appear in ui.R style strings.
ignored_tokens <- c("input", "summary", "th", "wtHolder", "metab_summary", "glyco_summary", "pkg", "pkg_name")
detected_pkgs <- setdiff(detected_pkgs, ignored_tokens)

base_and_recommended <- rownames(installed.packages(priority = c("base", "recommended")))
detected_non_base <- sort(setdiff(detected_pkgs, base_and_recommended))

missing_from_expected <- setdiff(detected_non_base, expected_pkgs)
missing_from_installed_from_code <- detected_non_base[
  !vapply(detected_non_base, requireNamespace, logical(1), quietly = TRUE)
]
missing_from_installed_expected <- expected_pkgs[
  !vapply(expected_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

cat(sprintf("Scanned %d R files.\\n", length(r_files)))
cat(sprintf("Detected %d package references (%d non-base/recommended).\\n",
            length(detected_pkgs), length(detected_non_base)))
cat(sprintf("Expected package set size: %d\\n", length(expected_pkgs)))

if (length(missing_from_expected) > 0) {
  cat("Packages used in code but missing from expected package set:\n")
  cat(paste0(" - ", sort(missing_from_expected), collapse = "\n"), "\n")
}

if (length(missing_from_installed_from_code) > 0) {
  cat("Packages used in code but not installed:\n")
  cat(paste0(" - ", sort(missing_from_installed_from_code), collapse = "\n"), "\n")
}

if (length(missing_from_installed_expected) > 0) {
  cat("Expected packages not installed:\n")
  cat(paste0(" - ", sort(missing_from_installed_expected), collapse = "\n"), "\n")
}

if (length(missing_from_expected) > 0 ||
    length(missing_from_installed_from_code) > 0 ||
    length(missing_from_installed_expected) > 0) {
  stop("Package audit failed. See missing package report above.")
}

cat("Package audit passed: expected set covers code usage and all required packages are installed.\n")
