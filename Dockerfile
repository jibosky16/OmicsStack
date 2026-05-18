# ============================================================================
# Dockerfile for OmicsStack Shiny Application
# ============================================================================
# Optimized for faster builds with consolidated RUN layers and cache mounts

FROM rocker/shiny:4.4.0 AS base

# Set environment variables for production
# Keep CRAN pre-ggplot2-4 because the R 4.4 ggtree stack expects ggplot2's check_linewidth helper.
ENV DEBIAN_FRONTEND=noninteractive \
    PRELOAD_ANNOTATION_DBS=TRUE \
    SHINY_SERVER=TRUE \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/2025-08-31 \
    GGPLOT2_VERSION=3.5.2

# ============================================================================
# Stage 1: Install system dependencies
# ============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libbz2-dev \
    liblzma-dev \
    libglpk-dev \
    libgmp-dev \
    libmpfr-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libxt-dev \
    libhdf5-dev \
    libmagick++-dev \
    libclang-dev \
    cmake \
    pandoc \
    pandoc-citeproc \
    git \
    cargo-1.85 \
    rustc-1.85 \
    wget \
    && ln -sf /usr/bin/cargo-1.85 /usr/local/bin/cargo \
    && ln -sf /usr/bin/rustc-1.85 /usr/local/bin/rustc \
    && cargo --version \
    && rustc --version \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# Stage 2: Install R packages with consolidated layers and BuildKit cache
# ============================================================================

# Install BiocManager and all CRAN packages in single RUN layer
# Using BuildKit cache mount to accelerate subsequent rebuilds
# Use normal R startup so Rocker's site profile enables Posit binary packages.
RUN --mount=type=cache,target=/root/.cache/R \
    R -q -e "\
    cran_repo <- Sys.getenv('CRAN_REPO'); \
    options(timeout=max(1200, getOption('timeout'))); \
    install.packages(c('BiocManager', 'remotes'), repos=cran_repo); \
    cran_pkgs <- c( \
      'shiny', 'shinydashboard', 'shinyjs', 'shinycssloaders', 'shinyWidgets', 'shinyBS', 'bslib', \
      'dplyr', 'tidyr', 'stringr', 'readxl', 'openxlsx', 'writexl', 'DT', 'rhandsontable', 'digest', \
      'plotly', 'ggprism', 'ggrepel', 'ggsignif', 'ggVennDiagram', 'ggupset', 'patchwork', 'reshape2', \
      'gridExtra', 'factoextra', 'RColorBrewer', 'viridis', 'scales', 'colourpicker', 'VennDiagram', \
      'UpSetR', 'heatmaply', 'pheatmap', 'htmlwidgets', 'visNetwork', 'igraph', 'ggraph', \
      'randomForest', 'randomForestExplainer', 'glmnet', 'missMDA', 'plyr', 'multcomp', 'agricolae', \
      'PMCMRplus', 'caret', 'e1071', 'ranger', 'gbm', 'xgboost', 'iml', 'MLmetrics', \
      'rstatix', 'car', 'emmeans', 'dunn.test', 'mice', 'foreach', 'doParallel', \
      'pROC', 'PRROC', 'data.table', 'httr', 'httr2', 'htmltools', 'jsonlite', 'later', \
      'moments', 'png', 'future', 'future.apply', 'promises', 'pwrss', 'cli', 'rlang', \
      'umap', 'Rtsne', 'svglite', 'Cairo', 'ggridges', 'ggbeeswarm', 'cowplot', 'fmsb', 'yulab.utils', \
      'zip', 'PubChemR', 'webchem', 'DBI', 'RSQLite', 'RSpectra', 'corrplot', 'commonmark', \
      'dynamicTreeCut', 'flashClust', 'reticulate', 'viridisLite', 'gprofiler2', 'enrichR' \
    ); \
    # Use Bioconductor plus Ubuntu binary CRAN packages to avoid fragile source builds in Docker.
    repos <- BiocManager::repositories(); \
    repos <- repos[names(repos) != 'CRAN']; \
    repos['CRAN'] <- cran_repo; \
    options(repos = repos); \
    install.packages(cran_pkgs, Ncpus=1); \
    if (length(missing <- setdiff(cran_pkgs, installed.packages()[,'Package'])) > 0) stop(paste('Failed to install CRAN packages:', paste(missing, collapse=', '))); \
    "

# Install Bioconductor packages in single layer
# Use normal R startup so CRAN dependencies still come from Posit binaries.
RUN --mount=type=cache,target=/root/.cache/R \
    R -q -e "\
    cran_repo <- Sys.getenv('CRAN_REPO'); \
    options(timeout=max(1200, getOption('timeout'))); \
    repos <- BiocManager::repositories(); \
  repos <- repos[names(repos) != 'CRAN']; \
  repos['CRAN'] <- cran_repo; \
    options(repos = repos); \
    remotes::install_version('ggplot2', version=Sys.getenv('GGPLOT2_VERSION'), repos='https://cloud.r-project.org', upgrade='never', dependencies=FALSE); \
    bioc_pkgs1 <- c( \
      'UCSC.utils', 'GenomeInfoDb', 'Biostrings', 'KEGGREST', 'AnnotationDbi', 'GO.db', \
      'GOSemSim', 'DOSE', 'enrichplot', 'clusterProfiler', 'ReactomePA', 'treeio', 'ggtree', \
      'ComplexHeatmap', 'circlize', 'DESeq2', 'edgeR', 'limma', 'sva', 'preprocessCore', \
      'SummarizedExperiment', 'vsn', 'ROTS', 'topGO', 'msigdbr', 'pathview', \
      'org.Hs.eg.db', 'org.Mm.eg.db', 'org.Rn.eg.db', 'org.Dr.eg.db', 'org.At.tair.db', \
      'org.Dm.eg.db', 'org.Ce.eg.db', 'org.Sc.sgd.db', 'org.EcK12.eg.db' \
    ); \
    BiocManager::install(bioc_pkgs1, update=FALSE, ask=FALSE, INSTALL_opts=c('--no-test-load', '--no-docs')); \
    if (length(missing <- setdiff(bioc_pkgs1, installed.packages()[,'Package'])) > 0) stop(paste('Failed to install Bioconductor packages:', paste(missing, collapse=', '))); \
    bioc_pkgs2 <- c( \
      'mixOmics', 'MOFA2', 'MOFAdata', 'impute', 'Rgraphviz', 'KEGGgraph', \
      'hmdbQuery', 'rhdf5', 'biodb', 'biodbChebi', 'BiocParallel' \
    ); \
    BiocManager::install(bioc_pkgs2, update=FALSE, ask=FALSE, INSTALL_opts=c('--no-test-load', '--no-docs')); \
    if (length(missing <- setdiff(bioc_pkgs2, installed.packages()[,'Package'])) > 0) stop(paste('Failed to install Bioconductor packages:', paste(missing, collapse=', '))) \
    "

# Install CRAN packages that need compatibility pins after shared dependencies settle.
RUN --mount=type=cache,target=/root/.cache/R \
    R -q -e "\
    cran_repo <- Sys.getenv('CRAN_REPO'); \
    options(timeout=max(1200, getOption('timeout'))); \
    repos <- BiocManager::repositories(); \
    repos <- repos[names(repos) != 'CRAN']; \
    repos['CRAN'] <- cran_repo; \
    options(repos = repos); \
    install.packages(c('Hmisc', 'fastcluster', 'matrixStats'), Ncpus=1); \
    remotes::install_version('xfun', version='0.52', repos='https://cloud.r-project.org', upgrade='never', dependencies=FALSE); \
    remotes::install_version('ggplot2', version=Sys.getenv('GGPLOT2_VERSION'), repos='https://cloud.r-project.org', upgrade='never', dependencies=FALSE); \
    remotes::install_version('markdown', version='1.13', repos='https://cloud.r-project.org', upgrade='never', dependencies=FALSE); \
    remotes::install_version('Boruta', version='9.0.0', repos='https://cloud.r-project.org', upgrade='never', dependencies=FALSE); \
    install.packages('WGCNA', repos=cran_repo, dependencies=FALSE, Ncpus=1); \
    pinned_cran_pkgs <- c('xfun', 'ggplot2', 'markdown', 'Boruta', 'WGCNA'); \
    if (as.character(packageVersion('xfun')) != '0.52' || !'attr' %in% getNamespaceExports('xfun')) stop('Pinned xfun 0.52 with exported attr is required for rmarkdown/htmlTable/WGCNA dependencies'); \
    if (as.character(packageVersion('ggplot2')) != Sys.getenv('GGPLOT2_VERSION') || !exists('check_linewidth', envir=asNamespace('ggplot2'), inherits=FALSE)) stop(paste('Pinned ggplot2', Sys.getenv('GGPLOT2_VERSION'), 'with check_linewidth is required')); \
    if (as.character(packageVersion('markdown')) != '1.13') stop('Pinned markdown 1.13 is required to avoid litedown requiring xfun >= 0.55'); \
    if (as.character(packageVersion('Boruta')) != '9.0.0') stop('Pinned Boruta 9.0.0 is required to avoid fru on R 4.4'); \
    if (length(missing <- setdiff(pinned_cran_pkgs, installed.packages()[,'Package'])) > 0) stop(paste('Failed to install pinned CRAN packages:', paste(missing, collapse=', '))); \
    "


# ============================================================================
# Stage 3: Copy application files and set up directories
# ============================================================================

# Create app directory
RUN mkdir -p /srv/shiny-server/omics

# Copy application files
COPY app.R /srv/shiny-server/omics/app.R
COPY www/ /srv/shiny-server/omics/www/
COPY omics-dashboard.css /srv/shiny-server/omics/omics-dashboard.css
COPY scripts/ /srv/shiny-server/omics/scripts/
COPY *.R /srv/shiny-server/omics/
COPY sample_data/ /srv/shiny-server/omics/sample_data/

# Build-time package audit (creates file if missing)
RUN if [ -f /srv/shiny-server/omics/scripts/packages.R ]; then \
      Rscript /srv/shiny-server/omics/scripts/packages.R --code-dir=/srv/shiny-server/omics; \
    fi

# Create symlink for Shiny Server default location
RUN ln -s /srv/shiny-server/omics /srv/shiny-server/OmicsStack

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/

# Create logs directory for the app
RUN mkdir -p /var/log/shiny-server && chown -R shiny:shiny /var/log/shiny-server

# ============================================================================
# Stage 4: Configure and run Shiny Server
# ============================================================================

# Create a custom Shiny Server configuration
RUN echo "\
run_as shiny; \n\
\n\
server { \n\
  listen 3838; \n\
  log_dir /var/log/shiny-server; \n\
  \n\
  location /OmicsStack { \n\
    app_dir /srv/shiny-server/omics; \n\
    sanitize_errors false; \n\
  } \n\
}" > /etc/shiny-server/shiny-server.conf

# Expose port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD wget -q --spider http://localhost:3838/OmicsStack/ || exit 1

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
