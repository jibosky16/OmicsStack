# ============================================================================
# Dockerfile for OmicsStack Shiny Application
# ============================================================================
# Optimized for faster builds with consolidated RUN layers and cache mounts

FROM rocker/shiny:4.4.0 AS base

# Set environment variables for production
ENV DEBIAN_FRONTEND=noninteractive \
    PRELOAD_ANNOTATION_DBS=TRUE \
    SHINY_SERVER=TRUE \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8

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
    cmake \
    pandoc \
    pandoc-citeproc \
    git \
    wget \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# Stage 2: Install R packages with consolidated layers and BuildKit cache
# ============================================================================

# Install BiocManager and all CRAN packages in single RUN layer
# Using BuildKit cache mount to accelerate subsequent rebuilds
RUN --mount=type=cache,target=/root/.cache/R \
    R --vanilla -e "\
    install.packages('BiocManager', repos='https://cloud.r-project.org'); \
    cran_pkgs <- c( \
      'shiny', 'shinydashboard', 'shinyjs', 'shinycssloaders', 'shinyWidgets', 'shinyBS', 'bslib', \
      'dplyr', 'tidyr', 'stringr', 'readxl', 'openxlsx', 'writexl', 'DT', 'rhandsontable', 'digest', \
      'ggplot2', 'plotly', 'ggprism', 'ggrepel', 'ggsignif', 'ggVennDiagram', 'ggupset', 'patchwork', 'reshape2', \
      'gridExtra', 'grid', 'factoextra', 'RColorBrewer', 'viridis', 'scales', 'colourpicker', 'VennDiagram', \
      'UpSetR', 'heatmaply', 'pheatmap', 'htmlwidgets', 'visNetwork', 'igraph', 'ggraph', \
      'randomForest', 'randomForestExplainer', 'glmnet', 'missMDA', 'plyr', 'multcomp', 'agricolae', \
      'PMCMRplus', 'caret', 'e1071', 'Boruta', 'ranger', 'gbm', 'xgboost', 'iml', 'MLmetrics', \
      'rstatix', 'car', 'emmeans', 'dunn.test', 'mice', 'foreach', 'doParallel', \
      'pROC', 'PRROC', 'data.table', 'httr', 'httr2', 'htmltools', 'jsonlite', 'later', 'markdown', \
      'moments', 'png', 'future', 'future.apply', 'promises', 'pwrss', 'cli', 'rlang', 'xfun', \
      'umap', 'Rtsne', 'svglite', 'Cairo', 'ggridges', 'ggbeeswarm', 'cowplot', 'fmsb', 'yulab.utils', \
      'zip', 'PubChemR', 'webchem', 'DBI', 'RSQLite', 'RSpectra', 'WGCNA', 'corrplot', \
      'dynamicTreeCut', 'flashClust', 'reticulate', 'viridisLite', 'gprofiler2', 'enrichR' \
    ); \
    # Use both Bioconductor and the latest CRAN to ensure up-to-date dependencies and cross-repo resolution
    repos <- BiocManager::repositories(); \
    repos['CRAN'] <- 'https://cloud.r-project.org'; \
    options(repos = repos); \
    install.packages(cran_pkgs, Ncpus=4); \
    "

# Install Bioconductor packages in single layer
RUN --mount=type=cache,target=/root/.cache/R \
    R --vanilla -e "\
    repos <- BiocManager::repositories(); \
    repos['CRAN'] <- 'https://cloud.r-project.org'; \
    options(repos = repos); \
    BiocManager::install(c( \
      'ComplexHeatmap', 'circlize', 'DESeq2', 'edgeR', 'limma', 'sva', 'preprocessCore', \
      'SummarizedExperiment', 'vsn', 'ROTS', 'AnnotationDbi', 'GO.db', 'GOSemSim', 'topGO', \
      'clusterProfiler', 'enrichplot', 'DOSE', 'ReactomePA', 'msigdbr', 'pathview', \
      'org.Hs.eg.db', 'org.Mm.eg.db', 'org.Rn.eg.db', 'org.Dr.eg.db', 'org.At.tair.db', \
      'org.Dm.eg.db', 'org.Ce.eg.db', 'org.Sc.sgd.db', 'org.EcK12.eg.db', \
      'mixOmics', 'MOFA2', 'MOFAdata', 'impute', 'Rgraphviz', 'KEGGgraph', 'KEGGREST', \
      'hmdbQuery', 'rhdf5', 'biodb', 'biodbChebi', 'BiocParallel' \
    ), update=FALSE, ask=FALSE, INSTALL_opts=c('--no-test-load', '--no-docs')) \
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
