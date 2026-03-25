# ============================================================================
# Dockerfile for OmicsStack Shiny Application
# ============================================================================
# Multi-stage build to reduce final image size and improve deployment speed
# Pre-installs all dependencies and annotation databases

FROM rocker/shiny:4.4.0 as base

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
    pandoc \
    pandoc-citeproc \
    git \
    wget \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# Stage 2: Install R packages with proper dependency resolution
# ============================================================================

# Install BiocManager first
RUN R --vanilla -e "install.packages('BiocManager', repos='http://cran.r-project.org')"

# Install core Shiny packages
RUN R --vanilla -e "install.packages(c(\
    'shiny', \
    'shinydashboard', \
    'shinyjs', \
    'shinycssloaders', \
    'shinyWidgets', \
    'shinyBS', \
    'bslib' \
  ), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install data manipulation and I/O packages
RUN R --vanilla -e "install.packages(c(\
    'dplyr', \
    'tidyr', \
    'stringr', \
    'readxl', \
    'openxlsx', \
    'writexl', \
    'DT', \
    'rhandsontable', \
    'digest' \
  ), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install visualization packages
RUN R --vanilla -e "install.packages(c(\
    'ggplot2', \
    'plotly', \
    'ggprism', \
    'ggrepel', \
    'ggsignif', \
    'ggVennDiagram', \
    'ggupset', \
    'patchwork', \
    'reshape2', \
    'gridExtra', \
    'grid', \
    'factoextra', \
    'RColorBrewer', \
    'viridis', \
    'scales', \
    'colourpicker', \
    'VennDiagram', \
    'UpSetR', \
    'heatmaply', \
    'pheatmap', \
    'htmlwidgets', \
    'visNetwork', \
    'igraph', \
    'ggraph' \
  ), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install Bioconductor packages for annotation and analysis
RUN R --vanilla -e "BiocManager::install(c(\
    'ComplexHeatmap', \
    'circlize', \
    'DESeq2', \
    'edgeR', \
    'limma', \
    'sva', \
    'preprocessCore', \
    'vsn', \
    'ROTS', \
    'AnnotationDbi', \
    'GO.db', \
    'GOSemSim', \
    'topGO' \
  ), update=FALSE, ask=FALSE)"

# Install enrichment analysis packages
RUN R --vanilla -e "BiocManager::install(c(\
    'clusterProfiler', \
    'enrichplot', \
    'DOSE', \
    'ReactomePA', \
    'msigdbr', \
    'pathview' \
  ), update=FALSE, ask=FALSE)"

# Install annotation databases for all supported organisms
# Human, Mouse, Rat, Zebrafish, Arabidopsis, Fruit Fly, C. elegans, Yeast, E. coli
RUN R --vanilla -e "BiocManager::install(c(\
    'org.Hs.eg.db', \
    'org.Mm.eg.db', \
    'org.Rn.eg.db', \
    'org.Dr.eg.db', \
    'org.At.tair.db', \
    'org.Dm.eg.db', \
    'org.Ce.eg.db', \
    'org.Sc.sgd.db', \
    'org.EcK12.eg.db' \
  ), update=FALSE, ask=FALSE)"

# Install machine learning packages
RUN R --vanilla -e "install.packages(c(\
    'randomForest', \
    'randomForestExplainer', \
    'glmnet', \
    'mixOmics', \
    'missMDA', \
    'plyr', \
    'multcomp', \
    'agricolae', \
    'PMCMRplus' \
  ), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install statistical and utility packages
RUN R --vanilla -e "install.packages(c(\
    'pROC', \
    'PRROC', \
    'httr', \
    'httr2', \
    'jsonlite', \
    'markdown', \
    'png', \
    'future', \
    'promises', \
    'pwrss', \
    'userfriendlyscience', \
    'cli', \
    'rlang', \
    'xfun' \
  ), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install gprofiler2 and enrichR
RUN R --vanilla -e "install.packages(c('gprofiler2', 'enrichR'), repos='http://cran.r-project.org', dependencies=TRUE)"

# Install BiocParallel for parallel processing
RUN R --vanilla -e "BiocManager::install('BiocParallel', update=FALSE, ask=FALSE)"

# ============================================================================
# Stage 3: Copy application files and set up directories
# ============================================================================

# Create app directory
RUN mkdir -p /srv/shiny-server/omics

# Copy application files
COPY app.R /srv/shiny-server/omics/app.R
COPY www/ /srv/shiny-server/omics/www/
COPY omics-dashboard.css /srv/shiny-server/omics/omics-dashboard.css
COPY seo_config.R /srv/shiny-server/omics/seo_config.R
COPY *.R /srv/shiny-server/omics/
COPY sample_data/ /srv/shiny-server/omics/sample_data/

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
