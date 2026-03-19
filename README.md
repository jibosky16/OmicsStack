# OmicsStack

OmicsStack is an open-source **R Shiny** platform and **Dockerized** application for unified single- and multi-omics analysis.  
It combines preprocessing, statistics, machine learning, visualization, and biological interpretation in one interface, with dedicated modules for **glycomics**, **metabolomics**, and **multi-omics integration**.  
An embedded context-aware AI assistant (**Jibosky**) provides real-time guidance for analysis decisions and interpretation.

---

## Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Analysis Modules](#analysis-modules)
- [Built-In AI Assistant](#built-in-ai-assistant)
- [Quick Start](#quick-start)
  - [Option A: Run with Docker (Recommended)](#option-a-run-with-docker-recommended)
  - [Option B: Run Locally with R](#option-b-run-locally-with-r)
- [Data Requirements](#data-requirements)
- [Outputs and Export](#outputs-and-export)
- [Performance and Reproducibility](#performance-and-reproducibility)
- [Deployment](#deployment)
- [Troubleshooting](#troubleshooting)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [Citation](#citation)
- [License](#license)

---

## Overview

OmicsStack is designed for researchers who need an end-to-end platform for:

- Omics data ingestion and preprocessing
- Quality control and normalization workflows
- Feature discovery and model-driven biomarker prioritization
- Differential and enrichment analysis
- Specialized glycomics and metabolomics interpretation
- Integrative multi-omics analysis for cross-layer biological signals

The platform supports both interactive exploration and reproducible output generation, with downloadable plots/tables across modules.

---

## Key Features

- **Unified Omics Workflow**  
  From upload to interpretation in a single dashboard.

- **Interactive Data Processing**  
  Raw data review, normalization, missing-value handling, imputation comparison, and batch/signal-drift correction.

- **Dimensionality Reduction**  
  PCA, sPCA, UMAP, and t-SNE with customizable visual settings and export support.

- **Machine Learning Feature Selection**  
  LASSO/Elastic Net, Random Forest importance, permutation importance, RFE, and Boruta; includes enhanced stability analysis.

- **Advanced Downstream Analytics**  
  Heatmaps, distribution analysis, differential analysis, K-means clustering, correlation analysis, WGCNA network analysis, and intersection analysis (Venn/UpSet).

- **Biological Interpretation Modules**  
  ROC/PR biomarker evaluation and enrichment analysis (including pathway/network-oriented views).

- **Domain-Specific Modules**  
  Dedicated glycomics and metabolomics workflows with tailored visualization and interpretation tools.

- **Multi-Omics Integration**  
  Multi-dataset upload and integration-oriented analytics for cross-omics signal discovery.

- **Power Analysis**  
  Feature-level and global power summaries for study planning and sample-size decisions.

- **Integrated AI Guidance**  
  Context-aware assistant to help with interpretation, method selection, and analysis rationale.

---

## Analysis Modules

OmicsStack currently includes:

1. Data
2. Dimensionality Reduction
3. ML Feature Selection
4. Heatmaps
5. Distribution Analysis
6. Differential Analysis
7. K-Means Clustering
8. Feature Correlation
9. Network Analysis (WGCNA)
10. Intersection Analysis
11. ROC/PR Analysis
12. Enrichment Analysis
13. Glycomics
14. Metabolomics
15. Multi-Omics Integration
16. Power Analysis
17. Help & About

---

## Built-In AI Assistant

OmicsStack includes **Jibosky**, an embedded AI research assistant for:

- Explaining outputs in plain scientific language
- Suggesting appropriate next analyses
- Helping interpret significance, effect size, and model behavior
- Providing concise R-oriented guidance when needed

### Optional environment variables for AI backends

Set one or more of the following before launch:

- `OPENROUTER_API_KEY`
- `GEMINI_API_KEY`
- `ANTHROPIC_API_KEY`

If no API key is set, the core analytics platform still works; only AI responses are unavailable.

---

## Quick Start

### Option A: Run with Docker (Recommended)

#### 1) Build and start

```bash
docker compose up --build
```

#### 2) Open in browser

```text
http://localhost:3838/OmicsStack/
```

This launches Shiny Server and serves OmicsStack on port `3838`.

---

### Option B: Run Locally with R

#### 1) Clone the repository

```bash
git clone https://github.com/jibosky16/OmicsStack.git
cd OmicsStack
```

#### 2) Launch app

```r
source("app.R")
```

or

```r
shiny::runApp()
```

> On first run, required packages may be installed/loaded automatically.

---

## Data Requirements

OmicsStack accepts standard tabular omics input (e.g., CSV/Excel).  
Typical workflow assumptions:

- Features and samples are organized in matrix form
- Group/condition labels are provided or mappable
- Optional metadata can be included for richer comparisons/integration
- Missing values and zero handling are configurable in-app

A sample dataset is included to help validate setup and explore workflows quickly.

---

## Outputs and Export

Across modules, OmicsStack supports export of:

- Publication-ready plots (PNG/SVG where applicable)
- Result tables (CSV/Excel-style exports where supported)
- Module-specific summaries and statistics
- Saved intermediate results for downstream workflows

This enables reproducible reporting and easy handoff to collaborators.

---

## Performance and Reproducibility

OmicsStack includes several engineering features for scalable workflows:

- Lazy loading of heavy package groups
- Caching for repeated operations (e.g., conversion/enrichment-related steps)
- On-demand loading of organism annotation resources
- Built-in controls for plot dimensions, DPI, and consistent output formatting
- Session-oriented save/export patterns for iterative analysis

---

## Deployment

OmicsStack is container-ready and deployable to:

- Local Docker environments
- VM/server-based Shiny Server
- Managed Shiny hosting services

A ShinyApps.io deployment has been used in practice (URL can be shared here if public).

---

## Troubleshooting

- **App does not start in Docker**  
  Ensure Docker Engine and Compose are installed; rerun `docker compose up --build`.

- **Port conflict on 3838**  
  Change host port mapping in compose config (e.g., `8080:3838`).

- **AI assistant unavailable**  
  Confirm at least one API key environment variable is set and valid.

- **Heavy analyses are slow**  
  Start with filtered feature sets, adjust thresholds, and use module-level subset controls.

- **Package installation issues (local R)**  
  Update R to a recent version and ensure system dependencies for Bioconductor packages are available.

---

## Roadmap

Planned and ongoing directions include:

- Expanded multi-omics integration workflows
- Additional annotation/database connectors
- Enhanced model explainability and validation views
- More guided reproducibility/reporting templates
- Continued performance optimization for large datasets

---

## Contributing

Contributions are welcome.

Suggested contribution flow:

1. Fork the repository
2. Create a feature branch
3. Implement and test changes
4. Open a pull request with a clear summary and reproducible example

For substantial changes, open an issue first to discuss scope and design.

---

## Citation

If OmicsStack supports your research, please cite the repository and release version used in your analysis.

**Official citation:**

>Fowowe M, Daramola O, Oluokun A, Sandilya V, Onigbinde S & Mechref Y. *OmicsStack: An Open-Source Web Server and Docker Image Incorporating ML/AI for the Processing, Visualization and Integration of Single-Omics and Multi-Omics Datasets.*

---

## License

Add your preferred open-source license here (e.g., MIT, GPL-3.0, Apache-2.0).  
If you have not selected one yet, add a license file before public release clarity/compliance.
