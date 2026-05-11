# OmicsStack

OmicsStack is an open-source R Shiny platform and Dockerized application for unified single- and multi-omics analysis. 

It combines preprocessing, statistics, machine learning, visualization, and biological interpretation into a single interface, featuring dedicated modules for **glycomics**, **metabolomics**, and **multi-omics integration**. An embedded context-aware AI chatbot assistant provides real-time guidance for analysis decisions and biological interpretation.

---

## 🚀 Key Features

*   **Unified Omics Workflow**: Everything from raw data upload to downstream interpretation in a single dashboard.
*   **Interactive Data Processing**: Real-time review, normalization, missing-value handling, imputation comparison, and batch/signal-drift correction.
*   **Machine Learning Feature Selection**: Support for LASSO/Elastic Net, Random Forest importance, permutation importance, RFE, Boruta, and stability analysis.
*   **Advanced Downstream Analytics**: Differential analysis, heatmaps, distribution analysis, K-means clustering, correlation mapping, WGCNA network analysis, and intersection analysis (Venn/UpSet).
*   **Domain-Specific Modules**: Specialized workflows directly addressing glycomics and metabolomics interpretation.
*   **Multi-Omics Integration**: Multi-dataset upload and integration-oriented analytics for cross-omics signal discovery.
*   **Integrated AI Guidance**: Built-in, context-aware chatbot helper to explain methods, help with interpretation, and validate analysis rationale.

---

## 🏃 Getting Started (How to Run)

To run the application, you need [Docker](https://docs.docker.com/get-docker/) installed on your machine.

### Option 1: Using `docker run` (Quick Start)

Run the following command in your terminal to pull and start the OmicsStack container:

```bash
docker run -d -p 3838:3838 jibosky10/omicsstack:latest
```

Once the container is running and finished initializing, open your web browser and navigate to:
**[http://localhost:3838/OmicsStack](http://localhost:3838/OmicsStack)**

### Option 2: Using `docker-compose`

If you prefer using `docker-compose` for easy management, create a `docker-compose.yml` file with the following content:

```yaml
version: '3.8'

services:
  omicsstack:
    image: jibosky10/omicsstack:latest
    container_name: omicsstack-shiny
    ports:
      - "3838:3838"
    environment:
      # Optional: Provide your API keys for the embedded AI chatbot assistant
      - ANTHROPIC_API_KEY=your_anthropic_key_here
      - GEMINI_API_KEY=your_gemini_key_here
      - OPENROUTER_API_KEY=your_openrouter_key_here
    volumes:
      # Optional: Mount a local directory for persistent data storage
      - ./data:/srv/shiny-server/omics/data
    restart: unless-stopped
```

Once you have saved the file:
1. Open your terminal or command prompt.
2. Navigate to the folder where you saved the `docker-compose.yml` file.
3. Start the application by running (note: on newer Docker versions, you can use `docker compose` without the hyphen):
   ```bash
   docker-compose up -d
   ```
4. Once it finishes downloading and initializing, open your web browser and navigate to: **[http://localhost:3838/OmicsStack](http://localhost:3838/OmicsStack)**

*(To stop the application later, simply run `docker-compose down` in that same folder).*

---

## 📖 Learn More
For more detailed documentation, source code, data requirements, and contribution guidelines, please visit the official [OmicsStack GitHub Repository](https://github.com/jibosky16/OmicsStack).

----
*License: Check the GitHub repository for detailed licensing information.*