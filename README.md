# Applied Descriptive Analytics in Property Management: A Proof of Concept

## Overview
This repository contains a proof-of-concept analytical framework demonstrating the application of Operations Research (OR) and Descriptive Analytics (Data Science) to large-scale property management and maintenance operations. 

Using a high-volume proxy dataset (municipal 311 service requests), this project evaluates systemic performance, identifies logistical constraints.

## Key Methodologies

### 1. System Stability & Queueing Theory
*   **Survival Analysis (Kaplan-Meier & Cox Proportional-Hazards):** Corrects for survivorship bias in open ticket queues, calculating the operational velocity of distinct service departments.
*   **Backlog Velocity:** Applies Queueing Theory ($\lambda$ vs $\mu$) to evaluate macro-system equilibrium, identifying whether backlog accumulation is driven by seasonal demand or capacity deficits.

### 2. Spatial Distribution of Service (Logistical Constraints)
*   **Global Moran’s I:** Evaluates the spatial autocorrelation of Service Level Agreement (SLA) exception rates to prove that service timelines are deeply tied to physical topology.
*   **LISA (Local Indicators of Spatial Association):** Isolates specific geographic clusters of operational variance, identifying actionable logistical bottlenecks (e.g., infrastructure density, transit constraints, waterway separations) for targeted resource routing.

### 3. NLP & Generative AI Reliability
*   **Expected & Pointwise Mutual Information (EMI/NPMI):** Extracts statistically significant, high-density operational vocabulary from unstructured maintenance logs, filtering out administrative noise.
*   **Topological Signal Isolation (Hartigan's Dip Test):** Uses bimodality testing on NPMI distributions to mathematically establish the boundary between physical operational signals and baseline conversational boilerplate.
*   **Generative Stability Testing:** Evaluates the operational safety of deploying local LLMs for automated summarization. 

## Tech Stack
*   **Language:** R (via `tidyverse`, `tidytext`, `survival`, `spdep`, `diptest`) and Python
*   **Reporting:** Quarto (for dynamic, reproducible technical documentation)
*   **Generative AI:** Local LLM execution via Ollama (`llama3`), optimized for Apple Silicon hardware acceleration.

## Repository Structure
*   `/data/` - Contains the anonymized/cleaned proxy dataset and processed tabular outputs.
*   `/R/` - Core analytical scripts (e.g., pipeline execution, PMI extraction, survival modeling).
*   `/report/` - Quarto markdown files (`.qmd`) and generated HTML/PDF documentation.

## Getting Started

### Prerequisites
1. Install [R](https://cran.r-project.org/) and required packages (listed in `R/requirements.R`).
2. Install [Quarto](https://quarto.org/) to render the final report and diagrams.
3. Install [Ollama](https://ollama.com/) for local LLM inference. Ensure the `llama3` model is pulled and actively running on localhost (`http://localhost:11434`) before executing the NLP pipeline scripts.

### Execution
1. Start your local Ollama instance.
2. Run `R/generalized_seasonal_pipeline.R` to process the textual data and generate the NPMI LLM prompts.
3. Render the full analytical report by navigating to the `/report/` directory and running `quarto render main.qmd`.

## License
[MIT License](LICENSE)