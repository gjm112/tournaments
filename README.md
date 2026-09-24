# Assessing Ranking Fidelity of Competition Structures

Research project evaluating how well different tournament and competition formats recover the true underlying ranking of teams. Simulations are run under the Bradley–Terry model (and related strength distributions), and formats are compared using **weighted mutual information** and related rank-correlation metrics.

**Authors:** Zachary Culp, Josie Peterburs, Ryan P. A. McShane, Gregory J. Matthews  
**Affiliations:** Loyola University Chicago; The University of Chicago

---

## Overview

Competition structures—round robin, single elimination, double elimination, group stage, stepladder, repechage, and hybrids—produce different amounts of information about team strength. This repository contains:

- Simulation code for many tournament formats
- Metrics for comparing true ranks to tournament outcomes (weighted Kendall tau, mutual information, salient-rank weights)
- Manuscript drafts and a JASA-formatted Quarto version
- A CMSAC conference presentation
- An NFL case study applying estimated team strengths to playoff structures

---

## Getting Started

### Requirements

- [R](https://cran.r-project.org/) (≥ 4.x recommended)
- [RStudio](https://posit.co/download/rstudio/) or any R IDE
- [Quarto](https://quarto.org/) (for `.qmd` documents)
- LaTeX distribution such as [TinyTeX](https://yihui.org/tinytex/) (for PDF output)

### Setup

1. Clone or download this repository.
2. Open `tournaments.Rproj` in RStudio (sets the project root as the working directory).
3. Install required R packages:

```r
install.packages(c(
  "tidyverse", "combinat", "ggplot2", "knitr", "rmarkdown",
  "kableExtra", "future", "future.apply", "progressr"
))
```

4. For JASA manuscript rendering, ensure the Quarto JASA extension is available (included under `_extensions/` and `manuscriptTAS/_extensions/`).

---

## Project Structure

```
tournaments/
├── Tournament Simulations/   # Core simulation engines and analysis notebooks
├── Metric Creation/          # Mutual information curves, swap metrics, evaluation plots
├── Weighting Function/       # Weighted Kendall tau and salient-rank weight definitions
├── Potential Case Study (NFL)/  # NFL 2010/2023 data and playoff simulations
├── manuscript/               # Main paper (R Markdown / LaTeX)
├── manuscriptTAS/            # JASA-formatted Quarto manuscript
├── CMSAC_presentation/       # Conference slides (Beamer + Reveal.js)
├── Images/                   # Shared figures and plotting scripts
├── R Data Files/             # Saved simulation results (.RData)
├── library/                  # PDF literature for reference
├── Meeting Notes/            # Research meeting notes and literature summaries
├── TAS template/             # JASA submission template (starter files)
└── _extensions/              # Quarto journal extensions (JASA)
```

### Folder Guide

| Folder | Contents |
|--------|----------|
| **Tournament Simulations** | `simulations.qmd` (main pipeline), format-specific scripts (`round_robin.qmd`, `double_elimination.qmd`, `repechage.R`, etc.) |
| **Metric Creation** | Weighted mutual information computation and comparison plots |
| **Weighting Function** | `weighted_kendall.R`, `partial_kendall.R` — salient-rank weighting schemes |
| **Potential Case Study (NFL)** | Real NFL season data, estimated θ values, playoff structure comparison |
| **manuscript** | Primary paper source: `paper.Rmd` / `paper.tex` |
| **manuscriptTAS** | JASA submission draft: `tournamentsgit.qmd` |
| **CMSAC_presentation** | Slide decks and presentation assets |
| **Images** | Cross-project figures (e.g. tournament comparison plots) |
| **R Data Files** | Large saved objects (`curves_data.RData`, `mi_results.RData`) — gitignored / LFS |
| **library** | Reference PDFs (Bradley–Terry, Kendall & Smith, tournament design, etc.) |

---

## Key Workflows

### Run tournament simulations

Open and knit/render `Tournament Simulations/simulations.qmd`. This document defines parameter grids for each format, runs parallel simulations, and saves figures to `manuscript/images/`.

Individual formats can also be explored in their dedicated files (e.g. `round_robin.qmd`, `group_stage.qmd`).

### Generate mutual information comparison plots

Render `Metric Creation/mutual_information_curves.qmd`. Output figures are saved to `Images/`.

### Build the manuscript

- **Standard PDF:** knit `manuscript/paper.Rmd`
- **JASA format:** render `manuscriptTAS/tournamentsgit.qmd` with Quarto

### CMSAC presentation

Render files in `CMSAC_presentation/`. Presentations load precomputed results from `R Data Files/curves_data.RData`.

---

## Tournament Formats Implemented

- Round robin (1, 2, 4 rounds; partial designs)
- Single elimination (series length, third-place game, reseeding, bad seeding)
- Double elimination (true and consolation variants)
- Group stage (round robin → knockout)
- Stepladder and staged round robin
- Repechage
- Tennis-style ladder tournaments

Strengths are drawn from Normal, Uniform, Exponential, or equal-strength (coin-flip) distributions, or supplied manually.

---

## Metrics

- **Weighted mutual information** — information-theoretic measure of how well a format preserves true rank order, with salient weights emphasizing the top *k* teams
- **Weighted Kendall tau** — rank correlation with position-dependent weights
- **Probability curves** — P(correctly identifying the next-best team among remaining competitors)
- **Swap distance** — number of pairwise swaps needed to reconcile predicted and true orderings

---

## Data Notes

- `R Data Files/curves_data.RData` and `R Data Files/mi_results.RData` are excluded from git tracking (see `.gitignore`). Regenerate via the simulation and metric notebooks if missing locally.
- NFL CSV files live in `Potential Case Study (NFL)/`.

---

## Citation

If you use this work, please cite the manuscript (in preparation):

> Culp, Z., Peterburs, J., McShane, R. P. A., and Matthews, G. J. *Assessing Ranking Fidelity of Competition Structures via Weighted Mutual Information.*

---

## License

Contact the authors for licensing and usage questions.
