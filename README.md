# The analysis stage: a difference-in-differences lab

Lab for the POL30860 Advanced Seminar in Politics, University College Dublin (Week 10, 10 November 2026).

**Run it in your browser, no install or account needed:** [www.elisadamico.net/analysis-stage-lab](https://www.elisadamico.net/analysis-stage-lab/)

The lab uses the replication data from D'Amico, "Difference-in-Differences Estimation: A Quasi-Experimental Approach to Sustainable Development Interventions", *Global Environmental Politics*. It asks what happens to conflict in places that get a large hydropower dam, using the Callaway and Sant'Anna estimator from the paper.

## Running it in RStudio or Posit Cloud instead

To run R on your own computer, install [R](https://cran.r-project.org/) first, then [RStudio Desktop](https://posit.co/download/rstudio-desktop/). Both are free. [Posit Cloud](https://posit.cloud/) runs RStudio in a browser.

1. Download `lab.R` (or use **Download script** in the browser lab to keep your answers). It reads the data from this repository, so you only need an internet connection. In Posit Cloud you can also choose **New Project → New Project from Git Repository** and paste this page's address.
2. Install the packages once. The lab needs version 2.3.0 of `did`, because newer versions refuse this panel (some grid-cell years appear twice):
   ```r
   install.packages(c("dplyr", "ggplot2", "BMisc", "DRDID", "pbapply", "fastglm", "dreamerr"))
   install.packages("https://cran.r-project.org/src/contrib/Archive/did/did_2.3.0.tar.gz",
                    repos = NULL, type = "source")
   ```
3. Open `lab.R` and run it one line at a time (Ctrl + Enter on Windows, Cmd + Enter on Mac).

## Files

| File | What it is |
|---|---|
| `lab.R` | The lab script, in steps, with questions |
| `tools.R` | Small helper functions the lab loads with `source()`: `explore()`, `run_did()`, `save_run()` |
| `data/hydropower_conflict.csv` | One row per grid cell per year, 1989–2024, for the 8 countries in the paper |
| `data/dams.csv` | One row per large dam (75 MW or more) in those grid cells, with status and start year. Step 1 builds the treatment from it |
| `index.html` | The in-browser version (R runs in your browser through webR) |
| `stub/` | A small stand-in for the `bigmemory` package, which has no browser build and is not used by this lab |

## Data and credit

- Paper replication data: Harvard Dataverse, [doi:10.7910/DVN/FGCPNU](https://doi.org/10.7910/DVN/FGCPNU)
- Conflict: [UCDP Georeferenced Event Dataset](https://ucdp.uu.se/), Uppsala Conflict Data Program
- Hydropower: [Global Hydropower Tracker](https://globalenergymonitor.org/projects/global-hydropower-tracker/), Global Energy Monitor
- `first_disaster` (an alternative treatment, Step 3 and Challenge E): year a grid cell was first hit by a climate disaster with coordinates in [EM-DAT](https://www.emdat.be/) (CRED, UCLouvain), 2001–2020
- Estimator: Callaway and Sant'Anna (2021), R package [did](https://bcallaway11.github.io/did/)
- R in the browser: [webR](https://docs.r-wasm.org/webr/latest/)
