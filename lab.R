# =============================================================
# The analysis stage: a difference-in-differences lab
# POL30860 Advanced Seminar in Politics, 10 November 2026
# =============================================================
# Based on D'Amico, "Difference-in-Differences Estimation: A
# Quasi-Experimental Approach to Sustainable Development
# Interventions", Global Environmental Politics.
#
# Question: what happens to conflict in places that get a large
# hydropower dam (75 MW or more)?
#
# How to use this script
#   Run it one line at a time: Ctrl + Enter (Windows) or
#   Cmd + Enter (Mac). Work in pairs for Steps 0 to 5 (one laptop
#   between two is fine) and answer the QUESTIONS together. Step 6
#   is about your own thesis, so do it on your own.
#   Type your answers after each "# ANSWER:" line.
#   Keep the # at the start, so R treats your answer as a note.
#   Download your script at the end to keep your notes.
#
#   %>% is the "pipe". Read it as "and then": take the data, and
#   then filter it, and then count it...
# =============================================================

library(did)       # the Callaway and Sant'Anna estimator used in the paper
library(dplyr)     # for working with data: filter, arrange, group_by...
library(ggplot2)   # for plots


# -------------------------------------------------------------
# Load the data
# -------------------------------------------------------------
# These read the data straight from GitHub, so the script works
# anywhere with an internet connection (here, Posit Cloud, RStudio).

# 1. The panel: one row per hexagonal grid cell (about 140 km
#    across) per year, 1989 to 2024, for the 8 countries in the paper.
data <- read.csv("https://raw.githubusercontent.com/elisadamico/analysis-stage-lab/main/data/hydropower_conflict.csv")

# 2. The dams: one row per large dam (75 MW or more) in those
#    cells, from the Global Hydropower Tracker.
dams <- read.csv("https://raw.githubusercontent.com/elisadamico/analysis-stage-lab/main/data/dams.csv")

# No internet? If you downloaded the data folder, use these instead:
# data <- read.csv("data/hydropower_conflict.csv")
# dams <- read.csv("data/dams.csv")

# To see the data as a table, click the DATA tab in the bottom-right
# pane (or run View(data) in Step 0). In RStudio, View(data) opens it
# in a new tab.
#
# Columns in data:
#   country          one of 8 countries
#   hex_id           the grid cell
#   year             1989 to 2024
#   conflict_events  armed conflict events that year (UCDP)
#   civilian_deaths  civilian deaths that year (UCDP)
#   total_deaths     all deaths, UCDP "best" estimate
#   first_disaster   year the cell was first hit by a climate disaster
#                    with map coordinates in EM-DAT (0 = none recorded).
#                    Used only in Challenge E.
# Columns in dams:
#   hex_id, dam_name, country, capacity_mw, status, start_year


# =============================================================
# STEP 0. Look before you analyse
# =============================================================
View(data)
View(dams)

data %>% count(country)                      # rows per country

# Each cell should appear once per year. Does it?
data %>%
  count(hex_id, year) %>%
  filter(n > 1) %>%
  nrow()

# QUESTIONS
#  0a. Some cell-years appear more than once (cells on a border
#      belong to two countries). What would you do, and what would
#      you write down about it?
#      ANSWER:


# =============================================================
# STEP 1. Build the treatment
# =============================================================
# The dams data includes each dam's status. What statuses are there?
dams %>% count(status)

# The estimator needs ONE number per cell: the year it was first
# treated, with 0 for cells that never get a dam. The dams data
# has one row per dam, and some cells have several. So:
first_dam <- dams %>%
  arrange(hex_id, start_year) %>%        # sort each cell's dams by year
  # filter(status == "operating") %>%    # a choice: see question 1b
  group_by(hex_id) %>%
  slice(1) %>%                           # keep each cell's first dam
  ungroup() %>%
  select(hex_id, first_treated = start_year)   # keep and rename

data <- data %>%
  left_join(first_dam, by = "hex_id") %>%      # merge back onto the panel
  mutate(first_treated = coalesce(first_treated, 0))   # no dam = 0

# How many cells in each country ever get a dam?
data %>%
  filter(first_treated > 0) %>%
  distinct(country, hex_id) %>%
  count(country)

# QUESTIONS
#  1a. Which countries have very few treated cells? Why might that
#      worry you before you run anything?
#      ANSWER:
#  1b. Look at the statuses you counted above. Should a dam that is only "announced" or "under construction"
#      count as treated? Look at their start years too. What would
#      change if you removed the # in front of the filter line and
#      ran Step 1 again?
#      ANSWER:


# =============================================================
# STEP 2. Reproduce a published result
# =============================================================
# This is the exact specification from the paper.
ph <- data %>% filter(country == "Philippines")

set.seed(1)
gt <- att_gt(
  yname = "conflict_events",        # outcome
  tname = "year",                   # time
  idname = "hex_id",                # unit
  gname = "first_treated",          # year first treated (0 = never)
  data = ph,
  control_group = "notyettreated",  # compare with cells not yet treated
  anticipation = 1,                 # effects may start a year early
  bstrap = TRUE,                    # wild bootstrap
  panel = FALSE
)

es <- aggte(gt, type = "dynamic", na.rm = TRUE)   # event study
es                                                # overall ATT at the top
ggdid(es)                                         # the event-study plot

# Published in the paper (Philippines): ATT -1.007, SE 0.185

# QUESTIONS
#  2a. Did you reproduce it? (The SE can differ a little: the
#      bootstrap is random.)
#      ANSWER:
#  2b. Put the overall ATT into one plain-English sentence.
#      ANSWER:
#  2c. Look at the plot BEFORE year 0. What should it look like if
#      the parallel trends assumption holds? Does it?
#      ANSWER:


# =============================================================
# Tools for the rest of the lab
# =============================================================
# Run this block once (put your cursor on "explore <- function"
# and press Ctrl/Cmd + Enter, then do the same for save_run and
# run_did). You do not need to read it closely. From here on, YOU
# make the choices.

# explore() shows you a country before you estimate anything:
# how many cells get a dam, when, and what conflict looked like
# over time in cells that get a dam versus cells that never do.
explore <- function(country_name, outcome = "conflict_events", treatment = "first_treated") {
  x <- data %>%
    filter(country == country_name) %>%
    mutate(first_treated = as.numeric(.data[[treatment]]))   # did needs numbers
  what <- if (treatment == "first_treated") "dam" else "disaster"
  cells <- x %>% distinct(hex_id, first_treated)
  n_treated <- sum(cells$first_treated > 0)
  cat(sprintf("\n%s: %d grid cells, %d get a %s, %d never do\n", country_name,
              nrow(cells), n_treated, what, sum(cells$first_treated == 0)))
  yrs <- cells %>% filter(first_treated > 0) %>% count(first_treated)
  cat(sprintf("Year of first %s (number of cells):", what),
      paste0(yrs$first_treated, " (", yrs$n, ")", collapse = ", "), "\n")
  # cells treated before the data start (or the year after, with one
  # year of anticipation) have no "before" period, so did drops them
  early <- sum(cells$first_treated > 0 & cells$first_treated <= min(x$year) + 1)
  if (early > 0)
    cat(sprintf("Note: %d of these cells were treated before the data start, so the analysis can only use %d.\n",
                early, n_treated - early))
  avg <- x %>%
    mutate(group = if_else(first_treated > 0, "treated", "never treated")) %>%
    group_by(year, group) %>%
    summarise(average = mean(.data[[outcome]]), .groups = "drop")
  print(ggplot(avg, aes(year, average, colour = group)) + geom_line(linewidth = 1) +
          geom_vline(xintercept = yrs$first_treated, linetype = "dotted", colour = "grey50") +
          scale_colour_manual(values = c("treated" = "#00b5c8", "never treated" = "#e0559a")) +
          labs(title = paste0(country_name, " - raw averages (dotted lines = years of first ", what, ")"),
               x = NULL, y = paste("average", outcome), colour = NULL) +
          theme_minimal() + theme(legend.position = "bottom"))
  invisible(x)
}

# save_run() prints a one-line summary of a result and adds it to
# my_results, so you can compare your runs side by side.
my_results <- data.frame()
add_result <- function(es, country_name, outcome, treatment, control, anticipation, years) {
  lo <- es$overall.att - 1.96 * es$overall.se
  hi <- es$overall.att + 1.96 * es$overall.se
  cat(sprintf("\n%s | %s | treatment: %s | control: %s | anticipation: %d | %s\n",
              country_name, outcome, treatment, control, anticipation, years))
  cat(sprintf("Overall ATT %+.3f  (SE %.3f, 95%% interval %+.3f to %+.3f)%s\n",
              es$overall.att, es$overall.se, lo, hi,
              if (lo > 0 | hi < 0) "  <- interval excludes zero" else ""))
  my_results <<- bind_rows(my_results, tibble(
    country = country_name, outcome, treatment, control, anticipation, years,
    ATT = round(es$overall.att, 3), SE = round(es$overall.se, 3),
    excludes_zero = lo > 0 | hi < 0))
}
save_run <- function(es) {
  add_result(es, my_country, my_outcome, "first_treated", my_control,
             my_anticipation, "1989-2024")
}

# run_did() does everything Step 4 does in one line: picks the
# country (and, if you like, years), runs att_gt() and aggte(),
# saves the run and draws the plot. Handy for the Step 5 challenges.
run_did <- function(country_name, outcome = "conflict_events", control = "notyettreated",
                    anticipation = 1, first_year = 1989, last_year = 2024, plot = TRUE,
                    treatment = "first_treated") {
  x <- data %>%
    filter(country == country_name, year >= first_year, year <= last_year) %>%
    mutate(first_treated = as.numeric(.data[[treatment]]))   # did needs numbers
  set.seed(1)
  gt <- att_gt(yname = outcome, tname = "year", idname = "hex_id", gname = "first_treated",
               data = x, control_group = control, anticipation = anticipation,
               bstrap = TRUE, panel = FALSE)
  es <- aggte(gt, type = "dynamic", na.rm = TRUE)
  add_result(es, country_name, outcome, treatment, control, anticipation,
             paste0(first_year, "-", last_year))
  if (plot) print(ggdid(es) + ggtitle(paste(country_name, outcome)))
  invisible(es)
}

# Published estimates (conflict events, paper's choices):
#   Philippines -1.007   Venezuela -2.509   Sudan +7.324   Myanmar +2.413
#   Pakistan   -19.592   Ethiopia +12.574   Mali  -1.981   Colombia -4.806
# (The paper uses 1990 onwards for Sudan and years up to 2020 for Myanmar.)
# Speed in the browser: Philippines, Venezuela and Mali take seconds,
# Colombia about half a minute, Myanmar, Pakistan and Ethiopia up to
# a minute.


# =============================================================
# STEP 3. Explore before you estimate
# =============================================================
# Look at at least THREE countries. Change the name and run again.
# Countries: Philippines, Venezuela, Sudan, Myanmar, Pakistan,
#            Ethiopia, Mali, Colombia
explore("Colombia")

# You can also look at other outcomes:
#   "conflict_events", "civilian_deaths", "total_deaths"
explore("Colombia", outcome = "civilian_deaths")

# QUESTIONS
#  3a. Which countries look like a clean test (many usable treated
#      cells, dams spread over time, similar trends before)? Which
#      do not?
#      ANSWER:
#  3b. Pick ONE country to analyse. Why that one?
#      ANSWER:


# =============================================================
# STEP 4. Your analysis
# =============================================================
# Fill in YOUR choices, then run the lines below them. This is the
# same model as Step 2, with your settings plugged in.
# Before you run it, write down what you expect.
my_country      <- "Colombia"         # any of the 8 countries
my_outcome      <- "conflict_events"  # "conflict_events", "civilian_deaths", "total_deaths"
my_control      <- "notyettreated"    # "notyettreated" or "nevertreated"
my_anticipation <- 1                  # 0 = no early effects, 1 = from a year before

x <- data %>% filter(country == my_country)   # keep only your country

set.seed(1)
gt <- att_gt(
  yname = my_outcome,
  tname = "year",
  idname = "hex_id",
  gname = "first_treated",
  data = x,
  control_group = my_control,
  anticipation = my_anticipation,
  bstrap = TRUE,
  panel = FALSE
)
es <- aggte(gt, type = "dynamic", na.rm = TRUE)
es
ggdid(es)
save_run(es)                          # adds this run to my_results

# Now change ONE setting above, predict, and run again. Do this at
# least three times. Every run is saved in my_results:
View(my_results)

# QUESTIONS
#  4a. Which of your changes moved the estimate most? Which only
#      changed how sure we are?
#      ANSWER:
#  4b. Which of your choices could a careful researcher defend?
#      Which would a reviewer push back on?
#      ANSWER:
#  4c. If you could report only ONE of your results, which one,
#      and what would go in an appendix?
#      ANSWER:


# =============================================================
# STEP 5. Challenge: pick one (or more)
# =============================================================
# No code is given. Use explore(), run_did(), View(data), dplyr, and
# anything else you like. Write what you did and what you found.
#
#  A. Fragile or solid? Find the country whose result changes most
#     under ONE defensible choice. What is that choice?
#  B. What is "conflict"? Compare events and deaths for two or three
#     countries. Where do they tell different stories, and why?
#     (Tip: data %>% filter(country == "Ethiopia") %>%
#     arrange(desc(total_deaths)) shows what drives the deaths.)
#  C. Few treated cells. How many treated cells does each country's
#     result actually use? (explore() tells you.) How much would
#     you trust a result built on one or two cells?
#  D. The paper's choices. The paper uses years up to 2020 for
#     Myanmar and from 1990 for Sudan. Run both with and without,
#     e.g. run_did("Myanmar", last_year = 2020).
#     Was each choice justified?
#  E. A different treatment: climate disasters. Same design, but the
#     "treatment" is a cell's first flood, storm or other climate
#     disaster in EM-DAT, for example:
#       explore("Philippines", treatment = "first_disaster")
#       run_did("Philippines", treatment = "first_disaster", first_year = 1990, last_year = 2021)
#     Careful: EM-DAT only gives map coordinates for about one event in
#     four, and only for 2001-2020. So some "never treated" cells were
#     probably hit too. What does that do to the comparison? Would you
#     trust these results? How would you measure the treatment better?
#
# Your code:


#  5a. Which challenge, what did you do, what did you find?
#      ANSWER:
#  5b. One honest sentence you would put in a paper.
#      ANSWER:


# =============================================================
# STEP 6. Is there a "treatment" in your thesis? (on your own)
# =============================================================
# No code. A treatment is anything that happens at a point in time
# to some units but not others: a policy, a project, an agreement,
# a shock. Some examples:
#   - US states raising the minimum wage in different years
#     (unit: state; outcome: employment)
#   - mobile phone coverage reaching different regions at
#     different times (unit: region; outcome: protest)
#   - the Brexit vote hitting all UK firms at once, but firms that
#     export more to the EU are more exposed (unit: firm; outcome:
#     investment)
#   - "Is civil disobedience ever justified?" No treatment here.
# Not every thesis has one. Normative or theoretical projects may not,
# and that is fine: then say what your analysis DOES rest on.
#
# Which kind of DiD would fit? A quick guide:
#   - Different units treated at DIFFERENT times (dams, minimum
#     wage rises) -> staggered DiD, like this lab (Callaway and
#     Sant'Anna, the did package).
#   - EVERYONE treated at the SAME time (one election, one global
#     shock) -> no "not yet treated" group exists. You need units that
#     are more and less exposed, and a classic 2x2 DiD or event study.
#   - Treatment switches ON and OFF (a policy reversed, then restored)
#     -> possible, but harder. Each switch needs its own comparison.
#   - Nothing like a treatment -> DiD is not the tool. That is fine.
#
#  6a. Does your research question have a treatment? If so, what
#      is the unit, and when does it happen?
#      ANSWER:
#  6b. Who would be the comparison group? Why are they comparable?
#      ANSWER:
#  6c. What would "parallel trends" mean in your case, and what
#      could break it?
#      ANSWER:
#  6d. If your project has no treatment: what is your single most
#      important analysis choice, and what alternative would a
#      careful reader suggest?
#      ANSWER:
#  6e. What result or evidence would make you change your conclusion?
#      ANSWER:


# -------------------------------------------------------------
# Done? Please give feedback (anonymous, about two minutes). Use the
# "Give feedback" button at the top of the page, or this link:
# https://docs.google.com/forms/d/e/1FAIpQLSdnv_2-59uCcJANiYm6V0aKmLC5sP3Fg4HR9Dl7oUeG3kxlEA/viewform
# -------------------------------------------------------------


# =============================================================
# AFTER CLASS: using this on your own data
# =============================================================
# To run R on your own computer, install both (free):
#   R:        https://cran.r-project.org/
#   RStudio:  https://posit.co/download/rstudio-desktop/
# Install R first, then RStudio. Or use Posit Cloud in a browser:
#   https://posit.cloud/
# Then install the packages once:
#   install.packages(c("did", "dplyr", "ggplot2"))
#
# If your thesis has a staggered treatment, this is all you need.
# Your data should have ONE row per unit per year, a numeric outcome,
# and a 0/1 column that is 1 from the year the treatment starts.
# Change the names in CAPITALS to your own column names.
#
# 1. Build "year first treated" from the 0/1 column, the same way
#    as Step 1 (0 = never treated):
# mydata <- read.csv("MY_FILE.csv")
# first <- mydata %>%
#   arrange(UNIT, YEAR) %>%
#   filter(TREATED == 1) %>%
#   group_by(UNIT) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(UNIT, first_treated = YEAR)
# mydata <- mydata %>%
#   left_join(first, by = "UNIT") %>%
#   mutate(first_treated = coalesce(first_treated, 0))
#
# 2. Estimate (the same model as Step 2):
# gt <- att_gt(yname = "OUTCOME", tname = "YEAR", idname = "UNIT",
#              gname = "first_treated", data = mydata,
#              control_group = "notyettreated", panel = TRUE)
# es <- aggte(gt, type = "dynamic", na.rm = TRUE)
# es
# ggdid(es)
#
# Things to check:
#   - UNIT must be a number. If yours is a name, make an id with
#     mydata <- mydata %>% mutate(UNIT_ID = as.numeric(factor(UNIT)))
#   - Once treated, always treated: if your 0/1 column goes back to
#     0, "first year treated" ignores that. Say so, or rethink.
#   - Units treated in the first year of your data have no "before"
#     period, so they are dropped (as in the Philippines).
#   - Use panel = TRUE and the current did package. This lab uses
#     panel = FALSE and an old version only because the paper's
#     data repeat some cell-years.
