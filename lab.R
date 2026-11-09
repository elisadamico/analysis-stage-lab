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
#                    An alternative treatment (Step 3, Challenge E).
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
# This line loads three small tools I wrote for this lab. Run it once.
# (Curious how they work? Open tools.R on the GitHub page.)
source("https://raw.githubusercontent.com/elisadamico/analysis-stage-lab/main/tools.R")
# No internet? Use this instead:  source("tools.R")
#
#   explore("Colombia")    Look at a country BEFORE estimating anything
#                          (options: outcome = ..., treatment = ...)
#   run_did("Colombia")    The Step 2 model in one line (for Step 5)
#   save_run(es)           Adds a result to my_results (Step 4)
#   View(my_results)       All your runs, side by side

# Published estimates (conflict events, paper's choices):
#   Philippines -1.007   Venezuela -2.509   Sudan +7.324   Myanmar +2.413
#   Pakistan   -19.592   Ethiopia +12.574   Mali  -1.981   Colombia -4.806
# (The paper uses 1990 onwards for Sudan and years up to 2020 for Myanmar.)


# =============================================================
# STEP 3. Explore before you estimate
# =============================================================
# Before you run a model, look at what it will be built on.
# explore() prints how many cells get a dam and when, and plots
# average conflict over time in cells that get a dam (cyan) versus
# cells that never do (pink). Look for three things:
#   - How many treated cells can the model actually USE? (Cells
#     treated before the data start are dropped. It tells you.)
#   - Are the dam years spread out, or bunched together?
#   - BEFORE the dams, do the two lines move together? That is
#     what parallel trends needs.
explore("Colombia")

# Now explore differently. Change ONE thing at a time:
#   the country:  Philippines, Venezuela, Sudan, Myanmar, Pakistan,
#                 Ethiopia, Mali, Colombia
explore("Sudan")
#   the outcome:  "conflict_events", "civilian_deaths", "total_deaths"
explore("Sudan", outcome = "total_deaths")
#   the treatment: a cell's first climate disaster instead of its
#                  first dam (more on this in Challenge E)
explore("Philippines", treatment = "first_disaster")
# Look at at least THREE countries before you answer.

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

# ROBUSTNESS (also called SENSITIVITY) CHECKS
# Every analysis rests on choices: which outcome, which comparison
# group, whether effects can start early. Often more than one choice
# is defensible. A robustness check asks: if I make a DIFFERENT
# choice that I could also defend, does the result still hold?
#   - Robust: the sign stays the same and the size moves a little.
#   - Fragile: the sign flips, the size changes a lot, or the
#     interval now includes zero.
# A result that only holds under one exact set of choices is weak
# evidence, even if it has an asterisk. Change ONE thing at a time,
# so you know which choice moved it.
#
# Now change ONE setting above, predict, and run again. Do this at
# least three times. Every run is saved in my_results:
View(my_results)

# QUESTIONS
#  4a. Is your result robust or fragile? Which change moved the
#      estimate most, and which only changed how sure we are?
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
