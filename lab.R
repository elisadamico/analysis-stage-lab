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
#   Cmd + Enter (Mac). Work in pairs for Steps 0 to 4 (one laptop
#   between two is fine) and answer the QUESTIONS together. Step 5
#   is about your own thesis, so do it on your own.
#   Type your answers after each "# ANSWER:" line.
#   Keep the # at the start, so R treats your answer as a note.
#   Download your script at the end to keep your notes.
# =============================================================

library(did)       # the Callaway and Sant'Anna estimator used in the paper
library(ggplot2)   # for plots


# -------------------------------------------------------------
# Load the data
# -------------------------------------------------------------
# One row per hexagonal grid cell (about 140 km across) per year,
# 1989 to 2024, for the 8 countries analysed in the paper.
data <- read.csv("data/hydropower_conflict.csv")

# The did package needs the treatment years stored as decimal
# numbers, not whole-number integers. These two lines convert them.
data$first_treated  <- as.numeric(data$first_treated)
data$first_disaster <- as.numeric(data$first_disaster)

# To see the data as a table, click the DATA tab in the bottom-right
# pane (or run View(data) in Step 0). In RStudio, View(data) opens it in a new tab.
#
# Columns:
#   country          one of 8 countries
#   hex_id           the grid cell
#   year             1989 to 2024
#   first_treated    year the cell's first large dam was commissioned
#                    (0 = never got one in this period)
#   conflict_events  armed conflict events that year (UCDP)
#   civilian_deaths  civilian deaths that year (UCDP)
#   total_deaths     all deaths, UCDP "best" estimate
#   first_disaster   year the cell was first hit by a climate disaster
#                    with map coordinates in EM-DAT (0 = none recorded).
#                    Used only in Challenge E.


# =============================================================
# STEP 0. Look before you analyse
# =============================================================
View(data)

table(data$country)                                    # rows per country

# How many cells in each country ever get a dam?
treated <- unique(data[data$first_treated > 0, c("country", "hex_id")])
table(treated$country)

# Each cell should appear once per year. Does it?
sum(duplicated(data[, c("hex_id", "year")]))

# QUESTIONS
#  0a. Which countries have very few treated cells? Why might that
#      worry you before you run anything?
#      ANSWER: 
#  0b. Some cell-years appear more than once. What would you do,
#      and what would you write down about it?
#      ANSWER: 


# =============================================================
# STEP 1. Reproduce a published result
# =============================================================
# This is the exact specification from the paper.
ph <- data[data$country == "Philippines", ]

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
#  1a. Did you reproduce it? (The SE can differ a little: the
#      bootstrap is random.)
#      ANSWER: 
#  1b. Put the overall ATT into one plain-English sentence.
#      ANSWER: 
#  1c. Look at the plot BEFORE year 0. What should it look like if
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
explore <- function(country, outcome = "conflict_events", treatment = "first_treated") {
  x <- data[data$country == country, ]
  x$first_treated <- x[[treatment]]
  what <- if (treatment == "first_treated") "a dam" else "a disaster"
  cells <- unique(x[, c("hex_id", "first_treated")])
  cat(sprintf("\n%s: %d grid cells, %d get %s, %d never do\n", country,
              nrow(cells), sum(cells$first_treated > 0), what, sum(cells$first_treated == 0)))
  yrs <- table(cells$first_treated[cells$first_treated > 0])
  cat(sprintf("Year of first %s (number of cells):", sub("a ", "", what)),
      paste0(names(yrs), " (", yrs, ")", collapse = ", "), "\n")
  # cells whose dam came before the data start (or the year after, with
  # one year of anticipation) have no "before" period, so did drops them
  early <- sum(cells$first_treated > 0 & cells$first_treated <= min(x$year) + 1)
  if (early > 0)
    cat(sprintf("Note: %d of these cells were treated before the data start, so the analysis can only use %d.\n",
                early, sum(cells$first_treated > 0) - early))
  x$group <- ifelse(x$first_treated > 0, "treated", "never treated")
  avg <- aggregate(x[[outcome]], by = list(year = x$year, group = x$group), FUN = mean)
  print(ggplot(avg, aes(year, x, colour = group)) + geom_line(linewidth = 1) +
          geom_vline(xintercept = as.numeric(names(yrs)), linetype = "dotted", colour = "grey50") +
          scale_colour_manual(values = c("treated" = "#00b5c8", "never treated" = "#e0559a")) +
          labs(title = paste0(country, " - raw averages (dotted lines = years of first ", sub("a ", "", what), ")"),
               x = NULL, y = paste("average", outcome), colour = NULL) +
          theme_minimal() + theme(legend.position = "bottom"))
  invisible(x)
}

# save_run() prints a one-line summary of a result and adds it to
# my_results, so you can compare your runs side by side.
my_results <- data.frame()
save_run <- function(es) {
  lo <- es$overall.att - 1.96 * es$overall.se; hi <- es$overall.att + 1.96 * es$overall.se
  cat(sprintf("\n%s | %s | control: %s | anticipation: %d\n", my_country, my_outcome,
              my_control, my_anticipation))
  cat(sprintf("Overall ATT %+.3f  (SE %.3f, 95%% interval %+.3f to %+.3f)%s\n",
              es$overall.att, es$overall.se, lo, hi,
              if (lo > 0 | hi < 0) "  <- interval excludes zero" else ""))
  my_results <<- rbind(my_results, data.frame(
    country = my_country, outcome = my_outcome, treatment = "first_treated",
    control = my_control, anticipation = my_anticipation, years = "1989-2024",
    ATT = round(es$overall.att, 3), SE = round(es$overall.se, 3),
    excludes_zero = lo > 0 | hi < 0))
}

# run_did() does everything Step 3 does in one line: picks the
# country (and, if you like, years), runs att_gt() and aggte(),
# saves the run and draws the plot. Handy for the Step 4 challenges.
run_did <- function(country, outcome = "conflict_events", control = "notyettreated",
                    anticipation = 1, first_year = 1989, last_year = 2024, plot = TRUE,
                    treatment = "first_treated") {
  x <- data[data$country == country & data$year >= first_year & data$year <= last_year, ]
  x$first_treated <- x[[treatment]]
  set.seed(1)
  gt <- att_gt(yname = outcome, tname = "year", idname = "hex_id", gname = "first_treated",
               data = x, control_group = control, anticipation = anticipation,
               bstrap = TRUE, panel = FALSE)
  es <- aggte(gt, type = "dynamic", na.rm = TRUE)
  lo <- es$overall.att - 1.96 * es$overall.se; hi <- es$overall.att + 1.96 * es$overall.se
  cat(sprintf("\n%s | %s | treatment: %s | control: %s | anticipation: %d | %d-%d\n", country, outcome,
              treatment, control, anticipation, first_year, last_year))
  cat(sprintf("Overall ATT %+.3f  (SE %.3f, 95%% interval %+.3f to %+.3f)%s\n",
              es$overall.att, es$overall.se, lo, hi,
              if (lo > 0 | hi < 0) "  <- interval excludes zero" else ""))
  my_results <<- rbind(my_results, data.frame(
    country, outcome, treatment, control, anticipation, years = paste0(first_year, "-", last_year),
    ATT = round(es$overall.att, 3), SE = round(es$overall.se, 3),
    excludes_zero = lo > 0 | hi < 0))
  if (plot) print(ggdid(es) + ggtitle(paste(country, outcome)))
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
# STEP 2. Explore before you estimate
# =============================================================
# Look at at least THREE countries. Change the name and run again.
# Countries: Philippines, Venezuela, Sudan, Myanmar, Pakistan,
#            Ethiopia, Mali, Colombia
explore("Colombia")

# You can also look at other outcomes:
#   "conflict_events", "civilian_deaths", "total_deaths"
explore("Colombia", outcome = "civilian_deaths")

# QUESTIONS
#  2a. Which countries look like a clean test (many usable treated
#      cells, dams spread over time, similar trends before)? Which
#      do not?
#      ANSWER: 
#  2b. Pick ONE country to analyse. Why that one?
#      ANSWER: 


# =============================================================
# STEP 3. Your analysis
# =============================================================
# Fill in YOUR choices, then run the lines below them. This is the
# same model as Step 1, with your settings plugged in.
# Before you run it, write down what you expect.
my_country      <- "Colombia"         # any of the 8 countries
my_outcome      <- "conflict_events"  # "conflict_events", "civilian_deaths", "total_deaths"
my_control      <- "notyettreated"    # "notyettreated" or "nevertreated"
my_anticipation <- 1                  # 0 = no early effects, 1 = from a year before

x <- data[data$country == my_country, ]     # keep only your country

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
#  3a. Which of your changes moved the estimate most? Which only
#      changed how sure we are?
#      ANSWER: 
#  3b. Which of your choices could a careful researcher defend?
#      Which would a reviewer push back on?
#      ANSWER: 
#  3c. If you could report only ONE of your results, which one,
#      and what would go in an appendix?
#      ANSWER: 


# =============================================================
# STEP 4. Challenge: pick one (or more)
# =============================================================
# No code is given. Use explore(), run_did(), View(data), and anything
# else you like. Write what you did and what you found.
#
#  A. Fragile or solid? Find the country whose result changes most
#     under ONE defensible choice. What is that choice?
#  B. What is "conflict"? Compare events and deaths for two or three
#     countries. Where do they tell different stories, and why?
#     (Tip: sort a country's data by deaths to see what drives it.)
#  C. Few treated cells. How many treated cells does each country's
#     result actually use? (explore() tells you.) How much would
#     you trust a result built on one or two cells?
#  D. The paper's choices. The paper uses years up to 2020 for
#     Myanmar and from 1990 for Sudan. Run both with and without.
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


#  4a. Which challenge, what did you do, what did you find?
#      ANSWER: 
#  4b. One honest sentence you would put in a paper.
#      ANSWER: 


# =============================================================
# STEP 5. Is there a "treatment" in your thesis? (on your own)
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
#  5a. Does your research question have a treatment? If so, what
#      is the unit, and when does it happen?
#      ANSWER: 
#  5b. Who would be the comparison group? Why are they comparable?
#      ANSWER: 
#  5c. What would "parallel trends" mean in your case, and what
#      could break it?
#      ANSWER: 
#  5d. If your project has no treatment: what is your single most
#      important analysis choice, and what alternative would a
#      careful reader suggest?
#      ANSWER: 
#  5e. What result or evidence would make you change your conclusion?
#      ANSWER: 


# =============================================================
# AFTER CLASS: using this on your own data
# =============================================================
# If your thesis has a staggered treatment, this is all you need.
# Your data should have ONE row per unit per year, a numeric outcome,
# and a 0/1 column that is 1 from the year the treatment starts.
# Change the names in CAPITALS to your own column names.
#
# 1. Build "year first treated" from the 0/1 column (0 = never):
# mydata <- read.csv("MY_FILE.csv")
# first <- aggregate(YEAR ~ UNIT, data = mydata[mydata$TREATED == 1, ], FUN = min)
# names(first)[2] <- "first_treated"
# mydata <- merge(mydata, first, by = "UNIT", all.x = TRUE)
# mydata$first_treated[is.na(mydata$first_treated)] <- 0
# mydata$first_treated <- as.numeric(mydata$first_treated)
#
# 2. Estimate (the same model as Step 1):
# gt <- att_gt(yname = "OUTCOME", tname = "YEAR", idname = "UNIT",
#              gname = "first_treated", data = mydata,
#              control_group = "notyettreated", panel = TRUE)
# es <- aggte(gt, type = "dynamic", na.rm = TRUE)
# es
# ggdid(es)
#
# Things to check:
#   - UNIT must be a number. If yours is a name, make an id with
#     mydata$UNIT_ID <- as.numeric(factor(mydata$UNIT))
#   - Once treated, always treated: if your 0/1 column goes back to
#     0, "first year treated" ignores that. Say so, or rethink.
#   - Units treated in the first year of your data have no "before"
#     period, so they are dropped (as in the Philippines).
#   - Use panel = TRUE and the current did package. This lab uses
#     panel = FALSE and an old version only because the paper's
#     data repeat some cell-years.
