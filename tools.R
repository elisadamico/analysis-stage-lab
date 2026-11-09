# Helper functions for the analysis stage lab (POL30860).
# lab.R loads this file with source(). You do not need to read it,
# but you are welcome to.

library(did)
library(dplyr)
library(ggplot2)

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
    filter(year <= 2023) %>%
    mutate(group = if_else(first_treated > 0, paste("cells that ever get a", what),
                           paste("cells that never get a", what))) %>%
    group_by(year, group) %>%
    summarise(average = mean(.data[[outcome]]), .groups = "drop")
  in_range <- yrs$first_treated[yrs$first_treated >= min(x$year)]
  cols <- setNames(c("#00b5c8", "#e0559a"),
                   c(paste("cells that ever get a", what), paste("cells that never get a", what)))
  print(ggplot(avg, aes(year, average, colour = group)) + geom_line(linewidth = 1) +
          geom_vline(xintercept = in_range, linetype = "dotted", colour = "grey50") +
          scale_colour_manual(values = cols) +
          labs(title = paste0(country_name, ": raw averages, ", gsub("_", " ", outcome)),
               subtitle = paste0("Dotted lines: years a cell got its first ", what,
                                 if (any(yrs$first_treated < min(x$year))) paste0(" (years before ", min(x$year), " not shown)") else "",
                                 "."),
               x = NULL, y = paste("average", gsub("_", " ", outcome), "per cell"), colour = NULL) +
          theme_minimal() + theme(legend.position = "bottom", plot.subtitle = element_text(size = 9, colour = "grey40")))
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
