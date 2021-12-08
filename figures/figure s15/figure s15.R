rm(list = ls())
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
                 ggtext, patchwork, data.table)
librarian::shelf(data.table, janitor, ggplot2, patchwork, ggpubr)

source("figures/figure s10/esir_ally.R")

ggplot2::theme_set(
  ggplot2::theme_classic() +
    ggplot2::theme(
      text            = ggplot2::element_text(family = "Helvetica Neue"),
      legend.position = "top",
      legend.title    = ggplot2::element_blank(),
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
      plot.caption    = ggtext::element_markdown(hjust = 0)
    )
)

# specifications ----------
hosp_cap <- 1.9e6 / 1.366e9 * 10000 # beds per 10,000
icu_cap  <- 95000 / 1.366e9 * 10000 # beds per 10,000

hosp_rate <- (.0604 * .921) + (.2817 * (1 - 0.921))
icu_rate  <- 0.11 # conditional on hospitalization

line_col  <- "#138808"
unity_col <- "gray40"

start_date <- "2021-01-01"
end_date   <- "2021-06-30"



# data ------------
dat <- fread("figures/figure s10/covid19india_national_counts_20211031.csv")[, date := as.Date(date)]

dat <- dat[, active_cases := (total_cases - total_recovered) / 10000][
  , `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), scenario = "Observed", date = as.Date(date))][]

extracto <- function(x, start = as.Date("2021-02-19"), l_out = 199,
                     scen = "Tier II\n(February 19)", sp = 0.75,
                     correct = TRUE) {
  
  load(x)
  
  tmp <- data.table(
    date   = seq.Date(from = start, length.out = l_out, by = "day"),
    i_prop = colMeans(theta_pp[, , 2])
  )[, active_cases := (1.34e9 * i_prop) / 10000][]
  
  tmp_out <- rbindlist(
    list(
      dat[date < as.Date(start)],
      tmp[data.table::between(date, as.Date(start), as.Date(end_date))][, place := "India"]
    ), fill = TRUE
  )[,
    smooth_active := predict(loess(active_cases ~ as.numeric(date), span = sp))
  ][
    , `:=` (
      hosp_cases = smooth_active * hosp_rate,
      icu_cases  = smooth_active * hosp_rate * icu_rate,
      scenario   = scen
    )][, `:=` (hosp_cases = ifelse(hosp_cases < 0, 0, hosp_cases), icu_cases = ifelse(icu_cases < 0, 0, icu_cases))][date >= as.Date(start)][]
  
  if (correct == TRUE) {
    tmp_out[, hosp_cases := hosp_cases * dat[date == start][, last(hosp_cases)] / tmp_out[, first(hosp_cases)]]
    tmp_out[, icu_cases := icu_cases * dat[date == start][, last(icu_cases)] / tmp_out[, first(icu_cases)]]
  }
  
  return(tmp_out)
  
}

feb_19 <- extracto(
  x     = glue("figures/figure s10/2021-02-19_t2_r2_forecast_MCMC.RData"),
  start = as.Date("2021-02-19"),
  scen  = "Tier II\n(February 19)",
  sp    = 0.15
)

mar_13 <- extracto(
  x     = glue("figures/figure s10/2021-03-13_t3_r2_forecast_MCMC.RData"),
  start = as.Date("2021-03-13"),
  scen  = "Tier III\n(March 13)",
  sp    = 0.15
)

mar_19 <- extracto(
  x     = glue("figures/figure s10/2021-03-19_t4_r2_forecast_MCMC.RData"),
  start = as.Date("2021-03-19"),
  scen  = "Tier IV\n(March 19)",
  sp    = 0.15
)

mar_30 <- extracto(
  x     = glue("figures/figure s10/2021-03-30_t4_r2_forecast_MCMC.RData"),
  start = as.Date("2021-03-30"),
  scen  = "Tier IV\n(March 30)",
  sp    = 0.15
)

apr_15 <- extracto(
  x     = glue("figures/figure s10/2021-04-15_t4_r2_forecast_MCMC.RData"),
  start = as.Date("2021-04-15"),
  scen  = "Tier IV\n(April 15)",
  sp    = 0.15
)

# combine -----------
plot_dat <- rbindlist(
  list(
    dat[data.table::between(date, as.Date(start_date), as.Date(end_date))],
    feb_19,
    mar_13,
    mar_19,
    mar_30,
    apr_15
  ), fill = TRUE
)

cols <- c(
  "Observed"                                      = "#0b0c0c",
  "Moderate PHI\n(non-lockdown)\n(February 19)"   = "#e6811c",
  "Strengthened PHI\n(non-lockdown)\n(March 13)"  = "#e0101a",
  "Moderate lockdown\n(March 19)"                 = "#9900d1",
  "Moderate lockdown\n(March 30)"                 = "#9900d1",
  "Moderate lockdown\n(April 15)"                 = "#9900d1"
)

plot_dat <- plot_dat[
  scenario == "Tier II\n(February 19)", scenario := "Moderate PHI\n(non-lockdown)\n(February 19)"][
    scenario == "Tier III\n(March 13)", scenario := "Strengthened PHI\n(non-lockdown)\n(March 13)"][
      scenario == "Tier IV\n(March 19)", scenario := "Moderate lockdown\n(March 19)"][
        scenario == "Tier IV\n(March 30)", scenario := "Moderate lockdown\n(March 30)"][
          scenario == "Tier IV\n(April 15)", scenario := "Moderate lockdown\n(April 15)"
        ][]

lts <- c(
  "Observed" = "solid",
  "Moderate PHI\n(non-lockdown)\n(February 19)" = "solid",
  "Strengthened PHI\n(non-lockdown)\n(March 13)" = "solid",
  "Moderate lockdown\n(March 19)" = "solid",
  "Moderate lockdown\n(March 30)" = "dashed",
  "Moderate lockdown\n(April 15)" = "dotted"
)

hosp_plot <- plot_dat %>% 
  ggplot(aes(x = date, y = hosp_cases, group = scenario)) +
  geom_ribbon(data = plot_dat[hosp_cases > hosp_cap], aes (x = date, ymax = hosp_cases), ymin = hosp_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = hosp_cap, color = unity_col, size = 1) +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, show.legend = TRUE) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  annotate(geom = "text", label = glue::glue("{format(round(hosp_cap, 1), nsmall = 1)} hospital beds per 10,000"),
           x = as.Date(start_date), y = hosp_cap + 2, hjust = 0, color = unity_col, fontface = "bold") + 
  labs(
    title = "Hospital needs",
    x = "Date",
    y = "Hospitalized cases (per 10,000)"
  ) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Helvetica Neue"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

leg <- as_ggplot(get_legend(hosp_plot))

hosp_plot <- hosp_plot + theme(legend.position = "none")

icu_plot <- plot_dat %>% 
  ggplot(aes(x = date, y = icu_cases, group = scenario, color = scenario)) +
  geom_ribbon(data = plot_dat[icu_cases > icu_cap], aes (x = date, ymax = icu_cases), ymin = icu_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = icu_cap, color = unity_col, size = 1) +
  geom_line(aes(linetype = scenario), size = 1) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  annotate(geom = "text", label = glue::glue("{format(round(icu_cap, 1), nsmall = 1)} ICU beds per 10,000"),
           x = as.Date(start_date), y = icu_cap + .25, hjust = 0, color = unity_col, fontface = "bold") + 
  labs(
    title = "ICU needs",
    x = "Date",
    y = "ICU cases (per 10,000)"
  ) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Helvetica Neue"),
    legend.position = "none",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

patched   <- hosp_plot / icu_plot / leg
full_plot <- patched +
  plot_layout(ncol = 1, heights = c(3, 3, 1)) &
  plot_annotation(
    title    = "Estimated COVID-19 hospital and ICU bed requirement",
    subtitle = glue::glue("{format(as.Date(start_date), '%B %e, %Y')} to {format(as.Date(end_date), '%B %e, %Y')}"),
    #caption  = glue::glue("**\uA9 COV-IND-19 Study Group**"),
    tag_levels = list(c("A", "B", ""))
  )  &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

ggsave(
  filename = glue("figures/figure s15/figure s15.pdf"),
  plot = full_plot,
  width = 8, height = 9,
  device = cairo_pdf
)

