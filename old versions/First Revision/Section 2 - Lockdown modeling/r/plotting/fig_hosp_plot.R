library(data.table)
library(janitor)
library(ggplot2)
library(patchwork)
library(here)

f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

hosp_cap <- 1.9e6 / 1.366e9 * 10000 # beds per 10,000
icu_cap  <- 95000 / 1.366e9 * 10000 # beds per 10,000

hosp_rate <- (.0604 * .921) + (.2817 * (1 - 0.921))
icu_rate  <- 0.11 # conditional on hospitalization

line_col  <- "#138808"
# unity_col <- "#FF9933"
unity_col <- "gray40"

start_date <- "2021-01-01"
end_date   <- "2021-06-30"

dat <- fread("http://data.covid19india.org/csv/latest/case_time_series.csv",
             showProgress = FALSE)[, -c("Date")]
setnames(dat, "Date_YMD", "date")
setnames(dat, names(dat), make_clean_names(names(dat)))

dat <- dat[, active_cases := (total_confirmed - total_recovered) / 10000][
  , `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), scenario = "Observed", date = as.Date(date))][]

#### early intervention -----------
load(here("data", "early_intervention", "2021-01-02_20pct_smooth1_mcmc.RData"))

pred_dat <- data.table(
  date   = seq.Date(from = as.Date("2021-01-02"), length.out = 199, by = "day"),
  i_prop = colMeans(theta_pp[, , 2])
)[, active_cases := (1.34e9 * i_prop) / 10000]

pred_dat <- rbindlist(
  list(
    dat[date < as.Date(start_date)],
    pred_dat[data.table::between(date, as.Date(start_date), as.Date(end_date))]
  ), fill = TRUE
)[,
  smooth_active := predict(loess(active_cases ~ as.numeric(date), span = 0.75))
][
  , `:=` (
    hosp_cases = smooth_active * hosp_rate,
    icu_cases  = smooth_active * hosp_rate * icu_rate,
    scenario   = "Early non-lockdown\nintervention\n(January 1)"
  )][, `:=` (hosp_cases = ifelse(hosp_cases < 0, 0, hosp_cases), icu_cases = ifelse(icu_cases < 0, 0, icu_cases))][date >= as.Date(start_date)][]

#### early lockdown -----------
load(here("data", "early_lockdown", "2021-03-15_smooth1_mh_mcmc.RData"))

pred_dat_el <- data.table(
  date   = seq.Date(from = as.Date("2021-03-15"), length.out = 199, by = "day"),
  i_prop = colMeans(theta_pp[, , 2])
)[, active_cases := (1.34e9 * i_prop) / 10000]

pred_dat_el <- rbindlist(
  list(
    dat[date < as.Date("2021-03-15")],
    pred_dat_el[data.table::between(date, as.Date("2021-03-15"), as.Date(end_date))]
  ), fill = TRUE
)[,
  smooth_active := predict(loess(active_cases ~ as.numeric(date), span = 1))
][
  , `:=` (
    hosp_cases = smooth_active * hosp_rate,
    icu_cases  = smooth_active * hosp_rate * icu_rate,
    scenario   = "Moderate lockdown\n(March 15)"
  )][, `:=` (hosp_cases = ifelse(hosp_cases < 0, 0, hosp_cases), icu_cases = ifelse(icu_cases < 0, 0, icu_cases))][date >= as.Date("2021-03-15")][]

# combine -----------
plot_dat <- rbindlist(
  list(
    dat[data.table::between(date, as.Date(start_date), as.Date(end_date))],
    pred_dat,
    pred_dat_el
  ), fill = TRUE
)
####

cols <- c(
  "Observed"           = colores[["Observed"]][[1]],
  "Early non-lockdown\nintervention\n(January 1)" = colores[["MH Pre-lock"]][[1]],
  "Moderate lockdown\n(March 15)"  = colores[["Moderate lockdown"]][[2]]
)

hosp_plot <- plot_dat |>
  ggplot(aes(x = date, y = hosp_cases, group = scenario, color = scenario)) +
  geom_ribbon(data = plot_dat[hosp_cases > hosp_cap], aes (x = date, ymax = hosp_cases), ymin = hosp_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = hosp_cap, color = unity_col, size = 1) +
  geom_line(size = 1) +
  scale_color_manual(values = cols) +
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
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

hosp_plot

icu_plot <- plot_dat |>
  ggplot(aes(x = date, y = icu_cases, group = scenario, color = scenario)) +
  geom_ribbon(data = plot_dat[icu_cases > icu_cap], aes (x = date, ymax = icu_cases), ymin = icu_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = icu_cap, color = unity_col, size = 1) +
  geom_line(size = 1) +
  scale_color_manual(values = cols) +
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
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

patched   <- hosp_plot / icu_plot
full_plot <- patched +
  plot_annotation(
    title    = "Estimated COVID-19 hospital and ICU bed need",
    subtitle = glue::glue("{format(as.Date(start_date), '%B %e, %Y')} to {format(as.Date(end_date), '%B %e, %Y')}"),
    caption  = glue::glue("**\uA9 COV-IND-19 Study Group**"),
    tag_levels = c("A")
  )  &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

cairo_pdf(filename = here("fig", "hospital_capacity_plot.pdf"), width = 8, height = 7)
print(full_plot)
dev.off()

