# libraries ----------
#install.packages("librarian")
librarian::shelf(data.table, janitor, ggplot2, patchwork, ggpubr)
source("model/esir_ally.R")
source("model/r_profile.R")

# source scripts and functions ----------
# f <- list.files(here("src"))
# for (i in seq_along(f)) {source(here("src", f[i]))}

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
dat <- fread("model/covid19india_national_counts_20211031.csv")

dat <- dat[, active_cases := (total_cases - total_recovered) / 10000][
  , `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), scenario = "Observed", date = as.Date(date))][]

feb_19 <- fread("model/seir_results/plot_data/Tier2_A.csv")[, scenario := "Moderate PHI\n(non-lockdown)\n(February 19)"][, .(date = dates, active_cases = value / 10000, scenario)][, `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), date = as.Date(date))][date >= as.Date("2021-02-19")]
mar_13 <- fread("model/seir_results/plot_data/Tier3_A.csv")[, scenario := "Strengthened PHI\n(non-lockdown)\n(March 13)"][, .(date = dates, active_cases = value / 10000, scenario)][, `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), date = as.Date(date))][date >= as.Date("2021-03-13")]
mar_19 <- fread("model/seir_results/plot_data/Tier4_1_A.csv")[, scenario := "Moderate lockdown\n(March 19)"][, .(date = dates, active_cases = value / 10000, scenario)][, `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), date = as.Date(date))][date >= as.Date("2021-03-19")]
mar_30 <- fread("model/seir_results/plot_data/Tier4_2_A.csv")[, scenario := "Moderate lockdown\n(March 30)"][, .(date = dates, active_cases = value / 10000, scenario)][, `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), date = as.Date(date))][date >= as.Date("2021-03-30")]
apr_15 <- fread("model/seir_results/plot_data/Tier4_3_A (1).csv")[, scenario := "Moderate lockdown\n(April 15)"][, .(date = dates, active_cases = value / 10000, scenario)][, `:=` (hosp_cases = active_cases * hosp_rate, icu_cases = active_cases * (hosp_rate * icu_rate), date = as.Date(date))][date >= as.Date("2021-04-15")]

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
)[date <= as.Date("2021-06-30")]
####

cols <- c(
  "Observed"                                      = colores4[[1]],
  "Moderate PHI\n(non-lockdown)\n(February 19)"   = colores4[[2]],
  "Strengthened PHI\n(non-lockdown)\n(March 13)"  = colores4[[3]],
  "Moderate lockdown\n(March 19)"                 = colores4[[4]],
  "Moderate lockdown\n(March 30)"                 = colores4[[4]],
  "Moderate lockdown\n(April 15)"                 = colores4[[4]]
)

plot_dat <- plot_dat[
  scenario == "Tier II\n(February 19)", scenario := "Moderate PHI\n(non-lockdown)\n(February 19)"][
    scenario == "Tier III\n(March 13)", scenario := "Strengthened PHI\n(non-lockdown)\n(March 13)"][
      scenario == "Tier IV\n(March 19)", scenario := "Moderate lockdown\n(March 19)"][
        scenario == "Tier IV\n(March 30)", scenario := "Moderate lockdown\n(March 30)"][
          scenario == "Tier IV\n(April 15)", scenario := "Moderate lockdown\n(April 15)"
        ][, lt := "solid"][scenario == "Moderate lockdown\n(March 30)", lt := "dashed"][
          scenario == "Moderate lockdown\n(April 15)", lt := "dotted"
        ]

lts <- c(
  "Observed" = "solid",
  "Moderate PHI\n(non-lockdown)\n(February 19)" = "solid",
  "Strengthened PHI\n(non-lockdown)\n(March 13)" = "solid",
 "Moderate lockdown\n(March 19)" = "solid",
 "Moderate lockdown\n(March 30)" = "dashed",
 "Moderate lockdown\n(April 15)" = "dotted"
)

hosp_plot <- plot_dat |>
  ggplot(aes(x = date, y = hosp_cases, group = scenario)) +
  geom_ribbon(data = plot_dat[hosp_cases > hosp_cap], aes (x = date, ymax = hosp_cases), ymin = hosp_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = hosp_cap, color = unity_col, size = 1) +
  geom_line(aes(color = scenario, linetype = scenario), size = 1, show.legend = TRUE) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  # scale_linetype_identity() +
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

icu_plot <- plot_dat |>
  ggplot(aes(x = date, y = icu_cases, group = scenario, color = scenario)) +
  geom_ribbon(data = plot_dat[icu_cases > icu_cap], aes (x = date, ymax = icu_cases), ymin = icu_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = icu_cap, color = unity_col, size = 1) +
  geom_line(aes(linetype = scenario), size = 1) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  # scale_linetype_identity() +
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
    title    = "Estimated COVID-19 hospital and ICU bed need",
    subtitle = glue::glue("{format(as.Date(start_date), '%B %e, %Y')} to {format(as.Date(end_date), '%B %e, %Y')}"),
    tag_levels = list(c("A", "B", ""))
  )  &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

ggsave(
  filename = "figures/figure_s16/figure_s16.pdf",
  plot = full_plot,
  width = 8, height = 9,
  device = cairo_pdf
)
