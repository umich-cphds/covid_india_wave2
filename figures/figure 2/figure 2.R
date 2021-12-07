# libraries --------------
librarian::shelf(data.table, covid19india, tidyverse, glue, here, ally, patchwork, ggtext)
source("model/esir_ally.R")
source("model/r_profile.R")

# specs --------------
start_date <- as.Date("2021-02-15")
end_date   <- as.Date("2021-07-31")
cols       <- c("place", "date", "daily_cases", "total_cases")
line_col   <- "#138808"
unity_col  <- "gray40"
mark_dates <- c("2021-03-28", "2021-04-14", "2021-06-07")

cols_1 <- c(
  "Moderate PHI\n(non-lockdown)"     = colores4[["Tier 2"]],
  "Strengthened PHI\n(non-lockdown)" = colores4[["Tier 3"]],
  "Moderate lockdown"                = colores4[["Tier 4"]],
  "Strong lockdown"                  = colores4[["Observed"]]
)

cols_2 <- c(
  "Moderate CFR" = colores4[["Tier 3"]],
  "High CFR"     = colores4[["Tier 4"]],
  "Low CFR"      = colores4[["Tier 2"]]
)

# data --------------
data <- fread("model/covid19india_state_counts_20211031.csv")[place == "Maharashtra"][, date := as.Date(date)][, ..cols]

r_data <- covid19india::get_r0(data)

combo <- data.table::merge.data.table(
  data,
  r_data,
  by = c("place", "date"),
  all.x = TRUE
)[between(date, start_date, end_date)]

start_r <- mean(combo[between(date, as.Date("2021-03-28") - 7, as.Date("2021-03-28") - 1), r], na.rm = TRUE)

combo <- combo[date >= as.Date("2021-03-28"), pis := r / start_r][
  date >= as.Date("2021-03-28"), smooth_pis := predict(loess(pis ~ as.numeric(date), span = 1))][]

combo <- combo[, color := fcase(
  date < "2021-03-28", "#138808",
  between(date, as.Date("2021-03-28"), as.Date("2021-04-13")), cols_1[["Strengthened PHI\n(non-lockdown)"]],
  date >= "2021-04-14", cols_1[["Moderate lockdown"]]
)][]

pis <- fread("model/pi_schedule_extended.txt")[
  place %in% c("India", "Maharashtra", "Maharashtra early", "MH Pre-lock +20%")][
    , place := fcase(
      place == "India", "Strong lockdown",
      place == "Maharashtra", "Moderate lockdown",
      place == "Maharashtra early", "Strengthened PHI\n(non-lockdown)",
      place == "MH Pre-lock +20%", "Moderate PHI\n(non-lockdown)"
    )
  ][]

cfrs <- melt.data.table(fread("model/cfr_schedule_14day_lag.txt")[, .(
  date, "Moderate CFR" = cfr_mod_smooth, "High CFR" = cfr_high_smooth, "Low CFR" = cfr_low_smooth
)], id.vars = "date", variable.name = "Location", value.name = "CFR")[date <= end_date]

# plots ----------

## panel a: daily cases ------------
case_plot <- combo[daily_cases >= 0] |>
  ggplot(aes(x = date, y = daily_cases))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    case_plot <- case_plot + geom_vline(xintercept = as.Date(mark_dates[i]), size = 1, linetype = 2, color = "gray40")
  }
}

case_plot <- case_plot +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Daily COVID-19 case count in Maharashtra",
    x = "Date",
    y = "Daily case count"
  ) +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

## panel b: time-varying r plot -----------
r_plot <- combo[!is.na(r)] |>
  ggplot(aes(x = date, y = r))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    r_plot <- r_plot + geom_vline(xintercept = as.Date(mark_dates[i]), size = 1, linetype = 2, color = "gray40")
  }
}

r_plot <- r_plot +
  geom_hline(yintercept = 1, color = unity_col, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = line_col, alpha = 0.5) +
  geom_line(color = line_col, size = 1) +
  geom_point(size = 0.25, shape = 3) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Time-varying R in Maharashtra", x = "Date", y = "R(t)"
  ) +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

## panel c: pi plot ----------
pi_plot <- combo |>
  ggplot(aes(x = date, y = smooth_pis))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    pi_plot <- pi_plot + geom_vline(xintercept = as.Date(mark_dates[i]), size = 1, linetype = 2, color = "gray40")
  }
}

pi_plot <- pi_plot +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Example intervention schedule (\u03c0(t))",
    x = "Date",
    y = "\u03c0(t)"
  ) +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

## panel d: pi schedules ------------
pi_sched_plot <- pis |>
  ggplot(aes(x = nom, y = smooth_pis, group = place, color = place)) +
  geom_hline(yintercept = 1, size = 1, linetype = 2, color = "gray40") +
  geom_line(size = 1) +
  labs(
    title   = "Intervention schedules",
    x       = "Days since start of intervention",
    y       = "\u03c0(t)",
    caption = "**Note:** Dashed line represents no change to pi schedule"
  ) +
  scale_color_manual(values = cols_1) +
  theme_classic() +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

## panel e: cfr plot -----------
cfr_plot <- cfrs %>%
  ggplot(aes(x = date, y = CFR, group = Location, color = Location)) +
  geom_line(size = 1) +
  labs(
    title   = "CFR schedules",
    x       = "Date",
    y       = "Case fatality rate (CFR)",
    caption = glue::glue("Note: February 15, 2021 to {format(end_date, '%B %e, %Y')}")
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%B %Y") +
  scale_color_manual(values = cols_2) +
  theme_classic() +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

# patch! ------------


patch_v1     <- case_plot / r_plot / pi_plot / (pi_sched_plot + cfr_plot)
full_plot_v1 <- patch_v1 +
  plot_annotation(
  title    = "Derivation of \u03c0(t) schedules and \u03c0(t) and CFR schedules used",
  caption  = glue("**Abbrev:** CFR, case fatality rate<br>",
                  "**\uA9 COV-IND-19 Study Group**"),
  tag_levels = c("A")
) &
  theme(
    text              = element_text(family = "Helvetica Neue"),
    plot.title        = element_text(size = 14, face = "bold"),
    plot.subtitle     = element_text(size = 12, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 8, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 14, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

ggsave(
  filename = "figures/figure 2/figure_2_out.pdf",
  plot = full_plot_v1,
  width = 15,
  height = 10,
  device = cairo_pdf
)
