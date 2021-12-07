# libraries ----------

# libraries ----------
rm(list = ls())
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
                 ggtext, patchwork, data.table)
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, data.table,
                 janitor, glue, here, ggtext, patchwork, ggpubr)

source("esir_ally.R")

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

end_date     <- "2021-06-30"
tmp_outname  <- "esir_lol_case_plot.pdf"

# load data ----------
obs <- fread("covid19india_national_counts_20211031.csv")[
  , `:=` (
    date     = as.Date(date),
    scenario = "Observed"
  )][
    date >= "2021-02-15"][]

get_pred <- function(x, r_0 = 2) {
  
  tmp_date <- as.Date(x, "%Y%m%d")
  tmp_4wk_casename <- glue("{tmp_date}_t6_r{r_0}")
  tmp_6wk_casename <- glue("{tmp_date}_t7_r{r_0}")
  tmp_8wk_casename <- glue("{tmp_date}_t8_r{r_0}")
  
  tmp_4wk_pred <- fread(glue("{tmp_4wk_casename}_data.txt"))[, date := as.Date(date)][date >= as.Date(x, "%Y%m%d")][, `:=` (scenario = "4 weeks", start_date = as.Date(x, "%Y%m%d"))][]
  tmp_6wk_pred <- fread(glue("{tmp_6wk_casename}_data.txt"))[, date := as.Date(date)][date >= as.Date(x, "%Y%m%d")][, `:=` (scenario = "6 weeks", start_date = as.Date(x, "%Y%m%d"))][]
  tmp_8wk_pred <- fread(glue("{tmp_8wk_casename}_data.txt"))[, date := as.Date(date)][date >= as.Date(x, "%Y%m%d")][, `:=` (scenario = "8 weeks", start_date = as.Date(x, "%Y%m%d"))][]
  
  rbindlist(list(
    tmp_4wk_pred,
    tmp_6wk_pred,
    tmp_8wk_pred
  ))
  
}

apr15 <- get_pred(x = "20210415")
mar30 <- get_pred(x = "20210330")
mar19 <- get_pred(x = "20210319")

# prepare data ----------
clean_prep <- function(x) {
  
  alt_end <- x[, unique(start_date)]
  
  none <- obs %>% select(c(date, daily_cases)) %>% rename(incidence = daily_cases) %>% 
    add_column(scenario = "No intervention") %>% filter(date <= end_date)
  wk4    <- obs %>% clean_scenario(p = x, stop_obs = alt_end, scen = "4 weeks", end_date = end_date)
  wk6    <- obs %>% clean_scenario(p = x, stop_obs = alt_end, scen = "6 weeks", end_date = end_date)
  wk8    <- obs %>% clean_scenario(p = x, stop_obs = alt_end, scen = "8 weeks", end_date = end_date)
  
  total <- none %>%
    add_row(wk4) %>% 
    add_row(wk6) %>% 
    add_row(wk8)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.25),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= end_date) %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "4 weeks", 
                     date >= alt_end - 6)) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "6 weeks", 
                     date >= alt_end - 6)) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "8 weeks", 
                     date >= alt_end - 6)) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "4 weeks", "6 weeks", 
                                                  "8 weeks"))) %>%
    filter(date <= end_date) %>%
    mutate(start_date = alt_end)
  
  return(total.smoothed.plot)
  
}

apr15_prepped <- clean_prep(x = apr15)
mar30_prepped <- clean_prep(x = mar30)
mar19_prepped <- clean_prep(x = mar19)

plot_data <- bind_rows(
  mar19_prepped,
  mar30_prepped,
  apr15_prepped
)

# plot -----------
colores <- c(
  "Observed" = "black",
  "4 weeks"  = pal_lancet()(3)[1],
  "6 weeks"  = pal_lancet()(3)[2],
  "8 weeks"  = pal_lancet()(3)[3]
)

lol_plot <- function(x) {
  
  tmp_plot_data <- plot_data %>% filter(start_date == x)
  
  tmp_plot_data %>%
    filter(date >= "2021-02-15" & date <= end_date) %>%
    ggplot(aes(x = date, y = fitted)) + 
    geom_line(aes(color = scenario), size = 1) +
    scale_color_manual(values = colores) +
    geom_vline(data = tmp_plot_data %>% 
                 group_by(scenario) %>% 
                 filter(date == min(date)) %>% 
                 dplyr::ungroup() %>% 
                 select(scenario, date) %>% 
                 filter(!(scenario %in% c("Observed", "No intervention"))), 
               aes(xintercept = date, color = scenario), 
               linetype = 'dashed') + 
    geom_label_repel(data = tmp_plot_data %>% 
                       group_by(scenario) %>% 
                       filter(fitted == max(fitted)) %>% 
                       dplyr::ungroup() %>% 
                       select(scenario, date, fitted) %>% 
                       filter(!(scenario %in% c("Observed", "No intervention"))), 
                     aes(x = date, 
                         y = fitted, 
                         label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                         color = scenario,
                         family = "Helvetica Neue"), 
                     nudge_y = 100000, 
                     nudge_x = -10, 
                     size = 4, 
                     show.legend  = FALSE, 
                     segment.size = 1) + 
    guides(color = guide_legend(nrow = 1)) + 
    labs(title    = glue("{format(as.Date(x), '%B %e, %Y')}"),
         y        = "Daily cases",
         x        = "",
         subtitle = glue::glue("February 15, 2021 to {format(as.Date(end_date), '%B %e, %Y')}"),
         color    = "Date of intervention") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B") +
    theme_classic() +
    theme(
      text            = element_text(family = "Helvetica Neue"),
      axis.text.x     = element_text(size = 11, vjust = 0.5),
      axis.text.y     = element_text(size = 11),
      axis.title.x    = element_text(size = 11, face = "bold"),
      axis.title.y    = element_text(size = 11, face = "bold"),
      legend.text     = element_text(size = 10),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title      = element_text(size = 14, face = "bold"),
      plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
      plot.caption    = element_markdown(size = 10, hjust = 0)
    )
  
}

mar19_plot <- lol_plot(x = "2021-03-19")
mar30_plot <- lol_plot(x = "2021-03-30") + theme(legend.position = "none")
apr15_plot <- lol_plot(x = "2021-04-15") + theme(legend.position = "none")

leg <- as_ggplot(get_legend(mar19_plot))
mar19_plot <- mar19_plot + theme(legend.position = "none")

patched <- mar19_plot / mar30_plot / apr15_plot / leg

full_plot <- patched +
  plot_layout(ncol = 1, heights = c(3, 3, 3, 1)) &
  plot_annotation(
    title    = "Effect of length of moderate lockdown on various start dates",
    subtitle = glue::glue("{format(as.Date('2021-02-15'), '%B %e, %Y')} to {format(as.Date(end_date), '%B %e, %Y')}"),
   # caption  = glue::glue("**\uA9 COV-IND-19 Study Group**"),
    tag_levels = list(c("A", "B", "C", ""))
  )  &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

# save output ----------
ggsave(filename =  glue("figure s9.pdf"),
       plot     = full_plot,
       height   = 10,
       width    = 10,
       units = "in", device = cairo_pdf)
