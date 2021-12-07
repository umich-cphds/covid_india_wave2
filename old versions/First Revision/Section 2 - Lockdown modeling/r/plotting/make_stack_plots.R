ally::libri(data.table, covid19india, tidyverse, glue, here, ally, patchwork)

make_r_count_stack_plots <- function(place, start_date = "2021-01-01", end_date = "2021-07-31", line_col = "#138808", unity_col = "#FF9933", mark_dates = NULL) {
  
  if (!(tolower(place) %in% c("india", "maharashtra", "mumbai", "delhi"))) {
    stop("assuming `place` is one of: 'india', 'maharashtra', 'mumbai', 'delhi'")
  }
  
  cols <- c("place", "date", "daily_cases", "total_cases")
  
  if (tolower(place) == "india") {
    data <- covid19india::get_nat_counts(useDT = TRUE)[, date := as.Date(date)][, ..cols]
  }
  if (tolower(place) == "maharashtra") {
    data <- covid19india::get_state_counts(useDT = TRUE)[place == "Maharashtra"][, date := as.Date(date)][, ..cols]
  }
  if (tolower(place) %in% c("mumbai", "delhi")) {
    data <- data.table::setDT(covid19india::get_district_counts())[district == place][, place := place][, ..cols][, date := as.Date(date)]
  }
  
  r_data <- data.table::setDT(covid19india::get_r0(data))
  
  comb <- data.table::merge.data.table(data[data.table::between(date, lower = as.Date(start_date), upper = as.Date(end_date))], r_data[data.table::between(date, lower = as.Date(start_date), upper = as.Date(end_date))], by = c("place", "date"), all.x = TRUE)
  
  case_plt <- comb[daily_cases >= 0] |>
    ggplot(aes(x = date, y = daily_cases))
  
  if (!is.null(mark_dates)) {
    for(i in seq_along(mark_dates)) {
      case_plt <- case_plt + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
    }
  }
  
  case_plt <- case_plt +
    geom_line(size = 1, color = line_col) +
    geom_point(size = 0.25, shape = 3) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B %Y") +
    labs(
      title = "Daily case count",
      x = "Date",
      y = "Daily case count"
    )
  
  r_plt <- comb[!is.na(r)] |>
    ggplot(aes(x = date, y = r))
  
  if (!is.null(mark_dates)) {
    for(i in seq_along(mark_dates)) {
      r_plt <- r_plt + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
    }
  }
  
  r_plt <- r_plt +
    geom_hline(yintercept = 1, color = unity_col, size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = line_col, alpha = 0.5) +
    geom_line(color = line_col, size = 1) +
    geom_point(size = 0.25, shape = 3) +
    scale_x_date(date_labels = "%B %Y") +
    labs(
      title = "Time-varying R", x = "Date", y = "R(t)"
    )
  
  if (tolower(place) != "maharashtrah") {
    location <- place
    pi_data <- fread("data_for_lockdown_extended.csv")[place == location]
    # pi_data  <- fread("pi_schedule_extended.txt")[place == location]
    start_r <- mean(pi_data[data.table::between(date, as.Date("2021-03-28") - 7, as.Date("2021-03-28") - 1)][, r_est], na.rm = TRUE)
    pi_data <- pi_data[data.table::between(date, as.Date("2021-03-28"), as.Date(end_date))][
      , date := as.Date(date)][
        , pis := r_est / start_r][
          , smooth_pis := predict(loess(pis ~ as.numeric(date), span = 1))
        ][]
    
    if (start_date < min(pi_data$date)) {
      pi_data <- rbindlist(list(
        pi_data,
        data.table(
          date = seq.Date(from = as.Date(start_date), to = (min(pi_data$date) - 1), by = "day"),
          place = location)
      ), fill = TRUE)[order(date)]
    }
  
    pi_plt <- pi_data %>% ggplot(aes(x = date, y = smooth_pis))
    
    if (!is.null(mark_dates)) {
      for(i in seq_along(mark_dates)) {
        pi_plt <- pi_plt + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
      }
    }
    
    pi_plt <- pi_plt +
      geom_line(size = 1, color = line_col) +
      geom_point(size = 0.25, shape = 3) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%B %Y") +
      labs(
        title = "Intervention schedule",
        x = "Date",
        y = "\u03c0(t)"
      )
    
    patched <- case_plt / r_plt / pi_plt
  
  } else {
    patched <- case_plt / r_plt
  }
  
  
  return(
    patched + patchwork::plot_annotation(title = glue::glue("{place}"),
                                         tag_levels = c("A")
    ) &
      theme(
        text              = element_text(family = "Lato"),
        plot.title        = element_text(size = 14, face = "bold"),
        plot.subtitle     = element_text(size = 12, hjust = 0, color = "gray40"),
        plot.caption      = element_markdown(size = 8, hjust = 0),
        plot.tag.position = c(0, 1),
        plot.tag          = element_text(size = 14, hjust = 0, vjust = 1, family = "Lato", face = "bold")
      )
    
  )
  
}

save_plot <- function(plot, path, w = 7, h = 5) {
  
  cairo_pdf(filename = path, width = w, height = h)
  print(plot)
  dev.off()
  
}

make_r_count_stack_plots(place = "Maharashtra", mark_dates = c("2021-02-18", "2021-03-28", "2021-04-14", "2021-06-07"), start_date = "2021-03-01", end_date = "2021-07-31") %>% save_plot(path = here("fig", "stack_plots", "maharashtra_stack_plot_marked.pdf"), h = 6)

make_r_count_stack_plots(place = "India") %>% save_plot(path = here("fig", "stack_plots", "india_stack_plot.pdf"))
 
make_r_count_stack_plots(place = "Maharashtra") %>% save_plot(path = here("fig", "stack_plots", "maharashtra_stack_plot.pdf"))

make_r_count_stack_plots(place = "Maharashtra", mark_dates = c("2021-02-18", "2021-03-28", "2021-04-14", "2021-06-07")) %>% save_plot(path = here("fig", "stack_plots", "maharashtra_stack_plot_marked.pdf"))

make_r_count_stack_plots(place = "Mumbai") %>% save_plot(path = here("fig", "stack_plots", "mumbai_stack_plot.pdf"))

make_r_count_stack_plots(place = "Mumbai", mark_dates = c("2021-03-28", "2021-04-14", "2021-06-07")) %>% save_plot(path = here("fig", "stack_plots", "mumbai_stack_plot_marked.pdf"))

make_r_count_stack_plots(place = "Delhi") %>% save_plot(path = here("fig", "stack_plots", "delhi_stack_plot.pdf"))
