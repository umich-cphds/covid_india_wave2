# libraries ----------
#install.packages("librarian")
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel,
            janitor, glue, here, ggtext, patchwork,
            ggpubr)
source("model/r_profile.R")

end_date <- "2021-06-30"

# load data ----------
obs <- fread("model/covid19india_national_counts_20211031.csv")[, date := as.Date(date)][date >= "2021-02-15"][, value := predict(loess(daily_cases ~ as.numeric(date), span = 0.25))][, .(date, value, variable = "Observed")]

load_pred <- function(x) {
  
  tmp_pred <- fread(glue("model/seir_results/plot_data/Daily_lol_{x}.csv"))[, date := as.Date(dates)]
  tmp_pred <- melt(tmp_pred[, !c("V1", "dates")], id.vars = "date")
  tmp_pred <- tmp_pred[
    , variable := fcase(
      variable == "P_daily_4", "4 weeks",
      variable == "P_daily_6", "6 weeks",
      variable == "P_daily_8", "8 weeks"
    )][date >= as.Date(x, "%Y%m%d")][
      , start_date := as.Date(x, "%Y%m%d")][]
  
  return(tmp_pred)
  
}

pred_apr15 <- load_pred(x = "20210415")
pred_mar30 <- load_pred(x = "20210330")
pred_mar19 <- load_pred(x = "20210319")

plot_data <- rbindlist(list(
  obs,
  pred_apr15,
  pred_mar30,
  pred_mar19
), use.names = TRUE, fill = TRUE)[date <= end_date]

tmp_title    <- "Effect of length of lockdown"

# plot -----------
colores <- c(
  "Observed" = "black",
  "4 weeks"  = pal_lancet()(3)[1],
  "6 weeks"  = pal_lancet()(3)[2],
  "8 weeks"  = pal_lancet()(3)[3]
)

lol_plot <- function(x) {
  
  plot_data[variable == "Observed" | start_date == as.Date(x, "%Y%m%d")][date >= as.Date("2021-02-15") & date <= end_date] %>%
    ggplot(aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    scale_color_manual(values = colores) +
    geom_vline(data = plot_data[start_date == as.Date(x, "%Y%m%d")] %>% 
                 group_by(variable) %>% 
                 filter(date == min(date)) %>% 
                 dplyr::ungroup() %>% 
                 select(variable, date) %>% 
                 filter(!(variable %in% c("Observed", "No intervention"))), 
               aes(xintercept = date, color = variable), 
               linetype = 'dashed') + 
    geom_label_repel(data = plot_data[start_date == as.Date(x, "%Y%m%d")] %>% 
                       group_by(variable) %>% 
                       filter(value == max(value)) %>% 
                       dplyr::ungroup() %>% 
                       select(variable, date, value) %>% 
                       filter(!(variable %in% c("Observed", "No intervention"))), 
                     aes(x = date, 
                         y = value, 
                         label = paste0(formatC(round(value), format="f", big.mark=",", digits=0), " cases"),
                         color = variable,
                         family = "Helvetica Neue"), 
                     nudge_y = 100000, 
                     nudge_x = -10, 
                     size = 4, 
                     show.legend  = FALSE, 
                     segment.size = 1) + 
    guides(color = guide_legend(nrow = 1)) + 
    labs(title    = glue("{format(as.Date(x, '%Y%m%d'), '%B %e, %Y')}"),
         y        = "Daily cases",
         x        = "",
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

apr15_plot <- lol_plot(x = "20210415")
mar30_plot <- lol_plot(x = "20210330") + theme(legend.position = "none")
mar15_plot <- lol_plot(x = "20210319") + theme(legend.position = "none")

leg <- as_ggplot(get_legend(apr15_plot))

apr15_plot <- apr15_plot + theme(legend.position = "none")

patched <- mar15_plot / mar30_plot / apr15_plot / leg

full_plot <- patched +
  plot_layout(ncol = 1, heights = c(3, 3, 3, 1)) &
  plot_annotation(
    title    = "Effect of length of moderate lockdown on various start dates",
    subtitle = glue::glue("{format(as.Date('2021-02-15'), '%B %e, %Y')} to {format(as.Date(end_date), '%B %e, %Y')}"),
    tag_levels = list(c("A", "B", "C", ""))
  )  &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

# save output ----------
ggsave(filename = "figures/figure_s14/figure_s14.pdf",
       plot     = full_plot,
       height   = 10,
       width    = 10,
       units = "in", device = cairo_pdf)
