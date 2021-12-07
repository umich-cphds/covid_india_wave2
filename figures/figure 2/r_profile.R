options(tidyverse.quiet = TRUE)

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
