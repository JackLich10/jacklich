% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphing.R
\name{theme_jack}
\alias{theme_jack}
\title{Minimal ggplot2 theme building off of theme_bw()}
\usage{
theme_jack(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  aspect = F,
  ...
)
}
\arguments{
\item{base_size}{base font size}

\item{aspect}{logical specifying whether to change aspect ratio to 9/16
which is useful when using images}

\item{...}{Other arguments passed to \code{theme_bw}}

\item{strip_text_size, strip_text_margin}{plot strip text size and margin}

\item{subtitle_size, subtitle_margin}{plot subtitle size and margin}

\item{plot_title_size, plot_title_margin}{plot title size and margin}
}
\description{
Minimal ggplot2 theme building off of theme_bw()
}
\examples{
\dontrun{
library(ggplot2)

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    labs(title = "A Lovely Plot",
         subtitle = "What can the subtitle tell us?") +
    theme_jack()

ggplot(diamonds, aes(carat, price, color = clarity)) +
    geom_point(alpha = 0.7) +
    facet_wrap(~cut) +
    labs(title = "A Lovely Plot",
         subtitle = "What can the subtitle tell us?") +
         theme_jack()

}

}
