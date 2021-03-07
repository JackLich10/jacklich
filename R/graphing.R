#' Minimal ggplot2 theme building off of theme_bw()
#'
#' @param base_size base font size
#' @param aspect logical specifying whether to change aspect ratio to 9/16
#' which is useful when using images
#' @param strip_text_size,strip_text_margin plot strip text size and margin
#' @param subtitle_size,subtitle_margin plot subtitle size and margin
#' @param plot_title_size,plot_title_margin plot title size and margin
#' @param ... Other arguments passed to \code{theme_bw}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'     theme_jack()
#'
#' ggplot(diamonds, aes(carat, price, color = clarity)) +
#'     geom_point(alpha = 0.7) +
#'     facet_wrap(~cut) +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'          theme_jack()
#'
#'}
#'
#' @export
theme_jack <- function(base_size = 11,
                       base_family = "",
                       base_line_size = base_size/22,
                       base_rect_size = base_size/22,
                       aspect = F,
                       ...) {
  ret <- ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 28/ggplot2::.pt, hjust = 0),
                   plot.subtitle = ggplot2::element_text(face = "italic", size = 24/ggplot2::.pt),
                   plot.caption = ggplot2::element_text(face = "italic", size = 20/ggplot2::.pt),
                   strip.background = ggplot2::element_rect(color = "black", fill = "#C0C0C0", size = 3, linetype = "blank"),
                   strip.text = ggplot2::element_text(face = "bold", size = 24/ggplot2::.pt),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 24/ggplot2::.pt),
                   axis.title = ggplot2::element_text(face = "bold", size = 26/ggplot2::.pt),
                   legend.title = ggplot2::element_text(size = 26/ggplot2::.pt),
                   legend.text = ggplot2::element_text(size = 24/ggplot2::.pt))

  if (aspect) {
    ret <- ret +
      ggplot2::theme(aspect.ratio = 9/16)
  }
  return(ret)
}

#' Find the logo url for a college basketball team or program
#'
#' @param team name of the team for which to return their logo
#'
#' @examples
#' \dontrun{
#' find_cbb_team_logo(team = "Duke")
#'}
#'
#' @export
find_cbb_team_logo <- function(team) {
  ncaahoopR::dict %>%
    dplyr::left_join(ncaahoopR::ncaa_colors %>%
                       dplyr::select(-ncaa_name),
                     by = c("ESPN" = "espn_name",
                            "conference")) %>%
    tidyr::pivot_longer(cols = c(NCAA:sref_name),
                        names_to = "organization",
                        values_to = "team_name") %>%
    dplyr::select(conference, organization, team_name,
                  dplyr::everything()) %>%
    dplyr::filter(team_name == team) %>%
    dplyr::distinct(team_name, logo_url) %>%
    dplyr::pull(logo_url)
}
