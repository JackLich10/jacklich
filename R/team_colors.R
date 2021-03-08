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
  pivot_long_team_info() %>%
    dplyr::filter(team_name == team) %>%
    dplyr::distinct(team_name, team_logo_espn) %>%
    dplyr::pull(team_logo_espn)
}

#' Find the two primary colors for a college basketball team or program
#'
#' @param team name of the team for which to return their logo
#' @param switch_col logical of whether or not to switch the order of
#' the returned colors
#' @examples
#' \dontrun{
#' find_cbb_team_colors(team = "Duke", switch_col = F)
#'}
#'
#' @export
find_cbb_team_colors <- function(team, switch_col = F) {
  colors <- pivot_long_team_info() %>%
    dplyr::filter(team_name == team) %>%
    dplyr::distinct(team_name, primary_color, secondary_color) %>%
    dplyr::mutate(secondary_color = dplyr::case_when(
      team_name == "Duke" ~ "#235F9C",
      T ~ secondary_color)) %>%
    dplyr::select(dplyr::ends_with("_color")) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::pull(value) %>%
    purrr::set_names(., nm = rep(team, 2))

  if (isTRUE(switch_col)) {
    colors <- c(colors[2], colors[1])
  }
  return(colors)
}

#' Join in team information for a college basketball team or program
#'
#' @param data dataframe with college basketball data and team names
#' @param join_name column in which to join team information on
#' @examples
#' \dontrun{
#' dplyr::tibble(team = "Duke") %>%
#' join_cbb_team_info(data = ., join_name = "team")
#'}
#'
#' @export
join_cbb_team_info <- function(data, join_name) {
  # convert `join_name` to a symbol
  join_name <- dplyr::sym(join_name)

  data %>%
    dplyr::mutate(team_name = !!join_name) %>%
    dplyr::left_join(pivot_long_team_info(),
                     by = "team_name")
}

# Helper for team color functions
pivot_long_team_info <- function() {
  jacklich::cbb_team_info %>%
    tidyr::pivot_longer(cols = c(team_name:sref_name),
                        names_to = "organization",
                        values_to = "team_name") %>%
    dplyr::distinct(team_name, .keep_all = T) %>%
    dplyr::select(conference, organization, team_name,
                  dplyr::everything())
}
