#' Convert shots to averages to apply to 'league_averages' input for hex chart
#'
#' @param shots shot location data from play-by-play source
#'
#' @examples
#' \dontrun{
#' convert_to_avgs(shots = shots)
#'}
#'
#' @export
convert_to_avgs <- function(shots) {
  shots %>%
    dplyr::group_by(Location, shot_zone_area, shot_zone_range, shot_value) %>%
    dplyr::summarise(fga = n(),
                     fgm = sum(shot_made_numeric),
                     .groups = "drop") %>%
    dplyr::mutate(fg_pct = fgm/fga) %>%
    dplyr::ungroup()
}

#' Converting shot location data into hexbins for plotting
#'
#' @param shots shot location data from play-by-play source
#' @param league_averages league average accuracy from shot location areas
#' @param binwidths,min_radius_factor hex chart sizing specifications
#' @param fg_diff_limits,fg_freq_limits,fg_pct_limits,pps_limits values to bound
#' shot chart legend
#'
#' @examples
#' \dontrun{
#' calculate_hexbins_from_shots(shots = shots,
#' league_averages = league_averages, ...)
#'}
#'
#' @export
calculate_hexbins_from_shots = function(shots, league_averages = NULL,
                                        binwidths = c(1, 1), min_radius_factor = 0.6,
                                        fg_diff_limits = c(-0.15, 0.15), fg_pct_limits = c(0.2, 0.7),
                                        fg_freq_limits = c(-0.075, 0.075), pps_limits = c(0.5, 1.5)) {
  if (nrow(shots) == 0) {
    return(list())
  }

  shots <- shots %>%
    dplyr::filter(!is.na(loc_x), !is.na(loc_y))

  grouped_shots <- dplyr::group_by(shots, shot_zone_range, shot_zone_area)

  zone_stats <- grouped_shots %>%
    dplyr::summarise(zone_attempts = n(),
                     zone_pct = mean(shot_made_numeric),
                     zone_points_scored = sum(shot_made_numeric * shot_value),
                     zone_points_per_shot = mean(shot_made_numeric * shot_value),
                     .groups = "drop") %>%
    dplyr::mutate(zone_freq = zone_attempts/sum(zone_attempts))

  hex_data <- calculate_hex_coords(shots, binwidths = binwidths)

  join_keys <- c("shot_zone_range", "shot_zone_area")

  if (is.null(league_averages)) {
    hex_data = hex_data %>%
      dplyr::inner_join(zone_stats, by = join_keys)
  } else {
    league_zone_stats <- league_averages %>%
      dplyr::group_by(shot_zone_range, shot_zone_area) %>%
      dplyr::summarise(fgm = sum(fgm),
                       fga = sum(fga),
                       .groups = "drop") %>%
      dplyr::mutate(league_pct = fgm/fga,
                    league_freq = fga/sum(fga))

    hex_data <- hex_data %>%
      dplyr::inner_join(zone_stats, by = join_keys) %>%
      dplyr::inner_join(league_zone_stats, by = join_keys) %>%
      dplyr::mutate(bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_freq_diff = pmin(pmax(zone_freq - league_freq, fg_freq_limits[1]), fg_freq_limits[2]))
  }

  max_hex_attempts <- max(hex_data$hex_attempts)

  hex_data <- hex_data %>%
    dplyr::mutate(radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                  adj_x = center_x + radius_factor * (x - center_x),
                  adj_y = center_y + radius_factor * (y - center_y),
                  bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                  bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
}

#' Generate a hex chart for a given team or player
#'
#' @param shots shot location data from play-by-play source
#' @param league_averages league average accuracy from shot location areas
#' @param team team for which the shot location data is from
#' @param player player for which the shot location data is from (default is NULL)
#' @param type type of shot chart to create, one of sym(c("bounded_fg_diff",
#' "bounded_freq_diff", "bounded_fg_pct", "bounded_points_per_shot"))
#' @param ... other arguments passed to \code{calculate_hexbins_from_shots}
#'
#' @examples
#' \dontrun{
#' team_player_hex_chart(shots = duke, league_averages = league_averages,
#' team = "Duke", player = "Matthew Hurt", type = sym("bounded_fg_diff"),
#' binwidths = c(2, 2), min_radius_factor = 0.05)
#'}
#'
#' @export
team_player_hex_chart <- function(shots, league_averages, team, player = NULL,
                                  type = sym(c("bounded_fg_diff", "bounded_freq_diff", "bounded_fg_pct", "bounded_points_per_shot")), ...) {
  # if player input, change title and get their image
  if (!is.null(player)) {
    team_label <- player

    player_logo <- ncaahoopR::get_roster(team = "Duke") %>%
      dplyr::filter(name == player) %>%
      dplyr::pull(player_image)

    subtitle <- paste0("Among his ", scales::comma(nrow(shots)),
                       " ESPN charted shots during the ", unique(shots$season), " season")
  } else {
    team_label <- team
    subtitle <- paste0("Among their ", scales::comma(nrow(shots)),
                       " ESPN charted shots during the ", unique(shots$season), " season")
  }
  # find team logo
  team_logo <- find_logo(team = team)

  # calculate hex shots
  hex <- calculate_hexbins_from_shots(shots = shots,
                                      league_averages = league_averages, ...)

  # plot
  p <- generate_hex_chart(hex_shots = hex, team = team_label,
                          type = type) +
    scale_x_continuous(limits = c(0.25, 49.75)) +
    labs(subtitle = subtitle,
         caption = "Chart: @jacklich10 | Data: @ncaahoopR")

  if (!is.null(player)) {
    cowplot::ggdraw(p) +
      cowplot::draw_image(player_logo, y = 0.3, scale = 0.2) +
      cowplot::draw_image(team_logo, x = 0.055, y = 0.35, scale = 0.05)
  } else {
    cowplot::ggdraw(p) +
      cowplot::draw_image(team_logo, y = 0.3, scale = 0.15)
  }
}

