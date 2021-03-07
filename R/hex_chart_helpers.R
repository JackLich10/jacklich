# Find logos for a team
find_logo <- function(team) {
  ncaahoopR::ncaa_colors$logo_url[ncaahoopR::ncaa_colors$espn_name == team]
}

# Helper functions to generate hex charts
hex_bounds <- function(x, binwidth) {
  c(plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6)
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds <- hex_bounds(shots$loc_x, binwidths[1])
  xbins <- diff(xbnds) / binwidths[1]
  ybnds <- hex_bounds(shots$loc_y, binwidths[2])
  ybins <- diff(ybnds) / binwidths[2]

  hb <- hexbin::hexbin(x = shots$loc_x,
                       y = shots$loc_y,
                       xbins = xbins,
                       xbnds = xbnds,
                       ybnds = ybnds,
                       shape = ybins / xbins,
                       IDs = TRUE)

  shots <- dplyr::mutate(shots, hexbin_id = hb@cID)

  hexbin_stats <- shots %>%
    dplyr::group_by(hexbin_id) %>%
    dplyr::summarise(hex_attempts = n(),
                     hex_pct = mean(shot_made_numeric),
                     hex_points_scored = sum(shot_made_numeric * shot_value),
                     hex_points_per_shot = mean(shot_made_numeric * shot_value)) %>%
    dplyr::ungroup()

  hexbin_ids_to_zones <- shots %>%
    dplyr::group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(attempts = n(),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(hexbin_id, desc(attempts)) %>%
    dplyr::group_by(hexbin_id) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::select(hexbin_id, shot_zone_range, shot_zone_area)

  hexbin_stats = dplyr::inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")

  # from hexbin package, see: https://github.com/edzer/hexbin
  sx <- hb@xbins / diff(hb@xbnds)
  sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx <- 1 / (2 * sx)
  dy <- 1 / (2 * sqrt(3) * sy)
  origin_coords <- hexbin::hexcoords(dx, dy)

  hex_centers <- hexbin::hcell2xy(hb)

  hexbin_coords <- dplyr::bind_rows(lapply(1:hb@ncells, function(i) {
    dplyr::tibble(x = origin_coords$x + hex_centers$x[i],
                  y = origin_coords$y + hex_centers$y[i],
                  center_x = hex_centers$x[i],
                  center_y = hex_centers$y[i],
                  hexbin_id = hb@cell[i])
  }))

  hexbin_stats <- dplyr::inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")

  essential_cols <- c("shot_zone_range", "shot_zone_area")
  extra_cols <- c("label_loc_x", "label_loc_y", "shot_accuracy_lab",
                  "fg_fga_lab", "side_by_side")

  filtered <- shots %>%
    dplyr::select(tidyselect::all_of(essential_cols), tidyselect::any_of(extra_cols)) %>%
    dplyr::distinct()

  dplyr::inner_join(hexbin_stats, filtered, by = c("shot_zone_area", "shot_zone_range"))
}


