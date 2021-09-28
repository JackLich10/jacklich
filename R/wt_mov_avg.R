#' Weighted rolling averages with a dynamic window
#'
#' @param var vector of data to take rolling average
#' @param weight vector of data to weight rolling average with
#' @param window length of rolling average window
#' @param type one of `type = c("s", "t", "w", "m", "e", "r")` from `pracma::movavg`
#' @param moving logical specifying if rolling average should have a dynamic window or not
#'
#' @import pracma
#'
#' @examples
#' \dontrun{
#'
#' wt_mov_avg(var = fgm, weight = fga, window = window, type = "s", moving = T)
#'
#'}
#'
#' @export
wt_mov_avg <- function(var, weight, window, type, moving = T) {
  if (length(weight) == 1 & weight[1] == 1) {
    weight <- rep(1, length(var))
  }
  if (moving) {
    max <- length(var)-1

    if (max > 1) {
      dplyr::case_when(
        window == 2 ~ pracma::movavg(var*weight, n = pmin(max, 2), type = type)/
          pracma::movavg(weight, n = pmin(max, 2), type = type),
        window == 3 ~ pracma::movavg(var*weight, n = pmin(max, 3), type = type)/
          pracma::movavg(weight, n = pmin(max, 3), type = type),
        window == 4 ~ pracma::movavg(var*weight, n = pmin(max, 4), type = type)/
          pracma::movavg(weight, n = pmin(max, 4), type = type),
        window == 5 ~ pracma::movavg(var*weight, n = pmin(max, 5), type = type)/
          pracma::movavg(weight, n = pmin(max, 5), type = type),
        window == 6 ~ pracma::movavg(var*weight, n = pmin(max, 6), type = type)/
          pracma::movavg(weight, n = pmin(max, 6), type = type),
        window == 7 ~ pracma::movavg(var*weight, n = pmin(max, 7), type = type)/
          pracma::movavg(weight, n = pmin(max, 7), type = type),
        window == 8 ~ pracma::movavg(var*weight, n = pmin(max, 8), type = type)/
          pracma::movavg(weight, n = pmin(max, 8), type = type),
        window == 9 ~ pracma::movavg(var*weight, n = pmin(max, 9), type = type)/
          pracma::movavg(weight, n = pmin(max, 9), type = type),
        window == 10 ~ pracma::movavg(var*weight, n = pmin(max, 10), type = type)/
          pracma::movavg(weight, n = pmin(max, 10), type = type),
        window == 11 ~ pracma::movavg(var*weight, n = pmin(max, 11), type = type)/
          pracma::movavg(weight, n = pmin(max, 11), type = type),
        window == 12 ~ pracma::movavg(var*weight, n = pmin(max, 12), type = type)/
          pracma::movavg(weight, n = pmin(max, 12), type = type),
        window == 13 ~ pracma::movavg(var*weight, n = pmin(max, 13), type = type)/
          pracma::movavg(weight, n = pmin(max, 13), type = type),
        window == 14 ~ pracma::movavg(var*weight, n = pmin(max, 14), type = type)/
          pracma::movavg(weight, n = pmin(max, 14), type = type),
        window == 15 ~ pracma::movavg(var*weight, n = pmin(max, 15), type = type)/
          pracma::movavg(weight, n = pmin(max, 15), type = type),
        window == 16 ~ pracma::movavg(var*weight, n = pmin(max, 16), type = type)/
          pracma::movavg(weight, n = pmin(max, 16), type = type),
        window == 17 ~ pracma::movavg(var*weight, n = pmin(max, 17), type = type)/
          pracma::movavg(weight, n = pmin(max, 17), type = type),
        window == 18 ~ pracma::movavg(var*weight, n = pmin(max, 18), type = type)/
          pracma::movavg(weight, n = pmin(max, 18), type = type),
        window == 19 ~ pracma::movavg(var*weight, n = pmin(max, 19), type = type)/
          pracma::movavg(weight, n = pmin(max, 19), type = type),
        window == 20 ~ pracma::movavg(var*weight, n = pmin(max, 20), type = type)/
          pracma::movavg(weight, n = pmin(max, 20), type = type),
        window == 21 ~ pracma::movavg(var*weight, n = pmin(max, 21), type = type)/
          pracma::movavg(weight, n = pmin(max, 21), type = type),
        window == 22 ~ pracma::movavg(var*weight, n = pmin(max, 22), type = type)/
          pracma::movavg(weight, n = pmin(max, 22), type = type),
        window == 23 ~ pracma::movavg(var*weight, n = pmin(max, 23), type = type)/
          pracma::movavg(weight, n = pmin(max, 23), type = type),
        window == 24 ~ pracma::movavg(var*weight, n = pmin(max, 24), type = type)/
          pracma::movavg(weight, n = pmin(max, 24), type = type),
        window == 25 ~ pracma::movavg(var*weight, n = pmin(max, 25), type = type)/
          pracma::movavg(weight, n = pmin(max, 25), type = type),
        window == 26 ~ pracma::movavg(var*weight, n = pmin(max, 26), type = type)/
          pracma::movavg(weight, n = pmin(max, 26), type = type),
        window == 27 ~ pracma::movavg(var*weight, n = pmin(max, 27), type = type)/
          pracma::movavg(weight, n = pmin(max, 27), type = type),
        window == 28 ~ pracma::movavg(var*weight, n = pmin(max, 28), type = type)/
          pracma::movavg(weight, n = pmin(max, 28), type = type),
        window == 29 ~ pracma::movavg(var*weight, n = pmin(max, 29), type = type)/
          pracma::movavg(weight, n = pmin(max, 29), type = type),
        window == 30 ~ pracma::movavg(var*weight, n = pmin(max, 30), type = type)/
          pracma::movavg(weight, n = pmin(max, 30), type = type),
        window == 31 ~ pracma::movavg(var*weight, n = pmin(max, 31), type = type)/
          pracma::movavg(weight, n = pmin(max, 31), type = type),
        window == 32 ~ pracma::movavg(var*weight, n = pmin(max, 32), type = type)/
          pracma::movavg(weight, n = pmin(max, 32), type = type),
        window == 33 ~ pracma::movavg(var*weight, n = pmin(max, 33), type = type)/
          pracma::movavg(weight, n = pmin(max, 33), type = type),
        window == 34 ~ pracma::movavg(var*weight, n = pmin(max, 34), type = type)/
          pracma::movavg(weight, n = pmin(max, 34), type = type),
        window == 35 ~ pracma::movavg(var*weight, n = pmin(max, 35), type = type)/
          pracma::movavg(weight, n = pmin(max, 35), type = type),
        window == 36 ~ pracma::movavg(var*weight, n = pmin(max, 36), type = type)/
          pracma::movavg(weight, n = pmin(max, 36), type = type),
        window == 37 ~ pracma::movavg(var*weight, n = pmin(max, 37), type = type)/
          pracma::movavg(weight, n = pmin(max, 37), type = type),
        window == 38 ~ pracma::movavg(var*weight, n = pmin(max, 38), type = type)/
          pracma::movavg(weight, n = pmin(max, 38), type = type),
        window == 39 ~ pracma::movavg(var*weight, n = pmin(max, 39), type = type)/
          pracma::movavg(weight, n = pmin(max, 39), type = type),
        window == 40 ~ pracma::movavg(var*weight, n = pmin(max, 40), type = type)/
          pracma::movavg(weight, n = pmin(max, 40), type = type),
        window == 41 ~ pracma::movavg(var*weight, n = pmin(max, 41), type = type)/
          pracma::movavg(weight, n = pmin(max, 41), type = type)
      )
    } else {
      sum(var*weight)/sum(weight)
    }
  } else {
    pracma::movavg(var*weight, n = window, type = type)/
      pracma::movavg(weight, n = window, type = type)
  }
}
