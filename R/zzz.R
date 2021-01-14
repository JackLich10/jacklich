.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  jacklich10_attach()

  # if (!"package:conflicted" %in% search()) {
  #   x <- jacklich10_conflicts()
  #   msg(jacklich10_conflict_message(x), startup = TRUE)
  # }

  msg(cli::rule(right = crayon::bold("Ready to go!")),startup = TRUE)

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
