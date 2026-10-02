#' Launch the MF Attribution Shiny App
#'
#' @param ... arguments passed to \code{shiny::runApp()} (e.g. port, launch.browser)
#' @export
run_mf_app <- function(...) {
  app_dir <- system.file("app", package = "MFattributionGBA")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `MFattribution`.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}