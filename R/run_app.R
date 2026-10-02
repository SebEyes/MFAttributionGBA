#' Launch the MF Attribution Shiny App
#'
#' @param ... arguments passed to \code{shiny::runApp()} (e.g. port, launch.browser)
#' @export
run_mf_app <- function(...) {
  app_dir <- system.file("app", package = "MFAttributionGBA")
  if (!nzchar(app_dir)) {
    stop("App directory not found in the installed package.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}