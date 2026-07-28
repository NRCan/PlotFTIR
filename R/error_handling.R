#' Bilingual Error Handling for PlotFTIR
#'
#' @description Provides bilingual error, warning, and information handling functions
#'   that respect the user's language setting from `options("PlotFTIR.lang")`.
#'   All messages are bilingual (English/French) and automatically displayed in
#'   the appropriate language.
#'
#' @param message_en English version of the message
#' @param message_fr French version of the message
#' @param call The calling environment for error positioning
#' @param ... Additional arguments to pass to cli functions
#'
#' @keywords internal
NULL

.pkg_abort <- function(message_en, message_fr, call = rlang::caller_env(), ...) {
  lang <- getOption("PlotFTIR.lang", default = "en")
  l <- substr(lang, 1, 2)
  
  if (l == "fr") {
    cli::cli_abort(message_fr, call = call, ...)
  } else {
    cli::cli_abort(message_en, call = call, ...)
  }
}

.pkg_warn <- function(message_en, message_fr, call = rlang::caller_env(), ...) {
  lang <- getOption("PlotFTIR.lang", default = "en")
  l <- substr(lang, 1, 2)
  
  if (l == "fr") {
    cli::cli_warn(message_fr, call = call, ...)
  } else {
    cli::cli_warn(message_en, call = call, ...)
  }
}

.pkg_inform <- function(message_en, message_fr, call = rlang::caller_env(), ...) {
  lang <- getOption("PlotFTIR.lang", default = "en")
  l <- substr(lang, 1, 2)
  
  if (l == "fr") {
    cli::cli_inform(message_fr, call = call, ...)
  } else {
    cli::cli_inform(message_en, call = call, ...)
  }
}