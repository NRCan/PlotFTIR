#' Bilingual Error Handling Functions
#'
#' Helper functions for generating bilingual error and warning messages
#'
#' @noRd
NULL

#' Get current language setting
#'
#' @return character string with the current language code
.get_language <- function() {
  lang <- getOption("PlotFTIR.lang", default = "en")
  if (lang %in% c("en", "fr")) {
    return(lang)
  } else {
    return("en")
  }
}

#' Bilingual abort function
#'
#' @param messages Named character vector with language codes as names and
#'   messages as values
#' @param call The call to use for the error
#' @param ... Additional arguments passed to cli_abort
#' @noRd
.pkg_abort <- function(messages, call = rlang::caller_env(), ...) {
  if (
    all(!is.character(messages), !is.list(messages), is.null(names(messages)))
  ) {
    stop("messages must be a named character vector or a named list of vectors")
  }

  lang <- .get_language()
  message <- messages[[lang]]

  if (is.null(message)) {
    # Fallback to English if language not found
    message <- messages[["en"]]
  }

  cli::cli_abort(message, call = call, ...)
}

#' Bilingual warning function
#'
#' @param messages Named character vector with language codes as names and
#'   messages as values
#' @param call The call to use for the warning
#' @param ... Additional arguments passed to cli_warn
#' @noRd
.pkg_warn <- function(messages, call = rlang::caller_env(), ...) {
  if (
    all(!is.character(messages), !is.list(messages), is.null(names(messages)))
  ) {
    stop("messages must be a named character vector or a named list of vectors")
  }

  lang <- .get_language()
  message <- messages[[lang]]

  if (is.null(message)) {
    # Fallback to English if language not found
    message <- messages[["en"]]
  }

  cli::cli_warn(message, call = call, ...)
}

#' Bilingual inform function
#'
#' @param messages Named character vector with language codes as names and
#'   messages as values
#' @param call The call to use for the inform
#' @param ... Additional arguments passed to cli_inform
#' @noRd
.pkg_inform <- function(messages, call = rlang::caller_env(), ...) {
  if (
    all(!is.character(messages), !is.list(messages), is.null(names(messages)))
  ) {
    stop("messages must be a named character vector or a named list of vectors")
  }

  lang <- .get_language()
  message <- messages[[lang]]

  if (is.null(message)) {
    # Fallback to English if language not found
    message <- messages[["en"]]
  }

  cli::cli_inform(message, call = call, ...)
}
