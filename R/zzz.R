# nocov start

#' @keywords internal
#' @returns (`character(1)`) A lowercase two-letter language code (e.g. `"fr"`, `"en"`).
.detect_system_language <- function() {
  # Check environment variables
  lang_env <- Sys.getenv("LANG")
  language_env <- Sys.getenv("LANGUAGE")

  # Check message locale
  messages_locale <- Sys.getlocale("LC_MESSAGES")

  # If we have a LANG environment variable, try to extract language code
  if (nzchar(lang_env)) {
    # Extract language part from LANG (e.g., "fr_FR.UTF-8" -> "fr", case-insensitive)
    lang_match <- regmatches(
      lang_env,
      regexpr("(?i)^[a-z]{2}", lang_env, perl = TRUE)
    )
    if (length(lang_match) > 0 && nzchar(lang_match)) {
      return(tolower(lang_match))
    }
  }

  # If we have a LANGUAGE environment variable, try to extract first language
  if (nzchar(language_env)) {
    # Extract language part from LANGUAGE (e.g., "fr:en" -> "fr")
    lang_match <- regmatches(
      language_env,
      regexpr("(?i)^[a-z]{2}", language_env, perl = TRUE)
    )
    if (length(lang_match) > 0 && nzchar(lang_match)) {
      return(tolower(lang_match))
    }
  }

  # If we have a messages locale, try to extract language code
  if (nzchar(messages_locale)) {
    # Extract language part from messages locale (e.g., "fr_FR.UTF-8" -> "fr")
    lang_match <- regmatches(
      messages_locale,
      regexpr("(?i)^[a-z]{2}", messages_locale, perl = TRUE)
    )
    if (length(lang_match) > 0 && nzchar(lang_match)) {
      return(tolower(lang_match))
    }
  }

  # Default to English if no language detected
  return("en")
}

.onLoad <- function(libname, pkgname) {
  # Set default language option based on system detection only if not already set
  lang_option <- getOption("PlotFTIR.lang")

  if (is.null(lang_option)) {
    detected_lang <- .detect_system_language()

    if (
      detected_lang %in% c("fr", "fra", "french", "francais", "fran\u00e7ais")
    ) {
      options("PlotFTIR.lang" = "fr")
    } else {
      options("PlotFTIR.lang" = "en")
    }
  }
}

.onAttach <- function(libname, pkgname) {
  lang_option <- getOption("PlotFTIR.lang")

  if (
    tolower(lang_option) %in%
      c("fr", "fra", "french", "francais", "fran\u00e7ais")
  ) {
    packageStartupMessage(
      'Trac\u00e9 des spectres avec PlotFTIR. Veuillez citer si les tracu00e9s sont utilisu00e9s dans un publication (`citation("plotFTIR")`).'
    )
  } else {
    packageStartupMessage(
      'Plotting spectra with PlotFTIR. Please cite if plots are used in publishing (`citation("plotFTIR")`).\n',
      'PlotFTIR is set to English as default. Changer au fran\u00e7ais par la fonction `options("PlotFTIR.lang" = "fr")`'
    )
  }
}
# nocov end
