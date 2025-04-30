# nocov start

.onAttach <- function(libname, pkgname) {
  # default to memory cache if not set
  lang_option <- getOption("PlotFTIR.lang")

  if (is.null(lang_option)) {
    lang_option <- "en"
    options("PlotFTIR.lang" = "en")
    packageStartupMessage(
      'Plotting spectra with PlotFTIR. Please cite if plots are used in publishing (`citation("plotFTIR")`).\n',
      'PlotFTIR is set to English as default. Changer au fran\u00e7ais par la fonction `options("PlotFTIR.lang" = "en")`'
    )
  } else if (
    tolower(lang_option) %in%
      c("fr", "fra", "french", "francais", "fran\u00e7ais")
  ) {
    packageStartupMessage(
      'Trac\u00e9 des spectres avec PlotFTIR. Veuillez citer si les tracu00e9s sont utilisu00e9s dans un publication (`citation("plotFTIR")`).'
    )
  } else {
    packageStartupMessage(
      'Plotting spectra with PlotFTIR. Please cite if plots are used in publishing (`citation("plotFTIR")`).'
    )
  }
}
# nocov end
