#' Convert Between Absorbance and Transmittance
#'
#' @description These functions allow for the convenient conversion between \%
#'  Transmittance and Absorbance units for the Y axis.
#'
#'  Converting between %Transmittance and absorbance units for the Y axis is not
#'  a simple flipping of axis or inversion. Instead we know that the two are
#'  related by the following formulas:
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'  and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }.
#'
#'  Ces fonctions permettent une conversion pratique entre les unités \% de
#'  transmission et d'absorption pour l'axe Y.
#'
#'  La conversion entre les unités de \% de transmission et d'absorbance pour
#'  l'axe Y n'est pas un simple retournement d'axe ou une inversion. Au lieu de
#'  cela, nous savons que les deux sont liés par les formules suivantes:
#'
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'  and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }
#'
#' @param ftir A data.frame of FTIR spectral data including column to be
#'  converted. Can't contain both `absorbance` and `transmittance` column as the
#'  receiving column would be overwritten
#'
#'  Une data.frame de données spectrales FTIR comprenant la colonne à convertir.
#'  Ne peut pas contenir à la fois les colonnes `absorbance` et `transmittance`,
#'  car la colonne de réception serait écrasée.
#'
#' @return a data.frame of FTIR spectral data with conversion between absorbance
#'  and transmittance as requested. Note the original data column is removed
#'  since FTIR spectral data frames can't be fed into plotting functions with
#'  both transmittance and absorbance data included.
#'
#'  une data.frame de données spectrales FTIR avec conversion entre
#'  l'absorbance et la transmission comme demandé. Notez que la colonne de
#'  données d'origine est supprimée car les trames de données spectrales FTIR ne
#'  peuvent pas être introduites dans les fonctions de traçage avec les données
#'  de transmission et d'absorbance incluses.
#'
#' @examples
#' # Convert from absorbance to transmittance
#' sample_spectra_transmittance <- absorbance_to_transmittance(sample_spectra)
#'
#' # Convert back to absorbance
#' sample_spectra_absorbance <- transmittance_to_absorbance(sample_spectra_transmittance)
#'
#' @name conversion
NULL

#' @export
#' @rdname conversion
absorbance_to_transmittance <- function(ftir) {
  if ("absorbance" %in% colnames(ftir) && "transmittance" %in% colnames(ftir)) {
    cli::cli_abort("{.arg ftir} cannot contain both {.var absorbance} and {.var transmittance} columns.")
  }
  if (!"absorbance" %in% colnames(ftir)) {
    cli::cli_abort("{.arg ftir} must contain a {.var absorbance} column.")
  }
  ftir$transmittance <- (10^(ftir$absorbance * -1)) * 100
  ftir$absorbance <- NULL

  ftir <- ftir %>% dplyr::relocate("transmittance", .after = "wavenumber")

  return(ftir)
}

#' @export
#' @rdname conversion
transmittance_to_absorbance <- function(ftir) {
  if ("absorbance" %in% colnames(ftir) && "transmittance" %in% colnames(ftir)) {
    cli::cli_abort("{.arg ftir} cannot contain both {.var absorbance} and {.var transmittance} columns.")
  }
  if (!"transmittance" %in% colnames(ftir)) {
    cli::cli_abort("{.arg ftir} must contain a {.var transmitance} column.")
  }
  ftir$absorbance <- -log(ftir$transmittance / 100, base = 10)
  ftir$transmittance <- NULL

  ftir <- ftir %>% dplyr::relocate("absorbance", .after = "wavenumber")

  return(ftir)
}
