#data 

#' FTIR example data
#'
#' @description
#' This dataset hold example data from an ATR type FTIR instrument. 
#' 
#' 
#' The samples included in this data set are:
#' \itemize{
#'  \item `toluene` a FTIR spectra of toluene.
#'  \item `isopropanol` a FTIR spectra of isopropanol.
#'  \item `heptanes` a FTIR spectra of heptanes.
#'  \item `paper` a FTIR spectra of a commercially available white paper.
#'  \item `polystyrene` a FTIR spectra of polystyrene film.
#' }
#' 
#' @format The data is in long format, ready for use, and as such has the following variables:
#' 
#' \describe{
#'   \item `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'   \item `absorbance` The absorbance of the sample at each specified wavenumber.
#'   \item `sample_id` Sample ID of the various included spectra. See below for descriptions.
#' }
"sample_spectra"

#' FTIR Bio-diesel data
#'
#' @description
#' This dataset hold example data from an ATR type FTIR instrument collected for biodiesel analysis. 
#' 
#' 
#' The samples included in this data set are:
#' \itemize{
#'   \item `biodiesel_0` A calibration sample with 0.0 % biodiesel.
#'   \item `biodiesel_0_25` A calibration sample with 0.25 % biodiesel.
#'   \item `biodiesel_0_50` A calibration sample with 0.50 % biodiesel.
#'   \item `biodiesel_1_0` A calibration sample with 1.0 % biodiesel.
#'   \item `biodiesel_2_5` A calibration sample with 2.5 % biodiesel.
#'   \item `biodiesel_5_0` A calibration sample with 5.0 % biodiesel.
#'   \item `biodiesel_7_5` A calibration sample with 7.5 % biodiesel.
#'   \item `biodiesel_10_0` A calibration sample with 10.0 % biodiesel.
#'   \item `biodiesel_B0_5` A commercially available sample with approximately 0.5 % biodiesel.
#'   \item `biodiesel_B5` A commercially available sample with approximately 5.0 % biodiesel.
#'   \item `diesel_unknown` A commercially available sample with unknown biodiesel content.
#' }
#' 
#' @format The data is in long format, ready for use, and as such has the following variables:
#' 
#' \describe{
#'   \item `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'   \item `absorbance` The absorbance of the sample at each specified wavenumber.
#'   \item `sample_id` Sample ID of the various included spectra. See below for descriptions.
#' }
"biodiesel"