#data 

#' FTIR example data
#'
#' @description This dataset holds example data from ATR type FTIR instruments.
#' Solvents were collected on 5-Bounce ZnSe ATR-FTIR and solids were collected
#' on a 1-Bounce Diamond ATR-FTIR.
#'
#'
#' The samples included in this data set are:
#'    * `toluene` a FTIR spectra of toluene.
#'    * `isopropanol` a FTIR spectra of isopropanol.
#'    * `heptanes` a FTIR spectra of heptanes.
#'    * `paper` a FTIR spectra of a commercially available white paper.
#'    * `polystyrene` a FTIR spectra of polystyrene film.
#'
#' @format The data is in long format, ready for use, and as such has the
#'   following variables:
#'

#'     * `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'     * `absorbance` The absorbance of the sample at each specified wavenumber.
#'     * `sample_id` Sample ID of the various included spectra. See below for descriptions.

"sample_spectra"

#' FTIR Bio-diesel data
#'
#' @description This dataset holds example data from an 5-Bounce ZnSe ATR-FTIR
#'   instrument collected for biodiesel analysis.
#'
#'
#'   The samples included in this data set are:
#'     * `biodiesel_0` A calibration sample with 0.0 % biodiesel.
#'     * `biodiesel_0_25` A calibration sample with 0.25 % biodiesel.
#'     * `biodiesel_0_50` A calibration sample with 0.50 % biodiesel.
#'     * `biodiesel_1_0` A calibration sample with 1.0 % biodiesel.
#'     * `biodiesel_2_5` A calibration sample with 2.5 % biodiesel.
#'     * `biodiesel_5_0` A calibration sample with 5.0 % biodiesel.
#'     * `biodiesel_7_5` A calibration sample with 7.5 % biodiesel.
#'     * `biodiesel_10_0` A calibration sample with 10.0 % biodiesel.
#'     * `biodiesel_B0_5` A commercially available sample with approximately 0.5 % biodiesel.
#'     * `biodiesel_B5` A commercially available sample with approximately 5.0 % biodiesel.
#'     * `diesel_unknown` A commercially available sample with unknown biodiesel content.

#'
#' @format The data is in long format, ready for use, and as such has the
#'   following variables:
#'

#'     * `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'     * `absorbance` The absorbance of the sample at each specified wavenumber.
#'     * `sample_id` Sample ID of the various included spectra. See below for descriptions.

"biodiesel"