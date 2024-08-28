# data

#' FTIR example data
#'
#' @description This dataset holds example data from Attenuated Total
#'   Reflectance (ATR) type FTIR instruments. Solvents were collected on
#'   5-Bounce ZnSe ATR-FTIR and solids were collected on a 1-Bounce Diamond
#'   ATR-FTIR.
#'
#'
#'   The samples included in this data set are:
#'    * `toluene` a FTIR spectra of toluene.
#'    * `isopropanol` a FTIR spectra of isopropanol.
#'    * `heptanes` a FTIR spectra of heptanes.
#'    * `paper` a FTIR spectra of a commercially available white paper.
#'    * `polystyrene` a FTIR spectra of polystyrene film.
#'
#'   Ce donnée contient des exemples de données provenant
#'   d'instruments IRTF de type réflectance totale atténuée (RTA). Les solvants
#'   ont été collectés sur un RTA-IRTF ZnSe à 5 rebonds et les solides ont été
#'   collectés sur un RTA-IRTF diamant à 1 rebond.
#'
#'   Les échantillons inclus dans cet ensemble de données sont :
#'    * `toluene` un spectre IRTF du toluène.
#'    * `isopropanol` un spectre IRTF de l'isopropanol.
#'    * `heptanes` un spectre IRTF d'heptanes.
#'    * `paper` un spectre IRTF d'un livre blanc disponible dans le commerce.
#'    * `polystyrene` un spectre IRTF d'un film de polystyrène
#'
#' @format The data is in a long format data.frame, ready for use, and with the
#'   following variables:
#'
#'     * `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'     * `absorbance` the absorbance of the sample at each specified wavenumber.
#'     * `sample_id` the sample identity of the various included spectra.
#'
#'   Les données sont dans un format long data.frame, prêt à l'emploi, et avec les variables suivantes:
#'
#'    * `wavenmber` valeur du nombre d'onde (axe x) pour chaque point. En unités de nombres d'ondes (cm^-1).
#'    * `absorbance` l'absorbance de l'échantillon à chaque nombre d'onde spécifié.
#'    * `sample_id` identité d'échantillon des différents spectres inclus.
"sample_spectra"

#' FTIR Bio-diesel data
#'
#' @description This dataset holds example data from an 5-Bounce ZnSe Attenuated
#'   Total Reflectance (ATR) FTIR instrument collected for biodiesel analysis.
#'   The biodiesel samples were purchased calibration standards or available on
#'   the Canadian commercial market in 2024.
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
#'
#'   Ce donnée contient des exemples de données provenant d'un
#'   instrument IRTF à réflectance totale atténuée (RTA) ZnSe à 5 rebonds
#'   collectées pour l'analyse du biodiesel. Les échantillons de biodiesel
#'   étaient des étalons d'étalonnage achetés ou disponibles sur le marché
#'   commercial canadien en 2024.
#'
#'   Les échantillons inclus dans cet ensemble de données sont :
#'     * `biodiesel_0` Un échantillon d'étalonnage avec 0,0 % de biodiesel.
#'     * `biodiesel_0_25` Un échantillon d'étalonnage avec 0,25 % de biodiesel.
#'     * `biodiesel_0_50` Un échantillon d'étalonnage avec 0,50 % de biodiesel.
#'     * `biodiesel_1_0` Un échantillon d'étalonnage avec 1,0 % de biodiesel.
#'     * `biodiesel_2_5` Un échantillon d'étalonnage avec 2,5 % de biodiesel.
#'     * `biodiesel_5_0` Un échantillon d'étalonnage avec 5,0 % de biodiesel.
#'     * `biodiesel_7_5` Un échantillon d'étalonnage avec 7,5 % de biodiesel.
#'     * `biodiesel_10_0` Un échantillon d'étalonnage avec 10,0 % de biodiesel.
#'     * `biodiesel_B0_5` Un échantillon disponible dans le commerce contenant environ 0,5 % de biodiesel.
#'     * `biodiesel_B5` Un échantillon disponible dans le commerce contenant environ 5,0 % de biodiesel.
#'     * `diesel_unknown` Un échantillon disponible dans le commerce avec une teneur en biodiesel inconnue.
#'
#' @format The data is in a long format data.frame, ready for use, and with the
#'   following variables:
#'
#'     * `wavenumber` wavenumber (x axis) value for each point. In units of wavenumbers (cm^-1).
#'     * `absorbance` the absorbance of the sample at each specified wavenumber.
#'     * `sample_id` the sample identity of the various included spectra.
#'
#'   Les données sont dans un format long data.frame, prêt à l'emploi, et avec les variables suivantes:
#'
#'    * `wavenmber` valeur du nombre d'onde (axe x) pour chaque point. En unités de nombres d'ondes (cm^-1).
#'    * `absorbance` l'absorbance de l'échantillon à chaque nombre d'onde spécifié.
#'    * `sample_id` identité d'échantillon des différents spectres inclus.
"biodiesel"
