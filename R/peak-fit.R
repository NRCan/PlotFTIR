#' Find FTIR Peaks
#' @description This function finds peaks in FTIR spectra by identifying minima
#'   of the double derivative, then re-scanning for maxima of peaks missed by
#'   the derivative method. This double-check ensures that both sharp peaks
#'   (like C-H stretch) and wide gentle peaks (like O-H stretch) are found. The
#'   spectra is smoothed by a Savitzky-Golay filter prior to analysis and as
#'   such there are a number of optional tuning parameters that can be provided
#'   (the defaults work well for typical spectra).
#'
#'   Cette fonction permet de trouver des pics dans les spectres IRTF en
#'   identifiant les minima de la double dérivée, puis en recherchant à nouveau
#'   les maxima des pics manqués par la méthode de la dérivée. Cette double
#'   vérification permet de s'assurer que les pics aigus (comme l'étirement C-H)
#'   et les pics larges et doux (comme l'étirement O-H) sont trouvés. Le spectre
#'   est lissé par un filtre de Savitzky-Golay avant l'analyse et, à ce titre,
#'   un certain nombre de paramètres de réglage facultatifs peuvent être fournis
#'   (les valeurs par défaut fonctionnent bien pour les spectres typiques).
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`. The `absorbance` column may be
#'   replaced by a `transmittance` column for transmittance plots.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`. La colonne `absorbance` peut
#'   être remplacée par une colonne `transmittance` pour les tracés de
#'   transmittance.
#' @param ... Additional optional parameters to pass to peak finding algorithm.
#' * `sg_p_norm` The polynomial degree used in smoothing the spectra for finding peaks by signal maxima. Default `3`.
#' * `sg_p_deriv` The polynomial degree used in smoothing the derivative for finding peaks by minima. Default `3`.
#' * `sg_n_norm` The number of points used in smoothing the spectra for finding peaks by signal maxima. Default `13`.
#' * `sg_n_deriv` The number of points used in smoothing the derivative for finding peaks by minima. Default `13`.
#' * `window_norm` The width of the window (in wavenumbers) to ensure that a peak is a true maxima and not just noise. Default `10`. Works best on data with consistent resolution, and will round up to the next data point.
#' * `window_deriv` The width of the window (in wavenumbers) to ensure that a derivative minima is a true minima and not just noise. Default `5`. Works best on data with consistent resolution, and will round up to the next data point.
#' * `window_align` The width of the window (in wavenumbers) whereby derivative and normal peaks are compared. Normal peaks are added to the derivative peak list if they are outside of the window distance of any other peak
#' * `zero_norm` Spectra have baseline noise removed before searching for peaks by setting signal value below the zero threshold to 0. Default `1e-2`.
#' * `zero_deriv`Derivative have baseline noise removed before searching for peaks by setting values below the zero threshold to 0. Default `1e-4`.
#'
#'
#'   Paramètres optionnels supplémentaires à transmettre à l'algorithme de
#'   recherche de pics. #' * `sg_p_norm` Le degré polynomial utilisé pour lisser
#'   les spectres afin de trouver les pics par les maxima du signal. Valeur par
#'   défaut `3`.
#' * `sg_p_deriv` Le degré polynomial utilisé dans le lissage de la dérivée pour trouver les pics par les minima. Par défaut `3`.
#' * `sg_n_norm` Le nombre de points utilisés pour lisser les spectres afin de trouver les pics par maxima du signal. Valeur par défaut `13`.
#' * `sg_n_deriv` Le nombre de points utilisés dans le lissage de la dérivée pour trouver les pics par minima. Par défaut `13`.
#' * `window_norm` La largeur de la fenêtre (en wavenumbers) pour s'assurer qu'un pic est un vrai maxima et pas seulement du bruit. Valeur par défaut `10`. Fonctionne mieux sur des données avec une résolution cohérente, et arrondit au point de données suivant.
#' * `window_deriv` La largeur de la fenêtre (en wavenumbers) pour s'assurer qu'un minima de dérivée est un vrai minima et pas seulement du bruit. Valeur par défaut `5`. Fonctionne mieux sur des données avec une résolution cohérente, et arrondira au point de données suivant.
#' * `window_align` La largeur de la fenêtre (en wavenumbers) par laquelle les pics dérivés et normaux sont comparés. Les pics normaux sont ajoutés à la liste des pics dérivés s'ils se trouvent à l'extérieur de la distance de la fenêtre de tout autre pic.
#' * `zero_norm` Les spectres sont débarrassés du bruit de base avant de rechercher les pics en fixant à 0 la valeur du signal en dessous du seuil zéro. Valeur par défaut `1e-2`.
#' * `zero_deriv`La dérivée est débarrassée du bruit de base avant la recherche des pics en fixant à 0 les valeurs inférieures au seuil zéro. Valeur par défaut `1e-4`.
#' @return A vector of wavenumbers corresponding to peaks found in the provided
#'   FTIR spectra.
#'
#'   Un vecteur de nombres d'ondes correspondant aux pics trouvés dans les
#'   spectres IRTF fournis.
#' @export
#' @seealso [signal::sgolayfilt()], [smooth_ftir()], [shift_baseline()]
#' @md
#' @references Savitzky, A.; Golay, M.J.E. (1964). "Smoothing and
#'   Differentiation of Data by Simplified Least Squares Procedures". Analytical
#'   Chemistry 36. pp. 1627–1639. doi:10.1021/ac60214a047
#' @examples
#' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Find peaks using default settings
#' peaks_default <- find_ftir_peaks(ftir_data)
#' print("Peaks found with default settings:")
#' print(peaks_default)
#'
#' # Find peaks with adjusted smoothing and window parameters
#' # Example: Less smoothing on derivative, wider window for normal peaks
#' peaks_adjusted <- find_ftir_peaks(
#'   ftir_data,
#'   sg_n_deriv = 11, # Fewer points for derivative smoothing
#'   window_norm = 15 # Wider window (wavenumbers) for normal peak check
#' )
#' print("Peaks found with adjusted settings:")
#' print(peaks_adjusted)
find_ftir_peaks <- function(ftir, ...) {
  #check args
  ftir <- PlotFTIR::check_ftir_data(ftir)

  if (length(unique(ftir$sample_id)) != 1) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::find_ftir_peaks}. {.arg ftir} must only contain one sample spectra."
    )
  }

  if (!("absorbance" %in% colnames(ftir))) {
    # because this just returns a wavenumber list, we can do this transformation
    # without angering the user
    ftir <- PlotFTIR::transmittance_to_absorbance(ftir)
  }

  args <- list(...)
  # assign from dots
  sg_p_norm <- `if`("sg_p_norm" %in% names(args), args$sg_p_norm, 3)
  sg_p_deriv <- `if`("sg_p_deriv" %in% names(args), args$sg_p_deriv, 3)
  sg_n_norm <- `if`("sg_n_norm" %in% names(args), args$sg_n_norm, 13)
  sg_n_deriv <- `if`("sg_n_deriv" %in% names(args), args$sg_n_deriv, 15)
  window_norm <- `if`("window_norm" %in% names(args), args$window_norm, 10)
  window_deriv <- `if`("window_deriv" %in% names(args), args$window_deriv, 5)
  window_align <- `if`("window_align" %in% names(args), args$window_align, 10)
  zero_norm <- `if`("zero_norm" %in% names(args), args$zero_norm, 1e-2)
  zero_deriv <- `if`("zero_deriv" %in% names(args), args$zero_deriv, 1e-4)

  if (!is.numeric(zero_norm)) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::find_ftir_peaks}. {.arg zero_norm} must be numeric."
    )
  }
  if (!is.numeric(zero_deriv)) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::find_ftir_peaks}. {.arg zero_deriv} must be numeric."
    )
  }

  sg <- signal::sgolayfilt(ftir$absorbance, p = sg_p_norm, n = sg_n_norm, m = 0)
  sg_deriv <- signal::sgolayfilt(
    ftir$absorbance,
    p = sg_p_deriv,
    n = sg_n_deriv,
    m = 2
  )

  if (zero_norm > max(abs(sg), na.rm = TRUE)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::find_ftir_peaks}. {.arg zero_norm} is larger than the highest point in the spectra.",
      i = "Set {.arg zero_norm} to remove noise, typically around 1e-4."
    ))
  }
  if (zero_deriv > max(abs(sg_deriv), na.rm = TRUE)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::find_ftir_peaks}. {.arg zero_deriv} is larger than the highest point in the derivative spectra.",
      i = "Set {.arg zero_deriv} to remove noise, typically around 1e-4."
    ))
  }

  # need resolution to convert windows in wavenumbers to data index units by
  # ceiling window/resolution. E.g. window of 10 wavenumber, 4 wavenumber
  # resolution -> peaks must be 10/4 = 3 data index apart (i.e. a peak can't be
  # at 2000 cm-1 and 2008 cm-1, next option is 2012 cm-1)
  resolution <- abs(mean(diff(ftir$wavenumber)))

  deriv_peaks <- ftir$wavenumber[minima(
    zero_threshold(sg_deriv, zero_deriv),
    ceiling(window_deriv / resolution)
  )]
  norm_peaks <- ftir$wavenumber[maxima(
    zero_threshold(sg, zero_norm),
    ceiling(window_norm / resolution)
  )]

  all_peaks <- deriv_peaks
  for (i in seq_along(norm_peaks)) {
    if (sum(abs(all_peaks - norm_peaks[i]) < window_align) == 0) {
      all_peaks <- c(all_peaks, norm_peaks[i])
    }
  }

  all_peaks <- sort(all_peaks)

  # Gotta check that front and back edges aren't incorrectly IDd as peaks. This
  # happened sometimes when the spectra is flat at the edges (because of the
  # shape of the derivative)
  if (min(all_peaks, na.rm = TRUE) == min(ftir$wavenumber, na.rm = TRUE)) {
    last <- ftir[rank(ftir$wavenumber) == 1, "absorbance"]
    secondlast <- ftir[rank(ftir$wavenumber) == 1, "absorbance"]
    if (last <= secondlast) {
      all_peaks <- all_peaks[all_peaks != min(ftir$wavenumber, na.rm = TRUE)]
    }
  }

  if (max(all_peaks, na.rm = TRUE) == max(ftir$wavenumber, na.rm = TRUE)) {
    first <- ftir[
      rank(ftir$wavenumber) == length(ftir$wavenumber),
      "absorbance"
    ]
    second <- ftir[
      rank(ftir$wavenumber) == (length(ftir$wavenumber) - 1),
      "absorbance"
    ]
    if (first <= second) {
      all_peaks <- all_peaks[all_peaks != max(ftir$wavenumber, na.rm = TRUE)]
    }
  }

  return(all_peaks)
}


maxima <- function(x, window = 1) {
  # in this form, window is COUNTS of values away, not wavenumbers
  lenx <- length(x)
  x <- c(rep(-Inf, window), x, rep(-Inf, window))
  m <- c()
  for (i in seq_along(x)) {
    # don't evaluate in filler region
    if (i <= window) {
      next
    }
    if (i > (lenx + window)) {
      next
    }
    if (
      max(x[seq(i - window, length.out = window)], na.rm = TRUE) < x[i] &&
        max(x[seq(i + 1, length.out = window)], na.rm = TRUE) < x[i]
    ) {
      # x is a maxima
      m <- c(m, i - window)
    }
  }
  return(m)
}


minima <- function(x, window = 1) {
  return(maxima(x = x * -1, window))
}


zero_threshold <- function(x, threshold = 1e-4) {
  x[abs(x) < threshold] <- 0
  return(x)
}


#' Fit Peaks
#' @description Once peaks are found by [find_ftir_peaks()], they can be fitted
#'   by adjusting intensity (area) standard deviation (width), and shape
#'   parameters (gam, eta, and/or alpha). This can be done by
#'   Expectation-Maximization methods, implemented here by the `EMpeaksR`
#'   package's technique. Note that the spectra provided is shifted to baseline
#'   to reduce the work of the peak fitter in producing background noise.
#'
#'   Une fois les pics trouvés par [find_ftir_peaks()], ils peuvent être ajustés
#'   en ajustant l'intensité (surface), l'écart-type (largeur) et les paramètres
#'   de forme (gam, eta, et/ou alpha). Ceci peut être fait par des méthodes
#'   d'espérance-maximisation, implémentées ici par la technique du paquet
#'   `EMpeaksR`. Notez que le spectre fourni est décalé par rapport à la ligne
#'   de base afin de réduire le travail de l'ajusteur de pics en produisant un
#'   bruit de fond.
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`.
#' @param peaklist The locations of peaks from `[find_ftir_peaks()]`. If none
#'   provided, will search for peaks using the default parameters of that
#'   function. Note that you could provide a common list of peaks for fitting
#'   multiple different spectra to compare results between samples.
#'
#'   Les emplacements des pics de `[find_ftir_peaks()]`. Si aucune valeur n'est
#'   fournie, les pics seront recherchés en utilisant les paramètres par défaut
#'   de cette fonction. Notez que vous pouvez fournir une liste commune de pics
#'   pour l'ajustement de plusieurs spectres différents afin de comparer les
#'   résultats entre les échantillons.
#' @param method The peak style / fitting method. Theoretically FTIR peaks are
#'   Lorentzian shaped, but with Gaussian broadening the pseudo-Voigt shape
#'   matches best. Some success is seen using Doniach-Šunjić-Gauss peak shapes
#'   since these can adopt undetected shoulder peaks in an asymmetric measure
#'   for each peak. Options are:
#' * `voigt` (default): Fit Voigt shaped peaks [EMpeaksR::spect_em_pvmm()]
#' * `gauss` Fit Gauss shaped peaks [EMpeaksR::spect_em_gmm()]
#' * `lorentz` Fit Lorentz shaped peaks [EMpeaksR::spect_em_lmm()]
#' * `dsg` Fit Doniach-Šunjić-Gauss shaped peaks [EMpeaksR::spect_em_dsgmm()]
#'
#'   Le style des pics / la méthode d'ajustement. En théorie, les pics IRTF ont
#'   une forme de Lorentz, mais avec un élargissement Gaussien, c'est la forme
#'   pseudo-Voigt qui convient le mieux. Les formes de pics de
#'   Doniach-Šunjić-Gauss donnent de bons résultats, car elles permettent
#'   d'adopter des pics d'épaulement non détectés dans le cadre d'une mesure
#'   asymétrique pour chaque pic. Les options sont les suivantes :
#' * `voigt` (par défaut) : Ajuster les pics en forme de Voigt [EMpeaksR::spect_em_pvmm()]
#' * `gauss` Ajuster les pics en forme de Gauss [EMpeaksR::spect_em_gmm()]
#' * `lorentz` Ajuster les pics en forme de Lorentz [EMpeaksR::spect_em_lmm()]
#' * `dsg` Ajuster les pics en forme de Doniach-Šunjić-Gauss [EMpeaksR::spect_em_dsgmm()]
#' @param fixed_peaks Boolean, whether to fix the peak locations to the provided
#'   values or allow the optimizer to move peaks as needed.
#'
#'   Booléen, pour savoir s'il faut fixer l'emplacement des pics aux valeurs
#'   fournies ou permettre à l'optimiseur de déplacer les pics selon les
#'   besoins.
#' @param ... Control parameters for fitting functions (`conv_cri` and/or
#'   `maxit`) or additional parameters to pass to [find_ftir_peaks()] if needed.
#'   Paramètres de contrôle pour les fonctions d'ajustement (`conv_cri` et/ou
#'   `maxit`) ou paramètres supplémentaires à passer à [find_ftir_peaks()] si
#'   nécessaire.
#' @return An `EMpeaksR` style fitted model. See the documentation for each peak
#'   shape.
#'
#'   Un modèle ajusté de type `EMpeaksR`. Voir la documentation pour chaque forme de pic.
#' @export
#' @md
#' @seealso [spect_em_gmm()], [spect_em_lmm()], [spect_em_pvmm()],
#'   [spect_em_dsgmm()]
#' @references Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y.
#' (2019) "Spectrum adapted expectation-maximization algorithm for
#' high-throughput peak shift analysis". Science and technology of advanced
#' materials, 20(1), pp 733-745. doi:10.1080/14686996.2019.1620123 Matsumura,
#' T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2021) "Spectrum adapted
#' expectation-conditional maximization algorithm for extending high–throughput
#' peak separation method in XPS analysis". Science and Technology of Advanced
#' Materials: Methods, 1(1), pp 45-55. doi:10.1080/27660400.2021.1899449
#' @examples
#' #' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Choose a subset of the data (reducing run time)
#' ftir_data <- ftir_data[
#'   ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
#' ]
#'
#' # Example 1: Fit peaks using the default 'voigt' method
#' # Peaks will be found automatically using find_ftir_peaks defaults
#' fitted_voigt_default <- fit_peaks(ftir_data)
#' print("Fitted Voigt Peaks (Default):")
#' 
#' # Show key results like final parameters and convergence status
#' print(fit_peak_df(fitted_voigt_default))
#' print(paste("Convergence:", fitted_voigt_default$convergence))
#'
#' \dontrun{
#' # Example 2: Fit peaks using the 'gauss' method
#' fitted_gauss <- fit_peaks(ftir_data, method = "gauss")
#' print("Fitted Gaussian Peaks:")
#' print(fit_peak_df(fitted_gauss))
#'
#' # Example 3: Provide a pre-defined list of peaks
#' # First, find some peaks (maybe with custom settings)
#' initial_peaks <- find_ftir_peaks(ftir_data, window_norm = 20)
#' print("Initial peaks found:")
#' print(initial_peaks)
#' # Now fit using this specific list
#' fitted_voigt_custom_peaks <- fit_peaks(ftir_data, peaklist = initial_peaks)
#' print("Fitted Voigt Peaks (Custom Initial List):")
#' print(fit_peak_df(fitted_voigt_custom_peaks))
#'
#' # Example 4: Fit peaks but keep their locations fixed
#' # Use a smaller subset of peaks for demonstration
#' fixed_peak_locations <- c(1130, 1375, 1460)
#' fitted_voigt_fixed <- fit_peaks(
#'   ftir_data,
#'   peaklist = fixed_peak_locations,
#'   fixed_peaks = TRUE
#'  )
#' print("Fitted Voigt Peaks (Fixed Locations):")
#' print(fit_peak_df(fitted_voigt_fixed))
#'
#' # Example 5: Pass control parameters (e.g., lower convergence criterion)
#' # Note: This might take longer or behave differently
#' fitted_voigt_tight_conv <- fit_peaks(
#'   ftir_data,
#'   conv_cri = 1e-4 # Tighter convergence
#' )
#' print("Fitted Voigt Peaks (Tighter Convergence):")
#' print(paste("Iterations:", fitted_voigt_tight_conv$it))
#' print(paste("Convergence:", fitted_voigt_tight_conv$convergence))
#' }
fit_peaks <- function(
  ftir,
  peaklist = NA,
  method = "voigt",
  fixed_peaks = FALSE,
  ...
) {
  PlotFTIR::check_ftir_data(ftir)

  if (!("absorbance" %in% colnames(ftir))) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::fit_peaks}. {.arg ftir} must be supplied in absorbance units."
    )
  }

  if (length(unique(ftir$sample_id)) != 1) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::fit_peaks}. {.arg ftir} must only contain one sample spectra."
    )
  }

  if (
    !(tolower(method) %in%
      c(
        "v",
        "pv",
        "voigt",
        "pseudo-voigt",
        "gauss",
        "gaussian",
        "normal",
        "g",
        "dsg",
        "doniach-\u0161unji\u0107-gauss",
        "doniach-sunjic-gauss",
        "l",
        "lorentz"
      ))
  ) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::fit_peaks}. {.arg method} must be one of {.code voigt}, {.code lorentz}, {.code gauss} or {.code dsg}."
    )
  }

  args <- list(...)

  if (all(is.na(peaklist))) {
    peaklist <- find_ftir_peaks(
      ftir,
      ... = args[
        !(names(args) %in%
          c("sigma", "mix_ratio", "eta", "gam", "alpha", "maxit", "conv_cri"))
      ]
    )
  }
  n <- length(peaklist)

  # sort out optional args
  # `if` documented by Hadley http://adv-r.had.co.nz/Functions.html
  conv_cri <- `if`("conv_cri" %in% names(args), args$conv_cri, 1e-2)
  maxit <- `if`("maxit" %in% names(args), args$maxit, 1e3)
  sigma <- `if`("sigma" %in% names(args), args$sigma, rep(10, n))
  gam <- `if`("gam" %in% names(args), args$gam, rep(10, n))
  mix_ratio <- `if`("mix_ratio" %in% names(args), args$mix_ratio, rep(1 / n, n))
  eta <- `if`("eta" %in% names(args), args$eta, rep(0.5, n))
  alpha <- `if`("alpha" %in% names(args), args$alpha, rep(1e-4, n))

  # simple baseline the ftir to minimize the work of peaks bringing up the noise.
  ftir$absorbance <- ftir$absorbance - min(abs(ftir$absorbance), na.rm = TRUE)

  if (tolower(method) %in% c("g", "gauss", "gaussian", "normal")) {
    method <- "gauss"
    utils::capture.output(
      res <- spect_em_gmm(
        x = ftir$wavenumber,
        y = ftir$absorbance,
        mu = peaklist,
        sigma = sigma,
        mix_ratio = mix_ratio,
        maxit = maxit,
        conv_cri = conv_cri,
        fixed_mu = fixed_peaks
      ),
      file = nullfile()
    )
  } else if (tolower(method) %in% c("v", "pv", "voigt", "pseudo-voigt")) {
    method <- "voigt"
    utils::capture.output(
      res <- spect_em_pvmm(
        x = ftir$wavenumber,
        y = ftir$absorbance,
        mu = peaklist,
        sigma = sigma,
        eta = eta,
        mix_ratio = mix_ratio,
        maxit = maxit,
        conv_cri = conv_cri,
        fixed_mu = fixed_peaks
      ),
      file = nullfile()
    )
  } else if (tolower(method) %in% c("l", "lorentz")) {
    method <- "lorentz"
    utils::capture.output(
      res <- spect_em_lmm(
        x = ftir$wavenumber,
        y = ftir$absorbance,
        mu = peaklist,
        gam = gam,
        mix_ratio = mix_ratio,
        maxit = maxit,
        conv_cri = conv_cri,
        fixed_mu = fixed_peaks
      ),
      file = nullfile()
    )
  } else {
    method <- "doniach-\u0161unji\u0107-gauss"

    utils::capture.output(
      res <- spect_em_dsgmm(
        x = ftir$wavenumber,
        y = ftir$absorbance,
        mu = peaklist,
        sigma = sigma,
        alpha = alpha,
        eta = eta,
        mix_ratio = mix_ratio,
        maxit = maxit,
        conv_cri = conv_cri,
        fixed_mu = fixed_peaks
      ),
      file = nullfile()
    )
  }
  res$method <- method
  res$sample_id <- unique(ftir$sample_id)
  res$fixed_peaks <- fixed_peaks

  return(res)
}


#' Fitted Peaks Data.Frame
#' @description Reformat the [fit_peaks()] object to a data.frame of peak
#' specifications.
#'
#' Reformater l'objet [fit_peaks()] en un data.frame de spécifications de pics.
#' @param fitted_peaks An object from [fit_peaks()].
#'
#'   Un objet de [fit_peaks()].
#' @returns A data.frame of peak properties.
#'
#'   Un data.frame des propriétés des pics.
#' @export
#' @examples
#' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Choose a subset of the data (reducing run time)
#' ftir_data <- ftir_data[
#'   ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
#' ]
#'

#' # First, fit the peaks (using the default 'voigt' method)
#' fitted_voigt <- fit_peaks(ftir_data, method = "voigt")
#'
#' # Now, convert the fitted model object to a data frame
#' peak_df_voigt <- fit_peak_df(fitted_voigt)
#'
#' print("Peak Data Frame from Voigt Fit:")
#' print(peak_df_voigt)
fit_peak_df <- function(fitted_peaks) {
  peak_table <- data.frame(
    "sample_id" = fitted_peaks$sample_id,
    "peak" = seq_along(fitted_peaks$mu),
    "wavenumber" = fitted_peaks$mu
  )

  if ("sigma" %in% names(fitted_peaks)) {
    peak_table$sigma <- fitted_peaks$sigma
  }
  if ("gam" %in% names(fitted_peaks)) {
    peak_table$gam <- fitted_peaks$gam
  }
  if ("eta" %in% names(fitted_peaks)) {
    peak_table$eta <- fitted_peaks$eta
  }
  if ("alpha" %in% names(fitted_peaks)) {
    peak_table$alpha <- fitted_peaks$alpha
  }
  peak_table$mix_ratio <- fitted_peaks$mix_ratio
  peak_table$peak_shape <- fitted_peaks$method

  peak_table <- peak_table[order(peak_table$wavenumber), ]

  return(peak_table)
}


#' Get Fit Method
#'
#' @description Determine the types of peaks used to create the [fit_peaks()]
#'   object.
#'
#'   Déterminez les types de pics utilisés pour créer l'objet [fit_peaks()].
#'
#' @param fitted_peaks An object from [fit_peaks()].
#'
#'   Un objet de [fit_peaks()].
#'
#' @return A character value for the peak type fitted to the spectra.
#'
#'   Une valeur de caractère pour le type de pic ajusté aux spectres.
#' @keywords internal
get_fit_method <- function(fitted_peaks) {
  if (!("method" %in% names(fitted_peaks))) {
    cli::cli_warn(
      "Warning in {.fn PlotFTIR::get_fit_method}. {.arg fitted_peaks} should be generated with {.fn PlotFTIR::fit_peaks}."
    )
    if ("alpha" %in% names(fitted_peaks)) {
      method <- "doniach-\u0161unji\u0107-gauss"
    } else if ("gam" %in% names(fitted_peaks)) {
      method <- "lorentz"
    } else if ("eta" %in% names(fitted_peaks)) {
      method <- "voigt"
    } else {
      method <- "gauss"
    }
  } else {
    method <- fitted_peaks$method
  }
  return(method)
}


#' Get Fit Spectra
#'
#' @description Given a fitted peak object and the FTIR source of the fit,
#'   produce a resultant `absorbance` column. Typically used to plot fitted
#'   peaks or calculate residuals.
#'
#'   Étant donné un objet pic IRTF ajusté et la source IRTF de l'ajustement,
#'   produire une colonne `absorbance` résultante. Généralement utilisé pour
#'   tracer les pics ajustés ou calculer les résidus.
#'
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`.
#' @param fitted_peaks An object from [fit_peaks()]. Should match the provided
#'   sample in `ftir`.
#'
#'   Un objet de [fit_peaks()]. Doit correspondre à l'échantillon fourni dans
#'   `ftir`.
#' @param peak A peak index if getting single peak spectra, else returns the sum
#'   of all fitted peaks.
#'
#'   Un index des pics si l'on obtient des spectres à un seul pic, sinon la
#'   somme de tous les pics ajustés est renvoyée.
#'
#' @return The calculated absorbance intensities as numeric vector of the same
#'   length as the FTIR spectra.
#'
#'   Les intensités d'absorption calculées sous forme de tableau numérique de
#'   même longueur que les spectres IRTF.
#' @keywords internal
#' @references Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y.
#' (2019) "Spectrum adapted expectation-maximization algorithm for
#' high-throughput peak shift analysis". Science and technology of advanced
#' materials, 20(1), pp 733-745. doi:10.1080/14686996.2019.1620123 Matsumura,
#' T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2021) "Spectrum adapted
#' expectation-conditional maximization algorithm for extending high–throughput
#' peak separation method in XPS analysis". Science and Technology of Advanced
#' Materials: Methods, 1(1), pp 45-55. doi:10.1080/27660400.2021.1899449
get_fit_spectra <- function(ftir, fitted_peaks, peak = NULL) {
  PlotFTIR::check_ftir_data(ftir)
  method <- get_fit_method(fitted_peaks)
  if (!is.null(peak)) {
    if (!is.numeric(peak)) {
      cli::cli_abort(
        "Error in PlotFTIR:::get_fit_spectra: requested peak must be an integer value. You provided {.obj_type_friendly {peak}}."
      )
    } else if (peak %% 1 != 0) {
      cli::cli_abort(
        "Error in PlotFTIR:::get_fit_spectra: requested peak must be an integer value. You provided something with decimals."
      )
    }
    if (peak > length(fitted_peaks$mu) || peak < 1) {
      cli::cli_abort(
        "Error in PlotFTIR:::get_fit_spectra: requested peak {.val {peak}} is out of range, only {{length(fitted_peaks$mu}} peaks are fitted."
      )
    }
  }

  if (method == "gauss") {
    y <- Reduce(
      "+",
      lapply(
        seq_along(fitted_peaks$mu),
        FUN = function(x) {
          fitted_peaks$mix_ratio[x] *
            truncated_g(
              ftir$wavenumber,
              mu = fitted_peaks$mu[x],
              sigma = fitted_peaks$sigma[x]
            )
        }
      )
    )
  } else if (method == "voigt") {
    y <- Reduce(
      "+",
      lapply(
        seq_along(fitted_peaks$mu),
        FUN = function(x) {
          fitted_peaks$mix_ratio[x] *
            truncated_pv(
              ftir$wavenumber,
              mu = fitted_peaks$mu[x],
              sigma = fitted_peaks$sigma[x],
              eta = fitted_peaks$eta[x]
            )
        }
      )
    )
  } else if (method == "lorentz") {
    y <- Reduce(
      "+",
      lapply(
        seq_along(fitted_peaks$mu),
        FUN = function(x) {
          fitted_peaks$mix_ratio[x] *
            truncated_l(
              ftir$wavenumber,
              mu = fitted_peaks$mu[x],
              gam = fitted_peaks$gam[x]
            )
        }
      )
    )
  } else {
    y <- Reduce(
      "+",
      lapply(
        seq_along(fitted_peaks$mu),
        FUN = function(x) {
          fitted_peaks$mix_ratio[x] *
            truncated_dsg(
              ftir$wavenumber,
              mu = fitted_peaks$mu[x],
              sigma = fitted_peaks$sigma[x],
              alpha = fitted_peaks$alpha[x],
              eta = fitted_peaks$eta[x]
            )
        }
      )
    )
  }

  scale_factor <- (1 / max(y, na.rm = TRUE)) *
    max(ftir$absorbance, na.rm = TRUE)

  if (is.null(peak)) {
    return(y * scale_factor)
  }

  # we need to only produce a single peak. We did all of the fitting math to
  # determine the scale factor, but now recalculate the peak of interest.
  if (method == "gauss") {
    y <- fitted_peaks$mix_ratio[peak] *
      truncated_g(
        ftir$wavenumber,
        mu = fitted_peaks$mu[peak],
        sigma = fitted_peaks$sigma[peak]
      )
  } else if (method == "voigt") {
    y <- fitted_peaks$mix_ratio[peak] *
      truncated_pv(
        ftir$wavenumber,
        mu = fitted_peaks$mu[peak],
        sigma = fitted_peaks$sigma[peak],
        eta = fitted_peaks$eta[peak]
      )
  } else if (method == "lorentz") {
    y <- fitted_peaks$mix_ratio[peak] *
      truncated_l(
        ftir$wavenumber,
        mu = fitted_peaks$mu[peak],
        gam = fitted_peaks$gam[peak]
      )
  } else {
    y <- fitted_peaks$mix_ratio[peak] *
      truncated_dsg(
        ftir$wavenumber,
        mu = fitted_peaks$mu[peak],
        sigma = fitted_peaks$sigma[peak],
        alpha = fitted_peaks$alpha[peak],
        eta = fitted_peaks$eta[peak]
      )
  }
  return(y * scale_factor)
}


# peak_fit_plots

#' Plot Components
#' @description Produces a plot of components of the peak fitting results.
#'
#'   Produit un graphique des composantes des résultats de l'ajustement des
#'   pics.
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`.
#' @param fitted_peaks An object from [fit_peaks()]. Should match the provided
#'   sample in `ftir`.
#'
#'   Un objet de [fit_peaks()]. Doit correspondre à l'échantillon fourni dans
#'   `ftir`.
#' @param plot_fit Boolean, whether to plot the peak fit (default FALSE)
#'
#'   Booléen, pour savoir s'il faut tracer l'ajustement du pic (par défaut
#'   FALSE)
#' @param lang An optional argument for language. If set to one of `fr`,
#'   `french`, `francais`, or `français` the axis and default plot and legend
#'   titles will change to french. If non-default legend or plot titles are
#'   provided they are used as-is. You can also provide `en`, `english` or
#'   `anglais`, or (the default) `NA` will use the default language from user
#'   options. To set a permanent default, set `options("PlotFTIR.lang" = "en")`
#'   or `options("PlotFTIR.lang" = "fr")` for English or French, respectively.
#'
#'   Un argument optionnel pour la langue. S'il vaut `Fr`, `French`, `Francais`,
#'   ou `Français`, l'axe et les titres par défaut de le tracé et du légende
#'   seront en français. Si des titres du légende ou de tracé autres que ceux
#'   par défaut sont fournis, ils seront utilisés tels quels. Vous pouvez aussi
#'   fournir `en`, `english` ou `anglais`, ou (le défaut) `NA` qui utilisera le
#'   langue par défaut des options de l'utilisateur. Pour définir une valeur
#'   par défaut permanente, mettez `options("PlotFTIR.lang" = "en")` ou
#'   `options("PlotFTIR.lang" = "fr")` pour l'anglais ou le français,
#'   respectivement.
#' @param ... optional argument `fitted_sample_name` for naming the fitted peaks
#'   on the plot, or extra parameters to pass to [PlotFTIR::plot_ftir()].
#'
#'   Argument optionnel `fitted_sample_name` pour nommer les pics ajustés sur le
#'   graphique, ou des paramètres supplémentaires à passer à
#'   [PlotFTIR::plot_ftir()].
#' @returns A [PlotFTIR] graphic with residuals plotted against wavenumber
#'
#' Un graphique [PlotFTIR] avec les résidus tracés en fonction du nombre d'ondes
#' @export
#' @examples
#' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' #' # Choose a subset of the data (reducing run time)
#' ftir_data <- ftir_data[
#'   ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
#' ]
#'
#' # First, fit the peaks using the default 'voigt' method
#' fitted_voigt <- fit_peaks(ftir_data, method = "voigt")
#'
#' # --- Example 1: Plot components only (default) ---
#' \dontrun{
#'   plot_components(ftir_data, fitted_voigt)
#' }
#'
#' # --- Example 2: Plot components AND the overall fitted sum ---
#' \dontrun{
#'   plot_components(ftir_data, fitted_voigt, plot_fit = TRUE)
#' }
#'
#' # --- Example 3: Plot components and fit with custom titles and name ---
#' \dontrun{
#'   plot_components(
#'     ftir_data,
#'     fitted_voigt,
#'     plot_fit = TRUE,
#'     plot_title = c("Isopropanol Peak Fit", "Voigt Components"),
#'     legend_title = "Spectrum Type",
#'     fitted_sample_name = "Total Fit (Voigt)"
#'   )
#' }
#'
#' # --- Example 4: Plot components in French ---
#' \dontrun{
#'   plot_components(ftir_data, fitted_voigt, plot_fit = TRUE, lang = "fr")
#' }
plot_components <- function(
  ftir,
  fitted_peaks,
  plot_fit = FALSE,
  lang = NA,
  ...
) {
  PlotFTIR::check_ftir_data(ftir)
  if (!("absorbance" %in% colnames(ftir))) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_components}. {.arg ftir} must be supplied in absorbance units."
    )
  }
  if (length(unique(ftir$sample_id)) != 1) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_components}. {.arg ftir} must only contain one sample spectra."
    )
  }

  lang <- process_language(lang)

  # simple baseline the ftir to minimize the work of peaks bringing up the noise.
  ftir$absorbance <- ftir$absorbance - min(abs(ftir$absorbance), na.rm = TRUE)

  argnames <- names(list(...))
  if (
    any(
      !(argnames %in%
        c("plot_title", "legend_title", "lang", "fitted_sample_name"))
    )
  ) {
    unused <- argnames[
      !(argnames %in%
        c("plot_title", "legend_title", "lang", "fitted_sample_name"))
    ]
    lun <- length(unused)
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_components}. Supplied {lun} unused argument{?s}: {argnames}."
    )
  }

  args <- list(...)

  legend_title <- `if`(
    "legend_title" %in% argnames,
    args$legend_title,
    ifelse(lang == "en", "Sample ID", "ID de l'\u00e9chantillon")
  )

  method <- get_fit_method(fitted_peaks)

  if (!("sample_id" %in% names(fitted_peaks))) {
    cli::cli_warn(
      "Warning in {.fn FTIRtools::plot_components}. {.arg fitted_peaks} should be generated with {.fn FTIRtools::fit_peaks}."
    )
    fitted_peaks$sample_id <- ""
  } else if (fitted_peaks$sample_id != unique(ftir$sample_id)) {
    cli::cli_warn(c(
      "Warning in {.fn FTIRtools::plot_components}. {.arg fitted_peaks} does not contain fit peaks that match the ftir sample provided.",
      i = 'The peaks were fit for sample "{fitted_peaks$sample_id}" and you provided "{unique(ftir$sample_id)[1]}".'
    ))
  }

  if ("plot_title" %in% argnames) {
    plot_title <- args$plot_title
  } else {
    if (lang == "en") {
      plot_title <- c(
        "Fitted FTIR Plot",
        paste0(
          "Showing as-analyzed spectra and components of ",
          tools::toTitleCase(method),
          " fitted peaks"
        )
      )
    } else {
      plot_title <- c(
        "Trac\u00e9 IRTF ajust\u00e9",
        paste0(
          "Montrer les spectres et es composants analys\u00e9s de pics ajust\u00e9 par la m\u00e9thode ",
          tools::toTitleCase(method)
        )
      )
    }
  }

  n_peaks <- length(fitted_peaks$mu)
  fit_spectra <- data.frame(
    wavenumber = numeric(),
    absorbance = numeric(),
    sample_id = character()
  )
  for (i in seq(n_peaks)) {
    s <- paste("Component", i)
    y <- get_fit_spectra(ftir, fitted_peaks, i)
    fit_spectra <- rbind(
      fit_spectra,
      data.frame(wavenumber = ftir$wavenumber, absorbance = y, sample_id = s)
    )
  }

  plotdata <- ftir

  if (plot_fit) {
    fitted_sample_name <- `if`(
      "fitted_sample_name" %in% argnames,
      args$fitted_sample_name,
      ifelse(
        lang == "en",
        trimws(paste("fitted", fitted_peaks$sample_id)),
        trimws(paste(fitted_peaks$sample_id, "ajust\u00e9"))
      )
    )
    fitted_sample <- data.frame(
      wavenumber = ftir$wavenumber,
      absorbance = get_fit_spectra(ftir, fitted_peaks = fitted_peaks),
      sample_id = fitted_sample_name
    )
    plotdata <- rbind(plotdata, fitted_sample)
  }

  plotdata <- rbind(plotdata, fit_spectra)

  # Now need to reorder factor levels to make plot logical
  # First original sample, next fitted, afterwards remaining components.
  if (plot_fit) {
    sampleids <- unique(plotdata$sample_id)
    plotdata$sample_id <- factor(
      plotdata$sample_id,
      c(
        ftir$sample_id[1],
        fitted_sample_name,
        sampleids[!(sampleids %in% c(ftir$sample_id[1], fitted_sample_name))]
      )
    )
  } else {
    sampleids <- unique(plotdata$sample_id)
    plotdata$sample_id <- factor(
      plotdata$sample_id,
      c(ftir$sample_id[1], sampleids[!(sampleids %in% c(ftir$sample_id[1]))])
    )
  }

  # This will warn about too many samples if not suppressed
  p <- suppressWarnings(PlotFTIR::plot_ftir(
    plotdata,
    plot_title = plot_title,
    legend_title = legend_title,
    lang = lang
  ))

  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    suppressWarnings(
      p <- p +
        ggplot2::scale_color_viridis_d()
    )
  } else {
    suppressWarnings(
      p <- p +
        ggthemes::scale_color_calc()
    )
  }

  if (plot_fit) {
    utils::capture.output(
      p <- PlotFTIR::highlight_sample(
        p,
        c(ftir$sample_id[1], fitted_sample_name)
      ),
      file = nullfile()
    )
  } else {
    utils::capture.output(
      p <- PlotFTIR::highlight_sample(
        p,
        ftir$sample_id[1]
      ),
      file = nullfile()
    )
  }

  return(p)
}


#' Plot Residuals
#' @description Produce a plot of the error between predicted and actual FTIR
#' spectra.
#'
#' Produisez un graphique de l'erreur entre les spectres IRTF prédits et réels.
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`.
#' @param fitted_peaks An object from [fit_peaks()]. Should match the provided
#'   sample in `ftir`.
#'
#'   Un objet de [fit_peaks()]. Doit correspondre à l'échantillon fourni dans
#'   `ftir`.
#' @param lang An optional argument for language. If set to one of `fr`,
#'   `french`, `francais`, or `français` the axis and default plot and legend
#'   titles will change to french. If non-default legend or plot titles are
#'   provided they are used as-is. You can also provide `en`, `english` or
#'   `anglais`, or (the default) `NA` will use the default language from user
#'   options. To set a permanent default, set `options("PlotFTIR.lang" = "en")`
#'   or `options("PlotFTIR.lang" = "fr")` for English or French, respectively.
#'
#'   Un argument optionnel pour la langue. S'il vaut `Fr`, `French`, `Francais`,
#'   ou `Français`, l'axe et les titres par défaut de le tracé et du légende
#'   seront en français. Si des titres du légende ou de tracé autres que ceux
#'   par défaut sont fournis, ils seront utilisés tels quels. Vous pouvez aussi
#'   fournir `en`, `english` ou `anglais`, ou (le défaut) `NA` qui utilisera le
#'   langue par défaut des options de l'utilisateur. Pour définir une valeur
#'   par défaut permanente, mettez `options("PlotFTIR.lang" = "en")` ou
#'   `options("PlotFTIR.lang" = "fr")` pour l'anglais ou le français,
#'   respectivement.
#' @param ... optional argument `fitted_sample_name` for naming the fitted peaks
#'   on the plot, or extra parameters to pass to [PlotFTIR::plot_ftir()].
#'
#'   Argument optionnel `fitted_sample_name` pour nommer les pics ajustés sur le
#'   graphique, ou des paramètres supplémentaires à passer à
#'   [PlotFTIR::plot_ftir()].
#' @returns A [PlotFTIR::plot_ftir()] graphic with residuals plotted against
#'   wavenumber.
#'
#'   Un graphique [PlotFTIR::plot_ftir()] avec les résidus tracés en fonction du
#'   nombre d'ondes.
#' @export
#' @examples
#' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Choose a subset of the data (reducing run time)
#' ftir_data <- ftir_data[
#'   ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
#' ]
#'
#' # First, fit the peaks using the default 'voigt' method
#' fitted_voigt <- fit_peaks(ftir_data, method = "voigt")
#'
#' # --- Example 1: Plot residuals with default settings ---
#' \dontrun{
#'   plot_fit_residuals(ftir_data, fitted_voigt)
#' }
#'
#' # --- Example 2: Plot residuals with custom titles in French ---
#' \dontrun{
#'   plot_fit_residuals(
#'     ftir_data,
#'     fitted_voigt,
#'     lang = "fr",
#'     plot_title = c(
#'       "R\u00e9sidus de l'ajustement",
#'       "Diff\u00e9rence entre le spectre et l'ajustement Voigt"
#'     )
#'   )
#' }
#'
plot_fit_residuals <- function(ftir, fitted_peaks, lang = NA, ...) {
  PlotFTIR::check_ftir_data(ftir)

  if (!("absorbance" %in% colnames(ftir))) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg ftir} must be supplied in absorbance units."
    )
  }
  if (length(unique(ftir$sample_id)) != 1) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg ftir} must only contain one sample spectra."
    )
  }

  if (!("sample_id" %in% names(fitted_peaks))) {
    cli::cli_warn(
      "Warning in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg fitted_peaks} should be generated with {.fn FTIRtools::fit_peaks}."
    )
    fitted_peaks$sample_id <- ""
  } else if (fitted_peaks$sample_id != unique(ftir$sample_id)) {
    cli::cli_warn(c(
      "Warning in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg fitted_peaks} does not contain fit peaks that match the ftir sample provided.",
      i = 'The peaks were fit for sample "{fitted_peaks$sample_id}" and you provided "{unique(ftir$sample_id)[1]}".'
    ))
  }

  lang <- process_language(lang)

  # simple baseline the ftir to minimize the work of peaks bringing up the noise.
  ftir$absorbance <- ftir$absorbance - min(abs(ftir$absorbance), na.rm = TRUE)

  method <- get_fit_method(fitted_peaks = fitted_peaks)

  fitted_y <- get_fit_spectra(ftir, fitted_peaks)

  residual <- fitted_y - ftir$absorbance

  plotdata <- data.frame(
    "wavenumber" = ftir$wavenumber,
    "absorbance" = residual,
    "sample_id" = ifelse(lang == "en", "Residual", "R\u00e9sidu")
  )

  argnames <- names(list(...))
  if (any(!(argnames %in% c("plot_title", "legend_title", "lang")))) {
    unused <- argnames[
      !(argnames %in%
        c("plot_title", "legend_title", "lang"))
    ]
    lun <- length(unused)
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_components}. Supplied {lun} unused argument{?s}: {argnames}."
    )
  }

  args <- list(...)

  legend_title <- `if`(
    "legend_title" %in% argnames,
    args$legend_title,
    ""
  )

  method <- get_fit_method(fitted_peaks)

  if ("plot_title" %in% argnames) {
    plot_title <- args$plot_title
  } else {
    if (lang == "en") {
      plot_title <- c(
        "Residual Plot",
        paste0(
          "Residual of ",
          tools::toTitleCase(method),
          " fitted peaks and ",
          unique(ftir$sample_id)
        )
      )
    } else {
      plot_title <- c(
        "Trac\u00e9 des r\u00e9sidus",
        paste0(
          "R\u00e9sidu de ",
          tools::toTitleCase(method),
          " pics ajust\u00e9s et ",
          unique(ftir$sample_id)
        )
      )
    }
  }

  return(suppressWarnings(PlotFTIR::plot_ftir(
    plotdata,
    plot_title = plot_title,
    legend_title = legend_title,
    lang = lang
  )))
}


#' Plot Fitted Peaks
#'
#' @description Plot the spectra and sum of fitted peaks from [fit_peaks()]
#'   using [PlotFTIR::plot_ftir()].
#'
#'   Tracez les spectres et la somme des pics ajustés à partir de [fit_peaks()]
#'   en utilisant [PlotFTIR::plot_ftir()].
#'
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`.
#' @param fitted_peaks An object from [fit_peaks()]. Should match the provided
#'   sample in `ftir`.
#'
#'   Un objet de [fit_peaks()]. Doit correspondre à l'échantillon fourni dans
#'   `ftir`.
#' @param plot_components Boolean, whether to include the component peaks (see
#'   [plot_components()]).
#'
#'   Booléen, pour savoir s'il faut inclure les pics des composants (voir
#'   [plot_components()]).
#' @param lang An optional argument for language. If set to one of `fr`,
#'   `french`, `francais`, or `français` the axis and default plot and legend
#'   titles will change to french. If non-default legend or plot titles are
#'   provided they are used as-is. You can also provide `en`, `english` or
#'   `anglais`, or (the default) `NA` will use the default language from user
#'   options. To set a permanent default, set `options("PlotFTIR.lang" = "en")`
#'   or `options("PlotFTIR.lang" = "fr")` for English or French, respectively.
#'
#'   Un argument optionnel pour la langue. S'il vaut `Fr`, `French`, `Francais`,
#'   ou `Français`, l'axe et les titres par défaut de le tracé et du légende
#'   seront en français. Si des titres du légende ou de tracé autres que ceux
#'   par défaut sont fournis, ils seront utilisés tels quels. Vous pouvez aussi
#'   fournir `en`, `english` ou `anglais`, ou (le défaut) `NA` qui utilisera le
#'   langue par défaut des options de l'utilisateur. Pour définir une valeur
#'   par défaut permanente, mettez `options("PlotFTIR.lang" = "en")` ou
#'   `options("PlotFTIR.lang" = "fr")` pour l'anglais ou le français,
#'   respectivement.
#' @param ... Optional argument `fitted_sample_name` for naming the fitted peaks
#'   on the plot, or extra parameters to pass to [PlotFTIR::plot_ftir()].
#'
#'   Argument optionnel `fitted_sample_name` pour nommer les pics ajustés sur le
#'   graphique, ou des paramètres supplémentaires à passer à
#'   [PlotFTIR::plot_ftir()].
#'
#' @return A [PlotFTIR::plot_ftir()] graphic.
#'
#'   Un graphique [PlotFTIR::plot_ftir()].
#' @export
#' @seealso [PlotFTIR::plot_ftir()]
#' @examples
#' # Load the isopropanol sample spectrum from the PlotFTIR package
#' ftir_data <- PlotFTIR::sample_spectra[
#'   PlotFTIR::sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Choose a subset of the data (reducing run time)
#' ftir_data <- ftir_data[
#'   ftir_data$wavenumber < 1500 & ftir_data$wavenumber > 1000,
#' ]
#'
#' if(!requireNamespace('signal', quietly = TRUE)){
#'   # First, fit the peaks using the default 'voigt' method
#'   fitted_voigt <- fit_peaks(ftir_data, method = "voigt")
#' }
#'
#' # --- Example 1: Plot original spectrum and the overall fitted sum ---
#' \dontrun{
#'   plot_fit_ftir_peaks(ftir_data, fitted_voigt)
#' }
#'
#' # --- Example 2: Plot original, overall fit, AND individual components ---
#' # This internally calls plot_components() with plot_fit = TRUE
#' \dontrun{
#'   plot_fit_ftir_peaks(ftir_data, fitted_voigt, plot_components = TRUE)
#' }
#'
#' # --- Example 3: Plot original and fit with custom titles and name ---
#' \dontrun{
#'   plot_fit_ftir_peaks(
#'     ftir_data,
#'     fitted_voigt,
#'     plot_title = c("Isopropanol Fit Comparison", "Original vs. Voigt Sum"),
#'     legend_title = "Spectrum Source",
#'     fitted_sample_name = "Total Voigt Fit"
#'   )
#' }
#'
#' # --- Example 4: Plot original and fit in French ---
#' \dontrun{
#'   plot_fit_ftir_peaks(ftir_data, fitted_voigt, lang = "fr")
#' }
#'
plot_fit_ftir_peaks <- function(
  ftir,
  fitted_peaks,
  plot_components = FALSE,
  lang = NA,
  ...
) {
  if (plot_components) {
    return(plot_components(
      ftir = ftir,
      fitted_peaks = fitted_peaks,
      plot_fit = TRUE,
      lang = lang,
      ... = ...
    ))
  }
  PlotFTIR::check_ftir_data(ftir)

  if (!("absorbance" %in% colnames(ftir))) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg ftir} must be supplied in absorbance units."
    )
  }
  if (length(unique(ftir$sample_id)) != 1) {
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg ftir} must only contain one sample spectra."
    )
  }

  lang <- process_language(lang)

  # simple baseline the ftir to minimize the work of peaks bringing up the noise.
  ftir$absorbance <- ftir$absorbance - min(abs(ftir$absorbance), na.rm = TRUE)

  argnames <- names(list(...))
  if (
    any(
      !(argnames %in%
        c("plot_title", "legend_title", "lang", "fitted_sample_name"))
    )
  ) {
    unused <- argnames[
      !(argnames %in%
        c("plot_title", "legend_title", "lang", "fitted_sample_name"))
    ]
    lun <- length(unused)
    cli::cli_abort(
      "Error in {.fn FTIRtools::plot_fit_ftir_peaks}. Supplied {lun} unused argument{?s}: {argnames}."
    )
  }

  args <- list(...)

  legend_title <- `if`(
    "legend_title" %in% argnames,
    args$legend_title,
    ifelse(lang == "en", "Sample ID", "ID de l'\u00e9chantillon")
  )

  method <- get_fit_method(fitted_peaks)

  if (!("sample_id" %in% names(fitted_peaks))) {
    cli::cli_warn(
      "Warning in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg fitted_peaks} should be generated with {.fn FTIRtools::fit_peaks}."
    )
    fitted_peaks$sample_id <- ""
  } else if (fitted_peaks$sample_id != unique(ftir$sample_id)) {
    cli::cli_warn(c(
      "Warning in {.fn FTIRtools::plot_fit_ftir_peaks}. {.arg fitted_peaks} does not contain fit peaks that match the ftir sample provided.",
      i = 'The peaks were fit for sample "{fitted_peaks$sample_id}" and you provided "{unique(ftir$sample_id)[1]}".'
    ))
  }

  fitted_sample_name <- ifelse(
    "fitted_sample_name" %in% argnames,
    args$fitted_sample_name,
    ifelse(
      lang == "en",
      trimws(paste("fitted", fitted_peaks$sample_id)),
      trimws(paste(fitted_peaks$sample_id, "ajust\u00e9"))
    )
  )
  if ("plot_title" %in% argnames) {
    plot_title <- args$plot_title
  } else {
    if (lang == "en") {
      plot_title <- c(
        "Fitted FTIR Plot",
        paste0(
          "Showing as-analyzed spectra and sum of ",
          tools::toTitleCase(method),
          " fitted peaks"
        )
      )
    } else {
      plot_title <- c(
        "Trac\u00e9 IRTF ajust\u00e9",
        paste0(
          "Montrer les spectres et de la somme des pics ajust\u00e9s par la m\u00e9thode ",
          tools::toTitleCase(method)
        )
      )
    }
  }

  fitted_y <- get_fit_spectra(ftir = ftir, fitted_peaks = fitted_peaks)

  plotdata <- data.frame(
    "wavenumber" = rep(ftir$wavenumber, 2),
    "absorbance" = c(ftir$absorbance, fitted_y),
    "sample_id" = c(ftir$sample_id, rep(fitted_sample_name, length(fitted_y)))
  )

  PlotFTIR::plot_ftir(
    plotdata,
    plot_title = plot_title,
    legend_title = legend_title,
    lang = lang
  )
}


process_language <- function(lang) {
  # if language is provided, check against permitted, else use default from options.
  l <- NA
  if (!is.na(lang)) {
    tryCatch(
      l <- match.arg(
        lang,
        choices = c(
          "en",
          "english",
          "anglais",
          "fr",
          "french",
          "francais",
          "fran\u00e7ais"
        ),
        several.ok = FALSE
      ),
      error = function(x) {
        cli::cli_warn(c(
          "Warning: language must be one of 'en', 'english', anglais', 'fr', 'french', 'francais' or 'fran\u00e7ais', not '{lang}'.",
          i = "Using default language '{getOption('FTIRtools.lang', default = 'en')}'."
        ))
      }
    )
  }
  if (is.na(l)) {
    # either lang was NA or failed the match.arg. Default to getOptions result
    l <- getOption("FTIRtools.lang", default = "en")
  }

  l <- substr(l, 0, 2)

  return(l)
}


# EMpeaksR algorithms modified with the following changes:
#  - removal of verbose printing (now optional as message for easier silencing)
#  - optionally optimize with fixed wavenumber (mu) values.
#  - upper bounds on sigma were lifted to 1000 instead of 100
#    (to allow for broader peaks (etc. supports -OH band > 3000))
#  - additional comments in-code and formatting by {{ Posit::air }} to aid readability
#  - additionally verbose documentation
#  - Error checking of provided parameter values.

# EMpeaksR is originally released under an MIT license and the license statement is
# listed in compliance with the requirements of this code. Nothing in this license
# statement affects any other portion of this package not contained in this file.

# YEAR: 2023
# COPYRIGHT HOLDER: Tarojiro Matsumura

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Peak Optimization
#'
#' @description Perform peak optimization (component location/wavenumber,
#'   component width, proportional area, and/or shape parameters) for all
#'   provided component peaks against an absorbance intensity. Uses expectation
#'   maximization algorithms from Matsumura *et. al.*. The specific function
#'   called results in different peak types and has different input parameters
#'   to optimize:
#' * [spect_em_gmm()] optimizes Gauss shaped component peaks with the parameters:
#'   * `sigma` - standard deviation (sigma) of the component peak
#' * [spect_em_lmm()] optimizes Lorentz shaped component peaks with the parameters:
#'   * `gam` - width (gamma) of the peak(s). Can be thought of as standard deviation.
#' * [spect_em_pvmm()] optimizes pseudo-Voigt shaped component peaks (a blending of Gauss and Lorentz) with the following parameters:
#'   * `sigma` - standard deviation (sigma) of the component peak
#'   * `eta` - mixing of Gauss and Lorentz distribution for the component (proportion of Lorentz from 0-1)
#' * [spect_em_dsgmm()] optimizes Doniach-Šunjić-Gauss shaped component peaks (pseudo-Voigt but can be skew/asymmetrical) with the following parameters:
#'   * `sigma` - standard deviation (sigma) of the component peak
#'   * `alpha` - proportion asymmetric (0-1) of the component peak
#'   * `eta` - mixing of Gauss and Lorentz distribution for the component (proportion of Lorentz from 0-1)
#'
#'   Optimisation des pics (emplacement des composants/nombre d'ondes, largeur
#'   des composants, surface proportionnelle et/ou paramètres de forme) pour
#'   tous les pics de composants fournis par rapport à une intensité
#'   d'absorption. Utilise les algorithmes de maximisation de l'espérance de
#'   Matsumura *et. al.*. La fonction spécifique appelée produit différents
#'   types de pics et a différents paramètres d'entrée à optimiser :
#' * [spect_em_gmm()] optimise les pics des composants en forme de Gauss avec les paramètres :
#'   * `sigma` - écart-type (sigma) du pic de la composante
#' * [spect_em_lmm()] optimise les pics des composants en forme de Lorentz avec les paramètres :
#'   * `gam` - largeur (gamma) du (des) pic(s). On peut l'assimiler à un écart-type.
#' * [spect_em_pvmm()] optimise les pics des composantes en forme de pseudo-Voigt (un mélange de Gauss et de Lorentz) avec les paramètres suivants :
#'   * `sigma` - écart-type (sigma) du pic de la composante
#'   * `eta` - mélange des distributions de Gauss et de Lorentz pour le composant (proportion de Lorentz de 0 à 1)
#' * [spect_em_dsgmm()] optimise les pics des composantes en forme de Doniach-Šunjić-Gauss (pseudo-Voigt mais peut être asymétrique/asymétrique) avec les paramètres suivants :
#'   * `sigma` - écart-type (sigma) du pic de la composante.
#'   * `alpha` - proportion asymétrique (0-1) du pic de la composante
#'   * `eta` - mélange des distributions de Gauss et de Lorentz pour le composant (proportion de Lorentz de 0 à 1)
#' @param x A numeric vector of x values (wavenumbers) of the spectra against
#'   which the components are being optimized.
#'
#'   Un tableau numérique des valeurs x (nombres d'ondes) des spectres par
#'   rapport auxquels les composants sont optimisés.
#' @param y A numeric vector of absorbance values (of same length as `x`) of the
#'   spectra against which the components are being optimized.
#'
#'   A numeric vector of absorbance values (of same length as `x`) of the
#'   spectra against which the components are being optimized.
#' @param mu A numeric vector of component peak centers.
#'
#'   Un tableau numérique des centres de pics des composants.
#' @param sigma A numeric vector of component peak standard deviation (sigma)
#'   values. Must be the same length as `mu`.
#'
#'   Un tableau numérique des valeurs d'écart-type (sigma) des pics des
#'   composants. Doit être de la même longueur que `mu`.
#' @param alpha A numeric vector of component proportion asymmetric (alpha)
#'   values. Must all be between 0 and 1. Must be the same length as `mu`.
#'
#'   A numeric vector of component proportion asymmetric (alpha) values. Must
#'   all be between 0 and 1. Must be the same length as `mu`.
#' @param eta A numeric vector of component mixing of Gauss and Lorentz
#'   characteristics. Must all be between 0 and 1. Must be the same length as
#'   `mu`.
#'
#'   Un tableau numérique du mélange des composantes des caractéristiques de
#'   Gauss et de Lorentz. Doit être compris entre 0 et 1. Doit être de la même
#'   longueur que `mu`.
#' @param gam A numeric vector of component peak widths (gamma) values. Must be
#'   the same length as `mu`.
#'
#'   Un tableau numérique des valeurs de largeur des pics des composants
#'   (gamma). Doit être de la même longueur que `mu`.
#' @param mix_ratio A numeric vector of mix ratios (e.g. proportionate area
#'   under the curve) for each component peak. Must be the same length as `mu`.
#'
#'   Un tableau numérique des rapports de mélange (par exemple, l'aire
#'   proportionnelle sous la courbe) pour chaque pic de composant. Doit être de
#'   la même longueur que `mu`.
#' @param conv_cri The value (in absolute units) to which convergence is
#'   measured. If sequential optimization cycles differ by less than this
#'   convergence amount, then the resulting parameters are returned. Smaller
#'   values of `conv_cri` result in tighter convergence at higher computational
#'   cost.
#'
#'   La valeur (en unités absolues) à laquelle la convergence est est mesurée.
#'   Si les cycles d'optimisation séquentiels diffèrent de moins de cette valeur
#'   de convergence, les paramètres résultants sont renvoyés. Des valeurs plus
#'   petites de `conv_cri` résultent en une convergence plus serrée à un coût de
#'   calcul plus élevé.
#' @param maxit The maximum number of optimization cycles permitted. If
#'   `conv_cri` has not been met by the completion of `maxit` cycles an error is
#'   returned.
#'
#'   Le nombre maximum de cycles d'optimisation autorisés. Si `conv_cri` n'a pas
#'   été atteint à la fin des cycles `maxit`, une erreur est renvoyée.
#' @param fixed_mu Whether to allow peak centers to be moved in optimization
#'   (TRUE) or fixed to the provided values (FALSE).
#'
#'   Permet de déplacer les centres de pic lors de l'optimisation (VRAI) ou de
#'   les fixer aux valeurs fournies (FAUX).
#' @param verbose Whether to output a status message at the conclusion of each
#'   optimization cycle.
#'
#'   Indique si un message d'état doit être émis à la fin de chaque cycle
#'   d'optimisation.
#' @return A named list object, with values corresponding to the optimal peak
#'   location, width, area, and shape parameters (function dependent), as well
#'   as the optimization record of each optimized parameter, count of
#'   optimization iterations, convergence status, and the type of optimization.
#'
#'   Un objet liste nommé, avec des valeurs correspondant aux paramètres
#'   optimaux d'emplacement, de largeur, de surface et de forme du pic
#'   (dépendant de la fonction), ainsi que l'enregistrement d'optimisation de
#'   chaque paramètre optimisé, le nombre d'itérations d'optimisation, l'état de
#'   convergence et le type d'optimisation.
#'
#' @seealso [fit_peaks()]
#'
#' @references
#' * Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2019) "Spectrum adapted expectation-maximization algorithm for high-throughput peak shift analysis". Science and technology of advanced materials, 20(1), pp 733-745. doi:10.1080/14686996.2019.1620123
#' * Matsumura, T., Nagamura, N., Akaho, S., Nagata, K., & Ando, Y. (2021) "Spectrum adapted expectation-conditional maximization algorithm for extending high–throughput peak separation method in XPS analysis". Science and Technology of Advanced Materials: Methods, 1(1), pp 45-55. doi:10.1080/27660400.2021.1899449
#'
#' @name optimization
#' @md
NULL

#' @rdname optimization
spect_em_dsgmm <- function(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  alpha = rep(0.5, length(mu)),
  eta = rep(0.5, length(mu)),
  mix_ratio = rep(1 / length(mu), length(mu)),
  conv_cri = 1e-2,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
) {
  # Function Prep
  f_k <- function(i) {
    mix_ratio[i] * truncated_dsg(x, mu[i], sigma[i], alpha[i], eta[i])
  }

  LL <- function(x, y, mu, sigma, alpha, eta, mix_ratio) {
    pL <- sapply(1:K, f_k)
    sum(y * log(apply(pL, 1, sum)))
  }

  Q_fun <- function(x, w_k, mu, sigma, alpha, eta, mix_ratio) {
    w_k %*% (log(mix_ratio) + log(truncated_dsg(x, mu, sigma, alpha, eta)))
  }

  # Error checking
  if (length(x) != length(y)) {
    cli::cli_abort(
      "Error in {.fn spect_em_dsgmm}. Provided {.param x} and {.param y} vectors must be of the same length."
    )
  }
  if (
    any(
      length(mix_ratio) != length(mu),
      length(eta) != length(mu),
      length(alpha) != length(mu),
      length(sigma) != length(mu)
    )
  ) {
    cli::cli_abort(
      "Error in {.fn spect_em_dsgmm}. All of {.param mu}, {.param sigma}, {.param alpha}, {.param eta} and {.param mix_ratio} must be of the same length."
    )
  }
  if (!maxit > 1) {
    cli::cli_abort(
      "Error in {.fn spect_em_dsgmm}. Provided {.param maxit} must be greater than 1 to perform optimization."
    )
  }

  # Initial Values
  start_cal <- Sys.time()
  status <- "Not converged"
  N <- length(x)
  LL_1 <- numeric(0)
  mix_ratio_1 <- numeric(0)
  sigma_1 <- numeric(0)
  mu_1 <- numeric(0)
  alpha_1 <- numeric(0)
  eta_1 <- numeric(0)
  n_k <- numeric(0)
  K <- length(mu)
  LL_1[1] <- LL(x, y, mu, sigma, alpha, eta, mix_ratio)
  mu_1 <- rbind(mu_1, mu)
  sigma_1 <- rbind(sigma_1, sigma)
  alpha_1 <- rbind(alpha_1, alpha)
  eta_1 <- rbind(eta_1, eta)
  mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

  # Iterative optimization
  for (i in 1:maxit) {
    tmp <- sapply(1:K, f_k)
    den <- apply(tmp, 1, sum)
    w_k <- matrix(NA, nrow = K, ncol = N)
    for (j in 1:K) {
      w_k[j, ] <- y *
        mix_ratio[j] *
        truncated_dsg(x, mu[j], sigma[j], alpha[j], eta[j]) /
        den
    }
    n_k <- apply(w_k, 1, sum)
    n_k[which(is.na(n_k))] <- 0
    mu_cal <- c()
    sigma_cal <- c()
    alpha_cal <- c()
    eta_cal <- c()
    mix_ratio <- n_k / sum(y)

    # Update Mu
    if (!fixed_mu) {
      for (k in 1:K) {
        opt <- stats::optimize(
          Q_fun,
          interval = c(min(x), max(x)),
          tol = 1e-06,
          x = x,
          sigma = sigma[k],
          alpha = alpha[k],
          eta = eta[k],
          w_k = w_k[k, ],
          mix_ratio = mix_ratio[k],
          maximum = TRUE
        )
        mu_cal <- c(mu_cal, opt$maximum)
      }
      mu <- mu_cal
    }

    # Update Sigma
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(0.1, 1000),
        tol = 1e-06,
        x = x,
        mu = mu[k],
        alpha = alpha[k],
        eta = eta[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      sigma_cal <- c(sigma_cal, opt$maximum)
    }
    sigma <- sigma_cal

    # Update Alpha
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(1e-06, 0.999999),
        tol = 1e-06,
        x = x,
        mu = mu[k],
        sigma = sigma[k],
        eta = eta[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      alpha_cal <- c(alpha_cal, opt$maximum)
    }
    alpha <- alpha_cal

    # Update Eta
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(1e-06, 0.999999),
        tol = 1e-06,
        x = x,
        mu = mu[k],
        sigma = sigma[k],
        alpha = alpha[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      eta_cal <- c(eta_cal, opt$maximum)
    }
    eta <- eta_cal

    # Record Updates
    LL_1[i + 1] <- LL(x, y, mu, sigma, alpha, eta, mix_ratio)
    mu_1 <- rbind(mu_1, mu)
    sigma_1 <- rbind(sigma_1, sigma)
    alpha_1 <- rbind(alpha_1, alpha)
    eta_1 <- rbind(eta_1, eta)
    mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

    #Check for convergance
    if (abs(LL_1[i + 1] - LL_1[i]) < conv_cri) {
      status <- "converged"
      cal_time <- difftime(Sys.time(), start_cal, units = "sec")
      if (verbose) {
        cli::cli_alert_success(
          "Converged in {i} iterations ({round(cal_time)} seconds)."
        )
      }
      break
    } else if (verbose) {
      cli::cli_alert_info("LL: {LL_1[i+1]}")
    }
  }

  if (is.na(cal_time)) {
    cal_time <- difftime(Sys.time(), start_cal, units = "sec")
  }

  # mu: component peak centres
  # sigma: component peak widths (specifically, standard deviation of the component centred at mu)
  # alpha: estimated asymmetry of the component
  # eta: mixing of Gauss and Lorentz distribution for the component (proportion Lorentz 0-1)
  # mix_ratio: component peak heights (should sum to 1)
  # it: number of iterations to convergence or maxit if not converged
  # LL: log likelihood values at each iteration
  # MU: mu values at each iteration
  # SIGMA: sigma values at each iteration
  # ALPHA: alpha values at each iteration
  # ETA: eta values at each iteration
  # MIX_RATIO: mix_ratio values at each iteration
  # W_K: decomposed curve of each component [i,] at each x value [,j]
  # convergence: message of convergence in calculation
  # cal_time: time to converge
  list(
    mu = mu,
    sigma = sigma,
    alpha = alpha,
    eta = eta,
    mix_ratio = mix_ratio,
    it = i,
    LL = LL_1,
    MU = mu_1,
    SIGMA = sigma_1,
    ALPHA = alpha_1,
    ETA = eta_1,
    MIX_RATIO = mix_ratio_1,
    convergence = status,
    W_K = w_k,
    cal_time = cal_time
  )
}


#' @rdname optimization
spect_em_gmm <- function(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  mix_ratio = rep(1 / length(mu), length(mu)),
  conv_cri = 1e-2,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
) {
  # Function Setup
  f_k <- function(i) {
    mix_ratio[i] * stats::dnorm(x, mu[i], sigma[i])
  }
  LL <- function(x, y, mu, sigma, mix_ratio) {
    pL <- sapply(1:K, f_k)
    sum(y * log(apply(pL, 1, sum)))
  }

  # Error checking
  if (length(x) != length(y)) {
    cli::cli_abort(
      "Error in {.fn spect_em_gmm}. Provided {.param x} and {.param y} vectors must be of the same length."
    )
  }
  if (any(length(mix_ratio) != length(mu), length(sigma) != length(mu))) {
    cli::cli_abort(
      "Error in {.fn spect_em_gmm}. All of {.param mu}, {.param sigma}, and {.param mix_ratio} must be of the same length."
    )
  }
  if (!maxit > 1) {
    cli::cli_abort(
      "Error in {.fn spect_em_gmm}. Provided {.param maxit} must be greater than 1 to perform optimization."
    )
  }

  # Initial Values
  start_cal <- Sys.time()
  status <- "Not converged"
  N <- length(x)
  LL_1 <- numeric(0)
  mix_ratio_1 <- numeric(0)
  sigma_1 <- numeric(0)
  mu_1 <- numeric(0)
  n_k <- numeric(0)
  K <- length(mu)
  LL_1[1] <- LL(x, y, mu, sigma, mix_ratio)
  mu_1 <- rbind(mu_1, mu)
  sigma_1 <- rbind(sigma_1, sigma)
  mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

  # Iterative optimization
  for (i in 1:maxit) {
    tmp <- sapply(1:K, f_k)
    den <- apply(tmp, 1, sum)
    w_k <- matrix(NA, nrow = K, ncol = N)
    for (j in 1:K) {
      w_k[j, ] <- y * mix_ratio[j] * stats::dnorm(x, mu[j], sigma[j]) / den
    }
    n_k <- apply(w_k, 1, sum)

    # Update mu and sigma
    for (j in 1:K) {
      if (!fixed_mu) {
        mu[j] <- sum((w_k[j, ] * x)) / n_k[j]
      }
      sigma[j] <- sqrt(sum(w_k[j, ] * (x - mu[j])^2) / n_k[j])
    }
    mix_ratio <- n_k / sum(y)

    # record values
    LL_1[i + 1] <- LL(x, y, mu, sigma, mix_ratio)
    mu_1 <- rbind(mu_1, mu)
    sigma_1 <- rbind(sigma_1, sigma)
    mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

    #Check for convergance
    if (abs(LL_1[i + 1] - LL_1[i]) < conv_cri) {
      status <- "converged"
      cal_time <- difftime(Sys.time(), start_cal, units = "sec")
      if (verbose) {
        cli::cli_alert_success(
          "Converged in {i} iterations ({round(cal_time)} seconds)."
        )
      }
      break
    } else if (verbose) {
      cli::cli_alert_info("LL: {LL_1[i+1]}")
    }
  }

  if (is.na(cal_time)) {
    cal_time <- difftime(Sys.time(), start_cal, units = "sec")
  }

  # mu: component peak centres
  # sigma: component peak widths (specifically, standard deviation of the normal distribution centred at mu)
  # mix_ratio: component peak heights (should sum to 1)
  # it: number of iterations to convergence or maxit if not converged
  # LL: log likelihood values at each iteration
  # MU: mu values at each iteration
  # SIGMA: sigma values at each iteration
  # MIX_RATIO: mix_ratio values at each iteration
  # W_K: decomposed curve of each component [i,] at each x value [,j]
  # convergence: message of convergence in calculation
  # cal_time: time to converge
  list(
    mu = mu,
    sigma = sigma,
    mix_ratio = mix_ratio,
    it = i,
    LL = LL_1,
    MU = mu_1,
    SIGMA = sigma_1,
    MIX_RATIO = mix_ratio_1,
    convergence = status,
    W_K = w_k,
    cal_time = cal_time
  )
}

#' @rdname optimization
spect_em_lmm <- function(
  x,
  y,
  mu,
  gam = rep(10, length(mu)),
  mix_ratio = rep(1 / length(mu), length(mu)),
  conv_cri = 1e-2,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
) {
  # Function Setup
  f_k <- function(i) {
    mix_ratio[i] * dCauchy(x, mu[i], gam[i])
  }
  LL <- function(x, y, mu, gam, mix_ratio) {
    pL <- sapply(1:K, f_k)
    sum(y * log(apply(pL, 1, sum)))
  }
  Q_fun <- function(x, w_k, mu, gam, mix_ratio) {
    w_k %*% (log(mix_ratio) + log(dCauchy(x, mu, gam)))
  }

  # Error checking
  if (length(x) != length(y)) {
    cli::cli_abort(
      "Error in {.fn spect_em_lmm}. Provided {.param x} and {.param y} vectors must be of the same length."
    )
  }
  if (any(length(mix_ratio) != length(mu), length(gam) != length(mu))) {
    cli::cli_abort(
      "Error in {.fn spect_em_lmm}. All of {.param mu}, {.param gam}, and {.param mix_ratio} must be of the same length."
    )
  }
  if (!maxit > 1) {
    cli::cli_abort(
      "Error in {.fn spect_em_lmm}. Provided {.param maxit} must be greater than 1 to perform optimization."
    )
  }

  # Initial Values
  start_cal <- Sys.time()
  status <- "Not converged"
  N <- length(x)
  LL_1 <- numeric(0)
  mix_ratio_1 <- numeric(0)
  gam_1 <- numeric(0)
  mu_1 <- numeric(0)
  n_k <- numeric(0)
  K <- length(mu)

  LL_1[1] <- LL(x, y, mu, gam, mix_ratio)
  mu_1 <- rbind(mu_1, mu)
  gam_1 <- rbind(gam_1, gam)
  mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

  # Iterative optimization
  for (i in 1:maxit) {
    tmp <- sapply(1:K, f_k)
    den <- apply(tmp, 1, sum)
    w_k <- matrix(NA, nrow = K, ncol = N)
    for (j in 1:K) {
      w_k[j, ] <- y *
        mix_ratio[j] *
        dCauchy(x, mu[j], gam[j]) /
        den
    }
    n_k <- apply(w_k, 1, sum)
    n_k[which(is.na(n_k))] <- 0
    mu_cal <- c()
    gam_cal <- c()
    mix_ratio <- n_k / sum(y)

    # Update Mu
    if (!fixed_mu) {
      for (k in 1:K) {
        opt <- stats::optimize(
          Q_fun,
          interval = c(min(x), max(x)),
          tol = 1e-10,
          x = x,
          gam = gam[k],
          w_k = w_k[k, ],
          mix_ratio = mix_ratio[k],
          maximum = TRUE
        )
        mu_cal <- c(mu_cal, opt$maximum)
      }
      mu <- mu_cal
    }

    # Update GAM
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(0.001, 1000),
        tol = 1e-10,
        x = x,
        mu = mu[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      gam_cal <- c(gam_cal, opt$maximum)
    }
    gam <- gam_cal

    # Record Values
    LL_1[i + 1] <- LL(x, y, mu, gam, mix_ratio)
    mu_1 <- rbind(mu_1, mu)
    gam_1 <- rbind(gam_1, gam)
    mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

    #Check for convergance
    if (abs(LL_1[i + 1] - LL_1[i]) < conv_cri) {
      status <- "converged"
      cal_time <- difftime(Sys.time(), start_cal, units = "sec")
      if (verbose) {
        cli::cli_alert_success(
          "Converged in {i} iterations ({round(cal_time)} seconds)."
        )
      }
      break
    } else if (verbose) {
      cli::cli_alert_info("LL: {LL_1[i+1]}")
    }
  }

  if (is.na(cal_time)) {
    cal_time <- difftime(Sys.time(), start_cal, units = "sec")
  }

  # mu: component peak centres
  # gam: component peak widths (specifically, scale parameter of the lorenzian centred at mu)
  # mix_ratio: component peak heights (should sum to 1)
  # it: number of iterations to convergence or maxit if not converged
  # LL: log likelihood values at each iteration
  # MU: mu values at each iteration
  # GAM: gam values at each iteration
  # MIX_RATIO: mix_ratio values at each iteration
  # W_K: decomposed curve of each component [i,] at each x value [,j]
  # convergence: message of convergence in calculation
  # cal_time: time to converge
  list(
    mu = mu,
    gam = gam,
    mix_ratio = mix_ratio,
    it = i,
    LL = LL_1,
    MU = mu_1,
    GAM = gam_1,
    MIX_RATIO = mix_ratio_1,
    convergence = status,
    W_K = w_k,
    cal_time = cal_time
  )
}

#' @rdname optimization
spect_em_pvmm <- function(
  x,
  y,
  mu,
  sigma = rep(10, length(mu)),
  eta = rep(0.5, length(mu)),
  mix_ratio = rep(1 / length(mu), length(mu)),
  conv_cri = 1e-2,
  maxit = 1000,
  fixed_mu = FALSE,
  verbose = FALSE
) {
  # Function Setup
  f_k <- function(i) {
    mix_ratio[i] * truncated_pv(x, mu[i], sigma[i], eta[i])
  }
  LL <- function(x, y, mu, sigma, eta, mix_ratio) {
    pL <- sapply(1:K, f_k)
    sum(y * log(apply(pL, 1, sum)))
  }
  Q_fun <- function(x, w_k, mu, sigma, eta, mix_ratio) {
    w_k %*% (log(mix_ratio) + log(truncated_pv(x, mu, sigma, eta)))
  }

  # Error checking
  if (length(x) != length(y)) {
    cli::cli_abort(
      "Error in {.fn spect_em_pvmm}. Provided {.param x} and {.param y} vectors must be of the same length."
    )
  }
  if (
    any(
      length(mix_ratio) != length(mu),
      length(eta) != length(mu),
      length(sigma) != length(mu)
    )
  ) {
    cli::cli_abort(
      "Error in {.fn spect_em_pvmm}. All of {.param mu}, {.param sigma}, {.param eta}, and {.param mix_ratio} must be of the same length."
    )
  }
  if (!maxit > 1) {
    cli::cli_abort(
      "Error in {.fn spect_em_pvmm}. Provided {.param maxit} must be greater than 1 to perform optimization."
    )
  }

  # Initial Values
  start_cal <- Sys.time()
  status <- "Not converged"
  N <- length(x)
  LL_1 <- numeric(0)
  mix_ratio_1 <- numeric(0)
  sigma_1 <- numeric(0)
  mu_1 <- numeric(0)
  eta_1 <- numeric(0)
  n_k <- numeric(0)
  K <- length(mu)
  LL_1[1] <- LL(x, y, mu, sigma, eta, mix_ratio)
  mu_1 <- rbind(mu_1, mu)
  sigma_1 <- rbind(sigma_1, sigma)
  eta_1 <- rbind(eta_1, eta)
  mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

  #Iterative optimization
  for (i in 1:maxit) {
    tmp <- sapply(1:K, f_k)
    den <- apply(tmp, 1, sum)
    w_k <- matrix(NA, nrow = K, ncol = N)
    for (j in 1:K) {
      w_k[j, ] <- y *
        mix_ratio[j] *
        truncated_pv(x, mu[j], sigma[j], eta[j]) /
        den
    }
    n_k <- apply(w_k, 1, sum)
    n_k[which(is.na(n_k))] <- 0
    mu_cal <- c()
    sigma_cal <- c()
    eta_cal <- c()
    mix_ratio <- n_k / sum(y)

    # Update mu
    if (!fixed_mu) {
      for (k in 1:K) {
        opt <- stats::optimize(
          Q_fun,
          interval = c(min(x), max(x)),
          tol = 1e-10,
          x = x,
          sigma = sigma[k],
          eta = eta[k],
          w_k = w_k[k, ],
          mix_ratio = mix_ratio[k],
          maximum = TRUE
        )
        mu_cal <- c(mu_cal, opt$maximum)
      }
      mu <- mu_cal
    }

    # Update Sigma
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(0.001, 1000),
        tol = 1e-10,
        x = x,
        mu = mu[k],
        eta = eta[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      sigma_cal <- c(sigma_cal, opt$maximum)
    }
    sigma <- sigma_cal

    # Update Eta
    for (k in 1:K) {
      opt <- stats::optimize(
        Q_fun,
        interval = c(0, 1),
        tol = 1e-10,
        x = x,
        mu = mu[k],
        sigma = sigma[k],
        w_k = w_k[k, ],
        mix_ratio = mix_ratio[k],
        maximum = TRUE
      )
      eta_cal <- c(eta_cal, opt$maximum)
    }
    eta <- eta_cal

    # Record values
    LL_1[i + 1] <- LL(x, y, mu, sigma, eta, mix_ratio)
    mu_1 <- rbind(mu_1, mu)
    sigma_1 <- rbind(sigma_1, sigma)
    eta_1 <- rbind(eta_1, eta)
    mix_ratio_1 <- rbind(mix_ratio_1, mix_ratio)

    #Check for convergance
    if (abs(LL_1[i + 1] - LL_1[i]) < conv_cri) {
      status <- "converged"
      cal_time <- difftime(Sys.time(), start_cal, units = "sec")
      if (verbose) {
        cli::cli_alert_success(
          "Converged in {i} iterations ({round(cal_time)} seconds)."
        )
      }
      break
    } else if (verbose) {
      cli::cli_alert_info("LL: {LL_1[i+1]}")
    }
  }

  if (is.na(cal_time)) {
    cal_time <- difftime(Sys.time(), start_cal, units = "sec")
  }
  # mu: component peak centres
  # sigma: component peak widths (specifically, standard deviation of the component centred at mu)
  # eta: mixing of Gauss and Lorentz distribution for the component (proportion Lorentz 0-1)
  # mix_ratio: component peak heights (should sum to 1)
  # it: number of iterations to convergence or maxit if not converged
  # LL: log likelihood values at each iteration
  # MU: mu values at each iteration
  # SIGMA: gam values at each iteration
  # ETA: eta values at each iteration
  # MIX_RATIO: mix_ratio values at each iteration
  # W_K: decomposed curve of each component [i,] at each x value [,j]
  # convergence: message of convergence in calculation
  # cal_time: time to converge
  list(
    mu = mu,
    sigma = sigma,
    eta = eta,
    mix_ratio = mix_ratio,
    it = i,
    LL = LL_1,
    MU = mu_1,
    SIGMA = sigma_1,
    ETA = eta_1,
    MIX_RATIO = mix_ratio_1,
    convergence = status,
    W_K = w_k,
    cal_time = cal_time
  )
}


# These are from the EMpeaksR and are unexported helper functions

truncated_pv <- function(x, mu, sigma, eta) {
  (eta *
    stats::dcauchy(x, mu, sqrt(2 * log(2)) * sigma) +
    (1 - eta) * stats::dnorm(x, mu, sigma)) /
    sum(
      eta *
        stats::dcauchy(x, mu, sqrt(2 * log(2)) * sigma) +
        (1 - eta) * stats::dnorm(x, mu, sigma)
    )
}

truncated_dsg <- function(x, mu, sigma, alpha, eta) {
  ((eta *
    (((gamma(1 - alpha)) /
      ((x - mu)^2 + (sqrt(2 * log(2)) * sigma)^2)^((1 - alpha) / 2)) *
      cos(
        (pi * alpha / 2) +
          (1 - alpha) *
            atan(
              (x - mu) /
                (sqrt(2 * log(2)) * sigma)
            )
      ))) +
    (1 - eta) * stats::dnorm(x, mu, sigma)) /
    sum(
      ((eta *
        (((gamma(1 - alpha)) /
          ((x - mu)^2 + (sqrt(2 * log(2)) * sigma)^2)^((1 - alpha) / 2)) *
          cos(
            (pi * alpha / 2) +
              (1 - alpha) *
                atan(
                  (x - mu) /
                    (sqrt(2 * log(2)) * sigma)
                )
          ))) +
        (1 - eta) * stats::dnorm(x, mu, sigma))
    )
}

dCauchy <- function(x, mu, gam) {
  return((stats::dcauchy(x, mu, gam)) / sum(stats::dcauchy(x, mu, gam)))
}

truncated_l <- function(x, mu, gam) {
  return(dCauchy(x = x, mu = mu[1], gam = gam[1]))
}

truncated_g <- function(x, mu, sigma) {
  return(stats::dnorm(x = x, mean = mu[1], sd = sigma[1]))
}
