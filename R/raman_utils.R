#' Compute Prominence-Based FWHM for a Single Peak
#'
#' @description Internal helper computing the full width at half maximum of a
#'   single peak using the definition implemented by SciPy's
#'   `scipy.signal.peak_widths` with `rel_height = 0.5`. On each flank, the
#'   search area extends from the peak to the first strictly higher sample (or
#'   the spectral edge if none exists), and the flank base is the minimum value
#'   within that area, ties resolved toward the peak. The width is measured
#'   where both flanks cross the level `h_peak - 0.5 * (h_peak - max(bases))`,
#'   i.e. half the prominence above the dominant base, with linear
#'   interpolation between samples.
#'
#'   Fonction interne calculant la largeur à mi-hauteur d'un pic unique selon
#'   la définition implémentée par `scipy.signal.peak_widths` de SciPy avec
#'   `rel_height = 0.5`. Sur chaque flanc, le domaine de recherche s'étend du
#'   pic jusqu'au premier échantillon strictement plus élevé (ou à la borne
#'   spectrale si aucun n'existe), et la base du flanc est la valeur minimale
#'   dans ce domaine, les égalités étant résolues vers le pic. La largeur est
#'   mesurée là où les deux flancs croisent le niveau
#'   `h_peak - 0.5 * (h_peak - max(bases))`, c'est-à-dire à mi-prominence au-dessus
#'   de la base dominante, avec interpolation linéaire entre les échantillons.
#'
#' @param wavenumber Numeric vector of wavenumbers in cm^-1^, sorted ascending.
#'
#'   Vecteur numérique de nombres d'ondes en cm^-1^, trié en ordre croissant.
#'
#' @param intensity Numeric vector of intensities matching `wavenumber`.
#'
#'   Vecteur numérique d'intensités correspondant à `wavenumber`.
#'
#' @param peak_idx Integer position of the peak maximum in both vectors.
#'
#'   Position entière du maximum du pic dans les deux vecteurs.
#'
#' @return A single numeric value: the FWHM in cm^-1^, or `NA_real_` when a
#'   flank crossing cannot be determined (e.g., degenerate flat signals).
#'
#'   Une valeur numérique unique : la FWHM en cm^-1^, ou `NA_real_` lorsqu'une
#'   intersection de flanc ne peut pas être déterminée (p. ex., signaux plats
#'   dégénérés).
#'
#' @noRd
.peak_prominence_fwhm <- function(wavenumber, intensity, peak_idx) {
  n <- length(intensity)

  # find_peak_maxima only reports strict interior maxima; the guard keeps this
  # helper safe when called on boundary or invalid indices.
  if (peak_idx < 2 || peak_idx > n - 1L) {
    return(NA_real_)
  }

  h_peak <- intensity[peak_idx]

  # Search-area borders: first strictly higher sample walking outward. Equal
  # heights are ignored, per the SciPy prominence definition; index 1 and n
  # need no explicit check because the default is already the array edge.
  # `seq.int(a, b)` / `seq.int(a, b, by = -1)` do not yield an empty sequence
  # when the bounds are already inverted (unlike `seq_len()`): they either walk
  # in the wrong direction or error with "wrong sign in 'by' argument". Every
  # loop below is guarded so it is skipped entirely when its range is empty,
  # which routinely happens for peaks close to the signal edge or to a
  # neighbouring peak.
  left_border <- 1L
  if (peak_idx - 1L >= 2L) {
    for (j in seq.int(peak_idx - 1L, 2L, by = -1L)) {
      if (intensity[j] > h_peak) {
        left_border <- j
        break
      }
    }
  }

  right_border <- n
  if (peak_idx + 1L <= n - 1L) {
    for (j in seq.int(peak_idx + 1L, n - 1L)) {
      if (intensity[j] > h_peak) {
        right_border <- j
        break
      }
    }
  }

  # Flank bases: minimum within each bordered interval. Scanning outward with
  # strict less-than keeps the occurrence closest to the peak on ties, as SciPy
  # does.
  left_base <- peak_idx - 1L
  base_left_val <- intensity[left_base]
  if (peak_idx - 2L >= left_border) {
    for (j in seq.int(peak_idx - 2L, left_border, by = -1L)) {
      if (intensity[j] < base_left_val) {
        base_left_val <- intensity[j]
        left_base <- j
      }
    }
  }

  right_base <- peak_idx + 1L
  base_right_val <- intensity[right_base]
  if (peak_idx + 2L <= right_border) {
    for (j in seq.int(peak_idx + 2L, right_border)) {
      if (intensity[j] < base_right_val) {
        base_right_val <- intensity[j]
        right_base <- j
      }
    }
  }

  prominence <- h_peak - max(base_left_val, base_right_val)
  eval_level <- h_peak - 0.5 * prominence

  # Left crossing: first outward pair straddling the evaluation level. The
  # pair condition guarantees a strictly positive denominator.
  wn_left <- NA_real_
  for (i in seq.int(peak_idx - 1L, left_base, by = -1L)) {
    if (intensity[i + 1L] > eval_level && intensity[i] <= eval_level) {
      denom <- intensity[i + 1L] - intensity[i]
      wn_left <- wavenumber[i] +
        (wavenumber[i + 1L] - wavenumber[i]) *
          (eval_level - intensity[i]) /
          denom
      break
    }
  }
  if (is.na(wn_left)) {
    # No crossing before the base column: clamp to the base position, as SciPy
    # does when the contour reaches the peak's base.
    wn_left <- wavenumber[left_base]
  }

  wn_right <- NA_real_
  for (i in seq.int(peak_idx, right_base - 1L)) {
    if (intensity[i] > eval_level && intensity[i + 1L] <= eval_level) {
      denom <- intensity[i] - intensity[i + 1L]
      wn_right <- wavenumber[i] +
        (wavenumber[i + 1L] - wavenumber[i]) *
          (intensity[i] - eval_level) /
          denom
      break
    }
  }
  if (is.na(wn_right)) {
    wn_right <- wavenumber[right_base]
  }

  abs(wn_right - wn_left)
}

#' Find Peak Maxima in Raman Spectra
#'
#' @description Identifies local maxima above a specified height threshold. For
#'   Raman spectra, this is commonly used to identify characteristic vibrational
#'   bands (e.g., D, G, 2D bands in carbon materials).
#'
#'   Identify les maximums locaux au-dessus d'un seuil de hauteur spécifié. Pour
#'   les spectres Raman, cela est couramment utilisé pour identifier les bandes
#'   vibrationnelles caractéristiques (p. ex., bandes D, G, 2D dans les matériaux
#'   carbonés).
#'
#' @inheritParams .shared-params
#'
#' @param height A numeric threshold. Peaks below this intensity are not reported.
#'   If `NULL`, defaults to `0.05 * max(intensity across all selected samples)`.
#'
#'   Un seuil numérique. Les pics en dessous de cette intensité ne sont pas signalés.
#'   Si `NULL`, la valeur par défaut est `0.05 * max(intensité sur tous les échantillons sélectionnés)`.
#'
#' @param distance Minimum separation between peaks in cm^-1^. Peaks closer than
#'   this distance are filtered, keeping the higher of any two conflicting peaks.
#'   If `NULL`, defaults to the median inter-point spacing across all samples.
#'
#'   Séparation minimale entre les pics en cm^-1^. Les pics plus proches que cette
#'   distance sont filtrés, conservant le plus élevé de deux pics en conflit.
#'   Si `NULL`, la valeur par défaut est l'espacement médian inter-point sur tous les échantillons.
#'
#' @param compute_fwhm If `TRUE`, also computes the full width at half maximum
#'   (FWHM) for each detected peak. Default is `FALSE`.
#'
#'   The FWHM follows the definition implemented by SciPy's
#'   `scipy.signal.peak_widths` with `rel_height = 0.5`: on each flank, the
#'   search area extends from the peak to the first strictly higher sample (or
#'   the spectral edge if none is found), and the base of that flank is its
#'   minimum value within this area. The width is measured where both flanks
#'   cross the level `h_peak - 0.5 * (h_peak - max(bases))`, i.e. half the
#'   prominence above the dominant base, with linear interpolation between
#'   samples. For overlapping peaks this reports a contour width of the
#'   composite signal rather than an intrinsic component width; consider
#'   model-based deconvolution for closely spaced bands.
#'
#'   Si `TRUE`, calcule également la largeur à mi-hauteur (FWHM) pour chaque pic détecté.
#'   La valeur par défaut est `FALSE`.
#'
#'   La FWHM suit la définition implémentée par `scipy.signal.peak_widths` de
#'   SciPy avec `rel_height = 0.5` : sur chaque flanc, le domaine de recherche
#'   s'étend du pic jusqu'au premier échantillon strictement plus élevé (ou à
#'   la borne spectrale si aucun n'est trouvé), et la base de ce flanc est sa
#'   valeur minimale dans ce domaine. La largeur est mesurée là où les deux
#'   flancs croisent le niveau `h_peak - 0.5 * (h_peak - max(bases))`, c'est-à-dire
#'   à mi-prominence au-dessus de la base dominante, avec interpolation linéaire
#'   entre les échantillons. Pour des pics superposés, la valeur rapportée est
#'   une largeur de contour du signal composite plutôt que la largeur intrinsèque
#'   d'un composant ; envisagez une déconvolution par modèle pour les bandes
#'   proches.
#'
#' @return A data.frame with columns:
#' * `sample_id`: the sample from which the peak was detected
#' * `wavenumber`: wavenumber position of the peak in cm^-1^
#' * `intensity`: intensity value at the peak maximum
#' * If `compute_fwhm = TRUE`, an additional column `fwhm` with width in cm^-1^.
#'   Values are `NA` when a flank crossing cannot be determined (e.g.,
#'   degenerate flat signals).
#'
#'   Un data.frame avec les colonnes :
#' * `sample_id` : l'échantillon à partir duquel le pic a été détecté
#' * `wavenumber` : position en nombre d'ondes du pic en cm^-1^
#' * `intensity` : valeur d'intensité au maximum du pic
#' * Si `compute_fwhm = TRUE`, une colonne supplémentaire `fwhm` avec la largeur en cm^-1^.
#'   Les valeurs sont `NA` lorsqu'une intersection de flanc ne peut pas être
#'   déterminée (p. ex., signaux plats dégénérés).
#'
#' @export
#'
#' @examples
#' # Generate synthetic Raman data with known peaks
#' wn <- seq(100, 2000, by = 5)
#' peak1 <- 100 * exp(-(wn - 500)^2 / 5000)
#' peak2 <- 50 * exp(-(wn - 1000)^2 / 8000)
#' baseline <- 5
#' noise <- rnorm(length(wn), 0, 2)
#' intensity <- peak1 + peak2 + baseline + noise
#' raman_data <- data.frame(
#'   wavenumber = wn,
#'   intensity = intensity,
#'   sample_id = "sample"
#' )
#' attr(raman_data, "intensity") <- "raman"
#'
#' # Find peaks with default parameters
#' peaks <- find_peak_maxima(raman_data)
#'
#' # Find peaks and compute FWHM
#' peaks_with_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
find_peak_maxima <- function(
  raman,
  sample_ids = NA,
  height = NULL,
  distance = NULL,
  compute_fwhm = FALSE
) {
  raman <- check_ftir_data(raman)

  if (!attr(raman, "intensity") %in% c("raman", "normalized raman")) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::find_peak_maxima}. {.arg raman} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::find_peak_maxima}. L'attribut {.arg raman} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  sample_ids <- .resolve_sample_ids(raman, sample_ids)

  if (!is.null(height) && (!is.numeric(height) || length(height) != 1)) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::find_peak_maxima}. {.arg height} must be a single numeric value or NULL.",
        fr = "Erreur dans {.fn PlotFTIR::find_peak_maxima}. {.arg height} doit \u00eatre une valeur num\u00e9rique unique ou NULL."
      ),
      call = rlang::caller_env()
    )
  }

  if (
    !is.null(distance) &&
      (!is.numeric(distance) || length(distance) != 1 || distance <= 0)
  ) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::find_peak_maxima}. {.arg distance} must be a positive numeric value or NULL.",
        fr = "Erreur dans {.fn PlotFTIR::find_peak_maxima}. {.arg distance} doit \u00eatre une valeur num\u00e9rique positive ou NULL."
      ),
      call = rlang::caller_env()
    )
  }

  if (!is.logical(compute_fwhm) || length(compute_fwhm) != 1) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::find_peak_maxima}. {.arg compute_fwhm} must be a logical value.",
        fr = "Erreur dans {.fn PlotFTIR::find_peak_maxima}. {.arg compute_fwhm} doit \u00eatre une valeur bool\u00e9enne."
      ),
      call = rlang::caller_env()
    )
  }

  all_wavenumbers <- unique(raman$wavenumber)
  sorted_wn <- sort(all_wavenumbers)
  median_spacing <- stats::median(diff(sorted_wn))

  if (is.null(height)) {
    max_intensity <- max(
      raman[raman$sample_id %in% sample_ids, "intensity"],
      na.rm = TRUE
    )
    height <- 0.05 * max_intensity
  }

  if (is.null(distance)) {
    distance <- median_spacing
  }

  results_list <- list()

  for (sid in sample_ids) {
    sample_data <- raman[raman$sample_id == sid, ]
    sample_data <- sample_data[order(sample_data$wavenumber), ]

    wavenumber <- sample_data$wavenumber
    intensity <- sample_data$intensity

    n <- length(intensity)
    if (n < 3) {
      next
    }

    is_peak <- logical(n)
    for (i in 2:(n - 1)) {
      if (intensity[i] > intensity[i - 1] && intensity[i] > intensity[i + 1]) {
        is_peak[i] <- TRUE
      }
    }

    peak_indices <- which(is_peak & (intensity >= height))

    if (length(peak_indices) == 0) {
      next
    }

    peaks_df <- data.frame(
      sample_id = sid,
      wavenumber = wavenumber[peak_indices],
      intensity = intensity[peak_indices]
    )

    if (length(peak_indices) > 1 && distance > 0) {
      sorted_peaks <- peaks_df[order(-peaks_df$intensity), ]

      kept_indices <- integer(0)
      for (i in seq_len(nrow(sorted_peaks))) {
        current_wn <- sorted_peaks$wavenumber[i]
        keep <- TRUE
        for (j in kept_indices) {
          if (abs(current_wn - sorted_peaks$wavenumber[j]) < distance) {
            keep <- FALSE
            break
          }
        }
        if (keep) {
          kept_indices <- c(kept_indices, i)
        }
      }

      peaks_df <- sorted_peaks[kept_indices, ]
    }

    if (compute_fwhm) {
      peak_positions <- vapply(
        seq_len(nrow(peaks_df)),
        function(i) which(wavenumber == peaks_df$wavenumber[i])[1L],
        integer(1)
      )

      fwhm_values <- vapply(
        peak_positions,
        .peak_prominence_fwhm,
        numeric(1),
        wavenumber = wavenumber,
        intensity = intensity
      )

      peaks_df$fwhm <- fwhm_values
    }

    results_list[[sid]] <- peaks_df
  }

  if (length(results_list) == 0) {
    if (compute_fwhm) {
      return(data.frame(
        sample_id = character(),
        wavenumber = numeric(),
        intensity = numeric(),
        fwhm = numeric()
      ))
    } else {
      return(data.frame(
        sample_id = character(),
        wavenumber = numeric(),
        intensity = numeric()
      ))
    }
  }

  results <- do.call(rbind, results_list)
  rownames(results) <- NULL

  return(results)
}

#' Smooth Spectra Using Savitzky-Golay Filter
#'
#' @description Applies a moving-window polynomial filter to denoise spectra.
#'   Based on the Savitzky-Golay algorithm, which fits an order m polynomial
#'   within a sliding window of length 2*n+1 to preserve peak shapes better
#'   than simple boxcar averaging.
#'
#'   Applique un filtre polynomial à fenêtre mobile pour atténuer le bruit des
#'   spectres. Basé sur l'algorithme de Savitzky-Golay, qui ajuste un polynôme
#'   d'ordre m dans une fenêtre glissante de longueur 2*n+1 afin de préserver
#'   les formes d'ondes mieux qu'une moyenne simple.
#'
#' @inheritParams .shared-params
#'
#' @param window_length The length of the smoothing window (must be odd).
#'   Default is 7. Must be at least `polyorder + 1`.
#'
#'   La longueur de la fenêtre de lissage (doit être impaire).
#'   Par défaut, 7. Doit être au moins `polyorder + 1`.
#'
#' @param polyorder The order of the polynomial fit within the window.
#'   Default is 2. Must be less than `window_length`.
#'
#'   L'ordre du polynôme à ajuster dans la fenêtre.
#'   Par défaut, 2. Doit être inférieur à `window_length`.
#'
#' @return a data.frame containing the smoothed Raman spectra with the same
#'   structure as the input (`wavenumber`, `intensity`, `sample_id`).
#'
#'   un data.frame contenant les spectres Raman lissés avec la même structure
#'   que l'entrée (`wavenumber`, `intensity`, `sample_id`).
#'
#' @export
#'
#' @examples
#' if (requireNamespace("signal", quietly = TRUE)) {
#'   # Generate synthetic Raman data with noise
#'   wn <- seq(100, 2000, by = 2)
#'   peaks <- 100 * exp(-(wn - 500)^2 / 5000) + 50 * exp(-(wn - 1000)^2 / 8000)
#'   noisy <- peaks + rnorm(length(wn), sd = 10)
#'
#'   raman_noisy <- data.frame(
#'     wavenumber = wn,
#'     intensity = noisy,
#'     sample_id = "noisy_sample"
#'   )
#'   attr(raman_noisy, "intensity") <- "raman"
#'
#'   # Smooth the spectra
#'   raman_smooth <- smooth_spectra(raman_noisy, window_length = 11, polyorder = 2)
#' }
smooth_spectra <- function(
  raman,
  sample_ids = NA,
  window_length = 7,
  polyorder = 2
) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    .pkg_abort(
      list(
        en = c(
          "{.pkg PlotFTIR} requires {.pkg signal} package installation for this function.",
          i = "Install {.pkg signal} with {.run install.packages('signal')}"
        ),
        fr = c(
          "{.pkg PlotFTIR} n\u00e9cessite l'installation du paquet {.pkg signal} pour cette fonction.",
          i = "Installez {.pkg signal} avec {.run install.packages('signal')}"
        )
      ),
      call = rlang::caller_env()
    )
  }

  raman <- check_ftir_data(raman)

  if (!attr(raman, "intensity") %in% c("raman", "normalized raman")) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::smooth_spectra}. {.arg raman} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::smooth_spectra}. L'attribut {.arg raman} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  sample_ids <- .resolve_sample_ids(raman, sample_ids)

  if (
    !is.numeric(window_length) ||
      length(window_length) != 1 ||
      window_length < 1
  ) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::smooth_spectra}. {.arg window_length} must be a positive integer.",
        fr = "Erreur dans {.fn PlotFTIR::smooth_spectra}. {.arg window_length} doit \u00eatre un entier positif."
      ),
      call = rlang::caller_env()
    )
  }

  if (window_length != round(window_length)) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::smooth_spectra}. {.arg window_length} must be an integer.",
        fr = "Erreur dans {.fn PlotFTIR::smooth_spectra}. {.arg window_length} doit \u00eatre un entier."
      ),
      call = rlang::caller_env()
    )
  }

  if (window_length %% 2 == 0) {
    .pkg_warn(
      list(
        en = cli::format_inline(
          "{.fn PlotFTIR::smooth_spectra} auto-corrected {.arg window_length} from {as.integer(window_length)} to {as.integer(window_length + 1)} (must be odd)."
        ),
        fr = cli::format_inline(
          "{.fn PlotFTIR::smooth_spectra} a automatiquement corrig\u00e9 {.arg window_length} de {as.integer(window_length)} \u00e0 {as.integer(window_length + 1)} (doit \u00eatre impair)."
        )
      ),
      call = rlang::caller_env()
    )
    window_length <- window_length + 1
  }

  if (!is.numeric(polyorder) || length(polyorder) != 1 || polyorder < 0) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::smooth_spectra}. {.arg polyorder} must be a non-negative integer.",
        fr = "Erreur dans {.fn PlotFTIR::smooth_spectra}. {.arg polyorder} doit \u00eatre un entier non n\u00e9gatif."
      ),
      call = rlang::caller_env()
    )
  }

  if (polyorder >= window_length) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::smooth_spectra}. {.arg polyorder} must be less than {.arg window_length}.",
        fr = "Erreur dans {.fn PlotFTIR::smooth_spectra}. {.arg polyorder} doit \u00eatre inf\u00e9rieur \u00e0 {.arg window_length}."
      ),
      call = rlang::caller_env()
    )
  }

  for (sid in sample_ids) {
    idx <- raman$sample_id == sid
    intensity_vec <- raman[idx, "intensity"]
    smoothed <- signal::sgolayfilt(
      x = intensity_vec,
      p = polyorder,
      n = window_length,
      m = 0
    )
    raman[idx, "intensity"] <- smoothed
  }

  return(raman)
}

#' Correct Spectrum Baseline
#'
#' @description Fits and subtracts a baseline from spectra using Asymmetric
#'   Least Squares (AsLS), as described by Eilers (2004). Commonly used in
#'   Raman spectroscopy to remove fluorescence background that manifests as
#'   a broad, slowly varying signal.
#'
#'   Ajuste et soustrait une ligne de base des spectres à l'aide du Moindre
#'   Carré Asymétrique (AsLS), comme décrit par Eilers (2004). Couramment
#'   utilisé en spectroscopie Raman pour éliminer le fond de fluorescence qui
#'   se manifeste comme un signal large et lentement variable.
#'
#' @inheritParams .shared-params
#'
#' @param lambda The smoothness parameter, expressed as a base-10 exponent: the
#'   smoothing penalty applied is `10^lambda`. Larger values give smoother
#'   baselines. Default is 6 (a penalty of 1e6). Typical range: 3 to 9
#'   (1e3 to 1e9).
#'
#'   Le paramètre de lissage, exprimé sous forme d'exposant en base 10 : la
#'   pénalité de lissage appliquée est `10^lambda`. Des valeurs plus élevées
#'   donnent des lignes de base plus lisses. Par défaut, 6 (une pénalité de
#'   1e6). Plage typique : 3 à 9 (1e3 à 1e9).
#'
#' @param p The asymmetry parameter. Controls how much positive residuals are
#'   favored over negative ones. Must be in (0, 0.5]. Default is 0.001.
#'
#'   Le paramètre d'asymétrie. Contrôle dans quelle mesure les résidus positifs
#'   sont favorisés par rapport aux négatifs. Doit être dans (0, 0.5]. Par défaut,
#'   0.001.
#'
#' @return a data.frame containing the baseline-corrected Raman spectra.
#'
#'   un data.frame contenant les spectres Raman corrigés de la ligne de base.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("baseline", quietly = TRUE)) {
#'   # Generate synthetic Raman data with baseline
#'   wn <- seq(100, 2000, by = 2)
#'   peaks <- 100 * exp(-(wn - 500)^2 / 5000) + 50 * exp(-(wn - 1000)^2 / 8000)
#'   background <- 0.001 * (wn - 100)^2
#'   measured <- peaks + background + rnorm(length(wn), sd = 5)
#'
#'   raman_data <- data.frame(
#'     wavenumber = wn,
#'     intensity = measured,
#'     sample_id = "sample_with_baseline"
#'   )
#'   attr(raman_data, "intensity") <- "raman"
#'
#'   # Correct the baseline
#'   raman_corrected <- baseline_correct(raman_data, lambda = 6, p = 0.01)
#' }
baseline_correct <- function(
  raman,
  sample_ids = NA,
  lambda = 6,
  p = 0.001
) {
  if (!requireNamespace("baseline", quietly = TRUE)) {
    .pkg_abort(
      list(
        en = c(
          "{.pkg PlotFTIR} requires {.pkg baseline} package installation for this function.",
          i = "Install {.pkg baseline} with {.run install.packages('baseline')}"
        ),
        fr = c(
          "{.pkg PlotFTIR} n\u00e9cessite l'installation du paquet {.pkg baseline} pour cette fonction.",
          i = "Installez {.pkg baseline} avec {.run install.packages('baseline')}"
        )
      ),
      call = rlang::caller_env()
    )
  }

  raman <- check_ftir_data(raman)

  if (!attr(raman, "intensity") %in% c("raman", "normalized raman")) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::baseline_correct}. {.arg raman} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::baseline_correct}. L'attribut {.arg raman} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  sample_ids <- .resolve_sample_ids(raman, sample_ids)

  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::baseline_correct}. {.arg lambda} must be a positive numeric value.",
        fr = "Erreur dans {.fn PlotFTIR::baseline_correct}. {.arg lambda} doit \u00eatre une valeur num\u00e9rique positive."
      ),
      call = rlang::caller_env()
    )
  }

  if (!is.numeric(p) || length(p) != 1 || p <= 0 || p > 0.5) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR::baseline_correct}. {.arg p} must be a numeric value in (0, 0.5].",
        fr = "Erreur dans {.fn PlotFTIR::baseline_correct}. {.arg p} doit \u00eatre une valeur num\u00e9rique dans (0, 0,5]."
      ),
      call = rlang::caller_env()
    )
  }

  for (sid in sample_ids) {
    idx <- raman$sample_id == sid
    intensity_vec <- raman[idx, "intensity"]

    baseline_fit <- baseline::baseline(
      matrix(intensity_vec, nrow = 1),
      method = 'als',
      lambda = lambda,
      p = p
    )

    raman[idx, "intensity"] <- as.vector(baseline::getCorrected(baseline_fit))
  }

  return(raman)
}

#' Normalize Raman Intensity Spectra
#'
#' @description Scales spectra to a common intensity reference. Supports two
#'   methods: (1) vector norm (L2 normalization — each spectrum has unit length),
#'   or (2) maximum peak scaling (each spectrum's highest point is set to 1).
#'   Both are standard practices in Raman spectroscopy for comparing relative
#'   band intensities across samples.
#'
#'   Met à l'échelle les spectres selon une référence d'intensité commune.
#'   Supporte deux méthodes : (1) norme vectorielle (normalisation L2), ou
#'   (2) mise à l'échelle par le pic maximum (le point le plus élevé de chaque
#'   spectre est fixé à 1). Les deux sont des pratiques standard en spectroscopie
#'   Raman pour comparer les intensités relatives des bandes entre échantillons.
#'
#' @inheritParams .shared-params
#'
#' @param method One of two values:
#' * `"vector"` — L2 normalization: each spectrum is divided by its Euclidean norm.
#' * `"max"` — Maximum scaling: each spectrum is divided by its maximum intensity value.
#'
#'   Une des deux valeurs :
#' * `"vector"` — Normalisation L2 : chaque spectre est divisé par sa norme euclidienne.
#' * `"max"` — Mise à l'échelle maximale : chaque spectre est divisé par sa valeur d'intensité maximale.
#'
#' @return A data.frame containing the normalized Raman spectra with
#'   `attr(raman, "intensity")` set to `"normalized raman"`.
#'
#'   Un data.frame contenant les spectres Raman normalisés avec
#'   `attr(raman, "intensity")` défini sur `"normalized raman"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Normalize all samples using vector norm (L2)
#' normalize_raman(raman_data, method = "vector")
#'
#' # Normalize all samples by maximum peak height
#' normalize_raman(raman_data, method = "max")
#' }
normalize_raman <- function(
  raman,
  sample_ids = NA,
  method = "vector"
) {
  raman <- check_ftir_data(raman)

  if (!attr(raman, "intensity") %in% c("raman", "normalized raman")) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::normalize_raman}. {.arg raman} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::normalize_raman}. L'attribut {.arg raman} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  sample_ids <- .resolve_sample_ids(raman, sample_ids)

  permitted_methods <- c("vector", "max")
  if (length(method) != 1 || !(method %in% permitted_methods)) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::normalize_raman}. {.arg method} must be a string.",
          i = cli::format_inline(
            "{.arg method} must be one of {.val {permitted_methods}}."
          )
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::normalize_raman}. {.arg method} doit \u00eatre une cha\u00eene de caract\u00e8res.",
          i = cli::format_inline(
            "{.arg method} doit \u00eatre l'un des {.val {permitted_methods}}."
          )
        )
      ),
      call = rlang::caller_env()
    )
  }

  for (sid in sample_ids) {
    idx <- raman$sample_id == sid
    intensity_vec <- raman[idx, "intensity"]

    if (length(intensity_vec) == 0 || all(is.na(intensity_vec))) {
      .pkg_abort(
        list(
          en = c(
            "Error in {.fn PlotFTIR::normalize_raman}. Cannot normalize a spectrum with no finite intensity values.",
            x = cli::format_inline(
              "{.arg sample_id} {.val {sid}} is empty or entirely {.code NA}."
            )
          ),
          fr = c(
            "Erreur dans {.fn PlotFTIR::normalize_raman}. Impossible de normaliser un spectre sans valeurs d'intensit\u00e9 finies.",
            x = cli::format_inline(
              "{.arg sample_id} {.val {sid}} est vide ou enti\u00e8rement {.code NA}."
            )
          )
        ),
        call = rlang::caller_env()
      )
    }

    if (anyNA(intensity_vec)) {
      .pkg_warn(
        list(
          en = cli::format_inline(
            "{.fn PlotFTIR::normalize_raman} ignored {sum(is.na(intensity_vec))} {.code NA} intensity value{?s} when normalizing {.arg sample_id} {.val {sid}}."
          ),
          fr = cli::format_inline(
            "{.fn PlotFTIR::normalize_raman} a ignor\u00e9 {sum(is.na(intensity_vec))} valeur{?s} d'intensit\u00e9 {.code NA} lors de la normalisation de {.arg sample_id} {.val {sid}}."
          )
        ),
        call = rlang::caller_env()
      )
    }

    if (method == "vector") {
      norm_val <- sqrt(sum(intensity_vec^2, na.rm = TRUE))
      if (norm_val == 0) {
        .pkg_abort(
          list(
            en = "Error in {.fn PlotFTIR::normalize_raman}. Cannot normalize spectrum with zero magnitude.",
            fr = "Erreur dans {.fn PlotFTIR::normalize_raman}. Impossible de normaliser un spectre avec une amplitude nulle."
          ),
          call = rlang::caller_env()
        )
      }
      raman[idx, "intensity"] <- intensity_vec / norm_val
    } else if (method == "max") {
      max_val <- max(intensity_vec, na.rm = TRUE)
      # A non-positive maximum (possible after baseline_correct()) would flip
      # the sign of the whole spectrum, so reject it rather than rescale.
      if (max_val <= 0) {
        .pkg_abort(
          list(
            en = c(
              "Error in {.fn PlotFTIR::normalize_raman}. Cannot normalize spectrum with a non-positive maximum.",
              x = cli::format_inline(
                "{.arg sample_id} {.val {sid}} has a maximum intensity of {.val {max_val}}."
              ),
              i = "Scaling by a non-positive maximum would invert the spectrum."
            ),
            fr = c(
              "Erreur dans {.fn PlotFTIR::normalize_raman}. Impossible de normaliser un spectre avec un maximum non positif.",
              x = cli::format_inline(
                "{.arg sample_id} {.val {sid}} a une intensit\u00e9 maximale de {.val {max_val}}."
              ),
              i = "La mise \u00e0 l'\u00e9chelle par un maximum non positif inverserait le spectre."
            )
          ),
          call = rlang::caller_env()
        )
      }
      raman[idx, "intensity"] <- intensity_vec / max_val
    }
  }

  attr(raman, "intensity") <- "normalized raman"

  return(raman)
}
