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
  ftir,
  sample_ids = NA,
  height = NULL,
  distance = NULL,
  compute_fwhm = FALSE
) {
  ftir <- check_ftir_data(ftir)

  if (!attr(ftir, "intensity") %in% c("raman", "normalized raman")) {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR::find_peak_maxima}. {.arg ftir} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR::find_peak_maxima}. L'attribut {.arg ftir} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  if (length(sample_ids) <= 1) {
    if (is.na(sample_ids) || is.null(sample_ids) || length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    }
  }

  if (any(!(sample_ids %in% unique(ftir$sample_id)))) {
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    .pkg_abort(
      list(
        en = c(
          "All provided {.arg sample_ids} must be in {.arg ftir} data.",
          x = cli::format_inline(
            "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."
          )
        ),
        fr = c(
          "Tous les {.arg sample_ids} fournis doivent \u00eatre dans les donn\u00e9es {.arg ftir}.",
          x = cli::format_inline(
            "Les {.arg sample_id{?s}} suivants ne sont pas pr\u00e9sents: {.val {mismatch}}."
          )
        )
      ),
      call = rlang::caller_env()
    )
  }

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

  all_wavenumbers <- unique(ftir$wavenumber)
  sorted_wn <- sort(all_wavenumbers)
  median_spacing <- stats::median(diff(sorted_wn))

  if (is.null(height)) {
    max_intensity <- max(
      ftir[ftir$sample_id %in% sample_ids, "intensity"],
      na.rm = TRUE
    )
    height <- 0.05 * max_intensity
  }

  if (is.null(distance)) {
    distance <- median_spacing
  }

  results_list <- list()

  for (sid in sample_ids) {
    sample_data <- ftir[ftir$sample_id == sid, ]
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
