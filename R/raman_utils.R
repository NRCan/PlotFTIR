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
#'   Si `TRUE`, calcule également la largeur à mi-hauteur (FWHM) pour chaque pic détecté.
#'   La valeur par défaut est `FALSE`.
#'
#' @return A data.frame with columns:
#' * `sample_id`: the sample from which the peak was detected
#' * `wavenumber`: wavenumber position of the peak in cm^-1^
#' * `intensity`: intensity value at the peak maximum
#' * If `compute_fwhm = TRUE`, an additional column `fwhm` with width in cm^-1^
#'
#'   Un data.frame avec les colonnes :
#' * `sample_id` : l'échantillon à partir duquel le pic a été détecté
#' * `wavenumber` : position en nombre d'ondes du pic en cm^-1^
#' * `intensity` : valeur d'intensité au maximum du pic
#' * Si `compute_fwhm = TRUE`, une colonne supplémentaire `fwhm` avec la largeur en cm^-1^
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
      fwhm_values <- numeric(nrow(peaks_df))
      for (i in seq_len(nrow(peaks_df))) {
        peak_idx <- which(wavenumber == peaks_df$wavenumber[i])
        half_max <- peaks_df$intensity[i] / 2

        wn_left <- wavenumber[1]
        wn_right <- wavenumber[n]
        int_left <- intensity[1]
        int_right <- intensity[n]

        for (j in seq_along(intensity)) {
          if (j == 1) {
            next
          }
          if (intensity[j - 1] <= half_max && intensity[j] > half_max) {
            if (intensity[j] - intensity[j - 1] > 0) {
              wn_left <- wavenumber[j - 1] +
                (wavenumber[j] - wavenumber[j - 1]) *
                  (half_max - intensity[j - 1]) /
                  (intensity[j] - intensity[j - 1])
            } else {
              wn_left <- wavenumber[j - 1]
            }
            break
          }
        }

        for (j in seq_along(intensity)) {
          if (j == 1) {
            next
          }
          if (intensity[j - 1] > half_max && intensity[j] <= half_max) {
            if (intensity[j - 1] - intensity[j] > 0) {
              wn_right <- wavenumber[j - 1] +
                (wavenumber[j] - wavenumber[j - 1]) *
                  (half_max - intensity[j - 1]) /
                  (intensity[j - 1] - intensity[j])
            } else {
              wn_right <- wavenumber[j]
            }
            break
          }
        }

        fwhm_values[i] <- abs(wn_right - wn_left)
      }

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
