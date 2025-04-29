## Holds functions to do maths on spectra

#' Average FTIR Spectra
#'
#' @description Calculates an average of two or more spectra.
#'
#'   Calcule la moyenne de deux spectres ou plus.
#'
#' @param ftir A data.frame of FTIR spectral data including spectra to be
#'   converted.
#'
#'   Un data.frame de données spectrales IRTF comprenant les spectres à
#'   convertir.
#'
#' @param sample_ids A vector of sample IDs to be averaged together. All sample
#'   IDs must be present in the `ftir` data.frame. If averaging all spectra,
#'   provide `NA` or `NULL.`
#'
#'   Un vecteur d'identifiants d'échantillons dont la moyenne doit être
#'   calculée.. Tous les identifiants des échantillons doivent être présents
#'   dans le data.frame `ftir`. Si la moyenne est calculée pour tous les
#'   spectres, indiquez `NA` ou `NULL.`
#'
#' @param average_id The name to be used as `sample_id` for the averaged
#'   spectra.
#'
#'   Le nom à utiliser en tant que `sample_id` pour les spectres moyennés.
#'
#' @return A data.frame containing the averaged FTIR spectra, with `sample_id`
#'   corresponding to the provided `average_id`.
#'
#'   Un data.frame contenant les spectres IRTF moyennés, avec `sample_id`
#'   correspondant à l'identifiant `average_id` fourni.
#'
#' @export
#'
#' @examples
#' # Calculate the average of biodiesel B5 spectra and the unknown spectra
#'
#' average_spectra(biodiesel, c("biodiesel_5_0", "biodiesel_B5", "diesel_unknown"))
#' @md
average_spectra <- function(
  ftir,
  sample_ids = NA,
  average_id = "averaged_spectra"
) {
  ftir <- check_ftir_data(ftir)
  intensity_attribute <- attr(ftir, "intensity")

  if (length(sample_ids) <= 1) {
    if (is.na(sample_ids) || is.null(sample_ids) || length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    } else if (sample_ids %in% unique(ftir$sample_id)) {
      # Just one sampleID provided, return with new name
      avg_spectra <- ftir[ftir$sample_id == sample_ids, ]
      avg_spectra$sample_id <- average_id
      return(avg_spectra)
    }
  }

  if (any(!(sample_ids %in% unique(ftir$sample_id)))) {
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    cli::cli_abort(c(
      "All provided {.arg sample_ids} must be in {.arg ftir} data.",
      x = "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."
    ))
  }

  if (!is.character(average_id)) {
    cli::cli_abort("{.arg average_id} must be a character value.")
  }

  # drop everything not needed
  ftir <- ftir[ftir$sample_id %in% sample_ids, ]

  # check wavenumbers matches
  first_wavenumbers <- ftir[ftir$sample_id == sample_ids[1], "wavenumber"]
  other_wavenumbers <- ftir[ftir$sample_id != sample_ids[1], "wavenumber"]
  if (
    all(first_wavenumbers %in% other_wavenumbers) &&
      all(other_wavenumbers %in% first_wavenumbers)
  ) {
    # make average - when all wavenumbers are present in all samples
    if (grepl("absorbance", intensity_attribute)) {
      avg_spectra <- stats::aggregate(
        absorbance ~ wavenumber,
        data = ftir,
        FUN = mean
      )
    } else {
      avg_spectra <- stats::aggregate(
        transmittance ~ wavenumber,
        data = ftir,
        FUN = mean
      )
    }

    avg_spectra$sample_id <- average_id
  } else {
    # Mismatch in wavenumbers. Try first to subset data, then if not Can we interpolate & average across the whole range?
    cli::cli_warn(c(
      "There is a mismatch in the wavenumber axis between sample_ids.",
      i = "Only wavenumber ranges within all samples will be averaged, using linear interpolation."
    ))

    # Determine the range over which data should be averaged
    max_min <- max(sapply(
      sample_ids,
      function(x) min(ftir[ftir$sample_id == x, "wavenumber"]),
      simplify = TRUE,
      USE.NAMES = FALSE
    ))
    min_max <- min(sapply(
      sample_ids,
      function(x) max(ftir[ftir$sample_id == x, "wavenumber"]),
      simplify = TRUE,
      USE.NAMES = FALSE
    ))

    first_wavenumbers <- ftir[
      ftir$sample_id == sample_ids[1] &
        ftir$wavenumber < min_max &
        ftir$wavenumber > max_min,
      "wavenumber"
    ]
    other_wavenumbers <- ftir[
      ftir$sample_id != sample_ids[1] &
        ftir$wavenumber < min_max &
        ftir$wavenumber > max_min,
      "wavenumber"
    ]
    if (
      length(first_wavenumbers) > 1 &&
        length(other_wavenumbers) > 1 &&
        all(first_wavenumbers %in% other_wavenumbers) &&
        all(other_wavenumbers %in% first_wavenumbers)
    ) {
      # make average - now wavenumbers are present in all samples for the reduced range
      ftir <- ftir[ftir$wavenumber < min_max & ftir$wavenumber > max_min, ]
      # RECURSION FTW
      return(average_spectra(
        ftir = ftir,
        sample_ids = sample_ids,
        average_id = average_id
      ))
    } else {
      # Determine the biggest step size in the data. Most FTIR is resolved to 4 wavenumbers but not all
      wavenumber_step <- max(sapply(
        sample_ids,
        function(x) mean(diff(ftir[ftir$sample_id == x, "wavenumber"])),
        simplify = TRUE,
        USE.NAMES = FALSE
      ))
      interpolated_wavenumbers <- seq(
        from = max_min,
        to = min_max,
        by = wavenumber_step
      )
      interp_ftir <- data.frame(
        wavenumber = numeric(),
        sample_id = character(),
        signal = numeric()
      )
      colnames(interp_ftir)[colnames(interp_ftir) == "signal"] <- colnames(
        ftir
      )[!(colnames(ftir) %in% c("wavenumber", "sample_id"))]

      for (i in seq_along(sample_ids)) {
        interp_signal <- stats::approx(
          ftir[ftir$sample_id == sample_ids[i], ]$wavenumber,
          ftir[
            ftir$sample_id == sample_ids[i],
            colnames(ftir) %in% c("absorbance", "transmittance")
          ],
          xout = interpolated_wavenumbers
        )$y
        interp_spectra <- data.frame(
          "wavenumber" = interpolated_wavenumbers,
          "sample_id" = sample_ids[i],
          "signal" = interp_signal
        )
        colnames(interp_spectra)[
          colnames(interp_spectra) == "signal"
        ] <- colnames(ftir)[!(colnames(ftir) %in% c("wavenumber", "sample_id"))]

        interp_ftir <- rbind(interp_ftir, interp_spectra)
      }

      # RECURSION AGAIN!
      return(average_spectra(
        ftir = interp_ftir,
        sample_ids = sample_ids,
        average_id = average_id
      ))
    }
  }

  attr(avg_spectra, "intensity") <- intensity_attribute

  return(avg_spectra)
}


#' Add or Subtract Scalar Value
#'
#' @description Add or subtract a constant (scalar) value to each data point in
#' a FTIR spectra. Shifts the plot up or down on the y axis by the specified
#' amount without any other change.
#'
#' Ajoute ou soustrait une valeur constante (scalaire) à chaque point de données
#' d'un spectre IRTF. Décale le tracé vers le haut ou vers le bas sur l'axe des
#' y de la valeur spécifiée sans aucune autre modification.
#'
#' @param ftir A data.frame of FTIR spectral data including spectra to be
#'   shifted.
#'
#'   Un data.frame de données spectrales IRTF comprenant les spectres à décalés.
#'
#' @param sample_ids A vector of sample IDs to be shifted. All sample IDs must
#'   be present in the `ftir` data.frame. If modifying all spectra, provide NA
#'   or NULL.
#'
#'   Un vecteur d'identifiants d'échantillons dont la moyenne doit être décalée.
#'   Tous les identifiants des échantillons doivent être présents dans le
#'   data.frame `ftir`. Si modifiez tous les spectres, indiquez NA ou NULL.
#'
#' @param value The numeric value to add or subtract.
#'
#'   Le valeur numerique d'ajute ou soustrait.
#'
#' @return A data.frame containing the adjusted FTIR spectra.
#'
#'   Un data.frame contenant les spectres IRTF ajustee.
#'
#' @examples
#' # Add 0.1 to each spectra in biodiesel
#' add_scalar_value(biodiesel, 0.1)
#'
#' # Subtract 0.05 from biodiesel_0 and biodiesel_0_25
#' subtract_scalar_value(biodiesel, 0.05, sample_ids = c("biodiesel_0", "biodiesel_0_25"))
#'
#' @name add_subtract_scalar
NULL

#' @export
#' @rdname add_subtract_scalar
#' @md
add_scalar_value <- function(ftir, value, sample_ids = NA) {
  ftir <- check_ftir_data(ftir)

  if (length(sample_ids) <= 1) {
    if (is.na(sample_ids) || is.null(sample_ids) || length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    }
  }

  if (any(!(sample_ids %in% unique(ftir$sample_id)))) {
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    cli::cli_abort(c(
      "All provided {.arg sample_ids} must be in {.arg ftir} data.",
      x = "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."
    ))
  }

  if (!is.numeric(value)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::add_scalar_value}. Provided {.arg value} must be numeric.",
      x = "You provided {.obj_type_friendly value}."
    ))
  }

  if ("absorbance" %in% colnames(ftir)) {
    ftir[ftir$sample_id %in% sample_ids, ]$absorbance <- ftir[
      ftir$sample_id %in% sample_ids,
    ]$absorbance +
      value
  } else {
    ftir[ftir$sample_id %in% sample_ids, ]$transmittance <- ftir[
      ftir$sample_id %in% sample_ids,
    ]$transmittance +
      value
  }

  return(ftir)
}

#' @export
#' @rdname add_subtract_scalar
subtract_scalar_value <- function(ftir, value, sample_ids = NA) {
  ftir <- check_ftir_data(ftir)

  if (!is.numeric(value)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::subtract_scalar_value}. Provided {.arg value} must be numeric.",
      x = "You provided {.obj_type_friendly value}."
    ))
  }
  return(add_scalar_value(
    ftir = ftir,
    sample_ids = sample_ids,
    value = value * -1
  ))
}


#' Shift Baseline
#'
#' @md
#' @description It may be desired to shift the baseline signal (0 for absorbance
#'   or 100 for transmittance) to aid in plotting the spectra. This can be done
#'   for all samples or a subset, using the same shift for all adjusted samples
#'   or calculated individually.
#'
#'   Shift to baseline/max transmittance can be done following
#'   one of a few methods:
#'  * To shift baseline based on the value at a given wavenumber:
#'   `shift_baseline(ftir, wavenumber_range = [numeric], method =
#'   'point')`
#'  * To shift baseline based on the average value across a provided wavenumber range:
#'   `shift_baseline(ftir, wavenumber_range = c([numeric], [numeric]),
#'   method = 'average')`
#'  * To shift baseline based on the value at the single lowest point of absorbance
#'   (or highest point of transmittance) across the whole spectra
#'   `shift_baseline(ftir, method = 'minimum')`
#'  * To shift baseline based on the value at the single lowest point of absorbance
#'   (or highest point of transmittance) in a given range
#'   `shift_baseline(ftir, wavenumber_range = c([numeric], [numeric]),
#'   method = 'minimum')`
#'
#'   To perform the exact same baseline adjustment on all samples, specify
#'   `individually = FALSE`. To adjust with a unique determination for each
#'   sample, specify `individualy = TRUE`.
#'
#'
#'   Il peut être souhaitable de décaler le signal de la ligne de base (0 pour
#'   l'absorbance ou 100 pour la transmittance) pour faciliter le tracé des
#'   spectres. Cela peut être fait pour tous les échantillons ou un
#'   sous-ensemble, en utilisant le même décalage pour tous les échantillons
#'   ajustés ou calculés individuellement.
#'
#'   Le décalage de la ligne de base/transmittance maximale peut
#'   être effectué en suivant l'une des méthodes suivantes :
#' * Pour décaler la ligne de base en fonction de la valeur à un nombre d'ondes donné :
#'   `shift_baseline(ftir, wavenumber_range = [numeric], method =
#'   'point')`
#' * Pour décaler la ligne de base en fonction de la valeur moyenne sur un nombre
#' d'ondes donné : #' `shift_baseline(ftir) = [numerique], method = 'point')
#'   `shift_baseline(ftir, wavenumber_range = c([numeric], [numeric]),
#'   method = 'average')`
#' * Pour décaler la ligne de base en fonction de la valeur du point d'absorbance
#' le plus bas (ou du point de transmittance le plus élevé) sur l'ensemble des spectres.
#'   `shift_baseline(ftir, method = 'minimum')`
#' * Décaler la ligne de base en fonction de la valeur du point d'absorbance le
#' plus bas (ou du point de transmittance le plus élevé) dans une gamme donnée.
#'   `shift_baseline(ftir, wavenumber_range = c([numeric], [numeric]),
#'   method = 'minimum')`
#'
#'   Pour effectuer exactement le même ajustement de la ligne de base sur tous
#'   les échantillons, spécifiez `individually = FALSE`. Pour ajuster avec une
#'   détermination unique pour chaque échantillon, spécifiez `individualy =
#'   TRUE`.
#' @param ftir A data.frame of FTIR spectral data including spectra to be
#'   baseline shifted
#'
#'   Un data.frame de données spectrales IRTF comprenant les spectres à ajuster
#'   à la ligne de base.
#'
#' @param sample_ids A vector of sample IDs to be shifted. All sample IDs must
#'   be present in the `ftir` data.frame. If adjusting all spectra, provide NA
#'   or NULL. Unlisted `sample_id` from `ftir` will be left alone.
#'
#'   Un vecteur d'ID d'échantillons à ajuster. Tous les ID d'échantillons doivent
#'   être présents dans la base de données `ftir` data.frame. Si l'ajustement
#'   concerne tous les spectres, fournir NA ou NULL. Les `sample_id` non listés
#'   de `ftir` seront laissés seuls.
#'
#' @param wavenumber_range If specifying a single point wavenumber; a single
#'   numeric value. If specifying a wavenumber range, then a vector of two
#'   numeric values.
#'
#'   Si l'on spécifie un nombre d'ondes ponctuel, une seule valeur numérique. Si
#'   l'on spécifie un nombre d'ondes, alors un vecteur de deux valeurs
#'   numériques.
#'
#' @param method One of three values:
#' * If adjusting by the value from a specific wavenumber, provide `"point"`,
#' * If adjusting by the average from a range, provide `"average"`.
#' * If adjusting by the minimum (for absorbance) or maximum (for transmittance) from a range or spectra, provide `"minimum"` or `"maximum"`, the appropriate transformation will be performed based on spectra type.
#'
#'   Une des trois valeurs :
#' * Si l'ajustement se fait par la valeur d'un nombre d'ondes spécifique, fournir `"point"`,
#' * Si l'ajustement se fait par la moyenne d'une gamme, fournir `"average"`.
#' * Si l'ajustement se fait par le minimum (pour l'absorbance) ou le maximum (pour la transmittance) d'une gamme ou de spectres, indiquez `"minimum"` ou `"maximum"`, la transformation appropriée sera effectuée en fonction du type de spectre.
#' @param individually If adjusting all samples by the same amount, specify
#'   `TRUE`, else specify `FALSE` for unique adjustments. When `TRUE`, the
#'   smallest absolute individual sample adjustment to achieve baseline will be
#'   applied to all named samples.
#'
#'   Si vous ajustez tous les échantillons de la même manière, spécifiez `TRUE`,
#'   sinon spécifiez `FALSE` pour des ajustements uniques. Si `TRUE`, le plus
#'   petit ajustement absolu d'un échantillon individuel pour atteindre la ligne
#'   de base sera appliqué à tous les échantillons nommés.
#'
#' @return A data.frame containing the adjusted FTIR spectra.
#'
#'   Un data.frame contenant les spectres IRTF ajustee.
#' @export
#'
#' @examples
#' # Adjust the biodiesel spectra to minimum for each sample
#' shift_baseline(biodiesel, method = "minimum", individually = TRUE)
shift_baseline <- function(
  ftir,
  sample_ids = NA,
  wavenumber_range = NA,
  method = "average",
  individually = TRUE
) {
  ftir <- check_ftir_data(ftir)

  if (length(sample_ids) <= 1) {
    if (is.na(sample_ids) || is.null(sample_ids) || length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    }
  }

  if (any(!(sample_ids %in% unique(ftir$sample_id)))) {
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. All provided {.arg sample_ids} must be in {.arg ftir} data.",
      x = "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."
    ))
  }

  if (length(wavenumber_range) < 1 || length(wavenumber_range) > 2) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be of length 1 or 2."
    ))
  }
  if (!(all(is.na(wavenumber_range)) || all(is.numeric(wavenumber_range)))) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be {.code numeric} or {.code NA}.",
      x = "You provided a {.obj_type_friendly wavenumber_range}."
    ))
  }

  if (!is.logical(individually)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg individually} must be a boolean value.",
      x = "You provided a {.obj_type_friendly individually}."
    ))
  }

  permitted_methods <- c("point", "average", "minimum", "maximum")
  if (length(method) != 1 || !(method %in% permitted_methods)) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg method} must be a string.",
      i = "{.arg method} must be one of {.val {permitted_methods}}."
    ))
  }

  if (method == "point" && length(wavenumber_range) == 2) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be one numeric value if {.code method = 'point'}.",
      i = "The value at the provided wavenumber will be used to baseline adjust data."
    ))
  }
  if (
    method %in%
      c("minimum", "maximum") &&
      all(length(wavenumber_range) == 1, !is.na(wavenumber_range))
  ) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be {.code NA} or two numeric values if {.code method = '{method}'}.",
      i = "The minimum (for absorbance spectra) or maximum (for transmittance spectra) value between the provided wavenumbers will be used to baseline adjust data.",
      i = "To adjust by a single point, call the function with {.code method = 'point'}"
    ))
  }

  if (method == "point") {
    if (length(wavenumber_range) != 1 || is.na(wavenumber_range)) {
      cli::cli_abort(c(
        "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be a single numeric value.",
        i = "The value at the provided wavenumber will be used to baseline adjust data."
      ))
    }
    if (individually) {
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          if (
            wavenumber_range <
              min(ftir[ftir$sample_id == sample_ids[i], ]$wavenumber) ||
              wavenumber_range >
                max(ftir[ftir$sample_id == sample_ids[i], ]$wavenumber)
          ) {
            cli::cli_warn(c(
              "Warning in {.fn PlotFTIR::shift_baseline}. Provided wavenumber is not within spectral range.",
              i = "Using {round(ftir[ftir$sample_id == sample_ids[i],]$wavenumber[which(abs(wavenumber_range - ftir[ftir$sample_id == sample_ids[i],]$wavenumber) == min(abs(wavenumber_range - ftir[ftir$sample_id == sample_ids[i],]$wavenumber)))], 0)} cm-1 instead of provided {round(wavenumber_range, 0)} cm-1."
            ))
          } else if (
            min(abs(
              wavenumber_range -
                ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
            )) >
              10
          ) {
            cli::cli_warn(c(
              "Warning in {.fn PlotFTIR::shift_baseline}. No wavenumber values in spectra within 10 cm-1 of supplied point.",
              i = "Using {round(ftir[ftir$sample_id == sample_ids[i],]$wavenumber[which(abs(wavenumber_range - ftir[ftir$sample_id == sample_ids[i],]$wavenumber) == min(abs(wavenumber_range - ftir[ftir$sample_id == sample_ids[i],]$wavenumber)))], 0)} cm-1 instead of provided {round(wavenumber_range, 0)} cm-1."
            ))
          }
          adj <- ftir[ftir$sample_id == sample_ids[i], ]$absorbance[which(
            abs(
              wavenumber_range -
                ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
            ) ==
              min(abs(
                wavenumber_range -
                  ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
              ))
          )]
          ftir[ftir$sample_id == sample_ids[i], ]$absorbance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$absorbance -
            adj
        } else {
          adj <- 100 -
            ftir[ftir$sample_id == sample_ids[i], ]$transmittance[which(
              abs(
                wavenumber_range -
                  ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
              ) ==
                min(abs(
                  wavenumber_range -
                    ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
                ))
            )]
          ftir[ftir$sample_id == sample_ids[i], ]$transmittance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$transmittance +
            adj
        }
      }
    } else {
      adj <- 1e10
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          adj_i <- ftir[ftir$sample_id == sample_ids[i], ]$absorbance[which(
            abs(
              wavenumber_range -
                ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
            ) ==
              min(abs(
                wavenumber_range -
                  ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
              ))
          )]
        } else {
          adj_i <- 100 -
            ftir[ftir$sample_id == sample_ids[i], ]$transmittance[which(
              abs(
                wavenumber_range -
                  ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
              ) ==
                min(abs(
                  wavenumber_range -
                    ftir[ftir$sample_id == sample_ids[i], ]$wavenumber
                ))
            )]
        }
        adj <- c(adj, adj_i)[which.min(abs(c(adj, adj_i)))] # returns absolute minimum but keeps the sign
      }
      if ("absorbance" %in% colnames(ftir)) {
        ftir[ftir$sample_id %in% sample_ids, ]$absorbance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$absorbance -
          adj
      } else {
        ftir[ftir$sample_id %in% sample_ids, ]$transmittance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$transmittance +
          adj
      }
    }
  } else if (method == "average") {
    if (all(is.na(wavenumber_range))) {
      cli::cli_warn(c(
        "Adjusting spectra baseline by the average of all values is not analytically useful",
        i = "Provide a wavenumber range to adjust by the average in that spectral region."
      ))
      wavenumber_range <- range(
        ftir[ftir$sample_id %in% sample_ids, ]$wavenumber
      )
    } else if (length(wavenumber_range) != 2) {
      cli::cli_abort(c(
        "Error in {.fn PlotFTIR::shift_baseline}. {.arg wavenumber_range} must be two numeric values.",
        i = "The average value between the provided wavenumbers will be used to baseline adjust data."
      ))
    }
    if (individually) {
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          adj <- mean(
            ftir[
              ftir$sample_id == sample_ids[i] &
                ftir$wavenumber >= min(wavenumber_range) &
                ftir$wavenumber <= max(wavenumber_range),
            ]$absorbance
          )
          ftir[ftir$sample_id == sample_ids[i], ]$absorbance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$absorbance -
            adj
        } else {
          adj <- 100 -
            mean(
              ftir[
                ftir$sample_id == sample_ids[i] &
                  ftir$wavenumber >= min(wavenumber_range) &
                  ftir$wavenumber <= max(wavenumber_range),
              ]$transmittance
            )
          ftir[ftir$sample_id == sample_ids[i], ]$transmittance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$transmittance +
            adj
        }
      }
    } else {
      adj <- 1e10
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          adj_i <- mean(
            ftir[
              ftir$sample_id == sample_ids[i] &
                ftir$wavenumber >= min(wavenumber_range) &
                ftir$wavenumber <= max(wavenumber_range),
            ]$absorbance
          )
        } else {
          adj_i <- 100 -
            mean(
              ftir[
                ftir$sample_id == sample_ids[i] &
                  ftir$wavenumber >= min(wavenumber_range) &
                  ftir$wavenumber <= max(wavenumber_range),
              ]$transmittance
            )
        }
        adj <- c(adj, adj_i)[which.min(abs(c(adj, adj_i)))] # returns absolute minimum but keeps the sign
      }
      if ("absorbance" %in% colnames(ftir)) {
        ftir[ftir$sample_id %in% sample_ids, ]$absorbance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$absorbance -
          adj
      } else {
        ftir[ftir$sample_id %in% sample_ids, ]$transmittance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$transmittance +
          adj
      }
    }
  } else {
    # method is in c("minimum", "maximum")
    if (all(is.na(wavenumber_range))) {
      wavenumber_range <- range(
        ftir[ftir$sample_id %in% sample_ids, ]$wavenumber
      )
    }
    if (individually) {
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          adj <- min(
            ftir[
              ftir$sample_id == sample_ids[i] &
                ftir$wavenumber >= min(wavenumber_range) &
                ftir$wavenumber <= max(wavenumber_range),
            ]$absorbance
          )
          ftir[ftir$sample_id == sample_ids[i], ]$absorbance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$absorbance -
            adj
        } else {
          adj <- 100 -
            max(
              ftir[
                ftir$sample_id == sample_ids[i] &
                  ftir$wavenumber >= min(wavenumber_range) &
                  ftir$wavenumber <= max(wavenumber_range),
              ]$transmittance
            )
          ftir[ftir$sample_id == sample_ids[i], ]$transmittance <- ftir[
            ftir$sample_id == sample_ids[i],
          ]$transmittance +
            adj
        }
      }
    } else {
      adj <- 1e10
      for (i in seq_along(sample_ids)) {
        if ("absorbance" %in% colnames(ftir)) {
          adj_i <- min(
            ftir[
              ftir$sample_id == sample_ids[i] &
                ftir$wavenumber >= min(wavenumber_range) &
                ftir$wavenumber <= max(wavenumber_range),
            ]$absorbance
          )
        } else {
          adj_i <- 100 -
            max(
              ftir[
                ftir$sample_id == sample_ids[i] &
                  ftir$wavenumber >= min(wavenumber_range) &
                  ftir$wavenumber <= max(wavenumber_range),
              ]$transmittance
            )
        }
        adj <- c(adj, adj_i)[which.min(abs(c(adj, adj_i)))] # returns absolute minimum but keeps the sign
      }
      if ("absorbance" %in% colnames(ftir)) {
        ftir[ftir$sample_id %in% sample_ids, ]$absorbance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$absorbance -
          adj
      } else {
        ftir[ftir$sample_id %in% sample_ids, ]$transmittance <- ftir[
          ftir$sample_id %in% sample_ids,
        ]$transmittance +
          adj
      }
    }
  }

  return(ftir)
}


#' Normalize FTIR spectra
#'
#' @description Normalizing spectra restricts the range of absorbance values from
#' 0 to 1 inclusive. This function shifts and scales spectra to achieve this
#' absorbance range. It can be applied to a whole spectral set or just one
#' sample, and across the entire spectra or by normalizing within a wavenumber
#' region. This function does not operate on transmittance data, it will return
#' an error.
#'
#' La normalisation des spectres restreint la gamme des valeurs d'absorbance de 0
#' à 1 inclus. Cette fonction décale et met à l'échelle les spectres pour
#' atteindre cette gamme d'absorbance. Elle peut être appliquée à un ensemble de
#' spectres ou à un seul échantillon, et sur l'ensemble des spectres ou en
#' normalisant dans une région de nombre d'ondes. Cette fonction ne fonctionne
#' pas sur les données de transmittance, elle renverra une erreur.
#'
#' @inherit shift_baseline params return
#' @export
#' @examples
#' # Normalize all samples in `biodiesel`
#' normalize_spectra(biodiesel)
#'
#' # Normalize just `paper` and `isopropanol` spectra from 4000 to 3100 cm^-1^
#' normalize_spectra(sample_spectra,
#'   sample_ids = c("paper", "isopropanol"),
#'   wavenumber_range = c(4000, 3100)
#' )
normalize_spectra <- function(ftir, sample_ids = NA, wavenumber_range = NA) {
  # Check inputs
  ftir <- check_ftir_data(ftir)
  if (
    "transmittance" %in%
      colnames(ftir) ||
      attr(ftir, "intensity") == "transmittance"
  ) {
    # Can't normalize transmission spectra
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::normalize_spectra}: Normalization of transmittance spectra not supported.",
      i = "Convert spectra to absorbance using {.fn transmittance_to_absorbance} then try again."
    ))
  }

  if (length(sample_ids) <= 1) {
    if (is.na(sample_ids) || is.null(sample_ids) || length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    }
  }

  if (any(!(sample_ids %in% unique(ftir$sample_id)))) {
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::normalize_spectra}. All provided {.arg sample_ids} must be in {.arg ftir} data.",
      x = "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."
    ))
  }

  if (all(is.na(wavenumber_range))) {
    wavenumber_range <- range(ftir$wavenumber, na.rm = TRUE)
  }

  if (length(wavenumber_range) < 2 || length(wavenumber_range) > 2) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::normalize_spectra}. {.arg wavenumber_range} must be of length 2."
    ))
  }
  if (any(is.na(wavenumber_range)) | !all(is.numeric(wavenumber_range))) {
    cli::cli_abort(c(
      "Error in {.fn PlotFTIR::normalize_spectra}. {.arg wavenumber_range} must be {.code numeric} or {.code NA}.",
      x = "You provided a {.obj_type_friendly wavenumber_range}."
    ))
  }

  for (i in seq_along(sample_ids)) {
    sid <- sample_ids[i]
    spectra <- ftir[ftir$sample_id == sid, ]
    spectra_range <- range(
      spectra[
        spectra$wavenumber >= min(wavenumber_range) &
          spectra$wavenumber <= max(wavenumber_range),
      ]$absorbance
    )
    spectra$absorbance <- spectra$absorbance - spectra_range[1]
    spectra$absorbance <- spectra$absorbance *
      (1 / (spectra_range[2] - spectra_range[1]))
    ftir[ftir$sample_id == sid, ]$absorbance <- spectra$absorbance
  }

  attr(ftir, "intensity") <- "normalized absorbance"

  return(ftir)
}


#' Convert Between Absorbance and Transmittance
#'
#' @description These functions allow for the convenient conversion between
#'   \%Transmittance and Absorbance units for the Y axis.
#'
#'   Converting between \%Transmittance and absorbance units for the Y axis is
#'   not a simple flipping of axis or inversion. Instead, the two are related by
#'   the following formulas:
#'
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'   and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }.
#'
#'   Ces fonctions permettent une conversion pratique entre les unités
#'   \%Transmittance et Absorbance pour l'axe Y. La conversion entre les unités
#'   \%Transmittance et Absorbance pour l'axe Y n'est pas un simple retournement
#'   d'axe ou une inversion. Au lieu de cela, les deux sont liés par les
#'   formules suivantes :
#'
#' \deqn{
#'  A=-log_{10}(\tfrac{\%T}{100})
#' }
#'   and
#' \deqn{
#'  \%T=10^{-A}\cdot 100
#' }
#'
#' @param ftir A data.frame of FTIR spectral data including column to be
#'   converted. Can't contain both `absorbance` and `transmittance` column as
#'   the receiving column would be overwritten
#'
#'   Un data.frame de données spectrales IRTF incluant la colonne à convertir.
#'   Ne peut pas contenir les colonnes `absorbance` et `transmittance` car la
#'   colonne de réception serait écrasée.
#'
#' @return a data.frame of FTIR spectral data with conversion between absorbance
#'   or transmittance as requested. Note the original data column is removed
#'   since FTIR spectral data frames can't be fed into plotting functions with
#'   both transmittance and absorbance data included.
#'
#'   un data.frame de données spectrales IRTF avec conversion entre l'absorbance
#'   ou la transmittance comme demandé. Notez que la colonne de données
#'   d'origine est supprimée car les trames de données spectrales IRTF ne
#'   peuvent pas être introduites dans les fonctions de tracé avec les données
#'   de transmittance et d'absorbance incluses.
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
  ftir <- check_ftir_data(ftir)
  normalized <- grepl("normalized", attr(ftir, "intensity"))
  if (
    !("absorbance" %in% colnames(ftir)) ||
      attr(ftir, "intensity") %in%
        c("transmittance", "normalized transmittance")
  ) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::absorbance_to_transmittance}. {.arg ftir} must be absorbance data or contain a {.var absorbance} column."
    )
  }
  ftir$transmittance <- (10^(ftir$absorbance * -1)) * 100
  ftir$absorbance <- NULL

  # this drops the attributes
  ftir <- ftir[, c("wavenumber", "transmittance", "sample_id")]

  if (normalized) {
    attr(ftir, "intensity") <- "normalized transmittance"
  } else {
    attr(ftir, "intensity") <- "transmittance"
  }

  return(ftir)
}

#' @export
#' @rdname conversion
transmittance_to_absorbance <- function(ftir) {
  ftir <- check_ftir_data(ftir)
  normalized <- grepl("normalized", attr(ftir, "intensity"))

  if (
    !("transmittance" %in% colnames(ftir)) ||
      attr(ftir, "intensity") %in% c("absorbance", "normalized absorbance")
  ) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::transmittance_to_absorbance}. {.arg ftir} must be transmittance data or contain a {.var transmittance} column."
    )
  }

  ftir$absorbance <- -log(ftir$transmittance / 100, base = 10)
  ftir$transmittance <- NULL

  ftir <- ftir[, c("wavenumber", "absorbance", "sample_id")]

  if (normalized) {
    attr(ftir, "intensity") <- "normalized absorbance"
  } else {
    attr(ftir, "intensity") <- "absorbance"
  }

  return(ftir)
}


#' Smooth FTIR with Savitzky-Golay filter
#'
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance`. The `absorbance` column may be
#'   replaced by a `transmittance` column for transmittance plots.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance`. La colonne `absorbance` peut
#'   être remplacée par une colonne `transmittance` pour les tracés de
#'   transmittance.
#' @param polynomial Savitzky-Golay polynomial term.
#'
#'   Terme polynomial de Savitzky-Golay.
#' @param points Savitzky-Golay points term.
#'
#'   Terme de points de Savitzky-Golay
#' @param derivative Which derivative to return (default = 0 to smooth spectrum,
#'   but can alos determine smoothed derivatives)
#'
#'   Dérivée à retourner (par défaut = 0 pour lisser le spectre, mais on peut
#'   aussi déterminer des dérivées lissées)
#'
#' @return a data.frame with an FTIR spectrum, smoothed (or the derivative)
#'
#'   un data.frame avec un FTIR lissé, lissé (ou la dérivée lissée)
#' @export
#'
#' @seealso [signal::sgolayfilt()]
#'
#' @references
#' * Savitzky, A.; Golay, M.J.E. (1964). "Smoothing and Differentiation of Data by Simplified Least Squares Procedures". Analytical Chemistry 36. pp. 1627–1639. doi:10.1021/ac60214a047
#' @examples
#' # Load the isopropanol sample spectrum
#' ftir_data <- sample_spectra[
#'   sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Apply smoothing
#' ftir_smoothed <- smooth_ftir(ftir_data)

#' # --- Optional: Visualize the results ---
#' \dontrun{
#'   plot_ftir(ftir_smoothed, plot_title = "Smoothed FTIR")
#' }
smooth_ftir <- function(ftir, polynomial = 2, points = 13, derivative = 0) {
  if (!is.null(attr(ftir, "treatment"))) {
    if (grepl("smoothed", attr(ftir, "treatment"))) {
      cli::cli_warn(c(
        "Warning in {.fn PlotFTIR::smooth_ftir}: Spectra have been previously smoothed.",
        i = "Repeat smoothing of spectra may eliminate small or shoulder peaks."
      ))
    } else {
      attr(ftir, "treatment") <- paste(attr(ftir, "treatment"), "smoothed")
    }
  } else {
    attr(ftir, "treatment") <- "smoothed"
  }

  ftir <- check_ftir_data(ftir)

  for (i in seq_along(unique(ftir$sample_id))) {
    s <- unique(ftir$sample_id)[i]
    if ("absorbance" %in% colnames(ftir)) {
      intensity <- ftir[ftir$sample_id == s, ]$absorbance
    } else {
      intensity <- ftir[ftir$sample_id == s, ]$transmittance
    }

    smoothed_spectra <- signal::sgolayfilt(
      intensity,
      p = polynomial,
      n = points,
      m = derivative
    )

    if ("absorbance" %in% colnames(ftir)) {
      ftir[ftir$sample_id == s, ]$absorbance <- smoothed_spectra
    } else {
      ftir[ftir$sample_id == s, ]$transmittance <- smoothed_spectra
    }
  }

  return(ftir)
}


#' Baseline FTIR
#'
#' @description Correct the baseline of an FTIR spectrum using one of the
#' techniques available in the [baseline::baseline()] package.
#'
#' Corrigez la ligne de base d'un spectre IRTF en utilisant l'une des techniques
#' disponibles dans le package [baseline::baseline()].
#'
#' @param ftir A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, and `absorbance`. The `absorbance` column may be replaced by
#'   a `transmittance` column for transmittance plots.
#'
#'   Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
#'   et `absorbance`. La colonne `absorbance` peut être remplacée par une
#'   colonne `transmittance` pour les tracés de transmission.
#' @param method A method from [baseline::baseline()]. For FTIR data, best
#'   results are achieved by selecting either `modpolyfit`, `peakDetection` or
#'   `rfbaseline`.
#'
#'   Une méthode de [baseline::baseline()]. Pour les données IRTF, les meilleurs
#'   résultats sont obtenus en sélectionnant soit `modpolyfit`, soit
#'   `peakDetection`, soit `rfbaseline`.
#' @param ... Additional parameters required by specific methods in
#'   [baseline::baseline()].
#'
#'   Paramètres supplémentaires requis par certaines méthodes de
#'   [baseline::baseline()].
#'
#' @return A FTIR spectral data.frame with baseline corrected intensity column.
#'
#' Un data.frame contenant le spectre IRTF corrigé de la ligne de base
#'
#' @export
#'
#' @seealso [baseline::baseline()]
#'
#' @references
#' * Kristian Hovde Liland, Trygve Almøy, Bjørn-Helge Mevik (2010) Optimal Choice of Baseline Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016. doi:10.1366/000370210792434350
#' * Chad A. Lieber and Anita Mahadevan-Jansen (2003) Automated Method for Subtraction of Fluorescence from Biological Raman Spectra, Applied Spectroscopy 57, pp. 1363-1367. doi:10.1366/000370203322554518
#' * Kevin R. Coombes et al. (2003) Quality control and peak finding for proteomics data collected from nipple aspirate fluid by surface-enhanced laser desorption and ionization. Clinical Chemistry 49, pp. 1615-1623. doi:10.1373/49.10.1615
#' * Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field, James A. Dodd (2001) Baseline subtraction using robust local regression estimation. Journal of Quantitative Spectroscopy and Radiative Transfer 68, pp.. 179-193. doi:10.1016/S0022-4073(00)00021-2
#' * Xianchun Shen et al. (2018) Applied Optics 57 pp. 5794-5799 doi:10.1364/AO.57.0057947
#' @examples
#' # Load the isopropanol sample spectrum
#' ftir_data <- sample_spectra[
#'   sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Apply baseline correction using the default 'modpolyfit' method
#' ftir_baselined_modpoly <- baseline_ftir(ftir_data)
#'
#' # Apply baseline correction using the 'lowpass' method
#' ftir_baselined_lowpass <- baseline_ftir(ftir_data, method = "lowpass")
#'
#' # --- Optional: Visualize the results ---
#' \dontrun{
#'   plot_ftir(ftir_baselined_modpoly, plot_title = "ModPoly Baselined FTIR")
#'
#'   plot_ftir(ftir_baselined_lowpass, plot_title = "Lowpass Baselined FTIR")
#' }
baseline_ftir <- function(ftir, method = "modpolyfit", ...) {
  if (
    !(method %in%
      c(
        "als",
        "fillPeaks",
        "irls",
        "lowpass",
        "medianWindow",
        "modpolyfit",
        "peakDetection",
        "rfbaseline",
        "rollingBall",
        "shirley",
        "TAP"
      ))
  ) {
    cli::cli_abort(
      "Error in {.pkg PlotFTIR::baseline_ftir}: {.arg method} should be one of which the {.pkg baseline} is capable of performing."
    )
  }

  if (method %in% c("TAP", "shirley", "rollingBall")) {
    cli::cli_warn(
      "Method {.code {method}} was not designed for use with FTIR data."
    )
  }

  ftir <- check_ftir_data(ftir)

  if (!is.null(attr(ftir, "treatment"))) {
    if (grepl("baselined", attr(ftir, "treatment"))) {
      cli::cli_warn(c(
        "Warning in {.fn PlotFTIR::baseline_ftir}: Spectra have been previously baselined.",
        i = "Repeat baseline adjustment of spectra may produce unexpected results."
      ))
    } else {
      attr(ftir, "treatment") <- paste(attr(ftir, "treatment"), "baselined")
    }
  } else {
    attr(ftir, "treatment") <- "baselined"
  }

  for (i in seq_along(unique(ftir$sample_id))) {
    s <- unique(ftir$sample_id)[i]

    if ("absorbance" %in% colnames(ftir)) {
      intensity <- ftir[ftir$sample_id == s, ]$absorbance
    } else {
      cli::cli_alert_danger(
        "Note from {.fn PlotFTIR::baseline_ftir}: Baselining with transmittance spectra may not behave as expected."
      )
      intensity <- ftir[ftir$sample_id == s, ]$transmittance
    }

    baselined_spectra <- baseline::baseline(
      matrix(intensity, ncol = length(intensity)),
      method = method,
      ... = ...
    )
    corrected <- as.vector(baseline::getCorrected(baselined_spectra))

    if ("absorbance" %in% colnames(ftir)) {
      ftir[ftir$sample_id == s, ]$absorbance <- corrected
    } else {
      ftir[ftir$sample_id == s, ]$transmittance <- corrected
    }
  }
  return(ftir)
}


# Function(s) for dealing with reflectance in absorbance spectra - continuum removal
# * Clark, R.N. and Roush, T.L. (1984) J. Geophysical Res. 89 pp 6329-6340 <doi:10.1029/JB089iB07p06329>

# Cubic Spline continuum is recommended, "although straight-line segments would produce nearly the same result in this case"
# ToDo: test cubic vs hermite.
# Should be done as division in reflectance spectra, or as subtraction for absorbtive spectra

#' Remove Continuum from FTIR Spectra
#'
#' @description This function removes the continuum from FTIR spectra using
#'   either spline or linear interpolation. The continuum is defined as the
#'   convex hull of the spectrum, and is either subtracted or divided from the
#'   original spectrum. This is a common preprocessing step in reflectance
#'   spectroscopy to highlight absorption features.
#'
#'   Cette fonction supprime le continuum des spectres IRTF en utilisant une
#'   interpolation spline ou linéaire. Le continuum est défini comme l'enveloppe
#'   convexe du spectre, et est soit soustrait, soit divisé du spectre original.
#'   Il s'agit d'une étape de prétraitement courante en spectroscopie de
#'   réflectance pour mettre en évidence les caractéristiques d'absorption.
#'
#' @param ftir A data.frame in long format with a single FTIR spectra in columns
#'   `sample_id`, `wavenumber`, and `absorbance` or `transmittance`.
#'
#'   Un data.frame au format long avec un seul spectre IRTF dans les colonnes
#'   `sample_id`, `wavenumber`, et `absorbance` ou `transmittance`.
#' @param type The type of interpolation to use for the continuum. Options are
#'   `spline` (default) or `linear`.
#'
#'   Le type d'interpolation à utiliser pour le continuum. Les options sont
#'   `spline` (par défaut) ou `linear`.
#' @param application How to apply the continuum to the spectra. Options are
#'   `subtraction` (default) or `division`.
#'
#'   Comment appliquer le continuum aux spectres. Les options sont `subtraction`
#'   (par défaut) ou `division`.
#' @param ... Additional arguments (currently unused).
#'
#'   Arguments supplémentaires (actuellement inutilisés).
#'
#' @return A data.frame with the continuum removed from the spectra. The
#'   `absorbance` or `transmittance` column will be modified.
#'
#'   Un data.frame avec le continuum supprimé des spectres. La colonne
#'   `absorbance` ou `transmittance` sera modifiée.
#'
#' @export
#'
#' @references Clark, R.N. and Roush, T.L. (1984) J. Geophysical Res. 89 pp
#'   6329-6340 <doi:10.1029/JB089iB07p06329>
#'
#' @examples
#' # Load the isopropanol sample spectrum
#' ftir_data <- sample_spectra[
#'   sample_spectra$sample_id == "isopropanol",
#' ]
#'
#' # Remove the continuum using spline interpolation and subtraction
#' ftir_no_continuum <- remove_continuum_ftir(ftir_data)
#'
#' # Remove the continuum using linear interpolation and division
#' ftir_no_continuum_linear_div <- remove_continuum_ftir(
#'   ftir_data,
#'   type = "linear",
#'   application = "division"
#' )
remove_continuum_ftir <- function(
  ftir,
  type = "spline",
  application = "subtraction",
  ...
) {
  # error checking
  ftir <- PlotFTIR::check_ftir_data(ftir)

  if (!(application %in% c("subtraction", "division"))) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::remove_continuum_ftir}: {.arg application} must be either {.val subtraction} or {.val division}."
    )
  }

  if (!(type %in% c("spline", "linear"))) {
    cli::cli_abort(
      "Error in {.fn PlotFTIR::remove_continuum_ftir}: {.arg type} must be either {.val spline} or {.val linear}."
    )
  }

  if (!is.null(attr(ftir, "treatment"))) {
    if (grepl("continuum removed", attr(ftir, "treatment"))) {
      cli::cli_warn(c(
        "Warning in {.fn PlotFTIR::remove_continuum_ftir}: Spectra have previously had continuum removed.",
        i = "Repeat continuum removal of spectra may produce unexpected results."
      ))
    } else {
      attr(ftir, "treatment") <- paste(
        attr(ftir, "treatment"),
        "continuum removed"
      )
    }
  } else {
    attr(ftir, "treatment") <- "continuum removed"
  }

  # needs to be ordered for rubberbanding
  ftir <- ftir[order(ftir$wavenumber), ]
  x <- ftir$wavenumber
  if ("absorbance" %in% colnames(ftir)) {
    y <- ftir$absorbance
    # invert absorbance for convex hull
    y_for_hull <- 1 / y
  } else {
    y <- ftir$transmittance
    y_for_hull <- y
  }

  # find convex shape of curve (rubberband)
  # add bumpers, build hull, and sort
  hull <- sort(grDevices::chull(
    c(x[1] - 1, x, x[length(x)] + 1),
    c(0, y_for_hull, 0)
  ))
  #take off the bumpers, subtract by 1 to 'shift everything' back to actual x indexes
  hull <- hull[2:(length(hull) - 1)] - 1

  # Build functions for interpolating
  if (type == "spline") {
    continuum <- stats::splinefun(x = x[hull], y = y_for_hull[hull])(x)
  } else {
    # Type is linear
    continuum <- stats::approx(x = x[hull], y = y[hull], xout = x)$y
  }

  # Apply the continuum to the spectra by subtraction or division
  if (application == "subtraction") {
    y_corrected <- y - continuum
  } else {
    y_corrected <- y / continuum
  }

  if ("absorbance" %in% colnames(ftir)) {
    ftir$absorbance <- 1 / y_corrected
  } else {
    ftir$transmittance <- y_corrected
  }

  return(ftir)
}
