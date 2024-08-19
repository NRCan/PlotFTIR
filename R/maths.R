## Holds functions to do maths on spectra Currently: average like spectra
##
## Plans: rebaseline (scalar subtraction, subtract minimum, subtract wavelength value, average over wavelength range)

#' Average FTIR Spectra
#'
#' @description Calculates an average of two or more spectra. Spectra must have identical x axis (wavenumber) data
#'   points.
#'
#'   Calcule la moyenne de deux spectres ou plus. Les spectres doivent avoir des points de données identiques sur l'axe
#'   des x (nombre d'ondes).
#'
#' @param ftir A data.frame of FTIR spectral data including spectra to be converted.
#'
#'   Un data.frame de données spectrales IRTF comprenant les spectres à convertir.
#'
#' @param sample_ids A vector of sample IDs to be averaged together. All sample IDs must be present in the `ftir`
#'   data.frame. If averaging all spectra, provide NA or NULL.
#'
#'   Un vecteur d'identifiants d'échantillons dont la moyenne doit être calculée.. Tous les identifiants des
#'   échantillons doivent être présents dans le data.frame `ftir`. Si la moyenne est calculée pour tous les spectres,
#'   indiquez NA ou NULL.
#'
#' @param average_id The name to be used as sample_id for the averaged spectra.
#'
#'   Le nom à utiliser en tant qu'identifiant d'échantillon pour les spectres moyennés.
#'
#' @return A data.frame containing the averaged FTIR spectra, with sample_id corresponding to the provided `average_id`.
#'
#'   Un data.frame contenant les spectres IRTF moyennés, l'identifiant de l'échantillon correspondant à l'identifiant
#'   `average_id` fourni.
#'
#' @export
#'
#' @examples
#' #Calculate the average of biodiesel B5 spectra and the unknown spectra
#'
#' average_spectra(biodiesel, c("biodiesel_5_0", "biodiesel_B5", "diesel_unknown"))
average_spectra <- function(ftir, sample_ids = NA, average_id = "averaged_spectra") {
  if (!(is.data.frame(ftir))) {
    cli::cli_abort("{.arg ftir} must be a data frame. You provided {.obj_type_friendly ftir}.")
  }
  if (!("sample_id" %in% colnames(ftir))) {
    cli::cli_abort(c("{.arg ftir} is missing a column.",
                     i = "It must contain a column named {.var sample_id}."
    ))
  }
  if (!("wavenumber" %in% colnames(ftir))) {
    cli::cli_abort(c("{.arg ftir} is missing a column.",
                     i = "It must contain a column named {.var wavenumber}."
    ))
  }
  if (!any(colnames(ftir) == "absorbance", colnames(ftir) == "transmittance")) {
    cli::cli_abort("{.arg ftir} must have one of {.var absorbance} or {.var transmittance} columns.")
  }
  if ("absorbance" %in% colnames(ftir) && "transmittance" %in% colnames(ftir)) {
    cli::cli_abort("{.arg ftir} cannot contain both {.var absorbance} and {.var transmittance} columns.")
  }
  if (any(!(colnames(ftir) %in% c("sample_id", "wavenumber", "absorbance", "transmittance")))) {
    cli::cli_abort("{.arg ftir} may only contain columns {.var sample_id}, {.var wavenumber}, and one of {.var absorbance} or {.var transmittance}.")
  }

  if(length(sample_ids) <= 1){
    if (is.na(sample_ids) | is.null(sample_ids) | length(sample_ids) == 0) {
      sample_ids <- unique(ftir$sample_id)
    } else if (sample_ids %in% unique(ftir$sample_id)){
      # Just one sampleID provided, return with new name
      avg_spectra <- ftir[ftir$sample_id == sample_ids,]
      avg_spectra$sample_id <- average_id
      return(avg_spectra)
    }
  }

  if(any(!(sample_ids %in% unique(ftir$sample_id)))){
    mismatch <- sample_ids[!(sample_ids %in% unique(ftir$sample_id))]
    nmismatch <- length(mismatch)
    cli::cli_abort(c("All provided {.arg sample_ids} must be in {.arg ftir} data.",
                   x = "The following {.arg sample_id{?s}} are not present: {.val {mismatch}}."))
  }

  if(!is.character(average_id)){
    cli::cli_abort("{.arg average_id} must be a character value.")
  }

    #drop everything not needed
    ftir <- ftir[ftir$sample_id %in% sample_ids, ]

    #check wavenumbers matches
    first_wavenumbers <- ftir[ftir$sample_id == sample_ids[1], "wavenumber"]
    other_wavenumbers <- ftir[ftir$sample_id != sample_ids[1], "wavenumber"]
    if(!all(first_wavenumbers %in% other_wavenumbers) & !all(other_wavenumbers %in% first_wavenumbers)){
      cli::cli_abort("There is a mismatch in the wavenumber axis between sample_ids. Please check input data")
    }

    #make average
    if("absorbance" %in% names(ftir)){
      avg_spectra <- aggregate(absorbance ~ wavenumber, data = ftir, FUN = mean)
    } else {
      avg_spectra <- aggregate(transmittance ~ wavenumber, data = ftir, FUN = mean)
    }

    avg_spectra$sample_id <- average_id


  return(avg_spectra)

}
