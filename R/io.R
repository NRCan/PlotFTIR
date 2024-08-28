#' Read FTIR file
#'
#' @description Reads a provided file and returns a data.frame in the proper format for PlotFTIR functions.
#'
#' @param path
#' Path to the file. Default is the current working directory, as ".".
#'
#' Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de travail actuel, sous la forme ".".
#'
#' @param file
#' File name, required.
#'
#' Nom du fichier, obligatoire.
#'
#' @param sample_name
#' Name for sample_id column in the returned data.frame. If not provided, the file name is used without the extension.
#'
#' Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas fourni, le nom du fichier est utilisé sans l'extension.
#'
#' @param ...
#' Additional parameters to pass to the file reading function. For CSV files, see [utils::read.csv()], it may be wise to pass `col.names` to disambiguate the input data.
#'
#' Paramètres supplémentaires à transmettre à la fonction de lecture de fichier. Pour les fichiers CSV, voir [utils::read.csv()], il peut être judicieux de passer `col.names` pour désambiguïser les données d'entrée.
#'
#' @return
#' a data.frame containing the spectral data from the file.
#'
#' un data.frame contenant les données spectrales du fichier.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a .csv file from the working directory and call it `sample1`
#' read_ftir(".", "ftir_sample_1.csv", "sample1")
#' }
#' @md
read_ftir <- function(path = ".", file, sample_name = NA, ...) {
  # Check inputs
  if (length(path) != 1 || is.na(path)) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg path} must be a single string value.")
  }
  if (length(file) != 1 || is.na(file)) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg file} must be a single string value.")
  }
  if (length(sample_name) != 1) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg sample_name} must be a single string value or single {.val NA}.")
  }
  if (!is.na(sample_name) && !is.character(sample_name)) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg sample_name} must be a string value or {.val NA}.")
  }

  # check file exists
  if (!file.exists(file.path(path, file))) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. File {.val file.path(path, file)} does not appear to exist.")
  }

  # Dispatch
  filetype <- tools::file_ext(file)

  if (filetype %in% c("csv", "txt")) {
    return(read_ftir_csv(path = path, file = file, sample_name = sample_name, ...))
  } else if (filetype == "spc") {
    return(read_ftir_spc(path = path, file = file, sample_name = sample_name, ...))
  } else if (filetype == "a2r") {
    return(read_ftir_a2r(path = path, file = file, sample_name = sample_name, ...))
  } else if (filetype == "asp") {
    return(read_ftir_asp(path = path, file = file, sample_name = sample_name, ...))
  } else {
    cli::cli_abort(c("Error in {.fn PlotFTIR::read_ftir}. Input file of type {{filetype}} could not be processed.",
      i = "PlotFTIR currently supports .csv/.txt and .asp files."
    ))
  }
}

read_ftir_csv <- function(path, file, sample_name = NA, ...) {
  input_file <- utils::read.csv(file = file.path(path, file), ...)

  if (ncol(input_file) > 2) {
    # this file is too ambiguous to read
    cli::cli_abort(c("Error in {.fn PlotFTIR:::read_ftir_csv}. Input file has too many columns.",
      x = "{.fn PlotFTIR::read_ftir} is only equipped to read single spectra files.",
      i = "Input .csv files should have only wavenumber and {.arg intensity}, {.arg absorbance}, or {.arg transmittance} values."
    ))
  }

  colnames(input_file) <- tolower(colnames(input_file))

  if (!("wavenumber" %in% colnames(input_file))) {
    if (any(c("x", "energy", "wavelength") %in% colnames(input_file))) {
      colnames(input_file)[colnames(input_file) %in% c("x", "energy", "wavelength")] <- "wavenumber"
    } else {
      # One of the input values should have a correlation to a integer sequence near one, the other shouldn't.
      if (stats::cor(input_file[, 1], seq_along(input_file[, 1])) == 1 && stats::cor(input_file[, 2], seq_along(input_file[, 2])) < 0.95) {
        cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[1]}} is {.val wavenumber}.")
        colnames(input_file)[1] <- "wavenumber"
      } else if (stats::cor(input_file[, 2], seq_along(input_file[, 2])) == 1 && stats::cor(input_file[, 1], seq_along(input_file[, 1])) < 0.95) {
        cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[2]}} is {.val wavenumber}.")
        colnames(input_file)[2] <- "wavenumber"
      } else {
        cli::cli_abort(c("Error in {.fn PlotFTIR:::read_ftir_csv}. Could not confidently determine which column contains wavenumber data.",
          i = "Check the input file or provide a {.arg col.names} input parameter to simplify reading data."
        ))
      }
    }
  }
  if (!("absorbance" %in% colnames(input_file)) && !("transmittance" %in% colnames(input_file))) {
    if (max(input_file[, colnames(input_file) != "wavenumber"]) > 10) {
      # must be intensity = transmittance
      cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[colnames(input_file) != 'wavenumber']}} is {.val transmittance}.")
      colnames(input_file)[colnames(input_file) != "wavenumber"] <- "transmittance"
    } else {
      # must be intensity = absorbance
      cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[colnames(input_file) != 'wavenumber']}} is {.val absorbance}.")
      colnames(input_file)[colnames(input_file) != "wavenumber"] <- "absorbance"
    }
  }

  # add sample_id
  if (is.na(sample_name)) {
    sample_name <- tools::file_path_sans_ext(file)
  }
  input_file$sample_id <- sample_name

  return(input_file)
}

read_ftir_asp <- function(path, file, sample_name = NA, ...) {
  input_file <- readLines(con = file.path(path, file))
  data_rows <- as.numeric(input_file[1])
  max_wavenumber <- as.numeric(input_file[2])
  min_wavenumber <- as.numeric(input_file[3])

  if (is.na(sample_name)) {
    sample_name <- tools::file_path_sans_ext(file)
  }

  ftir_data <- data.frame(
    "wavenumber" = seq(from = min_wavenumber, to = max_wavenumber, by = (max_wavenumber - min_wavenumber + 1) / data_rows),
    "intensity" = as.numeric(input_file[length(input_file):7]),
    "sample_id" = sample_name
  )

  if (max(ftir_data$intensity) > 10) {
    # must be intensity = transmittance
    cli::cli_inform("{.fn PlotFTIR:::read_ftir_spc} has deduced that input data is in {.val transmittance} units.")
    colnames(ftir_data)[colnames(ftir_data) == "intensity"] <- "transmittance"
  } else {
    # must be intensity = absorbance
    cli::cli_inform("{.fn PlotFTIR:::read_ftir_spc} has deduced that input data is in {.val absorbance} units.")
    colnames(ftir_data)[colnames(ftir_data) == "intensity"] <- "absorbance"
  }

  return(ftir_data)
}

read_ftir_spc <- function(path, file, sample_name = NA, ...) {
  cli::cli_abort(c("Error in {.fn PlotFTIR:::read_ftir_spc}. PlotFTIR is not (yet) able to read .spc files.",
    i = "The {.pkg hyperSpec} package may be able to read this file."
  ))
}

read_ftir_a2r <- function(path, file, sample_name = NA, ...) {
  cli::cli_abort(c("Error in {.fn PlotFTIR:::read_ftir_a2r}. PlotFTIR is not (yet) able to read .a2r files.",
    i = "The {.pkg hyperSpec} package may be able to read this file."
  ))
}
