#' Read FTIR file
#'
#' @description
#' Reads a provided file and returns a data.frame in the proper format for PlotFTIR functions.
#'
#' Lit un fichier fourni et renvoie un data.frame dans le format approprié pour les fonctions PlotFTIR.
#'
#' @param path
#' Path to the file. Default is the current working directory, as `"."`. Can include the filename, in which case provide `NA` as the filename.
#'
#' Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de travail actuel, sous la forme `"."`. Peut inclure le nom du fichier, auquel cas il faut fournir `NA` comme nom de fichier.
#'
#' @param file
#' File name, required. If the file and path are provided together as `path`, then `NA` is accepted.
#'
#' Nom du fichier, obligatoire. Si le fichier et le chemin sont fournis ensemble en tant que `chemin`, alors `NA` est accepté.
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
#' @seealso [read_ftir_directory()]
read_ftir <- function(path = ".", file = NA, sample_name = NA, ...) {
  # Check inputs
  if (length(path) != 1 || !is.character(path)) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg path} must be a single string value.")
  }
  if (any(is.na(file), is.null(file)) & (tools::file_ext(path) %in% c("txt", "csv", "spc", "a2r", "asp"))) {
    file <- basename(path)
    path <- dirname(path)
  }
  if (length(file) != 1 || !is.character(file)) {
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
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. File {.val {file.path(path, file)}} does not appear to exist.")
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

#' Read FTIR file
#'
#' @description
#' Reads provided files and returns a data.frame in the proper format for PlotFTIR functions.
#'
#' Lit les fichiers fournis et renvoie un data.frame au format approprié pour les fonctions PlotFTIR.
#'
#' @param path
#' Path to the file. Default is the current working directory, as `"."`.
#'
#' Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de travail actuel, sous la forme `"."`.
#'
#' @param files
#' File names, required.
#'
#' Noms de fichiers, obligatoires.
#'
#' @param sample_names
#' Name for sample_id column in the returned data.frame. If not provided, the file names are used without the extension.
#'
#' Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas fourni, les noms de fichiers sont utilisés sans l'extension.
#'
#' @param ...
#' Additional parameters to pass to the file reading function. For CSV files, see [utils::read.csv()], it may be wise to pass `col.names` to disambiguate the input data.
#'
#' Paramètres supplémentaires à transmettre à la fonction de lecture de fichier. Pour les fichiers CSV, voir [utils::read.csv()], il peut être judicieux de passer `col.names` pour désambiguïser les données d'entrée.
#'
#' @return
#' a data.frame containing the spectral data from the files.
#'
#' un data.frame contenant les données spectrales des fichiers.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read .csv files from the working directory and call them `sample-1` and `sample-2`
#' read_ftir(".", c("ftir_sample_1.csv", "ftir_sample_2.csv"), c("sample-1", "sample-2"))
#' }
#' @md
#' @seealso [read_ftir()]
read_ftir_directory <- function(path, files, sample_names = NA, ...) {
  # Check inputs
  if (length(path) != 1 || !is.character(path)) {
    cli::cli_abort(c("Error in {.fn PlotFTIR::read_ftir_directory}. {.arg path} must be a single string value.",
      i = "{.fn PlotFTIR::read_ftir_directory} can only read multiple files from one directory."
    ))
  }

  if (!all(is.character(files))) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir_directory}. {.arg file} must be a vector of string values.")
  }

  if (!all(is.na(sample_names))) {
    if (length(sample_names) != length(files)) {
      cli::cli_abort(c("Error in {.fn PlotFTIR::read_ftir_directory}: If providing {.arg sample_names} the same number of names as the number of {.arg files} must be provided.",
        i = "You provided {length(sample_names)} {.arg sample_name{?s}} and {length(files)} {.arg file{?s}}"
      ))
    }
  } else {
    sample_names <- rep(NA, length(files))
  }

  ftir <- data.frame()
  for (i in seq_along(files)) {
    tryCatch(
      {
        f <- read_ftir(path, files[i], sample_names[i])
        ftir <- rbind(ftir, f)
      },
      error = function(e) cli::cli_warn(c("{e}", i = "{.fn PlotFTIR::read_ftir_directory} will try to continue with the next file."))
    )
  }
  if (nrow(ftir) > 0) {
    return(ftir)
  } else {
    cli::cli_abort(c("Error in {.fn PlotFTIR::read_ftir_directory}: No spectral data was read from files.",
      i = "Check input file list and directory."
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


#' Save FTIR Plot
#'
#' @description
#' Save FTIR plot object to file. Uses [ggplot2::ggsave()] to save to disk.
#'
#' Enregistrer l'objet de tracé IRTF dans un fichier. Utilise [ggplot2::ggsave()] pour enregistrer sur le disque.
#'
#' @param ftir_spectra_plot A plot generated by [plot_ftir()] or
#'   [plot_ftir_stacked()].
#'
#'   Un tracé généré par [plot_ftir()] ou [plot_ftir_stacked()].
#'
#' @param filename
#'  Name and directory of the file you wish to create. If it includes a extension the function will produce a file of that type. Options for filetypes include "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (on windows only).
#'
#'  Nom et répertoire du fichier que vous souhaitez créer. S'il contient une extension, la fonction produira un fichier de ce type. Les options pour les types de fichiers incluent "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" ou "wmf" (sur Windows uniquement).
#' @param ...
#'  Additional arguements to pass to [ggplot2::ggsave()].
#'
#'  Arguments supplémentaires à passer à [ggplot2::ggsave()].
#'
#' @return invisible `TRUE`
#' @export
#'
#' @examples
#' \dontrun{
#' save_plot(plot_ftir(biodiesel), filename = "biodiesel_plot.png")
#' }
save_plot <- function(ftir_spectra_plot, filename, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ggplot2} package installation.",
      i = "Install {.pkg ggplot2} with {.code install.packages('ggplot2')}"
    ))
  }

  if (!ggplot2::is.ggplot(ftir_spectra_plot)) {
    cli::cli_abort("Error in {.fn PlotFTIR::save_plt}. {.arg ftir_spectra_plot} must be a ggplot object. You provided {.obj_type_friendly {ftir_spectra_plot}}.")
  }

  ggplot2::ggsave(filename = filename, plot = ftir_spectra_plot, ...)
}
