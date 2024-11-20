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
#' # Writing a temporary file to read later
#' tf <- tempfile(fileext = ".csv")
#' write.csv(sample_spectra[sample_spectra$sample_id == "paper", c("wavenumber", "absorbance")],
#'   file = tf, row.names = FALSE
#' )
#'
#' # Read the .csv file and call the sample `sample1`
#' read_ftir(tf, sample_name = "sample1")
#' @md
#' @seealso [read_ftir_directory()]
read_ftir <- function(path = ".", file = NA, sample_name = NA, ...) {
  # Check inputs
  if (length(path) != 1 || !is.character(path)) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg path} must be a single string value.")
  }
  if (any(is.na(file), is.null(file)) && (tools::file_ext(path) %in% c("txt", "csv", "spc", "a2r", "asp"))) {
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
#' # Putting some files in a temp dir to read back into PlotFTIR:
#' td <- tempdir()
#' write.csv(sample_spectra[sample_spectra$sample_id == "paper", c("wavenumber", "absorbance")],
#'   file = file.path(td, "ftir_sample_1.csv"), row.names = FALSE
#' )
#' write.csv(sample_spectra[sample_spectra$sample_id == "toluene", c("wavenumber", "absorbance")],
#'   file = file.path(td, "ftir_sample_2.csv"), row.names = FALSE
#' )
#'
#' # Read .csv files from the temp directory and call them `sample-1` and `sample-2`
#' read_ftir_directory(td, c("ftir_sample_1.csv", "ftir_sample_2.csv"), c("sample-1", "sample-2"))
#'
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
  intensity <- NA
  for (i in seq_along(files)) {
    tryCatch(
      {
        f <- read_ftir(path, files[i], sample_names[i])
        if (is.na(intensity)) {
          intensity <- attr(f, "intensity")
        }
        if (attr(f, "intensity") == intensity) {
          ftir <- rbind(ftir, f)
        } else {
          if (intensity <- "absorbance") {
            ftir <- rbind(ftir, transmittance_to_absorbance(f))
          } else {
            ftir <- rbind(ftir, absorbance_to_transmittance(f))
          }
        }
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
    if (intensity_type(input_file) == "transmittance") {
      cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[colnames(input_file) != 'wavenumber']}} is {.val transmittance}.")
      colnames(input_file)[colnames(input_file) != "wavenumber"] <- "transmittance"
      attr(input_file, "intensity") <- "transmittance"
    } else {
      # must be intensity = absorbance
      cli::cli_inform("{.fn PlotFTIR:::read_ftir_csv} has deduced that input data column {.arg {colnames(input_file)[colnames(input_file) != 'wavenumber']}} is {.val absorbance}.")
      colnames(input_file)[colnames(input_file) != "wavenumber"] <- "absorbance"
      attr(input_file, "intensity") <- "absorbance"
    }
  } else {
    if ("absorbance" %in% colnames(input_file)) {
      attr(input_file, "intensity") <- "absorbance"
    } else {
      attr(input_file, "intensity") <- "transmittance"
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

  if (intensity_type(ftir_data) == "transmittance") {
    # must be intensity = transmittance
    cli::cli_inform("{.fn PlotFTIR:::read_ftir_spc} has deduced that input data is in {.val transmittance} units.")
    colnames(ftir_data)[colnames(ftir_data) == "intensity"] <- "transmittance"
    attr(input_file, "intensity") <- "transmittance"
  } else {
    # must be intensity = absorbance
    cli::cli_inform("{.fn PlotFTIR:::read_ftir_spc} has deduced that input data is in {.val absorbance} units.")
    colnames(ftir_data)[colnames(ftir_data) == "intensity"] <- "absorbance"
    attr(input_file, "intensity") <- "absorbance"
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
#' Save FTIR plot object to file. Uses [ggplot2::ggsave()] to save to disk. Specify a filename ending with `.svg` for vector graphics, if requeste by a journal.
#'
#' Enregistrer l'objet de tracé IRTF dans un fichier. Utilise [ggplot2::ggsave()] pour enregistrer sur le disque. Spécifier un nom de fichier se terminant par `.svg` pour les graphiques vectoriels, si un journal le demande.
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
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   td <- tempdir()
#'   save_plot(plot_ftir(biodiesel), filename = file.path(td, "biodiesel_plot.png"))
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


#' Convert `ir` to `PlotFTIR` data format
#'
#' @description
#' convert data from the `ir` package to a structure that will work with `PlotFTIR`.
#'
#' convertir les données du paquet `ir` en une structure qui fonctionnera avec `PlotFTIR`.
#'
#' @param ir_data data of class `ir` from `ir` package
#'
#' données de la classe `ir` du paquet `ir`.
#' @param what which samples to convert to `PlotFTIR` format. Defaults to all available spectra.
#'
#' les échantillons à convertir au format `PlotFTIR`. Par défaut, tous les spectres disponibles
#'
#' @return
#' a data.frame compatible with `PlotFTIR` functions
#'
#' un data.frame compatible avec les fonctions `PlotFTIR`.
#'
#' @export
#'
#' @seealso [ir::ir_get_spectrum()] for information on how ir passes out data.
#'
#' @examples
#' if (requireNamespace("ir", quietly = TRUE)) {
#'   # Convert samples 1 & 4 to PlotFTIR format
#'   ir_to_plotftir(ir::ir_sample_data, c(1, 4))
#' }
#'
ir_to_plotftir <- function(ir_data, what = NA) {
  # Package Checks
  if (!requireNamespace("ir", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ir} package installation for this function.",
      i = "Install {.pkg ir} with {.code install.packages('ir')}"
    ))
  }

  # Param checks

  if (!("ir" %in% class(ir_data))) {
    cli::cli_abort("Error in {.fn PlotFTIR::ir_to_plotftir}. {.arg ir_data} must be of class {.cls ir}, produced by the {.pkg ir} package. You provided {.obj_type_friendly {ir_data}}.")
  }

  if (all(is.na(what))) {
    what <- seq_along(ir_data$spectra)
  }

  if (suppressWarnings(any(is.na(as.numeric(what))))) {
    if (all(what %in% ir_data$id_sample)) {
      what <- which(what %in% ir_data$id_sample)
    } else {
      cli::cli_abort("Error in {.fn PlotFTIR::ir_to_plotftir}. {.arg what} must contain the row numbers of sample spectra to extract, or exact names matching what is in {.code ir_data$id_sample}.")
    }
  }

  if (all(is.numeric(what))) {
    if (max(what, na.rm = TRUE) > nrow(ir_data) || min(what) < 1) {
      cli::cli_abort("Error in {.fn PlotFTIR::ir_to_plotftir}. {.arg what} must contain the row numbers of sample spectra to extract, or exact names matching what is in {.code ir_data$id_sample}.")
    }
  }

  # Call function
  return(ir_to_df(ir = ir_data, what = what))
}

ir_to_df <- function(ir, what) {
  # Internal function for ir_to_plotftir()
  if (!requireNamespace("ir", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ir} package installation for this function.",
      i = "Install {.pkg ir} with {.code install.packages('ir')}"
    ))
  }

  # Param checks
  if (!("ir" %in% class(ir))) {
    cli::cli_abort("Error in {.fn PlotFTIR::ir_to_df}. {.arg ir} must be of class {.cls ir}, produced by the {.pkg ir} package. You provided {.obj_type_friendly {ir}}.")
  }

  irdata <- ir::ir_get_spectrum(ir, what = what)
  irdata <- mapply(cbind, irdata, "sample_id" = names(irdata), SIMPLIFY = FALSE)
  irdata <- do.call(rbind, irdata)
  colnames(irdata)[colnames(irdata) == "x"] <- "wavenumber"

  intensity <- NA
  ftir <- data.frame()
  for (s in seq_along(unique(irdata$sample_id))) {
    id <- unique(irdata$sample_id)[s]
    sampleir <- irdata[irdata$sample_id == id, ]
    intensity <- intensity_type(sampleir)

    sample_intensity <- intensity
    colnames(sampleir)[colnames(sampleir) == "y"] <- intensity
    attr(sampleir, "intensity") <- intensity

    if (intensity == sample_intensity) {
      ftir <- rbind(ftir, sampleir)
    } else {
      if (intensity == "absorbance") {
        ftir <- rbind(ftir, transmittance_to_absorbance(sampleir))
      } else {
        ftir <- rbind(ftir, absorbance_to_transmittance(sampleir))
      }
    }
  }

  return(ftir)
}


#' Convert `PlotFTIR` data to `ir`
#'
#' @description
#' Converts `PlotFTIR` data to that ready to use by the `ir` package.
#'
#' Convertit les données `PlotFTIR` en données prêtes à être utilisées par le paquet `ir`.
#'
#' @param ftir
#'   A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, and `absorbance`. The `absorbance` column may be replaced by
#'   a `transmittance` column for transmittance plots. The code determines the
#'   correct y axis units and labels the plot/adjusts the margins appropriately.
#'
#'   Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
#'   et `absorbance`. La colonne `absorbance` peut être remplacée par une
#'   colonne `transmittance` pour les tracés de transmission. Le code détermine
#'   les unités correctes de l'axe y et étiquette le tracé/ajuste les marges de
#'   manière appropriée.
#'
#' @param metadata
#'   Additional data to pass to `ir` to include as metadata. Should be structured
#'   as a data.frame.
#'
#'   Données supplémentaires à transmettre à `ir` pour les inclure dans les métadonnées.
#'   Doit être structuré comme un data.frame.
#'
#' @seealso [ir::ir_new_ir()] for information on how ir takes in data.
#'
#' @return
#' an `ir` classed data.frame structured for use in that package.
#'
#' un data.frame de classe `ir` structuré pour être utilisé dans ce paquet.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("ir", quietly = TRUE)) {
#'   # convert biodiesel to a `ir` object
#'   plotftir_to_ir(biodiesel,
#'     metadata = data.frame("Biodiesel_Content" = c(0, 0.25, 0.5, 1, 2.5, 5, 7.5, 10, 0.5, 5, NA))
#'   )
#' }
plotftir_to_ir <- function(ftir, metadata = NA) {
  # Package checks
  if (!requireNamespace("ir", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ir} package installation for this function.",
      i = "Install {.pkg ir} with {.code install.packages('ir')}"
    ))
  }

  # Param Checks
  ftir <- check_ftir_data(ftir)
  if (!all(is.na(metadata))) {
    if (!is.data.frame(metadata)) {
      cli::cli_abort("Error in {.fn PlotFTIR::plotftir_to_ir}. {.arg metadata} must be either {.code NA} or a {.cls data.frame}.")
    }
  }

  samples <- unique(ftir$sample_id)
  colnames(ftir)[colnames(ftir) == "wavenumber"] <- "x"
  colnames(ftir)[colnames(ftir) %in% c("transmittance", "absorbance", "intensity")] <- "y"
  ftir_ir <- lapply(samples, FUN = function(x) ftir[ftir$sample_id == x, c("x", "y"), ])
  names(ftir_ir) <- samples
  if (all(is.na(metadata)) || !is.data.frame(metadata)) {
    metadata <- data.frame("id_sample" = samples)
  } else {
    if (!("id_sample" %in% colnames(metadata))) {
      metadata$id_sample <- samples
    }
  }
  irdata <- ir::ir_new_ir(spectra = ftir_ir, metadata = metadata)

  return(irdata)
}


#' Convert `PlotFTIR` data to `ChemoSpec` format
#'
#' @description
#' Converts `PlotFTIR` data to that ready to use by the `ChemoSpec` package.
#'
#' Convertit les données `PlotFTIR` en données prêtes à être utilisées par le paquet `ChemoSpec`.
#'
#' @param ftir
#'   A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, and `absorbance`. The `absorbance` column may be replaced by
#'   a `transmittance` column for transmittance plots. The code determines the
#'   correct y axis units and labels the plot/adjusts the margins appropriately.
#'
#'   Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
#'   et `absorbance`. La colonne `absorbance` peut être remplacée par une
#'   colonne `transmittance` pour les tracés de transmission. Le code détermine
#'   les unités correctes de l'axe y et étiquette le tracé/ajuste les marges de
#'   manière appropriée.
#'
#' @param group_crit
#' A vector of character strings. Corresponds to [ChemoSpec::files2SpectraObject()] `gr.crit` parameter.
#'
#' Un vecteur de chaînes de caractères. Correspond au paramètre `gr.crit` de [ChemoSpec::files2SpectraObject()].
#'
#' @param group_colours
#' Group colours. Corresponds to [ChemoSpec::files2SpectraObject()] `gr.cols` parameter.
#'
#' Couleurs du groupe. Correspond au paramètre `gr.cols` de [ChemoSpec::files2SpectraObject()].
#'
#' @param description
#' A description of the experiment. Corresponds to [ChemoSpec::files2SpectraObject()] `descrip` parameter.
#'
#' Description de l'expérience. Correspond au paramètre `descrip` de [ChemoSpec::files2SpectraObject()].
#'
#' @return
#' A `ChemoSpec` data object
#'
#' Un objet de données `ChemoSpec`
#' @export
#'
#' @seealso
#' [ChemoSpec::files2SpectraObject()] for import requirements, and [chemospec_to_plotftir()] for converting to `PlotFTIR` format.
#'
#' [ChemoSpec::files2SpectraObject()] pour les conditions d'importation, et [chemospec_to_plotftir()] pour la conversion au format `PlotFTIR`.
#'
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE) && interactive()) {
#'   # convert biodiesel to a `chemospec` object
#'   plotftir_to_chemospec(biodiesel)
#' }
plotftir_to_chemospec <- function(ftir, group_crit = NA, group_colours = "auto", description = "FTIR Study") {
  # Package checks
  if (!requireNamespace("R.utils", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} and {.pkg ChemoSpec} requires {.pkg R.utils} package installation for this function.",
      i = "Install {.pkg R.utils} with {.code install.packages('R.utils')}"
    ))
  }

  if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ChemoSpec} package installation for this function.",
      i = "Install {.pkg ChemoSpec} with {.code install.packages('ChemoSpec')}"
    ))
  }

  # Param Checks
  ftir <- check_ftir_data(ftir)

  if (nchar(description) > 40) {
    cli::cli_alert_warning("{.pkg ChemoSpec} advises that {.param description} is 40 characters or less. Your description is {nchar(description)} characters.")
  }

  if (length(group_colours) == 1) {
    if (!group_colours %in% c("auto", "Col7", "Col8", "Col12")) {
      cli::cli_abort("Error in {.fn PlotFTIR::plotftir_to_chemospec}. {.arg group_colours} must be one of {.code 'auto'}, {.code 'Col7'}, {.code 'Col8'}, {.code 'Col12'}, or a vector of the same length as {.param group_crit}.")
    }
  } else if (length(group_colours) != length(group_crit)) {
    cli::cli_abort("Error in {.fn PlotFTIR::plotftir_to_chemospec}. {.arg group_colours} must be one of {.code 'auto'}, {.code 'Col7'}, {.code 'Col8'}, {.code 'Col12'}, or a vector of the same length as {.param group_crit}.")
  }

  if (all(is.na(group_crit))) {
    group_crit <- unique(ftir$sample_id)
  }

  if (length(group_crit) > 8 && length(group_crit) <= 12 && length(group_colours) == 1) {
    cli::cli_alert_warning("Setting group_colours to {.code 'Col12'} to ensure enough colours available for groups.")
    group_colours <- "Col12"
  }

  if (length(group_crit) > 12) {
    cli::cli_abort("Error in {.fn PlotFTIR::plotftir_to_chemospec}. {.arg group_crit} has to make 12 or less groups for {.pkg ChemoSpec} to be happy.")
  }

  intensity <- ifelse("absorbance" %in% colnames(ftir), "absorbance", "transmittance")
  currentwd <- getwd()
  on.exit(setwd(currentwd))
  dir <- tempdir()
  setwd(dir)

  for (i in seq_along(unique(ftir$sample_id))) {
    sid <- unique(ftir$sample_id)[i]
    utils::write.csv(ftir[ftir$sample_id == sid, c("wavenumber", intensity)], file = paste0("./", sid, ".csv"), row.names = FALSE)
  }
  cs_ftir <- ChemoSpec::files2SpectraObject(gr.crit = group_crit, gr.cols = group_colours, freq.unit = "wavenumber", int.unit = intensity, fileExt = ".csv", descrip = description, header = TRUE, sep = ",", dec = ".")

  setwd(currentwd)

  return(cs_ftir)
}


#' `ChemoSec` to `PlotFTIR` conversions
#'
#' @description
#' Converts `ChemoSpec` data to that ready to use by `PlotFTIR`.
#'
#' Convertit les données `ChemoSpec` en données prêtes à être utilisées par `PlotFTIR`.
#'
#' @param csdata
#' `ChemoSpec` data to convert to `PlotFTIR.`
#' Données `ChemoSpec` a convertir à `PlotFTIR`.
#'
#' @return
#' a data.frame compatible with `PlotFTIR` functions
#'
#' un data.frame compatible avec les fonctions `PlotFTIR`.
#'
#' @export
#'
#' @seealso
#' [ChemoSpec::files2SpectraObject()] for import requirements, and [chemospec_to_plotftir()] for converting to `PlotFTIR` format.
#'
#' [ChemoSpec::files2SpectraObject()] pour les conditions d'importation, et [chemospec_to_plotftir()] pour la conversion au format `PlotFTIR`.
#'
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   # convert `chemospec` to PlotFTIR data
#'   data("SrE.IR", package = "ChemoSpec", envir = environment())
#'   chemospec_to_plotftir(SrE.IR)
#' }
chemospec_to_plotftir <- function(csdata) {
  # Package checks
  if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
    cli::cli_abort(c("{.pkg PlotFTIR} requires {.pkg ChemoSpec} package installation for this function.",
      i = "Install {.pkg ChemoSpec} with {.code install.packages('ChemoSpec')}"
    ))
  }

  # Param Checks
  if (!("Spectra" %in% class(csdata))) {
    cli::cli_abort("Error in {.fn PlotFTIR::chemospec_to_plotftir}. {.arg csdata} must be of class {.cls Spectra}, produced by the {.pkg ChemoSpec} package. You provided {.obj_type_friendly {csdata}}.")
  }
  if (!("wavenumber" %in% csdata$unit)) {
    cli::cli_abort("Error in {.fn PlotFTIR::chemospec_to_plotftir}. {.arg csdata} must be of IR spectra, this data appears to be from another instrument.")
  }

  ftir <- data.frame()
  allunits <- NA
  for (i in seq_along(csdata$names)) {
    df <- data.frame(
      "wavenumber" = csdata$freq,
      "intensity" = csdata$data[i, ],
      "sample_id" = csdata$names[i]
    )
    sample_units <- intensity_type(df)
    colnames(df)[colnames(df) == "intensity"] <- sample_units
    attr(df, "intensity") <- sample_units
    if (is.na(allunits)) {
      all_units <- sample_units
    }
    if (all_units != sample_units) {
      if (all_units == "absorbance") {
        df <- transmittance_to_absorbance(df)
      } else {
        df <- absorbance_to_transmittance(df)
      }
    }
    ftir <- rbind(ftir, df)
  }

  return(ftir)
}
