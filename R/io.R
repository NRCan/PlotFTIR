#' Read FTIR file
#'
#' @description Reads a provided file and returns a data.frame in the proper format for PlotFTIR functions.
#'
#' @param path
#' Path to the file. Default is the current working directory, as ".".
#'
#' Chemin d'accès au fichier. Par défaut, il s'agit du répertoire de travail actuel, sous la forme ".".
#' @param file
#' File name, required.
#'
#' Nom du fichier, obligatoire.
#' @param sample_name
#' Name for sample_id column in the returned data.frame. If not provided, the file name is used without the extension.
#'
#' Nom de la colonne sample_id dans le data.frame renvoyé. S'il n'est pas fourni, le nom du fichier est utilisé sans l'extension.
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
#' read_ftir(".", "ftir_sample_1.csv","sample1")
#' }
read_ftir <- function(path = ".", file, sample_name = NA){
  # Check inputs
  if(length(path) != 1) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg path} must be a single string value.")
  }
  if(length(file) != 1) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg file} must be a single string value.")
  }
  if(length(sample_name) != 1) {
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg sample_name} must be a single string value or single {.val NA}.")
  }
  if(!is.na(sample_name) & !is.character(sample_name)){
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. {.arg sample_name} must be a string value or {.val NA}.")
  }

  # check file exists
  if(!file.exists(file.path(path, file))){
    cli::cli_abort("Error in {.fn PlotFTIR::read_ftir}. File {.val file.path(path, file)} does not appear to exist.")
  }

  # Dispatch
  filetype <- tools::file_ext(file)

  if(filetype == "csv") {
    return(read_ftir_csv(path = path, file = file, sample_name = sample_name))
  } else if(filetype == "spc") {
    return(read_ftir_spc(path = path, file = file, sample_name = sample_name))
  } else if(filetype == "a2r") {
    return(read_ftir_a2r(path = path, file = file, sample_name = sample_name))
  } else if(filetype == "asp") {
    return(read_ftir_asp(path = path, file = file, sample_name = sample_name))
  }
}

read_ftir_csv <- function(path, file, sample_name = NA) {
  cli::cli_abort("{.fn PlotFTIR::read_ftir} is not (yet) able to read .csv files.")
}

read_ftir_spc <- function(path, file, sample_name = NA) {
  cli::cli_abort("{.fn PlotFTIR::read_ftir} is not (yet) able to read .spc files.")
}

read_ftir_a2r <- function(path, file, sample_name = NA) {
  cli::cli_abort("{.fn PlotFTIR::read_ftir} is not (yet) able to read .a2r files.")
}

read_ftir_asp <- function(path, file, sample_name = NA) {
  cli::cli_abort("{.fn PlotFTIR::read_ftir} is not (yet) able to read .asp files.")
}
