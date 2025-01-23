#' Get a summary of FTIR class objects
#'
#' @param x the ftir class object to summarize
#' @param ... additional parameters to print function.
#'
#' @returns invisible, the passed in object
#' @export
summary.ftir <- function(x, ...) {
  ftir <- check_ftir_data(x)

  samples <- unique(x$sample_id)
  nsamples<-length(samples)

  min_wavenumber <- min(x$wavenumber, na.rm = TRUE)
  max_wavenumber <- max(x$wavenumber, na.rm = TRUE)

  resolution <- get_resolution(x)

  units <- attr(x, "intensity")

  cat("FTIR data.frame containing", nsamples, "samples.\n")
  cat("Spectra range from", min_wavenumber, "to", max_wavenumber, "cm-1, with an average resolution of:", resolution, "cm-1.\n")
  cat("Sample intensity is in", units, "units.\n")
  cat("Sample IDs:\n ", paste0(samples, collapse = "\n  "))

  invisible(x)
}

get_resolution <- function(ftir){
  ftir <- check_ftir_data(ftir)
  avgdiff <- c()
  for(i in seq_along(unique(ftir$sample_id))){
    s<-unique(ftir$sample_id)[i]
    avgdiff <- c(avgdiff,  mean(diff(ftir[ftir$sample_id == s, "wavenumber"])))
  }
  return(mean(avgdiff))
}

add_ftir_class <- function(ftir){
  ftir <- check_ftir_data(ftir)
  if(!inherets(ftir, "ftir")){
    intensity <- attr(ftir, "intensity")
    ftir<-structure(ftir, class = c("ftir", class(ftir)))
    attr(ftir, "intensity") <- intensity
  }
  return(ftir)
}

#' Average together spectra
#'
#' @param x FTIR spectra data.frame of `class(x) == "ftir"`
#' @param ... Additional parameters to supply to the underlying `average_spectra()` function.
#'
#' @inherit average_spectra return description
#' @seealso [average_spectra()]
#' @export
mean.ftir <- function(x, ...){
  # This works with c(ftir, ftir) because c(ftir, ftir) already rbinds the ftir.
  average_spectra(x, ...)
}

#' Addition of FTIR objects
#'
#' @description Add a numeric (scalar) value to a FTIR spectra.
#'
#' @param x First item to add. Either `x` or `y` must be an object of `class() == "ftir"`, and `y` or `x` must be scalar
#' @param y Second item to add. Either `x` or `y` must be an object of `class() == "ftir"`, and `y` or `x` must be scalar
#'
#' @inherit add_scalar_value return description
#' @seealso [add_scalar_value()]
#' @export
`+.ftir` <- function(x, y) {
  if (is.ftir(x) & is.ftir(y)){
    cli::cli_abort("Error in {.fn `+.ftir`}. Can't add two spectra together yet.")
  } else if(is.ftir(x)){
    if(is.numeric(y)){
      return(add_scalar_value(x, y))
    } else {
      cli::cli_abort("Error in {.fn `+.ftir`}. Cannot add {.type {y}} to a ftir spectra.")
    }
  } else if (is.ftir(y)) {
    if(is.numeric(x)){
      return(add_scalar_value(y, x))
    } else {
      cli::cli_abort("Error in {.fn `+.ftir`}. Cannot add {.type {x}} to a ftir spectra.")
    }
  }
}

#' Subtraction of FTIR objects
#'
#' @description Subtract a numeric (scalar) value from a FTIR spectra. Note: always operates as FTIR - x regardless of the order of values provided.
#'
#' @param x First item to subtract. Either `x` or `y` must be an object of `class() == "ftir"`, and `y` or `x` must be scalar
#' @param y Second item to subtract. Either `x` or `y` must be an object of `class() == "ftir"`, and `y` or `x` must be scalar
#'
#' @inherit subtract_scalar_value return description
#' @seealso [subtract_scalar_value()]
#' @export
`-.ftir` <- function(x, y) {
  if (is.ftir(x) & is.ftir(y)){
    cli::cli_abort("Error in {.fn `-.ftir`}. Can't subtract two spectra yet.")
  } else if(is.ftir(x)) {
    if(is.numeric(y)){
      return(subtract_scalar_value(x, y))
    } else {
      cli::cli_abort("Error in {.fn `+.ftir`}. Cannot subtract {.type {y}} from a ftir spectra.")
    }
  } else if (is.ftir(y)){
    if(is.numeric(x)){
      return(subtract_scalar_value(y, x))
    } else {
      cli::cli_abort("Error in {.fn `+.ftir`}. Cannot subtract {.type {x}} from a ftir spectra.")
    }
  }
}


#' Is FTIR?
#'
#' @description Test if an object inherits class `ftir`
#' @param x object to test for type `ftir`
#'
#' @return True / False on if the item is `ftir`
#'
#' @export
is.ftir <- function(x) {
  if(inherits(x, "ftir")){
    check_ftir_data(x)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Vector FTIR
#'
#' Produces an unelementable ftir spectra object
#'
#' @param ... multiple objects to put in a single object
#'
#' @return a single ftir object
#'
#' @export
c.ftir <- function(...){
  args <- list(...)
  # Make sure all items are ftir
  if(all(sapply(args, is.ftir))){
    return(add_ftir_class(do.call(rbind, args)))
  }
  NextMethod()
}

