# Plot FTIR Spectra


#' Plot FTIR
#'
#' @description
#' Plot the FTIR spectra in a journal prepared format. This is a baseplot only
#'
#' @param ftir A data.frame in long format with columns `sample_id`, `wavenumber`, `absorbance`
#' The `absorbance` column may be replaced by a `transmittance` column for transmittance plots.
#' @param plot_title A title for a plot. Defaults to "FTIR Spectra"
#' @param legend_title A title for the legend. Defaults to "Sample ID"
#'
#' @return a ggplot2 object
#' @export
plot_ftir <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID"){
  if(!('sample_id' %in% colnames(ftir) & 'wavenumber' %in% colnames(ftir))){
    cli::cli_abort(c("{.arg ftir} is missing column(s).", i = "It must contain columns named both {.val sample_id} and {.val absorbance}."))
  }
  if('absorbance' %in% colnames(ftir) & 'transmittance' %in% colnames(ftir)){
    cli::cli_abort("{.arg ftir} cannot contain both {.val absorbance} and {.val transmittance} columns.")
  }
  if(any(!(colnames %in% c("sample_id", "wavenumber", "absorbance", "transmittance")))){
    cli::cli_abort("{.arg ftir} may only contain columns named {.val sample_id}, {.val wavenumber}, and one of {.val absorbance} or {.val transmittance}.")
  }

  mode = ifelse('absorbance' %in% colnames(ftir), "absorbance", "transmittance")

  ggplot2::ggplot(ftir) +
    ggplot2::geom_line(ggplot2::aes(x = wavenumber, y = absorbance, color = sample_id)) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(title = plot_title, x = bquote('Wavenumber'~(cm^-1)),
                  y = ifelse(mode == 'absorbance', 'Absorbance', 'Transmission')) +
    ggplot2::guides(color=ggplot2::guide_legend(title=legend_title))
  return(NULL)
}
