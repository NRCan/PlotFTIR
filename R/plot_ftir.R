# Plot FTIR Spectra


#' Plot FTIR core plot generator
#'
#' @description Plot the FTIR spectra in a journal prepared format. This is a
#' simple plot only
#'
#' @param ftir A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, `absorbance` The `absorbance` column may be replaced by a
#'   `transmittance` column for transmittance plots.
#' @param plot_title A title for a plot. Defaults to "FTIR Spectra"
#' @param legend_title A title for the legend. Defaults to "Sample ID"
#'
#' @return a ggplot2 object
#' @export
plot_ftir_core <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID"){
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

  ftir <- ftir[complete.cases(ftir), ]
  ftir$wavenumber <- as.numeric(ftir$wavenumber)
  
  if(mode == "absorbance"){
    ftir$absorbance <- as.numeric(ftir$absorbance)
  } else {
    ftir$transmittance <- as.numeric(ftir$transmittance)
  }
  
  p <- ggplot2::ggplot(ftir) +
    ggplot2::geom_line(ggplot2::aes(x = wavenumber, y = absorbance, color = sample_id)) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(title = plot_title, x = bquote('Wavenumber'~(cm^-1)),
                  y = ifelse(mode == 'absorbance', 'Absorbance', 'Transmission')) +
    ggplot2::guides(color=ggplot2::guide_legend(title=legend_title))
  
  return(p)
}



#' Plot FTIR in stacked format
#'
#' @description Plot the FTIR spectra in a journal prepared format. It may be
#'   desirable to plot spectra 'stacked and offset' by a certain amount. In this
#'   case the Y axis becomes non-labelled and each charts baseline (0 for
#'   absorbance or 100 for transmittance) is offset by a certain amount.
#'
#'
#' @inheritParams plot_ftir_core
#' @param stack_offset the amount in percentage of stacking offset to use. For
#'   transmittance this is directly linked to the units of Y axis, for
#'   absorbance this is about 0.2 absorbance units
#'
#' @return a ggplot2 object
#' @export
plot_ftir_stacked <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID", stack_offset = 10){
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
  
  # Stack FTIR traces by 10% of range number of unique samples
  stack_samples <- unique(ftir$sample_id)
  nsamples <- length(unique(stack_samples))
  
  if(nsamples > 1){
    if(mode == "absorbance"){
      #Transmittance gets an offset of stack_offset % against a percentage scale
      #for absorbance, most signals max out around 2 so that's the range.
      stack_offset <- (stack_offset/100) * 2.0
    }
    offset <- data.frame("sample_id" = stack_samples,
                     "offset" = seq(from=0, by=stack_offset, length.out = nsamples))
    
    ftir <- dplyr::left_join(ftir, offset, by= sample_id)
    if (mode == "absorbance"){
      ftir$absorbance <- ftir$absorbance + ftir$offset
    } else {
      ftir$transmittance <- ftir$transmittance + ftir$offset
    }
    ftir$offset <- NULL
  }
  
  p<-plot_fitr_core(ftir = ftir, plot_title = plot_title, legend_title = legend_title)
  
  p <- p + ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                          axis.ticks.y=ggplot2::element_blank()) 

  return(p)
}