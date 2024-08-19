# Plot FTIR Spectra


#' PlotFTIR core plot generator
#'
#' @description Plot the FTIR spectra in a journal prepared format. This is the
#'   core plot code, please call [plot_ftir()] for basic (overlaid) plots and
#'   [plot_ftir_stacked()] for stacked and offset plots.
#'
#'   Tracez les spectres IRTF dans un format préparé par un journal. C'est le
#'   code de tracé principal, veuillez appeler [plot_ftir()] pour les tracés de
#'   base (superposés) et [plot_ftir_stacked()] pour les tracés empilés et
#'   décalés.
#'
#' @param ftir A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, and `absorbance`. The `absorbance` column may be replaced by
#'   a `transmittance` column for transmittance plots. The code determines the
#'   correct y axis units and labels the plot/adjusts the margins appropriately.
#'
#'   Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
#'   et `absorbance`. La colonne `absorbance` peut être remplacée par une
#'   colonne `transmittance` pour les tracés de transmission. Le code détermine
#'   les unités correctes de l'axe Y et étiquette le tracé/ajuste les marges de
#'   manière appropriée.
#'
#' @param plot_title A title for a plot. Defaults to "FTIR Spectra". If a vector
#'   length two, the second element will be used for a subtitle.
#'
#'   Un titre pour une trace. La valeur par défaut est «FTIR Spectra». Si un
#'   vecteur mesure deux, le deuxième élément sera utilisé pour un sous-titre.
#'
#' @param legend_title A title for the legend. Defaults to "Sample ID".
#'
#'   Un titre pour la légende. La valeur par défaut est «Sample ID».
#'
#' @keywords internal
#'
#' @return a ggplot object containing a  FTIR spectral plot. The plot and legend
#'   titles are as provided, with each sample provided a different default color
#'   from ggplot2. Because this is a ggplot object, any other ggplot modifiers,
#'   layers, or changes can be applied to the returned object. Further
#'   manipulations can be performed by this package.
#'
#'   un objet ggplot contenant un tracé spectral IRTF Les titres de le tracé et
#'   de la légende sont tels que fournis, chaque échantillon étant doté d'une
#'   couleur par défaut différente de celle de ggplot2. Puisqu'il s'agit d'un
#'   objet ggplot, tous les autres modificateurs, calques ou modifications
#'   ggplot peuvent être appliqués à l'objet renvoyé. D'autres manipulations
#'   peuvent être effectuées par ce package.
#'
#' @seealso [zoom_in_on_range()] to 'zoom' into a specified range,
#'   [compress_low_energy()] to make the x axis non-linear (compressing lower
#'   energy regions), and [add_wavenumber_marker()] to add markers to highlight
#'   important wavenumbers.
#'
#'   [zoom_in_on_range()] pour 'zoomer' sur une zone spécifiée,
#'   [compress_low_energy()] pour rendre l'axe x non linéaire (compression des
#'   régions à basse énergie) et [add_wavenumber_marker()] pour ajouter des
#'   marqueurs pour mettre en évidence les nombres d'onde importants.
#'
plot_ftir_core <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID") {
  # Package Checks
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("{.fun plot_ftir} requires {.pkg ggplot2} package installation.")
  }
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    cli::cli_abort("{.fun plot_ftir} requires {.pkg ggthemes} package installation.")
  }

  if (!(is.data.frame(ftir))) {
    cli::cli_abort("{.arg ftir} must be a data frame. You provided {.obj_type_friendly ftir}.")
  }
  if (!("sample_id" %in% colnames(ftir) && "wavenumber" %in% colnames(ftir))) {
    cli::cli_abort(c("{.arg ftir} is missing column(s).",
      i = "It must contain columns named both {.var sample_id} and {.var wavenumber}."
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
  if (!is.character(plot_title) || length(plot_title) > 2) {
    cli::cli_abort("{.arg plot_title} must be a character string or vector of strings with length not more than two.")
  }
  if (!is.character(legend_title) || length(legend_title) > 1) {
    cli::cli_abort("{.arg legend_title} must be a single character string.")
  }
  if (length(unique(ftir$sample_id)) > 12) {
    cli::cli_abort(c("The color palette in use works only with 12 or fewer unique samples in {.arg ftir}.",
      i = "You have a total of {length(unique(ftir$sample_id))} unique sample IDs."
    ))
  }

  mode <- ifelse("absorbance" %in% colnames(ftir), "absorbance", "transmittance")

  ftir <- ftir[stats::complete.cases(ftir), ]
  ftir$wavenumber <- as.numeric(ftir$wavenumber)

  if (mode == "absorbance") {
    ftir$absorbance <- as.numeric(ftir$absorbance)
    p <- ggplot2::ggplot(ftir) +
      ggplot2::geom_line(ggplot2::aes(x = .data$wavenumber, y = .data$absorbance, color = as.factor(.data$sample_id))) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_width(0.2))
  } else {
    ftir$transmittance <- as.numeric(ftir$transmittance)
    p <- ggplot2::ggplot(ftir) +
      ggplot2::geom_line(ggplot2::aes(x = .data$wavenumber, y = .data$transmittance, color = as.factor(.data$sample_id))) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_width(20))
  }

  p <- p +
    ggplot2::scale_x_reverse(minor_breaks = scales::breaks_width(-200)) +
    ggthemes::scale_color_calc() +
    ggplot2::labs(
      title = plot_title[1],
      subtitle = if (length(plot_title) < 2) NULL else plot_title[2], # Can't return Null from ifelse()
      x = bquote("Wavenumber" ~ (cm^-1)),
      y = ifelse(mode == "absorbance", "Absorbance", "% Transmittance")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = legend_title), x = ggplot2::guide_axis(minor.ticks = TRUE)) +
    ggplot2::theme_light()

  return(p)
}


#' Plot FTIR in stacked format
#'
#' @description Plot the FTIR spectra in a journal prepared format. It may be
#'  desirable to plot spectra 'stacked and offset' by a certain amount. In this
#'  case the Y axis becomes non-labelled and each charts baseline (0 for
#'  absorbance or 100 for transmittance) is offset by a certain amount.
#'
#'  Tracez les spectres IRTF dans un format préparé par un journal. C'est
#'  possible souhaitable pour tracer les spectres 'empilés et décalés' d'une
#'  certaine quantité. Dans ce cas l'axe Y devient non étiqueté et
#'  chaque ligne de base du graphique (0 pour absorbance ou 100 pour la
#'  transmission) est compensée d'une certaine quantité.
#'
#' @inheritParams plot_ftir_core
#' @param stack_offset The amount in percentage of stacking offset to use. For
#'  transmittance this is directly linked to the units of Y axis, for absorbance
#'  this is about 0.2 absorbance units.
#'
#'  Le montant en pourcentage de décalage d'empilement à utiliser. Pour
#'  transmittance ceci est directement lié aux unités de l'axe Y, pour
#'  l'absorbance cela représente environ 0,2 unités d'absorbance.
#'
#' @inherit plot_ftir_core return
#'
#' @inherit plot_ftir_core seealso
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Plot FTIR spectras stacked showing the differences in the `biodiesel` dataset
#'   plot_ftir_stacked(biodiesel)
#' }
plot_ftir_stacked <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID", stack_offset = 10) {
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
  if (!is.numeric(stack_offset) || length(stack_offset) > 1) {
    cli::cli_abort("{.arg stack_offset} must be a single numeric value.")
  }
  if (stack_offset < 0 || stack_offset > 200) {
    cli::cli_abort("{.arg stack_offset} must be between 0 and 200.")
  }

  mode <- ifelse("absorbance" %in% colnames(ftir), "absorbance", "transmittance")

  # Stack FTIR traces by 10% of range number of unique samples
  stack_samples <- unique(ftir$sample_id)
  nsamples <- length(unique(stack_samples))

  if (nsamples > 1) {
    if (mode == "absorbance") {
      # Transmittance gets an offset of stack_offset % against a percentage scale
      # for absorbance, most signals max out around 2 so that's the range.
      stack_offset <- (stack_offset / 100) * 2.0
    }
    offset <- data.frame(
      "sample_id" = stack_samples,
      "offset" = seq(from = 0, by = stack_offset, length.out = nsamples)
    )

    ftir <- merge(x = ftir, y = offset, by = "sample_id")
    if (mode == "absorbance") {
      ftir$absorbance <- ftir$absorbance + ftir$offset
    } else {
      ftir$transmittance <- ftir$transmittance + ftir$offset
    }
    ftir$offset <- NULL
  }

  p <- plot_ftir_core(ftir = ftir, plot_title = plot_title, legend_title = legend_title)

  p <- p + ggplot2::theme(
    axis.text.y = ggplot2::element_blank()
  )

  return(p)
}


#' Plot FTIR Spectra Overlaid
#'
#' @description Produce a basic spectra overlay plot for all samples found in
#' the FTIR dataset provided.
#'
#' Produisez un tracé de superposition de spectres de base pour tous les
#' échantillons trouvés dans l'ensemble de données IRTF fourni.
#'
#' @inherit plot_ftir_core params return
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Plot a basic FTIR Spectra overlay from the `sample_spectra` data set with default titles
#'   plot_ftir(sample_spectra)
#' }
plot_ftir <- function(ftir, plot_title = "FTIR Spectra", legend_title = "Sample ID") {
  p <- plot_ftir_core(ftir = ftir, plot_title = plot_title, legend_title = legend_title)

  return(p)
}
