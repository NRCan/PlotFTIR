# Plot Raman Spectra

#' Plot Raman core plot generator
#'
#' @description Plot the Raman spectra in a journal prepared format. Call
#'   [plot_raman()] for basic (overlaid) plots and [plot_raman_stacked()] for
#'   stacked and offset plots.
#'
#'   Tracez les spectres Raman dans un format préparé par un journal.Appelez
#'   [plot_raman()] pour les tracés de base (superposés) et [plot_raman_stacked()]
#'   pour les tracés empilés et décalés.
#'
#' @inheritParams .shared-params
#'
#' @param ftir A data.frame in long format with columns `sample_id`,
#'   `wavenumber`, and `intensity`. The `intensity` column contains raw Raman
#'   counts. The code determines the correct y axis units and labels the plot
#'   appropriately based on whether normalization has been applied.
#'
#'   Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
#'   et `intensity`. La colonne `intensity` contient des compteurs Raman bruts.
#'   Le code détermine les unités correctes de l'axe y et étiquette le tracé
#'   en conséquence selon que la normalisation a été appliquée ou non.
#'
#' @keywords internal
#'
#' @return a ggplot object containing a Raman spectral plot. The plot and legend
#'   titles are as provided, with each sample provided a different default
#'   color. Because this is a ggplot object, any other ggplot modifiers, layers,
#'   or changes can be applied to the returned object.
#'
#'   un objet ggplot contenant un tracé spectral Raman. Les titres de le tracé et
#'   de la légende sont tels que fournis, avec une couleur par défaut différente
#'   pour chaque échantillon. Puisqu'il s'agit d'un objet ggplot, tous les
#'   autres modificateurs, calques ou changements ggplot peuvent être appliqués
#'   à l'objet retourné.
#'
plot_raman_core <- function(
  ftir,
  plot_title = "Raman Spectra",
  legend_title = "Sample ID",
  lang = NA
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .pkg_abort(
      list(
        en = c(
          "{.pkg PlotFTIR} requires {.pkg ggplot2} package installation.",
          i = "Install {.pkg ggplot2} with {.run install.packages('ggplot2')}"
        ),
        fr = c(
          "{.pkg PlotFTIR} n\u00e9cessite l'installation du paquet {.pkg ggplot2}.",
          i = "Installez {.pkg ggplot2} avec {.run install.packages('ggplot2')}"
        )
      ),
      call = rlang::caller_env()
    )
  }

  ftir <- check_ftir_data(ftir)

  if (attr(ftir, "intensity") == "intensity") {
    .pkg_abort(
      list(
        en = c(
          "Error in {.fn PlotFTIR:::plot_raman_core}. {.arg ftir} intensity attribute not set.",
          i = "Expected 'raman' or 'normalized raman'."
        ),
        fr = c(
          "Erreur dans {.fn PlotFTIR:::plot_raman_core}. L'attribut {.arg ftir} d'intensit\u00e9 n'est pas d\u00e9fini.",
          i = "Attendu 'raman' ou 'normalized raman'."
        )
      ),
      call = rlang::caller_env()
    )
  }

  if (!is.character(plot_title) || length(plot_title) > 2) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR:::plot_raman_core}. {.arg plot_title} must be a character string or vector of strings with length not more than two.",
        fr = "Erreur dans {.fn PlotFTIR:::plot_raman_core}. {.arg plot_title} doit \u00eatre une cha\u00eene de caract\u00e8res ou un vecteur de cha\u00eenes de caract\u00e8res avec une longueur maximale de deux."
      ),
      call = rlang::caller_env()
    )
  }
  if (!is.character(legend_title) || length(legend_title) > 1) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR:::plot_raman_core}. {.arg legend_title} must be a single character string.",
        fr = "Erreur dans {.fn PlotFTIR:::plot_raman_core}. {.arg legend_title} doit \u00eatre une unique cha\u00eene de caract\u00e8res."
      ),
      call = rlang::caller_env()
    )
  }
  if (length(unique(ftir$sample_id)) > 12) {
    .pkg_warn(
      list(
        en = c(
          "Warning in {.fn PlotFTIR:::plot_raman_core}. The color palette in use works best with 12 or fewer unique samples in {.arg ftir}.",
          i = "You have a total of {length(unique(ftir$sample_id))} unique sample IDs."
        ),
        fr = c(
          "Avertissement dans {.fn PlotFTIR:::plot_raman_core}. La palette de couleurs utilis\u00e9e fonctionne mieux avec 12 \u00e9chantillons uniques ou moins dans {.arg ftir}.",
          i = "Vous avez un total de {length(unique(ftir$sample_id))} identifiants d'\u00e9chantillon uniques."
        )
      ),
      call = rlang::caller_env()
    )
  }

  if (!is.na(lang)) {
    lang <- rlang::arg_match(
      lang,
      values = c(
        "en",
        "english",
        "anglais",
        "fr",
        "french",
        "francais",
        "fran\u00e7ais"
      ),
      multiple = FALSE
    )
  } else {
    lang <- getOption("PlotFTIR.lang", default = "en")
  }

  l <- substr(lang, 0, 2)
  if (l == "fr") {
    if (all(plot_title == "Raman Spectra")) {
      plot_title <- "Spectres Raman"
    }
    if (legend_title == "Sample ID") {
      legend_title <- "ID de l'\u00e9chantillon"
    }
  }

  mode <- attr(ftir, "intensity")

  if (l == "fr") {
    xtitle <- bquote("D\u00e9calage Raman" ~ (cm^-1))
  } else {
    xtitle <- bquote("Raman shift" ~ (cm^-1))
  }

  ytitle <- ifelse(
    grepl("normalized", mode),
    "Normalized Intensity",
    "Intensity"
  )

  ftir <- ftir[stats::complete.cases(ftir), ]
  ftir$wavenumber <- as.numeric(ftir$wavenumber)
  ftir$intensity <- as.numeric(ftir$intensity)

  p <- ggplot2::ggplot(ftir) +
    ggplot2::geom_line(ggplot2::aes(
      x = .data$wavenumber,
      y = .data$intensity,
      color = as.factor(.data$sample_id)
    )) +
    ggplot2::scale_y_continuous()

  p <- p +
    ggplot2::labs(
      title = plot_title[1],
      subtitle = if (length(plot_title) < 2) NULL else plot_title[2],
      x = xtitle,
      y = ytitle
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = legend_title),
      x = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::theme_light() +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(),
      expand = ggplot2::expansion()
    )

  if (
    !requireNamespace("ggthemes", quietly = TRUE) ||
      length(unique(ftir$sample_id)) > 15
  ) {
    p <- p +
      ggplot2::scale_color_viridis_d()
  } else {
    p <- p +
      ggthemes::scale_color_calc()
  }

  if (grepl("normalized", mode)) {
    p <- p +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank()
      )
  }

  attr(p, "intensity") <- attr(ftir, "intensity")

  return(p)
}


#' Plot Raman in stacked format
#'
#' @description Plot the Raman spectra in a journal prepared format. It may be
#'  desirable to plot spectra 'stacked and offset' by a certain amount.
#'
#'  Tracez les spectres Raman dans un format préparé par un journal. Il peut être
#'  souhaitable de tracer les spectres 'empilés et décalés' d'une
#'  certaine quantité.
#'
#' @inheritParams plot_raman_core
#' @param stack_offset The amount in intensity units of stacking offset to use.
#'
#'  Le montant en unités d'intensité de décalage d'empilement à utiliser.
#'
#' @inherit plot_raman_core return
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Plot Raman spectras stacked showing the differences in a dataset
#'   plot_raman_stacked(raman_data)
#' }
plot_raman_stacked <- function(
  ftir,
  plot_title = "Raman Spectra",
  legend_title = "Sample ID",
  stack_offset = 10,
  lang = NA
) {
  ftir <- check_ftir_data(ftir)

  if (!is.numeric(stack_offset) || length(stack_offset) > 1) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR:::plot_raman_stacked}. {.arg stack_offset} must be a single numeric value.",
        fr = "Erreur dans {.fn PlotFTIR:::plot_raman_stacked}. {.arg stack_offset} doit \u00eatre une valeur num\u00e9rique unique."
      ),
      call = rlang::caller_env()
    )
  }
  if (stack_offset < 0) {
    .pkg_abort(
      list(
        en = "Error in {.fn PlotFTIR:::plot_raman_stacked}. {.arg stack_offset} must be non-negative.",
        fr = "Erreur dans {.fn PlotFTIR:::plot_raman_stacked}. {.arg stack_offset} doit \u00eatre sup\u00e9rieur ou \u00e9gal \u00e0 z\u00e9ro."
      ),
      call = rlang::caller_env()
    )
  }

  mode <- attr(ftir, "intensity")

  stack_samples <- unique(ftir$sample_id)
  nsamples <- length(unique(stack_samples))

  if (nsamples > 1) {
    offset <- data.frame(
      "sample_id" = stack_samples,
      "offset" = seq(from = 0, by = stack_offset, length.out = nsamples)
    )

    ftir <- merge(x = ftir, y = offset, by = "sample_id")
    ftir$intensity <- ftir$intensity + ftir$offset
    ftir$offset <- NULL
  }

  p <- plot_raman_core(
    ftir = ftir,
    plot_title = plot_title,
    legend_title = legend_title,
    lang = lang
  )

  p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  suppressMessages(p <- p + ggplot2::ylab("Intensity (a.u.)"))

  attr(p, "spectra_style") <- "stacked"

  return(p)
}


#' Plot Raman Spectra Overlaid
#'
#' @description Produce a basic spectra overlay plot for all samples found in
#' the Raman dataset provided.
#'
#' Produisez un tracé de base de superposition de spectres pour tous les
#' échantillons trouvés dans l'ensemble de données Raman fourni.
#'
#' @inherit plot_raman_core params return
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_raman(raman_data)
#' }
plot_raman <- function(
  ftir,
  plot_title = "Raman Spectra",
  legend_title = "Sample ID",
  lang = NA
) {
  ftir <- check_ftir_data(ftir)
  p <- plot_raman_core(
    ftir = ftir,
    plot_title = plot_title,
    legend_title = legend_title,
    lang = lang
  )

  attr(p, "spectra_style") <- "normal"

  return(p)
}
