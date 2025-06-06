% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{plotftir_to_ir}
\alias{plotftir_to_ir}
\title{Convert `PlotFTIR` data to `ir`}
\usage{
plotftir_to_ir(ftir, metadata = NA)
}
\arguments{
\item{ftir}{A data.frame in long format with columns `sample_id`,
  `wavenumber`, and `absorbance`. The `absorbance` column may be replaced by
  a `transmittance` column for transmittance plots. The code determines the
  correct y axis units and labels the plot/adjusts the margins appropriately.

  Un data.frame au format long avec les colonnes `sample_id`, `wavenumber`,
  et `absorbance`. La colonne `absorbance` peut être remplacée par une
  colonne `transmittance` pour les tracés de transmission. Le code détermine
  les unités correctes de l'axe y et étiquette le tracé/ajuste les marges de
  manière appropriée.}

\item{metadata}{Additional data to pass to `ir` to include as metadata. Should be structured
  as a data.frame.

  Données supplémentaires à transmettre à `ir` pour les inclure dans les métadonnées.
  Doit être structuré comme un data.frame.}
}
\value{
an `ir` classed data.frame structured for use in that package.

un data.frame de classe `ir` structuré pour être utilisé dans ce paquet.
}
\description{
Converts `PlotFTIR` data to that ready to use by the `ir` package.

Convertit les données `PlotFTIR` en données prêtes à être utilisées par le paquet `ir`.
}
\examples{
if (requireNamespace("ir", quietly = TRUE)) {
  # convert biodiesel to a `ir` object
  plotftir_to_ir(biodiesel,
    metadata = data.frame("Biodiesel_Content" = c(0, 0.25, 0.5, 1, 2.5, 5, 7.5, 10, 0.5, 5, NA))
  )
}
}
\seealso{
[ir::ir_new_ir()] for information on how ir takes in data.
}
