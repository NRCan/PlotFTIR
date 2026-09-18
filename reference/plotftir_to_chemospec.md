# Convert \`PlotFTIR\` data to \`ChemoSpec\` format

Converts \`PlotFTIR\` data to that ready to use by the \`ChemoSpec\`
package.

Convertit les données \`PlotFTIR\` en données prêtes à être utilisées
par le paquet \`ChemoSpec\`.

## Usage

``` r
plotftir_to_chemospec(
  ftir,
  group_crit = NA_character_,
  group_colours = "auto",
  description = "FTIR Study"
)
```

## Arguments

- ftir:

  A data.frame in long format with columns \`sample_id\`,
  \`wavenumber\`, and \`absorbance\`/\`transmittance\`. Un data.frame au
  format long avec les colonnes \`sample_id\`, \`wavenumber\`, et
  \`absorbance\`/\`transmittance\`.

- group_crit:

  A vector of character strings. Corresponds to
  \[ChemoSpec::files2SpectraObject()\] \`gr.crit\` parameter.

  Un vecteur de chaînes de caractères. Correspond au paramètre
  \`gr.crit\` de \[ChemoSpec::files2SpectraObject()\].

- group_colours:

  Group colours. Corresponds to \[ChemoSpec::files2SpectraObject()\]
  \`gr.cols\` parameter.

  Couleurs du groupe. Correspond au paramètre \`gr.cols\` de
  \[ChemoSpec::files2SpectraObject()\].

- description:

  A description of the experiment. Corresponds to
  \[ChemoSpec::files2SpectraObject()\] \`descrip\` parameter.

  Description de l'expérience. Correspond au paramètre \`descrip\` de
  \[ChemoSpec::files2SpectraObject()\].

## Value

A \`ChemoSpec\` data object

Un objet de données \`ChemoSpec\`

## See also

\[ChemoSpec::files2SpectraObject()\] for import requirements, and
\[chemospec_to_plotftir()\] for converting to \`PlotFTIR\` format.

\[ChemoSpec::files2SpectraObject()\] pour les conditions d'importation,
et \[chemospec_to_plotftir()\] pour la conversion au format
\`PlotFTIR\`.

## Examples

``` r
if (requireNamespace("ChemoSpec", quietly = TRUE) && interactive()) {
  # convert biodiesel to a `chemospec` object
  plotftir_to_chemospec(biodiesel)
}
```
