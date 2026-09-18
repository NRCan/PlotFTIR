# Correct Spectrum Baseline

Fits and subtracts a baseline from spectra using Asymmetric Least
Squares (AsLS), as described by Eilers (2004). Commonly used in Raman
spectroscopy to remove fluorescence background that manifests as a
broad, slowly varying signal.

Ajuste et soustrait une ligne de base des spectres à l'aide du Moindre
Carré Asymétrique (AsLS), comme décrit par Eilers (2004). Couramment
utilisé en spectroscopie Raman pour éliminer le fond de fluorescence qui
se manifeste comme un signal large et lentement variable.

## Usage

``` r
baseline_correct(raman, sample_ids = NA, lambda = 6, p = 0.001)
```

## Arguments

- raman:

  A data.frame of Raman spectra in long format with columns
  \`sample_id\`, \`wavenumber\`, and \`intensity\`, carrying an
  \`intensity\` attribute of \`"raman"\` or \`"normalized raman"\` (as
  produced by \[read_raman()\]). FTIR absorbance/transmittance data is
  not accepted.

  Un data.frame de spectres Raman au format long avec les colonnes
  \`sample_id\`, \`wavenumber\` et \`intensity\`, portant un attribut
  \`intensity\` de \`"raman"\` ou \`"normalized raman"\` (tel que
  produit par \[read_raman()\]). Les données FTIR
  d'absorbance/transmittance ne sont pas acceptées.

- sample_ids:

  A vector of one or more \`sample_id\`s to select.

  Un vecteur d'un ou plusieurs \`sample_id\`s à sélectionner.

- lambda:

  The smoothness parameter, expressed as a base-10 exponent: the
  smoothing penalty applied is \`10^lambda\`. Larger values give
  smoother baselines. Default is 6 (a penalty of 1e6). Typical range: 3
  to 9 (1e3 to 1e9).

  Le paramètre de lissage, exprimé sous forme d'exposant en base 10 : la
  pénalité de lissage appliquée est \`10^lambda\`. Des valeurs plus
  élevées donnent des lignes de base plus lisses. Par défaut, 6 (une
  pénalité de 1e6). Plage typique : 3 à 9 (1e3 à 1e9).

- p:

  The asymmetry parameter. Controls how much positive residuals are
  favored over negative ones. Must be in (0, 0.5\]. Default is 0.001.

  Le paramètre d'asymétrie. Contrôle dans quelle mesure les résidus
  positifs sont favorisés par rapport aux négatifs. Doit être dans (0,
  0.5\]. Par défaut, 0.001.

## Value

a data.frame containing the baseline-corrected Raman spectra.

un data.frame contenant les spectres Raman corrigés de la ligne de base.

## Examples

``` r
if (requireNamespace("baseline", quietly = TRUE)) {
  # Generate synthetic Raman data with baseline
  wn <- seq(100, 2000, by = 2)
  peaks <- 100 * exp(-(wn - 500)^2 / 5000) + 50 * exp(-(wn - 1000)^2 / 8000)
  background <- 0.001 * (wn - 100)^2
  measured <- peaks + background + rnorm(length(wn), sd = 5)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = measured,
    sample_id = "sample_with_baseline"
  )
  attr(raman_data, "intensity") <- "raman"

  # Correct the baseline
  raman_corrected <- baseline_correct(raman_data, lambda = 6, p = 0.01)
}
```
