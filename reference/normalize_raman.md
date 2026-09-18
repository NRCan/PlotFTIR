# Normalize Raman Intensity Spectra

Scales spectra to a common intensity reference. Supports two methods:
(1) vector norm (L2 normalization — each spectrum has unit length), or
(2) maximum peak scaling (each spectrum's highest point is set to 1).
Both are standard practices in Raman spectroscopy for comparing relative
band intensities across samples.

Met à l'échelle les spectres selon une référence d'intensité commune.
Supporte deux méthodes : (1) norme vectorielle (normalisation L2), ou
(2) mise à l'échelle par le pic maximum (le point le plus élevé de
chaque spectre est fixé à 1). Les deux sont des pratiques standard en
spectroscopie Raman pour comparer les intensités relatives des bandes
entre échantillons.

## Usage

``` r
normalize_raman(raman, sample_ids = NA, method = "vector")
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

- method:

  One of two values: \* \`"vector"\` — L2 normalization: each spectrum
  is divided by its Euclidean norm. \* \`"max"\` — Maximum scaling: each
  spectrum is divided by its maximum intensity value.

  Une des deux valeurs : \* \`"vector"\` — Normalisation L2 : chaque
  spectre est divisé par sa norme euclidienne. \* \`"max"\` — Mise à
  l'échelle maximale : chaque spectre est divisé par sa valeur
  d'intensité maximale.

## Value

A data.frame containing the normalized Raman spectra with \`attr(raman,
"intensity")\` set to \`"normalized raman"\`.

Un data.frame contenant les spectres Raman normalisés avec \`attr(raman,
"intensity")\` défini sur \`"normalized raman"\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Normalize all samples using vector norm (L2)
normalize_raman(raman_data, method = "vector")

# Normalize all samples by maximum peak height
normalize_raman(raman_data, method = "max")
} # }
```
